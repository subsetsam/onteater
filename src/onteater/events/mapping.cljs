(ns onteater.events.mapping
  "Events for the scenario-mapping workspace: scenario input, the LLM
  mapping run (chunked, structured-output, streaming progress, cancellable), entry
  curation (accept/reject/force), and session save/load. The mapping run threads
  chunks sequentially so forced constraints and merged results carry forward; all
  LLM traffic goes through the shared `:llm/request` effect, with request
  bodies/headers and response parsing per active provider (Ollama, cloud,
  Azure Gov) built by `onteater.llm.providers`."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.model.graph :as g]
            [onteater.model.mapping :as m]
            [onteater.llm.prompts :as prompts]
            [onteater.llm.providers :as providers]
            [onteater.io.file]))

;; --- helpers ----------------------------------------------------------------

(defn- session-model-stamp
  "Provider-qualified model string recorded on a session, e.g.
  \"claude-opus-4-8@cloud\" or \"llama3.1@ollama\" — an opaque string as far as
  model.mapping is concerned."
  [db]
  (let [cfg (providers/active-config db)]
    (str (:model cfg) "@" (name (:provider cfg)))))

(defn- active-session [db] (get-in db [:scenario :sessions (get-in db [:scenario :active])]))
(defn- put-session [db session] (assoc-in db [:scenario :sessions (:id session)] session))
(defn- update-active [db f & args]
  (if-let [s (active-session db)] (put-session db (apply f s args)) db))

;; --- scenario input ---------------------------------------------------------

(rf/reg-event-db
 :scenario/set-text
 (fn [db [_ text]] (assoc-in db [:scenario :raw-text] text)))

(rf/reg-event-db
 :scenario/toggle-rendered
 (fn [db _] (update-in db [:scenario :rendered?] not)))

(rf/reg-event-fx
 :scenario/load-file
 (fn [_ _]
   {:io/open-file {:on-loaded [:scenario/file-loaded] :on-error [:ui/error]
                   :accept {:description "Scenario text"
                            :mime "text/plain" :extensions [".txt" ".md"]}}}))

(rf/reg-event-db
 :scenario/file-loaded
 (fn [db [_ filename text _handle]]
   (-> db
       (assoc-in [:scenario :raw-text] text)
       (assoc-in [:scenario :source-file] filename))))

(rf/reg-event-db
 :scenario/select-entry
 (fn [db [_ id]] (assoc-in db [:scenario :selected-entry] id)))

(rf/reg-event-db
 :scenario/set-board-group
 (fn [db [_ g]] (assoc-in db [:scenario :board :group-by] g)))

(rf/reg-event-db
 :scenario/set-confidence-filter
 (fn [db [_ v]] (assoc-in db [:scenario :board :min-confidence] v)))

;; --- session lifecycle ------------------------------------------------------

(defn- ensure-session
  "Return db with an active session bound to the current scenario text + ontology,
  creating one if necessary."
  [db]
  (if (active-session db)
    db
    (let [model (get-in db [:ontology :model])
          sess  (m/new-session {:scenario {:text (get-in db [:scenario :raw-text])
                                           :source-file (get-in db [:scenario :source-file])}
                                :ontology-ref {:title (get-in model [:meta :title])
                                               :file (get-in db [:ontology :file :name])}
                                :model (session-model-stamp db)})]
      (-> db (put-session sess) (assoc-in [:scenario :active] (:id sess))))))

(rf/reg-event-db
 :mapping/new-session
 (fn [db _]
   (-> db
       (assoc-in [:scenario :active] nil)
       (ensure-session))))

;; --- the mapping run --------------------------------------------------------

(rf/reg-event-fx
 :mapping/run
 (fn [{:keys [db]} _]
   (let [model (get-in db [:ontology :model])
         not-ready (providers/ready? (providers/active-config db))
         text  (get-in db [:scenario :raw-text])]
     (cond
       (nil? model) {:dispatch [:ui/error "Load an ontology first (Ontology tab)."]}
       not-ready {:dispatch [:ui/error not-ready]}
       (str/blank? text) {:dispatch [:ui/error "Enter or upload a scenario first."]}
       :else
       (let [db     (-> db ensure-session
                        (update-active assoc-in [:scenario :text] text))
             chunks (prompts/chunk-scenario text)]
         {:db (-> db
                  (assoc-in [:scenario :run] {:status :running :chunks (count chunks)
                                              :done-chunks 0 :received 0 :buffer "" :error nil})
                  (assoc-in [:scenario :run :queue] (vec chunks)))
          :dispatch [:mapping/run-next]})))))

(rf/reg-event-fx
 :mapping/run-next
 (fn [{:keys [db]} _]
   (let [queue (get-in db [:scenario :run :queue])]
     (if (empty? queue)
       {:dispatch [:mapping/run-complete]}
       ;; Non-streaming: every provider returns the complete structured JSON in
       ;; one response once the model finishes (including any internal
       ;; "thinking"). This is far more robust than assembling token deltas —
       ;; thinking models stream empty `content` during reasoning. Cancellation
       ;; still works via AbortController; progress is shown per chunk. The
       ;; provider adapter builds the path/headers/body for the active provider
       ;; (native structured output where supported; the prompt-embedded JSON
       ;; shape + tolerant parser remain the parsing source of truth).
       (let [model  (get-in db [:ontology :model])
             sess   (active-session db)
             chunk  (first queue)
             cfg    (providers/active-config db)
             req    (providers/chat-request
                     cfg
                     {:messages (prompts/mapping-messages model sess chunk)
                      :json-schema (prompts/mapping-schema)
                      :temperature (get-in db [:ollama :options :temperature] 0.2)})]
         {:db (assoc-in db [:scenario :run :queue] (vec (rest queue)))
          :llm/request (merge req
                              {:key :mapping
                               :base-url (:base-url cfg)
                               :stream? false
                               :on-done [:mapping/chunk-done]
                               :on-error [:mapping/run-error]})})))))

(rf/reg-event-fx
 :mapping/chunk-done
 (fn [{:keys [db]} [_ response]]
   (let [model   (get-in db [:ontology :model])
         text    (get-in db [:scenario :raw-text])
         ;; Provider-specific extraction (Ollama content/thinking fallback,
         ;; Anthropic text blocks, OpenAI/Azure choices[0]) lives in the adapter.
         content (providers/response-text (providers/active-config db) response)
         {:keys [entries unmapped status]} (prompts/parse-response content)
         validated (prompts/validate-entries model text entries)
         db     (-> db
                    (update-active update :entries
                                   (fn [existing] (m/merge-entries existing validated)))
                    (update-active update-in [:unmapped :scenario-elements]
                                   (fn [xs] (into (vec xs) unmapped)))
                    (update-in [:scenario :run :done-chunks] inc)
                    ;; Sticky across chunks: if any chunk came back non-blank but
                    ;; unparseable, the model ignored the response schema — surface
                    ;; that distinctly rather than as a generic "no mappings".
                    (cond-> (= :no-json status)
                      (assoc-in [:scenario :run :schema-ignored?] true)))]
     ;; process the next chunk (or finish)
     {:db db :dispatch [:mapping/run-next]})))

(rf/reg-event-db
 :mapping/run-complete
 (fn [db _] (assoc-in db [:scenario :run :status] :done)))

(rf/reg-event-fx
 :mapping/run-error
 (fn [{:keys [db]} [_ err]]
   {:db (-> db (assoc-in [:scenario :run :status] :error)
            (assoc-in [:scenario :run :error] (:message err)))
    :dispatch [:ui/error (str "Mapping run failed: " (:message err))]}))

(rf/reg-event-fx
 :mapping/cancel
 (fn [{:keys [db]} _]
   {:llm/abort {:key :mapping}
    :db (assoc-in db [:scenario :run :status] :idle)}))

;; --- mapping history (undo/redo for curation + chat-applied changes) ---------

(def record-mapping
  "Interceptor: snapshot the active mapping session before a curation event and, if
  it changed, push the prior session onto the scenario undo stack (clearing redo).
  Mirrors the ontology history interceptor so chat-applied ops and accept/reject/
  force are all undoable."
  (rf/->interceptor
   :id :record-mapping
   :before (fn [ctx] (assoc-in ctx [:coeffects ::pre] (active-session (get-in ctx [:coeffects :db]))))
   :after
   (fn [ctx]
     (let [pre     (get-in ctx [:coeffects ::pre])
           post-db (or (get-in ctx [:effects :db]) (get-in ctx [:coeffects :db]))
           post    (active-session post-db)]
       (if (or (nil? pre) (= pre post))
         ctx
         (assoc-in ctx [:effects :db]
                   (-> post-db
                       (update-in [:scenario :undo] (fnil conj []) pre)
                       (assoc-in [:scenario :redo] []))))))))

(def mhist [record-mapping])

(rf/reg-event-db
 :mapping/undo
 (fn [db _]
   (let [undo (get-in db [:scenario :undo])]
     (if (empty? undo)
       db
       (let [prev (peek undo) cur (active-session db)]
         (-> db (put-session prev)
             (assoc-in [:scenario :active] (:id prev))
             (assoc-in [:scenario :undo] (pop undo))
             (update-in [:scenario :redo] (fnil conj []) cur)))))))

(rf/reg-event-db
 :mapping/redo
 (fn [db _]
   (let [redo (get-in db [:scenario :redo])]
     (if (empty? redo)
       db
       (let [nxt (peek redo) cur (active-session db)]
         (-> db (put-session nxt)
             (assoc-in [:scenario :active] (:id nxt))
             (assoc-in [:scenario :redo] (pop redo))
             (update-in [:scenario :undo] (fnil conj []) cur)))))))

;; --- entry curation ---------------------------------------------------------

(rf/reg-event-db
 :mapping/set-entry-status
 mhist
 (fn [db [_ id status]] (update-active db m/set-status id status)))

(rf/reg-event-db
 :mapping/force-entry
 mhist
 (fn [db [_ id node-id]] (update-active db m/force-entry id node-id)))

(rf/reg-event-db
 :mapping/remove-entry
 mhist
 (fn [db [_ id]] (update-active db m/remove-entry id)))


(rf/reg-event-db
 :mapping/remap-entry
 "Change an entry's target node (keeps status :proposed, re-validates)."
 (fn [db [_ id node-id]]
   (let [model (get-in db [:ontology :model])]
     (update-active db m/update-entry id
                    (fn [e] (-> e (assoc :node-id node-id :status :proposed)
                                (assoc :flags (if (g/exists? model node-id) #{} #{:invalid-target}))))))))

;; --- session save / load ----------------------------------------------------

(rf/reg-event-fx
 :mapping/save-session
 (fn [{:keys [db]} _]
   (if-let [s (active-session db)]
     (let [base (or (some-> (get-in s [:scenario :source-file]) (str/replace #"\.[^.]+$" ""))
                    "scenario")]
       {:io/save-as {:suggested-name (str base ".onteater-mapping.json")
                     :text (m/session->json s)
                     :on-saved [:mapping/session-saved] :on-error [:ui/error]}})
     {:dispatch [:ui/error "No mapping session to save."]})))

(rf/reg-event-fx
 :mapping/session-saved
 (fn [_ [_ name _handle]]
   {:dispatch [:ui/push-toast {:kind :info :text (str "Saved " name)}]}))

(rf/reg-event-fx
 :mapping/load-session
 (fn [_ _]
   {:io/open-file {:on-loaded [:mapping/session-file-loaded] :on-error [:ui/error]}}))

(rf/reg-event-db
 :mapping/session-file-loaded
 (fn [db [_ _filename text _handle]]
   (try
     (let [s (m/json->session text)]
       (-> db
           (assoc-in [:scenario :sessions (:id s)] s)
           (assoc-in [:scenario :active] (:id s))
           (assoc-in [:scenario :raw-text] (get-in s [:scenario :text]))))
     (catch :default _ db))))
