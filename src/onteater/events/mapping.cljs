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
            [cljs.reader :as reader]
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
 :scenario/clear
 ;; Reset the scenario workspace to a blank slate: empty the text and drop the
 ;; mapping board back to its pristine "No entries yet" state by orphaning the
 ;; active session (cf. `:mapping/new-session`) and clearing the run status +
 ;; curation history. Only touches `:scenario`, so LLM config under [:llm]/[:ollama]
 ;; is preserved — the same model stays ready for the next Run mapping.
 (fn [db _]
   (update db :scenario merge {:raw-text "" :active nil :selected-entry nil
                               :run nil :undo [] :redo []})))

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
;;
;; A run has one of three ontology-presentation strategies, chosen by
;; `prompts/choose-strategy` (or pinned by the user under
;; [:scenario :strategy]): :full (whole compaction, one pass), :scoped
;; (lexically scoped to each chunk, one pass), or :staged (coarse pass over the
;; hierarchy top, then per-branch refinement of every entry typed to a class
;; that still has subclasses). Requests carry an Ollama num_ctx sized to the
;; messages so long prompts are never silently truncated.

(defn- briefing-text* [db] (get-in db [:ontology :briefing :text]))

(defn- chunk-opts
  "The system-prompt opts for one chunk under `strategy`: the shared
  strategy→compaction mapping (prompts/strategy-opts — the same fn the eval
  harness uses) plus this session's curated extras."
  [db model chunk strategy]
  (merge (prompts/strategy-opts model chunk strategy)
         {:briefing (briefing-text* db)}
         (when (= :scoped strategy)
           {:module-summaries (get-in db [:ontology :briefing :module-summaries])})))

(defn- sized-request
  "Build the provider request for `messages` (+ optional schema), sizing num_ctx
  to the payload. Returns {:req r :overflow? b}; `:overflow?` also fires when a
  num_ctx the user pinned in Settings is smaller than the sized need (the
  provider adapter honors the user's value, so the prompt would silently
  truncate without the warning)."
  [db cfg messages json-schema]
  (let [{:keys [num-ctx overflow?]} (prompts/messages-num-ctx messages)
        user-ctx (get-in db [:ollama :options :num_ctx])]
    {:req (providers/chat-request
           cfg
           {:messages messages
            :json-schema json-schema
            :temperature (get-in db [:ollama :options :temperature] 0.2)
            :num-ctx num-ctx})
     :overflow? (or overflow?
                    (boolean (and (= :ollama (:provider cfg))
                                  (number? user-ctx) (< user-ctx num-ctx))))}))

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
       ;; A new run replaces the previous run's proposals ONCE, here — the user's
       ;; curated entries survive. Per-chunk results then accumulate via
       ;; merge-entries (which never drops existing entries), so a multi-chunk or
       ;; staged run keeps every chunk's proposals on the board.
       (let [db       (-> db ensure-session
                          (update-active assoc-in [:scenario :text] text)
                          (update-active m/clear-proposed))
             chunks   (prompts/chunk-scenario text)
             strategy (or (get-in db [:scenario :strategy])
                          (prompts/choose-strategy model text))]
         {:db (-> db
                  (assoc-in [:scenario :run] {:status :running :phase :map
                                              :strategy strategy
                                              :chunks (count chunks)
                                              :done-chunks 0 :received 0 :buffer "" :error nil
                                              :started-at (.now js/Date) :ended-at nil})
                  (assoc-in [:scenario :run :queue] (vec chunks)))
          :dispatch [:mapping/run-next]})))))

(rf/reg-event-fx
 :mapping/run-next
 (fn [{:keys [db]} _]
   (let [queue (get-in db [:scenario :run :queue])]
     (if (empty? queue)
       {:dispatch [:mapping/map-phase-complete]}
       ;; Non-streaming: every provider returns the complete structured JSON in
       ;; one response once the model finishes (including any internal
       ;; "thinking"). This is far more robust than assembling token deltas —
       ;; thinking models stream empty `content` during reasoning. Cancellation
       ;; still works via AbortController; progress is shown per chunk. The
       ;; provider adapter builds the path/headers/body for the active provider
       ;; (native structured output where supported; the prompt-embedded JSON
       ;; shape + tolerant parser remain the parsing source of truth).
       (let [model    (get-in db [:ontology :model])
             sess     (active-session db)
             chunk    (first queue)
             cfg      (providers/active-config db)
             strategy (get-in db [:scenario :run :strategy] :full)
             opts     (chunk-opts db model chunk strategy)
             messages (prompts/mapping-messages model sess chunk opts)
             ;; Staged coarse pass: restrict the grammar enum to the ids the
             ;; coarse view actually shows, or a llama.cpp runner could emit
             ;; the hidden leaf classes the pass is meant to defer to refine.
             schema   (prompts/mapping-schema model (when (= :staged strategy)
                                                      (:include-ids opts)))
             {:keys [req overflow?]} (sized-request db cfg messages schema)]
         {:db (-> db
                  (assoc-in [:scenario :run :queue] (vec (rest queue)))
                  (cond-> overflow? (assoc-in [:scenario :run :ctx-overflow?] true)))
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

(rf/reg-event-fx
 :mapping/map-phase-complete
 ;; All chunks mapped. A :staged run now narrows: every proposed entry typed to
 ;; a class with subclasses goes through per-branch refinement batches. Other
 ;; strategies (and a staged run with nothing to refine) finish here.
 (fn [{:keys [db]} _]
   (let [model    (get-in db [:ontology :model])
         sess     (active-session db)
         staged?  (= :staged (get-in db [:scenario :run :strategy]))
         batches  (when (and staged? model sess)
                    (prompts/refine-batches model sess))]
     (if (seq batches)
       {:db (-> db
                (assoc-in [:scenario :run :phase] :refine)
                (assoc-in [:scenario :run :refine-queue] (vec batches))
                (assoc-in [:scenario :run :refine-total] (count batches))
                (assoc-in [:scenario :run :refine-done] 0))
        :dispatch [:mapping/refine-next]}
       {:dispatch [:mapping/run-complete]}))))

(rf/reg-event-fx
 :mapping/refine-next
 ;; Pop the next refinement batch and send its per-branch narrowing request
 ;; (scoped fully-glossed subtree view + the entries to refine); finishes the
 ;; run when the queue empties. Same :key :mapping, so Cancel aborts this too.
 (fn [{:keys [db]} _]
   (let [queue (get-in db [:scenario :run :refine-queue])]
     (if (empty? queue)
       {:dispatch [:mapping/run-complete]}
       (let [model    (get-in db [:ontology :model])
             sess     (active-session db)
             batch    (first queue)
             cfg      (providers/active-config db)
             messages (prompts/refine-messages model sess batch)
             {:keys [req overflow?]} (sized-request db cfg messages
                                                    (prompts/mapping-schema model))]
         {:db (-> db
                  (assoc-in [:scenario :run :refine-queue] (vec (rest queue)))
                  (cond-> overflow? (assoc-in [:scenario :run :ctx-overflow?] true)))
          :llm/request (merge req
                              {:key :mapping
                               :base-url (:base-url cfg)
                               :stream? false
                               :on-done [:mapping/refine-done]
                               :on-error [:mapping/run-error]})})))))

(rf/reg-event-fx
 :mapping/refine-done
 (fn [{:keys [db]} [_ response]]
   (let [model   (get-in db [:ontology :model])
         text    (get-in db [:scenario :raw-text])
         content (providers/response-text (providers/active-config db) response)
         {:keys [entries status]} (prompts/parse-response content)
         db (-> db
                (update-active (fn [s] (prompts/apply-refinements model s entries)))
                ;; refinement may have retargeted entries — re-validate the
                ;; still-proposed ones (never curated entries: forcing an entry
                ;; deliberately clears its flags)
                (update-active update :entries
                               (fn [es]
                                 (mapv (fn [e]
                                         (if (= :proposed (:status e))
                                           (first (prompts/validate-entries model text [e]))
                                           e))
                                       es)))
                (update-in [:scenario :run :refine-done] (fnil inc 0))
                (cond-> (= :no-json status)
                  (assoc-in [:scenario :run :schema-ignored?] true)))]
     {:db db :dispatch [:mapping/refine-next]})))

(rf/reg-event-db
 :mapping/set-strategy
 ;; Pin the ontology-presentation strategy (:full/:scoped/:staged) or nil = auto.
 (fn [db [_ strategy]] (assoc-in db [:scenario :strategy] strategy)))

(rf/reg-event-db
 :mapping/run-complete
 (fn [db _] (-> db (assoc-in [:scenario :run :status] :done)
                (assoc-in [:scenario :run :ended-at] (.now js/Date)))))

(rf/reg-event-fx
 :mapping/run-error
 (fn [{:keys [db]} [_ err]]
   {:db (-> db (assoc-in [:scenario :run :status] :error)
            (assoc-in [:scenario :run :error] (:message err))
            (assoc-in [:scenario :run :ended-at] (.now js/Date)))
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

;; --- ontology briefing (generated once per ontology, user-curated) -----------
;;
;; A one-time meta-prompting pass (prompts/briefing-messages): the model studies
;; the compacted ontology and produces module summaries + disambiguation rules.
;; The reply is validated (ids must exist — prompts/parse-briefing), rendered to
;; text, and stored under [:ontology :briefing] where the user can EDIT it; the
;; text is injected verbatim into every subsequent mapping prompt. Never
;; regenerated silently.

(rf/reg-event-fx
 :briefing/run
 (fn [{:keys [db]} _]
   (let [model (get-in db [:ontology :model])
         cfg   (providers/active-config db)
         not-ready (providers/ready? cfg)]
     (cond
       (nil? model) {:dispatch [:ui/error "Load an ontology first (Ontology tab)."]}
       not-ready    {:dispatch [:ui/error not-ready]}
       :else
       (let [messages (prompts/briefing-messages model)
             {:keys [req]} (sized-request db cfg messages (prompts/briefing-schema))]
         {:db (assoc-in db [:ontology :briefing-run] {:status :running :error nil})
          :llm/request (merge req
                              {:key :briefing
                               :base-url (:base-url cfg)
                               :stream? false
                               :on-done [:briefing/done]
                               :on-error [:briefing/error]})})))))

(rf/reg-event-fx
 :briefing/done
 (fn [{:keys [db]} [_ response]]
   (let [model   (get-in db [:ontology :model])
         content (providers/response-text (providers/active-config db) response)
         {:keys [briefing dropped status]} (prompts/parse-briefing model content)]
     (if (= :ok status)
       {:db (-> db
                (assoc-in [:ontology :briefing]
                          {:text (prompts/briefing-text briefing)
                           :module-summaries (into {} (map (juxt :module :summary))
                                                   (:module-summaries briefing))
                           :generated-at (.now js/Date)
                           :dropped dropped})
                (assoc-in [:ontology :briefing-run] {:status :done :error nil}))
        :dispatch [:ui/push-toast
                   {:kind :info
                    :text (str "Briefing generated — review and edit it before mapping."
                               (when (pos? dropped)
                                 (str " (" dropped " rule(s) referencing unknown ids dropped.)")))}]}
       {:db (assoc-in db [:ontology :briefing-run]
                      {:status :error
                       :error (case status
                                :empty "The model returned an empty reply."
                                "The model did not return parseable briefing JSON.")})}))))

(rf/reg-event-fx
 :briefing/error
 (fn [{:keys [db]} [_ err]]
   ;; A user cancel aborts the fetch, which also rejects here with {:kind :aborted}.
   ;; `:briefing/cancel` already cleared the run status, so ignore that case rather
   ;; than flashing a spurious "Briefing failed" error.
   (if (= :aborted (:kind err))
     {}
     {:db (assoc-in db [:ontology :briefing-run] {:status :error :error (:message err)})
      :dispatch [:ui/error (str "Briefing failed: " (:message err))]})))

(rf/reg-event-fx
 :briefing/cancel
 ;; Abort an in-flight briefing pass. We only clear the transient run status —
 ;; `[:ontology :briefing]` is left untouched, so mapping falls back to the
 ;; previous briefing (if one was generated) or, when none exists, the default
 ;; no-briefing mode. Mirrors `:mapping/cancel`.
 (fn [{:keys [db]} _]
   {:llm/abort {:key :briefing}
    :db (update db :ontology dissoc :briefing-run)}))

(defn- briefing-idb-key
  "Per-ontology IndexedDB key for the persisted briefing — a briefing belongs to
  one ontology (title+version), not to a session."
  [model]
  (str "briefing:" (get-in model [:meta :title] "untitled")
       "@" (or (get-in model [:meta :version]) "-")))

(defn- persist-briefing-fx [db]
  (when-let [model (get-in db [:ontology :model])]
    {:io/idb-save {:key (briefing-idb-key model)
                   :value (pr-str (get-in db [:ontology :briefing]))}}))

(rf/reg-event-fx
 :briefing/set-text
 ;; The user's edit of the briefing text — the curated artifact that gets
 ;; injected into mapping prompts. Persisted per ontology.
 (fn [{:keys [db]} [_ text]]
   (let [db (assoc-in db [:ontology :briefing :text] text)]
     (merge {:db db} (persist-briefing-fx db)))))

(rf/reg-event-fx
 :briefing/clear
 (fn [{:keys [db]} _]
   (merge {:db (update db :ontology dissoc :briefing :briefing-run)}
          (when-let [model (get-in db [:ontology :model])]
            {:io/idb-delete {:key (briefing-idb-key model)}}))))

(rf/reg-event-fx
 :briefing/load-persisted
 ;; Dispatched by :ontology/load — restore this ontology's curated briefing.
 (fn [{:keys [db]} _]
   (if-let [model (get-in db [:ontology :model])]
     {:io/idb-load {:key (briefing-idb-key model)
                    :on-loaded [:briefing/persisted-loaded]}}
     {})))

(rf/reg-event-db
 :briefing/persisted-loaded
 ;; Restore a persisted briefing: `value` is an edn string we wrote ourselves,
 ;; but parse defensively (read-string can throw on a corrupt store) and only
 ;; install a map that actually carries briefing text.
 (fn [db [_ value]]
   (let [b (when (and value (string? value))
             (try (reader/read-string value) (catch :default _ nil)))]
     (if (and (map? b) (seq (:text b)))
       (assoc-in db [:ontology :briefing] b)
       db))))
