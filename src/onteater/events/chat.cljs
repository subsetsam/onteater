(ns onteater.events.chat
  "Chat events: send a turn with the mapping context assembled fresh,
  receive the assistant reply, parse any `mapping-update` blocks into proposed
  changes, and apply/dismiss them under the mapping-history interceptor (so applied
  ops are undoable). Ops touching FORCED entries are rejected client-side.

  Uses a non-streaming request: local 'thinking' models emit empty content during
  reasoning, which makes token-delta streaming unreliable; a single response with a
  'thinking…' placeholder is robust. Cancellation still works via AbortController."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [clojure.set :as set]
            [onteater.model.mapping :as m]
            [onteater.model.timeline :as tl]
            [onteater.llm.prompts :as prompts]
            [onteater.llm.providers :as providers]
            [onteater.events.mapping :refer [record-mapping]]))

(defn- active-session [db] (get-in db [:scenario :sessions (get-in db [:scenario :active])]))
(defn- update-active [db f & args]
  (if-let [s (active-session db)]
    (assoc-in db [:scenario :sessions (:id s)] (apply f s args))
    db))

(defn- update-message [session msg-id f]
  (update session :chat (fn [msgs] (mapv #(if (= msg-id (:id %)) (f %) %) msgs))))

(rf/reg-event-fx
 :chat/toggle
 (fn [{:keys [db]} _] {:db (update-in db [:scenario :chat-open?] not)}))

(rf/reg-event-fx
 :chat/discuss-entry
 (fn [{:keys [db]} [_ id]]
   {:db (-> db (assoc-in [:scenario :selected-entry] id)
            (assoc-in [:scenario :chat-open?] true))}))

(rf/reg-event-fx
 :chat/send
 (fn [{:keys [db]} [_ text]]
   (let [model (get-in db [:ontology :model])
         cfg   (providers/active-config db)
         not-ready (providers/ready? cfg)
         sess  (active-session db)]
     (cond
       (nil? model) {:dispatch [:ui/error "Load an ontology first."]}
       not-ready {:dispatch [:ui/error not-ready]}
       (nil? sess) {:dispatch [:ui/error "Run a mapping first, so there is a session to discuss."]}
       (str/blank? text) {}
       :else
       (let [transcript (:chat sess)
             messages   (prompts/chat-messages model sess transcript text)
             user-msg   {:id (str (random-uuid)) :role :user :content text}
             asst-id    (str (random-uuid))
             asst-msg   {:id asst-id :role :assistant :content "" :updates [] :pending? true}
             ;; No :json-schema — chat replies are prose (with optional
             ;; ```mapping-update``` blocks), not schema-constrained JSON.
             req        (providers/chat-request
                         cfg
                         {:messages messages
                          :temperature (get-in db [:ollama :options :temperature] 0.2)
                          :num-ctx (:num-ctx (prompts/messages-num-ctx messages))})]
         {:db (-> db (update-active update :chat (fnil into []) [user-msg asst-msg])
                  (assoc-in [:scenario :chat-input] ""))
          :llm/request (merge req
                              {:key :chat
                               :base-url (:base-url cfg)
                               :stream? false
                               :on-done [:chat/response asst-id]
                               :on-error [:chat/error asst-id]})})))))

(rf/reg-event-db
 :chat/response
 (fn [db [_ asst-id response]]
   (let [content (providers/response-text (providers/active-config db) response)
         {:keys [prose updates]} (prompts/parse-mapping-updates content)]
     (update-active db update-message asst-id
                    (fn [m] (assoc m :content prose :pending? false
                                   :updates (mapv (fn [u] (update u :ops #(mapv (fn [o] (assoc o :state :pending)) %)))
                                                  updates)))))))

(rf/reg-event-db
 :chat/error
 (fn [db [_ asst-id err]]
   (update-active db update-message asst-id
                  (fn [m] (assoc m :pending? false :error (:message err)
                                 :content (str "⚠ " (:message err)))))))

(rf/reg-event-fx
 :chat/cancel
 (fn [_ _] {:llm/abort {:key :chat}}))

(rf/reg-event-db
 :chat/set-input
 (fn [db [_ text]] (assoc-in db [:scenario :chat-input] text)))

;; --- applying proposed ops --------------------------------------------------
;; One diff-card pipeline serves BOTH mapping entries and timeline events/relations
;; (§6.5): route by the op's :target. Forced items are untouchable client-side.

(defn- op-forced-conflict? [sess op]
  (case (:target op)
    :entry (m/forced-op-conflict? sess op)
    (tl/forced-op-conflict? (:timeline sess) op)))

(defn- apply-op-to-session [sess op]
  (case (:target op)
    :entry (m/apply-op sess op)
    (update sess :timeline #(tl/apply-op % op))))

(defn- op-at [db msg-id ui-idx op-idx]
  (let [sess (active-session db)
        msg  (first (filter #(= msg-id (:id %)) (:chat sess)))]
    (get-in msg [:updates ui-idx :ops op-idx])))

(defn- set-op-state [db msg-id ui-idx op-idx state]
  (update-active db update-message msg-id
                 (fn [m] (assoc-in m [:updates ui-idx :ops op-idx :state] state))))

(rf/reg-event-fx
 :chat/apply-op
 [record-mapping]
 (fn [{:keys [db]} [_ msg-id ui-idx op-idx]]
   (let [sess (active-session db)
         op   (op-at db msg-id ui-idx op-idx)]
     (cond
       (nil? op) {:db db}
       (op-forced-conflict? sess op)
       {:db (set-op-state db msg-id ui-idx op-idx :rejected)
        :dispatch [:ui/push-toast {:kind :warn :text "That change touches a forced item and was not applied."}]}
       :else
       {:db (-> db (update-active apply-op-to-session op)
                (set-op-state msg-id ui-idx op-idx :applied))}))))

(rf/reg-event-fx
 :chat/apply-all
 [record-mapping]
 (fn [{:keys [db]} [_ msg-id ui-idx]]
   (let [sess (active-session db)
         msg  (first (filter #(= msg-id (:id %)) (:chat sess)))
         ops  (get-in msg [:updates ui-idx :ops])
         db'  (reduce (fn [d [i op]]
                        (if (or (not= :pending (:state op)) (op-forced-conflict? (active-session d) op))
                          (set-op-state d msg-id ui-idx i :rejected)
                          (-> d (update-active apply-op-to-session op) (set-op-state msg-id ui-idx i :applied))))
                      db (map-indexed vector ops))]
     {:db db'})))

(rf/reg-event-db
 :chat/dismiss-op
 (fn [db [_ msg-id ui-idx op-idx]]
   (set-op-state db msg-id ui-idx op-idx :dismissed)))

;; --- quick actions ----------------------------------------------------------

(rf/reg-event-fx
 :chat/quick-action
 (fn [{:keys [db]} [_ kind]]
   (let [sess (active-session db)
         sel  (when-let [id (get-in db [:scenario :selected-entry])] (m/entry sess id))
         prompt (case kind
                  :explain (str "Explain why "
                                (if sel (str "the excerpt \"" (:excerpt sel) "\" was mapped to " (:node-id sel))
                                    "the current mappings were chosen") ".")
                  :alternatives (str "Suggest alternative ontology nodes for "
                                     (if sel (str "\"" (:excerpt sel) "\"") "the low-confidence entries")
                                     ", and if you propose changes use a mapping-update block.")
                  :low-confidence "Re-examine the low-confidence entries and propose better mappings where possible (use mapping-update blocks)."
                  :unmapped "What significant scenario elements did you leave unmapped, and why?"
                  "")]
     {:dispatch [:chat/send prompt]})))

;; Timeline-aware quick actions (§6.5). These are GROUNDED (§12.14): the app attaches
;; the model-layer's *computed* cones/paths/gap data to the prompt, so the LLM narrates
;; real structure instead of re-deriving graph structure from the raw text.
(rf/reg-event-fx
 :chat/timeline-action
 (fn [{:keys [db]} [_ kind]]
   (let [sess  (active-session db)
         t     (:timeline sess)
         eid   (get-in db [:scenario :tl-ui :selected-event])
         e     (when eid (tl/event t eid))
         label #(or (:label (tl/event t %)) %)
         prompt
         (case kind
           :depends
           (if e
             (str "Using ONLY this computed dependency structure (do not infer new links), "
                  "explain what the event “" (:label e) "” depends on and why it matters:\n"
                  "Upstream (its ancestors, what it depended on): "
                  (str/join ", " (map label (tl/ancestors t eid))) "\n"
                  "Downstream (what depends on it): "
                  (str/join ", " (map label (tl/descendants t eid))))
             "Select an event on the timeline first, then ask again.")
           :trace
           (if e
             (let [anc     (tl/ancestors t eid)
                   sources (filter #(empty? (set/intersection anc (tl/ancestors t %))) anc)
                   paths   (mapcat #(tl/paths-between t % eid) sources)]
               (str "Trace the chain leading to “" (:label e) "”. Using ONLY these computed "
                    "paths, narrate them in causal order:\n"
                    (if (seq paths)
                      (str/join "\n" (map (fn [p] (str/join " → " (map label p))) paths))
                      "(this event has no upstream dependencies)")))
             "Select an event on the timeline first, then ask again.")
           :why-untyped
           (if (and e (nil? (:node-id e)))
             (str "The event “" (:label e) "” could not be typed against the ontology. The nearest "
                  "abstract class recorded is " (:nearest e) ", with reason: " (:why-no-fit e)
                  ". Explain the ontology gap this reveals and suggest a leaf class that would fill it.")
             "Select an untyped (?) event on the timeline first, then ask again.")
           "")]
     (if (str/blank? prompt)
       {}
       {:db (assoc-in db [:scenario :chat-open?] true)
        :dispatch [:chat/send prompt]}))))