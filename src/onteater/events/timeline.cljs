(ns onteater.events.timeline
  "Events for the temporal-mapping (timeline) feature: the second, occurrent-focused
  LLM extraction pass (independently re-runnable, non-streaming, cancellable), event/
  relation curation (accept/reject/force/remap, drag-to-create), the center-pane tab
  + timeline UI state (lane grouping, dependency-cone mode, selection, matrix cell),
  and the two feedback-loop actions of the gap report: 'Draft ontology element from
  this gap…' (jumps to the ontology workspace with a pre-filled add-node dialog) and
  'Re-run timeline pass' (which afterwards reports which gaps disappeared).

  Timeline edits share the scenario workspace's undo stack via the `record-mapping`
  interceptor (they snapshot the whole active session, `:timeline` included), so
  ⌘Z reverts a timeline change exactly as it reverts an entry change."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.model.graph :as g]
            [onteater.model.mapping :as m]
            [onteater.model.timeline :as tl]
            [onteater.llm.prompts :as prompts]
            [onteater.llm.providers :as providers]
            [onteater.events.mapping :refer [record-mapping]]
            [onteater.events.history :refer [record-history]]
            [onteater.io.file]
            [onteater.io.export]))

(defn- resolved-dark? [pref]
  (case pref :dark true :light false
        (boolean (and (exists? js/window)
                      (.-matches (js/matchMedia "(prefers-color-scheme: dark)"))))))

(defn- active-session [db] (get-in db [:scenario :sessions (get-in db [:scenario :active])]))
(defn- put-session [db session] (assoc-in db [:scenario :sessions (:id session)] session))
(defn- update-active [db f & args]
  (if-let [s (active-session db)] (put-session db (apply f s args)) db))
(defn- update-timeline
  "Apply `f` (a timeline -> timeline fn) to the active session's `:timeline`."
  [db f & args]
  (update-active db (fn [s] (apply update s :timeline f args))))

(def hist [record-mapping])

;; --- center-pane tab + timeline UI state ------------------------------------

(def default-tl-ui
  {:tab :mapping :grouping :entity :cone? false
   :selected-event nil :selected-relation nil :collapsed #{} :matrix-cell nil})

(defn- tl-ui [db] (get-in db [:scenario :tl-ui] default-tl-ui))
(defn- set-tl-ui [db k v] (assoc-in db [:scenario :tl-ui] (assoc (tl-ui db) k v)))

(rf/reg-event-db :scenario/set-center-tab (fn [db [_ tab]] (set-tl-ui db :tab tab)))
(rf/reg-event-db :timeline/set-grouping (fn [db [_ grp]] (set-tl-ui db :grouping grp)))
(rf/reg-event-db :timeline/toggle-cone (fn [db _] (set-tl-ui db :cone? (not (:cone? (tl-ui db))))))

(rf/reg-event-db
 :timeline/key-cone
 ;; The `d` shortcut only acts when the timeline tab of the scenario workspace is
 ;; active — elsewhere it is a no-op (so it never clobbers other contexts).
 (fn [db _]
   (if (and (= :scenario (:workspace db)) (= :timeline (:tab (tl-ui db))))
     (set-tl-ui db :cone? (not (:cone? (tl-ui db))))
     db)))
(rf/reg-event-db :timeline/set-matrix-cell (fn [db [_ cell]] (set-tl-ui db :matrix-cell cell)))

(rf/reg-event-db
 :timeline/select-event
 (fn [db [_ id]]
   (-> db (set-tl-ui :selected-event id) (set-tl-ui :selected-relation nil))))

(rf/reg-event-db
 :timeline/select-relation
 (fn [db [_ id]]
   (-> db (set-tl-ui :selected-relation id) (set-tl-ui :selected-event nil))))

(rf/reg-event-db
 :timeline/toggle-collapse
 (fn [db [_ id]]
   (let [c (:collapsed (tl-ui db))]
     (set-tl-ui db :collapsed (if (c id) (disj c id) (conj c id))))))

;; --- the extraction pass ----------------------------------------------------

(defn- session-model-stamp [db]
  (let [cfg (providers/active-config db)] (str (:model cfg) "@" (name (:provider cfg)))))

(defn- ensure-session
  "Guarantee an active session bound to the current scenario + ontology (a timeline
  pass can be the first thing a user runs)."
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

(rf/reg-event-fx
 :timeline/run
 (fn [{:keys [db]} _]
   (let [model     (get-in db [:ontology :model])
         not-ready (providers/ready? (providers/active-config db))
         text      (get-in db [:scenario :raw-text])]
     (cond
       (nil? model) {:dispatch [:ui/error "Load an ontology first (Ontology tab)."]}
       not-ready {:dispatch [:ui/error not-ready]}
       (str/blank? text) {:dispatch [:ui/error "Enter or upload a scenario first."]}
       :else
       (let [db      (-> db ensure-session
                         (update-active assoc-in [:scenario :text] text))
             ;; remember the gap count NOW so the run can report which gaps cleared
             pre-gap (tl/gap-count (tl/gap-report (:timeline (active-session db)) model))
             chunks  (prompts/chunk-scenario text)]
         {:db (-> db
                  (assoc-in [:scenario :timeline-run]
                            {:status :running :chunks (count chunks) :done-chunks 0
                             :error nil :schema-ignored? false :pre-gap pre-gap
                             :started-at (.now js/Date) :ended-at nil :queue (vec chunks)})
                  (set-tl-ui :tab :timeline))
          :dispatch [:timeline/run-next]})))))

(rf/reg-event-fx
 :timeline/run-next
 (fn [{:keys [db]} _]
   (let [queue (get-in db [:scenario :timeline-run :queue])]
     (if (empty? queue)
       {:dispatch [:timeline/run-complete]}
       ;; Non-streaming (CLAUDE.md): thinking models emit empty content while
       ;; reasoning; the complete JSON arrives once. Cancellation via AbortController.
       (let [model (get-in db [:ontology :model])
             sess  (active-session db)
             chunk (first queue)
             cfg   (providers/active-config db)
             messages (prompts/timeline-messages model sess chunk)
             req   (providers/chat-request
                    cfg {:messages messages
                         :json-schema (prompts/timeline-schema)
                         :temperature (get-in db [:ollama :options :temperature] 0.2)
                         :num-ctx (:num-ctx (prompts/messages-num-ctx messages))})]
         {:db (assoc-in db [:scenario :timeline-run :queue] (vec (rest queue)))
          :llm/request (merge req {:key :timeline
                                   :base-url (:base-url cfg)
                                   :stream? false
                                   :on-done [:timeline/chunk-done]
                                   :on-error [:timeline/run-error]})})))))

(rf/reg-event-fx
 :timeline/chunk-done
 (fn [{:keys [db]} [_ response]]
   (let [model   (get-in db [:ontology :model])
         text    (get-in db [:scenario :raw-text])
         cfg     (providers/active-config db)
         content (providers/response-text cfg response)
         {:keys [events relations status]} (prompts/parse-timeline-response content)
         {:keys [events relations]} (prompts/validate-timeline model text events relations)
         ;; Assign stable ids to the freshly-extracted events and rewrite each
         ;; relation's local endpoint ids onto them; then merge (preserving the
         ;; user's forced/accepted/rejected decisions) and prune any relation left
         ;; dangling by a de-dupe. Gaps (null typings) are carried through intact.
         uuid-of (into {} (map (fn [e] [(:local-id e) (str (random-uuid))])) events)
         new-events (mapv (fn [e] (tl/new-event (-> e (assoc :id (uuid-of (:local-id e))) (dissoc :local-id))))
                          events)
         new-rels   (mapv (fn [r] (tl/new-relation (-> r (assoc :source (uuid-of (:source r))
                                                                :target (uuid-of (:target r))))))
                          relations)
         db (-> db
                (update-timeline (fn [t] (update t :events #(tl/merge-events (or % []) new-events))))
                (update-timeline (fn [t] (update t :relations #(tl/merge-relations (or % []) new-rels))))
                (update-timeline tl/prune-dangling)
                (update-in [:scenario :timeline-run :done-chunks] inc)
                (cond-> (= :no-json status)
                  (assoc-in [:scenario :timeline-run :schema-ignored?] true)))]
     {:db db :dispatch [:timeline/run-next]})))

(rf/reg-event-fx
 :timeline/run-complete
 (fn [{:keys [db]} _]
   (let [model  (get-in db [:ontology :model])
         report (tl/gap-report (:timeline (active-session db)) model)
         post   (tl/gap-count report)
         pre    (get-in db [:scenario :timeline-run :pre-gap])
         cleared (max 0 (- (or pre 0) post))]
     (cond-> {:db (-> db (assoc-in [:scenario :timeline-run :status] :done)
                      (assoc-in [:scenario :timeline-run :ended-at] (.now js/Date))
                      (assoc-in [:scenario :timeline-run :post-gap] post)
                      (assoc-in [:scenario :timeline-run :cleared] cleared))}
       (and pre (pos? cleared))
       (assoc :dispatch [:ui/push-toast {:kind :info
                                         :text (str "Re-run cleared " cleared " gap"
                                                    (when (> cleared 1) "s") ".")}])))))

(rf/reg-event-fx
 :timeline/run-error
 (fn [{:keys [db]} [_ err]]
   {:db (-> db (assoc-in [:scenario :timeline-run :status] :error)
            (assoc-in [:scenario :timeline-run :error] (:message err))
            (assoc-in [:scenario :timeline-run :ended-at] (.now js/Date)))
    :dispatch [:ui/error (str "Timeline pass failed: " (:message err))]}))

(rf/reg-event-fx
 :timeline/cancel
 (fn [{:keys [db]} _]
   {:llm/abort {:key :timeline}
    :db (assoc-in db [:scenario :timeline-run :status] :idle)}))

;; --- event / relation curation (undoable via the scenario stack) ------------

(rf/reg-event-db :timeline/set-event-status hist
  (fn [db [_ id status]] (update-timeline db tl/set-event-status id status)))

(rf/reg-event-db :timeline/set-relation-status hist
  (fn [db [_ id status]] (update-timeline db tl/set-relation-status id status)))

(rf/reg-event-db :timeline/remove-event hist
  (fn [db [_ id]] (update-timeline db tl/remove-event id)))

(rf/reg-event-db :timeline/remove-relation hist
  (fn [db [_ id]] (update-timeline db tl/remove-relation id)))

(rf/reg-event-db :timeline/force-event hist
  (fn [db [_ id node-id]] (update-timeline db tl/force-event id node-id)))

(rf/reg-event-db :timeline/force-relation hist
  (fn [db [_ id type property-id]] (update-timeline db tl/force-relation id type property-id)))

(rf/reg-event-db :timeline/remap-event hist
  ;; Retype an event (keeps status :proposed, re-validates the target).
  (fn [db [_ id node-id]]
    (let [model (get-in db [:ontology :model])]
      (update-timeline db tl/update-event id
                       (fn [e] (-> e (assoc :node-id node-id :nearest nil :why-no-fit nil :status :proposed)
                                   (assoc :flags (if (g/exists? model node-id) #{} #{:invalid-target}))))))))

(rf/reg-event-db :timeline/add-relation hist
  ;; Drag-to-create: a new proposed relation between two events (type from the picker).
  (fn [db [_ source target type]]
    (if (= source target)
      db
      (update-timeline db tl/add-relation {:source source :target target :type type}))))

;; --- gap-report feedback loop -----------------------------------------------

(rf/reg-event-fx
 :timeline/draft-from-gap
 "Jump to the ontology workspace (use case A) with a pre-filled add-node dialog:
  label from the gap, suggested parent from `nearest`, gloss drafted from
  `why-no-fit` (§6.7.4). Directly serves the A↔B switching workflow (§1)."
 (fn [{:keys [db]} [_ {:keys [label nearest why-no-fit]}]]
   {:db (assoc db :workspace :ontology)
    :dispatch [:ui/open-dialog
               {:kind :add-node
                :title "Draft ontology element from gap"
                :label (or label "New occurrent")
                :parent (when (and nearest (g/exists? (get-in db [:ontology :model]) nearest)) nearest)
                :gloss (or why-no-fit "")}]}))

(rf/reg-event-db
 :timeline/create-drafted-node
 [record-history]
 ;; Commit the add-node dialog: create the class, subclass it under the suggested
 ;; parent when present, select it, and reveal it in the ontology outline/canvas.
 (fn [db [_ {:keys [label parent gloss kind]}]]
   (let [model  (get-in db [:ontology :model])
         prefix (or (some->> (keys (:nodes model))
                             (keep #(second (re-matches #"([^:]+:).*" %)))
                             frequencies (sort-by val >) ffirst)
                    "onteater:")
         base   (str prefix (str/replace (or label "NewClass") #"\s+" ""))
         id     (loop [n 1] (let [cand (str base (when (> n 1) n))]
                              (if (g/exists? model cand) (recur (inc n)) cand)))
         parent-mod (:module (g/node model parent))
         node   {:id id :label (or label "New occurrent") :kind (or kind :class)
                 :gloss (when (seq gloss) gloss)
                 :module (or parent-mod "spine") :external? false :provenance [] :props {}}]
     (-> db
         (update-in [:ontology :model]
                    (fn [mm] (cond-> (g/add-node mm node)
                               (and parent (g/exists? mm parent)) (g/add-edge id :subclass-of parent))))
         (assoc-in [:ontology :selection :node] id)
         (update-in [:ontology :view-spec] assoc :mode :neighborhood :focus #{id} :hops 1)))))

;; --- export -----------------------------------------------------------------

(defn- tl-base-name [db]
  (or (some-> (get-in db [:scenario :source-file]) (str/replace #"\.[^.]+$" "")) "timeline"))

(rf/reg-event-fx
 :timeline/export-svg
 (fn [{:keys [db]} _]
   {:io/export-svg {:filename (str (tl-base-name db) "-timeline.svg")
                    :selector ".tl-canvas"
                    :caption [(str "Timeline — " (get-in db [:ontology :model :meta :title] "scenario"))
                              (subs (.toISOString (js/Date.)) 0 10)]
                    :dark? (resolved-dark? (get-in db [:ui :theme]))}}))

(rf/reg-event-fx
 :timeline/export-png
 (fn [{:keys [db]} _]
   {:io/export-png {:filename (str (tl-base-name db) "-timeline.png") :scale 2
                    :selector ".tl-canvas"
                    :caption [(str "Timeline — " (get-in db [:ontology :model :meta :title] "scenario"))
                              (subs (.toISOString (js/Date.)) 0 10)]
                    :dark? (resolved-dark? (get-in db [:ui :theme]))
                    :on-error [:ui/error]}}))

(rf/reg-event-fx
 :timeline/export-gaps-md
 (fn [{:keys [db]} _]
   (let [model  (get-in db [:ontology :model])
         sess   (active-session db)
         report (tl/gap-report (:timeline sess) model)
         title  (get-in sess [:scenario :title] "scenario")]
     {:io/download-text {:filename (str (tl-base-name db) "-gaps.md")
                         :mime "text/markdown"
                         :text (tl/gap-report-markdown report title)}})))
