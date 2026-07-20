(ns onteater.events.ontology
  "re-frame events for the ontology workspace: opening/loading files, selection,
  and view-spec navigation (the anti-clutter focus/subtree/module/collapse
  transitions). Requiring the format adapter namespaces here is what
  registers them in the format registry.

  Editing events (inspector/canvas mutation, undo/redo) arrive in Milestone 3; the
  history interceptor they depend on is scaffolded in `onteater.events.history`."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.db :as db]
            [onteater.model.graph :as g]
            [onteater.format.core :as fmt]
            [onteater.format.geo]        ; registers :geo-reference-json
            [onteater.format.native]     ; registers :onteater-native
            [onteater.format.owl]        ; registers :owl2-turtle
            [onteater.io.file]))         ; registers :io/pick-file, :io/download-text

;; --- open / load ------------------------------------------------------------

;; Trigger the OS file picker (FS Access where available, retaining a handle for
;; in-place Save); the chosen file comes back to :ontology/file-loaded.
(rf/reg-event-fx
 :ontology/open
 (fn [_ _]
   {:io/open-file {:on-loaded [:ontology/file-loaded]
                   :on-error  [:ui/error]
                   ;; Allow both the JSON families (geo/native) and OWL Turtle;
                   ;; the format is auto-detected from content, not the extension.
                   :accept {:description "Ontology"
                            :mime "application/json"
                            :extensions [".json" ".onteater.json" ".ttl" ".owl"]}}}))

;; Parse loaded text via the format registry (auto-detected). On success install
;; the model; on failure surface a readable error and keep the app usable.
(rf/reg-event-fx
 :ontology/file-loaded
 (fn [_ [_ filename text handle]]
   (try
     (let [model  (fmt/open text)
           format (get-in model [:meta :format])]
       {:dispatch [:ontology/load model {:name filename :format format :handle handle}]})
     (catch :default e
       {:dispatch [:ui/error (or (:message (ex-data e)) (.-message e)
                                 "Could not parse this file.")]}))))

(rf/reg-event-fx
 :ontology/load
 "Install a parsed model as the active ontology and reset the view to the
  module overview. Drops the previous ontology's briefing and asks the mapping
  layer to restore any persisted briefing for THIS ontology (keyed by
  title+version in IndexedDB)."
 (fn [{:keys [db]} [_ model file-info]]
   {:db (-> db
            (assoc-in [:ontology :model] model)
            (assoc-in [:ontology :file] (merge {:handle nil :hash nil} file-info))
            (assoc-in [:ontology :dirty?] false)
            (assoc-in [:ontology :selection] {:node nil :edge nil :pinned #{}})
            (assoc-in [:ontology :view-spec] db/default-view-spec)
            (assoc-in [:ontology :outline] {:query "" :filters {} :expanded #{}})
            (assoc-in [:ontology :breadcrumbs] [])
            (assoc-in [:ontology :undo] [])
            (assoc-in [:ontology :redo] [])
            (update :ontology dissoc :briefing :briefing-run)
            (assoc :workspace :ontology))
    :dispatch [:briefing/load-persisted]}))

;; --- selection --------------------------------------------------------------

(rf/reg-event-db
 :ontology/select-node
 (fn [db [_ id]] (-> db (assoc-in [:ontology :selection :node] id)
                     (assoc-in [:ontology :selection :edge] nil))))

(rf/reg-event-db
 :ontology/select-edge
 (fn [db [_ id]] (-> db (assoc-in [:ontology :selection :edge] id)
                     (assoc-in [:ontology :selection :node] nil))))

(rf/reg-event-db
 :ontology/clear-selection
 (fn [db _] (-> db (assoc-in [:ontology :selection :node] nil)
                (assoc-in [:ontology :selection :edge] nil))))

(rf/reg-event-db
 :ontology/toggle-pin
 (fn [db [_ id]]
   (update-in db [:ontology :selection :pinned]
              (fn [s] (let [s (or s #{})] (if (s id) (disj s id) (conj s id)))))))

;; --- view-spec navigation ---------------------------------------------------

(defn- describe-spec
  "A short human label for a view-spec, for the breadcrumb trail."
  [db spec]
  (let [model (get-in db [:ontology :model])
        focus (first (:focus spec))
        lbl   (or (:label (g/node model focus)) focus)]
    (case (:mode spec)
      :overview     "Overview"
      :neighborhood (str "◎ " lbl)
      :subtree      (str "⊳ " lbl)
      :module       (str "▤ " (or (get-in model [:groups focus :label]) focus))
      :custom       (str "… " (or lbl "selection"))
      "View")))

(defn- push-breadcrumb
  "Snapshot the current view-spec onto the breadcrumb stack before navigating."
  [db]
  (let [spec (get-in db [:ontology :view-spec])]
    (update-in db [:ontology :breadcrumbs] (fnil conj [])
               {:label (describe-spec db spec) :spec spec})))

;; --- keeping the outline tree in sync with graph navigation ----------------
;; Focusing/expanding in the graph reveals the target in the tree by expanding the
;; groups on the path to it. This only UNIONS into the expanded set — nothing is
;; ever removed here — so navigating 'back' never collapses the tree.

(defn- reveal-node-in-tree [db id]
  (let [ids (g/reveal-group-ids (get-in db [:ontology :model]) id)]
    (update-in db [:ontology :outline :expanded] (fnil into #{}) ids)))

(defn- reveal-group-in-tree [db gid]
  (let [ids (g/group-ancestors (get-in db [:ontology :model]) gid)]
    (update-in db [:ontology :outline :expanded] (fnil into #{}) ids)))

(rf/reg-event-db
 :view/focus-node
 "Double-click / 'focus here': show `id` as an n-hop neighbourhood, remembering
  where we came from, and reveal it in the outline tree."
 (fn [db [_ id]]
   (let [hops (get-in db [:ontology :view-spec :hops] 1)]
     (-> (push-breadcrumb db)
         (update-in [:ontology :view-spec] assoc :mode :neighborhood :focus #{id} :hops hops)
         (assoc-in [:ontology :selection :node] id)
         (reveal-node-in-tree id)))))

(rf/reg-event-db
 :view/expand-group
 "Expand a collapsed module/section meta-node into its members (:module mode), and
  expand it in the outline tree too."
 (fn [db [_ group-id]]
   (-> (push-breadcrumb db)
       (update-in [:ontology :view-spec] assoc :mode :module :focus #{group-id})
       (reveal-group-in-tree group-id))))

(rf/reg-event-db
 :view/show-subtree
 (fn [db [_ id]]
   (-> (push-breadcrumb db)
       (update-in [:ontology :view-spec] assoc :mode :subtree :focus #{id})
       (assoc-in [:ontology :selection :node] id)
       (reveal-node-in-tree id))))

(rf/reg-event-db
 :view/show-ancestors
 (fn [db [_ id]]
   (let [model (get-in db [:ontology :model])
         anc   (g/ancestors model id)]
     (-> (push-breadcrumb db)
         (update-in [:ontology :view-spec] assoc :mode :custom :focus anc)
         (assoc-in [:ontology :selection :node] id)
         (reveal-node-in-tree id)))))

(rf/reg-event-db
 :view/set-layout
 "Switch layout without disturbing the current focus (acceptance: layout switch
  preserves focus)."
 (fn [db [_ layout]] (assoc-in db [:ontology :view-spec :layout] layout)))

(rf/reg-event-db
 :view/set-hops
 (fn [db [_ n]] (assoc-in db [:ontology :view-spec :hops] (max 1 (min 3 n)))))

(rf/reg-event-db
 :view/toggle-collapse
 (fn [db [_ id]]
   (update-in db [:ontology :view-spec :collapsed]
              (fn [s] (let [s (or s #{})] (if (s id) (disj s id) (conj s id)))))))

(rf/reg-event-db
 :view/toggle-edge-type
 (fn [db [_ t]]
   (update-in db [:ontology :view-spec :edge-types]
              (fn [s] (if (s t) (disj s t) (conj s t))))))

(rf/reg-event-db
 :view/toggle-kind
 (fn [db [_ k]]
   (update-in db [:ontology :view-spec :kinds]
              (fn [s] (if (s k) (disj s k) (conj s k))))))

(rf/reg-event-db
 :view/reset-overview
 (fn [db _]
   (-> db
       (update-in [:ontology :view-spec] merge {:mode :overview :focus #{} :collapsed #{}})
       (assoc-in [:ontology :breadcrumbs] []))))

(rf/reg-event-db
 :view/breadcrumb-back
 "Pop the breadcrumb stack and restore that view-spec."
 (fn [db _]
   (let [crumbs (get-in db [:ontology :breadcrumbs])]
     (if (empty? crumbs)
       db
       (-> db
           (assoc-in [:ontology :view-spec] (:spec (peek crumbs)))
           (assoc-in [:ontology :breadcrumbs] (pop crumbs)))))))

(rf/reg-event-db
 :view/breadcrumb-to
 "Jump to breadcrumb at index `i`, truncating the stack."
 (fn [db [_ i]]
   (let [crumbs (get-in db [:ontology :breadcrumbs])]
     (if (< i (count crumbs))
       (-> db
           (assoc-in [:ontology :view-spec] (:spec (nth crumbs i)))
           (assoc-in [:ontology :breadcrumbs] (subvec crumbs 0 i)))
       db))))

;; --- outline / search -------------------------------------------------------

(rf/reg-event-db
 :outline/set-query
 (fn [db [_ q]] (assoc-in db [:ontology :outline :query] q)))

(rf/reg-event-db
 :outline/focus-search-result
 "Clicking a search hit selects it, focuses its neighbourhood, and reveals it in
  the tree."
 (fn [db [_ id]]
   (let [hops (get-in db [:ontology :view-spec :hops] 1)]
     (-> (push-breadcrumb db)
         (update-in [:ontology :view-spec] assoc :mode :neighborhood :focus #{id} :hops hops)
         (assoc-in [:ontology :selection :node] id)
         (reveal-node-in-tree id)))))

;; --- outline tree expand/collapse (now app-db state, so graph nav can drive it) --

(rf/reg-event-db
 :outline/toggle-expand
 (fn [db [_ gid]]
   (update-in db [:ontology :outline :expanded]
              (fn [s] (let [s (or s #{})] (if (s gid) (disj s gid) (conj s gid)))))))

(rf/reg-event-db
 :outline/set-expanded
 (fn [db [_ gids]] (assoc-in db [:ontology :outline :expanded] (set gids))))

(rf/reg-event-db
 :outline/collapse-all
 (fn [db _] (assoc-in db [:ontology :outline :expanded] #{})))

;; --- error toast helper -----------------------------------------------------

(rf/reg-event-fx
 :ui/error
 (fn [_ [_ msg]]
   {:dispatch [:ui/push-toast {:kind :error :text (str msg)}]}))
