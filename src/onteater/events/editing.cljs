(ns onteater.events.editing
  "Ontology mutation events. Every handler that changes
  the model carries the `record-history` interceptor, so undo/redo and dirty
  tracking are automatic and uniform. Handlers delegate the actual transformation
  to the pure `onteater.model.graph` operations."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.model.graph :as g]
            [onteater.events.history :refer [record-history]]))

(def hist [record-history])

;; --- node field edits -------------------------------------------------------

(rf/reg-event-db
 :ontology/update-node-field
 hist
 (fn [db [_ id k v]]
   (update-in db [:ontology :model] g/set-node-attr id k v)))

(rf/reg-event-db
 :ontology/set-prop
 hist
 (fn [db [_ id prop-key prop-val]]
   (update-in db [:ontology :model] g/update-node id
              update :props assoc prop-key prop-val)))

(rf/reg-event-db
 :ontology/remove-prop
 hist
 (fn [db [_ id prop-key]]
   (update-in db [:ontology :model] g/update-node id
              update :props dissoc prop-key)))

(rf/reg-event-db
 :ontology/rename-prop
 hist
 (fn [db [_ id old-key new-key]]
   (update-in db [:ontology :model] g/update-node id
              update :props (fn [p] (-> p (assoc new-key (get p old-key)) (dissoc old-key))))))

(rf/reg-event-db
 :ontology/rename-node
 hist
 (fn [db [_ old-id new-id]]
   (-> db
       (update-in [:ontology :model] g/rename-id old-id new-id)
       (assoc-in [:ontology :selection :node] new-id))))

;; --- structural edits -------------------------------------------------------

(defn- free-id
  "Pick an id not already taken, based on the model's dominant namespace prefix."
  [model base]
  (let [prefix (or (some->> (keys (:nodes model))
                            (keep #(second (re-matches #"([^:]+:).*" %)))
                            frequencies (sort-by val >) ffirst)
                   "onteater:")]
    (loop [n 1]
      (let [id (str prefix base (when (> n 1) n))]
        (if (g/exists? model id) (recur (inc n)) id)))))

(rf/reg-event-db
 :ontology/add-node
 hist
 (fn [db [_ {:keys [kind label container]}]]
   (let [model (get-in db [:ontology :model])
         id    (free-id model (or (some-> label (str/replace #"\s+" "")) "NewClass"))
         node  {:id id :label (or label "New class") :kind (or kind :class)
                :gloss nil :module (or (some-> container second) "spine")
                :external? false :provenance []
                :props (if container {"__container" container} {})}]
     (-> db
         (update-in [:ontology :model] g/add-node node)
         (assoc-in [:ontology :selection :node] id)))))

(rf/reg-event-db
 :ontology/add-edge
 hist
 (fn [db [_ source type target]]
   (update-in db [:ontology :model] g/add-edge source type target)))

(rf/reg-event-db
 :ontology/add-child
 hist
 (fn [db [_ parent-id]]
   (let [model (get-in db [:ontology :model])
         id    (free-id model "Subclass")
         node  {:id id :label "New subclass" :kind :class :gloss nil
                :module (or (:module (g/node model parent-id)) "spine")
                :external? false :provenance [] :props {}}]
     (-> db
         (update-in [:ontology :model]
                    #(-> % (g/add-node node) (g/add-edge id :subclass-of parent-id)))
         (assoc-in [:ontology :selection :node] id)))))

(rf/reg-event-db
 :ontology/remove-edge
 hist
 (fn [db [_ eid]]
   (update-in db [:ontology :model] g/remove-edge eid)))

(rf/reg-event-fx
 :ontology/request-delete-node
 "Open a confirmation dialog previewing the cascade before deleting."
 (fn [{:keys [db]} [_ id]]
   (let [model (get-in db [:ontology :model])
         {:keys [edge-count]} (g/remove-node-preview model id)
         label (or (:label (g/node model id)) id)]
     {:dispatch [:ui/open-dialog
                 {:kind :confirm
                  :title "Delete node"
                  :message (str "Delete “" label "”"
                                (when (pos? edge-count)
                                  (str " and its " edge-count " relation"
                                       (when (> edge-count 1) "s")))
                                "? This can be undone.")
                  :confirm-label "Delete"
                  :danger? true
                  :on-confirm [:ontology/delete-node id]}]})))

(rf/reg-event-db
 :ontology/delete-node
 hist
 (fn [db [_ id]]
   (-> db
       (update-in [:ontology :model] g/remove-node id)
       (assoc-in [:ontology :selection :node] nil))))
