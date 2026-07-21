(ns onteater.subs.ontology
  "Subscriptions for the ontology workspace — the read side that materialises the
  bounded visible graph, the selection, the outline tree, and search results from
  app-db + the pure model/view functions."
  (:require [re-frame.core :as rf]
            [onteater.model.graph :as g]
            [onteater.model.view :as view]
            [onteater.viz.common :as viz]))

;; --- view-spec + the bounded visible graph ---------------------------------

(rf/reg-sub :view/spec (fn [db _] (get-in db [:ontology :view-spec])))
(rf/reg-sub :view/layout (fn [db _] (get-in db [:ontology :view-spec :layout])))
(rf/reg-sub :view/breadcrumbs (fn [db _] (get-in db [:ontology :breadcrumbs])))
(rf/reg-sub :ontology/force-opts (fn [db _] (get-in db [:ontology :force-opts])))

;; The exact nodes/edges the canvas should draw for the current model + view-spec
;; — the anti-clutter guarantee, computed by the pure view model.
(rf/reg-sub
 :view/visible-graph
 :<- [:ontology/model]
 :<- [:view/spec]
 (fn [[model spec] _]
   (view/visible-graph model spec)))

;; The categorical module order derived from the loaded ontology. Drives per-module
;; colour in the graph, legend, mapping board, and scenario highlights, so the
;; palette adapts to whatever ontology is open rather than a fixed module taxonomy.
(rf/reg-sub
 :ontology/module-order
 :<- [:ontology/model]
 (fn [model _] (when model (viz/ordered-modules model))))

;; --- selection --------------------------------------------------------------

(rf/reg-sub :ontology/selection (fn [db _] (get-in db [:ontology :selection])))

(rf/reg-sub
 :ontology/selected-node
 :<- [:ontology/model]
 :<- [:ontology/selection]
 (fn [[model sel] _] (when (:node sel) (g/node model (:node sel)))))

;; Edges incident to the selected node, resolved to {:edge :other-node :dir}.
(rf/reg-sub
 :ontology/selected-node-edges
 :<- [:ontology/model]
 :<- [:ontology/selection]
 (fn [[model sel] _]
   (when-let [id (:node sel)]
     (for [e (g/edges-touching model id)]
       {:edge e
        :dir (if (= (:source e) id) :out :in)
        :other (let [oid (if (= (:source e) id) (:target e) (:source e))]
                 (or (g/node model oid) {:id oid :label oid}))}))))

;; --- stats + validation -----------------------------------------------------

(rf/reg-sub
 :ontology/stats
 :<- [:ontology/model]
 (fn [model _] (when model (g/stats model))))

(rf/reg-sub
 :ontology/validation
 :<- [:ontology/model]
 (fn [model _] (when model (g/validate model))))

;; --- outline + search -------------------------------------------------------

(rf/reg-sub :outline/query (fn [db _] (get-in db [:ontology :outline :query])))
(rf/reg-sub :outline/expanded (fn [db _] (get-in db [:ontology :outline :expanded])))

(rf/reg-sub
 :outline/search-results
 :<- [:ontology/model]
 :<- [:outline/query]
 (fn [[model q] _]
   (when (and model (not-empty q))
     (mapv #(g/node model %) (g/search model q 40)))))

;; Parameterized search — the query is passed in the subscription vector, used by
;; node pickers (mapping Force, chat targets).
(rf/reg-sub
 :outline/search-results-for
 :<- [:ontology/model]
 (fn [model [_ q]]
   (when (and model (not-empty q))
     (mapv #(g/node model %) (g/search model q 12)))))

;; The outline hierarchy: top-level groups, each with nested subgroups and member
;; node summaries, for the left-hand tree pane.
(rf/reg-sub
 :outline/tree
 :<- [:ontology/model]
 (fn [model _]
   (when model
     (let [groups (:groups model)
           node-summary (fn [id] (when-let [n (g/node model id)]
                                   {:id id :label (:label n) :kind (:kind n)
                                    :external? (:external? n)}))
           ;; A node in the geo model is a member of BOTH its module and its
           ;; subsection/family, so a group's own row must show only the members
           ;; that do NOT live in one of its subgroups — otherwise every node
           ;; would appear twice (flat under the module and inside its family).
           sub-member-ids
           (fn sub-member-ids [gid]
             (let [grp (get groups gid)]
               (into (set (:members grp))
                     (mapcat sub-member-ids)
                     (:subgroups grp))))
           build (fn build [gid]
                   (let [grp     (get groups gid)
                         in-subs (into #{} (mapcat sub-member-ids) (:subgroups grp))
                         direct  (remove in-subs (:members grp))]
                     {:id gid
                      :label (:label grp)
                      :kind (:kind grp)
                      :count (count (:members grp))       ; total, for the badge
                      :members (->> direct (keep node-summary) (sort-by :label) vec)
                      :subgroups (->> (:subgroups grp) (map build) (sort-by :label) vec)}))]
       (->> (vals groups)
            (filter #(nil? (:parent %)))
            (map (comp build :id))
            (sort-by :label)
            vec)))))

;; --- documentation sections (Docs center-pane view) --------------------------

(rf/reg-sub
 :ontology/center-view
 (fn [db _] (get-in db [:ontology :center-view] :graph)))

(rf/reg-sub :docs/expanded (fn [db _] (get-in db [:ontology :docs-ui :expanded] #{})))

;; Docs sections grouped by their top-level key, in file order. A group whose
;; sections sit one level down (path length 2) came from a mixed node/prose
;; object — flagged :mixed? so the UI can hint that its classes are edited in
;; the Graph view.
(rf/reg-sub
 :docs/groups
 :<- [:ontology/model]
 (fn [model _]
   (when model
     (->> (:docs model)
          (partition-by (comp first :path))
          (mapv (fn [sections]
                  {:key      (first (:path (first sections)))
                   :mixed?   (some #(= 2 (count (:path %))) sections)
                   :sections (vec sections)}))))))

;; Only formats whose serializer writes docs back may grow new sections (an OWL
;; save would silently drop them).
(rf/reg-sub
 :docs/editable?
 :<- [:ontology/model]
 (fn [model _]
   (contains? #{:geo-reference-json :onteater-native}
              (get-in model [:meta :format]))))

;; --- theme resolution -------------------------------------------------------

(rf/reg-sub :ui/theme-pref (fn [db _] (get-in db [:ui :theme])))
