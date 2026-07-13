(ns onteater.model.view
  "The anti-clutter view model — the single most important UX rule in Onteater:
  the canvas NEVER shows the whole ontology. It renders a *view
  specification* into a small, bounded visible subgraph. This namespace turns a
  view-spec + model into exactly the nodes and edges the canvas should draw, as a
  pure, unit-tested function so the layout code downstream is trivial.

  A view-spec (see `onteater.db/default-view-spec`):

    {:mode       :overview | :neighborhood | :subtree | :module | :custom
     :focus      #{node-or-group-ids}
     :hops       1-3
     :edge-types #{:subclass-of :domain ...}
     :kinds      #{:class :property ...}
     :collapsed  #{ids}         ; hide the subclass-descendants of these
     :layout     :force | :tree | :radial | :cluster
     :roots      #{ids}}

  `visible-graph` returns:

    {:nodes     [node-or-meta-node ...]   ; may include synthetic :meta? bubbles
     :edges     [edge ...]                ; induced among visible nodes
     :truncated {:hidden n :reason ...}   ; when a cap trims the view (never silent)
     :mode      kw}

  In :overview mode the returned nodes are synthetic *meta-nodes* — one bubble per
  top-level section/module with a member count — so a large ontology opens as a
  handful of bubbles the user expands on demand."
  (:refer-clojure :exclude [ancestors parents])
  (:require [onteater.model.graph :as g]))

;; A hard ceiling on how many real nodes the canvas will draw at once. The view
;; model is supposed to keep the visible set small; if a query still exceeds this
;; we trim and report it (no silent truncation).
(def ^:const max-visible-nodes 600)

(defn- kind-ok? [view-spec node]
  (or (:meta? node)
      (contains? (:kinds view-spec) (:kind node))))

(defn- collapsed-hidden
  "Set of node ids hidden because they are a subclass-descendant of a collapsed
  node (the collapsed node itself stays visible; its descendants are hidden)."
  [model collapsed]
  (into #{}
        (mapcat (fn [cid] (disj (g/subtree model cid) cid)))
        collapsed))

(defn- meta-node
  "A synthetic bubble representing a whole group/section in :overview mode."
  [group]
  {:id       (str "group:" (:id group))
   :group-id (:id group)
   :label    (:label group)
   :kind     :group
   :meta?    true
   :count    (count (:members group))
   :module   (:id group)})

(defn overview-graph
  "Top-level groups (parent = nil) as meta-node bubbles. Inter-group edges are
  aggregated from subclass edges that cross group boundaries."
  [model]
  (let [top-groups (filter #(nil? (:parent %)) (g/groups model))
        metas      (mapv meta-node top-groups)
        ;; map each node id -> its top-level group id (for aggregating edges)
        node->group (into {}
                          (for [g' top-groups
                                mid (:members g')]
                            [mid (:id g')]))
        agg (reduce (fn [acc e]
                      (let [gs (node->group (:source e))
                            gt (node->group (:target e))]
                        (if (and gs gt (not= gs gt) (= :subclass-of (:type e)))
                          (conj acc #{gs gt})
                          acc)))
                    #{}
                    (g/edges model))
        edges (for [pair agg
                    :let [[a b] (vec pair)]]
                {:id (str "grouplink:" a "|" b)
                 :source (str "group:" a) :target (str "group:" b)
                 :type :group-link})]
    {:nodes metas :edges (vec edges) :truncated nil :mode :overview}))

(defn- induced-edges
  "Edges of allowed types whose endpoints are both in `id-set`."
  [model id-set edge-types]
  (into []
        (comp (filter #(contains? edge-types (:type %)))
              (filter #(and (id-set (:source %)) (id-set (:target %)))))
        (g/edges model)))

(defn- assemble
  "Given a set of candidate node ids, apply kind/collapse filtering and the cap,
  and return the visible-graph map."
  [model view-spec candidate-ids mode]
  (let [hidden   (collapsed-hidden model (:collapsed view-spec))
        kept-ids (into #{}
                       (comp (remove hidden)
                             (filter (fn [id] (when-let [n (g/node model id)]
                                                (kind-ok? view-spec n)))))
                       candidate-ids)
        capped?  (> (count kept-ids) max-visible-nodes)
        final-ids (if capped? (set (take max-visible-nodes kept-ids)) kept-ids)
        nodes    (keep #(g/node model %) final-ids)
        ;; annotate collapsed nodes with their hidden-descendant count for the badge
        nodes    (mapv (fn [n]
                         (if (contains? (:collapsed view-spec) (:id n))
                           (assoc n :collapsed? true
                                  :hidden-count (dec (count (g/subtree model (:id n)))))
                           n))
                       nodes)]
    {:nodes nodes
     :edges (induced-edges model final-ids (:edge-types view-spec))
     :truncated (when capped?
                  {:hidden (- (count kept-ids) max-visible-nodes)
                   :reason :node-cap})
     :mode mode}))

(defn- group-member-ids
  "All node ids belonging to any of `group-ids` (including nested subgroups)."
  [model group-ids]
  (loop [pending (vec group-ids) seen #{} members #{}]
    (if (empty? pending)
      members
      (let [gid (peek pending) pending (pop pending)]
        (if (seen gid)
          (recur pending seen members)
          (let [grp (get-in model [:groups gid])]
            (recur (into pending (:subgroups grp))
                   (conj seen gid)
                   (into members (:members grp)))))))))

(defn visible-graph
  "Turn `view-spec` into the bounded set of nodes/edges the canvas should draw."
  [model view-spec]
  (if (or (nil? model) (empty? (:nodes model)))
    {:nodes [] :edges [] :truncated nil :mode (:mode view-spec)}
    (case (:mode view-spec)
      :overview (overview-graph model)

      :neighborhood
      (let [nb (g/neighborhood model (:focus view-spec) (:hops view-spec) (:edge-types view-spec))]
        (assemble model view-spec (:nodes nb) :neighborhood))

      :subtree
      (let [ids (into #{} (mapcat #(g/subtree model %)) (:focus view-spec))]
        (assemble model view-spec ids :subtree))

      :module
      (let [ids (group-member-ids model (:focus view-spec))]
        (assemble model view-spec ids :module))

      ;; :custom — focus set is taken literally (used by search "show these")
      (assemble model view-spec (:focus view-spec) (:mode view-spec)))))

(defn focus-node
  "Return a view-spec that focuses `node-id` as an n-hop neighbourhood — the
  double-click / 'focus here' action."
  [view-spec node-id hops]
  (assoc view-spec :mode :neighborhood :focus #{node-id} :hops hops))
