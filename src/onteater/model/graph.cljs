(ns onteater.model.graph
  "The canonical, format-neutral ontology model and pure operations over it.

  Everything the UI and the LLM see goes through this model, never a raw file
  format. Keeping it format-neutral is what lets Onteater open more than the one
  sample file. Keeping it *pure* (no DOM, no re-frame, only data) is
  what makes the whole domain layer unit-testable headlessly.

  ## Model shape

    {:meta   {:title str :version str :namespaces {str str} :format kw ...}
     :nodes  {node-id Node}
     :edges  {edge-id Edge}
     :groups {group-id Group}
     :residual any        ; adapter-owned structural skeleton, preserved verbatim
     :order   [node-id]}  ; optional stable display/serialisation order hint

  ### Node

    {:id         \"geo:Leverage\"       ; string, unique across the model
     :label      \"Leverage\"
     :kind       :class                ; :class | :property | :individual | :value
     :gloss      \"...\"                ; optional documentation string, may be nil
     :props      {..}                  ; open map of any extra attributes
     :module     \"POW\"               ; grouping tag, or nil
     :external?  false                 ; true = referenced-but-undefined stub (e.g. bfo:*)
     :provenance [[path-seg ...] ...]} ; adapter's paths back into the source (may be empty)

  ### Edge

    {:id     edge-id                   ; generated, stable (see `edge-id`)
     :source node-id
     :target node-id
     :type   :subclass-of              ; :subclass-of | :domain | :range |
                                       ; :module-membership | any relation keyword
     :props  {..}}

  ### Group (nestable — modules -> subsections/families)

    {:id      group-id
     :label   \"Module POW — Power\"
     :kind    :module | :subsection | :family | :section | :custom
     :parent  group-id|nil
     :subgroups [group-id ...]
     :members   [node-id ...]}         ; nodes that belong directly to this group

  All functions here are pure: (model, args) -> new-model or a query result. Model
  mutation for undo/redo is handled one level up by a re-frame interceptor that
  snapshots the whole model — these functions never mutate in place."
  ;; These names shadow cljs.core intentionally — here they mean model queries
  ;; (subclass-of ancestors/parents, node existence), not the core hierarchy fns.
  (:refer-clojure :exclude [ancestors parents exists?])
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Construction
;; ---------------------------------------------------------------------------

(defn empty-model
  "Return a fresh, empty canonical model. `meta` merges into the default meta map."
  ([] (empty-model {}))
  ([meta]
   {:meta   (merge {:title "Untitled ontology" :namespaces {}} meta)
    :nodes  {}
    :edges  {}
    :groups {}
    :residual nil
    :order  []}))

(defn edge-id
  "Deterministic, stable id for an edge, derived from its endpoints and type so
  the same logical edge always gets the same id (idempotent re-derivation, clean
  diffs). Not a random uuid on purpose."
  [source type target]
  (str (name type) "␟" source "␟" target))

(defn make-edge
  "Construct an edge map with a derived stable id."
  ([source type target] (make-edge source type target {}))
  ([source type target props]
   {:id (edge-id source type target) :source source :target target
    :type type :props props}))

;; ---------------------------------------------------------------------------
;; Basic accessors
;; ---------------------------------------------------------------------------

(defn node   [model id] (get-in model [:nodes id]))
(defn nodes  [model]    (vals (:nodes model)))
(defn edges  [model]    (vals (:edges model)))
(defn groups [model]    (vals (:groups model)))
(defn node-count [model] (count (:nodes model)))
(defn edge-count [model] (count (:edges model)))

(defn exists? [model id] (contains? (:nodes model) id))

;; ---------------------------------------------------------------------------
;; CRUD — nodes
;; ---------------------------------------------------------------------------

(defn add-node
  "Add (or replace) a node. Also appends it to `:order` if new so freshly created
  nodes have a stable spot in outline/serialisation order."
  [model {:keys [id] :as node}]
  (let [new? (not (exists? model id))]
    (cond-> (assoc-in model [:nodes id] node)
      new? (update :order (fnil conj []) id))))

(defn update-node
  "Apply `f` (a map->map fn) to the node with `id`. No-op if it does not exist."
  [model id f & args]
  (if (exists? model id)
    (apply update-in model [:nodes id] f args)
    model))

(defn set-node-attr
  "Convenience: set a single attribute on a node."
  [model id k v]
  (update-node model id assoc k v))

(defn edges-touching
  "All edges with `id` as source or target."
  [model id]
  (filter #(or (= id (:source %)) (= id (:target %))) (edges model)))

(defn remove-node
  "Remove a node and cascade-delete every edge touching it. Returns the new model.
  Callers that need a confirmation preview should first call `remove-node-preview`."
  [model id]
  (let [touching (map :id (edges-touching model id))]
    (-> model
        (update :nodes dissoc id)
        (update :edges #(apply dissoc % touching))
        (update :order (fn [o] (vec (remove #{id} o))))
        ;; Drop from any group membership.
        (update :groups
                (fn [gs]
                  (reduce-kv (fn [m gid g]
                               (assoc m gid (update g :members (fn [ms] (vec (remove #{id} (or ms [])))))))
                             {} gs))))))

(defn remove-node-preview
  "Return data describing what `remove-node` would cascade, for a confirmation
  dialog: {:node id :edges [edge ...] :edge-count n}."
  [model id]
  (let [touching (edges-touching model id)]
    {:node id :edges (vec touching) :edge-count (count touching)}))

(defn rename-id
  "Rename node `old-id` to `new-id`, rewriting every reference: the node's own :id,
  all edges' :source/:target (and their derived ids), group membership, and order.
  No-op if `old-id` is absent or `new-id` already taken."
  [model old-id new-id]
  (if (or (not (exists? model old-id)) (exists? model new-id) (= old-id new-id))
    model
    (let [n   (assoc (node model old-id) :id new-id)
          rewrite-edge (fn [e]
                         (let [s (if (= (:source e) old-id) new-id (:source e))
                               t (if (= (:target e) old-id) new-id (:target e))]
                           (make-edge s (:type e) t (:props e))))
          new-edges (into {} (map (fn [e] (let [e' (rewrite-edge e)] [(:id e') e']))) (edges model))]
      (-> model
          (update :nodes dissoc old-id)
          (assoc-in [:nodes new-id] n)
          (assoc :edges new-edges)
          (update :order (fn [o] (mapv #(if (= % old-id) new-id %) o)))
          (update :groups
                  (fn [gs]
                    (reduce-kv (fn [m gid g]
                                 (assoc m gid (update g :members
                                                      (fn [ms] (mapv #(if (= % old-id) new-id %) (or ms []))))))
                               {} gs)))))))

;; ---------------------------------------------------------------------------
;; CRUD — edges
;; ---------------------------------------------------------------------------

(defn add-edge
  "Add an edge. Accepts either a full edge map or (source type target)."
  ([model edge] (assoc-in model [:edges (:id edge)] edge))
  ([model source type target]
   (let [e (make-edge source type target)]
     (assoc-in model [:edges (:id e)] e))))

(defn remove-edge [model eid] (update model :edges dissoc eid))

;; ---------------------------------------------------------------------------
;; Queries
;; ---------------------------------------------------------------------------

(defn out-edges [model id] (filter #(= id (:source %)) (edges model)))
(defn in-edges  [model id] (filter #(= id (:target %)) (edges model)))

(defn neighbors
  "Set of node ids adjacent to `id` over edges whose :type is in `edge-types`
  (all types if `edge-types` is nil). Direction-agnostic."
  ([model id] (neighbors model id nil))
  ([model id edge-types]
   (let [keep? (if edge-types #(contains? edge-types (:type %)) (constantly true))]
     (into #{}
           (comp (filter keep?)
                 (mapcat (fn [e] [(:source e) (:target e)]))
                 (remove #{id}))
           (edges-touching model id)))))

(defn neighborhood
  "The n-hop neighbourhood subgraph around one or more `focus` ids: the set of
  node ids reachable within `hops` steps over edges of the allowed `edge-types`.
  Returns `{:nodes #{ids} :edges #{edge-ids}}`. This is the workhorse behind the
  anti-clutter focus mode."
  [model focus hops edge-types]
  (let [focus (set focus)]
    (loop [frontier focus seen focus hop hops]
      (if (or (<= hop 0) (empty? frontier))
        ;; collect induced edges among `seen`
        (let [edge-ok? (if edge-types #(contains? edge-types (:type %)) (constantly true))
              induced  (into #{} (comp (filter edge-ok?)
                                       (filter #(and (seen (:source %)) (seen (:target %))))
                                       (map :id))
                             (edges model))]
          {:nodes seen :edges induced})
        (let [nxt (into #{} (mapcat #(neighbors model % edge-types)) frontier)
              new (remove seen nxt)]
          (recur (set new) (into seen new) (dec hop)))))))

(defn parents
  "Direct :subclass-of parents of `id` (nodes it is a subclass of)."
  [model id]
  (into #{} (comp (filter #(= :subclass-of (:type %)))
                  (filter #(= id (:source %)))
                  (map :target))
        (edges model)))

(defn children
  "Direct :subclass-of children of `id`."
  [model id]
  (into #{} (comp (filter #(= :subclass-of (:type %)))
                  (filter #(= id (:target %)))
                  (map :source))
        (edges model)))

(defn subtree
  "Transitive :subclass-of descendants of `root` (inclusive). Cycle-safe."
  [model root]
  (loop [stack [root] seen #{}]
    (if (empty? stack)
      seen
      (let [x (peek stack) stack (pop stack)]
        (if (seen x)
          (recur stack seen)
          (recur (into stack (children model x)) (conj seen x)))))))

(defn ancestors
  "Transitive :subclass-of ancestors of `node-id` (inclusive). Cycle-safe."
  [model node-id]
  (loop [stack [node-id] seen #{}]
    (if (empty? stack)
      seen
      (let [x (peek stack) stack (pop stack)]
        (if (seen x)
          (recur stack seen)
          (recur (into stack (parents model x)) (conj seen x)))))))

(defn by-module
  "All node ids whose :module equals `module`."
  [model module]
  (into #{} (comp (filter #(= module (:module %))) (map :id)) (nodes model)))

(defn group-ancestors
  "Set of `gid` and every ancestor group (following :parent) — the group ids that
  must be expanded for `gid` itself to be visible in the outline tree."
  [model gid]
  (loop [g gid acc #{}]
    (if (or (nil? g) (acc g))
      acc
      (recur (get-in model [:groups g :parent]) (conj acc g)))))

(defn reveal-group-ids
  "The set of group ids to expand so `node-id` becomes visible in the outline tree:
  every group that lists it as a member, plus each of their ancestors. Used to sync
  the tree-view with graph navigation."
  [model node-id]
  (let [direct (into #{}
                     (comp (filter (fn [[_ g]] (some #{node-id} (:members g)))) (map key))
                     (:groups model))]
    (into #{} (mapcat #(group-ancestors model %)) direct)))

(defn roots
  "Node ids that are not a :subclass-of anything defined in the model (top of the
  class hierarchy). External stubs are excluded."
  [model]
  (into #{} (comp (remove :external?)
                  (filter #(empty? (parents model (:id %))))
                  (map :id))
        (nodes model)))

(defn orphans
  "Node ids with no edges at all — visually and structurally isolated."
  [model]
  (into #{} (comp (filter #(empty? (edges-touching model (:id %)))) (map :id))
        (nodes model)))

;; ---------------------------------------------------------------------------
;; Search
;; ---------------------------------------------------------------------------

(defn- normalize [s] (some-> s str/lower-case))

(defn search
  "Substring search over id, label, and gloss (case-insensitive). Returns a vector
  of node ids ranked: label prefix > label substring > id substring > gloss
  substring. `limit` caps results (default 50)."
  ([model q] (search model q 50))
  ([model q limit]
   (let [q (normalize (str/trim (or q "")))]
     (if (str/blank? q)
       []
       (let [scored (keep (fn [{:keys [id label gloss] :as _n}]
                            (let [l (normalize label) i (normalize id) g (normalize gloss)
                                  score (cond
                                          (and l (str/starts-with? l q)) 0
                                          (and l (str/includes? l q))    1
                                          (and i (str/includes? i q))    2
                                          (and g (str/includes? g q))    3
                                          :else nil)]
                              (when score [score id])))
                          (nodes model))]
         (->> scored (sort-by first) (map second) (take limit) vec))))))

;; ---------------------------------------------------------------------------
;; Integrity validation (advisory, never blocking)
;; ---------------------------------------------------------------------------

(defn- subclass-cycles
  "Find nodes participating in a :subclass-of cycle. Returns a set of node ids."
  [model]
  (let [child->parents (fn [id] (parents model id))]
    (reduce (fn [acc {:keys [id]}]
              (loop [stack [id] seen #{}]
                (cond
                  (empty? stack) acc
                  :else (let [x (peek stack) stack (pop stack)]
                          (cond
                            (and (seen x) (= x id)) (conj acc id) ; came back to start
                            (seen x) (recur stack seen)
                            :else (recur (into stack (child->parents x)) (conj seen x)))))))
            #{}
            (nodes model))))

(defn validate
  "Return a vector of advisory warnings about the model. Never throws, never
  blocks editing. Each warning is {:kind kw :message str :refs [...]}.
  Checks: dangling edge endpoints, duplicate order entries, :subclass-of cycles,
  and edges whose endpoints are missing."
  [model]
  (let [ids (set (keys (:nodes model)))
        dangling (for [e (edges model)
                       :when (or (not (ids (:source e))) (not (ids (:target e))))]
                   {:kind :dangling-edge
                    :message (str "Edge " (:id e) " references a missing node")
                    :refs [(:source e) (:target e)]})
        cyc (subclass-cycles model)
        cyc-warn (when (seq cyc)
                   [{:kind :subclass-cycle
                     :message (str (count cyc) " node(s) participate in a subclass-of cycle")
                     :refs (vec cyc)}])]
    (vec (concat dangling cyc-warn))))

;; ---------------------------------------------------------------------------
;; Stats
;; ---------------------------------------------------------------------------

(defn stats
  "Summary counts for the status bar: totals plus per-kind and external counts."
  [model]
  {:nodes (node-count model)
   :edges (edge-count model)
   :external (count (filter :external? (nodes model)))
   :by-kind (frequencies (map :kind (nodes model)))
   :modules (count (:groups model))})
