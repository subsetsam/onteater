(ns onteater.viz.tree
  "Tidy-tree and radial-tree layouts.

  Unlike the force layouts these are *static*: given the visible subgraph and a
  set of roots, we build a d3.hierarchy over the subclass-of structure and read
  fixed x/y positions out of `d3.tree`. The visible graph is a general graph (a
  node may have several parents), so we first project it onto a spanning tree —
  each node keeps the first parent reached in a breadth-first sweep from the roots;
  unreachable nodes hang off a synthetic root so nothing is dropped.

  Returns plain data (positions + parent/child links); the owning component turns
  it into SVG. No DOM, no app-db here."
  (:require ["d3" :as d3]))

(defn- spanning-parents
  "Depth-first sweep of the visible subclass structure from `roots`; return a map
  child-id -> parent-id giving each reachable node a single parent (first one
  reached wins). A DFS spanning tree is fine here — we only need each node to have
  exactly one parent so d3.hierarchy is well-formed."
  [id-set child->parents roots]
  (loop [stack (vec roots)
         seen  (set roots)
         parent-of {}]
    (if (empty? stack)
      parent-of
      (let [pid (peek stack) stack (pop stack)
            ;; children of pid = visible nodes that list pid among their parents
            kids (for [cid id-set
                       :when (and (not (seen cid))
                                  (contains? (child->parents cid) pid))]
                   cid)]
        (recur (into stack kids)
               (into seen kids)
               (into parent-of (map (fn [c] [c pid])) kids))))))

(defn hierarchy-data
  "Build a nested {:id :children [...]} tree from the visible graph for d3.hierarchy.
  `nodes` is the visible node vector; `edges` the visible edges; `roots` the chosen
  root ids (falls back to visible nodes with no visible parent). Nodes not reachable
  from any root (multi-parent remainders, orphans) hang directly off the synthetic
  root so nothing is silently dropped."
  [nodes edges roots]
  (let [id-set (set (map :id nodes))
        subclass (filter #(= :subclass-of (:type %)) edges)
        child->parents (reduce (fn [m e] (update m (:source e) (fnil conj #{}) (:target e)))
                               {} subclass)
        has-parent? (fn [id] (some id-set (child->parents id)))
        real-roots (if (seq roots)
                     (vec (filter id-set roots))
                     (vec (remove has-parent? (map :id nodes))))
        parent-of (spanning-parents id-set child->parents real-roots)
        placed    (into (set real-roots) (keys parent-of))
        orphans   (remove placed (map :id nodes))
        top-ids   (distinct (concat real-roots orphans))
        children-of (reduce (fn [m [c p]] (update m p (fnil conj []) c)) {} parent-of)
        build (fn build [id]
                #js {:id id
                     :children (apply array (map build (get children-of id [])))})]
    #js {:id "__root__"
         :children (apply array (map build top-ids))}))

(defn positions
  "Compute static positions for a tidy (`radial?`=false) or radial tree.
  Returns {:pos {id -> {:x :y}} :links [{:source id :target id}]}. Coordinates are
  laid out to fill `w`×`h` with margins."
  [nodes edges roots radial? w h]
  (let [root-data (hierarchy-data nodes edges roots)
        root (d3/hierarchy root-data)
        pad 40
        layout (if radial?
                 (-> (d3/tree)
                     (.size #js [(* 2 js/Math.PI) (- (/ (min w h) 2) pad)])
                     (.separation (fn [a b] (/ (if (= (.-parent a) (.-parent b)) 1 2)
                                               (max 1 (.-depth a))))))
                 (-> (d3/tree) (.size #js [(- w (* 2 pad)) (- h (* 2 pad))])))
        _ (layout root)
        cx (/ w 2) cy (/ h 2)
        descendants (.descendants root)
        pos (reduce (fn [m d]
                      (if (= "__root__" (.-id (.-data d)))
                        m
                        (let [id (.-id (.-data d))]
                          (if radial?
                            (let [angle (.-x d) radius (.-y d)
                                  x (+ cx (* radius (js/Math.cos (- angle (/ js/Math.PI 2)))))
                                  y (+ cy (* radius (js/Math.sin (- angle (/ js/Math.PI 2)))))]
                              (assoc m id {:x x :y y}))
                            (assoc m id {:x (+ pad (.-x d)) :y (+ pad (.-y d))})))))
                    {}
                    descendants)
        links (for [d descendants
                    :let [p (.-parent d)]
                    :when (and p (not= "__root__" (.-id (.-data p)))
                               (not= "__root__" (.-id (.-data d))))]
                {:source (.-id (.-data p)) :target (.-id (.-data d))})]
    {:pos pos :links (vec links)}))
