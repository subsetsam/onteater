(ns onteater.viz.common
  "Shared graph rendering: the palette/encodings and the single Reagent component
  that owns a d3-managed <svg> for every layout.

  ## d3 ↔ Reagent contract

  - The component renders exactly one <svg>. Everything inside it is owned by d3
    via the enter/update/exit (join) pattern; React never reconciles that subtree.
  - New data flows in through `component-did-update` from props (subscriptions).
  - Events flow out ONLY by `re-frame/dispatch` from d3 handlers. We never read
    app-db in here.
  - The force simulation is stopped on unmount to avoid leaks.

  ## Encodings (validated via the dataviz skill)

  - Node SHAPE encodes :kind — circle = class, diamond = property, square =
    individual, big bubble = collapsed module/group.
  - Node COLOR encodes :module using the skill's 8-hue categorical palette (light
    and dark steps), assigned in fixed order to the primary modules; structural
    sections (spine, relations, axioms, external, …) get dedicated neutral tones.
    Because CVD separation sits in the 8–12 floor band, color is always backed by
    the secondary encodings the skill requires: the permanent node label, the
    legend, and cluster position.
  - External stub nodes render with a dashed outline; the selection gets a halo."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            ["d3" :as d3]
            [onteater.model.graph :as g]
            [onteater.viz.graph :as vgraph]
            [onteater.viz.tree :as vtree]))

;; --- palette ---------------------------------------------------------------

(def categorical-light ["#2a78d6" "#1baf7a" "#eda100" "#008300" "#4a3aa7" "#e34948" "#e87ba4" "#eb6834"])
(def categorical-dark  ["#3987e5" "#199e70" "#c98500" "#008300" "#9085e9" "#e66767" "#d55181" "#d95926"])

;; Structural sections are format-level — the non-module homes the geo adapter
;; assigns (spine/relations/axioms/…) plus the synthetic `external` stub group. They
;; get fixed neutral tones so the ontology's *content* modules carry the categorical
;; hues. Every other :module value is a content module coloured by its position in
;; the loaded ontology's module order (see `ordered-modules`).
(def structural-light
  {"spine" "#64748b" "relations" "#7c7c8a" "axioms" "#8a7f5a"
   "prospectus_alignment" "#5f8a86" "external" "#9aa4b0"})
(def structural-dark
  {"spine" "#94a3b8" "relations" "#a7a7b6" "axioms" "#bcae86"
   "prospectus_alignment" "#8fb8b4" "external" "#c3ccd6"})

(defn ordered-modules
  "The categorical module order for the loaded `model`: the distinct :module values
  of real (non-external) nodes, excluding the structural sections above, in the
  model's stable node order. `node-color` assigns categorical hues by index into
  this vector, so ANY loaded ontology gets per-module colours — the encoding is
  derived from the data, never hard-coded to one ontology's module taxonomy."
  [model]
  (let [structural  (set (keys structural-light))
        ordered-ids (if (seq (:order model)) (:order model) (map :id (g/nodes model)))]
    (into []
          (comp (map #(g/node model %))
                (remove nil?)
                (remove :external?)
                (keep :module)
                (remove structural)
                (distinct))
          ordered-ids)))

(defn node-color
  "Resolve a node's `module` to a hex colour for the given theme. `order` is the
  vector from `ordered-modules` for the loaded model: a module is coloured by its
  index into it (hues cycle if an ontology has more than eight modules). Structural
  sections fall back to their fixed neutral tone, anything unknown to a mid grey."
  [dark? order module]
  (let [cats (if dark? categorical-dark categorical-light)
        i    (.indexOf (to-array order) module)]
    (if (neg? i)
      (or (get (if dark? structural-dark structural-light) module)
          (if dark? "#8a94a1" "#9aa4b0"))
      (nth cats (mod i (count cats))))))

(defn legend-entries
  "Module -> colour pairs for the on-canvas legend, restricted to modules actually
  present in the visible graph. `order` is the model's `ordered-modules` vector."
  [dark? order visible-nodes]
  (->> visible-nodes (map :module) (remove nil?) distinct
       (map (fn [m] {:module m :color (node-color dark? order m)}))))

;; --- shape paths -----------------------------------------------------------

(defn- shape-path
  "An SVG path string for a node of `kind` at radius `r`, centred at the origin."
  [kind r]
  (case kind
    :property (str "M0," (- r) "L" r ",0 L0," r " L" (- r) ",0 Z")            ; diamond
    :individual (str "M" (- r) "," (- r) " H" r " V" r " H" (- r) " Z")        ; square
    ;; circle (class, group, external, default) via two arcs
    (str "M" (- r) ",0 a" r "," r " 0 1,0 " (* 2 r) ",0 a" r "," r " 0 1,0 " (* -2 r) ",0")))

(defn- node-radius [d]
  (if (aget d "meta")
    (+ 16 (min 26 (* 2.2 (js/Math.sqrt (max 1 (aget d "count"))))))
    9))

;; --- node/link data prep ---------------------------------------------------

(defn- build-node-objs
  "Merge the visible nodes into persistent JS node objects (kept in `prev` by id so
  positions survive redraws). Returns [array-of-node-objs id->obj-map]. `order` is
  the model's `ordered-modules` vector, driving per-module colour."
  [visible-nodes prev w h dark? order]
  (let [objs (reduce
              (fn [m n]
                (let [id (:id n)
                      o  (or (get prev id)
                             (doto (js-obj)
                               (aset "x" (+ (/ w 2) (- (rand-int 120) 60)))
                               (aset "y" (+ (/ h 2) (- (rand-int 120) 60)))))]
                  (doto o
                    (aset "id" id)
                    (aset "label" (:label n))
                    (aset "kind" (name (or (:kind n) :class)))
                    (aset "kindkw" (:kind n))
                    (aset "module" (:module n))
                    (aset "meta" (boolean (:meta? n)))
                    (aset "count" (or (:count n) 0))
                    (aset "external" (boolean (:external? n)))
                    (aset "collapsed" (boolean (:collapsed? n)))
                    (aset "hidden" (or (:hidden-count n) 0))
                    (aset "color" (node-color dark? order (:module n)))
                    (aset "groupId" (:group-id n)))
                  (aset o "r" (node-radius o))
                  (assoc m id o)))
              {}
              visible-nodes)]
    [(apply array (vals objs)) objs]))

(defn- build-link-objs [visible-edges id->obj]
  (->> visible-edges
       (keep (fn [e]
               (when (and (id->obj (:source e)) (id->obj (:target e)))
                 (doto (js-obj)
                   (aset "source" (:source e))
                   (aset "target" (:target e))
                   (aset "type" (name (:type e)))))))
       (apply array)))

;; --- drawing ---------------------------------------------------------------
;;
;; All custom properties on d3 data objects are read with string-keyed `aget`
;; (never `.-foo`) to match how they are written with `aset`. This is essential:
;; the shipped artifact is an :advanced build, and `.-foo` on an untyped JS object
;; risks Closure renaming the property while the aset'd name stays literal.

(defn- dispatch-node-click [d]
  (rf/dispatch [:ontology/select-node (aget d "id")]))

(defn- dispatch-node-dblclick [d]
  (if (aget d "meta")
    (rf/dispatch [:view/expand-group (aget d "groupId")])
    (rf/dispatch [:view/focus-node (aget d "id")])))

(defn- draw-links! [g links]
  (let [sel (-> g (.select ".links")
                (.selectAll "line")
                (.data links (fn [d] (str (aget d "source") "|" (aget d "target") "|" (aget d "type")))))]
    (.remove (.exit sel))
    (-> (.enter sel)
        (.append "line")
        (.attr "class" (fn [d] (str "edge edge-" (aget d "type"))))
        (.attr "marker-end" (fn [d] (when (= "subclass-of" (aget d "type")) "url(#arrow)")))
        (.merge sel))))

(defn- clip-label [l]
  (if (and l (> (count l) 26)) (str (subs l 0 24) "…") l))

(defn- draw-nodes! [g nodes selected-id pinned]
  (let [sel (-> g (.select ".nodes")
                (.selectAll "g.node")
                (.data nodes (fn [d] (aget d "id"))))]
    (.remove (.exit sel))
    (let [enter (-> (.enter sel) (.append "g") (.attr "class" "node"))]
      (.append enter "path")
      (-> (.append enter "text") (.attr "class" "node-label"))
      (-> (.append enter "text") (.attr "class" "node-badge"))
      (let [merged (.merge enter sel)]
        (-> merged
            (.attr "data-id" (fn [d] (aget d "id")))
            (.classed "selected" (fn [d] (= selected-id (aget d "id"))))
            (.classed "pinned" (fn [d] (boolean (contains? pinned (aget d "id")))))
            (.classed "external" (fn [d] (aget d "external")))
            (.classed "meta" (fn [d] (aget d "meta"))))
        (-> merged (.select "path")
            (.attr "d" (fn [d] (shape-path (aget d "kindkw") (aget d "r"))))
            (.attr "fill" (fn [d] (aget d "color")))
            (.attr "stroke" (fn [d] (aget d "color"))))
        (-> merged (.select ".node-label")
            (.attr "y" (fn [d] (+ (aget d "r") 13)))
            (.text (fn [d] (clip-label (aget d "label")))))
        (-> merged (.select ".node-badge")
            (.attr "y" 4)
            (.text (fn [d] (cond
                             (aget d "meta") (str (aget d "count"))
                             (aget d "collapsed") (str "+" (aget d "hidden"))
                             :else ""))))
        merged))))

;; --- component -------------------------------------------------------------

(defn- ensure-scaffold!
  "Create the persistent <g> structure inside the svg once: a zoomable root group
  containing arrow defs, a links layer, and a nodes layer."
  [store el]
  (let [svg (d3/select el)]
    (let [defs (-> svg (.append "defs"))
          _    (-> defs (.append "marker")
                   (.attr "id" "arrow") (.attr "viewBox" "0 -5 10 10")
                   (.attr "refX" 20) (.attr "refY" 0)
                   (.attr "markerWidth" 6) (.attr "markerHeight" 6)
                   (.attr "orient" "auto")
                   (.append "path") (.attr "d" "M0,-4L8,0L0,4") (.attr "class" "arrowhead"))
          root (-> svg (.append "g") (.attr "class" "viz-root"))]
      (-> root (.append "g") (.attr "class" "links"))
      (-> root (.append "g") (.attr "class" "nodes"))
      (let [zoom (-> (d3/zoom)
                     (.scaleExtent #js [0.1 6])
                     (.on "zoom" (fn [ev] (.attr root "transform" (aget ev "transform")))))]
        (.call svg zoom)
        (.on svg "dblclick.zoom" nil)     ; free up dblclick for node focus
        (.on svg "click" (fn [ev]
                           (when (= (.-target ev) el)
                             (rf/dispatch [:ontology/clear-selection]))))
        (swap! store assoc :svg svg :root root :zoom zoom)))))

(defn- endpoint
  "Resolve a link's source/target, which is a node object once the force
  simulation has run but a bare id string in the static (tree/radial) case."
  [d k id->obj]
  (let [v (aget d k)] (if (string? v) (get id->obj v) v)))

(defn- position-static! [merged links-sel id->obj]
  (-> merged (.attr "transform" (fn [d] (str "translate(" (aget d "x") "," (aget d "y") ")"))))
  (-> links-sel
      (.attr "x1" (fn [d] (some-> (endpoint d "source" id->obj) (aget "x"))))
      (.attr "y1" (fn [d] (some-> (endpoint d "source" id->obj) (aget "y"))))
      (.attr "x2" (fn [d] (some-> (endpoint d "target" id->obj) (aget "x"))))
      (.attr "y2" (fn [d] (some-> (endpoint d "target" id->obj) (aget "y"))))))

(defn- attach-drag!
  "Node dragging for every layout. The node follows the pointer IMMEDIATELY — its
  x/y are updated and its `<g>` + incident edges repositioned on each move — so
  dragging works whether or not a force simulation is currently ticking (a settled
  sim would otherwise leave a fx/fy-pinned node visually stuck). When there IS a
  simulation the node is also pinned (fx/fy) and the sim gently reheated so
  neighbours make room; alt-drag releases the pin. In the static (tree/radial)
  layouts the position is remembered in `:manual-pos` so it survives later redraws.

  d3-drag stops the initiating mousedown from reaching the zoom behaviour, so
  dragging a node never pans the canvas. `event.x/y` are already in un-zoomed graph
  space (the drag container is the nodes group), matching the node's own x/y."
  [merged links-sel id->obj sim store]
  (let [reposition #(position-static! merged links-sel id->obj)
        dragger
        (-> (d3/drag)
            (.on "start" (fn [ev _d]
                           (when (and sim (not (aget ev "active")))
                             (-> sim (.alphaTarget 0.3) (.restart)))))
            (.on "drag" (fn [ev d]
                          (let [x (aget ev "x") y (aget ev "y") id (aget d "id")]
                            (aset d "x" x) (aset d "y" y)
                            (when sim (aset d "fx" x) (aset d "fy" y))
                            (when-not sim (swap! store assoc-in [:manual-pos id] {:x x :y y}))
                            (reposition))))
            (.on "end" (fn [ev d]
                         (when (and sim (not (aget ev "active"))) (.alphaTarget sim 0))
                         ;; alt-drag releases a simulation pin
                         (let [se (aget ev "sourceEvent")]
                           (when (and sim se (aget se "altKey"))
                             (aset d "fx" nil) (aset d "fy" nil))))))]
    (.call merged dragger)))

(defn- draw!
  "Full redraw for the current props. Handles all four layouts."
  [store {:keys [vg view-spec selection dark? force-opts module-order]}]
  (let [{:keys [el nodes-map]} @store
        w (.-clientWidth el) h (.-clientHeight el)
        w (if (pos? w) w 800) h (if (pos? h) h 600)
        layout (:layout view-spec)
        ;; Each layout gets its own arrangement: dropping the manual-drag overrides
        ;; when the layout changes lets a fresh tree/force compute clean positions.
        _ (when (not= layout (:prev-layout @store))
            (swap! store assoc :prev-layout layout :manual-pos {}))
        [node-objs id->obj] (build-node-objs (:nodes vg) (or nodes-map {}) w h dark? module-order)
        links (build-link-objs (:edges vg) id->obj)
        root (:root @store)
        selected-id (:node selection)
        pinned (or (:pinned selection) #{})]
    (swap! store assoc :nodes-map id->obj)
    (let [links-sel (draw-links! root links)
          merged    (draw-nodes! root node-objs selected-id pinned)]
      (.on merged "click" (fn [ev d] (.stopPropagation ev) (dispatch-node-click d)))
      (.on merged "dblclick" (fn [ev d] (.stopPropagation ev) (dispatch-node-dblclick d)))
      (.on merged "contextmenu"
           (fn [ev d]
             (.preventDefault ev) (.stopPropagation ev)
             (rf/dispatch [:ui/context-menu {:x (.-clientX ev) :y (.-clientY ev)
                                             :node-id (aget d "id") :meta? (aget d "meta")
                                             :external? (aget d "external")}])))
      (if (#{:force :cluster} layout)
        ;; --- dynamic (simulation) layouts ---
        (let [sim (or (:sim @store)
                      (vgraph/make-simulation node-objs links w h force-opts (fn [])))]
          (.nodes sim node-objs)
          (-> sim (.force "link") (.links links))
          ;; Rebind the tick to the CURRENT selections each redraw, else the
          ;; simulation would keep repositioning stale DOM after a data change.
          (.on sim "tick" (fn [] (position-static! merged links-sel id->obj)))
          (vgraph/apply-layout! sim layout node-objs w h)
          (attach-drag! merged links-sel id->obj sim store)
          (swap! store assoc :sim sim)
          (.alpha sim 0.7) (.restart sim))
        ;; --- static (tree/radial) layouts ---
        (do
          (when-let [sim (:sim @store)] (.stop sim) (swap! store dissoc :sim))
          (let [{:keys [pos]} (vtree/positions (:nodes vg) (:edges vg) (:roots view-spec)
                                               (= layout :radial) w h)
                manual (:manual-pos @store)]
            ;; Computed tree position, unless the user has dragged this node.
            (doseq [o node-objs]
              (let [id (aget o "id")
                    p  (or (get manual id) (get pos id))]
                (when p (aset o "x" (:x p)) (aset o "y" (:y p)))))
            (attach-drag! merged links-sel id->obj nil store)
            (position-static! merged links-sel id->obj)))))))

(defn graph-view
  "Reagent form-3 component. Props: {:vg :view-spec :selection :dark? :force-opts
  :module-order}. Owns the <svg> and all d3 rendering within it."
  [_props]
  (let [store (atom {})]
    (r/create-class
     {:display-name "graph-view"
      :component-did-mount
      (fn [this]
        (ensure-scaffold! store (:el @store))
        (draw! store (r/props this)))
      :component-did-update
      (fn [this _] (draw! store (r/props this)))
      :component-will-unmount
      (fn [_] (when-let [sim (:sim @store)] (.stop sim)))
      :reagent-render
      (fn [_props]
        [:svg.graph-canvas
         {:ref (fn [el] (when el (swap! store assoc :el el)))}])})))
