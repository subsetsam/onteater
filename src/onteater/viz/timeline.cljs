(ns onteater.viz.timeline
  "The swimlane DAG timeline view (§6.7.3): time/order on the x-axis, entity (or
  module / episode / single) lanes on the y-axis, event glyphs, and routed
  fork/join relation edges.

  ## d3 ↔ Reagent contract (same as `onteater.viz.common`)

  - One `<svg>`, owned entirely by d3 via the join pattern; React never reconciles
    inside it. Data flows in from props (the pure `onteater.model.timeline/layout`
    result + cone/selection); events flow out only through `re-frame/dispatch`.
  - The layout is fully computed upstream in ordinal units — this namespace only
    SCALES ordinal indices/lane rows to pixels and draws. There is no force
    simulation and no general graph-layout library: x is fixed by the ordering
    cascade, y by lane assignment (§12.13). d3 data are plain CLJS maps (the layout
    is static, nothing is mutated in place), so keyword access is safe under the
    `:advanced` build without the `aset`/`aget` discipline the force view needs.

  Encodings share the ontology canvas's module palette (one visual system, §12.12):
  glyph fill = module colour; a hollow “?” glyph marks an untyped event (an ontology
  gap, visible in place); a lock marks a forced item; dashed warning-coloured edges
  mark relations with no matching ontology property. In dependency-cone mode
  everything outside the selected event's cone dims, upstream and downstream tinted
  differently."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            ["d3" :as d3]
            [onteater.viz.common :as common]))

(def ^:private margin {:left 140 :top 18 :right 24 :bottom 34})
(def ^:private lane-h 52)
(def ^:private collapsed-lane-h 22)

(defn- relation-family
  "Coarse family for edge styling."
  [type]
  (case type
    (:causes :enables :terminates) :causal
    (:responds-to) :response
    :temporal))

;; --- scales -----------------------------------------------------------------

(defn- build-scales
  "Pure pixel scales for the current layout + element size. Returns
  {:x (index->px) :lane-y (lane-index->row-top) :lane-mid :height :width :col-w}."
  [layout w collapsed]
  (let [lanes    (:lanes layout)
        n-cols   (max 1 (count (:events layout)))
        collapsed? (fn [k] (contains? collapsed k))
        lane-unit (fn [l] (if (or (:auto-collapsed? l) (collapsed? (:key l))) collapsed-lane-h lane-h))
        ;; cumulative top of each lane by index
        tops     (reductions + (:top margin) (map lane-unit lanes))
        lane-top (into {} (map-indexed (fn [i l] [(:index l) (nth tops i)]) lanes))
        content-h (last tops)
        height   (+ content-h (:bottom margin))
        inner-w  (max 100 (- w (:left margin) (:right margin)))
        col-w    (/ inner-w n-cols)
        xf       (fn [index] (+ (:left margin) (* (+ index 0.5) col-w)))]
    {:x xf
     :lane-top lane-top
     :lane-h  (fn [l] (lane-unit l))
     :lane-mid (fn [l] (+ (get lane-top (:index l) (:top margin)) (/ (lane-unit l) 2)))
     :height height :width w :col-w col-w}))

;; --- drawing ----------------------------------------------------------------

(defn- clear! [el] (-> (d3/select el) (.selectAll "*") .remove))

(defn- draw-lanes! [root scales layout collapsed dark?]
  (let [g (-> root (.append "g") (.attr "class" "tl-lanes"))]
    (doseq [l (:lanes layout)]
      (let [top ((:lane-top scales) (:index l))
            h   ((:lane-h scales) l)
            collapsed? (or (:auto-collapsed? l) (contains? collapsed (:key l)))]
        (-> g (.append "rect")
            (.attr "class" (str "tl-lane" (when (odd? (:index l)) " tl-lane-alt")))
            (.attr "x" 0) (.attr "y" top)
            (.attr "width" (:width scales)) (.attr "height" h))
        (-> g (.append "text")
            (.attr "class" "tl-lane-label")
            (.attr "x" 8) (.attr "y" (+ top (/ h 2) 4))
            (.text (str (when collapsed? "▸ ")
                        (let [lbl (str (:label l))]
                          (if (> (count lbl) 18) (str (subs lbl 0 17) "…") lbl))
                        (when collapsed? (str "  (" (count (:event-ids l)) ")"))))
            (.on "click" (fn [_ _] (rf/dispatch [:timeline/toggle-collapse (:key l)]))))))))

(defn- draw-axis! [root scales layout]
  (let [g (-> root (.append "g") (.attr "class" "tl-axis"))
        y (- (:height scales) 20)]
    (-> g (.append "line") (.attr "class" "tl-axis-line")
        (.attr "x1" (:left margin)) (.attr "x2" (- (:width scales) (:right margin)))
        (.attr "y1" y) (.attr "y2" y))
    (doseq [e (:events layout)]
      (let [x ((:x scales) (:index e))]
        ;; hatch tick for ordinal (undated) slots so metric vs ordinal is honest
        (-> g (.append "line")
            (.attr "class" (if (:dated? e) "tl-tick tl-tick-metric" "tl-tick tl-tick-ordinal"))
            (.attr "x1" x) (.attr "x2" x) (.attr "y1" (- y 4)) (.attr "y2" (+ y 4)))
        (when (:dated? e)
          (-> g (.append "text") (.attr "class" "tl-tick-label")
              (.attr "x" x) (.attr "y" (+ y 16))
              (.text (or (get-in e [:when :start]) ""))))))))

(defn- in-cone? [cone id]
  (or (nil? cone)
      (= id (:focus cone))
      (contains? (:ancestors cone) id)
      (contains? (:descendants cone) id)))

(defn- cone-class [cone id]
  (cond
    (nil? cone) ""
    (= id (:focus cone)) " tl-cone-focus"
    (contains? (:ancestors cone) id) " tl-cone-up"
    (contains? (:descendants cone) id) " tl-cone-down"
    :else " tl-dimmed"))

(defn- draw-edges! [root scales layout by-id cone dark?]
  (let [g (-> root (.append "g") (.attr "class" "tl-edges"))]
    (doseq [e (:edges layout)]
      (let [s (get by-id (:source e)) t (get by-id (:target e))]
        (when (and s t)
          (let [x1 ((:x scales) (:index s)) x2 ((:x scales) (:index t))
                y1 (:cy s) y2 (:cy t)
                ;; fan-out control offset so sibling edges from one source separate
                mx (/ (+ x1 x2) 2)
                my (+ (/ (+ y1 y2) 2) (* (:fan e) 14))
                path (str "M" x1 "," y1 " Q" mx "," my " " x2 "," y2)
                fam (name (relation-family (:type e)))
                lit (and cone (in-cone? cone (:source e)) (in-cone? cone (:target e)))]
            (-> g (.append "path")
                (.attr "class" (str "tl-edge tl-edge-" fam
                                    (when (:untyped? e) " tl-edge-untyped")
                                    (when (= :forced (:status e)) " tl-edge-forced")
                                    (when (and cone (not lit)) " tl-dimmed")))
                (.attr "d" path)
                (.attr "marker-end" "url(#tl-arrow)")
                (.on "click" (fn [ev _] (.stopPropagation ev)
                               (rf/dispatch [:timeline/select-relation (:id e)]))))))))))

(defn- glyph-shape
  "Append the glyph mark for an event `<g>`. Untyped events get a hollow ‘?’ circle."
  [sel]
  (-> sel (.append "circle") (.attr "class" "tl-glyph-mark") (.attr "r" 10))
  (-> sel (.append "text") (.attr "class" "tl-glyph-q") (.attr "dy" 4) (.attr "text-anchor" "middle")
      (.text (fn [e] (if (nil? (:node-id e)) "?" ""))))
  (-> sel (.append "text") (.attr "class" "tl-glyph-lock") (.attr "dy" -12) (.attr "text-anchor" "middle")
      (.text (fn [e] (if (= :forced (:status e)) "🔒" ""))))
  (-> sel (.append "text") (.attr "class" "tl-glyph-label") (.attr "dy" 24) (.attr "text-anchor" "middle")
      (.text (fn [e] (let [l (str (:label e))] (if (> (count l) 20) (str (subs l 0 19) "…") l))))))

(defn- draw-events! [root scales layout by-id cone selected dark? order cycles]
  (let [g (-> root (.append "g") (.attr "class" "tl-events"))
        sel (-> g (.selectAll "g.tl-event")
                (.data (to-array (:events layout)) (fn [e] (:id e))))
        enter (-> (.enter sel) (.append "g") (.attr "class" "tl-event"))]
    (glyph-shape enter)
    (let [merged (.merge enter sel)
          ;; Drag glyph-to-glyph to create a relation (§6.7.3). A relation defaults
          ;; to :precedes and is selected so the user retypes it via the relation
          ;; detail dropdown (the type picker). `clickDistance` keeps still clicks
          ;; as selection, so dragging never clobbers click-to-select.
          drag (-> (d3/drag)
                   (.clickDistance 5)
                   (.on "end"
                        (fn [ev d]
                          (let [se  (aget ev "sourceEvent")
                                el  (js/document.elementFromPoint (aget se "clientX") (aget se "clientY"))
                                tgt (some-> el (.closest ".tl-event") (.getAttribute "data-event-id"))]
                            (when (and tgt (not= tgt (:id d)))
                              (rf/dispatch [:timeline/add-relation (:id d) tgt :precedes]))))))]
      (-> merged
          (.attr "transform" (fn [e] (str "translate(" (:cx (by-id (:id e))) "," (:cy (by-id (:id e))) ")")))
          (.attr "data-event-id" (fn [e] (:id e)))
          (.attr "class" (fn [e] (str "tl-event"
                                      (when (= selected (:id e)) " tl-selected")
                                      (when (contains? cycles (:id e)) " tl-cycle")
                                      (cone-class cone (:id e)))))
          (.call drag)
          (.on "click" (fn [ev e] (.stopPropagation ev)
                         (rf/dispatch [:timeline/select-event (:id e)]))))
      (-> merged (.select ".tl-glyph-mark")
          (.attr "fill" (fn [e] (if (:node-id e)
                                  (common/node-color dark? order (get-in by-id [(:id e) :module]))
                                  "transparent")))
          (.attr "stroke" (fn [e] (if (:node-id e)
                                    (common/node-color dark? order (get-in by-id [(:id e) :module]))
                                    "var(--warn, #d9822b)")))))))

(defn- draw-containers!
  "Translucent rounded rects behind each `:part-of` parent spanning its children —
  compound episodes visible as containers (§6.7.3)."
  [root scales layout by-id]
  (let [g (-> root (.append "g") (.attr "class" "tl-containers"))
        children-of (reduce (fn [m {:keys [child parent]}] (update m parent (fnil conj []) child))
                            {} (:part-of layout))]
    (doseq [[parent kids] children-of]
      (let [pts (keep by-id (conj (vec kids) parent))
            xs  (map :cx pts) ys (map :cy pts)]
        (when (seq xs)
          (-> g (.append "rect")
              (.attr "class" "tl-container")
              (.attr "x" (- (apply min xs) 18)) (.attr "y" (- (apply min ys) 18))
              (.attr "width" (+ (- (apply max xs) (apply min xs)) 36))
              (.attr "height" (+ (- (apply max ys) (apply min ys)) 36))
              (.attr "rx" 10)))))))

(defn- draw!
  [store {:keys [layout cone selected dark? module-order collapsed]}]
  (let [el (:el @store)]
    (when (and el layout)
      (clear! el)
      (let [w (max 600 (.-clientWidth el))
            scales (build-scales layout w collapsed)
            svg (-> (d3/select el)
                    (.attr "width" (:width scales))
                    (.attr "height" (:height scales)))
            ;; arrow marker + zoomable root
            defs (-> svg (.append "defs"))
            _ (-> defs (.append "marker")
                  (.attr "id" "tl-arrow") (.attr "viewBox" "0 -5 10 10")
                  (.attr "refX" 16) (.attr "refY" 0)
                  (.attr "markerWidth" 6) (.attr "markerHeight" 6) (.attr "orient" "auto")
                  (.append "path") (.attr "d" "M0,-4L8,0L0,4") (.attr "class" "tl-arrowhead"))
            root (-> svg (.append "g") (.attr "class" "tl-root"))
            ;; precompute pixel centres per event (skip events whose lane is collapsed)
            lane-by-key (into {} (map (juxt :key identity)) (:lanes layout))
            by-id (into {}
                        (keep (fn [e]
                                (let [lane (get lane-by-key (:lane e))
                                      collapsed? (or (:auto-collapsed? lane) (contains? collapsed (:lane e)))]
                                  (when-not collapsed?
                                    [(:id e) (assoc e :cx ((:x scales) (:index e))
                                                    :cy ((:lane-mid scales) lane))]))))
                        (:events layout))
            cycles (:cycles layout)]
        (draw-lanes! root scales layout collapsed dark?)
        (draw-containers! root scales layout by-id)
        (draw-axis! root scales layout)
        (draw-edges! root scales layout by-id cone dark?)
        (draw-events! root scales layout by-id cone selected dark? module-order cycles)
        ;; background click clears selection; zoom/pan on the root group
        (.on svg "click" (fn [_ _] (rf/dispatch [:timeline/select-event nil])))
        (let [zoom (-> (d3/zoom) (.scaleExtent #js [0.4 5])
                       (.on "zoom" (fn [ev] (.attr root "transform" (aget ev "transform")))))]
          (.call svg zoom)
          (.on svg "dblclick.zoom" nil))))))

(defn timeline-view
  "Reagent form-3 component. Props: {:layout :cone :selected :dark? :module-order
  :collapsed}. Owns the <svg> and all d3 rendering within it."
  [_props]
  (let [store (atom {})]
    (r/create-class
     {:display-name "timeline-view"
      :component-did-mount (fn [this] (draw! store (r/props this)))
      :component-did-update (fn [this _] (draw! store (r/props this)))
      :reagent-render
      (fn [_]
        [:svg.tl-canvas {:ref (fn [el] (when el (swap! store assoc :el el)))}])})))
