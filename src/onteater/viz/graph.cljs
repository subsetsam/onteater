(ns onteater.viz.graph
  "Force-directed and module-clustered layouts.

  These functions build and configure a d3-force simulation over the *visible*
  subgraph (kept small by the view model). The owning Reagent component
  (`onteater.viz.common`) holds the simulation instance and drives the tick loop;
  this namespace only knows d3-force, never the DOM or app-db, so the layout logic
  stays isolated and swappable."
  (:require ["d3" :as d3]))

(defn make-simulation
  "Create a d3-force simulation for `js-nodes` / `js-links` sized to `w`×`h`.
  `on-tick` is called each tick to reposition the DOM. Returns the simulation."
  [js-nodes js-links w h {:keys [link-distance charge]} on-tick]
  (let [link-force (-> (d3/forceLink js-links)
                       (.id (fn [d] (.-id d)))
                       (.distance (or link-distance 70))
                       (.strength 0.4))]
    (-> (d3/forceSimulation js-nodes)
        (.force "link" link-force)
        (.force "charge" (-> (d3/forceManyBody) (.strength (or charge -260))))
        (.force "center" (d3/forceCenter (/ w 2) (/ h 2)))
        (.force "collide" (-> (d3/forceCollide) (.radius (fn [d] (+ 6 (or (.-r d) 10))))))
        (.on "tick" on-tick))))

(defn apply-layout!
  "Reconfigure `sim` for the given `layout` (:force or :cluster). In :cluster mode
  nodes are pulled toward a per-module anchor laid out on a grid, so modules form
  visible spatial groups (the geo ontology's structure). In :force mode those
  positional forces are removed and only the generic charge/link/center forces
  remain."
  [sim layout js-nodes w h]
  (if (= layout :cluster)
    (let [modules (->> js-nodes (map #(.-module %)) (remove nil?) distinct vec)
          n       (count modules)
          cols    (max 1 (js/Math.ceil (js/Math.sqrt n)))
          rows    (max 1 (js/Math.ceil (/ n cols)))
          anchor  (into {}
                        (map-indexed (fn [i m]
                                       (let [col (mod i cols) row (quot i cols)]
                                         [m {:x (* (/ w cols) (+ 0.5 col))
                                             :y (* (/ h rows) (+ 0.5 row))}])))
                        modules)]
      (-> sim
          (.force "x" (-> (d3/forceX (fn [d] (get-in anchor [(.-module d) :x] (/ w 2)))) (.strength 0.28)))
          (.force "y" (-> (d3/forceY (fn [d] (get-in anchor [(.-module d) :y] (/ h 2)))) (.strength 0.28)))
          (.force "charge" (-> (d3/forceManyBody) (.strength -180)))))
    (-> sim
        (.force "x" nil)
        (.force "y" nil)
        (.force "charge" (-> (d3/forceManyBody) (.strength -260)))))
  sim)
