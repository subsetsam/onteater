(ns onteater.io.export
  "Visualization export effects: the live graph canvas as standalone SVG and as a
  rasterized PNG (Export). The on-screen graph is styled by external CSS
  and CSS custom properties, so a naive `serializeToString` would produce an SVG
  that renders blank elsewhere. We therefore clone the live <svg>, copy each
  element's *computed* style inline, frame it to the content bounding box, drop in
  a background and a small caption block, and serialize that. PNG rasterizes the
  same SVG through an <img> onto a 2× canvas for crispness."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

(def ^:private inlined-style-props
  ["fill" "fill-opacity" "stroke" "stroke-width" "stroke-dasharray" "stroke-opacity"
   "opacity" "font-size" "font-family" "font-weight" "text-anchor"
   "dominant-baseline" "paint-order" "stroke-linejoin"])

(defn- inline-computed-styles!
  "Copy a whitelist of computed style properties from every element of `src` onto
  the matching element of `clone` (same document order after cloneNode)."
  [src clone]
  (let [srcs (.querySelectorAll src "*")
        dsts (.querySelectorAll clone "*")]
    (dotimes [i (.-length srcs)]
      (let [s (.item srcs i) d (.item dsts i)
            cs (js/getComputedStyle s)]
        (doseq [p inlined-style-props]
          (let [v (.getPropertyValue cs p)]
            (when (and v (not= v ""))
              (.setProperty (.-style d) p v))))))))

(defn- content-bbox [svg]
  (if-let [root (or (.querySelector svg ".viz-root") (.querySelector svg ".tl-root"))]
    (let [b (.getBBox root)]
      {:x (- (.-x b) 30) :y (- (.-y b) 30)
       :w (+ (.-width b) 60) :h (+ (.-height b) 90)}) ; extra bottom room for caption
    {:x 0 :y 0 :w (.-clientWidth svg) :h (.-clientHeight svg)}))

(defn- svg-ns [] "http://www.w3.org/2000/svg")

(defn- prepend-bg! [clone {:keys [x y w h]} bg]
  (let [rect (.createElementNS js/document (svg-ns) "rect")]
    (doto rect
      (.setAttribute "x" x) (.setAttribute "y" y)
      (.setAttribute "width" w) (.setAttribute "height" h)
      (.setAttribute "fill" (or bg "#ffffff")))
    (.insertBefore clone rect (.-firstChild clone))))

(defn- append-caption! [clone {:keys [x y w h]} lines fg]
  (let [g (.createElementNS js/document (svg-ns) "text")]
    (.setAttribute g "x" (+ x 8))
    (.setAttribute g "y" (+ y (- h 34)))
    (.setAttribute g "font-family" "system-ui, sans-serif")
    (.setAttribute g "font-size" "12")
    (.setAttribute g "fill" (or fg "#5c6673"))
    (doseq [[i line] (map-indexed vector lines)]
      (let [tspan (.createElementNS js/document (svg-ns) "tspan")]
        (.setAttribute tspan "x" (+ x 8))
        (.setAttribute tspan "dy" (if (zero? i) 0 15))
        (set! (.-textContent tspan) line)
        (.appendChild g tspan)))
    (.appendChild clone g)))

(defn- theme-colors
  "Resolve the current visual theme's surface + muted-text colours from the live
  DOM, so an exported image matches what is on screen (labels stay legible)."
  [bg fg]
  (let [root (or (.querySelector js/document ".app-root") (.-body js/document))
        cs   (js/getComputedStyle root)
        pick (fn [v prop fallback]
               (or v (let [x (str/trim (.getPropertyValue cs prop))]
                       (if (str/blank? x) fallback x))))]
    [(pick bg "--bg-panel" "#ffffff")
     (pick fg "--text-muted" "#5c6673")]))

(defn- build-svg-string
  "Clone the live canvas into a self-contained SVG string framed to `bbox`. The
  `:selector` chooses which live <svg> to export (default the ontology graph canvas;
  the timeline view passes `.tl-canvas`)."
  [{:keys [caption bg fg selector]}]
  (when-let [svg (.querySelector js/document (or selector ".graph-canvas"))]
    (let [[bg fg] (theme-colors bg fg)
          clone (.cloneNode svg true)
          bbox  (content-bbox svg)]
      (inline-computed-styles! svg clone)
      (.setAttribute clone "xmlns" (svg-ns))
      (.setAttribute clone "width" (:w bbox))
      (.setAttribute clone "height" (:h bbox))
      (.setAttribute clone "viewBox" (str/join " " [(:x bbox) (:y bbox) (:w bbox) (:h bbox)]))
      (prepend-bg! clone bbox bg)
      (when (seq caption) (append-caption! clone bbox caption fg))
      [(str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            (.serializeToString (js/XMLSerializer.) clone))
       bbox])))

(defn- download-blob [blob filename]
  (let [url (js/URL.createObjectURL blob)
        a   (.createElement js/document "a")]
    (set! (.-href a) url) (set! (.-download a) filename)
    (.appendChild (.-body js/document) a) (.click a) (.removeChild (.-body js/document) a)
    (js/setTimeout #(js/URL.revokeObjectURL url) 1000)))

(rf/reg-fx
 :io/export-svg
 (fn [{:keys [filename] :as opts}]
   (when-let [[svg-str _] (build-svg-string opts)]
     (download-blob (js/Blob. #js [svg-str] #js {:type "image/svg+xml"})
                    (or filename "onteater-graph.svg")))))

(rf/reg-fx
 :io/export-png
 (fn [{:keys [filename scale on-error] :as opts}]
   (if-let [[svg-str bbox] (build-svg-string opts)]
     (let [scale (or scale 2)
           img   (js/Image.)
           blob  (js/Blob. #js [svg-str] #js {:type "image/svg+xml;charset=utf-8"})
           url   (js/URL.createObjectURL blob)]
       (set! (.-onload img)
             (fn [_]
               (let [canvas (.createElement js/document "canvas")]
                 (set! (.-width canvas) (* scale (:w bbox)))
                 (set! (.-height canvas) (* scale (:h bbox)))
                 (let [ctx (.getContext canvas "2d")]
                   (.scale ctx scale scale)
                   (.drawImage ctx img 0 0 (:w bbox) (:h bbox)))
                 (.toBlob canvas (fn [b] (download-blob b (or filename "onteater-graph.png"))) "image/png")
                 (js/URL.revokeObjectURL url))))
       (set! (.-onerror img)
             (fn [_] (js/URL.revokeObjectURL url)
               (when on-error (rf/dispatch (conj on-error "PNG rasterization failed.")))))
       (set! (.-src img) url))
     (when on-error (rf/dispatch (conj on-error "No graph to export."))))))
