(ns onteater.ui.scenario
  "The scenario pane: raw/rendered scenario input, and —
  when rendered — colored excerpt underlines that link back to mapping entries.

  Highlighting follows the anchoring rule (§6.2): excerpts are located in the
  RENDERED DOM by exact text search (not char offsets). We render the Markdown/math
  HTML ourselves into a ref'd, React-empty div and then wrap matched excerpts in
  <mark> nodes; excerpts we cannot locate are simply not drawn (they are surfaced
  as a flag on the entry elsewhere), never faked."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.ui.markdown :as markdown]
            [onteater.viz.common :as viz]))

(defn- resolved-dark? [pref]
  (case pref :dark true :light false
        (boolean (and (exists? js/window)
                      (.-matches (js/matchMedia "(prefers-color-scheme: dark)"))))))

(defn- make-mark [{:keys [entry-id module status]} color selected?]
  ;; NB: the mark's content is supplied by the caller via Range.extractContents —
  ;; do NOT set textContent here, or the excerpt would render twice.
  (let [mark (.createElement js/document "mark")]
    (set! (.-className mark) (str "excerpt excerpt-" (name (or status :proposed))
                                  (when selected? " excerpt-selected")))
    (set! (.. mark -style -borderBottomColor) color)
    (set! (.-title mark) (str "→ " (or module "")))
    (.setAttribute mark "data-entry-id" (str entry-id))
    (.addEventListener mark "click"
                       (fn [e] (.stopPropagation e)
                         (rf/dispatch [:scenario/select-entry entry-id])))
    mark))

(defn- text-nodes
  "All text nodes under `root` that are not already inside a <mark>."
  [root]
  (let [walker (.createTreeWalker js/document root js/NodeFilter.SHOW_TEXT nil)
        acc    (transient [])]
    (loop []
      (when-let [tn (.nextNode walker)]
        (let [pe (.-parentElement tn)]
          (when-not (and pe (.closest pe "mark")) (conj! acc tn)))
        (recur)))
    (persistent! acc)))

(defn- locate
  "Map a global char `offset` over `nodes` (their textContent concatenated) to
  {:node n :offset local}. `end?` picks the trailing boundary (<=) vs leading (<)."
  [nodes offset end?]
  (loop [ns nodes acc 0]
    (when (seq ns)
      (let [n (first ns) len (count (.-textContent n))]
        (if (if end? (<= offset (+ acc len)) (< offset (+ acc len)))
          {:node n :offset (- offset acc)}
          (recur (rest ns) (+ acc len)))))))

(defn- ws-flexible-index
  "Find `excerpt` in `full` allowing whitespace runs to differ; returns
  {:start i :len n} or nil. Falls back from an exact indexOf."
  [full excerpt]
  (let [exact (.indexOf full excerpt)]
    (if (>= exact 0)
      {:start exact :len (count excerpt)}
      (let [pat (-> excerpt
                    (str/replace #"[.*+?^${}()|\[\]\\]" "\\$&")
                    (str/replace #"\s+" (constantly "\\s+")))
            m   (.exec (js/RegExp. pat) full)]
        (when m {:start (.-index m) :len (count (aget m 0))})))))

(defn- wrap-excerpt!
  "Locate `excerpt` across `root`'s text (spanning element boundaries) and wrap the
  match in `mark` via a DOM Range. Robust to <strong>/<em> splitting the quote."
  [root excerpt mark]
  (let [nodes (text-nodes root)
        full  (apply str (map #(.-textContent %) nodes))
        hit   (ws-flexible-index full excerpt)]
    (when hit
      (let [s (locate nodes (:start hit) false)
            e (locate nodes (+ (:start hit) (:len hit)) true)]
        (when (and s e)
          (let [range (.createRange js/document)]
            (.setStart range (:node s) (:offset s))
            (.setEnd range (:node e) (:offset e))
            (.appendChild mark (.extractContents range))
            (.insertNode range mark)))))))

(defn- paint! [el text highlights dark? order selected-id]
  (set! (.-innerHTML el) (markdown/render-html text))
  (doseq [h highlights]
    (let [color (viz/node-color dark? order (:module h))]
      (wrap-excerpt! el (:excerpt h) (make-mark h color (= selected-id (:entry-id h)))))))

(defn- rendered-view []
  (let [store (r/atom {})]
    (r/create-class
     {:display-name "scenario-rendered"
      :component-did-mount
      (fn [this] (let [{:keys [text highlights dark? order selected]} (r/props this)]
                   (when-let [el (:el @store)] (paint! el text highlights dark? order selected))))
      :component-did-update
      (fn [this _] (let [{:keys [text highlights dark? order selected]} (r/props this)]
                     (when-let [el (:el @store)] (paint! el text highlights dark? order selected))))
      :reagent-render
      (fn [_] [:div.scenario-rendered {:ref #(when % (swap! store assoc :el %))}])})))

(defn- run-controls []
  (let [run   @(rf/subscribe [:scenario/run])
        model @(rf/subscribe [:llm/active-model-label])
        text  @(rf/subscribe [:scenario/raw-text])
        running? (= :running (:status run))]
    [:div.scenario-actions
     (if running?
       [:<>
        [:span.run-progress.run-spinner
         (str "Mapping… chunk " (inc (:done-chunks run)) "/" (:chunks run)
              " (this can take a while on a local model)")]
        [:button.btn.btn-danger {:on-click #(rf/dispatch [:mapping/cancel])} "Cancel"]]
       [:button.btn.btn-primary {:disabled (str/blank? text)
                                 :title (if model (str "Map with " model) "Choose a model in Settings")
                                 :on-click #(rf/dispatch [:mapping/run])}
        "▶ Run mapping"])]))

(defn pane []
  (let [text     @(rf/subscribe [:scenario/raw-text])
        rendered? @(rf/subscribe [:scenario/rendered?])
        highlights @(rf/subscribe [:scenario/highlights])
        selected  @(rf/subscribe [:scenario/selected-entry-id])
        order    @(rf/subscribe [:ontology/module-order])
        pref     @(rf/subscribe [:ui/theme-pref])]
    [:div.scenario-pane
     [:div.scenario-toolbar
      [:button.chip {:on-click #(rf/dispatch [:scenario/load-file])} "Upload .txt/.md"]
      [:div.seg-toggle
       [:button.chip {:class (when-not rendered? "chip-active")
                      :on-click #(when rendered? (rf/dispatch [:scenario/toggle-rendered]))} "Raw"]
       [:button.chip {:class (when rendered? "chip-active")
                      :on-click #(when-not rendered? (rf/dispatch [:scenario/toggle-rendered]))} "Rendered"]]
      [:div.toolbar-spacer]
      [run-controls]]
     [:div.scenario-body
      (if (and rendered? (seq text))
        [rendered-view {:text text :highlights highlights :dark? (resolved-dark? pref) :order order :selected selected}]
        [:textarea.scenario-input
         {:value text
          :placeholder "Paste or upload a scenario (plain text or Markdown, LaTeX math with $…$).\n\nThen choose an Ollama model in Settings (⚙) and press Run mapping."
          :on-change #(rf/dispatch [:scenario/set-text (.. % -target -value)])}])]]))
