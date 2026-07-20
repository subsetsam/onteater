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

(defn format-elapsed
  "Whole seconds -> \"hh:mm:ss\" (zero-padded). Public so the timeline run controls
  (`onteater.ui.timeline`) reuse the same format as the mapping timer."
  [secs]
  (let [pad (fn [n] (if (< n 10) (str "0" n) (str n)))]
    (str (pad (quot secs 3600)) ":"
         (pad (mod (quot secs 60) 60)) ":"
         (pad (mod secs 60)))))

(defn- mapping-progress
  "Live \"Mapping…\" status while a run is in flight. A single 250 ms interval
  drives both animations: the space-padded dots cycle \"   \"→\".  \"→\".. \"→\"...\"
  (one full cycle per second, held to constant width by `.run-dots`) and the
  elapsed-time clock, recomputed from the run's `:started-at`, ticks visibly once
  per second. The interval is cleared on unmount (run finished or cancelled)."
  [_run]
  (let [tick  (r/atom 0)
        timer (atom nil)]
    (r/create-class
     {:display-name "mapping-progress"
      :component-did-mount
      (fn [_] (reset! timer (js/setInterval #(swap! tick inc) 250)))
      :component-will-unmount
      (fn [_] (when-let [t @timer] (js/clearInterval t)))
      :reagent-render
      (fn [run]
        (let [n       (mod @tick 4)
              dots    (str (apply str (repeat n ".")) (apply str (repeat (- 3 n) " ")))
              elapsed (quot (- (.now js/Date) (:started-at run)) 1000)
              refine? (= :refine (:phase run))]
          [:span.run-progress.run-spinner
           (if refine? "Refining" "Mapping") [:span.run-dots dots]
           (str (if refine?
                  (str " batch " (inc (:refine-done run 0)) "/" (:refine-total run))
                  (str " chunk " (inc (:done-chunks run)) "/" (:chunks run)))
                (when-let [s (:strategy run)]
                  (when (not= :full s) (str " [" (name s) "]")))
                " (elapsed time: " (format-elapsed elapsed) ")")]))})))

(defn- animated-dots
  "A standalone \"…\" that cycles \"   \"→\".  \"→\".. \"→\"...\" once per second,
  held to constant width by `.run-dots`. Same 250 ms interval and cadence as
  `mapping-progress`; the interval is cleared on unmount."
  []
  (let [tick  (r/atom 0)
        timer (atom nil)]
    (r/create-class
     {:display-name "animated-dots"
      :component-did-mount
      (fn [_] (reset! timer (js/setInterval #(swap! tick inc) 250)))
      :component-will-unmount
      (fn [_] (when-let [t @timer] (js/clearInterval t)))
      :reagent-render
      (fn []
        (let [n (mod @tick 4)]
          [:span.run-dots
           (str (apply str (repeat n ".")) (apply str (repeat (- 3 n) " ")))]))})))

(defn- strategy-select
  "How the ontology is presented to the mapping LLM: auto (size-gated), full
  (whole compaction), scoped (lexically scoped per chunk), or staged (coarse pass
  + per-branch refinement)."
  []
  (let [strategy @(rf/subscribe [:scenario/strategy])]
    [:select.chip {:value (if strategy (name strategy) "auto")
                   :title "Ontology presentation strategy for the mapping run"
                   :on-change #(let [v (.. % -target -value)]
                                 (rf/dispatch [:mapping/set-strategy
                                               (when (not= "auto" v) (keyword v))]))}
     [:option {:value "auto"} "Strategy: auto"]
     [:option {:value "full"} "Strategy: full"]
     [:option {:value "scoped"} "Strategy: scoped"]
     [:option {:value "staged"} "Strategy: staged"]]))

(defn briefing-panel
  "The ontology-briefing artifact: generated once per ontology by the LLM
  (`:briefing/run`), validated, then EDITED here by the user; the text is
  injected into every mapping prompt. Collapsed to a toolbar chip until opened."
  []
  (let [open? (r/atom false)]
    (fn []
      (let [briefing @(rf/subscribe [:ontology/briefing])
            brun     @(rf/subscribe [:ontology/briefing-run])
            running? (= :running (:status brun))]
        [:div.briefing-wrap
         [:button.chip
          {:class (when @open? "chip-active")
           :title (if briefing
                    "Review/edit the ontology briefing injected into mapping prompts"
                    "Generate a curated briefing for this ontology (one LLM call)")
           :on-click #(swap! open? not)}
          (cond running? "Briefing…"
                briefing "Briefing ✓"
                :else "Briefing")]
         (when @open?
           [:div.briefing-panel
            (cond
              running?
              [:<>
               [:p.hint "Generating briefing" [animated-dots]]
               [:button.btn.btn-danger
                {:title (if briefing
                          "Cancel; mapping keeps using the current briefing"
                          "Cancel; mapping runs without a briefing")
                 :on-click #(rf/dispatch [:briefing/cancel])}
                "Cancel"]]
              (nil? briefing)
              [:<>
               [:p.hint (str "A briefing is a one-time LLM pass over the loaded ontology: "
                             "module summaries and disambiguation rules, validated and "
                             "then curated by you. It is injected into every mapping run.")]
               (when (= :error (:status brun)) [:p.hint.error (:error brun)])
               [:button.btn.btn-primary {:on-click #(rf/dispatch [:briefing/run])}
                "Generate briefing"]]
              :else
              [:<>
               [:textarea.briefing-input
                {:value (:text briefing)
                 :on-change #(rf/dispatch [:briefing/set-text (.. % -target -value)])}]
               [:div.scenario-actions
                [:button.btn {:on-click #(rf/dispatch [:briefing/run])} "Regenerate"]
                [:button.btn {:on-click #(rf/dispatch [:briefing/clear])} "Remove"]]])])]))))

(defn- run-controls []
  (let [run   @(rf/subscribe [:scenario/run])
        model @(rf/subscribe [:llm/active-model-label])
        text  @(rf/subscribe [:scenario/raw-text])
        session @(rf/subscribe [:scenario/active-session])
        running? (= :running (:status run))
        ;; After a finished run, keep the elapsed time on screen so the user can
        ;; see how long it took. `:ended-at` is set only on completion/error
        ;; (not on cancel), and `:scenario/clear` drops the run map entirely.
        done-secs (when (and (not running?) (:started-at run) (:ended-at run))
                    (quot (- (:ended-at run) (:started-at run)) 1000))]
    [:div.scenario-actions
     (when done-secs
       [:span.run-progress
        (str (if (= :error (:status run)) "Mapping failed" "Mapping complete")
             " (elapsed time: " (format-elapsed done-secs) ")")])
     (when (and (not running?) (:ctx-overflow? run))
       [:span.run-progress {:title (str "The assembled prompt exceeded the maximum "
                                        "context window; the model may not have seen "
                                        "all of it. Try the scoped or staged strategy.")}
        "⚠ context overflow"])
     (if running?
       [:<>
        [mapping-progress run]
        [:button.btn.btn-danger {:on-click #(rf/dispatch [:mapping/cancel])} "Cancel"]]
       [:<>
        [strategy-select]
        [:button.btn {:disabled (and (str/blank? text) (nil? session))
                      :title "Clear the scenario text and mapping board"
                      :on-click #(rf/dispatch [:scenario/clear])}
         "Clear"]
        [:button.btn.btn-primary {:disabled (str/blank? text)
                                  :title (if model (str "Map with " model) "Choose a model in Settings")
                                  :on-click #(rf/dispatch [:mapping/run])}
         "▶ Run mapping"]])]))

(defn pane []
  (let [text     @(rf/subscribe [:scenario/raw-text])
        rendered? @(rf/subscribe [:scenario/rendered?])
        highlights @(rf/subscribe [:scenario/highlights])
        selected  @(rf/subscribe [:scenario/selected-entry-id])
        order    @(rf/subscribe [:ontology/module-order])
        pref     @(rf/subscribe [:ui/theme-pref])]
    [:div.scenario-pane
     [:div.scenario-toolbar
      [:button.chip {:on-click #(rf/dispatch [:scenario/load-file])} "Upload txt/md"]
      [:div.seg-toggle
       [:button.chip {:class (when-not rendered? "chip-active")
                      :on-click #(when rendered? (rf/dispatch [:scenario/toggle-rendered]))} "Raw"]
       [:button.chip {:class (when rendered? "chip-active")
                      :on-click #(when-not rendered? (rf/dispatch [:scenario/toggle-rendered]))} "Rendered"]]
      [:div.toolbar-spacer]
      [briefing-panel]
      [run-controls]]
     [:div.scenario-body
      (if (and rendered? (seq text))
        [rendered-view {:text text :highlights highlights :dark? (resolved-dark? pref) :order order :selected selected}]
        [:textarea.scenario-input
         {:value text
          :placeholder "Paste or upload a scenario (plain text or Markdown, LaTeX math with $…$).\n\nThen choose an Ollama model in Settings (⚙) and press Run mapping."
          :on-change #(rf/dispatch [:scenario/set-text (.. % -target -value)])}])]]))
