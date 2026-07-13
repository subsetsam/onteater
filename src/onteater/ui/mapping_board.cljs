(ns onteater.ui.mapping-board
  "The mapping board — progressive disclosure in three linked levels:
   Level 1  a summary strip (headline counts, coverage bar, per-module bar chart);
   Level 2  entry cards grouped/filtered, each linking to the scenario + ontology;
   Level 3  entry detail with the LLM rationale and Accept/Reject/Force/Remap.
  Nothing here trusts the LLM: entries flagged :invalid-target / :excerpt-not-found
  are shown with a visible warning and a one-click repair path."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.viz.common :as viz]))

(defn- dark? []
  (let [pref @(rf/subscribe [:ui/theme-pref])]
    (case pref :dark true :light false
          (boolean (and (exists? js/window) (.-matches (js/matchMedia "(prefers-color-scheme: dark)")))))))

(defn- conf-pill [c]
  [:span.conf-pill {:class (cond (>= c 0.75) "conf-high" (>= c 0.4) "conf-med" :else "conf-low")}
   (str (js/Math.round (* 100 c)) "%")])

(defn- status-icon [status]
  (case status
    :accepted "✓" :rejected "✕" :forced "🔒" "•"))

;; --- Level 1: summary strip -------------------------------------------------

(defn- summary-strip []
  (let [s @(rf/subscribe [:scenario/summary])
        order @(rf/subscribe [:ontology/module-order])
        d (dark?)]
    (when s
      [:div.summary-strip
       [:div.summary-heads
        [:div.summary-stat [:div.stat-num (:entries s)] [:div.stat-label "entries"]]
        [:div.summary-stat [:div.stat-num (:nodes s)] [:div.stat-label "nodes"]]
        [:div.summary-stat [:div.stat-num (:modules s)] [:div.stat-label "modules"]]
        [:div.summary-coverage
         [:div.stat-label (str "scenario coverage " (js/Math.round (* 100 (:coverage s))) "%")]
         [:div.coverage-track [:div.coverage-fill {:style {:width (str (* 100 (:coverage s)) "%")}}]]]]
       (when (seq (:per-module s))
         (let [maxc (apply max (map :count (:per-module s)))]
           [:div.module-bars
            (for [{:keys [module count]} (:per-module s)]
              ^{:key module}
              [:div.module-bar-row
               [:span.module-bar-label module]
               [:div.module-bar-track
                [:div.module-bar-fill {:style {:width (str (* 100 (/ count maxc)) "%")
                                               :background (viz/node-color d order module)}}]]
               [:span.module-bar-count count]])]))])))

;; --- Level 2: board cards ---------------------------------------------------

(defn- entry-card [e selected-id d order]
  [:div.entry-card {:class (str (when (= selected-id (:id e)) "entry-selected")
                                (when (seq (:flags e)) " entry-flagged"))
                    :on-click #(rf/dispatch [:scenario/select-entry (:id e)])}
   [:div.entry-card-main
    [:span.entry-excerpt (str "“" (subs (:excerpt e) 0 (min 46 (count (:excerpt e))))
                              (when (> (count (:excerpt e)) 46) "…") "”")]
    [:span.entry-arrow "→"]
    [:span.entry-node {:style {:border-color (viz/node-color d order (:module e))}}
     (:node-label e)]]
   [:div.entry-card-meta
    [conf-pill (:confidence e)]
    [:span.entry-status {:class (str "status-" (name (:status e)))} (status-icon (:status e))]
    (when (seq (:flags e)) [:span.entry-flag "⚠"])]])

(defn- board-controls []
  (let [board @(rf/subscribe [:scenario/board])]
    [:div.board-controls
     [:div.seg-toggle
      (for [[g label] [[:module "Module"] [:confidence "Confidence"] [:scenario "Order"]]]
        ^{:key g}
        [:button.chip.chip-sm {:class (when (= g (or (:group-by board) :module)) "chip-active")
                               :on-click #(rf/dispatch [:scenario/set-board-group g])} label])]
     [:label.conf-filter "min conf "
      [:input {:type "range" :min 0 :max 1 :step 0.05
               :value (or (:min-confidence board) 0)
               :on-change #(rf/dispatch [:scenario/set-confidence-filter
                                         (js/parseFloat (.. % -target -value))])}]]]))

(defn- empty-board-message []
  (let [run @(rf/subscribe [:scenario/run])]
    [:div.board-empty
     (case (:status run)
       :done (if (:schema-ignored? run)
               "The model returned output that didn't match the response schema.
                MLX builds don't enforce structured outputs — try the GGUF build of
                this model (drop the “-mlx-…” suffix), or a more capable model."
               "The model returned no valid mappings. Small models sometimes ignore
                the response schema — try a more capable model in Settings (⚙).")
       :error (str "The mapping run failed. " (:error run))
       "No entries yet. Enter a scenario and press Run mapping to populate the board.")]))

;; --- Level 3: entry detail (force uses inline node search) ------------------

(defn- force-picker [entry-id]
  (let [q (r/atom "")]
    (fn [entry-id]
      (let [results (when (>= (count @q) 2)
                      @(rf/subscribe [:outline/search-results-for @q]))]
        [:div.force-picker
         [:input.insp-input {:placeholder "Search a node to force…"
                             :value @q :on-change #(reset! q (.. % -target -value))}]
         (when (seq results)
           [:ul.force-results
            (for [n (take 8 results)]
              ^{:key (:id n)}
              [:li.force-result {:on-click #(do (rf/dispatch [:mapping/force-entry entry-id (:id n)])
                                                (reset! q ""))}
               [:span.force-result-label (:label n)]
               [:span.force-result-id (:id n)]])])]))))

(defn- entry-detail []
  (let [e @(rf/subscribe [:scenario/selected-entry])]
    (when e
      [:div.entry-detail
       [:div.detail-head
        [:h4 "Entry"]
        [:button.icon-btn {:on-click #(rf/dispatch [:scenario/select-entry nil])} "×"]]
       [:div.detail-excerpt (str "“" (:excerpt e) "”")]
       (when (seq (:flags e))
         [:div.detail-flags
          (for [f (:flags e)]
            ^{:key f}
            [:div.detail-flag
             (case f
               :invalid-target "⚠ Target node does not exist in the ontology."
               :excerpt-not-found "⚠ Excerpt not found in the scenario text."
               (str "⚠ " (name f)))])])
       [:div.detail-target
        [:span.detail-label "maps to"]
        [:span.detail-node (or (get-in e [:node :label]) (:node-id e))]
        [:span.detail-node-id (:node-id e)]]
       (when-let [gloss (get-in e [:node :gloss])]
         [:div.detail-gloss gloss])
       [:div.detail-row
        [:span.detail-label "relation"] [:span (name (:relation e))]
        [:span.detail-label "confidence"] [conf-pill (:confidence e)]]
       (when (seq (:rationale e))
         [:div.detail-rationale [:span.detail-label "rationale"] [:p (:rationale e)]])
       [:div.detail-actions
        [:button.btn.btn-sm {:class (when (= :accepted (:status e)) "btn-primary")
                             :on-click #(rf/dispatch [:mapping/set-entry-status (:id e) :accepted])} "Accept"]
        [:button.btn.btn-sm {:class (when (= :rejected (:status e)) "btn-danger")
                             :on-click #(rf/dispatch [:mapping/set-entry-status (:id e) :rejected])} "Reject"]
        [:button.btn.btn-sm {:on-click #(rf/dispatch [:chat/discuss-entry (:id e)])} "Discuss in chat"]]
       [:div.detail-force
        [:span.detail-label "force to a node of your choice"]
        [force-picker (:id e)]]])))

;; --- Level 2: board cards (renders the selected entry's detail inline) ------

(defn- board []
  (let [groups @(rf/subscribe [:scenario/board-entries])
        selected @(rf/subscribe [:scenario/selected-entry-id])
        order @(rf/subscribe [:ontology/module-order])
        d (dark?)]
    [:div.board
     [board-controls]
     [:div.board-groups
      (if (seq (mapcat second groups))
        (for [[grp entries] groups]
          ^{:key grp}
          [:div.board-group
           [:div.board-group-head grp [:span.board-group-count (count entries)]]
           ;; The Accept/Reject detail is rendered inline, directly under the
           ;; selected card, rather than in a separate panel below the whole board.
           (for [e entries]
             ^{:key (:id e)}
             [:div.entry-slot
              [entry-card e selected d order]
              (when (= selected (:id e))
                [entry-detail])])])
        [empty-board-message])]]))

(defn view []
  [:div.mapping-board-wrap
   [summary-strip]
   [:div.board-and-detail
    [board]]])
