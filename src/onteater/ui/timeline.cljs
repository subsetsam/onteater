(ns onteater.ui.timeline
  "The Timeline and Gaps tabs of the scenario workspace's center pane.

  - Timeline tab: the swimlane DAG view (`onteater.viz.timeline`) plus a toolbar
    (lane grouping · dependency-cone toggle · run/cancel the extraction pass ·
    export), a dependency-cone side panel (the readable paths), the selected
    event/relation detail with the same Accept·Reject·Force·Remap·Discuss actions as
    mapping entries, and — for large scenarios — the entity dependency matrix.
  - Gaps tab: the measured completeness report (§6.7.4). Every untyped hole offers
    ‘Draft ontology element from this gap…’ (jumps to use case A with a pre-filled
    add-node dialog); a ‘Re-run timeline pass’ button then reports which gaps
    cleared; the report exports as Markdown.

  All timeline state is read through subscriptions; every action dispatches — the
  view holds no domain logic."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.ui.scenario :as scenario]
            [onteater.viz.timeline :as viz-tl]
            [onteater.viz.common :as viz]))

(defn- dark? []
  (let [pref @(rf/subscribe [:ui/theme-pref])]
    (case pref :dark true :light false
          (boolean (and (exists? js/window) (.-matches (js/matchMedia "(prefers-color-scheme: dark)")))))))

(defn- conf-pill [c]
  [:span.conf-pill {:class (cond (>= c 0.75) "conf-high" (>= c 0.4) "conf-med" :else "conf-low")}
   (str (js/Math.round (* 100 (or c 0))) "%")])

;; --- run controls -----------------------------------------------------------

(defn- timeline-progress
  "Live 'Extracting timeline…' status while the pass is in flight — the timeline
  counterpart of `onteater.ui.scenario/mapping-progress`. A single 250 ms interval
  drives the space-padded dots and the elapsed-time clock (recomputed from the run's
  `:started-at`), and is cleared on unmount. Reuses `scenario/format-elapsed` so the
  hh:mm:ss format stays identical to the Run mapping timer."
  [_run]
  (let [tick  (r/atom 0)
        timer (atom nil)]
    (r/create-class
     {:display-name "timeline-progress"
      :component-did-mount
      (fn [_] (reset! timer (js/setInterval #(swap! tick inc) 250)))
      :component-will-unmount
      (fn [_] (when-let [t @timer] (js/clearInterval t)))
      :reagent-render
      (fn [run]
        (let [n       (mod @tick 4)
              dots    (str (apply str (repeat n ".")) (apply str (repeat (- 3 n) " ")))
              elapsed (quot (- (.now js/Date) (:started-at run)) 1000)]
          [:span.run-progress.run-spinner
           "Extracting timeline" [:span.run-dots dots]
           (str " chunk " (inc (:done-chunks run)) "/" (:chunks run)
                " (elapsed time: " (scenario/format-elapsed elapsed) ")")]))})))

(defn- run-controls []
  (let [run   @(rf/subscribe [:timeline/run])
        text  @(rf/subscribe [:scenario/raw-text])
        model @(rf/subscribe [:llm/active-model-label])
        running? (= :running (:status run))
        ;; Keep the elapsed time on screen after a finished run (as the mapping
        ;; timer does). `:ended-at` is set on completion/error, not on cancel.
        done-secs (when (and (not running?) (:started-at run) (:ended-at run))
                    (quot (- (:ended-at run) (:started-at run)) 1000))]
    [:div.tl-run
     (cond
       running?
       [:<>
        [timeline-progress run]
        [:button.btn.btn-danger.btn-sm {:on-click #(rf/dispatch [:timeline/cancel])} "Cancel"]]
       :else
       [:button.btn.btn-primary.btn-sm
        {:disabled (str/blank? text)
         :title (if model (str "Extract events with " model) "Choose a model in Settings")
         :on-click #(rf/dispatch [:timeline/run])}
        "▶ Run timeline pass"])
     (when done-secs
       [:span.run-note
        (str (if (= :error (:status run)) "Timeline failed" "Timeline complete")
             " (elapsed time: " (scenario/format-elapsed done-secs) ")")])
     (when (and (= :done (:status run)) (:pre-gap run))
       [:span.run-note (str (:post-gap run) " gap(s)"
                            (when (pos? (:cleared run)) (str " · cleared " (:cleared run))))])]))

(defn- toolbar []
  (let [grouping @(rf/subscribe [:timeline/grouping])
        cone? @(rf/subscribe [:timeline/cone?])]
    [:div.tl-toolbar
     [:div.seg-toggle
      (for [[g label] [[:entity "Entity"] [:module "Module"] [:episode "Episode"] [:single "Single"]]]
        ^{:key g}
        [:button.chip.chip-sm {:class (when (= g grouping) "chip-active")
                               :on-click #(rf/dispatch [:timeline/set-grouping g])} label])]
     [:button.chip.chip-sm {:class (when cone? "chip-active")
                            :title "Dependency-cone mode (d): dim all but the selection's ancestors/descendants"
                            :on-click #(rf/dispatch [:timeline/toggle-cone])} "⤢ Cone (d)"]
     [:div.toolbar-spacer]
     [run-controls]
     [:button.chip.chip-sm {:on-click #(rf/dispatch [:timeline/export-svg])} "SVG"]
     [:button.chip.chip-sm {:on-click #(rf/dispatch [:timeline/export-png])} "PNG"]]))

;; --- force / remap picker (reuses the ontology search) ----------------------

(defn- node-picker [on-pick placeholder]
  (let [q (r/atom "")]
    (fn [on-pick placeholder]
      (let [results (when (>= (count @q) 2) @(rf/subscribe [:outline/search-results-for @q]))]
        [:div.force-picker
         [:input.insp-input {:placeholder placeholder :value @q
                             :on-change #(reset! q (.. % -target -value))}]
         (when (seq results)
           [:ul.force-results
            (for [n (take 8 results)]
              ^{:key (:id n)}
              [:li.force-result {:on-click #(do (on-pick (:id n)) (reset! q ""))}
               [:span.force-result-label (:label n)]
               [:span.force-result-id (:id n)]])])]))))

;; --- selected event / relation detail ---------------------------------------

(defn- event-detail []
  (let [e @(rf/subscribe [:timeline/selected-event])]
    (when e
      [:div.tl-detail
       [:div.detail-head
        [:h4 (if (:node-id e) "Event" "Event (untyped — a gap)")]
        [:button.icon-btn {:on-click #(rf/dispatch [:timeline/select-event nil])} "×"]]
       [:div.detail-excerpt (str "“" (:label e) "”")]
       (when (seq (:excerpt e)) [:div.tl-detail-excerpt (str "quote: “" (:excerpt e) "”")])
       (if (:node-id e)
         [:div.detail-target
          [:span.detail-label "typed as"]
          [:span.detail-node (or (get-in e [:node :label]) (:node-id e))]
          [:span.detail-node-id (:node-id e)]]
         [:div.tl-gap-note
          [:div "No fitting occurrent class — nearest: " [:code (or (:nearest e) "?")]]
          (when (:why-no-fit e) [:div.tl-why (:why-no-fit e)])
          [:button.btn.btn-sm {:on-click #(rf/dispatch [:timeline/draft-from-gap
                                                        {:label (:label e) :nearest (:nearest e)
                                                         :why-no-fit (:why-no-fit e)}])}
           "Draft ontology element…"]])
       (when (seq (:participants e))
         [:div.tl-participants
          [:span.detail-label "participants"]
          (for [[i p] (map-indexed vector (:participants e))]
            ^{:key i}
            [:div.tl-participant
             [:span.tl-part-entity (:entity p)]
             (when (:role-id p) [:span.tl-part-role (:role-id p)])
             (when-not (:role-id p) [:span.tl-part-unroled "unroled ⚠"])])])
       [:div.detail-row [:span.detail-label "confidence"] [conf-pill (:confidence e)]]
       (when (seq (:rationale e)) [:div.detail-rationale [:p (:rationale e)]])
       [:div.detail-actions
        [:button.btn.btn-sm {:class (when (= :accepted (:status e)) "btn-primary")
                             :on-click #(rf/dispatch [:timeline/set-event-status (:id e) :accepted])} "Accept"]
        [:button.btn.btn-sm {:class (when (= :rejected (:status e)) "btn-danger")
                             :on-click #(rf/dispatch [:timeline/set-event-status (:id e) :rejected])} "Reject"]
        [:button.btn.btn-sm {:on-click #(rf/dispatch [:chat/timeline-action :depends])} "What depends on this?"]
        [:button.btn.btn-sm {:on-click #(rf/dispatch [:chat/timeline-action :trace])} "Trace chain"]
        (when (nil? (:node-id e))
          [:button.btn.btn-sm {:on-click #(rf/dispatch [:chat/timeline-action :why-untyped])} "Why untyped?"])]
       [:div.detail-force
        [:span.detail-label "type this event (force / remap)"]
        [node-picker #(rf/dispatch [:timeline/force-event (:id e) %]) "Search an occurrent class…"]]])))

(def ^:private rel-type-options
  [[:precedes "precedes"] [:causes "causes"] [:enables "enables"]
   [:responds-to "responds to"] [:terminates "terminates"] [:part-of "part of"]])

(defn- relation-detail []
  (let [r @(rf/subscribe [:timeline/selected-relation])]
    (when r
      [:div.tl-detail
       [:div.detail-head [:h4 "Relation"]
        [:button.icon-btn {:on-click #(rf/dispatch [:timeline/select-relation nil])} "×"]]
       [:div.detail-row [:span.detail-label "type"]
        [:select {:value (name (:type r))
                  :on-change #(rf/dispatch [:timeline/force-relation (:id r)
                                            (keyword (.. % -target -value)) (:property-id r)])}
         (for [[k label] rel-type-options] ^{:key k} [:option {:value (name k)} label])]]
       [:div.detail-row [:span.detail-label "property"]
        (if (:property-id r) [:code (:property-id r)] [:span.tl-part-unroled "untyped ⚠"])]
       [:div.detail-actions
        [:button.btn.btn-sm {:class (when (= :accepted (:status r)) "btn-primary")
                             :on-click #(rf/dispatch [:timeline/set-relation-status (:id r) :accepted])} "Accept"]
        [:button.btn.btn-sm {:class (when (= :rejected (:status r)) "btn-danger")
                             :on-click #(rf/dispatch [:timeline/set-relation-status (:id r) :rejected])} "Reject"]
        [:button.btn.btn-sm.btn-danger {:on-click #(rf/dispatch [:timeline/remove-relation (:id r)])} "Remove"]]])))

;; --- dependency-cone side panel ---------------------------------------------

(defn- cone-panel []
  (let [cone @(rf/subscribe [:timeline/cone])
        paths @(rf/subscribe [:timeline/cone-paths])]
    (when cone
      [:div.tl-cone-panel
       [:h4 "Dependency cone"]
       [:div.tl-cone-counts
        [:span.tl-cone-up (str (count (:ancestors cone)) " upstream")]
        [:span.tl-cone-down (str (count (:descendants cone)) " downstream")]]
       (when (seq (:upstream paths))
         [:div.tl-paths [:div.detail-label "what it depended on"]
          (for [[i p] (map-indexed vector (:upstream paths))] ^{:key i} [:div.tl-path p])])
       (when (seq (:downstream paths))
         [:div.tl-paths [:div.detail-label "what depends on it"]
          (for [[i p] (map-indexed vector (:downstream paths))] ^{:key i} [:div.tl-path p])])])))

;; --- entity dependency matrix (large scenarios) -----------------------------

(defn- matrix-panel []
  (let [open (r/atom false)]
    (fn []
      (let [{:keys [entities cells]} @(rf/subscribe [:timeline/matrix])]
        (when (> (count entities) 2)
          [:div.tl-matrix-wrap
           [:button.chip.chip-sm {:on-click #(swap! open not)}
            (str (if @open "▾" "▸") " Entity dependency matrix (" (count entities) ")")]
           (when @open
             (let [maxc (apply max 1 (vals cells))]
               [:table.tl-matrix
                [:thead [:tr [:th] (for [c entities] ^{:key c} [:th.tl-matrix-col c])]]
                [:tbody
                 (for [rrow entities]
                   ^{:key rrow}
                   [:tr [:th.tl-matrix-row rrow]
                    (for [c entities]
                      (let [n (get cells [rrow c] 0)]
                        ^{:key c}
                        [:td.tl-matrix-cell
                         {:style {:background (when (pos? n)
                                                (str "rgba(42,120,214," (+ 0.15 (* 0.6 (/ n maxc))) ")"))}
                          :title (str rrow " → " c ": " n)
                          :on-click #(when (pos? n) (rf/dispatch [:timeline/set-matrix-cell [rrow c]]))}
                         (when (pos? n) n)]))])]]))])))))

;; --- the Timeline tab -------------------------------------------------------

(defn- empty-timeline []
  (let [run @(rf/subscribe [:timeline/run])]
    [:div.tl-empty
     (case (:status run)
       :done (if (:schema-ignored? run)
               "The model returned output that didn't match the timeline schema (MLX builds
                don't enforce it) — try a GGUF build or a more capable model."
               "No events were extracted. Try a more capable model, or a scenario with clearer temporal structure.")
       :error (str "The timeline pass failed. " (:error run))
       [:span "No timeline yet. Press " [:strong "Run timeline pass"] " to extract the
               scenario's events and their temporal/causal relations."])]))

(defn timeline-tab []
  (let [layout @(rf/subscribe [:timeline/layout])
        cone   @(rf/subscribe [:timeline/cone])
        sel    @(rf/subscribe [:timeline/selected-event-id])
        ui     @(rf/subscribe [:timeline/ui])
        order  @(rf/subscribe [:ontology/module-order])
        n      @(rf/subscribe [:timeline/event-count])]
    [:div.tl-tab
     [toolbar]
     (if (zero? (or n 0))
       [empty-timeline]
       [:div.tl-body
        [:div.tl-canvas-wrap
         [viz-tl/timeline-view {:layout layout :cone cone :selected sel
                                :dark? (dark?) :module-order order
                                :collapsed (:collapsed ui)}]]
        [:div.tl-side
         [cone-panel]
         [event-detail]
         [relation-detail]
         [matrix-panel]]])]))

;; --- the Gaps tab -----------------------------------------------------------

(defn- gap-group [title items render-item]
  (when (seq items)
    [:div.gap-group
     [:h4.gap-group-head title [:span.gap-count (count items)]]
     (for [[i it] (map-indexed vector items)] ^{:key i} (render-item it))]))

(defn- untyped-event-card [g]
  [:div.gap-nearest-group
   [:div.gap-nearest "nearest: " [:code (:nearest g)] [:span.gap-count (:count g)]]
   (for [it (:items g)]
     ^{:key (str (:id it))}
     [:div.gap-card
      [:div.gap-card-main [:span.gap-label (:label it)]
       [:span.gap-glyph "?"]]
      (when (:why-no-fit it) [:div.gap-why (:why-no-fit it)])
      [:button.btn.btn-sm {:on-click #(rf/dispatch [:timeline/draft-from-gap
                                                    {:label (:label it) :nearest (:nearest g)
                                                     :why-no-fit (:why-no-fit it)}])}
       "Draft ontology element from this gap…"]])])

(defn gaps-tab []
  (let [report @(rf/subscribe [:timeline/gap-report])
        run    @(rf/subscribe [:timeline/run])]
    [:div.gaps-tab
     [:div.gaps-head
      [:div.gaps-title "Gap report"
       [:span.gap-count-total (str @(rf/subscribe [:timeline/gap-count]) " findings")]]
      [:div.toolbar-spacer]
      [:button.chip.chip-sm {:on-click #(rf/dispatch [:timeline/run])
                             :title "Re-run the timeline pass against the (possibly edited) ontology"}
       "↻ Re-run timeline pass"]
      [:button.chip.chip-sm {:on-click #(rf/dispatch [:timeline/export-gaps-md])} "Export .md"]]
     (when (and (= :done (:status run)) (:pre-gap run) (pos? (:cleared run)))
       [:div.gaps-cleared (str "✓ The re-run cleared " (:cleared run) " gap(s).")])
     (if (nil? report)
       [:div.tl-empty "Run the timeline pass to measure where the ontology could not express this scenario."]
       [:div.gaps-body
        [gap-group "Untyped events (no fitting occurrent class)"
         (:untyped-events report) untyped-event-card]
        [gap-group "Untyped relations (no fitting property)"
         (:untyped-relations report)
         (fn [g] [:div.gap-card [:span.gap-label (str (:count g) "× " (:nearest g) " relation(s)")]])]
        [gap-group "Shallow typings (typed above available leaf classes)"
         (:shallow-typings report)
         (fn [e] [:div.gap-card [:span.gap-label (:label e)] [:code (:node-id e)]])]
        [gap-group "Unroled participants"
         (:unroled-participants report)
         (fn [u] [:div.gap-card [:span.gap-label (:participant u)] [:span.gap-in (str "in “" (:label u) "”")]])]
        (let [cov (:coverage report)]
          [:div.gap-coverage
           [:h4.gap-group-head "Coverage"]
           (for [[k label] [[:occurrent "Occurrent classes"] [:properties "Relation properties"] [:roles "Role classes"]]]
             ^{:key k}
             (let [c (get cov k)]
               [:div.gap-cov-row
                [:span.gap-cov-label label]
                [:div.gap-cov-track
                 [:div.gap-cov-fill {:style {:width (str (* 100 (/ (:used c) (max 1 (:total c)))) "%")}}]]
                [:span.gap-cov-num (str (:used c) "/" (:total c))]]))])])]))
