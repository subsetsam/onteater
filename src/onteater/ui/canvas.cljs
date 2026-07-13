(ns onteater.ui.canvas
  "The centre pane: the d3 graph canvas plus its surrounding chrome — layout
  switcher, focus breadcrumbs, hop control, edge/kind filters, module legend, and
  the no-silent-truncation notice. The heavy lifting is in `onteater.viz.common`;
  this namespace is the ordinary Reagent view around it."
  (:require [re-frame.core :as rf]
            [onteater.viz.common :as viz]))

(defn resolve-dark?
  "Turn the theme preference into a boolean for the graph palette. :auto follows
  the OS via matchMedia."
  [pref]
  (case pref
    :dark true
    :light false
    (boolean (and (exists? js/window)
                  (.-matches (js/matchMedia "(prefers-color-scheme: dark)"))))))

(def ^:private layouts
  [[:cluster "Clustered" "Modules gravitate into spatial groups"]
   [:force   "Force"     "General force-directed exploration"]
   [:tree    "Tree"      "Tidy subclass hierarchy from roots"]
   [:radial  "Radial"    "Compact radial subclass tree"]])

(defn- layout-switcher []
  (let [current @(rf/subscribe [:view/layout])]
    [:div.layout-switcher
     (for [[id label title] layouts]
       ^{:key id}
       [:button.chip {:class (when (= id current) "chip-active")
                      :title title
                      :on-click #(rf/dispatch [:view/set-layout id])}
        label])]))

(defn- breadcrumbs []
  (let [crumbs @(rf/subscribe [:view/breadcrumbs])
        spec   @(rf/subscribe [:view/spec])]
    [:div.breadcrumbs
     [:button.crumb {:on-click #(rf/dispatch [:view/reset-overview])
                     :title "Back to module overview"} "⌂ Overview"]
     (map-indexed
      (fn [i {:keys [label]}]
        ^{:key i}
        [:span.crumb-wrap
         [:span.crumb-sep "›"]
         [:button.crumb {:on-click #(rf/dispatch [:view/breadcrumb-to i])} label]])
      crumbs)
     (when (not= :overview (:mode spec))
       [:span.crumb-wrap [:span.crumb-sep "›"]
        [:span.crumb.crumb-current
         (case (:mode spec)
           :neighborhood "current focus"
           :subtree "subtree"
           :module "module"
           :custom "selection"
           "view")]])]))

(defn- hop-control []
  (let [spec @(rf/subscribe [:view/spec])]
    (when (= :neighborhood (:mode spec))
      [:div.hop-control
       [:span.hop-label "Hops"]
       (for [n [1 2 3]]
         ^{:key n}
         [:button.chip.chip-sm {:class (when (= n (:hops spec)) "chip-active")
                                :on-click #(rf/dispatch [:view/set-hops n])}
          (str n)])])))

(defn- filters []
  (let [spec @(rf/subscribe [:view/spec])]
    [:div.canvas-filters
     [:div.filter-group
      [:span.filter-label "Edges"]
      (for [[t label] [[:subclass-of "subclass"] [:module-membership "membership"]]]
        ^{:key t}
        [:label.toggle
         [:input {:type "checkbox"
                  :checked (contains? (:edge-types spec) t)
                  :on-change #(rf/dispatch [:view/toggle-edge-type t])}]
         label])]
     [:div.filter-group
      [:span.filter-label "Kinds"]
      (for [[k label] [[:class "class"] [:property "property"] [:individual "individual"]]]
        ^{:key k}
        [:label.toggle
         [:input {:type "checkbox"
                  :checked (contains? (:kinds spec) k)
                  :on-change #(rf/dispatch [:view/toggle-kind k])}]
         label])]]))

(defn- legend [dark? order nodes]
  (let [entries (viz/legend-entries dark? order nodes)]
    (when (seq entries)
      [:div.legend
       (for [{:keys [module color]} entries]
         ^{:key module}
         [:div.legend-item
          [:span.legend-swatch {:style {:background color}}]
          [:span.legend-label module]])])))

(defn- context-menu []
  (let [cm @(rf/subscribe [:ui/context-menu])]
    (when cm
      (let [{:keys [x y node-id meta? external?]} cm
            close #(rf/dispatch [:ui/close-context-menu])
            item (fn [label ev]
                   [:button.ctx-item {:on-click #(do (close) (rf/dispatch ev))} label])]
        [:div.ctx-overlay {:on-click close :on-context-menu #(.preventDefault %)}
         [:div.ctx-menu {:style {:left x :top y}
                         :on-click #(.stopPropagation %)}
          (if meta?
            [item "Expand module" [:view/expand-group (subs node-id (count "group:"))]]
            [:<>
             [item "Focus neighbourhood" [:view/focus-node node-id]]
             [item "Show subtree" [:view/show-subtree node-id]]
             [item "Show ancestors" [:view/show-ancestors node-id]]
             (when-not external?
               [:<>
                [:div.ctx-divider]
                [item "Add subclass" [:ontology/add-child node-id]]
                [item "Delete…" [:ontology/request-delete-node node-id]]])])]]))))

(defn view []
  (let [vg    @(rf/subscribe [:view/visible-graph])
        spec  @(rf/subscribe [:view/spec])
        sel   @(rf/subscribe [:ontology/selection])
        fopts @(rf/subscribe [:ontology/force-opts])
        mo    @(rf/subscribe [:ontology/module-order])
        pref  @(rf/subscribe [:ui/theme-pref])
        dark? (resolve-dark? pref)]
    [:div.canvas-pane
     [:div.canvas-toolbar
      [:button.chip.chip-back {:on-click #(rf/dispatch [:view/breadcrumb-back])
                               :title "Back"} "←"]
      [layout-switcher]
      [hop-control]
      [:button.chip.chip-add {:on-click #(rf/dispatch [:ontology/add-node {:kind :class :label "New class"}])
                              :title "Add a new class"} "+ Class"]
      [:div.toolbar-spacer]
      [filters]]
     [breadcrumbs]
     [:div.canvas-host
      (if (seq (:nodes vg))
        ^{:key "gv"} [viz/graph-view {:vg vg :view-spec spec :selection sel
                                      :dark? dark? :force-opts fopts :module-order mo}]
        [:div.canvas-empty
         [:p "Nothing to display for this view."]
         [:button.btn {:on-click #(rf/dispatch [:view/reset-overview])} "Reset to overview"]])
      [legend dark? mo (:nodes vg)]
      (when (:truncated vg)
        [:div.trunc-notice
         (str (:hidden (:truncated vg))
              " nodes hidden by the view cap — narrow the focus to see them.")])]
     [context-menu]]))
