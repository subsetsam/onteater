(ns onteater.ui.outline
  "Left pane: live search plus a collapsible outline tree of the ontology's groups
  (sections → modules → subsections/families → member nodes).

  Tree behaviour:
  - Click a group row (or its chevron) to expand/collapse it. A group's row shows
    only the members that do not live in one of its subgroups, so nothing appears
    twice. `Expand all` / `Collapse all` toggle the whole tree.
  - Each group has a hover 'show on canvas' button that opens it as a module view.
  - Click a member (leaf) to select it and focus its neighbourhood on the canvas.

  Expand/collapse state lives in app-db (`[:ontology :outline :expanded]`), not a
  local ratom, so graph navigation can drive it: double-clicking a node on the
  canvas reveals it here by expanding the path to it, and the revealed leaf scrolls
  into view. Navigation only ever ADDS to the expanded set, so the canvas 'back'
  button never collapses the tree."
  (:require [re-frame.core :as rf]))

(defn- kind-glyph [kind]
  (case kind :class "●" :property "◆" :individual "■" :group "▤" "•"))

(defn- search-box []
  (let [q @(rf/subscribe [:outline/query])]
    [:div.search-box
     [:input.search-input
      {:type "search" :placeholder "Search id / label / gloss…  (/)"
       :value (or q "")
       :on-change #(rf/dispatch [:outline/set-query (.. % -target -value)])}]
     (when (not-empty q)
       [:button.search-clear {:on-click #(rf/dispatch [:outline/set-query ""])
                              :title "Clear"} "×"])]))

(defn- search-results []
  (let [results @(rf/subscribe [:outline/search-results])
        sel     (:node @(rf/subscribe [:ontology/selection]))]
    (when results
      [:div.search-results
       (if (empty? results)
         [:div.search-empty "No matches."]
         [:ul.result-list
          (for [{:keys [id label kind gloss external?]} results]
            ^{:key id}
            [:li.result-item {:class (when (= id sel) "result-selected")
                              :on-click #(rf/dispatch [:outline/focus-search-result id])
                              :title (or gloss id)}
             [:span.result-glyph {:class (when external? "ext")} (kind-glyph kind)]
             [:span.result-label label]
             [:span.result-id id]])])])))

(declare group-node)

(defn- member-node [{:keys [id label kind external?]} sel]
  (let [selected? (= id sel)]
    [:li.tree-member
     (cond-> {:class (when selected? "tree-selected")
              :title (str "Focus " id " on the canvas")
              :on-click #(rf/dispatch [:outline/focus-search-result id])}
       ;; When this leaf is the selected node (e.g. revealed by a graph
       ;; double-click), scroll it into view. block:"nearest" is a no-op when it
       ;; is already visible, so clicking it in the tree doesn't jump the scroll.
       selected? (assoc :ref (fn [el] (when el (.scrollIntoView el #js {:block "nearest"})))))
     [:span.tree-glyph {:class (when external? "ext")} (kind-glyph kind)]
     [:span.tree-label label]]))

(defn- group-node [expanded {:keys [id label count members subgroups] :as _g} sel]
  (let [open?       (contains? expanded id)
        expandable? (or (seq subgroups) (seq members))]
    [:li.tree-group
     [:div.tree-group-head {:class (str (when open? "open ") (when-not expandable? "empty"))
                            :on-click (when expandable? #(rf/dispatch [:outline/toggle-expand id]))}
      [:span.tree-twist (cond (not expandable?) "·" open? "▾" :else "▸")]
      [:span.tree-group-label label]
      [:span.tree-count count]
      [:button.tree-focus {:title "Show this group on the canvas"
                           :on-click (fn [e] (.stopPropagation e)
                                       (rf/dispatch [:view/expand-group id]))}
       "⊙"]]
     (when (and open? expandable?)
       [:ul.tree-children
        (for [sg subgroups] ^{:key (:id sg)} [group-node expanded sg sel])
        (for [m members] ^{:key (:id m)} [member-node m sel])])]))

(defn- all-group-ids [tree]
  (mapcat (fn [g] (cons (:id g) (all-group-ids (:subgroups g)))) tree))

(defn view []
  (let [tree     @(rf/subscribe [:outline/tree])
        expanded @(rf/subscribe [:outline/expanded])
        sel      (:node @(rf/subscribe [:ontology/selection]))
        q        @(rf/subscribe [:outline/query])]
    [:div.outline-pane
     [search-box]
     (if (not-empty q)
       [search-results]
       [:div.outline-tree
        (if tree
          [:<>
           [:div.tree-toolbar
            [:button.tree-toolbtn {:on-click #(rf/dispatch [:outline/set-expanded (all-group-ids tree)])}
             "Expand all"]
            [:button.tree-toolbtn {:on-click #(rf/dispatch [:outline/collapse-all])}
             "Collapse all"]]
           [:ul.tree-root
            (for [g tree] ^{:key (:id g)} [group-node expanded g sel])]]
          [:div.outline-empty "Open an ontology to see its outline."])])]))
