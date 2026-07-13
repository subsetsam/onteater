(ns onteater.ui.workspace-ontology
  "Use case A workspace: explore and edit an ontology.

  Three-pane layout: outline/search on the left, the d3 graph canvas
  in the centre, the inspector on the right, with a status bar beneath. Panes are
  collapsible to fight clutter. When no ontology is loaded an empty state offers
  Open."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.ui.brand :as brand]
            [onteater.ui.canvas :as canvas]
            [onteater.ui.outline :as outline]
            [onteater.ui.inspector :as inspector]))

(defn- empty-state []
  [:div.empty-state
   [:div.empty-logo {:dangerouslySetInnerHTML {:__html brand/logo-svg}}]
   [:h2 "No ontology loaded"]
   [:p "Open an ontology file to explore it as an interactive graph. Onteater
        auto-detects the format and never modifies your file until you save."]
   [:div.empty-actions
    [:button.btn.btn-primary {:on-click #(rf/dispatch [:ontology/open])} "Open…"]]
   [:p.hint "Try the bundled examples/galactic-economic-ontology.json."]])

(defn- status-bar []
  (let [file  @(rf/subscribe [:ontology/file])
        dirty @(rf/subscribe [:ontology/dirty?])
        stats @(rf/subscribe [:ontology/stats])
        warns @(rf/subscribe [:ontology/validation])]
    [:footer.status-bar
     [:span.status-file (or (:name file) "untitled")
      (when dirty [:span.dirty-dot {:title "Unsaved changes"} " ●"])]
     [:span.status-sep "·"]
     [:span.status-counts (str (:nodes stats) " nodes · " (:edges stats) " edges")]
     (when (pos? (or (:external stats) 0))
       [:span.status-counts (str " · " (:external stats) " external")])
     [:div.status-spacer]
     (when (seq warns)
       [:span.status-warn {:title (str/join "\n" (map :message warns))}
        (str "⚠ " (count warns) " validation notice"
             (when (> (count warns) 1) "s"))])]))

(defn- workbench []
  (let [collapsed (r/atom {:outline false :inspector false})]
    (fn []
      [:div.workbench
       [:div.workbench-panes
        (when-not (:outline @collapsed)
          [:aside.pane.pane-outline [outline/view]])
        [:button.pane-toggle.toggle-outline
         {:on-click #(swap! collapsed update :outline not)
          :title "Toggle outline"}
         (if (:outline @collapsed) "›" "‹")]
        [:section.pane.pane-canvas [canvas/view]]
        [:button.pane-toggle.toggle-inspector
         {:on-click #(swap! collapsed update :inspector not)
          :title "Toggle inspector"}
         (if (:inspector @collapsed) "‹" "›")]
        (when-not (:inspector @collapsed)
          [:aside.pane.pane-inspector [inspector/view]])]
       [status-bar]])))

(defn view []
  (let [model @(rf/subscribe [:ontology/model])]
    [:div.ws-ontology
     (if model
       [workbench]
       [empty-state])]))
