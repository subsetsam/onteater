(ns onteater.ui.workspace-scenario
  "Use case B workspace: map a scenario onto the ontology with LLM assistance.
  Layout: scenario pane · mapping board (summary + entries) · chat
  drawer (Milestone 6). Requires a loaded ontology; otherwise it points the user to
  the Ontology tab."
  (:require [re-frame.core :as rf]
            [onteater.ui.scenario :as scenario]
            [onteater.ui.mapping-board :as board]
            [onteater.ui.chat :as chat]))

(defn- no-ontology []
  [:div.empty-state
   [:div.empty-mark "🗺"]
   [:h2 "Scenario mapping"]
   [:p "Load an ontology first (Ontology tab), then paste or upload a scenario here
        and let a local Ollama model propose a mapping onto it."]
   [:button.btn.btn-primary {:on-click #(rf/dispatch [:ontology/open])} "Open ontology…"]])

(defn- session-bar []
  [:div.session-bar
   [:span.session-title "Mapping session"]
   [:div.toolbar-spacer]
   [:button.chip {:on-click #(rf/dispatch [:mapping/new-session])} "New"]
   [:button.chip {:on-click #(rf/dispatch [:mapping/load-session])} "Load…"]
   [:button.chip {:on-click #(rf/dispatch [:mapping/save-session])} "Save…"]
   [:button.chip {:on-click #(rf/dispatch [:chat/toggle])} "💬 Chat"]])

(defn view []
  (let [model @(rf/subscribe [:ontology/model])
        chat-open? @(rf/subscribe [:chat/open?])]
    [:div.ws-scenario
     (if-not model
       [no-ontology]
       [:div.scenario-workbench
        [session-bar]
        [:div.scenario-panes
         [:section.pane.pane-scenario [scenario/pane]]
         [:section.pane.pane-board [board/view]]
         (when chat-open?
           [:aside.pane.pane-chat [chat/drawer]])]])]))
