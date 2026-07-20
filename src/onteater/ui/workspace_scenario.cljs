(ns onteater.ui.workspace-scenario
  "Use case B workspace: map a scenario onto the ontology with LLM assistance.
  Layout: scenario pane · a tabbed center pane (Mapping · Timeline · Gaps) · chat
  drawer. Requires a loaded ontology; otherwise it points the user to the Ontology
  tab. The center tabs share one scenario/mapping session — the timeline (§6.7) is a
  first-class part of that session, not a separate document."
  (:require [re-frame.core :as rf]
            [onteater.ui.scenario :as scenario]
            [onteater.ui.mapping-board :as board]
            [onteater.ui.timeline :as timeline]
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

(defn- center-tabs []
  (let [tab @(rf/subscribe [:timeline/tab])
        gaps @(rf/subscribe [:timeline/gap-count])]
    [:div.center-pane
     [:div.center-tabbar
      (for [[t label] [[:mapping "Mapping"] [:timeline "Timeline"] [:gaps "Gaps"]]]
        ^{:key t}
        [:button.center-tab {:class (when (= t (or tab :mapping)) "center-tab-active")
                             :on-click #(rf/dispatch [:scenario/set-center-tab t])}
         label
         (when (and (= t :gaps) (pos? gaps)) [:span.center-tab-badge gaps])])]
     [:div.center-tab-body
      (case (or tab :mapping)
        :timeline [timeline/timeline-tab]
        :gaps     [timeline/gaps-tab]
        [board/view])]]))

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
         [:section.pane.pane-board [center-tabs]]
         (when chat-open?
           [:aside.pane.pane-chat [chat/drawer]])]])]))
