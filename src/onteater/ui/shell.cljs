(ns onteater.ui.shell
  "The application shell: menu bar (File/Edit/View/Help dropdowns), workspace tabs,
  theme toggle, toast host, and modal dialog host. The shell is the stable frame
  around the two workspaces; it owns nothing domain-specific and routes to the
  active workspace view."
  (:require [re-frame.core :as rf]
            [onteater.ui.brand :as brand]
            [onteater.ui.workspace-ontology :as ws-onto]
            [onteater.ui.workspace-scenario :as ws-scenario]
            [onteater.ui.dialogs :as dialogs]
            [onteater.ui.help :as help]))

(defn- theme-class [theme]
  (case theme :light "theme-light" :dark "theme-dark" ""))

;; --- brand mark -------------------------------------------------------------

(defn- brand-mark []
  [:span.brand-mark {:dangerouslySetInnerHTML {:__html brand/logo-svg}}])

;; --- dropdown menus ---------------------------------------------------------

(defn- menu-item [{:keys [label shortcut on-click disabled? divider?]}]
  (if divider?
    [:div.menu-divider]
    [:button.menu-item {:disabled disabled?
                        :on-click (fn [] (rf/dispatch [:ui/close-menu]) (on-click))}
     [:span.menu-item-label label]
     (when shortcut [:span.menu-item-shortcut shortcut])]))

(defn- menu [id label items]
  (let [open @(rf/subscribe [:ui/menu])]
    [:div.menu {:class (when (= id open) "menu-open")}
     [:button.menu-trigger {:class (when (= id open) "menu-trigger-active")
                            :on-click #(rf/dispatch [:ui/toggle-menu id])}
      label]
     (when (= id open)
       [:div.menu-dropdown
        (for [[i it] (map-indexed vector items)]
          ^{:key i} [menu-item it])])]))

(defn- file-menu []
  (let [model @(rf/subscribe [:ontology/model])
        geo?  (= :geo-reference-json (get-in @(rf/subscribe [:ontology/file]) [:format]))]
    [menu :file "File"
     [{:label "Open…" :shortcut "⌘O" :on-click #(rf/dispatch [:ontology/open])}
      {:label "Save" :shortcut "⌘S" :disabled? (not model) :on-click #(rf/dispatch [:ontology/save])}
      {:label "Save As…" :disabled? (not model)
       :on-click #(rf/dispatch [:ui/open-dialog {:kind :format-picker}])}
      {:divider? true}
      {:label "Export graph as SVG" :disabled? (not model) :on-click #(rf/dispatch [:ontology/export-svg])}
      {:label "Export graph as PNG" :disabled? (not model) :on-click #(rf/dispatch [:ontology/export-png])}
      {:label "Export ontology (native JSON)" :disabled? (not model)
       :on-click #(rf/dispatch [:ontology/export-json :onteater-native])}
      {:label "Export ontology (OWL2 Turtle)" :disabled? (not model)
       :on-click #(rf/dispatch [:ontology/export-json :owl2-turtle])}
      (when geo?
        {:label "Export ontology (geo JSON)" :disabled? (not model)
         :on-click #(rf/dispatch [:ontology/export-json :geo-reference-json])})]]))

(defn- edit-menu []
  (let [undo? @(rf/subscribe [:ontology/can-undo?])
        redo? @(rf/subscribe [:ontology/can-redo?])]
    [menu :edit "Edit"
     [{:label "Undo" :shortcut "⌘Z" :disabled? (not undo?) :on-click #(rf/dispatch [:ontology/undo])}
      {:label "Redo" :shortcut "⌘⇧Z" :disabled? (not redo?) :on-click #(rf/dispatch [:ontology/redo])}]]))

(defn- view-menu []
  (let [model @(rf/subscribe [:ontology/model])]
    [menu :view "View"
     [{:label "Reset to module overview" :disabled? (not model)
       :on-click #(rf/dispatch [:view/reset-overview])}]]))

(defn- help-menu []
  [menu :help "Help"
   [{:label "Usage" :shortcut "?" :on-click #(rf/dispatch [:ui/help-toggle])}
    {:divider? true}
    {:label "About Onteater" :on-click #(rf/dispatch [:ui/open-dialog {:kind :about}])}]])

(defn- theme-toggle []
  (let [theme @(rf/subscribe [:ui/theme])]
    [:button.mb-btn.icon-btn
     {:title (str "Theme: " (name theme) " — click to cycle")
      :on-click #(rf/dispatch [:ui/set-theme (case theme :auto :light :light :dark :dark :auto)])}
     (case theme :light "☀" :dark "☾" "◐")]))

(defn- menu-bar []
  (let [active @(rf/subscribe [:app/workspace])
        menu-open @(rf/subscribe [:ui/menu])]
    [:header.menubar
     (when menu-open
       [:div.menu-overlay {:on-click #(rf/dispatch [:ui/close-menu])}])
     [:div.brand [brand-mark] [:span.brand-name "Onteater"]]
     [:nav.menus [file-menu] [edit-menu] [view-menu] [help-menu]]
     [:div.menubar-divider]
     [:nav.workspace-tabs
      (for [[id label] [[:ontology "Ontology"] [:scenario "Scenario"]]]
        ^{:key id}
        [:button.tab {:class    (when (= id active) "tab-active")
                      :on-click #(rf/dispatch [:app/set-workspace id])}
         label])]
     [:div.menubar-spacer]
     [:div.menubar-actions
      [:button.mb-btn.icon-btn {:title "LLM settings"
                                :on-click #(rf/dispatch [:ui/open-dialog {:kind :settings}])} "⚙"]
      [theme-toggle]]]))

(defn- toast-host []
  (let [toasts @(rf/subscribe [:ui/toasts])]
    (when (seq toasts)
      [:div.toast-host
       (for [{:keys [id kind text]} toasts]
         ^{:key id}
         [:div.toast {:class (str "toast-" (name (or kind :info)))
                      :on-click #(rf/dispatch [:ui/dismiss-toast id])}
          text])])))

(defn root []
  (let [theme     @(rf/subscribe [:ui/theme])
        workspace @(rf/subscribe [:app/workspace])]
    [:div.app-root {:class (theme-class theme)}
     [menu-bar]
     [:main.workspace
      (case workspace
        :ontology [ws-onto/view]
        :scenario [ws-scenario/view]
        [ws-onto/view])]
     [toast-host]
     [dialogs/view]
     [help/modal]]))
