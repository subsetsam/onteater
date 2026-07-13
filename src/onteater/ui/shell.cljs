(ns onteater.ui.shell
  "The application shell: menu bar (File/Edit/View/Help dropdowns), workspace tabs,
  theme toggle, toast host, and modal dialog host. The shell is the stable frame
  around the two workspaces; it owns nothing domain-specific and routes to the
  active workspace view."
  (:require [re-frame.core :as rf]
            [onteater.ui.workspace-ontology :as ws-onto]
            [onteater.ui.workspace-scenario :as ws-scenario]
            [onteater.ui.dialogs :as dialogs]
            [onteater.ui.help :as help]))

(defn- theme-class [theme]
  (case theme :light "theme-light" :dark "theme-dark" ""))

;; --- brand mark -------------------------------------------------------------

(def ^:private logo-svg
  "Inline SVG for the Onteater mark shown at the left of the menu bar: a
   front-facing tamandua head with big rounded ears, two glossy eyes, and a long
   dark snout. Embedded as a string (rather than an external asset) so it survives
   inlining into the shipped single-file build. See onteater_logo_alt.svg."
  (str
   "<svg viewBox='0 0 48 48' xmlns='http://www.w3.org/2000/svg' aria-hidden='true' focusable='false'>"
   "<defs>"
   "<linearGradient id='laHead' x1='0' y1='0' x2='0.4' y2='1'>"
   "<stop offset='0%' stop-color='#F5E7CB'/><stop offset='100%' stop-color='#E2CDA4'/></linearGradient>"
   "<linearGradient id='laDark' x1='0' y1='0' x2='0.3' y2='1'>"
   "<stop offset='0%' stop-color='#3C332B'/><stop offset='100%' stop-color='#1B1712'/></linearGradient>"
   "<radialGradient id='laNose' cx='38%' cy='30%' r='72%'>"
   "<stop offset='0%' stop-color='#4A4038'/><stop offset='100%' stop-color='#141009'/></radialGradient>"
   "</defs>"
   "<g transform='rotate(-20 12 8)'>"
   "<ellipse cx='12' cy='8' rx='6' ry='7.2' fill='url(#laDark)'/>"
   "<ellipse cx='12.6' cy='8.8' rx='4.6' ry='5.9' fill='url(#laHead)'/>"
   "<ellipse cx='12.9' cy='9.4' rx='2.4' ry='3.4' fill='#CE9E8B'/></g>"
   "<g transform='rotate(20 36 8)'>"
   "<ellipse cx='36' cy='8' rx='6' ry='7.2' fill='url(#laDark)'/>"
   "<ellipse cx='35.4' cy='8.8' rx='4.6' ry='5.9' fill='url(#laHead)'/>"
   "<ellipse cx='35.1' cy='9.4' rx='2.4' ry='3.4' fill='#CE9E8B'/></g>"
   "<path d='M24 5 C 14 5 8.5 12 8.5 21 C 8.5 31 15 39.5 24 39.5 C 33 39.5 39.5 31 39.5 21 C 39.5 12 34 5 24 5 Z' fill='url(#laHead)'/>"
   "<path d='M24 19 C 21.4 19 20.5 21.6 20.9 24.6 C 21.3 30 22 36 24 41 C 26 36 26.7 30 27.1 24.6 C 27.5 21.6 26.6 19 24 19 Z' fill='url(#laDark)'/>"
   "<ellipse cx='24' cy='40' rx='3.7' ry='3.1' fill='url(#laNose)'/>"
   "<ellipse cx='22.8' cy='38.6' rx='1.2' ry='0.9' fill='#6b5f54'/>"
   "<ellipse cx='16.8' cy='23' rx='3.1' ry='3.4' fill='url(#laDark)'/>"
   "<ellipse cx='31.2' cy='23' rx='3.1' ry='3.4' fill='url(#laDark)'/>"
   "<circle cx='16.8' cy='23' r='2.1' fill='#15100C'/><circle cx='31.2' cy='23' r='2.1' fill='#15100C'/>"
   "<circle cx='16.1' cy='22.2' r='0.8' fill='#F6EEDF'/><circle cx='30.5' cy='22.2' r='0.8' fill='#F6EEDF'/>"
   "</svg>"))

(defn- brand-mark []
  [:span.brand-mark {:dangerouslySetInnerHTML {:__html logo-svg}}])

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
   [{:label "Keyboard shortcuts" :shortcut "?" :on-click #(rf/dispatch [:ui/help-toggle])}
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
