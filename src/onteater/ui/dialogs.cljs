(ns onteater.ui.dialogs
  "Modal dialog host. A dialog is a plain descriptor pushed onto [:ui :dialogs];
  the top one renders here. Confirm dialogs carry :on-confirm / :on-cancel event
  vectors that fire on the respective action (both also pop the stack). All dialogs
  are Escape-dismissable and click-outside-dismissable."
  (:require [re-frame.core :as rf]
            [onteater.ui.mascot :as mascot]
            [onteater.ui.settings :as settings]))

(defn- confirm-dialog [{:keys [title message confirm-label cancel-label danger?]}]
  [:div.dialog
   [:h3.dialog-title title]
   [:p.dialog-message message]
   [:div.dialog-actions
    [:button.btn {:on-click #(rf/dispatch [:ui/dialog-cancel])}
     (or cancel-label "Cancel")]
    [:button.btn {:class (if danger? "btn-danger-solid" "btn-primary")
                  :on-click #(rf/dispatch [:ui/dialog-confirm])}
     (or confirm-label "OK")]]])

(defn- format-picker-dialog [{:keys [title message]}]
  [:div.dialog
   [:h3.dialog-title (or title "Save As…")]
   [:p.dialog-message (or message "Choose a format:")]
   [:div.dialog-actions.dialog-actions-col
    [:button.btn {:on-click #(do (rf/dispatch [:ui/close-dialog])
                                 (rf/dispatch [:ontology/save-as :geo-reference-json]))}
     "Geo reference JSON (source format, in-place edits)"]
    [:button.btn {:on-click #(do (rf/dispatch [:ui/close-dialog])
                                 (rf/dispatch [:ontology/save-as :onteater-native]))}
     "Onteater native JSON (lossless)"]
    [:button.btn {:on-click #(do (rf/dispatch [:ui/close-dialog])
                                 (rf/dispatch [:ontology/save-as :owl2-turtle]))}
     "OWL 2 Turtle (.ttl, modelled subset + preserved axioms)"]
    [:button.btn {:on-click #(rf/dispatch [:ui/close-dialog])} "Cancel"]]])

(defn- about-dialog [_]
  [:div.dialog.about-dialog
   [:div.settings-head
    [:h3.dialog-title "Onteater — About"]
    [:button.icon-btn {:on-click #(rf/dispatch [:ui/close-dialog])} "×"]]

   [:div.about-mascot {:dangerouslySetInnerHTML {:__html mascot/full-svg}}]

   [:p.help-lede "An offline-first, single-file studio for exploring, editing, and
         LLM-mapping ontologies. Everything but the LLM calls you configure (local
         Ollama, a cloud provider, or Azure Gov) works with no network."]

   [:div.help-about
    [:span "Onteater · v0.1"]
    [:span "HTML + ClojureScript · d3 · Reagent/re-frame · KaTeX"]
    ;; Link out to the public source repository. target=_blank with
    ;; rel=noopener keeps the opener window from being reachable via
    ;; window.opener when the tab is launched.
    [:span [:a {:href "https://github.com/subsetsam/onteater"
                :target "_blank"
                :rel "noopener noreferrer"}
            "github.com/subsetsam/onteater"]]]

   [:div.dialog-actions
    [:button.btn.btn-primary {:on-click #(rf/dispatch [:ui/close-dialog])} "Close"]]])

(defn view []
  (let [dialog @(rf/subscribe [:ui/top-dialog])]
    (when dialog
      [:div.dialog-backdrop
       {:on-click #(when (= (.-target %) (.-currentTarget %))
                     (rf/dispatch [:ui/dialog-cancel]))}
       (case (:kind dialog)
         :confirm       [confirm-dialog dialog]
         :format-picker [format-picker-dialog dialog]
         :settings      [settings/panel]
         :about         [about-dialog dialog]
         [confirm-dialog dialog])])))
