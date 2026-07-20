(ns onteater.ui.dialogs
  "Modal dialog host. A dialog is a plain descriptor pushed onto [:ui :dialogs];
  the top one renders here. Confirm dialogs carry :on-confirm / :on-cancel event
  vectors that fire on the respective action (both also pop the stack). All dialogs
  are Escape-dismissable and click-outside-dismissable."
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [onteater.ui.mascot :as mascot]
            [onteater.ui.settings :as settings]
            [onteater.version :as ver]))

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

(defn- add-node-dialog
  "The 'Draft ontology element from this gap…' dialog (§6.7.4). Pre-filled from the
  gap (label, suggested parent from `nearest`, gloss drafted from `why-no-fit`) and
  editable before the user commits. Confirming creates the class in the ontology
  workspace (undoable) and jumps there focused on it."
  [{:keys [title label parent gloss]}]
  (let [state (r/atom {:label label :parent (or parent "") :gloss gloss})]
    (fn [_]
      [:div.dialog.add-node-dialog
       [:h3.dialog-title (or title "Draft ontology element")]
       [:label.dialog-field "Label"
        [:input.insp-input {:value (:label @state)
                            :on-change #(swap! state assoc :label (.. % -target -value))}]]
       [:label.dialog-field "Parent class id (from ‘nearest’; optional)"
        [:input.insp-input {:value (:parent @state)
                            :placeholder "e.g. geo:Act"
                            :on-change #(swap! state assoc :parent (.. % -target -value))}]]
       [:label.dialog-field "Gloss"
        [:textarea.insp-input {:value (:gloss @state) :rows 3
                               :on-change #(swap! state assoc :gloss (.. % -target -value))}]]
       [:div.dialog-actions
        [:button.btn {:on-click #(rf/dispatch [:ui/close-dialog])} "Cancel"]
        [:button.btn.btn-primary
         {:disabled (str/blank? (:label @state))
          :on-click #(do (rf/dispatch [:ui/close-dialog])
                         (rf/dispatch [:timeline/create-drafted-node
                                       {:label (:label @state)
                                        :parent (let [p (:parent @state)] (when (seq p) p))
                                        :gloss (:gloss @state) :kind :class}]))}
         "Create class"]]])))

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
    [:span (str "Onteater · v" ver/string)]
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
         :llm-crypto    [settings/crypto-prompt]
         :about         [about-dialog dialog]
         :add-node      [add-node-dialog dialog]
         [confirm-dialog dialog])])))
