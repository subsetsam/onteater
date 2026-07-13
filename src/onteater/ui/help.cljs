(ns onteater.ui.help
  "The Help / About modal: keyboard shortcuts, a short feature tour,
  the Ollama CORS note (the one thing users hit that the app cannot fix for them),
  and version/about info. Toggled by `?` or Help ▸ Keyboard shortcuts, dismissed by
  Escape or clicking the backdrop."
  (:require [re-frame.core :as rf]))

(def ^:private shortcuts
  [["⌘/Ctrl + O" "Open an ontology"]
   ["⌘/Ctrl + S" "Save"]
   ["⌘/Ctrl + Z" "Undo (ontology edit or mapping change, per active tab)"]
   ["⌘/Ctrl + ⇧ + Z" "Redo"]
   ["/" "Jump to search"]
   ["Delete / Backspace" "Delete the selected node"]
   ["Double-click node" "Focus its neighbourhood"]
   ["Right-click node" "Context menu (focus, subtree, add subclass, delete)"]
   ["Drag node" "Reposition / pin (Alt-drag to unpin)"]
   ["Escape" "Close menus, dialogs, and this window"]
   ["?" "Toggle this help"]])

(defn modal []
  (when @(rf/subscribe [:ui/help-open?])
    [:div.dialog-backdrop
     {:on-click #(when (= (.-target %) (.-currentTarget %)) (rf/dispatch [:ui/help-toggle]))}
     [:div.dialog.help-dialog
      [:div.settings-head
       [:h3.dialog-title "Onteater — Help"]
       [:button.icon-btn {:on-click #(rf/dispatch [:ui/help-toggle])} "×"]]

      [:div.help-cols
       [:div.help-col
        [:h4 "Keyboard shortcuts"]
        [:table.help-shortcuts
         [:tbody
          (for [[k d] shortcuts]
            ^{:key k} [:tr [:td.help-key k] [:td d]])]]]
     
       [:div.help-col
        [:h4 "Two workflows"]
        [:ul.help-list
         [:li [:b "Ontology"] " — open a JSON ontology, navigate from the module
                    overview into focused neighbourhoods, edit any attribute, and save
                    back (edits land in place; the rest of the file is left untouched)."]
         [:li [:b "Scenario"] " — paste a scenario, pick an Ollama model, and map its
                    elements onto the ontology. Review entries, accept/reject/force them,
                    and discuss the mapping in chat."]]
     
        [:h4 "Connecting to Ollama"]
        [:p.help-note "Served from a " [:code "file://"] " page the browser origin is
              " [:code "null"] ", which Ollama rejects by default. Start the server so it
              accepts this origin, e.g.:"]
        [:pre.help-pre "OLLAMA_ORIGINS=\"*\" ollama serve"]
        [:p.help-note "Then set the base URL in Settings (⚙) and press Test connection."]]]

      [:div.dialog-actions
       [:button.btn.btn-primary {:on-click #(rf/dispatch [:ui/help-toggle])} "Close"]]]]))
