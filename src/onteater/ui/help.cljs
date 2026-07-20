(ns onteater.ui.help
  "The Help / Usage modal — a tabbed window: keyboard shortcuts, a guide to the
  Ontology workflow, a guide to the Scenario workflow (including everywhere an
  LLM is invoked), and LLM provider setup (with the Ollama CORS note — the one
  thing users hit that the app cannot fix for them). Toggled by `?` or
  Help ▸ Usage, dismissed by Escape or clicking the backdrop."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]))

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

(defn- shortcuts-tab []
  [:div.help-body
   [:h4 "Keyboard shortcuts"]
   [:table.help-shortcuts
    [:tbody
     (for [[k d] shortcuts]
       ^{:key k} [:tr [:td.help-key k] [:td d]])]]])

(defn- ontology-tab []
  [:div.help-body
   [:div.help-cols
    [:div.help-col
     [:h4 "Explore"]
     [:ul.help-list
      [:li "Open a JSON ontology, an Onteater native file, or OWL 2 Turtle
            (" [:code ".ttl"] ") with " [:b "File ▸ Open"] " — the format is
            auto-detected."]
      [:li "Start from the module overview and double-click into focused
            neighbourhoods; pick from four layouts (clustered, force, tidy tree,
            radial)."]
      [:li "Search (" [:code "/"] ") and filter to find nodes; click one to read
            every attribute in the detail pane."]]]
    [:div.help-col
     [:h4 "Edit & save"]
     [:ul.help-list
      [:li "Edit any attribute in place; add or delete nodes and relations
            (right-click a node for the context menu); undo/redo throughout."]
      [:li [:b "Save"] " writes edits into the source file " [:i "in place"] " —
            the rest of the file is left byte-for-byte untouched."]
      [:li "Export the current view as SVG/PNG, or the whole ontology as OWL 2
            (" [:b "File ▸ Export"] ")."]
      [:li "No LLM is needed anywhere in this workflow."]]]]])

(defn- scenario-tab []
  [:div.help-body
   [:div.help-cols
    [:div.help-col
     [:h4 "Workflow"]
     [:ul.help-list
      [:li "Paste or upload a scenario (Markdown + LaTeX math), connect a model
            (see " [:i "LLM setup"] "), and press " [:b "Run mapping"] "."]
      [:li [:b "Mapping"] " — the entry board: scenario excerpts typed onto
            ontology classes, every entry validated against the ontology and
            accept/reject/force-able, with excerpts highlighted back in the
            scenario text."]
      [:li [:b "Timeline"] " — dated events linked by precedes / causes /
            enables / responds-to / part-of / terminates relations, drawn as a
            swimlane DAG. Select an event for its dependency cone; drag
            glyph-to-glyph to add a relation."]
      [:li [:b "Gaps"] " — a completeness audit of where the ontology fails the
            scenario, with per-gap \"Draft ontology element…\" jumps into the
            editor. Pure analysis — no LLM."]]]
    [:div.help-col
     [:h4 "Where the LLM is used"]
     [:ul.help-list
      [:li [:b "Mapping runs"] " — the scenario is chunked and each chunk sent
            as one request; the strategy chip (auto / full / scoped / staged)
            controls how the ontology is presented. A staged run adds
            per-branch refinement batches."]
      [:li [:b "Briefing"] " — an optional one-time pass over the loaded
            ontology producing module summaries and disambiguation rules; you
            review and edit the text, and it is injected into every mapping
            prompt."]
      [:li [:b "Timeline extraction"] " — a second pass lifts the mapped
            entries into the temporal/causal event graph."]
      [:li [:b "Chat drawer"] " — answers questions about the mapping and can
            propose changes, applied only after you review them as diffs."]
      [:li "Every run shows progress and is cancellable; results are always
            validated against the ontology before they land."]]]]])

(defn- llm-tab []
  [:div.help-body
   [:div.help-cols
    [:div.help-col
     [:h4 "Providers"]
     [:p.help-note "Open " [:b "Settings (⚙)"] " — one tab per provider family:
           " [:b "Ollama"] ", " [:b "Cloud"] " (Anthropic / OpenAI /
           OpenAI-compatible), and " [:b "Azure Gov"] ". The selected tab "
      [:i "is"] " the active provider: whatever tab is showing is what mapping,
           chat, and every other LLM call use. One temperature slider is shared
           by all providers (Anthropic models ignore it)."]
     [:h4 "API keys"]
     [:p.help-note "Keys live in memory for the session only. Tick
           " [:b "\"Remember key on this device\""] " to keep one across reloads
           — it is then " [:b "encrypted with a passphrase you set"] " (AES-GCM,
           key derived via PBKDF2) and only the ciphertext is written to this
           browser's IndexedDB; after a reload, enter the passphrase to unlock
           saved keys. This protects keys at rest — session-only remains the
           strongest option. Non-secret settings (provider, base URLs, models)
           always persist."]]
    [:div.help-col
     [:h4 "Ollama (local)"]
     [:p.help-note "Served from a " [:code "file://"] " page the browser origin
           is " [:code "null"] ", which Ollama rejects by default. Start the
           server so it accepts this origin, e.g.:"]
     [:pre.help-pre "OLLAMA_ORIGINS=\"*\" ollama serve"]
     [:p.help-note "Then set the base URL, press " [:b "Test connection"] ", and
           pick a model. Structured-output support varies by model; capable
           instruct models produce the best mappings."]
     [:h4 "Cloud & Azure Gov"]
     [:p.help-note "Anthropic and OpenAI need only an API key
           (" [:b "Test & load models"] " fills the model picker); Custom takes
           any OpenAI-compatible base URL + model id, but the endpoint must
           allow this page's origin via CORS. Azure Gov targets an Azure OpenAI
           deployment: endpoint, deployment name, API version, and an API key or
           Entra ID bearer token."]]]])

(def ^:private tabs
  [[:shortcuts "Shortcuts"]
   [:ontology  "Ontology"]
   [:scenario  "Scenario"]
   [:llm       "LLM setup"]])

(defn modal []
  (let [tab (r/atom :shortcuts)]
    (fn []
      (when @(rf/subscribe [:ui/help-open?])
        [:div.dialog-backdrop
         {:on-click #(when (= (.-target %) (.-currentTarget %)) (rf/dispatch [:ui/help-toggle]))}
         [:div.dialog.help-dialog
          [:div.settings-head
           [:h3.dialog-title "Onteater — Help"]
           [:button.icon-btn {:on-click #(rf/dispatch [:ui/help-toggle])} "×"]]

          [:div.settings-tabs {:role "tablist"}
           (for [[id label] tabs]
             ^{:key id}
             [:button.settings-tab {:class (when (= id @tab) "settings-tab-active")
                                    :on-click #(reset! tab id)}
              label])]

          (case @tab
            :ontology [ontology-tab]
            :scenario [scenario-tab]
            :llm      [llm-tab]
            [shortcuts-tab])

          [:div.dialog-actions
           [:button.btn.btn-primary {:on-click #(rf/dispatch [:ui/help-toggle])} "Close"]]]]))))
