(ns onteater.ui.inspector
  "Right pane: the inspector for the selected node, now editable (Milestone 3).
  Every field commits through a re-frame edit event, so all edits are transactional
  and undoable via the shared history interceptor. Fields use uncontrolled inputs
  keyed by [id field value] so an external change (undo, reselection) remounts them
  with the current value while typing stays uninterrupted.

  Shows label, id (rename), kind, module, gloss, the open :props map (add/edit/
  remove), incident edges as navigable links, provenance, plus create/delete
  actions. External stub nodes are shown read-only (they are not defined here)."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; --- field widgets ----------------------------------------------------------

(defn- commit-text [commit-event current e]
  (let [v (.. e -target -value)]
    (when (not= v (or current "")) (rf/dispatch (conj commit-event v)))))

(defn- text-field [id field label value commit-event]
  [:div.insp-field
   [:label.insp-field-label label]
   ^{:key (str id "|" field "|" (hash value))}
   [:input.insp-input
    {:default-value (or value "")
     :on-blur #(commit-text commit-event value %)
     :on-key-down #(when (= "Enter" (.-key %)) (.blur (.-target %)))}]])

(defn- text-area [id field label value commit-event]
  [:div.insp-field
   [:label.insp-field-label label]
   ^{:key (str id "|" field "|" (hash value))}
   [:textarea.insp-input.insp-textarea
    {:rows 3 :default-value (or value "")
     :on-blur #(commit-text commit-event value %)}]])

(defn- kind-select [id kind]
  [:div.insp-field
   [:label.insp-field-label "Kind"]
   [:select.insp-input
    {:value (name (or kind :class))
     :on-change #(rf/dispatch [:ontology/update-node-field id :kind
                               (keyword (.. % -target -value))])}
    (for [k ["class" "property" "individual" "value"]]
      ^{:key k} [:option {:value k} k])]])

;; --- props editor -----------------------------------------------------------

(defn- props-editor [id props]
  [:div.insp-section
   [:h4.insp-heading "Properties"]
   [:div.props-list
    (for [[k v] props]
      ^{:key k}
      [:div.prop-row
       ^{:key (str id "|propk|" k)}
       [:input.prop-key {:default-value (str k)
                         :on-blur #(let [nk (.. % -target -value)]
                                     (when (and (not (str/blank? nk)) (not= nk (str k)))
                                       (rf/dispatch [:ontology/rename-prop id k nk])))}]
       ^{:key (str id "|propv|" k "|" (hash v))}
       [:input.prop-val {:default-value (str v)
                         :on-blur #(rf/dispatch [:ontology/set-prop id k (.. % -target -value)])}]
       [:button.prop-remove {:title "Remove property"
                             :on-click #(rf/dispatch [:ontology/remove-prop id k])} "×"]])]
   [:button.btn.btn-sm {:on-click #(rf/dispatch [:ontology/set-prop id "newKey" ""])}
    "+ Property"]])

;; --- edges ------------------------------------------------------------------

(defn- edges-section [id edges]
  [:div.insp-section
   [:div.insp-heading-row
    [:h4.insp-heading "Relations"]
    [:button.btn.btn-sm {:on-click #(rf/dispatch [:ontology/add-child id])
                         :title "Create a new subclass of this node"} "+ Subclass"]]
   (if (seq edges)
     [:ul.insp-edges
      (for [{:keys [edge dir other]} (sort-by (comp :label :other) edges)]
        ^{:key (:id edge)}
        [:li.insp-edge
         [:span.insp-edge-type (str (when (= dir :in) "↤ ") (name (:type edge))
                                    (when (= dir :out) " ↦"))]
         [:span.insp-edge-target {:on-click #(rf/dispatch [:ontology/select-node (:id other)])}
          (:label other)]
         [:button.insp-edge-del {:title "Remove relation"
                                 :on-click #(rf/dispatch [:ontology/remove-edge (:id edge)])} "×"]])]
     [:p.insp-none "No relations."])])

;; --- header/actions ---------------------------------------------------------

(defn- kind-badge [kind]
  [:span.kind-badge {:class (str "kind-" (name (or kind :class)))} (name (or kind :class))])

(defn- read-only-view [node edges]
  [:div.inspector-body
   [:div.insp-header
    [:h3.insp-title (:label node)]
    [kind-badge (:kind node)]
    [:span.ext-badge "external"]]
   [:div.insp-id (:id node)]
   [:p.insp-none "This is an external reference (defined outside this ontology) and
                  is shown read-only."]
   (when (seq edges)
     [edges-section (:id node) edges])])

(defn- editable-view [node edges]
  (let [id (:id node)]
    [:div.inspector-body
     [:div.insp-header
      [:h3.insp-title (:label node)]
      [kind-badge (:kind node)]]
     [:div.insp-actions
      [:button.btn.btn-sm {:on-click #(rf/dispatch [:view/focus-node id])} "Focus"]
      [:button.btn.btn-sm {:on-click #(rf/dispatch [:view/show-subtree id])} "Subtree"]
      [:button.btn.btn-sm.btn-danger {:on-click #(rf/dispatch [:ontology/request-delete-node id])} "Delete"]]
     [text-field id :label "Label" (:label node) [:ontology/update-node-field id :label]]
     [text-field id :id "ID (renames all references)" id [:ontology/rename-node id]]
     [kind-select id (:kind node)]
     [text-field id :module "Module" (:module node) [:ontology/update-node-field id :module]]
     [text-area id :gloss "Gloss" (:gloss node) [:ontology/update-node-field id :gloss]]
     [props-editor id (:props node)]
     [edges-section id edges]
     (when (seq (:provenance node))
       [:div.insp-section
        [:h4.insp-heading "Provenance"]
        [:ul.insp-prov
         (for [[i p] (map-indexed vector (:provenance node))]
           ^{:key i} [:li.insp-prov-path (str/join " / " (map str p))])]])]))

(defn view []
  (let [node  @(rf/subscribe [:ontology/selected-node])
        edges @(rf/subscribe [:ontology/selected-node-edges])]
    [:div.inspector-pane
     (cond
       (nil? node) [:div.inspector-empty
                    [:p "Select a node to inspect and edit it."]
                    [:p.hint "Click a node on the canvas or in the outline."]]
       (:external? node) [read-only-view node edges]
       :else [editable-view node edges])]))
