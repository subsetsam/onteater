(ns onteater.ui.docs
  "Center pane, Docs view: the ontology's documentation sections (worked
  examples, design decisions, revision notes, governance, metadata, …) as a
  full-width accordion, each section edited in place by one generic structured
  editor — strings become inputs/textareas, arrays become add/remove item
  lists, objects become key/value rows, recursively. The same component handles
  every section, known or unknown, so new files need no bespoke UI.

  Editing conventions match the inspector: uncontrolled inputs keyed by
  [path value-hash] committing on blur (Enter blurs single-line inputs), so
  undo/redo remounts fields with fresh values while typing stays uninterrupted.
  All commits dispatch `:docs/*` events, which carry the shared history
  interceptor — every docs edit is undoable and marks the model dirty."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [clojure.string :as str]))

(defn- humanize
  "Display label for a JSON key: underscores to spaces, sentence case. Display
  only — the raw key is what lives in the file."
  [k]
  (str/capitalize (str/replace (str k) "_" " ")))

(defn- enter-blurs [e]
  (when (= "Enter" (.-key e)) (.blur (.-target e))))

;; --- scalar editors ---------------------------------------------------------

(defn- commit-string [section-path value-path current e]
  (let [v (.. e -target -value)]
    (when (not= v (or current ""))
      (rf/dispatch [:docs/set-value section-path value-path v]))))

(defn- string-editor [section-path value-path v]
  (let [s (or v "")]
    (if (or (> (count s) 80) (str/includes? s "\n"))
      ^{:key (str value-path "|" (hash v))}
      [:textarea.insp-input.insp-textarea.doc-text
       {:rows (-> (+ (count (str/split-lines s)) (quot (count s) 90))
                  (max 2) (min 12))
        :default-value s
        :on-blur #(commit-string section-path value-path v %)}]
      ^{:key (str value-path "|" (hash v))}
      [:input.insp-input.doc-text
       {:default-value s
        :placeholder (when (nil? v) "null")
        :on-blur #(commit-string section-path value-path v %)
        :on-key-down enter-blurs}])))

(defn- number-editor [section-path value-path v]
  ^{:key (str value-path "|" (hash v))}
  [:input.insp-input.doc-num
   {:type "number" :default-value v
    :on-blur (fn [e]
               (let [n (js/parseFloat (.. e -target -value))]
                 (when (and (not (js/isNaN n)) (not= n v))
                   (rf/dispatch [:docs/set-value section-path value-path n]))))
    :on-key-down enter-blurs}])

(defn- boolean-editor [section-path value-path v]
  [:input.doc-check
   {:type "checkbox" :checked (boolean v)
    :on-change #(rf/dispatch [:docs/set-value section-path value-path
                              (.. % -target -checked)])}])

(defn- empty-val?
  "An empty scalar may be re-typed into a list or object via the type picker."
  [{:keys [t v]}]
  (and (= :val t) (or (nil? v) (= "" v))))

(defn- type-picker [section-path value-path]
  [:span.doc-type-picker
   [:button.doc-type-btn {:title "Turn into a list"
                          :on-click #(rf/dispatch [:docs/set-kind section-path value-path :vec])}
    "[ ]"]
   [:button.doc-type-btn {:title "Turn into an object"
                          :on-click #(rf/dispatch [:docs/set-kind section-path value-path :map])}
    "{ }"]])

;; --- the recursive generic editor -------------------------------------------

(declare value-editor)

(defn- scalar-row [section-path value-path {:keys [v] :as node}]
  [:div.doc-scalar
   (cond
     (boolean? v) [boolean-editor section-path value-path v]
     (number? v)  [number-editor section-path value-path v]
     :else        [string-editor section-path value-path v])
   (when (empty-val? node)
     [type-picker section-path value-path])])

(defn- vec-editor [section-path value-path items]
  [:div.doc-vec
   (for [[i item] (map-indexed vector items)]
     ^{:key i}
     (if (= :val (:t item))
       [:div.doc-vec-row
        [value-editor section-path (conj value-path i) item]
        [:button.prop-remove {:title "Remove item"
                              :on-click #(rf/dispatch [:docs/remove-at section-path (conj value-path i)])}
         "×"]]
       [:div.doc-vec-item
        [:div.doc-vec-item-head
         [:span.doc-idx (str "#" (inc i))]
         [:button.prop-remove {:title "Remove item"
                               :on-click #(rf/dispatch [:docs/remove-at section-path (conj value-path i)])}
          "×"]]
        [value-editor section-path (conj value-path i) item]]))
   [:button.btn.btn-sm {:on-click #(rf/dispatch [:docs/add-item section-path value-path])}
    "+ Add item"]])

(defn- map-editor [section-path value-path entries]
  [:div.doc-map
   (for [[k v] entries]
     ^{:key k}
     [:div.doc-map-row
      ^{:key (str value-path "|key|" k)}
      [:input.prop-key.doc-key
       {:default-value k
        :title "Field name (rename in place)"
        :on-blur #(let [nk (str/trim (.. % -target -value))]
                    (when (and (not (str/blank? nk)) (not= nk k))
                      (rf/dispatch [:docs/rename-key section-path value-path k nk])))
        :on-key-down enter-blurs}]
      [:div.doc-map-val [value-editor section-path (conj value-path k) v]]
      [:button.prop-remove {:title "Remove field"
                            :on-click #(rf/dispatch [:docs/remove-at section-path (conj value-path k)])}
       "×"]])
   [:button.btn.btn-sm {:on-click #(rf/dispatch [:docs/add-key section-path value-path])}
    "+ Add field"]])

(defn- value-editor
  "Render any docs subtree as its matching editor. `value-path` addresses the
  subtree within its section's tree."
  [section-path value-path node]
  (case (:t node)
    :vec [vec-editor section-path value-path (:items node)]
    :map [map-editor section-path value-path (:entries node)]
    [scalar-row section-path value-path node]))

;; --- sections / groups ------------------------------------------------------

(defn- section-block [expanded editable? {:keys [path value]}]
  (let [open? (contains? expanded path)]
    [:div.docs-section
     [:div.docs-section-head {:on-click #(rf/dispatch [:docs/toggle-expand path])}
      [:span.tree-twist (if open? "▾" "▸")]
      [:span.docs-section-label (humanize (last path))]
      [:span.docs-section-key (str/join "." path)]
      (when editable?
        [:button.prop-remove {:title "Remove this section from the file"
                              :on-click (fn [e]
                                          (.stopPropagation e)
                                          (rf/dispatch [:docs/request-remove-section path]))}
         "×"])]
     (when open?
       [:div.docs-section-body
        [value-editor path [] value]])]))

(defn- group-block
  "One top-level file key. A plain prose key renders as a single section; a
  mixed node/prose key renders a heading over its prose children with a hint
  that the node content lives in the Graph view."
  [expanded editable? {:keys [key mixed? sections]}]
  (if (and (not mixed?) (= 1 (count sections)))
    [section-block expanded editable? (first sections)]
    [:div.docs-group
     [:div.docs-group-head
      [:span.docs-group-label (humanize key)]
      (when mixed?
        [:span.docs-group-hint "classes and properties in this section are edited in the Graph view"])]
     (for [s sections]
       ^{:key (str (:path s))} [section-block expanded editable? s])]))

(defn- add-section-row []
  (let [state (r/atom {:key "" :kind :map})]
    (fn []
      (let [{:keys [key kind]} @state
            add! (fn []
                   (when-not (str/blank? key)
                     (rf/dispatch [:docs/add-section key kind])
                     (rf/dispatch [:docs/toggle-expand [(str/trim key)]])
                     (swap! state assoc :key "")))]
        [:div.docs-add-row
         [:input.insp-input.docs-add-key
          {:placeholder "new_section_key"
           :value key
           :on-change #(swap! state assoc :key (.. % -target -value))
           :on-key-down #(when (= "Enter" (.-key %)) (add!))}]
         [:select.insp-input.docs-add-kind
          {:value (name kind)
           :on-change #(swap! state assoc :kind (keyword (.. % -target -value)))}
          [:option {:value "map"} "object"]
          [:option {:value "vec"} "list"]
          [:option {:value "val"} "text"]]
         [:button.btn.btn-sm {:disabled (str/blank? key) :on-click add!}
          "+ Add section"]]))))

(defn view []
  (let [groups    @(rf/subscribe [:docs/groups])
        expanded  @(rf/subscribe [:docs/expanded])
        editable? @(rf/subscribe [:docs/editable?])]
    [:div.docs-pane
     [:div.docs-toolbar
      [:button.tree-toolbtn
       {:on-click #(rf/dispatch [:docs/set-expanded
                                 (mapcat (fn [g] (map :path (:sections g))) groups)])}
       "Expand all"]
      [:button.tree-toolbtn {:on-click #(rf/dispatch [:docs/set-expanded []])}
       "Collapse all"]]
     [:div.docs-scroll
      (if (seq groups)
        (for [g groups] ^{:key (:key g)} [group-block expanded editable? g])
        [:div.docs-empty
         [:p "This file has no documentation sections."]
         (when editable?
           [:p.hint "Add one below — it becomes a new top-level key in the file on save."])])
      (when editable? [add-section-row])]]))
