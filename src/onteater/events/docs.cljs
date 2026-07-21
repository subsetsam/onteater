(ns onteater.events.docs
  "Events for the documentation sections (`:docs` on the model): the generic
  structured editor's mutations, plus the Docs-view UI state. Model mutations
  carry the `record-history` interceptor (undo/redo + dirty + autosave for
  free) and delegate the tree surgery to the pure `onteater.model.docs` ops."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.model.docs :as docs]
            [onteater.events.history :refer [record-history]]))

(def hist [record-history])

(defn- update-section
  "Apply `f` to the docs tree of the section at `path` (no-op if absent)."
  [model path f]
  (update model :docs
          (fn [ss] (mapv #(if (= path (:path %)) (update % :value f) %) ss))))

(defn- sync-meta
  "Re-derive [:meta :title]/[:meta :version] from the `metadata` docs section so
  the status bar and export filenames track edits."
  [model]
  (if-let [tree (some #(when (= ["metadata"] (:path %)) (:value %)) (:docs model))]
    (let [title (:v (docs/get-at tree ["title"]))]
      (-> model
          (assoc-in [:meta :title] (if (and (string? title) (not (str/blank? title)))
                                     title
                                     "Untitled ontology"))
          (assoc-in [:meta :version] (:v (docs/get-at tree ["version"])))))
    model))

(defn- edit-section [db section-path f]
  (update-in db [:ontology :model]
             (fn [m] (-> m (update-section section-path f) sync-meta))))

;; --- generic value edits ----------------------------------------------------

(rf/reg-event-db
 :docs/set-value
 hist
 (fn [db [_ section-path value-path v]]
   (edit-section db section-path #(docs/set-at % value-path v))))

(rf/reg-event-db
 :docs/add-item
 hist
 (fn [db [_ section-path value-path]]
   (edit-section db section-path #(docs/vec-add % value-path))))

(rf/reg-event-db
 :docs/remove-at
 hist
 (fn [db [_ section-path value-path]]
   (edit-section db section-path #(docs/remove-at % value-path))))

;; Append a fresh entry to the map at `value-path`, auto-naming it new_key,
;; new_key2, … so the user can rename it in place.
(rf/reg-event-db
 :docs/add-key
 hist
 (fn [db [_ section-path value-path]]
   (edit-section db section-path
                 (fn [tree]
                   (let [taken (into #{} (map first)
                                     (:entries (docs/get-at tree value-path)))
                         k     (loop [n 1]
                                 (let [k (str "new_key" (when (> n 1) n))]
                                   (if (taken k) (recur (inc n)) k)))]
                     (docs/map-add-key tree value-path k))))))

(rf/reg-event-db
 :docs/rename-key
 hist
 (fn [db [_ section-path value-path old new]]
   (edit-section db section-path #(docs/rename-key % value-path old new))))

(rf/reg-event-db
 :docs/set-kind
 hist
 (fn [db [_ section-path value-path kind]]
   (edit-section db section-path #(docs/set-kind % value-path kind))))

;; --- whole sections ---------------------------------------------------------

(defn- forbidden-section-keys
  "Top-level keys a new section may not claim: existing sections, structural
  keys, and any key that node provenance paths pass through (writing a docs
  subtree there would clobber graph content)."
  [model]
  (into #{"namespaces"}
        (concat (map (comp first :path) (:docs model))
                (mapcat (fn [n] (keep first (:provenance n)))
                        (vals (:nodes model))))))

(rf/reg-event-db
 :docs/add-section
 hist
 (fn [db [_ k kind]]
   (let [model (get-in db [:ontology :model])
         k     (some-> k str/trim)]
     (if (or (str/blank? k) ((forbidden-section-keys model) k))
       db
       (update-in db [:ontology :model] update :docs (fnil conj [])
                  {:path [k]
                   :value (case kind
                            :vec {:t :vec :items []}
                            :val {:t :val :v ""}
                            {:t :map :entries []})})))))

(rf/reg-event-fx
 :docs/request-remove-section
 "Confirm before dropping a whole section (its key is removed from the file on
  save; undo restores it)."
 (fn [_ [_ path]]
   {:dispatch [:ui/open-dialog
               {:kind :confirm
                :title "Remove section"
                :message (str "Remove the “" (str/join " › " path)
                              "” section? It will be deleted from the file on save."
                              " This can be undone.")
                :confirm-label "Remove"
                :danger? true
                :on-confirm [:docs/remove-section path]}]}))

(rf/reg-event-db
 :docs/remove-section
 hist
 (fn [db [_ path]]
   (update-in db [:ontology :model]
              (fn [m]
                (-> m
                    (update :docs (fn [ss] (vec (remove #(= path (:path %)) ss))))
                    sync-meta)))))

;; --- Docs-view UI state (not history-tracked) -------------------------------

(rf/reg-event-db
 :ontology/set-center-view
 (fn [db [_ v]] (assoc-in db [:ontology :center-view] v)))

(rf/reg-event-db
 :docs/toggle-expand
 (fn [db [_ k]]
   (update-in db [:ontology :docs-ui :expanded]
              (fn [s] (let [s (or s #{})] (if (s k) (disj s k) (conj s k)))))))

(rf/reg-event-db
 :docs/set-expanded
 (fn [db [_ ks]] (assoc-in db [:ontology :docs-ui :expanded] (set ks))))
