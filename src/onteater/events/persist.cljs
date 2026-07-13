(ns onteater.events.persist
  "Persistence events: Save / Save As / Export (ontology JSON, graph SVG/PNG) and
  the IndexedDB autosave + crash recovery loop. Serialisation itself
  is pure (the format adapters); these handlers just choose a format, produce the
  text, and hand it to the appropriate I/O effect. Every explicit save clears the
  dirty flag and the autosave snapshot."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [cljs.reader :as reader]
            [onteater.db :as db]
            [onteater.format.core :as fmt]
            [onteater.io.file]
            [onteater.io.export]
            [onteater.io.idb]))

(defn- resolved-dark? [pref]
  (case pref
    :dark true
    :light false
    (boolean (and (exists? js/window)
                  (.-matches (js/matchMedia "(prefers-color-scheme: dark)"))))))

(defn- base-name [db]
  (or (some-> (get-in db [:ontology :file :name]) (str/replace #"\.[^.]+$" ""))
      (some-> (get-in db [:ontology :model :meta :title]) (str/replace #"[^\w\- ]" "") (str/replace #"\s+" "-"))
      "ontology"))

(defn- file-descriptor
  "Filename extension + MIME type for a serialisation format-id, used by both the
  save-as picker and the download fallback."
  [fmt-id]
  (case fmt-id
    :onteater-native    {:ext ".onteater.json" :mime "application/json"}
    :geo-reference-json {:ext ".json"          :mime "application/json"}
    :owl2-turtle        {:ext ".ttl"           :mime "text/turtle"}
    {:ext ".json" :mime "application/json"}))

;; --- Save / Save As ---------------------------------------------------------

;; Save through a retained FS Access handle (same file, same format). If there is
;; no handle (fallback browsers, or never-saved model) it defers to Save As.
(rf/reg-event-fx
 :ontology/save
 (fn [{:keys [db]} _]
   (let [model  (get-in db [:ontology :model])
         file   (get-in db [:ontology :file])
         fmt-id (or (:format file) (get-in model [:meta :format]) :onteater-native)]
     (cond
       (nil? model) {}
       (:handle file)
       (let [text (fmt/serialize-with fmt-id model)]
         {:io/save-file {:handle (:handle file) :text text
                         :on-saved [:ontology/saved (:name file) (:handle file)]
                         :on-error [:ui/error]}})
       :else {:dispatch [:ontology/save-as fmt-id]}))))

(rf/reg-event-fx
 :ontology/save-as
 (fn [{:keys [db]} [_ chosen-format]]
   (let [model  (get-in db [:ontology :model])
         fmt-id (or chosen-format (get-in db [:ontology :file :format])
                    (get-in model [:meta :format]) :onteater-native)
         text   (fmt/serialize-with fmt-id model)
         {:keys [ext mime]} (file-descriptor fmt-id)
         fname  (str (base-name db) ext)]
     (when model
       {:io/save-as {:suggested-name fname :text text :mime mime
                     :on-saved [:ontology/saved-as fmt-id] :on-error [:ui/error]}}))))

(rf/reg-event-fx
 :ontology/saved
 (fn [{:keys [db]} [_ name handle]]
   {:db (-> db (assoc-in [:ontology :dirty?] false)
            (assoc-in [:ontology :file :name] name)
            (assoc-in [:ontology :file :handle] handle))
    :io/idb-delete {:key "autosave"}
    :dispatch [:ui/push-toast {:kind :info :text (str "Saved " name)}]}))

(rf/reg-event-fx
 :ontology/saved-as
 (fn [{:keys [db]} [_ fmt-id name handle]]
   {:db (-> db (assoc-in [:ontology :dirty?] false)
            (update-in [:ontology :file] merge {:name name :handle handle :format fmt-id}))
    :io/idb-delete {:key "autosave"}
    :dispatch [:ui/push-toast {:kind :info :text (str "Saved " name)}]}))

;; --- Export -----------------------------------------------------------------

(rf/reg-event-fx
 :ontology/export-json
 (fn [{:keys [db]} [_ fmt-id]]
   (let [model  (get-in db [:ontology :model])
         fmt-id (or fmt-id :onteater-native)
         {:keys [ext mime]} (file-descriptor fmt-id)]
     (when model
       {:io/download-text {:filename (str (base-name db) ext)
                           :mime mime
                           :text (fmt/serialize-with fmt-id model)}}))))

(defn- caption [db]
  (let [model (get-in db [:ontology :model])
        stamp (subs (.toISOString (js/Date.)) 0 10)]
    [(or (get-in model [:meta :title]) "Ontology")
     (str (get-in db [:ontology :view-spec :mode] :view) " view · " stamp)]))

(rf/reg-event-fx
 :ontology/export-svg
 (fn [{:keys [db]} _]
   {:io/export-svg {:filename (str (base-name db) ".svg")
                    :caption (caption db)
                    :dark? (resolved-dark? (get-in db [:ui :theme]))}}))

(rf/reg-event-fx
 :ontology/export-png
 (fn [{:keys [db]} _]
   {:io/export-png {:filename (str (base-name db) ".png") :scale 2
                    :caption (caption db)
                    :dark? (resolved-dark? (get-in db [:ui :theme]))
                    :on-error [:ui/error]}}))

;; --- Autosave + recovery ----------------------------------------------------

(rf/reg-event-fx
 :persist/autosave
 (fn [{:keys [db]} _]
   (let [model (get-in db [:ontology :model])
         dirty (get-in db [:ontology :dirty?])
         last  (get-in db [:ontology :autosave-ref])]
     (if (and model dirty (not (identical? model last)))
       {:io/idb-save {:key "autosave"
                      :value (pr-str {:model model
                                      :view-spec (get-in db [:ontology :view-spec])
                                      :file-name (get-in db [:ontology :file :name])
                                      :ts (.now js/Date)})}
        :db (assoc-in db [:ontology :autosave-ref] model)}
       {}))))

(rf/reg-event-fx
 :persist/check-recovery
 (fn [_ _]
   {:io/idb-load {:key "autosave" :on-loaded [:persist/recovery-loaded]}}))

(rf/reg-event-fx
 :persist/recovery-loaded
 (fn [{:keys [db]} [_ value]]
   (if (and value (nil? (get-in db [:ontology :model])))
     (let [snap (try (reader/read-string value) (catch :default _ nil))]
       (if (:model snap)
         {:dispatch [:ui/open-dialog
                     {:kind :confirm
                      :title "Recover unsaved work?"
                      :message (str "Found an autosaved snapshot of “"
                                    (or (:file-name snap) "an ontology")
                                    "”. Restore it, or discard and start fresh?")
                      :confirm-label "Restore" :cancel-label "Discard"
                      :on-confirm [:persist/restore snap]
                      :on-cancel [:persist/discard-autosave]}]}
         {}))
     {})))

(rf/reg-event-db
 :persist/restore
 (fn [db [_ snap]]
   (-> db
       (assoc-in [:ontology :model] (:model snap))
       (assoc-in [:ontology :view-spec] (or (:view-spec snap) db/default-view-spec))
       (assoc-in [:ontology :file] {:name (:file-name snap) :handle nil
                                    :format (get-in snap [:model :meta :format]) :hash nil})
       (assoc-in [:ontology :dirty?] true))))

(rf/reg-event-fx
 :persist/discard-autosave
 (fn [_ _] {:io/idb-delete {:key "autosave"}}))
