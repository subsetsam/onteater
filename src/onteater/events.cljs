(ns onteater.events
  "re-frame event handlers.

  Convention: events are namespaced keywords grouped by feature
  (`:app/*`, `:ontology/*`, `:scenario/*`, `:ollama/*`, `:ui/*`). Handlers stay
  small and delegate all non-trivial logic to the pure domain layer
  (`onteater.model.*`, `onteater.format.*`) so behaviour is unit-testable without
  a running app. Side effects go through registered effects, never inline.

  Milestone 0 defines only bootstrap + basic shell events; later milestones add
  ontology-edit, file-I/O, and LLM events (many via their own require'd nses)."
  (:require [re-frame.core :as rf]
            [onteater.db :as db]
            [onteater.events.ontology]     ; ontology + view + file events
            [onteater.events.history]      ; undo/redo interceptor + events
            [onteater.events.editing]      ; model mutation events
            [onteater.events.persist]      ; save/export/autosave events
            [onteater.events.ollama]       ; ollama settings + connection events
            [onteater.events.providers]    ; cloud/Azure-Gov provider settings events
            [onteater.events.mapping]      ; scenario mapping events
            [onteater.events.timeline]     ; temporal-mapping (timeline) events
            [onteater.events.chat]))       ; chat drawer events (must load after mapping)

;; --- bootstrap ---------------------------------------------------------------

(rf/reg-event-db
 :app/initialize
 "Install the default database. Dispatched once (`dispatch-sync`) at boot before
  the first render so views never see an empty db. IndexedDB recovery, when
  implemented, will hydrate over this baseline afterwards."
 (fn [_ _]
   db/default-db))

;; --- shell / UI --------------------------------------------------------------

(rf/reg-event-db
 :app/set-workspace
 "Switch the active top-level workspace tab (:ontology | :scenario)."
 (fn [db [_ workspace]]
   (assoc db :workspace workspace)))

(rf/reg-event-db
 :ui/set-theme
 "Set the theme preference (:auto | :light | :dark)."
 (fn [db [_ theme]]
   (assoc-in db [:ui :theme] theme)))

(rf/reg-event-db
 :ui/toggle-menu
 "Open the named menu-bar menu, or close it if it is already open."
 (fn [db [_ menu-id]]
   (update-in db [:ui :menu] #(when-not (= % menu-id) menu-id))))

(rf/reg-event-db
 :ui/close-menu
 (fn [db _] (assoc-in db [:ui :menu] nil)))

(rf/reg-event-fx
 :ui/push-toast
 "Add a transient toast notification. `toast` is {:kind :info|:warn|:error :text s}.
  The toast auto-dismisses after 10s, and can also be dismissed by clicking it."
 (fn [{:keys [db]} [_ toast]]
   (let [id (gensym "toast")]
     {:db (update-in db [:ui :toasts] (fnil conj []) (assoc toast :id id))
      :dispatch-later [{:ms 10000 :dispatch [:ui/dismiss-toast id]}]})))

(rf/reg-event-db
 :ui/dismiss-toast
 (fn [db [_ id]]
   (update-in db [:ui :toasts] (fn [ts] (vec (remove #(= (:id %) id) ts))))))

;; --- dialogs (a stack; the top one renders) ---------------------------------

(rf/reg-event-db
 :ui/open-dialog
 (fn [db [_ desc]] (update-in db [:ui :dialogs] (fnil conj []) desc)))

(rf/reg-event-db
 :ui/close-dialog
 (fn [db _] (update-in db [:ui :dialogs] (fn [ds] (if (seq ds) (pop ds) ds)))))

(rf/reg-event-fx
 :ui/dialog-confirm
 (fn [{:keys [db]} _]
   (let [d (peek (get-in db [:ui :dialogs]))]
     (cond-> {:db (update-in db [:ui :dialogs] pop)}
       (:on-confirm d) (assoc :dispatch (:on-confirm d))))))

(rf/reg-event-fx
 :ui/dialog-cancel
 (fn [{:keys [db]} _]
   (let [d (peek (get-in db [:ui :dialogs]))]
     (cond-> {:db (update-in db [:ui :dialogs] pop)}
       (:on-cancel d) (assoc :dispatch (:on-cancel d))))))

;; --- keyboard shortcuts route through here ----------------------------------

(rf/reg-event-fx
 :ui/help-toggle
 (fn [{:keys [db]} _] {:db (update-in db [:ui :help-open?] not)}))

;; Undo/redo route to the active workspace's history (ontology edits vs mapping
;; curation), so ⌘Z does the contextually-right thing.
(rf/reg-event-fx
 :app/undo
 (fn [{:keys [db]} _]
   {:dispatch (if (= :scenario (:workspace db)) [:mapping/undo] [:ontology/undo])}))

(rf/reg-event-fx
 :app/redo
 (fn [{:keys [db]} _]
   {:dispatch (if (= :scenario (:workspace db)) [:mapping/redo] [:ontology/redo])}))

;; --- canvas context menu ----------------------------------------------------

(rf/reg-event-db
 :ui/context-menu
 (fn [db [_ menu]] (assoc-in db [:ui :context-menu] menu)))

(rf/reg-event-db
 :ui/close-context-menu
 (fn [db _] (assoc-in db [:ui :context-menu] nil)))

(rf/reg-event-fx
 :ui/delete-selection
 (fn [{:keys [db]} _]
   (when-let [id (get-in db [:ontology :selection :node])]
     {:dispatch [:ontology/request-delete-node id]})))

;; Escape closes whatever transient UI is topmost (dialog > context menu > menu).
(rf/reg-event-fx
 :ui/escape
 (fn [{:keys [db]} _]
   (cond
     (seq (get-in db [:ui :dialogs]))    {:dispatch [:ui/dialog-cancel]}
     (get-in db [:ui :context-menu])     {:db (assoc-in db [:ui :context-menu] nil)}
     (get-in db [:ui :menu])             {:db (assoc-in db [:ui :menu] nil)}
     (get-in db [:ui :help-open?])       {:db (assoc-in db [:ui :help-open?] false)}
     :else {})))
