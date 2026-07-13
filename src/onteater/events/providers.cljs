(ns onteater.events.providers
  "Events for the multi-provider LLM connection settings (Settings dialog tabs):
  provider selection, cloud/Azure-Gov field edits, connection testing, and
  IndexedDB persistence of the non-secret settings (API keys persist only with
  the per-provider 'Remember on this device' opt-in).

  The Ollama tab keeps its original events (onteater.events.ollama); this
  namespace covers the :cloud and :azgov providers plus the cross-provider
  persist/restore. All provider request/response knowledge lives in
  onteater.llm.providers — handlers here only move data between app-db, the
  :llm/request effect, and IndexedDB."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [cljs.reader :as reader]
            [onteater.llm.providers :as providers]
            [onteater.llm.client]     ; registers :llm/request, :llm/abort
            [onteater.io.idb]))       ; registers :io/idb-save, :io/idb-load

(defn- provider-path
  "app-db path of a (non-Ollama) provider's settings map."
  [provider]
  (case provider :cloud [:llm :cloud] :azgov [:llm :azgov]))

;; Field edits that invalidate a previous connection test result. (:model and
;; :remember-key? don't — switching models needs no re-test.)
(def ^:private status-resetting
  {:cloud #{:vendor :api-key :base-url}
   :azgov #{:base-url :deployment :api-version :auth-scheme :api-key}})

;; --- provider selection -------------------------------------------------------

(rf/reg-event-fx
 :llm/select-provider
 (fn [{:keys [db]} [_ provider]]
   {:db (assoc-in db [:llm :active] provider)
    :dispatch [:llm/persist-settings]}))

;; --- field edits ----------------------------------------------------------------

(rf/reg-event-fx
 :llm/set-cloud-field
 (fn [{:keys [db]} [_ k v]]
   (let [db (assoc-in db [:llm :cloud k] v)
         ;; A vendor switch changes endpoint + model namespace wholesale:
         ;; re-derive the base URL from the preset and drop the stale model list.
         db (cond-> db
              (= :vendor k)
              (update-in [:llm :cloud] merge
                         {:base-url (get-in providers/cloud-presets [v :base-url] "")
                          :model "" :models []})
              (contains? (:cloud status-resetting) k)
              (update-in [:llm :cloud] merge {:status :unknown :status-msg nil}))]
     {:db db :dispatch [:llm/persist-settings]})))

(rf/reg-event-fx
 :llm/set-azgov-field
 (fn [{:keys [db]} [_ k v]]
   (let [db (cond-> (assoc-in db [:llm :azgov k] v)
              (contains? (:azgov status-resetting) k)
              (update-in [:llm :azgov] merge {:status :unknown :status-msg nil}))]
     {:db db :dispatch [:llm/persist-settings]})))

;; --- connection test -------------------------------------------------------------

(defn- test-blockers
  "Fields that must be filled before a connection test can even be attempted
  (an empty base-url would fetch a relative URL against the page origin)."
  [provider cfg]
  (case provider
    :cloud (cond
             (str/blank? (:api-key cfg))  "Enter an API key first."
             (str/blank? (:base-url cfg)) "Enter the base URL first.")
    :azgov (cond
             (str/blank? (:base-url cfg))   "Enter the Azure endpoint first."
             (str/blank? (:deployment cfg)) "Enter the deployment name first."
             (str/blank? (:api-key cfg))    "Enter the API key or bearer token first.")
    nil))

(rf/reg-event-fx
 :llm/test-connection
 (fn [{:keys [db]} _]
   (let [provider (get-in db [:llm :active])]
     (if (= :ollama provider)
       ;; The Ollama tab's button dispatches :ollama/test-connection directly;
       ;; this route exists so a generic caller can test whatever is active.
       {:dispatch [:ollama/test-connection]}
       (let [cfg (providers/active-config db)
             pk  (provider-path provider)]
         (if-let [why (test-blockers provider cfg)]
           {:db (-> db (assoc-in (conj pk :status) :error)
                    (assoc-in (conj pk :status-msg) why))}
           {:db (-> db (assoc-in (conj pk :status) :checking)
                    (assoc-in (conj pk :status-msg) "Contacting server…"))
            :llm/request (merge (providers/test-request cfg)
                                {:key :llm-test
                                 :base-url (:base-url cfg)
                                 ;; provider rides along so a slow reply can't
                                 ;; land on whichever tab is active by then
                                 :on-done  [:llm/test-done provider]
                                 :on-error [:llm/test-error provider]})}))))))

(rf/reg-event-fx
 :llm/test-done
 (fn [{:keys [db]} [_ provider resp]]
   (let [pk     (provider-path provider)
         cfg    (assoc (get-in db pk) :provider provider)
         models (providers/test-response->models cfg resp)
         db     (-> db
                    (assoc-in (conj pk :status) :ok)
                    (assoc-in (conj pk :status-msg)
                              (if models
                                (str (count models) " model"
                                     (when (not= 1 (count models)) "s") " available")
                                "Connected — deployment responded")))
         db     (if models
                  (-> db
                      (assoc-in (conj pk :models) models)
                      ;; keep the user's model if the server still offers it,
                      ;; else preselect the vendor default / first listed
                      (update-in (conj pk :model)
                                 (fn [m] (if (and (seq m) (some #{m} models))
                                           m
                                           (or (providers/default-model cfg models) "")))))
                  db)]
     {:db db :dispatch [:llm/persist-settings]})))

(rf/reg-event-db
 :llm/test-error
 (fn [db [_ provider err]]
   (let [pk  (provider-path provider)
         ;; The client's :unreachable message carries Ollama-specific guidance
         ;; (OLLAMA_ORIGINS); substitute provider-appropriate wording here.
         msg (if (= :unreachable (:kind err))
               "Could not reach the server — network error, or the endpoint blocked this page's origin (CORS)."
               (:message err))
         msg (if (= :azgov provider)
               (str msg " " providers/azgov-cors-hint)
               msg)]
     (-> db
         (assoc-in (conj pk :status) (if (= :unreachable (:kind err)) :unreachable :error))
         (assoc-in (conj pk :status-msg) msg)))))

;; --- persistence (IndexedDB, key "llm-settings") ---------------------------------

(defn persistable-settings
  "The subset of provider settings worth persisting: provider choice, Ollama
  base-url/model/options, and the cloud/azgov configs minus volatile keys.
  Each provider's :api-key is included ONLY when its :remember-key? is true —
  keys are session-only by default (and stored unencrypted when opted in; the
  Settings checkbox states this). Public for unit tests."
  [db]
  (let [strip (fn [cfg]
                (let [cfg (dissoc cfg :status :status-msg :models)]
                  (if (:remember-key? cfg) cfg (dissoc cfg :api-key))))]
    {:active (get-in db [:llm :active])
     :ollama (select-keys (:ollama db) [:base-url :model :options])
     :cloud  (strip (get-in db [:llm :cloud]))
     :azgov  (strip (get-in db [:llm :azgov]))}))

(rf/reg-event-fx
 :llm/persist-settings
 (fn [{:keys [db]} _]
   {:io/idb-save {:key "llm-settings" :value (pr-str (persistable-settings db))}}))

(rf/reg-event-fx
 :llm/load-settings
 (fn [_ _]
   {:io/idb-load {:key "llm-settings" :on-loaded [:llm/settings-restored]}}))

(rf/reg-event-db
 :llm/settings-restored
 (fn [db [_ value]]
   (let [snap (when (string? value)
                (try (reader/read-string value) (catch :default _ nil)))]
     (if-not (map? snap)
       db                                        ; nothing saved / corrupt — ignore
       (-> db
           (update-in [:llm :active] #(or (:active snap) %))
           (update-in [:llm :cloud] merge (when (map? (:cloud snap)) (:cloud snap)))
           (update-in [:llm :azgov] merge (when (map? (:azgov snap)) (:azgov snap)))
           (update :ollama merge (when (map? (:ollama snap))
                                   (select-keys (:ollama snap) [:base-url :model :options]))))))))
