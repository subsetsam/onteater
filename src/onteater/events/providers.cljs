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
            [onteater.io.idb]         ; registers :io/idb-save, :io/idb-load
            [onteater.io.crypto]))    ; registers :io/encrypt-save, :io/unlock-all

(defn- provider-path
  "app-db path of a (non-Ollama) provider's settings map."
  [provider]
  (case provider :cloud [:llm :cloud] :azgov [:llm :azgov]))

;; Field edits that invalidate a previous connection test result. (:model and
;; :remember-key? don't — switching models needs no re-test.)
(def ^:private status-resetting
  {:cloud #{:vendor :api-key :base-url}
   :azgov #{:base-url :deployment :api-version :auth-scheme :api-key}})

;; Defined in the persistence section below; referenced by the field-edit events.
(declare persistable-settings commit)

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
         ;; It also selects a different saved-credential slot, so clear the live
         ;; key — one vendor's key must never linger under another.
         db (cond-> db
              (= :vendor k)
              (update-in [:llm :cloud] merge
                         {:base-url (get-in providers/cloud-presets [v :base-url] "")
                          :model "" :models [] :api-key ""})
              (contains? (:cloud status-resetting) k)
              (update-in [:llm :cloud] merge {:status :unknown :status-msg nil}))]
     (commit db :cloud k))))

(rf/reg-event-fx
 :llm/set-azgov-field
 (fn [{:keys [db]} [_ k v]]
   (let [db (cond-> (assoc-in db [:llm :azgov k] v)
              ;; A different auth method is a different saved slot: clear the
              ;; live key so a stored API key never shows in the token field.
              (= :auth-scheme k) (assoc-in [:llm :azgov :api-key] "")
              (contains? (:azgov status-resetting) k)
              (update-in [:llm :azgov] merge {:status :unknown :status-msg nil}))]
     (commit db :azgov k))))

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
  base-url/model/options, the cloud/azgov configs minus volatile/secret keys,
  and the `:saved` map of ENCRYPTED credential blobs. A live plaintext `:api-key`
  is stripped and never written — persistence of a key happens only through
  `:saved`, keyed by slot, in encrypted form. Public for unit tests."
  [db]
  (let [strip #(dissoc % :status :status-msg :models :api-key)]
    {:active (get-in db [:llm :active])
     :ollama (select-keys (:ollama db) [:base-url :model :options])
     :cloud  (strip (get-in db [:llm :cloud]))
     :azgov  (strip (get-in db [:llm :azgov]))
     :saved  (get-in db [:llm :saved])}))

(defn commit
  "Effect map produced after a provider field edit writes `db`. Always persists
  the non-secret settings; when the edited field affects the saved credential
  (:api-key or :remember-key?) it additionally manages the encrypted slot:

  - Remember off      → forget the slot's blob + any unlocked plaintext.
  - Remember on, key present, passphrase already unlocked → encrypt silently.
  - Remember toggled on without a passphrase → open the set/enter prompt.
  - Key edited while remember-on but locked → keep it session-only (no prompt)."
  [db provider changed]
  (let [remember? (boolean (get-in db [:llm provider :remember-key?]))
        slot      (providers/current-slot db)
        keyval    (get-in db [:llm provider :api-key])
        pp        (get-in db [:llm :crypto :passphrase])]
    (cond
      (not (contains? #{:api-key :remember-key?} changed))
      {:db db :dispatch [:llm/persist-settings]}

      (not remember?)
      {:db (-> db (update-in [:llm :saved] dissoc slot)
               (update-in [:llm :crypto :unlocked] dissoc slot))
       :dispatch [:llm/persist-settings]}

      (str/blank? keyval)
      {:db db :dispatch [:llm/persist-settings]}

      pp
      {:db db
       :io/encrypt-save {:passphrase pp :slot slot :plaintext keyval
                         :on-done [:llm/blob-ready slot]}}

      (= :remember-key? changed)
      {:db (assoc-in db [:llm :crypto :prompt]
                     {:mode (if (empty? (get-in db [:llm :saved])) :set :enter)
                      :target slot
                      :pending {:slot slot :plaintext keyval}
                      :error nil})
       :fx [[:dispatch [:ui/open-dialog {:kind :llm-crypto :on-cancel [:llm/prompt-cancel]}]]
            [:dispatch [:llm/persist-settings]]]}

      :else
      {:db db :dispatch [:llm/persist-settings]})))

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
           ;; New snapshots carry no plaintext :api-key; a LEGACY plaintext key
           ;; (from before encryption) merges straight into the live field for
           ;; back-compat and gets migrated into :saved on the next edit.
           (update-in [:llm :cloud] merge (when (map? (:cloud snap)) (:cloud snap)))
           (update-in [:llm :azgov] merge (when (map? (:azgov snap)) (:azgov snap)))
           (update :ollama merge (when (map? (:ollama snap))
                                   (select-keys (:ollama snap) [:base-url :model :options])))
           ;; Encrypted blobs only — live keys stay locked until the user
           ;; explicitly loads one (no boot prompt).
           (assoc-in [:llm :saved] (or (:saved snap) {})))))))

;; --- credential encryption / unlock ------------------------------------------

(rf/reg-event-fx
 :llm/blob-ready
 (fn [{:keys [db]} [_ slot blob]]
   ;; :io/encrypt-save finished — store the ciphertext and cache the plaintext
   ;; so a later "Load saved" for this slot fills without re-prompting.
   (let [plaintext (get-in db [:llm (first slot) :api-key])]
     {:db (cond-> (assoc-in db [:llm :saved slot] blob)
            (seq plaintext) (assoc-in [:llm :crypto :unlocked slot] plaintext))
      :dispatch [:llm/persist-settings]})))

(rf/reg-event-fx
 :llm/request-load-saved
 (fn [{:keys [db]} _]
   (let [slot     (providers/current-slot db)
         pp       (get-in db [:llm :crypto :passphrase])
         unlocked (get-in db [:llm :crypto :unlocked])]
     (cond
       (nil? slot) {}
       ;; already unlocked and we hold this slot's plaintext → fill immediately
       (and pp (contains? unlocked slot))
       {:db (assoc-in db [:llm (first slot) :api-key] (get unlocked slot))}
       ;; locked → prompt once; a valid passphrase unlocks every saved slot
       :else
       {:db (assoc-in db [:llm :crypto :prompt] {:mode :unlock :target slot :error nil})
        :dispatch [:ui/open-dialog {:kind :llm-crypto :on-cancel [:llm/prompt-cancel]}]}))))

(rf/reg-event-fx
 :llm/unlock-submit
 (fn [{:keys [db]} [_ passphrase]]
   (let [saved (get-in db [:llm :saved])]
     (if (empty? saved)
       {:dispatch [:llm/unlock-ok passphrase {}]}
       {:io/unlock-all {:passphrase passphrase :saved saved
                        :on-ok   [:llm/unlock-ok passphrase]
                        :on-fail [:llm/unlock-failed]}}))))

(rf/reg-event-fx
 :llm/unlock-ok
 (fn [{:keys [db]} [_ passphrase plaintexts]]
   (let [{:keys [target pending]} (get-in db [:llm :crypto :prompt])
         db (-> db
                (assoc-in [:llm :crypto :passphrase] passphrase)
                (update-in [:llm :crypto :unlocked] merge plaintexts)
                (assoc-in [:llm :crypto :prompt] nil))
         db (cond-> db
              (and target (contains? plaintexts target))
              (assoc-in [:llm (first target) :api-key] (get plaintexts target)))]
     (cond-> {:db db :dispatch [:ui/close-dialog]}
       ;; a save was waiting on the passphrase (Remember toggled on while blobs
       ;; already existed) → encrypt it now under the just-validated passphrase.
       pending
       (assoc :io/encrypt-save {:passphrase passphrase
                                :slot (:slot pending)
                                :plaintext (:plaintext pending)
                                :on-done [:llm/blob-ready (:slot pending)]})))))

(rf/reg-event-db
 :llm/unlock-failed
 (fn [db _]
   (assoc-in db [:llm :crypto :prompt :error] "Incorrect passphrase.")))

(rf/reg-event-fx
 :llm/set-passphrase-submit
 (fn [{:keys [db]} [_ passphrase]]
   (let [pending (get-in db [:llm :crypto :prompt :pending])
         db (-> db
                (assoc-in [:llm :crypto :passphrase] passphrase)
                (assoc-in [:llm :crypto :prompt] nil))]
     (cond-> {:db db :dispatch [:ui/close-dialog]}
       pending
       (assoc :io/encrypt-save {:passphrase passphrase
                                :slot (:slot pending)
                                :plaintext (:plaintext pending)
                                :on-done [:llm/blob-ready (:slot pending)]})))))

(rf/reg-event-fx
 :llm/prompt-cancel
 (fn [{:keys [db]} _]
   ;; Runs via :ui/dialog-cancel (Cancel button / backdrop / Esc), which already
   ;; pops the dialog — so this only cleans up state; it must NOT pop again.
   (let [{:keys [mode target]} (get-in db [:llm :crypto :prompt])
         db (assoc-in db [:llm :crypto :prompt] nil)
         db (case mode
              ;; load flow: leave the key field blank, as specified
              :unlock (cond-> db target (assoc-in [:llm (first target) :api-key] ""))
              ;; save flow: undo the opt-in; the typed key stays session-only
              (cond-> db target (assoc-in [:llm (first target) :remember-key?] false)))]
     {:db db :dispatch [:llm/persist-settings]})))
