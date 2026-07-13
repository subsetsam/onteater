(ns onteater.events.providers-test
  "Tests for the multi-provider credential persistence: the per-selection slot
  model, the guarantee that a plaintext key is NEVER written to disk, the
  `commit` effect map for field edits, live-key clearing on selection change,
  and legacy-plaintext restore. Plus a Web-Crypto encrypt→decrypt round-trip
  when `crypto.subtle` exists in this runtime (else it is covered by E2E).

  The pure functions are exercised directly; the field-clearing and restore
  behaviours are driven through real events (re-frame runs headless in
  :node-test — IndexedDB effects are best-effort no-ops there)."
  (:require [cljs.test :refer [deftest is testing async]]
            [cljs.reader :as reader]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [onteater.db :as db]
            [onteater.llm.providers :as providers]
            [onteater.events.providers :as ep]
            [onteater.io.crypto :as crypto]
            [onteater.events]))            ; registers all events

(defn- base-db []
  {:llm db/default-llm
   :ollama {:base-url "http://localhost:11434" :model "m" :options {:temperature 0.2}}})

;; --- slot model --------------------------------------------------------------

(deftest current-slot-by-selection
  (let [d (assoc-in (base-db) [:llm :active] :cloud)]
    (is (= [:cloud :anthropic] (providers/current-slot d)))
    (is (= [:cloud :openai]
           (providers/current-slot (assoc-in d [:llm :cloud :vendor] :openai)))))
  (let [d (assoc-in (base-db) [:llm :active] :azgov)]
    (is (= [:azgov :api-key] (providers/current-slot d)))
    (is (= [:azgov :bearer]
           (providers/current-slot (assoc-in d [:llm :azgov :auth-scheme] :bearer)))))
  (testing "Ollama has no key slot"
    (is (nil? (providers/current-slot (assoc-in (base-db) [:llm :active] :ollama))))))

;; --- what reaches disk -------------------------------------------------------

(deftest persistable-never-leaks-plaintext-key
  (let [blob {:v 1 :salt "s" :iv "i" :ct "c"}
        d    (-> (base-db)
                 (assoc-in [:llm :cloud :api-key] "sk-secret")
                 (assoc-in [:llm :cloud :remember-key?] true)
                 (assoc-in [:llm :azgov :api-key] "tok-secret")
                 (assoc-in [:llm :saved] {[:cloud :anthropic] blob}))
        snap (ep/persistable-settings d)
        edn  (pr-str snap)]
    (testing "no plaintext key in the snapshot"
      (is (nil? (:api-key (:cloud snap))))
      (is (nil? (:api-key (:azgov snap))))
      (is (not (re-find #"sk-secret" edn)))
      (is (not (re-find #"tok-secret" edn))))
    (testing "encrypted blobs are what's persisted, and round-trip through EDN"
      (is (= {[:cloud :anthropic] blob} (:saved snap)))
      (is (= (:saved snap) (:saved (reader/read-string edn)))))))

;; --- commit effect map -------------------------------------------------------

(deftest commit-remember-off-forgets-slot
  (let [d  (-> (base-db) (assoc-in [:llm :active] :cloud)
               (assoc-in [:llm :cloud :remember-key?] false)
               (assoc-in [:llm :saved] {[:cloud :anthropic] {:v 1}})
               (assoc-in [:llm :crypto :unlocked] {[:cloud :anthropic] "sk"}))
        fx (ep/commit d :cloud :remember-key?)]
    (is (= {} (get-in fx [:db :llm :saved])))
    (is (= {} (get-in fx [:db :llm :crypto :unlocked])))))

(deftest commit-encrypts-when-unlocked
  (let [d  (-> (base-db) (assoc-in [:llm :active] :cloud)
               (assoc-in [:llm :cloud :remember-key?] true)
               (assoc-in [:llm :cloud :api-key] "sk-live")
               (assoc-in [:llm :crypto :passphrase] "pw"))
        fx (ep/commit d :cloud :api-key)]
    (is (= "sk-live" (get-in fx [:io/encrypt-save :plaintext])))
    (is (= [:cloud :anthropic] (get-in fx [:io/encrypt-save :slot])))
    (is (= "pw" (get-in fx [:io/encrypt-save :passphrase])))))

(deftest commit-prompts-to-set-passphrase-when-locked
  (let [d  (-> (base-db) (assoc-in [:llm :active] :cloud)
               (assoc-in [:llm :cloud :remember-key?] true)
               (assoc-in [:llm :cloud :api-key] "sk-live"))
        fx (ep/commit d :cloud :remember-key?)]
    (testing "no blobs yet → :set mode targeting this slot"
      (is (= :set (get-in fx [:db :llm :crypto :prompt :mode])))
      (is (= [:cloud :anthropic] (get-in fx [:db :llm :crypto :prompt :target])))
      (is (= "sk-live" (get-in fx [:db :llm :crypto :prompt :pending :plaintext])))))
  (testing "blobs already exist → :enter (re-enter existing passphrase)"
    (let [d  (-> (base-db) (assoc-in [:llm :active] :cloud)
                 (assoc-in [:llm :cloud :remember-key?] true)
                 (assoc-in [:llm :cloud :api-key] "sk-live")
                 (assoc-in [:llm :saved] {[:cloud :openai] {:v 1}}))
          fx (ep/commit d :cloud :remember-key?)]
      (is (= :enter (get-in fx [:db :llm :crypto :prompt :mode]))))))

(deftest commit-key-edit-while-locked-stays-session-only
  (let [d  (-> (base-db) (assoc-in [:llm :active] :cloud)
               (assoc-in [:llm :cloud :remember-key?] true)
               (assoc-in [:llm :cloud :api-key] "sk-live"))
        fx (ep/commit d :cloud :api-key)]
    (is (nil? (:io/encrypt-save fx)) "no encryption without a passphrase")
    (is (nil? (get-in fx [:db :llm :crypto :prompt])) "no prompt on a mere key edit")
    (is (= [:llm/persist-settings] (:dispatch fx)))))

;; --- event-driven behaviours -------------------------------------------------

(deftest vendor-switch-clears-live-key
  (rf/dispatch-sync [:app/initialize])
  (rf/dispatch-sync [:llm/select-provider :cloud])
  (rf/dispatch-sync [:llm/set-cloud-field :api-key "sk-anthropic"])
  (is (= "sk-anthropic" (get-in @rfdb/app-db [:llm :cloud :api-key])))
  (rf/dispatch-sync [:llm/set-cloud-field :vendor :openai])
  (is (= "" (get-in @rfdb/app-db [:llm :cloud :api-key]))
      "switching vendor clears the live key so it can't bleed across vendors"))

(deftest auth-scheme-switch-clears-live-key
  (rf/dispatch-sync [:app/initialize])
  (rf/dispatch-sync [:llm/select-provider :azgov])
  (rf/dispatch-sync [:llm/set-azgov-field :api-key "my-api-key"])
  (is (= "my-api-key" (get-in @rfdb/app-db [:llm :azgov :api-key])))
  (rf/dispatch-sync [:llm/set-azgov-field :auth-scheme :bearer])
  (is (= "" (get-in @rfdb/app-db [:llm :azgov :api-key]))))

(deftest legacy-plaintext-key-restores
  (rf/dispatch-sync [:app/initialize])
  (let [legacy (pr-str {:active :cloud
                        :cloud {:vendor :anthropic :api-key "sk-legacy" :remember-key? true}
                        :azgov {}})]
    (rf/dispatch-sync [:llm/settings-restored legacy])
    (is (= "sk-legacy" (get-in @rfdb/app-db [:llm :cloud :api-key]))
        "a pre-encryption plaintext key still loads for back-compat")))

(deftest new-snapshot-loads-blobs-not-keys
  (rf/dispatch-sync [:app/initialize])
  (let [snap (pr-str {:active :cloud
                      :cloud {:vendor :anthropic :remember-key? true}
                      :azgov {}
                      :saved {[:cloud :anthropic] {:v 1 :salt "s" :iv "i" :ct "c"}}})]
    (rf/dispatch-sync [:llm/settings-restored snap])
    (is (= "" (get-in @rfdb/app-db [:llm :cloud :api-key])) "no key filled at boot")
    (is (contains? (get-in @rfdb/app-db [:llm :saved]) [:cloud :anthropic]))))

;; --- Web Crypto round-trip (skipped when unavailable) ------------------------

(deftest crypto-roundtrip
  (if-not (crypto/available?)
    (is true "crypto.subtle unavailable in this runtime; covered by E2E")
    (async done
      (.then
       (crypto/encrypt "pw" "sk-secret")
       (fn [blob]
         (is (= 1 (:v blob)))
         (.then
          (crypto/decrypt "pw" blob)
          (fn [pt]
            (is (= "sk-secret" pt) "correct passphrase recovers the plaintext")
            (.then
             (.catch (crypto/decrypt "wrong-pw" blob) (fn [_] ::rejected))
             (fn [r]
               (is (= ::rejected r) "wrong passphrase rejects")
               (done))))))))))
