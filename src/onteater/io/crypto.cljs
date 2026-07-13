(ns onteater.io.crypto
  "Passphrase-based encryption for persisted LLM credentials, built on the
  browser-native Web Crypto API (`crypto.subtle`) — no third-party dependency.

  A saved credential is encrypted with `AES-GCM` under a 256-bit key derived
  from the user's passphrase via `PBKDF2` (SHA-256, high iteration count) with a
  fresh random salt. Only the ciphertext, salt, and IV are ever written to
  IndexedDB (see onteater.events.providers); the passphrase and the plaintext
  key live only in memory for the session.

  A `blob` is the serialisable ciphertext record
  `{:v 1 :salt <b64> :iv <b64> :ct <b64>}` — pr-str-safe, so it round-trips
  through the existing EDN snapshot.

  Everything here is asynchronous: `encrypt`/`decrypt` return JS Promises, and
  the two re-frame effects (`:io/encrypt-save`, `:io/unlock-all`) do the async
  work and then dispatch a result event. This namespace is the effectful shell;
  the pure domain layer never calls it directly.

  Security note: a browser app that calls LLMs from page JS must be able to
  recover the key to plaintext at request time, so this protects the credential
  *at rest* (someone copying IndexedDB/disk) — not against in-page script
  injection. Session-only (never persisting) remains the strongest option."
  (:require [re-frame.core :as rf]))

;; Reach the Web Crypto object off globalThis so a missing `crypto` global
;; yields nil rather than a ReferenceError (keeps `available?` cheap in any
;; runtime, including the node test target).
(def ^:private webcrypto (.-crypto js/globalThis))

(defn available?
  "True when `crypto.subtle` is present (a secure context). The Settings UI
  disables key persistence when this is false rather than storing plaintext."
  []
  (some? (some-> webcrypto .-subtle)))

(def ^:private pbkdf2-iterations 200000)

;; --- base64 <-> bytes --------------------------------------------------------
;; Credentials are short (well under btoa's argument limits), so the simple
;; String.fromCharCode / charCodeAt bridge is safe here.

(defn- buf->b64 [array-buffer]
  (let [bytes (js/Uint8Array. array-buffer)]
    (js/btoa (.apply js/String.fromCharCode nil bytes))))

(defn- b64->bytes [b64]
  (let [s   (js/atob b64)
        n   (.-length s)
        arr (js/Uint8Array. n)]
    (dotimes [i n] (aset arr i (.charCodeAt s i)))
    arr))

;; --- key derivation ----------------------------------------------------------

(defn- derive-key
  "Promise<CryptoKey> — an AES-GCM key derived from `passphrase` and `salt`
  (a Uint8Array). Non-extractable; usable for encrypt + decrypt."
  [passphrase salt]
  (let [subtle (.-subtle webcrypto)
        raw    (.encode (js/TextEncoder.) passphrase)]
    (-> (.importKey subtle "raw" raw #js {:name "PBKDF2"} false #js ["deriveKey"])
        (.then (fn [base-key]
                 (.deriveKey subtle
                             #js {:name "PBKDF2" :salt salt
                                  :iterations pbkdf2-iterations :hash "SHA-256"}
                             base-key
                             #js {:name "AES-GCM" :length 256}
                             false
                             #js ["encrypt" "decrypt"]))))))

;; --- encrypt / decrypt -------------------------------------------------------

(defn encrypt
  "Promise<blob> — encrypt the plaintext `s` under `passphrase` with a fresh
  random salt (16 bytes) and IV (12 bytes)."
  [passphrase s]
  (let [subtle (.-subtle webcrypto)
        salt   (.getRandomValues webcrypto (js/Uint8Array. 16))
        iv     (.getRandomValues webcrypto (js/Uint8Array. 12))
        data   (.encode (js/TextEncoder.) s)]
    (-> (derive-key passphrase salt)
        (.then (fn [key]
                 (.encrypt subtle #js {:name "AES-GCM" :iv iv} key data)))
        (.then (fn [ct]
                 {:v 1
                  :salt (buf->b64 (.-buffer salt))
                  :iv   (buf->b64 (.-buffer iv))
                  :ct   (buf->b64 ct)})))))

(defn decrypt
  "Promise<string> — decrypt a `blob`. The promise REJECTS on a wrong passphrase
  (AES-GCM authentication-tag failure), which the caller reads as 'incorrect
  passphrase'."
  [passphrase {:keys [salt iv ct]}]
  (let [subtle  (.-subtle webcrypto)
        salt-b  (b64->bytes salt)
        iv-b    (b64->bytes iv)
        ct-b    (b64->bytes ct)]
    (-> (derive-key passphrase salt-b)
        (.then (fn [key]
                 (.decrypt subtle #js {:name "AES-GCM" :iv iv-b} key (.-buffer ct-b))))
        (.then (fn [buf] (.decode (js/TextDecoder.) buf))))))

;; --- re-frame effects --------------------------------------------------------

;; :io/encrypt-save {:passphrase :slot :plaintext :on-done [event slot]}
;; Encrypt one slot's key and dispatch `on-done` with the resulting blob
;; appended. Encryption errors are swallowed (autosave contract): a failed
;; encrypt simply leaves the previous saved blob untouched.
(rf/reg-fx
 :io/encrypt-save
 (fn [{:keys [passphrase plaintext on-done]}]
   (-> (encrypt passphrase plaintext)
       (.then (fn [blob] (rf/dispatch (conj on-done blob))))
       (.catch (fn [_] nil)))))

;; :io/unlock-all {:passphrase :saved {slot blob} :on-ok [ev ...] :on-fail [ev]}
;; Attempt to decrypt every saved blob with `passphrase`. Dispatches `on-ok`
;; with the {slot -> plaintext} map of successes appended when at least one
;; decrypts (a correct passphrase decrypts all, since one passphrase protects
;; every slot); dispatches `on-fail` when none do (wrong passphrase).
(rf/reg-fx
 :io/unlock-all
 (fn [{:keys [passphrase saved on-ok on-fail]}]
   (let [slots (vec (keys saved))]
     (-> (js/Promise.allSettled
          (into-array (map #(decrypt passphrase (get saved %)) slots)))
         (.then (fn [results]
                  (let [pairs (keep-indexed
                               (fn [i r]
                                 (when (= "fulfilled" (.-status r))
                                   [(nth slots i) (.-value r)]))
                               results)]
                    (if (seq pairs)
                      (rf/dispatch (conj on-ok (into {} pairs)))
                      (rf/dispatch on-fail)))))
         (.catch (fn [_] (rf/dispatch on-fail)))))))
