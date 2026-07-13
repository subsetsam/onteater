(ns onteater.io.idb
  "A tiny IndexedDB key/value wrapper for crash-safety autosave.
  localStorage is too small for a large ontology plus mapping sessions, so
  snapshots go to IndexedDB. We store plain strings (the caller pr-str's the
  app-db subset) under a fixed store, exposing two effects: `:io/idb-save` and
  `:io/idb-load`. All errors are swallowed to a no-op — autosave must never break
  the app."
  (:require [re-frame.core :as rf]))

(def ^:private db-name "onteater")
(def ^:private store-name "kv")

(defn- with-store
  "Open the database (creating the object store on first use) and invoke
  (f store) inside a transaction of `mode`. Best-effort; calls `on-err` on failure."
  [mode f on-err]
  (try
    (let [req (.open js/indexedDB db-name 1)]
      (set! (.-onupgradeneeded req)
            (fn [_] (let [db (.-result req)]
                      (when-not (.contains (.-objectStoreNames db) store-name)
                        (.createObjectStore db store-name)))))
      (set! (.-onsuccess req)
            (fn [_]
              (let [db (.-result req)
                    tx (.transaction db store-name mode)
                    st (.objectStore tx store-name)]
                (f st))))
      (set! (.-onerror req) (fn [_] (when on-err (on-err (.-error req))))))
    (catch :default e (when on-err (on-err e)))))

;; :io/idb-save {:key k :value string-value} — persist a string under a key.
(rf/reg-fx
 :io/idb-save
 (fn [{:keys [key value]}]
   (with-store "readwrite"
     (fn [st] (.put st value key))
     (fn [_] nil))))

;; :io/idb-load {:key k :on-loaded [event]} — read a string; dispatches on-loaded
;; with the value (or nil) appended.
(rf/reg-fx
 :io/idb-load
 (fn [{:keys [key on-loaded]}]
   (with-store "readonly"
     (fn [st]
       (let [req (.get st key)]
         (set! (.-onsuccess req)
               (fn [_] (when on-loaded (rf/dispatch (conj on-loaded (.-result req))))))
         (set! (.-onerror req)
               (fn [_] (when on-loaded (rf/dispatch (conj on-loaded nil)))))))
     (fn [_] (when on-loaded (rf/dispatch (conj on-loaded nil)))))))

;; :io/idb-delete {:key k} — remove a key (used after an explicit save/recovery).
(rf/reg-fx
 :io/idb-delete
 (fn [{:keys [key]}]
   (with-store "readwrite"
     (fn [st] (.delete st key))
     (fn [_] nil))))
