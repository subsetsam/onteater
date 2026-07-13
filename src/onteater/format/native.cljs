(ns onteater.format.native
  "Onteater's own lossless ontology format.

  Where the geo adapter must faithfully reproduce someone else's file layout, the
  native format's only job is to serialise the *entire canonical model* exactly —
  nodes, edges, groups, meta, and residual — so that parse ∘ serialize is the
  identity on the model. It is the 'Export as…' safety net and the natural home
  for ontologies authored from scratch inside Onteater.

  On-disk shape (JSON):

    {\"onteater/format-version\": 1,
     \"meta\":   {..edn-as-json..},
     \"nodes\":  {id -> node},
     \"edges\":  {id -> edge},
     \"groups\": {id -> group},
     \"residual\": <original source text or null>,
     \"order\":  [id ...]}

  Keyword keys/values in the model (e.g. :kind :class, edge :type) are encoded
  with an EDN-ish tagging scheme so they survive the JSON round-trip precisely."
  (:require [clojure.string :as str]
            [onteater.format.core :as fmt]))

(def format-version 1)

;; --- keyword <-> tagged-string encoding ------------------------------------
;; JSON has no keywords or sets. We encode keywords as \"\uEDNk:namespaced/name\"
;; and sets as {\"\uEDNset\": [items]} so the model round-trips exactly. The
;; sentinel prefix is a private-use unicode char so it cannot collide with real
;; string data.

(def ^:private kw-prefix "kw:")
(def ^:private set-key "set")

(defn- encode-key [k]
  (cond
    (keyword? k) (str kw-prefix (subs (str k) 1))
    :else        (str k)))

(defn- encode [x]
  (cond
    (keyword? x) (str kw-prefix (subs (str x) 1))   ; :a/b -> "kw:a/b"
    (set? x)     {set-key (mapv encode x)}
    (map? x)     (reduce-kv (fn [m k v] (assoc m (encode-key k) (encode v))) {} x)
    (coll? x)    (mapv encode x)
    :else        x))

(defn- decode-key [k]
  (if (and (string? k) (str/starts-with? k kw-prefix))
    (keyword (subs k (count kw-prefix)))
    k))

(defn- decode [x]
  (cond
    (and (string? x) (str/starts-with? x kw-prefix))
    (keyword (subs x (count kw-prefix)))

    (and (map? x) (contains? x set-key))
    (into #{} (map decode) (get x set-key))

    (map? x)
    (reduce-kv (fn [m k v] (assoc m (decode-key k) (decode v))) {} x)

    (vector? x) (mapv decode x)
    :else       x))

;; --- serialize / parse ------------------------------------------------------

(defn serialize-model
  "Serialise the whole canonical model to native JSON text (2-space indent)."
  [model]
  (let [payload {"onteater/format-version" format-version
                 "meta"     (encode (:meta model))
                 "nodes"    (encode (:nodes model))
                 "edges"    (encode (:edges model))
                 "groups"   (encode (:groups model))
                 "residual" (:residual model)
                 "order"    (encode (vec (:order model)))}]
    (js/JSON.stringify (clj->js payload) nil 2)))

(defn parse-str
  "Parse native JSON text back into a canonical model. Throws user-readable
  ex-info on malformed input or a version mismatch."
  [raw-str]
  (let [data (try (js->clj (js/JSON.parse raw-str) :keywordize-keys false)
                  (catch :default e
                    (throw (ex-info "This file is not valid JSON."
                                    {:message (str "JSON parse error: " (.-message e))}))))]
    (when-not (and (map? data) (contains? data "onteater/format-version"))
      (throw (ex-info "Not an Onteater native file."
                      {:message "Missing onteater/format-version header."})))
    {:meta     (decode (get data "meta"))
     :nodes    (decode (get data "nodes"))
     :edges    (decode (get data "edges"))
     :groups   (decode (get data "groups"))
     :residual (get data "residual")
     :order    (decode (get data "order" []))}))

(defn detect-str
  "Confidence that `raw-str` is an Onteater native file — decisive on the header."
  [raw-str]
  (if (and (fmt/looks-like-json? raw-str)
           (clojure.string/includes? raw-str "onteater/format-version"))
    0.99
    0.0))

(defrecord NativeFormat []
  fmt/OntologyFormat
  (format-id    [_] :onteater-native)
  (display-name [_] "Onteater native (JSON)")
  (detect       [_ raw-str] (detect-str raw-str))
  (parse        [_ raw-str] (parse-str raw-str))
  (serialize    [_ model]   (serialize-model model)))

(defonce ^{:doc "Registers the native adapter on namespace load."} registered
  (fmt/register! (->NativeFormat)))
