(ns onteater.format.core
  "Ontology format adapters: the protocol, a registry, and open/save dispatch.

  Onteater's data model is format-neutral. Each concrete file format
  is an adapter implementing `OntologyFormat`: it can guess whether a blob is in
  its format (`detect`), turn a blob into the canonical model (`parse`), and turn
  a canonical model back into a faithful blob (`serialize`). Adding OWL/JSON-LD,
  SKOS, or plain node-link JSON later is one new namespace + one `register!` call.

  `open` asks every registered adapter for a detection confidence and parses with
  the winner (the user may override in the UI). `serialize-with` serialises a model
  through a named adapter. Round-trip fidelity is each adapter's responsibility;
  the geo adapter's golden round-trip test is the gate on the whole
  project."
  (:require [clojure.string :as str]))

(defprotocol OntologyFormat
  "Contract every ontology file format adapter must satisfy."
  (format-id [_]
    "Return a stable keyword identifying this format, e.g. :geo-reference-json.")
  (display-name [_]
    "Human-readable format name for the UI (open/save dialogs).")
  (detect [_ raw-str]
    "Return a confidence in [0.0, 1.0] that `raw-str` is in this format.")
  (parse [_ raw-str]
    "Parse `raw-str` into a canonical model. Throw an ex-info with a
     user-readable :message on malformed input.")
  (serialize [_ model]
    "Serialise a canonical `model` back to a string in this format, as faithfully
     as the format allows (ideally byte-for-byte on an untouched round-trip)."))

;; A registry keyed by format-id. `defonce` so hot-reload does not clobber
;; registrations made by adapter namespaces at load time.
(defonce ^:private registry (atom {}))

(defn register!
  "Register an adapter instance under its `format-id`. Idempotent per id."
  [adapter]
  (swap! registry assoc (format-id adapter) adapter)
  adapter)

(defn adapters [] (vals @registry))
(defn adapter  [id] (get @registry id))

(defn detect-all
  "Return a vector of {:id kw :name str :confidence n} for every registered
  adapter, sorted by descending confidence — what the open dialog shows so the
  user can confirm or override the auto-detected format."
  [raw-str]
  (->> (adapters)
       (map (fn [a] {:id (format-id a)
                     :name (display-name a)
                     :confidence (try (detect a raw-str) (catch :default _ 0.0))}))
       (sort-by :confidence >)
       vec))

(defn best-format
  "The format-id with the highest detection confidence for `raw-str`, or nil if
  nothing is confident (max confidence below `threshold`, default 0.15)."
  ([raw-str] (best-format raw-str 0.15))
  ([raw-str threshold]
   (let [{:keys [id confidence]} (first (detect-all raw-str))]
     (when (and id (>= confidence threshold)) id))))

(defn open
  "Parse `raw-str`, choosing an adapter by auto-detection unless `format-id-kw` is
  supplied (user override). Returns the canonical model with [:meta :format] set.
  Throws an ex-info {:message ...} if no adapter matches or parsing fails."
  ([raw-str] (open raw-str nil))
  ([raw-str format-id-kw]
   (let [id (or format-id-kw (best-format raw-str))]
     (when-not id
       (throw (ex-info "Could not recognise this file's format. Try choosing one explicitly."
                       {:message "Unrecognised ontology format."})))
     (let [a (adapter id)
           model (parse a raw-str)]
       (assoc-in model [:meta :format] id)))))

(defn serialize-with
  "Serialise `model` through the adapter named `format-id-kw`."
  [format-id-kw model]
  (if-let [a (adapter format-id-kw)]
    (serialize a model)
    (throw (ex-info (str "No adapter registered for format " format-id-kw)
                    {:message (str "Unknown format " format-id-kw)}))))

(defn looks-like-json?
  "Cheap heuristic used by several adapters' `detect`: does the trimmed string
  start like a JSON object/array?"
  [raw-str]
  (let [t (str/triml (or raw-str ""))]
    (or (str/starts-with? t "{") (str/starts-with? t "["))))
