(ns onteater.eval
  "Node-library entry for the offline mapping-eval harness
  (build/eval-mapping.mjs). Exposes the REAL pipeline — format adapters,
  prompt construction (all strategies), response parsing, validation — to a
  plain-node script that supplies the HTTP transport. Nothing here duplicates
  prompt logic; the harness measures exactly what the app ships.

  Compile with `npx shadow-cljs compile eval` (build :eval, not part of
  `npm test` — the harness needs a live Ollama). All exported fns exchange
  JSON strings with the JS side, except the parsed ontology model, which rides
  through JS as an opaque CLJS value."
  (:require [clojure.string :as str]
            [onteater.format.core :as fmt]
            [onteater.format.geo]
            [onteater.format.native]
            [onteater.format.owl]
            [onteater.model.graph :as g]
            [onteater.model.mapping :as mapping]
            [onteater.llm.prompts :as prompts]
            [onteater.llm.providers :as providers]))

(defn parse-ontology
  "Parse ontology `text` (format auto-detected) -> opaque model value."
  [text]
  (fmt/open text))

(defn- session-for [model text]
  (mapping/new-session {:scenario {:text text}
                        :ontology-ref {:title (get-in model [:meta :title])}
                        :model "eval"}))

(defn- chunk-opts [model chunk strategy]
  (prompts/strategy-opts model chunk (keyword strategy)))

(defn- ollama-body [messages json-schema ollama-model]
  (let [{:keys [num-ctx]} (prompts/messages-num-ctx messages)
        req (providers/chat-request {:provider :ollama :model ollama-model}
                                    {:messages messages
                                     :json-schema json-schema
                                     :temperature 0.2
                                     :num-ctx num-ctx})]
    {:path (:path req) :body (:body req)}))

(defn choose-strategy
  "The strategy the app would auto-pick for this model+scenario (\"full\" /
  \"scoped\" / \"staged\")."
  [model text]
  (name (prompts/choose-strategy model text)))

(defn build-bodies
  "JSON array of {path, body} Ollama /api/chat requests — one per scenario chunk
  under `strategy` (\"full\" | \"scoped\" | \"staged\" | \"auto\")."
  [model text strategy ollama-model]
  (let [strategy (if (= "auto" strategy) (choose-strategy model text) strategy)
        sess     (session-for model text)
        chunks   (prompts/chunk-scenario text)]
    (js/JSON.stringify
     (clj->js {:strategy strategy
               :requests (for [c chunks
                               :let [opts (chunk-opts model c strategy)]]
                           (ollama-body (prompts/mapping-messages model sess c opts)
                                        ;; staged: restrict the enum to the coarse
                                        ;; view, exactly as the app does
                                        (prompts/mapping-schema
                                         model (when (= "staged" strategy)
                                                 (:include-ids opts)))
                                        ollama-model))}))))

;; Wire codec: strictly the app's own (prompts/->entry, prompts/entry->wire) —
;; the harness must decode/encode exactly as the app does or its metrics
;; measure a different pipeline.
(def ^:private wire->entry prompts/->entry)
(def ^:private entry->wire prompts/entry->wire)

(defn- metrics [model text entries statuses]
  (let [validated (prompts/validate-entries model text entries)
        n         (count validated)
        flagged   (fn [f] (count (filter #(contains? (:flags %) f) validated)))
        shallow   (count (filter #(and (seq (str (:node-id %)))
                                       (g/exists? model (:node-id %))
                                       (seq (g/children model (:node-id %))))
                                 validated))
        pct       (fn [c] (if (pos? n) (js/Math.round (* 100 (/ c n))) 0))]
    {:entries n
     :invalid-target (flagged :invalid-target)
     :invalid-target-pct (pct (flagged :invalid-target))
     :excerpt-not-found (flagged :excerpt-not-found)
     :excerpt-not-found-pct (pct (flagged :excerpt-not-found))
     :shallow shallow
     :shallow-pct (pct shallow)
     :mean-confidence (if (pos? n)
                        (/ (js/Math.round (* 100 (/ (transduce (map :confidence) + 0 validated) n))) 100)
                        0)
     :statuses statuses}))

(defn evaluate
  "Parse + merge + validate the raw reply `contents` (JSON array of response
  content strings, one per chunk). Returns JSON {metrics, entries} — `entries`
  in wire form, reusable as `refine-bodies` input for a staged run."
  [model text contents-json]
  (let [contents (js->clj (js/JSON.parse contents-json))
        parsed   (map prompts/parse-response contents)
        statuses (mapv (comp name :status) parsed)
        merged   (reduce (fn [acc p]
                           (mapping/merge-entries acc (prompts/validate-entries
                                                       model text (:entries p))))
                         [] parsed)]
    (js/JSON.stringify
     (clj->js {:metrics (metrics model text merged statuses)
               :entries (mapv entry->wire merged)}))))

(defn refine-bodies
  "For a staged run: JSON {batches, requests:[{path, body}]} narrowing the given
  wire `entries` (per-branch batches, exactly the app's refinement pass)."
  [model text entries-json ollama-model]
  (let [entries (mapv wire->entry (js->clj (js/JSON.parse entries-json) :keywordize-keys true))
        sess    (assoc (session-for model text) :entries entries)
        batches (prompts/refine-batches model sess)]
    (js/JSON.stringify
     (clj->js {:batches (mapv #(mapv entry->wire %) batches)
               :requests (for [b batches]
                           (ollama-body (prompts/refine-messages model sess b)
                                        (prompts/mapping-schema model)
                                        ollama-model))}))))

(defn apply-refinements
  "Fold refinement replies back into the wire `entries` and re-measure.
  `contents` = JSON array of refine-response content strings. Returns JSON
  {metrics, entries}."
  [model text entries-json contents-json]
  (let [entries  (mapv wire->entry (js->clj (js/JSON.parse entries-json) :keywordize-keys true))
        sess     (assoc (session-for model text) :entries entries)
        contents (js->clj (js/JSON.parse contents-json))
        parsed   (map prompts/parse-response contents)
        statuses (mapv (comp name :status) parsed)
        refined  (reduce (fn [s p] (prompts/apply-refinements model s (:entries p)))
                         sess parsed)]
    (js/JSON.stringify
     (clj->js {:metrics (metrics model text (:entries refined) statuses)
               :entries (mapv entry->wire (:entries refined))}))))

(defn prompt-report
  "JSON size report for one scenario: per-strategy ontology-block token estimates
  and the num_ctx each first-chunk request would get. Runs no LLM — quick sanity
  check that a strategy fits a model's context."
  [model text]
  (let [sess   (session-for model text)
        chunk  (first (prompts/chunk-scenario text))
        per    (fn [strategy]
                 (let [msgs (prompts/mapping-messages
                             model sess chunk (chunk-opts model chunk strategy))]
                   {:tokens (transduce (map #(prompts/estimate-tokens (:content %))) + 0 msgs)
                    :num-ctx (:num-ctx (prompts/messages-num-ctx msgs))}))]
    (js/JSON.stringify
     (clj->js {:auto (choose-strategy model text)
               :full (per "full") :scoped (per "scoped") :staged (per "staged")}))))
