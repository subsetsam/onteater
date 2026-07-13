(ns onteater.llm.prompts-test
  "Tests for the (pure, LLM-independent) mapping prompt + parsing layer.
  Response parsing/validation is tested against canned JSON, so the suite never
  needs a live model."
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [onteater.model.graph :as g]
            [onteater.format.geo :as geo]
            [onteater.model.mapping :as m]
            [onteater.llm.prompts :as p]))

(def ^:private fs (js/require "fs"))
(defn- geo-model [] (geo/parse-str (.readFileSync fs "examples/galactic-economic-ontology.json" "utf8")))

(deftest compaction-is-compact-and-complete
  (let [model (geo-model)
        compact (p/compact-ontology model)]
    (testing "every real node id appears in the compacted view"
      (is (str/includes? compact "geo:Leverage"))
      (is (str/includes? compact "geo:Chokepoint")))
    (testing "compaction is far smaller than the raw file"
      (is (< (p/estimate-tokens compact) (quot (p/estimate-tokens (:residual model)) 2))))
    (testing "fits a generous budget (no two-stage fallback needed for the sample)"
      (is (p/compaction-fits? model 12000)))))

(deftest chunking
  (testing "small scenarios are a single chunk"
    (is (= 1 (count (p/chunk-scenario "short text")))))
  (testing "large scenarios split into several chunks"
    (let [big (clojure.string/join "\n\n" (repeat 400 "A dense paragraph of scenario prose."))
          chunks (p/chunk-scenario big {:char-budget 2000})]
      (is (> (count chunks) 1))
      (is (every? #(<= (count %) 2600) chunks)))))   ; budget + overlap slack

(deftest parse-tolerant-and-safe
  (let [clean "{\"entries\":[{\"excerpt\":\"chokepoints\",\"occurrence\":1,\"node_id\":\"geo:Chokepoint\",\"relation\":\"instance-of\",\"confidence\":0.9,\"rationale\":\"r\"}],\"unmapped\":[\"x\"]}"]
    (testing "parses a well-formed structured-output JSON string -> :ok"
      (let [{:keys [entries unmapped status]} (p/parse-response clean)]
        (is (= 1 (count entries)))
        (is (= "geo:Chokepoint" (:node-id (first entries))))
        (is (= :instance-of (:relation (first entries))))
        (is (= ["x"] unmapped))
        (is (= :ok status))))
    (testing "recovers JSON wrapped in a Markdown code fence (MLX-runner symptom)"
      (let [{:keys [entries status]} (p/parse-response (str "```json\n" clean "\n```"))]
        (is (= :ok status))
        (is (= "geo:Chokepoint" (:node-id (first entries))))))
    (testing "recovers JSON embedded after prose / a thinking preamble"
      (let [{:keys [entries status]} (p/parse-response (str "Okay, here are the mappings I found:\n\n" clean))]
        (is (= :ok status))
        (is (= 1 (count entries)))))
    (testing "braces inside quoted values do not confuse the scanner"
      (let [json "{\"entries\":[{\"excerpt\":\"a {curly} term\",\"node_id\":\"geo:Chokepoint\",\"relation\":\"mentions\",\"confidence\":0.5,\"rationale\":\"r\"}]}"
            {:keys [entries status]} (p/parse-response (str "note: " json))]
        (is (= :ok status))
        (is (= "a {curly} term" (:excerpt (first entries)))))))
  (testing "non-blank, non-JSON reply (e.g. a bare id list) -> :no-json, empty entries, never throws"
    (let [{:keys [entries unmapped status]} (p/parse-response "geo:Chokepoint, geo:Leverage")]
      (is (= [] entries))
      (is (= [] unmapped))
      (is (= :no-json status))))
  (testing "blank/nil reply -> :empty"
    (is (= :empty (:status (p/parse-response ""))))
    (is (= :empty (:status (p/parse-response nil))))))

(deftest validation-flags-bad-entries
  (let [model (geo-model)
        text  "The chokepoints were seized."
        raw   [(m/new-entry {:excerpt "chokepoints" :node-id "geo:Chokepoint"})   ; ok if exists
               (m/new-entry {:excerpt "chokepoints" :node-id "geo:DoesNotExist"}) ; invalid target
               (m/new-entry {:excerpt "not in the text" :node-id "geo:Chokepoint"})] ; excerpt missing
        validated (p/validate-entries model text raw)]
    (is (contains? (:flags (nth validated 1)) :invalid-target))
    (is (contains? (:flags (nth validated 2)) :excerpt-not-found))
    (testing "a good entry has no flags (assuming geo:Chokepoint exists)"
      (when (g/exists? model "geo:Chokepoint")
        (is (empty? (:flags (nth validated 0))))))))

(deftest forced-constraints-appear-in-prompt
  (let [model (geo-model)
        session (-> (m/new-session {:scenario {:text "x"} :model "t"})
                    (m/add-entry {:excerpt "the Act" :node-id "geo:Chokepoint" :status :forced}))
        sys (p/system-prompt model session)]
    (is (str/includes? sys "HARD CONSTRAINTS"))
    (is (str/includes? sys "geo:Chokepoint"))))

(deftest schema-shape
  (let [s (p/mapping-schema)]
    (is (= "object" (:type s)))
    (is (contains? (:properties s) :entries))))

(deftest parse-mapping-updates-extracts-blocks
  (let [content (str "Sure, here's my reasoning.\n\n"
                     "```mapping-update\n"
                     "{\"ops\":[{\"op\":\"update\",\"entry\":{\"excerpt\":\"chokepoint\",\"node_id\":\"geo:Chokepoint\",\"relation\":\"instance-of\",\"confidence\":0.9,\"rationale\":\"better fit\"}}],\"reason\":\"more precise\"}\n"
                     "```\n\nHope that helps.")
        {:keys [prose updates]} (p/parse-mapping-updates content)]
    (testing "prose has the block removed"
      (is (not (str/includes? prose "mapping-update")))
      (is (str/includes? prose "Hope that helps.")))
    (testing "the update block is parsed into ops"
      (is (= 1 (count updates)))
      (is (= "more precise" (:reason (first updates))))
      (let [op (first (:ops (first updates)))]
        (is (= :update (:op op)))
        (is (= "geo:Chokepoint" (:node-id (:entry op))))
        (is (= :instance-of (:relation (:entry op)))))))
  (testing "no blocks -> empty updates, prose unchanged"
    (let [{:keys [prose updates]} (p/parse-mapping-updates "just prose")]
      (is (= "just prose" prose))
      (is (empty? updates))))
  (testing "malformed block JSON is skipped, never throws"
    (is (empty? (:updates (p/parse-mapping-updates "```mapping-update\n{bad json}\n```"))))))
