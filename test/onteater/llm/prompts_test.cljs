(ns onteater.llm.prompts-test
  "Tests for the (pure, LLM-independent) mapping prompt + parsing layer.
  Response parsing/validation is tested against canned JSON, so the suite never
  needs a live model."
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [onteater.model.graph :as g]
            [onteater.model.timeline :as timeline]
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

(deftest compaction-renders-structure
  (let [model (geo-model)
        compact (p/compact-ontology model)]
    (testing "module sections carry their real titles, not just codes"
      (is (str/includes? compact "Actors, Organs, and Roles")))
    (testing "subsection titles appear as sub-headings"
      (is (str/includes? compact "### ")))
    (testing "subclass nesting is rendered as indentation (a child on an indented line)"
      ;; geo:GalacticGovernment ⊂ geo:Polity ⊂ geo:Agent — must appear indented
      (is (re-find #"(?m)^\s{2,}geo:GalacticGovernment" compact)))
    (testing "the spine section renders before the modules"
      (is (< (str/index-of compact "Spine") (str/index-of compact "Actors, Organs"))))
    (testing "relation glosses survive (geo:memberOf has one)"
      (is (re-find #"geo:memberOf \| Member of \| .+" compact)))
    (testing "external stubs are not listed as lines"
      (is (not (re-find #"(?m)^\s*bfo:" compact))))))

(deftest compaction-never-loses-nodes
  (testing "members of a subclass cycle (no in-set root) still render"
    (let [model (-> (g/empty-model)
                    (g/add-node {:id "x:A" :label "A" :kind :class :module "M"})
                    (g/add-node {:id "x:B" :label "B" :kind :class :module "M"})
                    (g/add-edge "x:A" :subclass-of "x:B")
                    (g/add-edge "x:B" :subclass-of "x:A"))
          compact (p/compact-ontology model)]
      (is (str/includes? compact "x:A"))
      (is (str/includes? compact "x:B"))))
  (testing "non-class kinds are tagged so the LLM can tell them from classes"
    (let [model (-> (g/empty-model)
                    (g/add-node {:id "x:C" :label "C" :kind :class :module "M"})
                    (g/add-node {:id "x:I" :label "I" :kind :individual :module "M"}))
          compact (p/compact-ontology model)]
      (is (re-find #"x:I \| I \| individual" compact))
      (is (not (re-find #"x:C \| C \| class" compact))))))

(deftest sentence-truncation
  (testing "short strings pass through"
    (is (= "One sentence." (p/sentence-truncate "One sentence." 120))))
  (testing "cuts on the sentence boundary when one exists past n/3"
    (is (= "First sentence here."
           (p/sentence-truncate "First sentence here. Second sentence follows at length." 30))))
  (testing "hard-cuts with an ellipsis when no boundary is available"
    (let [out (p/sentence-truncate "averyunbrokenrunoftextwithnosentenceboundaryatall" 20)]
      (is (<= (count out) 20))
      (is (str/ends-with? out "…")))))

(deftest scoped-compaction-and-scoring
  (let [model (geo-model)]
    (testing "lexical scoring surfaces the mentioned class"
      (let [scores (p/score-nodes model "The blockade created chokepoints at the hyperlane.")]
        (is (contains? scores "geo:Chokepoint"))))
    (testing "scoped ids close over ancestors (orientation) and siblings (contrast)"
      (let [ids (p/scoped-ids model "chokepoints")]
        (is (contains? ids "geo:Chokepoint"))
        (is (some ids (g/parents model "geo:Chokepoint")))))
    (testing "a scoped view compresses unmatched modules to an omitted line"
      (let [compact (p/compact-ontology model {:include-ids #{"geo:Chokepoint"}})]
        (is (str/includes? compact "geo:Chokepoint"))
        (is (str/includes? compact "omitted"))
        (is (not (str/includes? compact "geo:GalacticGovernment")))))))

(deftest coarse-view-marks-hidden-subtrees
  (let [model (geo-model)
        {:keys [ids hidden]} (p/coarse-view model)]
    (testing "the coarse view is a strict subset with hidden-descendant counts"
      (is (pos? (count ids)))
      (is (seq hidden))
      (is (every? pos? (vals hidden))))
    (testing "the rendered coarse view carries the (+N more …) marker and is smaller"
      (let [coarse (p/compact-ontology model {:include-ids ids :hidden-children hidden})]
        (is (str/includes? coarse "more specific subclasses not shown"))
        (is (< (count coarse) (count (p/compact-ontology model))))))))

(deftest strategy-gate
  (let [model (geo-model)
        text  "The blockade of Naboo."]
    (testing "the sample ontology maps single-pass at the default budget"
      (is (= :full (p/choose-strategy model text))))
    (testing "a budget below :full but above the scoped size falls back to scoped"
      (let [scoped-tokens (p/estimate-tokens
                           (p/compact-ontology model (p/scoped-opts model text)))]
        (is (= :scoped (p/choose-strategy model text (inc scoped-tokens))))))
    (testing "a hopeless budget falls back to staged"
      (is (= :staged (p/choose-strategy model text 10))))))

(deftest num-ctx-sizing
  (testing "small requests keep the floor"
    (is (= 4096 (:num-ctx (p/messages-num-ctx [{:content "hi"}])))))
  (testing "larger payloads scale by powers of two"
    (let [msgs [{:content (apply str (repeat 40000 "x"))}]]   ; ~10k tokens
      (is (= 16384 (:num-ctx (p/messages-num-ctx msgs))))))
  (testing "the cap is enforced and overflow reported"
    (let [msgs [{:content (apply str (repeat 200000 "x"))}]]  ; ~50k tokens
      (is (= 32768 (:num-ctx (p/messages-num-ctx msgs))))
      (is (:overflow? (p/messages-num-ctx msgs))))))

(deftest routing-guide-derived-from-structure
  (let [model (geo-model)
        guide (p/routing-guide model)]
    (is (str/includes? guide "HOW TO CHOOSE A NODE"))
    (testing "occurrent, role, and disposition bullets are derived (never hard-coded ids)"
      (is (str/includes? guide "occurrent"))
      (is (str/includes? guide "ROLES"))
      (is (str/includes? guide "DISPOSITIONS")))
    (testing "the most-specific bullet is always present"
      (is (str/includes? guide "MOST SPECIFIC"))))
  (testing "disposition discovery finds the power-word subtree"
    (is (contains? (timeline/disposition-ids (geo-model)) "geo:Leverage")))
  (testing "an empty model degrades to just the most-specific bullet"
    (let [guide (p/routing-guide (g/empty-model))]
      (is (str/includes? guide "MOST SPECIFIC"))
      (is (not (str/includes? guide "DISPOSITIONS"))))))

(deftest schema-node-id-enum
  (let [model (geo-model)]
    (testing "without a model the schema stays unconstrained"
      (let [t (get-in (p/mapping-schema) [:properties :entries :items :properties :node_id])]
        (is (nil? (:enum t)))))
    (testing "with a model node_id is an enum of real ids"
      (let [t (get-in (p/mapping-schema model) [:properties :entries :items :properties :node_id])]
        (is (vector? (:enum t)))
        (is (some #{"geo:Chokepoint"} (:enum t)))
        (testing "external stubs are excluded"
          (is (not (some #(str/starts-with? % "bfo:") (:enum t)))))))))

(deftest system-prompt-composition
  (let [model (geo-model)
        session (-> (m/new-session {:scenario {:text "x"} :model "t"})
                    (m/add-entry {:excerpt "the blockade" :node-id "geo:Chokepoint"
                                  :status :accepted :rationale "clear fit"}))]
    (testing "routing guide + modeling notes (geo prompt-notes) are included"
      (let [sys (p/system-prompt model session)]
        (is (str/includes? sys "HOW TO CHOOSE A NODE"))
        (is (str/includes? sys "MODELING NOTES"))))
    (testing "accepted entries render as worked examples"
      (is (str/includes? (p/system-prompt model session) "already accepted")))
    (testing "a curated briefing is injected verbatim; coarse mode adds its rule"
      (let [sys (p/system-prompt model session {:briefing "Prefer geo:X over geo:Y."
                                                :coarse? true})]
        (is (str/includes? sys "ONTOLOGY BRIEFING"))
        (is (str/includes? sys "Prefer geo:X over geo:Y."))
        (is (str/includes? sys "COARSE view"))))))

(deftest refinement-pass
  (let [model (geo-model)
        ;; geo:Polity has subclasses (GalacticGovernment, MemberWorld) — refinable.
        ;; geo:Chokepoint entry: refinable only if it has children; the forced one
        ;; must never appear regardless.
        session (-> (m/new-session {:scenario {:text "Naboo is a member world."} :model "t"})
                    (m/add-entry {:excerpt "Naboo" :occurrence 1 :node-id "geo:Polity"
                                  :status :proposed})
                    (m/add-entry {:excerpt "member world" :node-id "geo:Polity"
                                  :status :forced}))]
    (testing "only proposed entries with subclassed targets are refinable"
      (let [refinable (p/refinable-entries model session)]
        (is (= 1 (count refinable)))
        (is (= "Naboo" (:excerpt (first refinable))))))
    (testing "batches group per module"
      (let [batches (p/refine-batches model session)]
        (is (= 1 (count batches)))
        (is (= "Naboo" (:excerpt (first (first batches)))))))
    (testing "refine messages render only the branch neighbourhood, fully glossed"
      (let [[{sys :content} {user :content}]
            (p/refine-messages model session (first (p/refine-batches model session)))]
        (is (str/includes? sys "geo:MemberWorld"))     ; the narrowing candidates
        (is (str/includes? sys "WRONG BRANCH"))        ; the escape hatch
        (is (str/includes? user "\"node_id\": \"geo:Polity\""))))
    (testing "apply-refinements retargets by (excerpt, occurrence), skipping curated + unknown ids"
      (let [refined [(m/new-entry {:excerpt "Naboo" :occurrence 1
                                   :node-id "geo:MemberWorld" :confidence 0.9
                                   :rationale "a Senate world"})
                     (m/new-entry {:excerpt "member world" :node-id "geo:GalacticGovernment"})
                     (m/new-entry {:excerpt "Naboo" :occurrence 1 :node-id "geo:Nope"})]
            s' (p/apply-refinements model session refined)
            naboo (first (filter #(= "Naboo" (:excerpt %)) (:entries s')))
            forced (first (filter #(= :forced (:status %)) (:entries s')))]
        (is (= "geo:MemberWorld" (:node-id naboo)))
        (is (= "geo:Polity" (:node-id forced)) "forced entry untouched")))))

(deftest briefing-parse-validate-render
  (let [model (geo-model)
        good  {:module_summaries [{:module "ACT" :summary "Actors and their organs."}]
              :disambiguations  [{:use "geo:Chokepoint" :not "geo:Polity" :rule "junctions, not polities"}
                                 {:use "geo:NotReal" :not "geo:Polity" :rule "bogus"}]
              :cautions ["Don't type roles as bearers."]}]
    (testing "unknown-id disambiguations are dropped and counted"
      (let [{:keys [briefing dropped status]} (p/parse-briefing model good)]
        (is (= :ok status))
        (is (= 1 dropped))
        (is (= 1 (count (:disambiguations briefing))))
        (testing "rendered text carries the surviving content"
          (let [text (p/briefing-text briefing)]
            (is (str/includes? text "geo:Chokepoint"))
            (is (str/includes? text "Actors and their organs."))
            (is (not (str/includes? text "geo:NotReal")))))))
    (testing "tolerant parsing: fenced JSON string, prose, blank"
      (is (= :ok (:status (p/parse-briefing model (str "```json\n"
                                                       (js/JSON.stringify (clj->js good))
                                                       "\n```")))))
      (is (= :no-json (:status (p/parse-briefing model "no json here"))))
      (is (= :empty (:status (p/parse-briefing model "")))))))

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
    (is (empty? (:updates (p/parse-mapping-updates "```mapping-update\n{bad json}\n```")))))
  (testing "a timeline-targeted op is normalized with target + value"
    (let [content (str "```mapping-update\n"
                       "{\"ops\":[{\"op\":\"update\",\"target\":\"relation\",\"value\":{\"id\":\"r1\",\"type\":\"causes\",\"property_id\":\"geo:respondsTo\"}}],\"reason\":\"tighter\"}\n"
                       "```")
          op (first (:ops (first (:updates (p/parse-mapping-updates content)))))]
      (is (= :relation (:target op)))
      (is (= :causes (get-in op [:value :type])))
      (is (= "geo:respondsTo" (get-in op [:value :property-id]))))))

;; --- timeline extraction pass ----------------------------------------------

(deftest occurrent-compaction-foregrounds-occurrents
  (let [model (geo-model)
        oc (p/occurrent-compaction model)]
    (testing "occurrent classes appear; a continuant-only class does not headline"
      (is (str/includes? oc "OCCURRENT"))
      (is (str/includes? oc "geo:Imposition")))
    (testing "role classes and relation properties are foregrounded"
      (is (str/includes? oc "geo:SenderRole"))
      (is (str/includes? oc "geo:respondsTo")))))

(deftest parse-timeline-null-typings-and-thinking-preamble
  (let [json (str "{\"events\":["
                  "{\"id\":\"e1\",\"label\":\"US imposes controls\",\"excerpt\":\"export controls\",\"occurrence\":1,"
                  "\"node_id\":\"geo:Imposition\",\"participants\":[{\"entity\":\"US\",\"node_id\":\"geo:State\",\"role_id\":\"geo:SenderRole\"}],"
                  "\"when\":{\"kind\":\"instant\",\"start\":\"2025-04-04\",\"precision\":\"day\",\"narrative_index\":0},\"confidence\":0.9,\"rationale\":\"r\"},"
                  "{\"id\":\"e2\",\"label\":\"A novel manoeuvre\",\"excerpt\":\"novel manoeuvre\",\"occurrence\":1,"
                  "\"node_id\":null,\"nearest\":\"geo:Act\",\"why_no_fit\":\"no leaf class\",\"participants\":[{\"entity\":\"China\",\"role_id\":null}],"
                  "\"when\":{\"kind\":\"unknown\",\"start\":null,\"narrative_index\":1},\"confidence\":0.5,\"rationale\":\"r\"}],"
                  "\"relations\":[{\"source\":\"e1\",\"target\":\"e2\",\"type\":\"responds-to\",\"property_id\":null,\"confidence\":0.6,\"rationale\":\"r\"}]}")]
    (testing "parses clean JSON"
      (let [{:keys [events relations status]} (p/parse-timeline-response json)]
        (is (= :ok status))
        (is (= 2 (count events)))
        (is (= "geo:Imposition" (:node-id (first events))))
        (testing "a null node_id is preserved as a gap with nearest + why-no-fit"
          (is (nil? (:node-id (second events))))
          (is (= "geo:Act" (:nearest (second events))))
          (is (= "no leaf class" (:why-no-fit (second events)))))
        (testing "a null role_id participant is a gap"
          (is (nil? (:role-id (first (:participants (second events)))))))
        (testing "relation carries local endpoint ids + null property (a gap)"
          (is (= "e1" (:source (first relations))))
          (is (nil? (:property-id (first relations)))))))
    (testing "recovers JSON after a thinking preamble (non-streaming thinking models)"
      (let [{:keys [events status]} (p/parse-timeline-response (str "Let me think... first I identify events.\n\n" json))]
        (is (= :ok status))
        (is (= 2 (count events)))))
    (testing "blank -> :empty; bare prose -> :no-json"
      (is (= :empty (:status (p/parse-timeline-response ""))))
      (is (= :no-json (:status (p/parse-timeline-response "just some words")))))))

(deftest validate-timeline-flags-and-repairs
  (let [model (geo-model)
        text  "The export controls were imposed. Then a novel manoeuvre followed."
        {:keys [events relations]}
        (p/parse-timeline-response
         (str "{\"events\":["
              "{\"id\":\"e1\",\"label\":\"controls\",\"excerpt\":\"export controls\",\"node_id\":\"geo:Imposition\",\"when\":{\"narrative_index\":0}},"
              "{\"id\":\"e2\",\"label\":\"manoeuvre\",\"excerpt\":\"novel manoeuvre\",\"node_id\":\"geo:NotARealClass\",\"when\":{\"narrative_index\":1}}],"
              ;; one good relation, one DANGLING (target e9 doesn't exist)
              "\"relations\":[{\"source\":\"e1\",\"target\":\"e2\",\"type\":\"precedes\",\"property_id\":\"geo:respondsTo\"},"
              "{\"source\":\"e1\",\"target\":\"e9\",\"type\":\"causes\",\"property_id\":null}]}"))
        {:keys [events relations dropped] :as _v} (p/validate-timeline model text events relations)]
    (testing "invalid node-id flagged, valid one clean"
      (is (empty? (:flags (first events))))
      (is (contains? (:flags (second events)) :invalid-target)))
    (testing "the dangling relation is repaired away (dropped), the good one kept"
      (is (= 1 (count relations)))
      (is (= 1 dropped))
      (is (= "e2" (:target (first relations)))))))
