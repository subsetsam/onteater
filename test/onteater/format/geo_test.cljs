(ns onteater.format.geo-test
  "Golden-file tests for the geo adapter — the hard gate for the whole project.
  The headline assertion is that parse ∘ serialize on the
  untouched sample is byte-for-byte identical (a stronger guarantee than the
  required semantic equality). Edit/delete/add fixtures then confirm serialisation
  touches only what changed."
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [onteater.model.graph :as g]
            [onteater.format.core :as fmt]
            [onteater.format.geo :as geo]))

;; The suite runs under :node-test, so we can read the sample straight off disk
;; (read-only — we never mutate the file itself).
(def ^:private fs (js/require "fs"))
(defn- read-sample [] (.readFileSync fs "examples/galactic-economic-ontology.json" "utf8"))

(defn- json-data [s] (js->clj (js/JSON.parse s) :keywordize-keys false))

(deftest golden-roundtrip-is-byte-identical
  (let [raw   (read-sample)
        model (geo/parse-str raw)
        out   (geo/serialize-model model)]
    (testing "untouched round-trip returns the source verbatim"
      (is (= raw out)
          "serialize of an unedited parse must equal the original file text"))
    (testing "and is therefore trivially semantically identical"
      (is (= (json-data raw) (json-data out))))))

(deftest parse-extracts-expected-structure
  (let [model (geo/parse-str (read-sample))
        real  (remove :external? (g/nodes model))]
    (testing "270 occurrences merge to 261 unique real nodes"
      (is (= 261 (count real))))
    (testing "kinds are classified (205 classes + 56 properties after merging 9 duplicate ids)"
      (let [by-kind (frequencies (map :kind real))]
        (is (= 205 (:class by-kind)))
        (is (= 56 (:property by-kind)))))
    (testing "duplicate ids merge into one node with multiple provenance"
      (let [dep (g/node model "geo:Dependence")]
        (is (= 2 (count (:provenance dep))))
        (is (= "Reified n-ary state: dependent, provider, channel, horizon, provenance (Decision 7)." (:gloss dep))
            "the richer (spine) gloss wins the merge")))
    (testing "external subClassOf targets become dashed stub nodes"
      (let [bfo (g/node model "bfo:IndependentContinuant")]
        (is (some? bfo))
        (is (:external? bfo))))
    (testing "subclass edges exist and point the right way"
      (is (contains? (g/parents model "geo:Agent") "bfo:IndependentContinuant")))))

(deftest edit-gloss-touches-only-that-node
  (let [raw   (read-sample)
        model (geo/parse-str raw)
        edited (g/set-node-attr model "geo:Leverage" :gloss "EDITED GLOSS")
        out    (geo/serialize-model edited)
        before (json-data raw)
        after  (json-data out)]
    (testing "output is still valid JSON and parses"
      (is (map? after)))
    (testing "the edited gloss appears where geo:Leverage is authoritatively defined"
      (is (str/includes? out "EDITED GLOSS")))
    (testing "no other class's fields changed"
      ;; Compare the spine.classes arrays element-by-element except the edited node.
      (let [before-spine (get-in before ["spine" "classes"])
            after-spine  (get-in after ["spine" "classes"])]
        (is (= (count before-spine) (count after-spine)))
        (doseq [[b a] (map vector before-spine after-spine)]
          (if (= (get b "id") "geo:Leverage")
            (is (= "EDITED GLOSS" (get a "gloss")))
            (is (= b a) (str "unrelated node changed: " (get b "id")))))))))

(deftest delete-node-removes-all-occurrences
  (let [raw   (read-sample)
        model (geo/parse-str raw)
        ;; geo:Leverage appears at spine.classes and modules.POW.subsections.6.2.classes[0]
        pruned (g/remove-node model "geo:Leverage")
        out    (geo/serialize-model pruned)
        after  (json-data out)]
    (testing "the node is gone from every provenance path"
      (is (map? after))
      (let [all-ids (->> (tree-seq coll? seq after)
                         (filter map?)
                         (keep #(get % "id"))
                         set)]
        (is (not (contains? all-ids "geo:Leverage")))))
    (testing "a sibling in the same array survives"
      (let [spine-ids (map #(get % "id") (get-in after ["spine" "classes"]))]
        (is (some #{"geo:Dependence"} spine-ids))))))

(deftest add-node-appends-cleanly
  (let [raw   (read-sample)
        model (geo/parse-str raw)
        added (g/add-node model {:id "geo:TestWidget" :label "Test widget"
                                 :kind :class :gloss "A synthetic node." :props {}
                                 :module "spine" :external? false :provenance []})
        added (g/add-edge added "geo:TestWidget" :subclass-of "geo:Entity")
        out   (geo/serialize-model added)
        after (json-data out)]
    (testing "new node lands in spine.classes with its subclass link"
      (let [match (->> (get-in after ["spine" "classes"])
                       (filter #(= (get % "id") "geo:TestWidget"))
                       first)]
        (is (some? match))
        (is (= "geo:Entity" (get match "subClassOf")))
        (is (= "A synthetic node." (get match "gloss")))))))

(deftest edit-propagates-to-all-provenance-paths
  ;; geo:Dependence lives at spine.classes[17] AND modules.POW.subsections.6.1.classes[0].
  ;; Editing its label must update BOTH occurrences, while each keeps its own
  ;; subClassOf (spine has one, POW does not).
  (let [raw    (read-sample)
        model  (geo/parse-str raw)
        edited (g/set-node-attr model "geo:Dependence" :label "Reliance")
        after  (json-data (geo/serialize-model edited))
        occs   (->> (tree-seq coll? seq after)
                    (filter map?)
                    (filter #(= (get % "id") "geo:Dependence")))]
    (is (= 2 (count occs)))
    (is (every? #(= "Reliance" (get % "label")) occs))
    (testing "per-path subClassOf asymmetry is preserved"
      (is (= #{"geo:DeonticEntity" nil}
             (set (map #(get % "subClassOf") occs)))))))

(deftest edit-and-delete-in-same-array-coexist
  ;; Edit one spine class and delete another; both live in spine.classes. The
  ;; patch (by original index) must survive the sibling's array splice.
  (let [raw    (read-sample)
        model  (geo/parse-str raw)
        m2     (-> model
                   (g/set-node-attr "geo:Agent" :gloss "EDITED AGENT")
                   (g/remove-node "geo:MaterialEntity"))
        after  (json-data (geo/serialize-model m2))
        spine  (get-in after ["spine" "classes"])
        ids    (map #(get % "id") spine)]
    (is (some #{"geo:Agent"} ids))
    (is (not (some #{"geo:MaterialEntity"} ids)))
    (is (= "EDITED AGENT"
           (->> spine (filter #(= (get % "id") "geo:Agent")) first (#(get % "gloss")))))))

(deftest detection-is-confident
  (is (>= (geo/detect-str (read-sample)) 0.9))
  (is (= 0.0 (geo/detect-str "not json at all")))
  (is (< (geo/detect-str "{\"random\":true}") 0.5)))

(deftest registered-in-registry
  (is (some? (fmt/adapter :geo-reference-json))))
