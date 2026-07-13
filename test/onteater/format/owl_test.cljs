(ns onteater.format.owl-test
  "Tests for the OWL 2 / Turtle adapter: structural extraction, semantic
  round-tripping (parse ∘ serialize ∘ parse is stable on nodes+edges), residual
  preservation of unmodelled axioms, format detection, and cross-format export
  (any model -> OWL Turtle) using the geo sample."
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [onteater.model.graph :as g]
            [onteater.format.geo :as geo]
            [onteater.format.owl :as owl]))

(def ^:private fs (js/require "fs"))
(defn- sample [] (.readFileSync fs "examples/solar-system.ttl" "utf8"))

(defn- targets-of [model id etype]
  (into #{} (comp (filter #(= etype (:type %))) (map :target)) (g/out-edges model id)))

(deftest parse-extracts-structure
  (let [model (owl/parse-str (sample))]
    (testing "ontology header becomes meta, not a node"
      (is (= "A tiny Solar System ontology" (get-in model [:meta :title])))
      (is (= "1.0" (get-in model [:meta :version])))
      (is (= :owl2-turtle (get-in model [:meta :format])))
      (is (nil? (g/node model "http://example.org/solar-system"))))

    (testing "classes, properties and individuals are typed correctly"
      (is (= :class    (:kind (g/node model "ss:CelestialBody"))))
      (is (= :class    (:kind (g/node model "ss:Planet"))))
      (is (= :property (:kind (g/node model "ss:orbits"))))
      (is (= :property (:kind (g/node model "ss:mass"))))
      (is (= :individual (:kind (g/node model "ss:Sun"))))
      (is (= :individual (:kind (g/node model "ss:Earth")))))

    (testing "labels and comments are captured"
      (is (= "Celestial body" (:label (g/node model "ss:CelestialBody"))))
      (is (str/starts-with? (:gloss (g/node model "ss:CelestialBody")) "Anything in space")))

    (testing "rdfs:subClassOf with an IRI object -> subclass edge"
      (is (contains? (targets-of model "ss:Planet" :subclass-of) "ss:CelestialBody")))

    (testing "rdfs:domain / rdfs:range -> edges"
      (is (contains? (targets-of model "ss:orbits" :domain) "ss:CelestialBody"))
      (is (contains? (targets-of model "ss:mass" :range) "xsd:double")))

    (testing "an individual's rdf:type to a user class -> instance-of edge"
      (is (contains? (targets-of model "ss:Sun" :instance-of) "ss:Star"))
      (is (contains? (targets-of model "ss:Earth" :instance-of) "ss:Planet")))

    (testing "an undeclared range target (xsd:double) becomes an external stub"
      (is (:external? (g/node model "xsd:double"))))))

(deftest roundtrip-is-semantically-stable
  (let [model1 (owl/parse-str (sample))
        ttl2   (owl/serialize-model model1)
        model2 (owl/parse-str ttl2)]
    (testing "serialised text is well-formed enough to re-parse"
      (is (string? ttl2))
      (is (pos? (g/node-count model2))))
    (testing "nodes survive the round-trip exactly"
      (is (= (:nodes model1) (:nodes model2))))
    (testing "edges survive the round-trip exactly"
      (is (= (:edges model1) (:edges model2))))
    (testing "the DatatypeProperty vs ObjectProperty distinction is preserved"
      (is (str/includes? ttl2 "owl:DatatypeProperty"))
      (is (str/includes? ttl2 "owl:ObjectProperty")))))

(deftest residual-preserves-unmodelled-axioms
  (let [model (owl/parse-str (sample))
        out   (owl/serialize-model model)]
    (testing "the anonymous owl:Restriction subclass axiom is not lost"
      (is (str/includes? out "owl:Restriction"))
      (is (str/includes? out "owl:someValuesFrom")))
    (testing "the ABox object-property assertion (Earth orbits Sun) is preserved"
      (is (str/includes? out "ss:orbits ss:Sun")))))

(deftest detection
  (testing "confident on OWL/Turtle, silent on JSON (so geo/native win their files)"
    (is (< 0.9 (owl/detect-str (sample))))
    (is (= 0.0 (owl/detect-str "{\"spine\": {}}")))
    (is (= 0.0 (owl/detect-str "{\"onteater/format-version\": 1}")))))

(deftest exports-any-model-to-owl
  (let [geo-model (geo/parse-str (.readFileSync fs "examples/galactic-economic-ontology.json" "utf8"))
        ttl       (owl/serialize-model geo-model)
        back      (owl/parse-str ttl)]
    (testing "a geo-origin model serialises to parseable OWL Turtle"
      (is (str/includes? ttl "owl:Class"))
      (is (pos? (g/node-count back))))
    (testing "class nodes and a known subclass axiom survive the export"
      (is (= :class (:kind (g/node back "geo:Agent"))))
      (is (contains? (targets-of back "geo:Agent" :subclass-of) "bfo:IndependentContinuant")))))
