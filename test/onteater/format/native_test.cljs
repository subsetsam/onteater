(ns onteater.format.native-test
  "Round-trip tests for the native lossless format. Confirms that
  the keyword/set encoding survives JSON so parse ∘ serialize is the identity on
  the canonical model, including a full model derived from the geo sample."
  (:require [cljs.test :refer [deftest is testing]]
            [onteater.model.graph :as g]
            [onteater.format.geo :as geo]
            [onteater.format.native :as native]))

(def ^:private fs (js/require "fs"))

(defn- rich-model []
  (-> (g/empty-model {:title "Native RT" :namespaces {"ex" "http://example/"}})
      (g/add-node {:id "A" :label "A" :kind :class :gloss "root"
                   :props {"weight" 3 "tags" ["x" "y"]} :module "m1"
                   :external? false :provenance [["spine" "classes" 0]]})
      (g/add-node {:id "B" :label "B" :kind :property :gloss nil
                   :props {} :module "m1" :external? false :provenance []})
      (g/add-edge "A" :subclass-of "B")
      (assoc-in [:groups "m1"] {:id "m1" :label "Module 1" :kind :section
                                :parent nil :subgroups #{"m1/x"} :members ["A" "B"]})))

(deftest native-roundtrip-identity
  (let [m   (rich-model)
        out (native/serialize-model m)
        m2  (native/parse-str out)]
    (testing "nodes survive exactly (keywords, nested props, nil gloss)"
      (is (= (:nodes m) (:nodes m2))))
    (testing "edges survive with keyword :type"
      (is (= (:edges m) (:edges m2))))
    (testing "groups survive, including the :subgroups set"
      (is (= (:groups m) (:groups m2)))
      (is (set? (get-in m2 [:groups "m1" :subgroups]))))
    (testing "meta survives"
      (is (= (:meta m) (:meta m2))))))

(deftest native-roundtrips-geo-model
  (let [raw   (.readFileSync fs "examples/galactic-economic-ontology.json" "utf8")
        model (geo/parse-str raw)
        out   (native/serialize-model model)
        back  (native/parse-str out)]
    (testing "the full geo-derived model survives a native round-trip"
      (is (= (:nodes model) (:nodes back)))
      (is (= (:edges model) (:edges back)))
      (is (= (:groups model) (:groups back)))
      (is (= (:residual model) (:residual back))))
    (testing "docs sections and the parse-time docs-index survive"
      (is (seq (:docs model)))
      (is (= (:docs model) (:docs back)))
      (is (= (:meta model) (:meta back))))))

(deftest native-without-docs-still-parses
  (let [m   (rich-model)
        out (native/serialize-model m)]
    (testing "a docs-less model emits no docs key and parses without one"
      (is (not (contains? (js->clj (js/JSON.parse out)) "docs")))
      (is (not (contains? (native/parse-str out) :docs))))))

(deftest native-detection
  (is (= 0.99 (native/detect-str (native/serialize-model (rich-model)))))
  (is (= 0.0 (native/detect-str "{\"other\":1}"))))
