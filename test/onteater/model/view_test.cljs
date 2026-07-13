(ns onteater.model.view-test
  "Tests for the anti-clutter view model. Uses the real geo sample so
  the bounded-visibility guarantees are exercised against genuine scale."
  (:require [cljs.test :refer [deftest is testing]]
            [onteater.model.graph :as g]
            [onteater.format.geo :as geo]
            [onteater.model.view :as view]
            [onteater.db :as db]))

(def ^:private fs (js/require "fs"))
(defn- model [] (geo/parse-str (.readFileSync fs "examples/galactic-economic-ontology.json" "utf8")))

(deftest overview-shows-a-handful-of-bubbles
  (let [vg (view/visible-graph (model) (assoc db/default-view-spec :mode :overview))]
    (testing "top-level sections become a small number of meta-node bubbles"
      (is (every? :meta? (:nodes vg)))
      (is (<= (count (:nodes vg)) 15))   ; ~12 sections, never hundreds
      (is (pos? (count (:nodes vg)))))
    (testing "each bubble carries a member count"
      (is (every? #(nat-int? (:count %)) (:nodes vg))))))

(deftest neighborhood-is-bounded-and-focused
  (let [m  (model)
        vs (view/focus-node db/default-view-spec "geo:Leverage" 2)
        vg (view/visible-graph m vs)
        ids (set (map :id (:nodes vg)))]
    (testing "focus node is present"
      (is (contains? ids "geo:Leverage")))
    (testing "the neighbourhood is small, not the whole graph"
      (is (< (count ids) 60)))
    (testing "edges are induced only among visible nodes"
      (is (every? #(and (ids (:source %)) (ids (:target %))) (:edges vg))))))

(deftest subtree-mode-follows-subclass
  (let [m  (model)
        vs (assoc db/default-view-spec :mode :subtree :focus #{"geo:Disposition"})
        vg (view/visible-graph m vs)
        ids (set (map :id (:nodes vg)))]
    (is (contains? ids "geo:Disposition"))
    (testing "known subclasses of Disposition appear"
      (is (contains? ids "geo:Leverage")))))   ; geo:Leverage subClassOf geo:Disposition

(deftest kind-filter-hides-properties
  (let [m  (model)
        vs (assoc db/default-view-spec :mode :module :focus #{"relations"}
                  :kinds #{:class})           ; hide properties
        vg (view/visible-graph m vs)]
    (testing "with only :class allowed, the properties module renders nothing"
      (is (empty? (:nodes vg))))))

(deftest collapse-hides-descendants
  (let [m  (model)
        base (assoc db/default-view-spec :mode :subtree :focus #{"geo:Disposition"})
        open (view/visible-graph m base)
        shut (view/visible-graph m (update base :collapsed conj "geo:Disposition"))]
    (testing "collapsing the root hides its descendants but keeps the root"
      (is (contains? (set (map :id (:nodes shut))) "geo:Disposition"))
      (is (< (count (:nodes shut)) (count (:nodes open)))))
    (testing "the collapsed root reports a hidden-count badge"
      (let [root (first (filter #(= "geo:Disposition" (:id %)) (:nodes shut)))]
        (is (:collapsed? root))
        (is (pos? (:hidden-count root)))))))

(deftest empty-model-is-safe
  (is (= [] (:nodes (view/visible-graph nil db/default-view-spec))))
  (is (= [] (:nodes (view/visible-graph (g/empty-model) db/default-view-spec)))))
