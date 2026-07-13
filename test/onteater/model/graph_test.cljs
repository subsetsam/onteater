(ns onteater.model.graph-test
  "Unit tests for the pure canonical-model operations."
  (:require [cljs.test :refer [deftest is testing]]
            [onteater.model.graph :as g]))

(defn- sample-model
  "A tiny hand-built ontology:  Animal <- Dog <- Puppy ,  Animal <- Cat ,
  plus an orphan Rock and a :domain edge Dog--eats-->Food."
  []
  (-> (g/empty-model {:title "Test"})
      (g/add-node {:id "Animal" :label "Animal" :kind :class})
      (g/add-node {:id "Dog"    :label "Dog"    :kind :class})
      (g/add-node {:id "Puppy"  :label "Puppy"  :kind :class})
      (g/add-node {:id "Cat"    :label "Cat"    :kind :class})
      (g/add-node {:id "Food"   :label "Food"   :kind :class})
      (g/add-node {:id "Rock"   :label "Rock"   :kind :class :gloss "an orphan"})
      (g/add-edge "Dog"   :subclass-of "Animal")
      (g/add-edge "Puppy" :subclass-of "Dog")
      (g/add-edge "Cat"   :subclass-of "Animal")
      (g/add-edge "Dog"   :eats "Food")))

(deftest crud-nodes
  (let [m (sample-model)]
    (is (= 6 (g/node-count m)))
    (testing "update-node"
      (let [m2 (g/set-node-attr m "Dog" :gloss "woof")]
        (is (= "woof" (:gloss (g/node m2 "Dog"))))))
    (testing "remove-node cascades touching edges"
      (let [preview (g/remove-node-preview m "Dog")]
        (is (= 3 (:edge-count preview))))   ; Dog<-Puppy, Dog->Animal, Dog->Food
      (let [m2 (g/remove-node m "Dog")]
        (is (not (g/exists? m2 "Dog")))
        (is (= 1 (count (g/edges m2))))      ; only Cat->Animal survives
        (is (empty? (filter #(or (= "Dog" (:source %)) (= "Dog" (:target %)))
                            (g/edges m2))))))))

(deftest rename-rewrites-all-references
  (let [m  (sample-model)
        m2 (g/rename-id m "Dog" "Canine")]
    (is (not (g/exists? m2 "Dog")))
    (is (g/exists? m2 "Canine"))
    (is (contains? (g/parents m2 "Puppy") "Canine"))
    (is (contains? (g/children m2 "Animal") "Canine"))
    (testing "no dangling references remain"
      (is (empty? (:refs (first (g/validate m2))))))))

(deftest queries
  (let [m (sample-model)]
    (testing "parents / children"
      (is (= #{"Animal"} (g/parents m "Dog")))
      (is (= #{"Dog" "Cat"} (g/children m "Animal"))))
    (testing "subtree (transitive descendants, inclusive)"
      (is (= #{"Animal" "Dog" "Cat" "Puppy"} (g/subtree m "Animal"))))
    (testing "ancestors (transitive, inclusive)"
      (is (= #{"Puppy" "Dog" "Animal"} (g/ancestors m "Puppy"))))
    (testing "roots excludes subclassed nodes"
      (is (contains? (g/roots m) "Animal"))
      (is (not (contains? (g/roots m) "Dog"))))
    (testing "orphans"
      (is (= #{"Rock"} (g/orphans m))))
    (testing "neighbors filtered by edge type"
      (is (= #{"Animal" "Puppy" "Food"} (g/neighbors m "Dog")))
      (is (= #{"Animal" "Puppy"} (g/neighbors m "Dog" #{:subclass-of}))))
    (testing "neighborhood 1-hop over subclass edges only"
      (let [nb (g/neighborhood m #{"Dog"} 1 #{:subclass-of})]
        (is (= #{"Dog" "Animal" "Puppy"} (:nodes nb)))))
    (testing "neighborhood 2-hop reaches grandparents/children"
      (let [nb (g/neighborhood m #{"Puppy"} 2 #{:subclass-of})]
        (is (contains? (:nodes nb) "Animal"))))))

(deftest search-ranks-sensibly
  (let [m (sample-model)]
    (is (= ["Animal"] (g/search m "anim")))
    (is (contains? (set (g/search m "o")) "Rock"))   ; gloss "an orphan" / labels
    (is (empty? (g/search m "")))))

(deftest validate-flags-problems
  (testing "clean model has no warnings"
    (is (empty? (g/validate (sample-model)))))
  (testing "dangling edge is reported"
    ;; Normal remove-node cascades edges, so we build a dangling edge directly:
    ;; an edge whose target node is not present in the model.
    (let [m (g/add-edge (sample-model) "Dog" :subclass-of "Ghost")]
      (is (seq (filter #(= :dangling-edge (:kind %)) (g/validate m))))))
  (testing "subclass cycle is reported"
    (let [m (g/add-edge (sample-model) "Animal" :subclass-of "Puppy")] ; Animal->Puppy->Dog->Animal
      (is (seq (filter #(= :subclass-cycle (:kind %)) (g/validate m)))))))

(deftest edge-ids-are-stable
  (is (= (g/edge-id "a" :t "b") (g/edge-id "a" :t "b")))
  (is (not= (g/edge-id "a" :t "b") (g/edge-id "b" :t "a"))))

(deftest reveal-group-ids-walks-ancestor-path
  ;; A node sits in both its module and its (nested) subgroup; revealing it must
  ;; expand the whole path so the outline tree can surface it.
  (let [m (-> (g/empty-model)
              (g/add-node {:id "X" :label "X" :kind :class})
              (assoc :groups {"mod" {:id "mod" :parent nil :subgroups #{"sub"} :members ["X" "Y"]}
                              "sub" {:id "sub" :parent "mod" :subgroups #{} :members ["X"]}}))]
    (testing "reveal collects every group containing the node plus their ancestors"
      (is (= #{"mod" "sub"} (g/reveal-group-ids m "X"))))
    (testing "a node only in the module reveals just the module"
      (is (= #{"mod"} (g/reveal-group-ids m "Y"))))
    (testing "group-ancestors climbs the :parent chain, inclusive"
      (is (= #{"sub" "mod"} (g/group-ancestors m "sub")))
      (is (= #{"mod"} (g/group-ancestors m "mod"))))
    (testing "an absent node reveals nothing"
      (is (= #{} (g/reveal-group-ids m "absent"))))))
