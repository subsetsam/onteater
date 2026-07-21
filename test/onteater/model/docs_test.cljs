(ns onteater.model.docs-test
  "Unit tests for the order-preserving docs tree: JS round-trip fidelity
  (including key order beyond CLJS's 8-entry hash-map threshold) and every
  mutation operation the generic structured editor dispatches."
  (:require [cljs.test :refer [deftest is testing]]
            [onteater.model.docs :as docs]))

(def ^:private sample-json
  "{\"a\":\"one\",\"b\":2,\"c\":true,\"d\":null,\"e\":[1,\"two\"],\"f\":{\"x\":\"y\"},\"g\":\"\",\"h\":0,\"i\":false,\"j\":\"ten\",\"k\":\"eleven\"}")

(defn- parse-tree [s] (docs/js->tree (js/JSON.parse s)))
(defn- emit [tree] (js/JSON.stringify (docs/tree->js tree)))

(deftest js-roundtrip-preserves-order-and-types
  (let [tree (parse-tree sample-json)]
    (testing "round-trip is the identity, key order included (11 keys > the
              8-entry CLJS hash-map threshold)"
      (is (= sample-json (emit tree))))
    (testing "scalars keep their JSON types"
      (is (= 2     (:v (docs/get-at tree ["b"]))))
      (is (= true  (:v (docs/get-at tree ["c"]))))
      (is (nil?    (:v (docs/get-at tree ["d"]))))
      (is (= "two" (:v (docs/get-at tree ["e" 1])))))))

(deftest get-at-and-set-at
  (let [tree (parse-tree "{\"m\":{\"list\":[{\"name\":\"n0\"}]}}")]
    (is (= "n0" (:v (docs/get-at tree ["m" "list" 0 "name"]))))
    (is (nil? (docs/get-at tree ["m" "nope"])))
    (is (nil? (docs/get-at tree ["m" "list" 5])))
    (testing "set-at replaces just the addressed scalar"
      (is (= "{\"m\":{\"list\":[{\"name\":\"edited\"}]}}"
             (emit (docs/set-at tree ["m" "list" 0 "name"] "edited")))))
    (testing "set-at on an unresolvable path is a no-op"
      (is (= tree (docs/set-at tree ["m" "nope" 3] "x"))))))

(deftest vec-add-clones-last-sibling-blanked
  (let [tree (parse-tree "{\"xs\":[{\"id\":\"A\",\"n\":3,\"ok\":true,\"tags\":[\"t\"]}]}")
        out  (docs/vec-add tree ["xs"])]
    (testing "new item keeps the sibling's shape with blanked leaves and empty
              nested vectors"
      (is (= {:t :map :entries [["id"   {:t :val :v ""}]
                                ["n"    {:t :val :v 0}]
                                ["ok"   {:t :val :v false}]
                                ["tags" {:t :vec :items []}]]}
             (docs/get-at out ["xs" 1]))))
    (testing "an empty list grows an empty string scalar"
      (is (= "{\"xs\":[\"\"]}"
             (emit (docs/vec-add (parse-tree "{\"xs\":[]}") ["xs"])))))))

(deftest remove-at-vec-and-map
  (let [tree (parse-tree "{\"a\":[10,20,30],\"b\":{\"x\":1,\"y\":2}}")]
    (is (= "{\"a\":[10,30],\"b\":{\"x\":1,\"y\":2}}"
           (emit (docs/remove-at tree ["a" 1]))))
    (is (= "{\"a\":[10,20,30],\"b\":{\"y\":2}}"
           (emit (docs/remove-at tree ["b" "x"]))))
    (testing "removing a top-level entry works via a one-segment path"
      (is (= "{\"b\":{\"x\":1,\"y\":2}}"
             (emit (docs/remove-at tree ["a"])))))))

(deftest map-add-and-rename-key
  (let [tree (parse-tree "{\"m\":{\"x\":1}}")]
    (is (= "{\"m\":{\"x\":1,\"new_key\":\"\"}}"
           (emit (docs/map-add-key tree ["m"] "new_key"))))
    (testing "blank or colliding keys are refused"
      (is (= tree (docs/map-add-key tree ["m"] "")))
      (is (= tree (docs/map-add-key tree ["m"] "x"))))
    (testing "rename keeps the entry's position"
      (is (= "{\"m\":{\"a\":1,\"renamed\":2,\"c\":3}}"
             (emit (docs/rename-key (parse-tree "{\"m\":{\"a\":1,\"b\":2,\"c\":3}}")
                                    ["m"] "b" "renamed")))))
    (testing "rename refuses blanks and collisions"
      (is (= tree (docs/rename-key tree ["m"] "x" "")))
      (let [two (parse-tree "{\"m\":{\"x\":1,\"y\":2}}")]
        (is (= two (docs/rename-key two ["m"] "x" "y")))))))

(deftest set-kind-only-retypes-empty-nodes
  (let [tree (parse-tree "{\"empty\":\"\",\"full\":\"text\"}")]
    (is (= "{\"empty\":[],\"full\":\"text\"}"
           (emit (docs/set-kind tree ["empty"] :vec))))
    (is (= "{\"empty\":{},\"full\":\"text\"}"
           (emit (docs/set-kind tree ["empty"] :map))))
    (testing "a non-empty node is never converted"
      (is (= tree (docs/set-kind tree ["full"] :vec))))
    (testing "empty collections can be converted back"
      (is (= "{\"e\":\"\"}"
             (emit (docs/set-kind (parse-tree "{\"e\":[]}") ["e"] :val)))))))
