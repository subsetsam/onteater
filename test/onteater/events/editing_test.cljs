(ns onteater.events.editing-test
  "Tests for the editing events and the shared history interceptor (undo/redo +
  dirty tracking). re-frame runs headlessly in :node-test, so we drive
  real events with dispatch-sync and read app-db directly."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [onteater.model.graph :as g]
            [onteater.events]))            ; registers all events + interceptor

(defn- fresh-model []
  (-> (g/empty-model {:title "T"})
      (g/add-node {:id "A" :label "A" :kind :class :gloss "old"})
      (g/add-node {:id "B" :label "B" :kind :class})
      (g/add-edge "B" :subclass-of "A")))

(use-fixtures :each
  {:before (fn []
             (rf/dispatch-sync [:app/initialize])
             (rf/dispatch-sync [:ontology/load (fresh-model) {:name "t.json" :format :onteater-native}]))})

(defn- model [] (get-in @rfdb/app-db [:ontology :model]))
(defn- dirty? [] (get-in @rfdb/app-db [:ontology :dirty?]))

(deftest load-is-clean
  (is (false? (dirty?)))
  (is (= "old" (:gloss (g/node (model) "A")))))

(deftest edit-marks-dirty-and-records-undo
  (rf/dispatch-sync [:ontology/update-node-field "A" :gloss "new"])
  (testing "edit applied + dirty flag set"
    (is (= "new" (:gloss (g/node (model) "A"))))
    (is (true? (dirty?))))
  (testing "undo restores the previous model"
    (rf/dispatch-sync [:ontology/undo])
    (is (= "old" (:gloss (g/node (model) "A")))))
  (testing "redo re-applies it"
    (rf/dispatch-sync [:ontology/redo])
    (is (= "new" (:gloss (g/node (model) "A"))))))

(deftest add-child-creates-node-and-edge
  (let [before (g/node-count (model))]
    (rf/dispatch-sync [:ontology/add-child "A"])
    (is (= (inc before) (g/node-count (model))))
    (let [new-id (get-in @rfdb/app-db [:ontology :selection :node])]
      (is (contains? (g/children (model) "A") new-id))
      (testing "a single undo removes both the node and its edge"
        (rf/dispatch-sync [:ontology/undo])
        (is (= before (g/node-count (model))))
        (is (not (g/exists? (model) new-id)))))))

(deftest delete-cascades-and-undoes
  (rf/dispatch-sync [:ontology/delete-node "A"])
  (testing "node gone and its edge cascaded"
    (is (not (g/exists? (model) "A")))
    (is (empty? (g/edges (model)))))
  (testing "undo brings back node and edge together"
    (rf/dispatch-sync [:ontology/undo])
    (is (g/exists? (model) "A"))
    (is (= 1 (count (g/edges (model)))))))

(deftest rename-rewrites-and-undoes
  (rf/dispatch-sync [:ontology/rename-node "A" "Z"])
  (is (g/exists? (model) "Z"))
  (is (contains? (g/parents (model) "B") "Z"))
  (rf/dispatch-sync [:ontology/undo])
  (is (g/exists? (model) "A"))
  (is (contains? (g/parents (model) "B") "A")))

(deftest redo-cleared-by-new-edit
  (rf/dispatch-sync [:ontology/update-node-field "A" :label "A1"])
  (rf/dispatch-sync [:ontology/undo])
  (is (seq (get-in @rfdb/app-db [:ontology :redo])))
  (rf/dispatch-sync [:ontology/update-node-field "B" :label "B1"])
  (testing "a fresh edit clears the redo stack"
    (is (empty? (get-in @rfdb/app-db [:ontology :redo])))))
