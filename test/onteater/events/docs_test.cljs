(ns onteater.events.docs-test
  "Tests for the documentation-section events: generic tree edits apply through
  the history interceptor (dirty + undo), whole sections add/remove with
  guard rails, and metadata edits re-sync the model's :meta."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [onteater.model.graph :as g]
            [onteater.model.docs :as docs]
            [onteater.events]))            ; registers all events + interceptor

(defn- fresh-model []
  (-> (g/empty-model {:title "T"})
      (g/add-node {:id "geo:A" :label "A" :kind :class
                   :provenance [["spine" "classes" 0]]})
      (assoc :docs [{:path ["metadata"]
                     :value (docs/js->tree #js {:title "T" :version "1.0"})}
                    {:path ["design_decisions"]
                     :value (docs/js->tree
                             (js/JSON.parse
                              "{\"decisions\":[{\"id\":\"DD-1\",\"title\":\"one\"}]}"))}])))

(use-fixtures :each
  {:before (fn []
             (rf/dispatch-sync [:app/initialize])
             (rf/dispatch-sync [:ontology/load (fresh-model)
                                {:name "t.json" :format :onteater-native}]))})

(defn- model [] (get-in @rfdb/app-db [:ontology :model]))
(defn- dirty? [] (get-in @rfdb/app-db [:ontology :dirty?]))
(defn- section [path]
  (some #(when (= path (:path %)) (:value %)) (:docs (model))))

(deftest set-value-applies-dirty-and-undoes
  (rf/dispatch-sync [:docs/set-value ["design_decisions"] ["decisions" 0 "title"] "EDITED"])
  (testing "edit applied + dirty flag set"
    (is (= "EDITED" (:v (docs/get-at (section ["design_decisions"])
                                     ["decisions" 0 "title"]))))
    (is (true? (dirty?))))
  (testing "undo restores the previous tree"
    (rf/dispatch-sync [:ontology/undo])
    (is (= "one" (:v (docs/get-at (section ["design_decisions"])
                                  ["decisions" 0 "title"]))))))

(deftest add-and-remove-items-and-keys
  (rf/dispatch-sync [:docs/add-item ["design_decisions"] ["decisions"]])
  (is (= 2 (count (:items (docs/get-at (section ["design_decisions"]) ["decisions"])))))
  (rf/dispatch-sync [:docs/remove-at ["design_decisions"] ["decisions" 1]])
  (is (= 1 (count (:items (docs/get-at (section ["design_decisions"]) ["decisions"])))))
  (testing "add-key auto-uniquifies, rename-key refuses collisions"
    (rf/dispatch-sync [:docs/add-key ["metadata"] []])
    (rf/dispatch-sync [:docs/add-key ["metadata"] []])
    (is (= ["title" "version" "new_key" "new_key2"]
           (map first (:entries (section ["metadata"])))))
    (rf/dispatch-sync [:docs/rename-key ["metadata"] [] "new_key2" "title"])
    (is (= ["title" "version" "new_key" "new_key2"]
           (map first (:entries (section ["metadata"])))))))

(deftest metadata-edits-resync-meta
  (rf/dispatch-sync [:docs/set-value ["metadata"] ["title"] "Renamed ontology"])
  (is (= "Renamed ontology" (get-in (model) [:meta :title])))
  (rf/dispatch-sync [:docs/set-value ["metadata"] ["version"] "2.0"])
  (is (= "2.0" (get-in (model) [:meta :version]))))

(deftest add-and-remove-sections-with-guard-rails
  (rf/dispatch-sync [:docs/add-section "curation_notes" :map])
  (is (some? (section ["curation_notes"])))
  (testing "existing, node-bearing, and structural keys are refused"
    (let [before (:docs (model))]
      (rf/dispatch-sync [:docs/add-section "metadata" :map])
      (rf/dispatch-sync [:docs/add-section "spine" :map])
      (rf/dispatch-sync [:docs/add-section "namespaces" :map])
      (rf/dispatch-sync [:docs/add-section "  " :map])
      (is (= before (:docs (model))))))
  (testing "remove-section drops it; undo restores it"
    (rf/dispatch-sync [:docs/remove-section ["curation_notes"]])
    (is (nil? (section ["curation_notes"])))
    (rf/dispatch-sync [:ontology/undo])
    (is (some? (section ["curation_notes"])))))
