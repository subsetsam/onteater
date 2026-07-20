(ns onteater.model.timeline-test
  "Tests for the pure temporal-mapping core: cycle-tolerant toposort, dependency
  cones on a fork/join DAG, the ordering cascade with mixed date precision, lane
  assignment, entity analyses, and the gap report — all headless, no LLM."
  (:require [cljs.test :refer [deftest is testing]]
            [onteater.model.graph :as g]
            [onteater.model.timeline :as tl]))

;; --- fixtures ---------------------------------------------------------------

(defn- ev [id label & [m]]
  (tl/new-event (merge {:id id :label label
                        :when {:narrative-index (get {"a" 0 "b" 1 "c" 2 "d" 3 "e" 4} id 0)}}
                       m)))

(defn- rel [src tgt type & [m]]
  (tl/new-relation (merge {:id (str src "->" tgt) :source src :target tgt :type type} m)))

;;   fork/join DAG:  a ─► b ─► d
;;                    └─► c ─┘
(defn- fork-join-timeline []
  {:events    [(ev "a" "Alpha") (ev "b" "Bravo") (ev "c" "Charlie") (ev "d" "Delta")]
   :relations [(rel "a" "b" :causes) (rel "a" "c" :causes)
               (rel "b" "d" :enables) (rel "c" "d" :enables)]})

;; A tiny ontology with a discoverable occurrent spine, one property, one role.
(defn- fixture-model []
  (-> (g/empty-model {:title "Fixture"})
      (g/add-node {:id "bfo:Process" :label "Process" :kind :class :external? true})
      (g/add-node {:id "ex:Act" :label "Act" :kind :class})
      (g/add-node {:id "ex:Imposition" :label "Imposition" :kind :class})
      (g/add-node {:id "ex:Role" :label "Role" :kind :class})
      (g/add-node {:id "ex:SenderRole" :label "SenderRole" :kind :class})
      (g/add-node {:id "ex:respondsTo" :label "responds to" :kind :property})
      (g/add-edge "ex:Act" :subclass-of "bfo:Process")
      (g/add-edge "ex:Imposition" :subclass-of "ex:Act")
      (g/add-edge "ex:SenderRole" :subclass-of "ex:Role")))

;; --- discovery --------------------------------------------------------------

(deftest discovers-occurrents-roles-properties
  (let [m (fixture-model)]
    (testing "occurrent classes = transitive subclasses of the process root"
      (let [occ (tl/occurrent-ids m)]
        (is (contains? occ "ex:Act"))
        (is (contains? occ "ex:Imposition"))
        (is (not (contains? occ "ex:Role")))))
    (testing "roles = descendants of a Role-named root"
      (is (contains? (tl/role-ids m) "ex:SenderRole")))
    (testing "relation properties are the :property nodes"
      (is (= #{"ex:respondsTo"} (tl/relation-property-ids m))))))

;; --- toposort ---------------------------------------------------------------

(deftest toposort-respects-fork-and-join
  (let [{:keys [order cycles]} (tl/toposort (fork-join-timeline))
        ix (into {} (map-indexed (fn [i id] [id i]) order))]
    (is (empty? cycles))
    (is (= 4 (count order)))
    (testing "a precedes b, c; b and c precede d"
      (is (< (ix "a") (ix "b")))
      (is (< (ix "a") (ix "c")))
      (is (< (ix "b") (ix "d")))
      (is (< (ix "c") (ix "d"))))))

(deftest toposort-tolerates-cycles
  (let [cyclic {:events [(ev "a" "A") (ev "b" "B") (ev "c" "C")]
                :relations [(rel "a" "b" :causes) (rel "b" "c" :causes) (rel "c" "a" :causes)]}
        {:keys [order cycles]} (tl/toposort cyclic)]
    (testing "does not crash; every event still ordered exactly once"
      (is (= 3 (count order)))
      (is (= #{"a" "b" "c"} (set order))))
    (testing "the cyclic nodes are flagged"
      (is (= #{"a" "b" "c"} cycles)))))

(deftest part-of-is-not-a-dependency-edge
  (let [t {:events [(ev "a" "A") (ev "b" "B")]
           :relations [(rel "a" "b" :part-of)]}]
    (is (empty? (tl/dependency-relations t)))
    (is (empty? (tl/ancestors t "b")))))

;; --- cones + paths ----------------------------------------------------------

(deftest dependency-cone-on-fork-join
  (let [t (fork-join-timeline)]
    (testing "d depends (transitively) on a, b, c"
      (is (= #{"a" "b" "c"} (tl/ancestors t "d"))))
    (testing "a's downstream cone is b, c, d"
      (is (= #{"b" "c" "d"} (tl/descendants t "a"))))
    (testing "cone map"
      (is (= {:ancestors #{"a" "b" "c"} :descendants #{}} (tl/dependency-cone t "d"))))))

(deftest paths-between-enumerates-both-branches
  (let [paths (tl/paths-between (fork-join-timeline) "a" "d")]
    (is (= 2 (count paths)))
    (is (= #{["a" "b" "d"] ["a" "c" "d"]} (set paths)))))

;; --- ordering cascade -------------------------------------------------------

(deftest ordering-cascade-mixed-precision
  (let [t {:events [(ev "a" "A" {:when {:kind :instant :start "2025-01-15" :precision :day :narrative-index 0}})
                    (ev "b" "B" {:when {:kind :unknown :start nil :narrative-index 1}})   ; undated, between a and c
                    (ev "c" "C" {:when {:kind :instant :start "2025-03" :precision :month :narrative-index 2}})]
           :relations [(rel "a" "b" :precedes) (rel "b" "c" :precedes)]}
        ord (tl/ordering t)
        ix  (into {} (map (juxt :id :index)) ord)
        dated (into {} (map (juxt :id :dated?)) ord)]
    (testing "dated events sit in date order, undated slots between its neighbours"
      (is (= ["a" "b" "c"] (mapv :id ord)))
      (is (< (ix "a") (ix "b") (ix "c"))))
    (testing "metric vs ordinal is marked honestly"
      (is (true? (dated "a")))
      (is (false? (dated "b")))
      (is (true? (dated "c"))))))

(deftest ordering-falls-back-to-narrative-when-undated
  (let [t {:events [(ev "a" "A" {:when {:narrative-index 2}})
                    (ev "b" "B" {:when {:narrative-index 0}})
                    (ev "c" "C" {:when {:narrative-index 1}})]
           :relations []}
        ord (mapv :id (tl/ordering t))]
    ;; No dates, no relations -> narrative index breaks the tie in toposort.
    (is (= ["b" "c" "a"] ord))))

;; --- lanes + entities -------------------------------------------------------

(deftest lanes-by-entity
  (let [t {:events [(ev "a" "A" {:participants [{:entity "US"}]})
                    (ev "b" "B" {:participants [{:entity "China"}]})
                    (ev "c" "C" {:participants [{:entity "US"}]})]
           :relations [(rel "a" "b" :causes) (rel "b" "c" :causes)]}
        {:keys [lanes event->lane]} (tl/assign-lanes t {:grouping :entity})]
    (is (= 2 (count lanes)))
    (is (= "US" (event->lane "a")))
    (is (= "China" (event->lane "b")))))

(deftest entity-dependency-matrix-counts-cross-entity-edges
  (let [t {:events [(ev "a" "A" {:participants [{:entity "US"}]})
                    (ev "b" "B" {:participants [{:entity "China"}]})]
           :relations [(rel "a" "b" :causes)]}
        {:keys [entities cells]} (tl/entity-dependency-matrix t)]
    (is (= ["China" "US"] entities))
    (is (= 1 (get cells ["US" "China"])))))

;; --- gap report -------------------------------------------------------------

(deftest gap-report-finds-known-holes
  (let [m (fixture-model)
        t {:events [;; typed OK to a leaf occurrent
                    (ev "a" "Imposes controls" {:node-id "ex:Imposition"
                                                :participants [{:entity "US" :node-id "ex:State" :role-id "ex:SenderRole"}]})
                    ;; untyped -> a gap, carries nearest + why
                    (ev "b" "Weird new act" {:node-id nil :nearest "ex:Act"
                                             :why-no-fit "no leaf class for sanctions-listing"
                                             :participants [{:entity "China" :node-id "ex:State" :role-id nil}]})
                    ;; shallow: typed to ex:Act which still has a leaf (ex:Imposition) below it
                    (ev "c" "Some act" {:node-id "ex:Act"})]
           :relations [(rel "a" "b" :responds-to {:property-id "ex:respondsTo"})
                       (rel "b" "c" :causes {:property-id nil})]}  ; untyped relation -> gap
        report (tl/gap-report t m)]
    (testing "untyped event surfaces grouped by nearest, why-no-fit intact"
      (is (= 1 (reduce + (map :count (:untyped-events report)))))
      (is (= "ex:Act" (:nearest (first (:untyped-events report))))))
    (testing "untyped relation surfaces"
      (is (= 1 (reduce + (map :count (:untyped-relations report))))))
    (testing "shallow typing (ex:Act has leaf subclasses) surfaces"
      (is (some #(= "ex:Act" (:node-id %)) (:shallow-typings report))))
    (testing "unroled participant surfaces"
      (is (= 1 (count (:unroled-participants report))))
      (is (= "China" (:participant (first (:unroled-participants report))))))
    (testing "coverage counts exercised vs total"
      (is (= 1 (get-in report [:coverage :properties :used])))
      (is (pos? (get-in report [:coverage :occurrent :total]))))
    (testing "gap-count aggregates the findings"
      (is (= (tl/gap-count report)
             (+ 1 1 (count (:shallow-typings report)) 1))))))

(deftest gap-report-markdown-renders
  (let [m (fixture-model)
        t {:events [(ev "b" "Weird new act" {:node-id nil :nearest "ex:Act" :why-no-fit "no fit"})]
           :relations []}
        md (tl/gap-report-markdown (tl/gap-report t m) "Test scenario")]
    (is (re-find #"Gap report — Test scenario" md))
    (is (re-find #"Weird new act" md))))

;; --- chat ops ---------------------------------------------------------------

(deftest apply-op-add-and-remove-event
  (let [t (tl/new-timeline)
        t (tl/apply-op t {:op :add :target :event :value {:id "x" :label "New event"}})
        _ (is (= 1 (count (tl/events t))))
        t (tl/apply-op t {:op :remove :target :event :value {:label "New event"}})]
    (is (zero? (count (tl/events t))))))

(deftest forced-op-conflict-blocks-forced-event
  (let [t (-> (tl/new-timeline)
              (tl/add-event {:id "x" :label "Fixed" :status :forced}))]
    (is (tl/forced-op-conflict? t {:op :update :target :event :value {:label "Fixed"}}))
    (is (not (tl/forced-op-conflict? t {:op :add :target :event :value {:label "Other"}})))))
