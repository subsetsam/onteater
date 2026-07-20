(ns onteater.subs.timeline
  "Subscriptions for the temporal-mapping feature: the active session's timeline, the
  pure swimlane layout (ordering × lanes × routed edges), the dependency-cone for the
  selected event, the entity dependency matrix, and the measured gap report. All
  derivations are pure (`onteater.model.timeline`); the viz/UI just render them."
  (:require [re-frame.core :as rf]
            [clojure.set :as set]
            [onteater.model.timeline :as tl]))

(rf/reg-sub
 :timeline/timeline
 :<- [:scenario/active-session]
 (fn [session _] (:timeline session)))

(rf/reg-sub :timeline/ui (fn [db _] (get-in db [:scenario :tl-ui]
                                            {:tab :mapping :grouping :entity :cone? false
                                             :selected-event nil :selected-relation nil
                                             :collapsed #{} :matrix-cell nil})))
(rf/reg-sub :timeline/run (fn [db _] (get-in db [:scenario :timeline-run])))

(rf/reg-sub :timeline/tab       :<- [:timeline/ui] (fn [ui _] (:tab ui)))
(rf/reg-sub :timeline/grouping  :<- [:timeline/ui] (fn [ui _] (:grouping ui)))
(rf/reg-sub :timeline/cone?     :<- [:timeline/ui] (fn [ui _] (:cone? ui)))
(rf/reg-sub :timeline/selected-event-id    :<- [:timeline/ui] (fn [ui _] (:selected-event ui)))
(rf/reg-sub :timeline/selected-relation-id :<- [:timeline/ui] (fn [ui _] (:selected-relation ui)))
(rf/reg-sub :timeline/matrix-cell :<- [:timeline/ui] (fn [ui _] (:matrix-cell ui)))

(rf/reg-sub
 :timeline/event-count
 :<- [:timeline/timeline]
 (fn [tl _] (count (tl/active-events tl))))

;; The pure layout in ordinal units — ordering (x) × lane assignment (y) × routed
;; dependency edges (fork/join fan-out). Auto-collapse of low-activity lanes kicks
;; in for busy scenarios (>40 events), per §6.7.3.
(rf/reg-sub
 :timeline/layout
 :<- [:timeline/timeline]
 :<- [:timeline/grouping]
 :<- [:scenario/node->module]
 (fn [[tl grouping n->m] _]
   (when tl
     (let [n (count (tl/active-events tl))]
       (tl/layout tl {:grouping grouping :node->module n->m
                      :collapse-threshold (if (> n 40) 2 1)})))))

(rf/reg-sub
 :timeline/selected-event
 :<- [:timeline/timeline]
 :<- [:timeline/selected-event-id]
 :<- [:ontology/model]
 (fn [[tl id model] _]
   (when (and tl id)
     (when-let [e (tl/event tl id)]
       (assoc e :node (when (:node-id e) (get-in model [:nodes (:node-id e)])))))))

(rf/reg-sub
 :timeline/selected-relation
 :<- [:timeline/timeline]
 :<- [:timeline/selected-relation-id]
 (fn [[tl id] _] (when (and tl id) (tl/relation tl id))))

;; Dependency cone (ancestors/descendants) for the selected event when cone mode is
;; on — the swimlane dims everything outside it.
(rf/reg-sub
 :timeline/cone
 :<- [:timeline/timeline]
 :<- [:timeline/cone?]
 :<- [:timeline/selected-event-id]
 (fn [[tl on? id] _]
   (when (and tl on? id)
     (assoc (tl/dependency-cone tl id) :focus id))))

;; The concrete dependency paths for the cone side panel: chains from each upstream
;; source into the focus, and from the focus out to each downstream sink, each step
;; labelled with its relation type — readable prose order (§6.7.4, objective 1).
(rf/reg-sub
 :timeline/cone-paths
 :<- [:timeline/timeline]
 :<- [:timeline/cone]
 (fn [[tl cone] _]
   (when cone
     (let [focus (:focus cone)
           by-id (into {} (map (juxt :id identity)) (tl/active-events tl))
           label #(or (:label (by-id %)) %)
           ;; upstream sources = ancestors with no ancestor of their own within the cone
           anc (:ancestors cone)
           des (:descendants cone)
           sources (filter #(empty? (set/intersection anc (tl/ancestors tl %))) anc)
           sinks   (filter #(empty? (set/intersection des (tl/descendants tl %))) des)
           chain->prose (fn [path]
                          (->> (partition 2 1 path)
                               (map (fn [[a b]] (str (label a) " —"
                                                     (name (:type (tl/relation-between tl a b))) "→ ")))
                               (apply str)
                               (#(str % (label (last path))))))]
       {:upstream   (mapv chain->prose (mapcat #(tl/paths-between tl % focus) sources))
        :downstream (mapv chain->prose (mapcat #(tl/paths-between tl focus %) sinks))}))))

(rf/reg-sub
 :timeline/matrix
 :<- [:timeline/timeline]
 (fn [tl _] (when tl (tl/entity-dependency-matrix tl))))

(rf/reg-sub
 :timeline/gap-report
 :<- [:timeline/timeline]
 :<- [:ontology/model]
 (fn [[tl model] _] (when (and tl model) (tl/gap-report tl model))))

(rf/reg-sub
 :timeline/gap-count
 :<- [:timeline/gap-report]
 (fn [report _] (if report (tl/gap-count report) 0)))
