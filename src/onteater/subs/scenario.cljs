(ns onteater.subs.scenario
  "Subscriptions for the scenario-mapping workspace: the active session, its entries
  (filtered/grouped for the board), the Level-1 summary, run status, and the
  excerpt→entry index the scenario pane uses to draw highlights."
  (:require [re-frame.core :as rf]
            [onteater.model.graph :as g]
            [onteater.model.mapping :as m]))

(rf/reg-sub :scenario/raw-text (fn [db _] (get-in db [:scenario :raw-text])))
(rf/reg-sub :scenario/rendered? (fn [db _] (get-in db [:scenario :rendered?])))
(rf/reg-sub :scenario/run (fn [db _] (get-in db [:scenario :run])))
(rf/reg-sub :scenario/strategy (fn [db _] (get-in db [:scenario :strategy])))
(rf/reg-sub :ontology/briefing (fn [db _] (get-in db [:ontology :briefing])))
(rf/reg-sub :ontology/briefing-run (fn [db _] (get-in db [:ontology :briefing-run])))
(rf/reg-sub :scenario/selected-entry-id (fn [db _] (get-in db [:scenario :selected-entry])))
(rf/reg-sub :scenario/board (fn [db _] (get-in db [:scenario :board])))
(rf/reg-sub :chat/open? (fn [db _] (get-in db [:scenario :chat-open?])))
(rf/reg-sub :chat/input (fn [db _] (get-in db [:scenario :chat-input])))

(rf/reg-sub
 :chat/messages
 :<- [:scenario/active-session]
 (fn [session _] (:chat session)))

(rf/reg-sub
 :chat/pending?
 :<- [:chat/messages]
 (fn [msgs _] (boolean (some :pending? msgs))))

(rf/reg-sub :mapping/can-undo? (fn [db _] (boolean (seq (get-in db [:scenario :undo])))))
(rf/reg-sub :mapping/can-redo? (fn [db _] (boolean (seq (get-in db [:scenario :redo])))))

(rf/reg-sub
 :scenario/active-session
 (fn [db _] (get-in db [:scenario :sessions (get-in db [:scenario :active])])))

;; node-id -> module, from the current ontology model (drives per-module grouping,
;; summary counts, and highlight colours).
(rf/reg-sub
 :scenario/node->module
 :<- [:ontology/model]
 (fn [model _]
   (if model
     (into {} (map (fn [n] [(:id n) (:module n)])) (g/nodes model))
     {})))

(rf/reg-sub
 :scenario/summary
 :<- [:scenario/active-session]
 :<- [:scenario/node->module]
 (fn [[session n->m] _]
   (when session (m/summary session n->m))))

(rf/reg-sub
 :scenario/entries
 :<- [:scenario/active-session]
 (fn [session _] (when session (m/entries session))))

;; Entries prepared for the board: annotated with target label + module, filtered
;; by the confidence threshold, grouped, and sorted.
(rf/reg-sub
 :scenario/board-entries
 :<- [:scenario/active-session]
 :<- [:ontology/model]
 :<- [:scenario/board]
 (fn [[session model board] _]
   (when session
     (let [min-conf (or (:min-confidence board) 0)
           annotate (fn [e]
                      (let [node (g/node model (:node-id e))]
                        (assoc e
                               :node-label (or (:label node) (:node-id e))
                               :module (:module node))))
           entries (->> (m/active-entries session)
                        (filter #(>= (:confidence %) min-conf))
                        (map annotate))]
       (case (:group-by board)
         :confidence (->> entries (sort-by :confidence >)
                          (group-by (fn [e] (cond (>= (:confidence e) 0.75) "High"
                                                  (>= (:confidence e) 0.4) "Medium"
                                                  :else "Low")))
                          (sort-by (fn [[k _]] ({"High" 0 "Medium" 1 "Low" 2} k 3))))
         :scenario   [["In scenario order" (vec entries)]]
         ;; default: by module
         (->> entries (group-by #(or (:module %) "—")) (sort-by first)))))))

;; excerpt -> {:entry-id :module :node-label ...} for the scenario highlighter.
(rf/reg-sub
 :scenario/highlights
 :<- [:scenario/active-session]
 :<- [:ontology/model]
 (fn [[session model] _]
   (when session
     (for [e (m/active-entries session)
           :when (seq (:excerpt e))]
       {:entry-id (:id e)
        :excerpt (:excerpt e)
        :occurrence (:occurrence e)
        :module (:module (g/node model (:node-id e)))
        :node-label (or (:label (g/node model (:node-id e))) (:node-id e))
        :status (:status e)}))))

(rf/reg-sub
 :scenario/selected-entry
 :<- [:scenario/active-session]
 :<- [:scenario/selected-entry-id]
 :<- [:ontology/model]
 (fn [[session id model] _]
   (when (and session id)
     (when-let [e (m/entry session id)]
       (assoc e :node (g/node model (:node-id e)))))))
