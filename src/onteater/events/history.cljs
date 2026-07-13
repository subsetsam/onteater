(ns onteater.events.history
  "One re-frame interceptor that gives every model mutation undo/redo and dirty
  tracking, for free and uniformly. Instead of hand-rolling
  per-feature undo, any event that changes [:ontology :model] just includes the
  `record-history` interceptor; the interceptor snapshots the pre-mutation model
  onto the undo stack, clears the redo stack, and flips the dirty flag. Persistent
  data structures make full-model snapshots cheap.

  Undo/redo events restore snapshots symmetrically. The undo stack is capped so a
  long editing session cannot grow memory without bound."
  (:require [re-frame.core :as rf]))

(def ^:const max-undo 100)

(def record-history
  "Interceptor: snapshot [:ontology :model] before the handler; if the handler
  changed it, push the old model onto the undo stack, clear redo, mark dirty."
  (rf/->interceptor
   :id :record-history
   :before
   (fn [context]
     (assoc-in context [:coeffects ::pre-model]
               (get-in context [:coeffects :db :ontology :model])))
   :after
   (fn [context]
     (let [pre  (get-in context [:coeffects ::pre-model])
           ;; the handler's new db (falls back to the unchanged coeffect db)
           post-db (or (get-in context [:effects :db])
                       (get-in context [:coeffects :db]))
           post (get-in post-db [:ontology :model])]
       (if (or (nil? post-db) (identical? pre post) (= pre post))
         context
         (let [undo (-> (get-in post-db [:ontology :undo] [])
                        (conj pre)
                        (->> (take-last max-undo) vec))
               new-db (-> post-db
                          (assoc-in [:ontology :undo] undo)
                          (assoc-in [:ontology :redo] [])
                          (assoc-in [:ontology :dirty?] true))]
           (assoc-in context [:effects :db] new-db)))))))

(rf/reg-event-db
 :ontology/undo
 (fn [db _]
   (let [undo (get-in db [:ontology :undo])]
     (if (empty? undo)
       db
       (let [prev (peek undo)
             cur  (get-in db [:ontology :model])]
         (-> db
             (assoc-in [:ontology :model] prev)
             (assoc-in [:ontology :undo] (pop undo))
             (update-in [:ontology :redo] (fnil conj []) cur)
             (assoc-in [:ontology :dirty?] true)))))))

(rf/reg-event-db
 :ontology/redo
 (fn [db _]
   (let [redo (get-in db [:ontology :redo])]
     (if (empty? redo)
       db
       (let [nxt (peek redo)
             cur (get-in db [:ontology :model])]
         (-> db
             (assoc-in [:ontology :model] nxt)
             (assoc-in [:ontology :redo] (pop redo))
             (update-in [:ontology :undo] (fnil conj []) cur)
             (assoc-in [:ontology :dirty?] true)))))))
