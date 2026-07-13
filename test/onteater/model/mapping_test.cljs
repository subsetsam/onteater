(ns onteater.model.mapping-test
  "Tests for the mapping session model."
  (:require [cljs.test :refer [deftest is testing]]
            [onteater.model.mapping :as m]))

(defn- session []
  (m/new-session {:scenario {:text "Alpha uses tariffs. Beta imposes tariffs on Alpha."}
                  :model "test"}))

(deftest entry-lifecycle
  (let [s (-> (session)
              (m/add-entry {:excerpt "tariffs" :occurrence 1 :node-id "geo:Tariff"
                            :relation :instance-of :confidence 0.8}))
        id (:id (first (m/entries s)))]
    (is (= 1 (count (m/entries s))))
    (testing "update snapshots history"
      (let [s2 (m/update-entry s id assoc :confidence 0.4)]
        (is (= 0.4 (:confidence (m/entry s2 id))))
        (is (= 1 (count (:history (m/entry s2 id)))))
        (is (= 0.8 (:confidence (first (:history (m/entry s2 id))))))))
    (testing "force sets status + node and clears flags"
      (let [s3 (m/force-entry s id "geo:CustomsDuty")]
        (is (= :forced (:status (m/entry s3 id))))
        (is (= "geo:CustomsDuty" (:node-id (m/entry s3 id))))
        (is (= 1 (count (m/forced-entries s3))))))))

(deftest merge-preserves-user-decisions
  (let [s (-> (session)
              (m/add-entry {:excerpt "tariffs" :node-id "geo:Tariff" :status :forced})
              (m/add-entry {:excerpt "Alpha" :node-id "geo:Agent" :status :rejected}))
        incoming [{:excerpt "tariffs" :node-id "geo:Tariff" :relation :mentions} ; dup of forced
                  {:excerpt "tariffs" :node-id "geo:Sanction"}                     ; new pairing
                  {:excerpt "Alpha" :node-id "geo:Agent"}]                          ; dup of rejected
        merged (m/merge-entries (m/entries s) incoming)]
    (testing "forced + rejected survive; their exact dups are not re-added"
      (is (= 1 (count (filter #(= :forced (:status %)) merged))))
      (is (= 1 (count (filter #(= :rejected (:status %)) merged)))))
    (testing "a genuinely new (excerpt,node) pairing is added"
      (is (some #(= "geo:Sanction" (:node-id %)) merged)))))

(deftest nth-occurrence-anchoring
  (let [text "tariffs here and tariffs there"]
    (is (= 0 (m/nth-occurrence text "tariffs" 1)))
    (is (= 17 (m/nth-occurrence text "tariffs" 2)))
    (is (nil? (m/nth-occurrence text "tariffs" 3)))
    (is (nil? (m/nth-occurrence text "absent" 1)))))

(deftest excerpt-locatable-normalizes-markdown
  (let [raw "the Meridian Union imposed a **tariff** of 25% and gained *leverage*."]
    (testing "an excerpt quoted from rendered prose matches raw markdown source"
      (is (m/excerpt-locatable? raw {:excerpt "tariff of 25%" :occurrence 1}))
      (is (m/excerpt-locatable? raw {:excerpt "leverage" :occurrence 1})))
    (testing "genuinely absent excerpts are still not locatable"
      (is (not (m/excerpt-locatable? raw {:excerpt "sanctions regime" :occurrence 1}))))
    (testing "whitespace differences are tolerated"
      (is (m/excerpt-locatable? "a\n\ntariff   of  25%" {:excerpt "tariff of 25%" :occurrence 1})))))

(deftest chat-ops-apply-and-respect-forced
  (let [s (-> (session)
              (m/add-entry {:excerpt "tariffs" :node-id "geo:Tariff" :status :forced})
              (m/add-entry {:excerpt "Alpha" :node-id "geo:Agent" :status :proposed}))]
    (testing "add op inserts a new entry"
      (let [s2 (m/apply-op s {:op :add :entry (m/new-entry {:excerpt "Beta" :node-id "geo:Agent"})})]
        (is (some #(= "Beta" (:excerpt %)) (m/entries s2)))))
    (testing "update op retargets the matched entry"
      (let [s2 (m/apply-op s {:op :update :entry {:excerpt "Alpha" :node-id "geo:State"}})
            e  (first (filter #(= "Alpha" (:excerpt %)) (m/entries s2)))]
        (is (= "geo:State" (:node-id e)))))
    (testing "forced-op-conflict? guards forced entries"
      (is (m/forced-op-conflict? s {:op :update :entry {:excerpt "tariffs" :node-id "geo:X"}}))
      (is (m/forced-op-conflict? s {:op :remove :entry {:excerpt "tariffs" :node-id "geo:Tariff"}}))
      (is (not (m/forced-op-conflict? s {:op :update :entry {:excerpt "Alpha" :node-id "geo:X"}}))))))

(deftest coverage-and-summary
  (let [s (-> (session)
              (m/add-entry {:excerpt "tariffs" :node-id "geo:Tariff"})
              (m/add-entry {:excerpt "Alpha" :node-id "geo:Agent"}))
        node->module {"geo:Tariff" "INS" "geo:Agent" "spine"}
        summ (m/summary s node->module)]
    (is (= 2 (:entries summ)))
    (is (= 2 (:nodes summ)))
    (is (= 2 (:modules summ)))
    (testing "coverage counts paragraphs with a mapped excerpt"
      (is (pos? (:coverage summ))))
    (testing "rejected entries drop out of active counts"
      (let [s2 (m/set-status s (:id (first (m/entries s))) :rejected)]
        (is (= 1 (:entries (m/summary s2 node->module))))))))
