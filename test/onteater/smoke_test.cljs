(ns onteater.smoke-test
  "A trivial test that exists so the :node-test runner has something to execute
  from Milestone 0 onward, proving the headless test harness itself works before
  the real model/format tests land in Milestone 1."
  (:require [cljs.test :refer [deftest is]]
            [onteater.db :as db]))

(deftest default-db-is-well-formed
  (is (= :ontology (:workspace db/default-db)))
  (is (contains? db/default-db :ontology))
  (is (contains? db/default-db :scenario))
  (is (contains? db/default-db :ollama))
  (is (= "http://localhost:11434" (get-in db/default-db [:ollama :base-url]))))

(deftest default-llm-provider-settings
  (is (= :ollama (get-in db/default-db [:llm :active]))
      "Ollama stays the default provider — existing flows unchanged")
  (is (= :anthropic (get-in db/default-db [:llm :cloud :vendor])))
  (is (= "2024-10-21" (get-in db/default-db [:llm :azgov :api-version])))
  (is (false? (get-in db/default-db [:llm :cloud :remember-key?]))
      "API keys are session-only unless explicitly remembered")
  (is (false? (get-in db/default-db [:llm :azgov :remember-key?]))))
