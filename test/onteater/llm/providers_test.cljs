(ns onteater.llm.providers-test
  "Unit tests for the provider adapter layer — request shapes, response
  parsing, schema hardening, and readiness checks for every provider. All
  pure: fixture maps in, maps out, no network."
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [onteater.db :as db]
            [onteater.llm.prompts :as prompts]
            [onteater.llm.providers :as p]))

(def messages
  [{:role "system" :content "SYS"}
   {:role "user" :content "USR"}])

(def cloud-anthropic
  {:provider :cloud :vendor :anthropic
   :base-url "https://api.anthropic.com" :api-key "sk-ant-test"
   :model "claude-opus-4-8"})

(def cloud-openai
  {:provider :cloud :vendor :openai
   :base-url "https://api.openai.com" :api-key "sk-test"
   :model "gpt-4o"})

(def azgov
  {:provider :azgov :base-url "https://myres.openai.azure.us"
   :deployment "DEP" :model "DEP" :api-version "2024-10-21"
   :auth-scheme :api-key :api-key "azkey"})

(def ollama
  {:provider :ollama :base-url "http://localhost:11434"
   :model "llama3.1" :options {:temperature 0.2}})

;; --- active-config ------------------------------------------------------------

(deftest active-config-per-provider
  (let [base db/default-db]
    (testing "default is ollama, flattened from the top-level :ollama key"
      (let [cfg (p/active-config base)]
        (is (= :ollama (:provider cfg)))
        (is (= "http://localhost:11434" (:base-url cfg)))))
    (testing "cloud resolves preset base-url and trims slashes"
      (let [cfg (p/active-config (-> base
                                     (assoc-in [:llm :active] :cloud)
                                     (assoc-in [:llm :cloud :vendor] :anthropic)))]
        (is (= :cloud (:provider cfg)))
        (is (= "https://api.anthropic.com" (:base-url cfg)))))
    (testing "custom vendor keeps the user's URL"
      (let [cfg (p/active-config (-> base
                                     (assoc-in [:llm :active] :cloud)
                                     (assoc-in [:llm :cloud :vendor] :custom)
                                     (assoc-in [:llm :cloud :base-url] "http://gw:8080/")))]
        (is (= "http://gw:8080" (:base-url cfg)))))
    (testing "azgov exposes the deployment as :model"
      (let [cfg (p/active-config (-> base
                                     (assoc-in [:llm :active] :azgov)
                                     (assoc-in [:llm :azgov :deployment] "gpt4-dep")))]
        (is (= :azgov (:provider cfg)))
        (is (= "gpt4-dep" (:model cfg)))))))

;; --- chat-request shapes --------------------------------------------------------

(deftest anthropic-chat-request-shape
  (let [req (p/chat-request cloud-anthropic
                            {:messages messages
                             :json-schema (prompts/mapping-schema)
                             :temperature 0.2})]
    (is (= "POST" (:method req)))
    (is (= "/v1/messages" (:path req)))
    (testing "all three browser-direct headers, exact"
      (is (= "sk-ant-test" (get-in req [:headers "x-api-key"])))
      (is (= "2023-06-01" (get-in req [:headers "anthropic-version"])))
      (is (= "true" (get-in req [:headers "anthropic-dangerous-direct-browser-access"]))))
    (testing "system message hoisted out of :messages into :system"
      (is (= "SYS" (get-in req [:body :system])))
      (is (= [{:role "user" :content "USR"}] (get-in req [:body :messages]))))
    (is (= p/anthropic-max-tokens (get-in req [:body :max_tokens])))
    (testing "no sampling params — current Anthropic models 400 on them"
      (is (not (contains? (:body req) :temperature))))
    (is (= "json_schema" (get-in req [:body :output_config :format :type])))))

(deftest anthropic-chat-request-without-schema
  (let [req (p/chat-request cloud-anthropic {:messages messages})]
    (is (not (contains? (:body req) :output_config)))))

(deftest anthropic-chat-request-without-system-message
  (let [req (p/chat-request cloud-anthropic {:messages [{:role "user" :content "hi"}]})]
    (is (not (contains? (:body req) :system)))
    (is (= 1 (count (get-in req [:body :messages]))))))

(deftest openai-chat-request-shape
  (let [req (p/chat-request cloud-openai
                            {:messages messages
                             :json-schema (prompts/mapping-schema)
                             :temperature 0.2})]
    (is (= "/v1/chat/completions" (:path req)))
    (is (= "Bearer sk-test" (get-in req [:headers "Authorization"])))
    (testing "system message stays in the array"
      (is (= messages (get-in req [:body :messages]))))
    (is (= 0.2 (get-in req [:body :temperature])))
    (testing "response_format json_schema with strict"
      (is (= "json_schema" (get-in req [:body :response_format :type])))
      (is (true? (get-in req [:body :response_format :json_schema :strict])))
      (is (map? (get-in req [:body :response_format :json_schema :schema]))))
    (testing "no token cap (deprecated max_tokens vs max_completion_tokens mess)"
      (is (not (contains? (:body req) :max_tokens)))
      (is (not (contains? (:body req) :max_completion_tokens))))))

(deftest azgov-chat-request-shape
  (let [req (p/chat-request azgov {:messages messages :temperature 0.2})]
    (is (= "/openai/deployments/DEP/chat/completions?api-version=2024-10-21" (:path req)))
    (is (= "azkey" (get-in req [:headers "api-key"])))
    (testing "no :model in the body — the deployment path selects it"
      (is (not (contains? (:body req) :model))))
    (testing "bearer scheme switches the header"
      (let [req' (p/chat-request (assoc azgov :auth-scheme :bearer) {:messages messages})]
        (is (= "Bearer azkey" (get-in req' [:headers "Authorization"])))
        (is (not (contains? (:headers req') "api-key")))))))

(deftest ollama-chat-request-shape
  (let [req (p/chat-request ollama {:messages messages
                                    :json-schema (prompts/mapping-schema)
                                    :temperature 0.7})]
    (is (= "/api/chat" (:path req)))
    (is (= {} (:headers req)))
    (is (= "llama3.1" (get-in req [:body :model])))
    (is (false? (get-in req [:body :stream])))
    (testing "the untouched schema rides in :format; cfg :options wins over the temperature arg"
      (is (= (prompts/mapping-schema) (get-in req [:body :format])))
      (is (= {:temperature 0.2} (get-in req [:body :options]))))))

;; --- response-text ---------------------------------------------------------------

(deftest response-text-per-provider
  (testing "anthropic concatenates text blocks"
    (is (= "AB" (p/response-text cloud-anthropic
                                 {:content [{:type "text" :text "A"}
                                            {:type "thinking" :thinking "x"}
                                            {:type "text" :text "B"}]
                                  :stop_reason "end_turn"}))))
  (testing "anthropic refusal yields nil"
    (is (nil? (p/response-text cloud-anthropic
                               {:content [] :stop_reason "refusal"}))))
  (testing "openai + azure read choices[0].message.content"
    (let [resp {:choices [{:message {:content "hello"}}]}]
      (is (= "hello" (p/response-text cloud-openai resp)))
      (is (= "hello" (p/response-text azgov resp)))))
  (testing "ollama prefers content, falls back to thinking"
    (is (= "c" (p/response-text ollama {:message {:content "c" :thinking "t"}})))
    (is (= "t" (p/response-text ollama {:message {:content "" :thinking "t"}})))))

;; --- connection test -------------------------------------------------------------

(deftest test-request-per-provider
  (testing "anthropic and openai list models with their auth headers"
    (let [req (p/test-request cloud-anthropic)]
      (is (= ["GET" "/v1/models"] [(:method req) (:path req)]))
      (is (= "sk-ant-test" (get-in req [:headers "x-api-key"]))))
    (let [req (p/test-request cloud-openai)]
      (is (= ["GET" "/v1/models"] [(:method req) (:path req)]))
      (is (= "Bearer sk-test" (get-in req [:headers "Authorization"])))))
  (testing "azgov has no list call — minimal chat against the deployment"
    (let [req (p/test-request azgov)]
      (is (= "POST" (:method req)))
      (is (str/includes? (:path req) "/openai/deployments/DEP/"))
      (is (= 1 (count (get-in req [:body :messages]))))))
  (testing "ollama probes /api/tags"
    (is (= "/api/tags" (:path (p/test-request ollama))))))

(deftest test-response->models-per-provider
  (is (= ["a" "b"] (p/test-response->models cloud-openai
                                            {:data [{:id "b"} {:id "a"}]})))
  (is (nil? (p/test-response->models azgov {:choices []})))
  (is (= ["m1"] (p/test-response->models ollama {:models [{:name "m1"}]}))))

(deftest default-model-prefers-preset
  (is (= "claude-opus-4-8"
         (p/default-model cloud-anthropic ["claude-haiku-4-5" "claude-opus-4-8"])))
  (is (= "claude-haiku-4-5"
         (p/default-model cloud-anthropic ["claude-haiku-4-5"]))))

;; --- strictify-schema -------------------------------------------------------------

(deftest strictify-schema-hardens-mapping-schema
  (let [s (p/strictify-schema (prompts/mapping-schema))
        entry (get-in s [:properties :entries :items])]
    (testing "every object gains additionalProperties false + full required"
      (is (false? (:additionalProperties s)))
      (is (= #{"entries" "unmapped"} (set (:required s))))
      (is (false? (:additionalProperties entry)))
      (is (= #{"excerpt" "occurrence" "node_id" "relation" "confidence" "rationale"}
             (set (:required entry)))))
    (testing "unsupported numeric constraints stripped"
      (is (not (contains? (get-in entry [:properties :occurrence]) :minimum)))
      (is (not (contains? (get-in entry [:properties :confidence]) :maximum))))
    (testing "enum retained verbatim"
      (is (= prompts/relation-enum (get-in entry [:properties :relation :enum]))))))

;; --- ready? -----------------------------------------------------------------------

(deftest ready?-flags-missing-fields
  (testing "complete configs are ready"
    (is (nil? (p/ready? cloud-anthropic)))
    (is (nil? (p/ready? azgov)))
    (is (nil? (p/ready? ollama))))
  (testing "cloud"
    (is (some? (p/ready? (assoc cloud-anthropic :api-key ""))))
    (is (some? (p/ready? (assoc cloud-anthropic :model nil))))
    (is (some? (p/ready? (assoc cloud-openai :vendor :custom :base-url "")))))
  (testing "azgov"
    (is (some? (p/ready? (assoc azgov :base-url ""))))
    (is (some? (p/ready? (assoc azgov :deployment ""))))
    (is (some? (p/ready? (assoc azgov :api-key nil)))))
  (testing "ollama"
    (is (some? (p/ready? (assoc ollama :model nil))))))
