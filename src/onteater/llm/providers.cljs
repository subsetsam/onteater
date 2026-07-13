(ns onteater.llm.providers
  "Provider adapter layer for the LLM connections: local Ollama, token-based
  cloud providers (Anthropic / OpenAI / OpenAI-compatible), and Azure
  Government-hosted Azure OpenAI deployments.

  Every function here is PURE — data in, data out, no fetch, no DOM — so the
  request shapes and response parsing are unit-tested in the node suite without
  any live provider (see test/onteater/llm/providers_test.cljs). All
  provider-specific knowledge (endpoints, header names, body shapes, response
  paths) lives in this namespace and nowhere else; the events layer only calls
  `active-config`, `ready?`, `chat-request`, `response-text`, `test-request`,
  and `test-response->models`.

  A `cfg` map is the flattened config for one provider, produced by
  `active-config`. It always carries `:provider` (:ollama | :cloud | :azgov)
  and `:base-url`; `:cloud` adds `:vendor` (:anthropic | :openai | :custom),
  `:api-key`, `:model`; `:azgov` adds `:deployment` (exposed also as `:model`),
  `:api-version`, `:auth-scheme`, `:api-key`.

  Security note: cloud/azgov request headers carry API credentials. Never log
  a request map produced here."
  (:require [clojure.string :as str]))

;; --- provider registry -------------------------------------------------------

(def cloud-presets
  "Per-vendor defaults for the Cloud provider. :base-url is used whenever the
  vendor is not :custom; :default-model is preselected after a successful
  'Test & load models' when the user has not chosen one."
  {:anthropic {:label "Anthropic" :base-url "https://api.anthropic.com" :default-model "claude-opus-4-8"}
   :openai    {:label "OpenAI"    :base-url "https://api.openai.com"    :default-model "gpt-4o"}
   :custom    {:label "Custom (OpenAI-compatible)" :base-url "" :default-model ""}})

(def azgov-cors-hint
  "Shown with Azure Gov connection failures and in the Endpoint help text.
  Azure OpenAI's data plane does not emit Access-Control-Allow-Origin headers,
  so browser-direct calls are typically blocked by CORS — including from
  file://. We do not try to work around this in code; the endpoint field is
  free-form precisely so a CORS-capable front door can be used instead."
  (str "Azure endpoints usually block browser calls (CORS). Front the resource "
       "with Azure API Management (with a CORS policy) or a local proxy, and "
       "put that URL in Endpoint."))

(defn- trim-slash
  "Strip trailing slashes from a base URL so path concatenation never yields //."
  [url]
  (str/replace (or url "") #"/+$" ""))

(defn cloud-base-url
  "Effective base URL for a cloud config: the vendor preset unless the vendor is
  :custom or the user typed an explicit URL."
  [{:keys [vendor base-url]}]
  (trim-slash
   (if (and (not= :custom vendor) (str/blank? base-url))
     (get-in cloud-presets [vendor :base-url] "")
     base-url)))

(defn active-config
  "Flatten the active provider's settings out of app-db into one cfg map (see
  ns docstring). Ollama settings stay at the top-level :ollama key (backward
  compat); the two new providers live under [:llm :cloud] / [:llm :azgov]."
  [db]
  (case (get-in db [:llm :active] :ollama)
    :cloud (let [c (get-in db [:llm :cloud])]
             (assoc c :provider :cloud :base-url (cloud-base-url c)))
    :azgov (let [a (get-in db [:llm :azgov])]
             (assoc a :provider :azgov
                    :base-url (trim-slash (:base-url a))
                    ;; expose the deployment under :model too so callers that
                    ;; only want "the model name" (session stamp, run-button
                    ;; label) need no azure special case
                    :model (:deployment a)))
    (let [o (:ollama db)]
      {:provider :ollama
       :base-url (trim-slash (:base-url o))
       :model    (:model o)
       :options  (:options o)})))

(defn current-slot
  "The saved-credential slot id for the active provider and its sub-selection:
  `[:cloud <vendor>]` on the Cloud tab, `[:azgov <auth-scheme>]` on Azure Gov,
  nil for Ollama (which has no key). Each slot holds an independently-encrypted
  credential, so switching vendor or auth method selects a different saved key."
  [db]
  (case (get-in db [:llm :active] :ollama)
    :cloud [:cloud (get-in db [:llm :cloud :vendor])]
    :azgov [:azgov (get-in db [:llm :azgov :auth-scheme])]
    nil))

(defn ready?
  "nil when the cfg is complete enough to run mapping/chat, else a
  human-readable reason (surfaced verbatim as the run error)."
  [cfg]
  (case (:provider cfg)
    :cloud (cond
             (str/blank? (:api-key cfg))
             "Enter an API key in Settings (⚙), Cloud tab."
             (and (= :custom (:vendor cfg)) (str/blank? (:base-url cfg)))
             "Enter the base URL of your OpenAI-compatible endpoint in Settings (⚙), Cloud tab."
             (str/blank? (:model cfg))
             "Choose or enter a model in Settings (⚙), Cloud tab.")
    :azgov (cond
             (str/blank? (:base-url cfg))
             "Enter the Azure endpoint in Settings (⚙), Azure Gov tab."
             (str/blank? (:deployment cfg))
             "Enter the Azure deployment name in Settings (⚙), Azure Gov tab."
             (str/blank? (:api-key cfg))
             "Enter the Azure API key or bearer token in Settings (⚙), Azure Gov tab.")
    ;; :ollama — same check the mapping run always made
    (when (str/blank? (:model cfg))
      "Choose an Ollama model in Settings (⚙).")))

(defn provider-label
  "Short display name of a cfg's provider, for the run-button tooltip."
  [cfg]
  (case (:provider cfg) :cloud "Cloud" :azgov "Azure Gov" "Ollama"))

;; --- structured-output schema hardening --------------------------------------

(defn strictify-schema
  "Adapt a JSON schema (as produced by prompts/mapping-schema) to the strict
  subsets Anthropic (output_config.format) and OpenAI (strict: true) enforce:

  - every object gains \"additionalProperties\" false and a `required` listing
    ALL of its properties;
  - unsupported numeric/string constraints are stripped (minimum, maximum,
    minLength, maxLength, multipleOf) — Anthropic rejects them;
  - enum / type / items / description pass through untouched.

  Pure and recursive; safe on any schema-shaped nested map."
  [s]
  (cond
    (map? s)
    (let [s (apply dissoc s [:minimum :maximum :minLength :maxLength :multipleOf])
          s (into {} (map (fn [[k v]]
                            [k (if (= :enum k) v (strictify-schema v))])
                          s))]
      (if (and (= "object" (:type s)) (map? (:properties s)))
        (assoc s
               :additionalProperties false
               :required (mapv name (keys (:properties s))))
        s))
    (sequential? s) (mapv strictify-schema s)
    :else s))

;; --- chat request construction -----------------------------------------------

(def anthropic-max-tokens
  "Anthropic's Messages API requires max_tokens on every request. Mapping and
  chat responses are short (JSON entry lists / prose turns), so one generous
  constant serves both."
  8192)

(defn- anthropic-headers [api-key]
  ;; All three are required for a browser-direct call:
  ;;  - x-api-key: auth
  ;;  - anthropic-version: mandatory API version pin
  ;;  - anthropic-dangerous-direct-browser-access: opts into CORS from a
  ;;    browser page (including file://). Without it the preflight is refused.
  {"x-api-key" api-key
   "anthropic-version" "2023-06-01"
   "anthropic-dangerous-direct-browser-access" "true"})

(defn- azgov-headers [{:keys [auth-scheme api-key]}]
  (if (= :bearer auth-scheme)
    {"Authorization" (str "Bearer " api-key)}
    {"api-key" api-key}))

(defn- azgov-chat-path [{:keys [deployment api-version]}]
  ;; Classic Azure OpenAI data-plane path: the deployment name stands in for
  ;; the model, the api-version rides as a query param.
  (str "/openai/deployments/" deployment
       "/chat/completions?api-version=" api-version))

(defn- anthropic-chat-request
  [{:keys [api-key model]} {:keys [messages json-schema]}]
  ;; The system message is a top-level field on Anthropic's Messages API, not
  ;; messages[0]. Deliberately NO temperature: current Anthropic models
  ;; (Opus 4.7+, Sonnet 5, Fable 5) return 400 on sampling params.
  (let [[head & tail] messages
        system? (= "system" (:role head))
        msgs    (vec (if system? tail messages))]
    {:method "POST"
     :path "/v1/messages"
     :headers (anthropic-headers api-key)
     :body (cond-> {:model model
                    :max_tokens anthropic-max-tokens
                    :messages msgs}
             system?     (assoc :system (:content head))
             json-schema (assoc :output_config
                                {:format {:type "json_schema"
                                          :schema (strictify-schema json-schema)}}))}))

(defn- openai-response-format [json-schema]
  {:type "json_schema"
   :json_schema {:name "onteater_mapping"
                 :strict true
                 :schema (strictify-schema json-schema)}})

(defn- openai-chat-request
  [{:keys [api-key model]} {:keys [messages json-schema temperature]}]
  ;; Deliberately no token cap: max_tokens is deprecated on newer OpenAI models
  ;; (max_completion_tokens replaces it) and defaults are ample; omitting both
  ;; avoids the per-model parameter mess.
  {:method "POST"
   :path "/v1/chat/completions"
   :headers {"Authorization" (str "Bearer " api-key)}
   :body (cond-> {:model model :messages messages}
           (number? temperature) (assoc :temperature temperature)
           json-schema           (assoc :response_format
                                        (openai-response-format json-schema)))})

(defn- azgov-chat-request
  [cfg {:keys [messages json-schema temperature]}]
  ;; Same body dialect as OpenAI (response_format needs api-version >=
  ;; 2024-08-01; the default 2024-10-21 qualifies) but no :model — the
  ;; deployment in the path selects it.
  {:method "POST"
   :path (azgov-chat-path cfg)
   :headers (azgov-headers cfg)
   :body (cond-> {:messages messages}
           (number? temperature) (assoc :temperature temperature)
           json-schema           (assoc :response_format
                                        (openai-response-format json-schema)))})

(defn- ollama-chat-request
  [{:keys [model options]} {:keys [messages json-schema temperature]}]
  ;; Exactly the request events/mapping + events/chat always built. Ollama's
  ;; native structured output rides in :format; :options carries temperature.
  {:method "POST"
   :path "/api/chat"
   :headers {}
   :body (cond-> {:model model
                  :messages messages
                  :stream false
                  :options (or options {:temperature temperature})}
           json-schema (assoc :format json-schema))})

(defn chat-request
  "Build the provider-appropriate chat request as
  {:method :path :headers :body} (body pre-JSON; the client stringifies).
  `json-schema` is optional (mapping passes one, chat does not); `temperature`
  is ignored by vendors that reject sampling params (Anthropic)."
  [cfg opts]
  (case (:provider cfg)
    :cloud (if (= :anthropic (:vendor cfg))
             (anthropic-chat-request cfg opts)
             (openai-chat-request cfg opts))
    :azgov (azgov-chat-request cfg opts)
    (ollama-chat-request cfg opts)))

;; --- response parsing ---------------------------------------------------------

(defn- anthropic-response-text
  "Concatenate the text blocks of an Anthropic Messages response (already
  js->clj'd with keyword keys by the client). A refusal stop_reason yields nil
  so the caller surfaces it as an empty reply rather than parsing garbage."
  [resp]
  (when-not (= "refusal" (:stop_reason resp))
    (->> (:content resp)
         (filter #(= "text" (:type %)))
         (map :text)
         (str/join))))

(defn- openai-response-text [resp]
  (get-in resp [:choices 0 :message :content]))

(defn- ollama-response-text
  "Prefer message.content; fall back to message.thinking for hybrid/reasoning
  models (some MLX builds) that leave content empty and put the answer — JSON
  included — in the thinking field."
  [resp]
  (let [c (get-in resp [:message :content])]
    (if (str/blank? c) (get-in resp [:message :thinking]) c)))

(defn response-text
  "Extract the assistant text from a parsed (CLJS map, keyword keys) chat
  response for `cfg`'s provider. Returns nil for empty/refused replies."
  [cfg resp]
  (case (:provider cfg)
    :cloud (if (= :anthropic (:vendor cfg))
             (anthropic-response-text resp)
             (openai-response-text resp))
    :azgov (openai-response-text resp)
    (ollama-response-text resp)))

;; --- connection testing --------------------------------------------------------

(defn test-request
  "Request map for the per-provider connection test.

  - Ollama / Anthropic / OpenAI: GET the models list — proves auth AND
    populates the model dropdown in one call.
  - Azure Gov: there is no data-plane 'list deployments' call, so the test is
    a minimal chat against the configured deployment."
  [cfg]
  (case (:provider cfg)
    :cloud (if (= :anthropic (:vendor cfg))
             {:method "GET" :path "/v1/models"
              :headers (anthropic-headers (:api-key cfg))}
             {:method "GET" :path "/v1/models"
              :headers {"Authorization" (str "Bearer " (:api-key cfg))}})
    :azgov {:method "POST"
            :path (azgov-chat-path cfg)
            :headers (azgov-headers cfg)
            :body {:messages [{:role "user"
                               :content "Reply with the single word: ok"}]}}
    {:method "GET" :path "/api/tags" :headers {}}))

(defn test-response->models
  "Model-id strings from a successful test response, or nil when the provider
  has no listable models (Azure Gov — the deployment field is the model)."
  [cfg resp]
  (case (:provider cfg)
    :cloud (vec (sort (keep :id (:data resp))))
    :azgov nil
    (vec (keep :name (:models resp)))))

(defn default-model
  "The model to preselect after a successful test when none is chosen: the
  vendor preset if the server offers it, else the first listed."
  [cfg models]
  (let [preferred (get-in cloud-presets [(:vendor cfg) :default-model])]
    (if (some #{preferred} models) preferred (first models))))
