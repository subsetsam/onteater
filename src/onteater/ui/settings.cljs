(ns onteater.ui.settings
  "The LLM settings panel — a tabbed dialog with one tab per provider:

    Ollama    — local server base URL, connection test, model picker.
    Cloud     — token-based hosted providers (Anthropic / OpenAI / any
                OpenAI-compatible endpoint): API key, test-and-list-models,
                model picker with filter or free-text fallback.
    Azure Gov — an Azure Government Azure-OpenAI deployment: endpoint,
                deployment name, api-version, api-key or Entra bearer token.

  The selected tab IS the active provider — whatever tab is showing is what
  'Run mapping' and chat use (state at [:llm :active]; see onteater.db and
  onteater.llm.providers). A shared temperature slider sits below the tabs
  (one stored value at [:ollama :options :temperature], read by every
  provider adapter; Anthropic ignores it — their current models reject
  sampling parameters).

  Security: key fields are type=password with a local-only reveal toggle;
  keys persist to IndexedDB only via the explicit 'Remember on this device'
  checkbox (see :llm/persist-settings). Rendered inside the modal dialog
  host."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.llm.providers :as providers]))

(defn- status-line
  "Connection status indicator, shared by all tabs. Pure view over the given
  status keyword + message (each tab passes its own provider's pair)."
  [status msg]
  [:div.conn-status {:class (str "conn-" (name (or status :unknown)))}
   [:span.conn-dot]
   [:span.conn-text
    (case status
      :checking "Checking…"
      :ok (str "Connected — " msg)
      :unreachable "Unreachable / CORS"
      :error "Error"
      "Not tested")]
   (when (and msg (contains? #{:unreachable :error} status))
     [:p.conn-detail msg])])

(defn- secret-input
  "Password input with a 👁 reveal toggle. The reveal flag is component-local
  (a Reagent atom) — never in app-db. Dispatches `on-set` with the value on
  blur, matching the base-url input pattern."
  [_props]
  (let [reveal? (r/atom false)]
    (fn [{:keys [value placeholder on-set]}]
      [:div.settings-row
       ^{:key value}
       [:input.insp-input {:type (if @reveal? "text" "password")
                           :default-value (or value "")
                           :placeholder placeholder
                           :auto-complete "off"
                           :on-blur #(on-set (.. % -target -value))}]
       [:button.btn.btn-reveal {:type "button"
                                :title (if @reveal? "Hide key" "Show key")
                                :on-click #(swap! reveal? not)}
        (if @reveal? "🙈" "👁")]])))

(defn- remember-key-checkbox
  "The explicit opt-in for persisting an API key. The wording is deliberate:
  IndexedDB storage is unencrypted, and the user should know that."
  [checked? set-event]
  [:label.settings-check
   [:input {:type "checkbox" :checked (boolean checked?)
            :on-change #(rf/dispatch [set-event :remember-key? (.. % -target -checked)])}]
   " Remember key on this device (stored unencrypted in this browser's storage)"])

;; --- Ollama tab (the original panel body, selectors preserved for E2E) --------

(defn- model-picker []
  (let [models @(rf/subscribe [:ollama/models])
        chosen @(rf/subscribe [:ollama/model])]
    [:div.settings-field
     [:label.settings-label "Model"]
     (if (seq models)
       [:select.insp-input
        {:value (or chosen "")
         :on-change #(rf/dispatch [:ollama/set-model (.. % -target -value)])}
        (for [m models]
          ^{:key (:name m)}
          [:option {:value (:name m)}
           (str (:name m)
                (when-let [p (get-in m [:details :parameter_size])] (str "  ·  " p)))])]
       [:p.settings-hint "Test the connection to list available models."])]))

(defn- ollama-tab []
  (let [settings @(rf/subscribe [:ollama/settings])]
    [:<>
     [:div.settings-field
      [:label.settings-label "Server base URL"]
      [:div.settings-row
       ^{:key (:base-url settings)}
       [:input.insp-input
        {:default-value (:base-url settings)
         :placeholder "http://localhost:11434"
         :on-blur #(rf/dispatch [:ollama/set-base-url (.. % -target -value)])}]
       [:button.btn.btn-primary {:on-click #(rf/dispatch [:ollama/test-connection])}
        "Test connection"]]]
     [status-line (:status settings) (:status-msg settings)]
     [model-picker]]))

;; --- Cloud tab ------------------------------------------------------------------

(defn- cloud-model-picker
  "Model chooser for the Cloud tab. With a fetched list: a select, plus a text
  filter when the list is long (OpenAI lists everything incl. embeddings).
  Without one (test not run / failed): free-text model-id entry, so the user
  can proceed regardless."
  [_props]
  (let [filter-text (r/atom "")]
    (fn [{:keys [models model]}]
      [:div.settings-field
       [:label.settings-label "Model"]
       (if (seq models)
         [:<>
          (when (> (count models) 20)
            [:input.insp-input.model-filter
             {:placeholder "Filter models…"
              :value @filter-text
              :on-change #(reset! filter-text (.. % -target -value))}])
          (let [q     (str/lower-case (str/trim @filter-text))
                shown (cond->> models
                        (seq q) (filter #(str/includes? (str/lower-case %) q)))
                ;; the chosen model always stays selectable, even filtered out
                shown (if (and (seq model) (not (some #{model} shown)))
                        (cons model shown)
                        shown)]
            [:select.insp-input.cloud-model
             {:value (or model "")
              :on-change #(rf/dispatch [:llm/set-cloud-field :model (.. % -target -value)])}
             (for [m shown] ^{:key m} [:option {:value m} m])])]
         [:<>
          ^{:key model}
          [:input.insp-input.cloud-model
           {:default-value (or model "")
            :placeholder "claude-opus-4-8"
            :on-blur #(rf/dispatch [:llm/set-cloud-field :model (.. % -target -value)])}]
          [:p.settings-hint "Test the connection to list models, or type a model id."]])])))

(defn- cloud-tab []
  (let [cloud   @(rf/subscribe [:llm/cloud])
        vendor  (:vendor cloud)
        preset? (not= :custom vendor)]
    [:<>
     [:div.settings-field
      [:label.settings-label "Provider"]
      [:select.insp-input.cloud-vendor
       {:value (name (or vendor :anthropic))
        :on-change #(rf/dispatch [:llm/set-cloud-field :vendor (keyword (.. % -target -value))])}
       (for [[k {:keys [label]}] providers/cloud-presets]
         ^{:key k} [:option {:value (name k)} label])]]

     [:div.settings-field
      [:label.settings-label "Base URL"]
      ;; Read-only for the known vendors (shown for transparency); editable for
      ;; Custom, where it points at any OpenAI-compatible gateway/proxy.
      ^{:key (str vendor "|" (:base-url cloud))}
      [:input.insp-input.cloud-base-url
       {:default-value (providers/cloud-base-url cloud)
        :read-only preset?
        :class (when preset? "input-readonly")
        :placeholder "https://llm-gateway.example.com"
        :on-blur #(when-not preset?
                    (rf/dispatch [:llm/set-cloud-field :base-url (.. % -target -value)]))}]]

     [:div.settings-field
      [:label.settings-label "API key"]
      [secret-input {:value (:api-key cloud)
                     :placeholder (case vendor :anthropic "sk-ant-…" :openai "sk-…" "token")
                     :on-set #(rf/dispatch [:llm/set-cloud-field :api-key %])}]]

     [:div.settings-row
      [:button.btn.btn-primary.llm-test {:on-click #(rf/dispatch [:llm/test-connection])}
       "Test & load models"]]
     [status-line (:status cloud) (:status-msg cloud)]

     [cloud-model-picker {:models (:models cloud) :model (:model cloud)}]
     [remember-key-checkbox (:remember-key? cloud) :llm/set-cloud-field]]))

;; --- Azure Gov tab -----------------------------------------------------------------

(defn- azgov-tab []
  (let [az @(rf/subscribe [:llm/azgov])]
    [:<>
     [:div.settings-field
      [:label.settings-label "Endpoint"]
      ^{:key (:base-url az)}
      [:input.insp-input.azgov-endpoint
       {:default-value (:base-url az)
        :placeholder "https://<resource>.openai.azure.us"
        :on-blur #(rf/dispatch [:llm/set-azgov-field :base-url (.. % -target -value)])}]
      ;; CORS reality check — see providers/azgov-cors-hint. We deliberately do
      ;; not try to work around it in code; the field accepts a proxy URL.
      [:p.settings-hint.settings-help providers/azgov-cors-hint]]

     [:div.settings-field
      [:label.settings-label "Deployment name"]
      ^{:key (:deployment az)}
      [:input.insp-input
       {:default-value (:deployment az)
        :placeholder "my-gpt4o-deployment"
        :on-blur #(rf/dispatch [:llm/set-azgov-field :deployment (.. % -target -value)])}]]

     [:div.settings-field
      [:label.settings-label "API version"]
      ^{:key (:api-version az)}
      [:input.insp-input
       {:default-value (:api-version az)
        :placeholder "2024-10-21"
        :on-blur #(rf/dispatch [:llm/set-azgov-field :api-version (.. % -target -value)])}]]

     [:div.settings-field
      [:label.settings-label "Authentication"]
      [:select.insp-input.azgov-auth
       {:value (name (or (:auth-scheme az) :api-key))
        :on-change #(rf/dispatch [:llm/set-azgov-field :auth-scheme (keyword (.. % -target -value))])}
       [:option {:value "api-key"} "API key"]
       [:option {:value "bearer"} "Bearer token (Entra ID)"]]
      (when (= :bearer (:auth-scheme az))
        [:p.settings-hint.settings-help
         "Paste a token obtained with:  az account get-access-token --resource https://cognitiveservices.azure.us"])]

     [:div.settings-field
      [:label.settings-label (if (= :bearer (:auth-scheme az)) "Bearer token" "API key")]
      [secret-input {:value (:api-key az)
                     :placeholder (if (= :bearer (:auth-scheme az)) "eyJ0eXAi…" "key")
                     :on-set #(rf/dispatch [:llm/set-azgov-field :api-key %])}]]

     [:div.settings-row
      [:button.btn.btn-primary.llm-test {:on-click #(rf/dispatch [:llm/test-connection])}
       "Test connection"]]
     [status-line (:status az) (:status-msg az)]

     [remember-key-checkbox (:remember-key? az) :llm/set-azgov-field]]))

;; --- the panel -----------------------------------------------------------------------

(def ^:private tabs
  [[:ollama "Ollama"] [:cloud "Cloud"] [:azgov "Azure Gov"]])

(defn panel []
  (let [active   (or @(rf/subscribe [:llm/active]) :ollama)
        cloud    @(rf/subscribe [:llm/cloud])
        settings @(rf/subscribe [:ollama/settings])]
    [:div.dialog.settings-dialog
     [:div.settings-head
      [:h3.dialog-title "LLM settings"]
      [:button.icon-btn {:on-click #(rf/dispatch [:ui/close-dialog])} "×"]]

     ;; Selecting a tab selects the active provider — configuring IS selecting.
     [:div.settings-tabs {:role "tablist"}
      (for [[id label] tabs]
        ^{:key id}
        [:button.settings-tab {:class (when (= id active) "settings-tab-active")
                               :on-click #(rf/dispatch [:llm/select-provider id])}
         label])]
     [:p.settings-hint.settings-tabs-caption "Mapping and chat use the selected tab."]

     (case active
       :cloud [cloud-tab]
       :azgov [azgov-tab]
       [ollama-tab])

     ;; One stored temperature, shared by every provider's adapter.
     [:div.settings-field
      [:label.settings-label
       (str "Temperature — " (get-in settings [:options :temperature]))]
      [:input {:type "range" :min 0 :max 1 :step 0.05
               :value (get-in settings [:options :temperature])
               :on-change #(rf/dispatch [:ollama/set-temperature
                                         (js/parseFloat (.. % -target -value))])}]
      (when (and (= :cloud active) (= :anthropic (:vendor cloud)))
        [:p.settings-hint "Not sent to Anthropic models (they reject sampling parameters)."])]

     [:div.dialog-actions
      [:button.btn {:on-click #(rf/dispatch [:ui/close-dialog])} "Done"]]]))
