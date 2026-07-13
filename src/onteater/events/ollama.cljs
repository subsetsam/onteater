(ns onteater.events.ollama
  "Events for configuring and probing the Ollama server. The
  connection test and model list both go through the single `:llm/request` effect,
  so auth (when added) and error classification are shared with mapping/chat."
  (:require [re-frame.core :as rf]
            [onteater.llm.client]))   ; registers :llm/request, :llm/abort

;; Setting events also persist the (non-secret) provider settings snapshot so
;; base-url/model/temperature survive a reload — see :llm/persist-settings.
(rf/reg-event-fx
 :ollama/set-base-url
 (fn [{:keys [db]} [_ url]]
   {:db (assoc-in db [:ollama :base-url] url)
    :dispatch [:llm/persist-settings]}))

(rf/reg-event-fx
 :ollama/set-model
 (fn [{:keys [db]} [_ m]]
   {:db (assoc-in db [:ollama :model] m)
    :dispatch [:llm/persist-settings]}))

(rf/reg-event-fx
 :ollama/set-temperature
 (fn [{:keys [db]} [_ t]]
   {:db (assoc-in db [:ollama :options :temperature] t)
    :dispatch [:llm/persist-settings]}))

;; Probe the server: GET /api/tags. Populates the model list on success; on
;; failure records a classified status with actionable guidance (incl. CORS).
(rf/reg-event-fx
 :ollama/test-connection
 (fn [{:keys [db]} _]
   {:db (-> db (assoc-in [:ollama :status] :checking)
            (assoc-in [:ollama :status-msg] "Contacting server…"))
    :llm/request {:key :tags
                  :base-url (get-in db [:ollama :base-url])
                  :method "GET" :path "/api/tags"
                  :on-done [:ollama/models-loaded]
                  :on-error [:ollama/connection-error]}}))

(rf/reg-event-db
 :ollama/models-loaded
 (fn [db [_ result]]
   (let [models (vec (:models result))]
     (-> db
         (assoc-in [:ollama :models] models)
         (assoc-in [:ollama :status] :ok)
         (assoc-in [:ollama :status-msg]
                   (str (count models) " model" (when (not= 1 (count models)) "s") " available"))
         ;; auto-select the first model if none chosen or the chosen one vanished
         (update-in [:ollama :model]
                    (fn [m] (if (some #(= m (:name %)) models) m (:name (first models)))))))))

(rf/reg-event-db
 :ollama/connection-error
 (fn [db [_ err]]
   (-> db
       (assoc-in [:ollama :status] (if (= :unreachable (:kind err)) :unreachable :error))
       (assoc-in [:ollama :status-msg] (:message err))
       (assoc-in [:ollama :models] []))))
