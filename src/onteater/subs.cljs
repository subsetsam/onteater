(ns onteater.subs
  "re-frame subscriptions — the read side of app-db.

  Views never read app-db directly; they subscribe. Subscriptions are grouped by
  feature to mirror the event namespaces. Layered subs (materialised views over
  the model) are added alongside the features that need them; Milestone 0 defines
  the shell-level subscriptions only."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [onteater.llm.providers :as providers]
            [onteater.subs.ontology]     ; ontology + view subscriptions
            [onteater.subs.scenario]))   ; scenario mapping subscriptions

(rf/reg-sub :app/workspace (fn [db _] (:workspace db)))

(rf/reg-sub :ui/theme      (fn [db _] (get-in db [:ui :theme])))
(rf/reg-sub :ui/menu       (fn [db _] (get-in db [:ui :menu])))
(rf/reg-sub :ui/toasts     (fn [db _] (get-in db [:ui :toasts])))
(rf/reg-sub :ui/help-open?  (fn [db _] (get-in db [:ui :help-open?])))

(rf/reg-sub :ontology/model (fn [db _] (get-in db [:ontology :model])))
(rf/reg-sub :ontology/file  (fn [db _] (get-in db [:ontology :file])))
(rf/reg-sub :ontology/dirty? (fn [db _] (get-in db [:ontology :dirty?])))
(rf/reg-sub :ontology/can-undo? (fn [db _] (boolean (seq (get-in db [:ontology :undo])))))
(rf/reg-sub :ontology/can-redo? (fn [db _] (boolean (seq (get-in db [:ontology :redo])))))

(rf/reg-sub :ui/dialogs (fn [db _] (get-in db [:ui :dialogs])))
(rf/reg-sub :ui/top-dialog (fn [db _] (peek (get-in db [:ui :dialogs]))))
(rf/reg-sub :ui/context-menu (fn [db _] (get-in db [:ui :context-menu])))

(rf/reg-sub :ollama/settings (fn [db _] (:ollama db)))
(rf/reg-sub :ollama/status   (fn [db _] (get-in db [:ollama :status])))
(rf/reg-sub :ollama/status-msg (fn [db _] (get-in db [:ollama :status-msg])))
(rf/reg-sub :ollama/models   (fn [db _] (get-in db [:ollama :models])))
(rf/reg-sub :ollama/model    (fn [db _] (get-in db [:ollama :model])))

;; --- multi-provider LLM connections (Settings tabs; see onteater.llm.providers)
(rf/reg-sub :llm/active (fn [db _] (get-in db [:llm :active])))
(rf/reg-sub :llm/cloud  (fn [db _] (get-in db [:llm :cloud])))
(rf/reg-sub :llm/azgov  (fn [db _] (get-in db [:llm :azgov])))
;; "model (Provider)" string for the Run-mapping tooltip; nil when the active
;; provider has no model/deployment chosen yet.
(rf/reg-sub :llm/active-model-label
            (fn [db _]
              (let [cfg (providers/active-config db)]
                (when-not (str/blank? (:model cfg))
                  (str (:model cfg) " (" (providers/provider-label cfg) ")")))))
