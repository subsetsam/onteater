(ns onteater.core
  "Application entry point.

  Responsibilities:
  - Wire up the re-frame default database (see `onteater.db`).
  - Mount the root Reagent view (`onteater.ui.shell/root`).
  - Expose `init` (called once at page load, per shadow-cljs `:init-fn`) and
    `reload` (called after every hot-reload during development).

  This namespace is deliberately thin: all behaviour lives in the events, subs,
  domain, and view namespaces. Keeping the entry point minimal makes the mount
  lifecycle easy to reason about and keeps hot-reload reliable."
  (:require [reagent.dom.client :as rdomc]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            ["d3" :as d3]
            [onteater.db]
            [onteater.events]
            [onteater.subs]
            [onteater.ui.keys :as keys]
            [onteater.ui.shell :as shell]))

;; A single React 18 root, reused across hot reloads. Creating a new root on
;; every reload would detach the old tree and leak; instead we create it once
;; and re-render into it.
(defonce ^:private react-root
  (atom nil))

(defn- ensure-root!
  "Return the memoised React 18 root for the #app mount point, creating it once."
  []
  (or @react-root
      (reset! react-root
              (rdomc/create-root (.getElementById js/document "app")))))

(defn ^:dev/after-load reload
  "Re-render the root view. Invoked by shadow-cljs after each hot reload.
  Clearing the subscription cache guarantees views pick up changed sub fns."
  []
  (rf/clear-subscription-cache!)
  (rdomc/render (ensure-root!) [shell/root]))

(defonce ^:private services-started?
  (atom false))

(defn- start-services!
  "Install one-time global side effects: keyboard shortcuts, the debounced
  autosave loop, crash-recovery check, and the unsaved-changes unload guard.
  Guarded so hot-reload does not stack duplicate listeners/intervals."
  []
  (when-not @services-started?
    (reset! services-started? true)
    (keys/install!)
    ;; Autosave loop: every 5s the handler no-ops unless the model changed.
    (js/setInterval #(rf/dispatch [:persist/autosave]) 5000)
    ;; Offer to recover an autosaved snapshot from a previous session.
    (rf/dispatch [:persist/check-recovery])
    ;; Restore persisted LLM provider settings (non-secret fields; keys only
    ;; when the user opted in via 'Remember on this device').
    (rf/dispatch [:llm/load-settings])
    ;; Warn before leaving with unsaved changes.
    (.addEventListener js/window "beforeunload"
                       (fn [e]
                         (when (get-in @rfdb/app-db [:ontology :dirty?])
                           (.preventDefault e)
                           (set! (.-returnValue e) ""))))))

(defn init
  "One-time application bootstrap (shadow-cljs `:init-fn`).
  Confirms the d3 interop bundled (so a missing/broken vendor import fails loudly
  at build/boot rather than deep inside a view), initialises app-db, starts the
  global services, then mounts."
  []
  (js/console.log "Onteater booting — d3 version" (.-version d3))
  (rf/dispatch-sync [:app/initialize])
  (start-services!)
  (reload))
