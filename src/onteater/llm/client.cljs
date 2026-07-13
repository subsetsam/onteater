(ns onteater.llm.client
  "LLM HTTP client. A single `request!` seam handles every outbound
  call — local Ollama (GET /api/tags, POST /api/chat) and the cloud/Azure-Gov
  providers (paths/headers/bodies built by `onteater.llm.providers`). Two
  design commitments:

  1. **One fetch site, zero call-site fetch logic.** Callers pass a `:headers`
     map (merged over the Content-Type default) built by the provider adapter;
     `*request-middleware*` remains a `request-map -> request-map` hook for a
     future mTLS-proxy story. Headers may carry API credentials — NEVER log a
     request map from this namespace.

  2. **Streaming + cancellation are first-class.** `/api/chat` is read through a
     `ReadableStream` reader, NDJSON parsed incrementally; every request wires an
     `AbortController` so runs are cancellable and mid-stream aborts discard partial
     results cleanly.

  Errors are classified so the UI can show actionable guidance — notably the
  `file://`-origin CORS case, which the browser reports as an opaque
  `TypeError` on an otherwise-reachable host."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

(def ^:dynamic *request-middleware*
  "fn: request-map -> request-map, applied to every outbound LLM request.
  Provider auth headers arrive via the caller's :headers (see
  onteater.llm.providers); this hook remains for cross-cutting concerns
  (mTLS proxy, request logging redaction). Default is identity."
  identity)

(defn- classify-error
  "Turn a fetch failure into {:kind kw :message str}. Distinguishes user aborts,
  and the file://-origin CORS/unreachable case from ordinary failures."
  [err]
  (let [name (some-> err .-name)]
    (cond
      (= name "AbortError")
      {:kind :aborted :message "Request cancelled."}
      ;; fetch rejects with a TypeError for both network failure and CORS. From a
      ;; file:// page the usual cause is Ollama refusing the null origin.
      (= name "TypeError")
      {:kind :unreachable
       :message (str "Could not reach the Ollama server, or it refused this page's "
                     "origin (CORS). If the server is running, start it so it accepts "
                     "this origin, e.g.  OLLAMA_ORIGINS=\"*\" ollama serve  (or list your "
                     "specific origin). See Help for details.")}
      :else
      {:kind :error :message (or (some-> err .-message) "Unknown network error.")})))

(defn- read-stream!
  "Read an NDJSON `ReadableStream` body, invoking `on-chunk` with each parsed JSON
  object (as a CLJS map) and `on-done` when the stream ends."
  [resp on-chunk on-done on-error]
  (let [reader  (.getReader (.-body resp))
        decoder (js/TextDecoder.)
        buffer  (atom "")]
    (letfn [(flush-lines! [final?]
              (let [parts (str/split @buffer #"\n")
                    ;; keep the trailing partial line unless this is the final flush
                    complete (if final? parts (butlast parts))]
                (reset! buffer (if final? "" (or (last parts) "")))
                (doseq [line complete
                        :when (not (str/blank? line))]
                  (when on-chunk
                    (try (on-chunk (js->clj (js/JSON.parse line) :keywordize-keys true))
                         (catch :default _ nil))))))
            (pump []
              (-> (.read reader)
                  (.then (fn [res]
                           (if (.-done res)
                             (do (flush-lines! true) (when on-done (on-done)))
                             (do (swap! buffer str (.decode decoder (.-value res) #js {:stream true}))
                                 (flush-lines! false)
                                 (pump)))))
                  (.catch (fn [err] (when on-error (on-error (classify-error err)))))))]
      (pump))))

(defn request!
  "Perform one LLM request. Options:
    :base-url :method :path :headers :body :stream? :signal :on-chunk :on-done :on-error
  `:headers` (optional) is merged over the Content-Type default — this is how
  provider API credentials travel; never log the request map. For streaming,
  `on-chunk` receives each NDJSON object; for non-streaming, `on-done` receives
  the whole parsed JSON. `on-error` receives {:kind :message}."
  [{:keys [base-url method path headers body stream? signal on-chunk on-done on-error]}]
  (let [req  (*request-middleware*
              {:url     (str base-url path)
               :method  (or method "GET")
               :headers (merge {"Content-Type" "application/json"} headers)
               :body    (when body (js/JSON.stringify (clj->js body)))})
        opts (cond-> #js {:method (:method req)
                          :headers (clj->js (:headers req))}
               (:body req) (doto (aset "body" (:body req)))
               signal      (doto (aset "signal" signal)))]
    (-> (js/fetch (:url req) opts)
        (.then (fn [resp]
                 (cond
                   (not (.-ok resp))
                   (when on-error (on-error {:kind :http
                                             :message (str "Server returned HTTP " (.-status resp)
                                                           " " (.-statusText resp))}))
                   stream? (read-stream! resp on-chunk on-done on-error)
                   :else   (-> (.json resp)
                               (.then (fn [j] (when on-done
                                                (on-done (js->clj j :keywordize-keys true)))))))))
        (.catch (fn [err] (when on-error (on-error (classify-error err))))))))

;; --- re-frame effects -------------------------------------------------------
;; Callbacks are supplied as event vectors; we wrap them to dispatch, appending
;; the chunk/result/error. AbortControllers are kept by key so :llm/abort can
;; cancel an in-flight request (there is one per workspace at a time).

(defonce ^:private controllers (atom {}))

(rf/reg-fx
 :llm/request
 (fn [{:keys [key on-chunk on-done on-error] :as opts}]
   (let [ctl (js/AbortController.)
         k   (or key :default)]
     (swap! controllers assoc k ctl)
     (request! (assoc opts
                      :signal (.-signal ctl)
                      :on-chunk (when on-chunk #(rf/dispatch (conj on-chunk %)))
                      :on-done  (fn [& [r]]
                                  (swap! controllers dissoc k)
                                  (when on-done (rf/dispatch (conj on-done r))))
                      :on-error (fn [e]
                                  (swap! controllers dissoc k)
                                  (when on-error (rf/dispatch (conj on-error e)))))))))

(rf/reg-fx
 :llm/abort
 (fn [{:keys [key]}]
   (when-let [c (get @controllers (or key :default))]
     (.abort c)
     (swap! controllers dissoc (or key :default)))))
