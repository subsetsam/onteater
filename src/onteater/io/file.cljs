(ns onteater.io.file
  "File I/O effects: opening ontology files (and, from Milestone 3, saving through
  a retained File System Access handle with a download fallback).

  These are re-frame *effect* handlers — the one place DOM/File APIs are touched.
  The rest of the app requests them declaratively via the effect map, keeping
  event handlers pure and testable. Milestone 2 implements Open via a
  plain `<input type=file>` (works in every browser); Milestone 3 layers the FS
  Access API on top for true in-place Save."
  (:require [re-frame.core :as rf]))

(defn fs-access?
  "Is the File System Access API (true in-place Save) available? Chromium-only.
  Firefox/Safari fall back to `<input>`-open and download-based save."
  []
  (and (exists? js/window) (exists? (.-showOpenFilePicker js/window))))

(defn- abort-error? [err]
  (= "AbortError" (.-name err)))   ; user cancelled the picker — not a failure

;; :io/pick-file — open the OS file picker and read the chosen file as text. On
;; success dispatches `on-loaded` with [filename text] appended; on read error
;; dispatches `on-error` with a message appended. `accept` overrides the filter.
(rf/reg-fx
 :io/pick-file
 (fn [{:keys [on-loaded on-error accept]}]
   (let [input (.createElement js/document "input")]
     (set! (.-type input) "file")
     (set! (.-accept input) (or accept ".json"))
     (set! (.-onchange input)
           (fn [e]
             (when-let [file (aget (.. e -target -files) 0)]
               (let [reader (js/FileReader.)]
                 (set! (.-onload reader)
                       (fn [_] (rf/dispatch (conj on-loaded (.-name file) (.-result reader)))))
                 (set! (.-onerror reader)
                       (fn [_] (when on-error
                                 (rf/dispatch (conj on-error (str "Could not read " (.-name file)))))))
                 (.readAsText reader file)))))
     (.click input))))

;; :io/download-text — trigger a browser download of `text` as `filename` (the
;; Save-As fallback for browsers without the File System Access API).
(rf/reg-fx
 :io/download-text
 (fn [{:keys [filename text mime]}]
   (let [blob (js/Blob. #js [text] #js {:type (or mime "application/json")})
         url  (js/URL.createObjectURL blob)
         a    (.createElement js/document "a")]
     (set! (.-href a) url)
     (set! (.-download a) (or filename "ontology.json"))
     (.appendChild (.-body js/document) a)
     (.click a)
     (.removeChild (.-body js/document) a)
     (js/setTimeout #(js/URL.revokeObjectURL url) 1000))))

;; :io/open-file — open an ontology. Uses the FS Access API when available (so we
;; retain a handle for true in-place Save); otherwise falls back to <input>.
;; on-loaded gets [filename text handle] appended (handle is nil on the fallback).
(rf/reg-fx
 :io/open-file
 ;; `accept` (optional) restricts the file chooser: {:description str :mime str
 ;; :extensions [".ext" ...]}. Defaults to the ontology JSON filter; the scenario
 ;; uploader passes a text/markdown filter.
 (fn [{:keys [on-loaded on-error accept]}]
   (let [{:keys [description mime extensions]
          :or {description "Ontology" mime "application/json" extensions [".json"]}} accept
         input-accept (apply str (interpose "," extensions))]
     (if (fs-access?)
       (let [accept-obj (js-obj)]
         (aset accept-obj mime (clj->js (vec extensions)))
         (-> (.showOpenFilePicker js/window
                                  #js {:types #js [#js {:description description
                                                        :accept accept-obj}]})
             (.then (fn [handles]
                      (let [h (aget handles 0)]
                        (-> (.getFile h)
                            (.then (fn [file]
                                     (-> (.text file)
                                         (.then (fn [txt] (rf/dispatch (conj on-loaded (.-name file) txt h)))))))))))
             (.catch (fn [err]
                       (when (and on-error (not (abort-error? err)))
                         (rf/dispatch (conj on-error (.-message err))))))))
       ;; fallback: hidden <input type=file>
       (let [input (.createElement js/document "input")]
         (set! (.-type input) "file")
         (set! (.-accept input) input-accept)
         (set! (.-onchange input)
               (fn [e]
                 (when-let [file (aget (.. e -target -files) 0)]
                   (let [reader (js/FileReader.)]
                     (set! (.-onload reader)
                           (fn [_] (rf/dispatch (conj on-loaded (.-name file) (.-result reader) nil))))
                     (.readAsText reader file)))))
         (.click input))))))

;; :io/save-file — write `text` through an existing FS Access `handle` (in place).
(rf/reg-fx
 :io/save-file
 (fn [{:keys [handle text on-saved on-error]}]
   (-> (.createWritable handle)
       (.then (fn [w] (-> (.write w text) (.then (fn [_] (.close w))))))
       (.then (fn [_] (when on-saved (rf/dispatch on-saved))))
       (.catch (fn [err] (when on-error (rf/dispatch (conj on-error (.-message err)))))))))

;; :io/save-as — pick a new location. FS Access returns a retained handle; the
;; fallback just downloads. on-saved gets [filename handle] appended (handle nil
;; on fallback).
(rf/reg-fx
 :io/save-as
 (fn [{:keys [suggested-name text on-saved on-error mime]}]
   (if (fs-access?)
     (-> (.showSaveFilePicker js/window #js {:suggestedName suggested-name})
         (.then (fn [h]
                  (-> (.createWritable h)
                      (.then (fn [w] (-> (.write w text) (.then (fn [_] (.close w))))))
                      (.then (fn [_] (when on-saved (rf/dispatch (conj on-saved (.-name h) h))))))))
         (.catch (fn [err] (when (and on-error (not (abort-error? err)))
                             (rf/dispatch (conj on-error (.-message err)))))))
     ;; fallback: download, no handle retained
     (let [blob (js/Blob. #js [text] #js {:type (or mime "application/json")})
           url  (js/URL.createObjectURL blob)
           a    (.createElement js/document "a")]
       (set! (.-href a) url) (set! (.-download a) suggested-name)
       (.appendChild (.-body js/document) a) (.click a) (.removeChild (.-body js/document) a)
       (js/setTimeout #(js/URL.revokeObjectURL url) 1000)
       (when on-saved (rf/dispatch (conj on-saved suggested-name nil)))))))
