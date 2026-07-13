(ns onteater.ui.keys
  "Global keyboard shortcuts. A single window keydown listener maps
  chords to re-frame events. Shortcuts that would clobber typing are suppressed
  when the focus is in a text field (Escape and the save/open chords still fire)."
  (:require [re-frame.core :as rf]))

(defn- editable-target? [e]
  (let [t (.-target e)
        tag (some-> t .-tagName)]
    (or (contains? #{"INPUT" "TEXTAREA" "SELECT"} tag)
        (and t (.-isContentEditable t)))))

(defn- handle [e]
  (let [key   (.-key e)
        mod   (or (.-metaKey e) (.-ctrlKey e))
        shift (.-shiftKey e)
        editing (editable-target? e)]
    (cond
      ;; Save / Open work everywhere and prevent the browser default.
      (and mod (= key "s")) (do (.preventDefault e) (rf/dispatch [:ontology/save]))
      (and mod (= key "o")) (do (.preventDefault e) (rf/dispatch [:ontology/open]))
      (and mod (= (.toLowerCase key) "z") (not shift)) (do (.preventDefault e) (rf/dispatch [:app/undo]))
      (and mod (= (.toLowerCase key) "z") shift) (do (.preventDefault e) (rf/dispatch [:app/redo]))
      (and mod (= (.toLowerCase key) "y")) (do (.preventDefault e) (rf/dispatch [:app/redo]))

      (= key "Escape") (rf/dispatch [:ui/escape])

      ;; The rest only when not typing into a field.
      editing nil

      (or (= key "Delete") (= key "Backspace"))
      (do (.preventDefault e) (rf/dispatch [:ui/delete-selection]))

      (= key "/")
      (do (.preventDefault e)
          (some-> (.querySelector js/document ".search-input") (.focus)))

      (= key "?")
      (rf/dispatch [:ui/help-toggle])

      :else nil)))

(defn install!
  "Attach the global keydown listener once."
  []
  (.addEventListener js/window "keydown" handle))
