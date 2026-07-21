(ns onteater.model.docs
  "Order-preserving tree model for ontology documentation sections.

  Geo-format files carry prose sections (`metadata`, `worked_examples`,
  `design_decisions`, `revision_notes`, `governance`, …) alongside the node
  graph. These must be editable AND re-emitted with their original JSON key
  order for minimal diffs — which rules out `js->clj` maps (CLJS hash maps lose
  insertion order above 8 entries). Instead a docs value is a tagged tree:

    {:t :map :entries [[\"title\" tree] ...]}   ; entry order = JSON key order
    {:t :vec :items [tree ...]}
    {:t :val :v x}                             ; string | number | boolean | nil

  JSON keys stay raw strings (never keywordized — keys like \"6.1\" or
  \"first_pass\" must survive untouched). A path into a tree is a vector of
  segments: a string selects a map entry, an integer a vector item.

  Everything here is pure (the JS interop in `js->tree`/`tree->js` is
  value-in/value-out) and fully headless-testable."
  (:require [clojure.string :as str]
            [goog.object :as gobj]))

;; ---------------------------------------------------------------------------
;; JS <-> tree
;; ---------------------------------------------------------------------------

(defn js->tree
  "Convert a parsed-JSON JS value into a tagged tree, preserving object key
  order (iterates `Object.keys`, which reflects insertion order)."
  [x]
  (cond
    (array? x)
    {:t :vec :items (mapv #(js->tree (aget x %)) (range (.-length x)))}

    (and (some? x) (object? x))
    {:t :map :entries (mapv (fn [k] [k (js->tree (gobj/get x k))])
                            (js/Object.keys x))}

    :else
    {:t :val :v x}))

(defn tree->js
  "Convert a tagged tree back to a JS value, writing map entries in order so
  the emitted JSON reproduces the original key order."
  [{:keys [t] :as tree}]
  (case t
    :val (:v tree)
    :vec (let [a (array)]
           (doseq [item (:items tree)] (.push a (tree->js item)))
           a)
    :map (let [o (js-obj)]
           (doseq [[k v] (:entries tree)] (gobj/set o k (tree->js v)))
           o)))

;; ---------------------------------------------------------------------------
;; Path navigation
;; ---------------------------------------------------------------------------

(defn- entry-index
  "Index of key `k` in a :map node's entries, or nil."
  [entries k]
  (first (keep-indexed (fn [i [ek _]] (when (= ek k) i)) entries)))

(defn get-at
  "The subtree at `path`, or nil when the path does not resolve."
  [tree path]
  (if (empty? path)
    tree
    (let [[seg & more] path]
      (cond
        (and (= :map (:t tree)) (string? seg))
        (when-let [i (entry-index (:entries tree) seg)]
          (get-at (second (nth (:entries tree) i)) more))

        (and (= :vec (:t tree)) (int? seg)
             (< -1 seg (count (:items tree))))
        (get-at (nth (:items tree) seg) more)))))

(defn update-at
  "Replace the subtree at `path` with `(f subtree)`. Returns `tree` unchanged
  when the path does not resolve."
  [tree path f]
  (if (empty? path)
    (f tree)
    (let [[seg & more] path]
      (cond
        (and (= :map (:t tree)) (string? seg))
        (if-let [i (entry-index (:entries tree) seg)]
          (update-in tree [:entries i 1] update-at more f)
          tree)

        (and (= :vec (:t tree)) (int? seg)
             (< -1 seg (count (:items tree))))
        (update-in tree [:items seg] update-at more f)

        :else tree))))

;; ---------------------------------------------------------------------------
;; Edits
;; ---------------------------------------------------------------------------

(defn set-at
  "Set the node at `path` to the scalar `v` (string/number/boolean/nil)."
  [tree path v]
  (update-at tree path (constantly {:t :val :v v})))

(defn blank-like
  "A blank template with `tree`'s shape: strings->\"\", numbers->0,
  booleans->false, nested vectors start empty, map keys are kept. Used to seed
  a new list item from its last sibling."
  [tree]
  (case (:t tree)
    :val {:t :val :v (let [v (:v tree)]
                       (cond (number? v)  0
                             (boolean? v) false
                             :else        ""))}
    :vec {:t :vec :items []}
    :map {:t :map :entries (mapv (fn [[k v]] [k (blank-like v)]) (:entries tree))}))

(defn vec-add
  "Append a new item to the :vec node at `path` — a blank-like clone of the
  last item, or an empty string scalar for an empty list."
  [tree path]
  (update-at tree path
             (fn [node]
               (if (= :vec (:t node))
                 (update node :items conj
                         (if-let [last-item (peek (:items node))]
                           (blank-like last-item)
                           {:t :val :v ""}))
                 node))))

(defn remove-at
  "Remove the map entry or vector item addressed by `path` (which must be
  non-empty — the last segment names what to remove)."
  [tree path]
  (let [parent (vec (butlast path))
        seg    (last path)]
    (update-at tree parent
               (fn [node]
                 (cond
                   (and (= :map (:t node)) (string? seg))
                   (update node :entries (fn [es] (vec (remove #(= seg (first %)) es))))

                   (and (= :vec (:t node)) (int? seg)
                        (< -1 seg (count (:items node))))
                   (update node :items
                           (fn [items]
                             (vec (concat (subvec items 0 seg)
                                          (subvec items (inc seg))))))

                   :else node)))))

(defn map-add-key
  "Append entry `k` (an empty string scalar) to the :map node at `path`.
  Refuses blank or already-present keys."
  [tree path k]
  (update-at tree path
             (fn [node]
               (if (and (= :map (:t node))
                        (not (str/blank? k))
                        (nil? (entry-index (:entries node) k)))
                 (update node :entries conj [k {:t :val :v ""}])
                 node))))

(defn rename-key
  "Rename entry `old` to `new` on the :map node at `path`, keeping its
  position. Refuses blank names and collisions."
  [tree path old new]
  (update-at tree path
             (fn [node]
               (if (and (= :map (:t node))
                        (not (str/blank? new))
                        (nil? (entry-index (:entries node) new)))
                 (update node :entries
                         (fn [es] (mapv (fn [[k v]] (if (= k old) [new v] [k v])) es)))
                 node))))

(defn- empty-node?
  "Is this node safe to re-type? Blank/nil scalars and empty collections."
  [node]
  (case (:t node)
    :val (let [v (:v node)] (or (nil? v) (and (string? v) (str/blank? v))))
    :vec (empty? (:items node))
    :map (empty? (:entries node))
    false))

(defn set-kind
  "Convert an *empty* node at `path` to a fresh node of `kind`
  (:val | :vec | :map) — lets the user author nested structure generically.
  Non-empty nodes are left alone (no destructive conversions)."
  [tree path kind]
  (update-at tree path
             (fn [node]
               (if (empty-node? node)
                 (case kind
                   :val {:t :val :v ""}
                   :vec {:t :vec :items []}
                   :map {:t :map :entries []}
                   node)
                 node))))
