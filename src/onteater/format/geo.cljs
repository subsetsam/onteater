(ns onteater.format.geo
  "Adapter for the `galactic-economic-ontology.json` family of reference ontologies.

  ## Why this adapter is the hard part

  The sample file is NOT a flat node-link graph. Class/Property objects (each an
  object carrying `id` + `kind`) are scattered across dozens of paths —
  `spine.classes`, `modules.*.subsections.*.classes`, `modules.INS.families.*`,
  `modules.INS.three_way_split.*`, `modules.*.core_classes`, `relations.properties`,
  `axioms.supporting_classes`, `prospectus_alignment.classes`, … — interleaved with
  large prose sections that must survive untouched. Some ids appear at more than
  one path with DIFFERENT content (the spine copy is authoritative and richer; the
  module copy is a terse reference).

  ## Round-trip strategy (the non-negotiable golden test)

  We keep the *entire original file text* as `:residual` (a string). Parsing
  extracts a canonical node/edge/group view plus a `node-index` (each node's
  provenance paths + the exact fields it was parsed from). Serialisation diffs the
  current model against that index:

    - No changes  -> return the original text verbatim (byte-for-byte identical).
    - Changes     -> re-parse the original text to a JS tree, mutate only the
                     objects at the changed nodes' provenance paths (preserving
                     every other object's key order, so edits produce minimal,
                     readable diffs), and re-stringify with 2-space indent.

  Duplicate ids merge into ONE canonical node with MULTIPLE provenance paths;
  field edits propagate to every path, but each path keeps its own `subClassOf`
  (so the authoritative-vs-reference asymmetry is preserved on write). Full
  subclass restructuring through the geo format is intentionally limited — the
  native format (`onteater.format.native`) is the lossless escape hatch.

  ## A note on JS interop here

  Parsing reads via `js->clj` (pure Clojure walk — fully headless-testable).
  Serialisation of an *edited* model mutates the re-parsed JS object tree in place
  (via goog.object) purely so key order is preserved for clean diffs; this is
  local to `serialize`, has no external side effects, and is the one considered
  exception to the domain layer's \"CLJS data only\" preference."
  (:require [clojure.string :as str]
            [goog.object :as gobj]
            [onteater.model.graph :as g]
            [onteater.format.core :as fmt]))

;; ---------------------------------------------------------------------------
;; kind <-> string
;; ---------------------------------------------------------------------------

(defn- kind->kw [s]
  (case s
    "Class"      :class
    "Property"   :property
    "Individual" :individual
    (keyword (str/lower-case (str s)))))

(defn- kw->kind [k]
  (case k
    :class      "Class"
    :property   "Property"
    :individual "Individual"
    (str/capitalize (name k))))

;; ---------------------------------------------------------------------------
;; Parse — walk the JSON tree, extract node occurrences with provenance
;; ---------------------------------------------------------------------------

(defn- node-object?
  "An object is a node iff it carries both `id` and `kind` strings."
  [m]
  (and (map? m) (string? (get m "id")) (string? (get m "kind"))))

(defn- walk-occurrences
  "Depth-first walk of the js->clj'd `data`, collecting every node occurrence as
  {:raw <the object> :path <vector of string/int segments>}. Recurses into node
  objects too (harmless — the sample never nests nodes)."
  [data]
  (letfn [(step [x path acc]
            (cond
              (map? x)
              (let [acc (if (node-object? x) (conj acc {:raw x :path path}) acc)]
                (reduce-kv (fn [a k v] (step v (conj path k) a)) acc x))
              (vector? x)
              (reduce (fn [a i] (step (nth x i) (conj path i) a)) acc (range (count x)))
              :else acc))]
    (step data [] [])))

(defn- normalize-subclass
  "The `subClassOf` value may be a single id string or a vector of ids; return a
  vector of target id strings (possibly empty)."
  [raw]
  (let [v (get raw "subClassOf")]
    (cond
      (nil? v)    []
      (string? v) [v]
      (vector? v) (vec v)
      :else       [])))

(def ^:private known-node-keys #{"id" "label" "kind" "subClassOf" "gloss"})

(defn- extra-props
  "Any keys on a raw node beyond the five canonical ones become `:props`."
  [raw]
  (into {} (remove (fn [[k _]] (known-node-keys k))) raw))

;; ---------------------------------------------------------------------------
;; Grouping — classify a provenance path into (section, optional subgroup)
;; ---------------------------------------------------------------------------

(defn- classify-path
  "Map a provenance `path` to {:section s :sub k? :sub-kind kw? :sub-label s?}.
  `section` is the top-level home (module code, or spine/relations/axioms/…);
  `sub` names a nested container (subsection, family, or a named block) when one
  applies. Drives the outline tree."
  [path]
  (let [s0 (get path 0)]
    (case s0
      "spine"                {:section "spine"}
      "relations"            {:section "relations"}
      "axioms"               {:section "axioms"}
      "prospectus_alignment" {:section "prospectus_alignment"}
      "modules"
      (let [code (get path 1)
            c    (get path 2)]
        (case c
          "subsections"          {:section code :sub (get path 3) :sub-kind :subsection}
          "families"             {:section code :sub (get path 3) :sub-kind :family}
          "core_classes"         {:section code :sub "core_classes" :sub-kind :block :sub-label "Core classes"}
          "named_strategies"     {:section code :sub "named_strategies" :sub-kind :block :sub-label "Named strategies"}
          "adjacent_instruments" {:section code :sub "adjacent_instruments" :sub-kind :block :sub-label "Adjacent instruments"}
          "three_way_split"      {:section code :sub "three_way_split" :sub-kind :block :sub-label "Three-way split"}
          "classes"              {:section code}
          {:section code :sub c :sub-kind :block :sub-label c}))
      ;; default: use the first segment as the section
      {:section (or s0 "root")})))

(defn- section-label [data section]
  (case section
    "spine"                "Spine — BFO upper level"
    "relations"            "Relations — object properties"
    "axioms"               "Axioms & supporting classes"
    "prospectus_alignment" "Prospectus alignment"
    (if-let [t (get-in data ["modules" section "title"])]
      (str "Module " section " — " t)
      section)))

(defn- sub-label [data section {:keys [sub sub-kind sub-label]}]
  (or sub-label
      (case sub-kind
        :subsection (or (get-in data ["modules" section "subsections" sub "title"]) (str "§" sub))
        :family     (or (get-in data ["modules" section "families" sub "title"]) sub)
        sub)))

;; ---------------------------------------------------------------------------
;; Parse — assemble the canonical model
;; ---------------------------------------------------------------------------

(defn- best-gloss [raws]
  (->> (map #(get % "gloss") raws)
       (remove str/blank?)
       (sort-by count >)
       first))

(defn- primary-path
  "Choose a node's definitional home path: prefer an occurrence that declares
  `subClassOf` (authoritative), else the lexicographically-first path (stable)."
  [occurrences]
  (let [with-sub (filter #(seq (normalize-subclass (:raw %))) occurrences)]
    (:path (or (first (sort-by (comp pr-str :path) with-sub))
               (first (sort-by (comp pr-str :path) occurrences))))))

(defn- prompt-notes
  "Collect the ontology's own modeling-discipline prose for the LLM prompt layer
  (`[:meta :prompt-notes]`): the spine discipline notes plus the natural-language
  axiom statements (capped). Format-specific extraction lives HERE in the adapter
  — prompts.cljs renders the strings verbatim and stays format-neutral."
  [data]
  (let [spine-notes (keep #(get-in data ["spine" %])
                          ["role_discipline_note" "disposition_discipline_note"])
        axioms      (->> (get-in data ["axioms" "axioms"])
                         (keep #(get % "statement"))
                         (take 10))]
    (vec (remove str/blank? (concat spine-notes axioms)))))

(defn- build-model [data raw-str]
  (let [occurrences (walk-occurrences data)
        by-id       (group-by (comp #(get % "id") :raw) occurrences)

        ;; --- canonical nodes (merge duplicate ids) ---
        real-nodes
        (into {}
              (map (fn [[id occs]]
                     (let [raws     (map :raw occs)
                           paths    (mapv :path occs)
                           parents  (into #{} (mapcat #(normalize-subclass %)) raws)
                           prim     (primary-path occs)
                           section  (:section (classify-path prim))
                           node     {:id         id
                                     :label      (some #(get % "label") raws)
                                     :kind       (kind->kw (some #(get % "kind") raws))
                                     :gloss      (best-gloss raws)
                                     :props      (reduce merge {} (map extra-props raws))
                                     :module     section
                                     :external?  false
                                     :provenance paths}]
                       [id node])))
              by-id)

        node-ids (set (keys real-nodes))

        ;; --- subclass edges + external stub targets ---
        parent-pairs (for [[id occs] by-id
                           t (into #{} (mapcat #(normalize-subclass (:raw %))) occs)]
                       [id t])
        external-targets (into #{} (comp (map second) (remove node-ids)) parent-pairs)
        external-nodes
        (into {}
              (map (fn [t]
                     [t {:id t
                         :label (let [n (last (str/split t #"[:#/]"))]
                                  (if (str/blank? n) t n))
                         :kind :class :gloss nil :props {}
                         :module "external" :external? true :provenance []}]))
              external-targets)

        nodes (merge real-nodes external-nodes)

        edges (into {}
                    (map (fn [[s t]]
                           (let [e (g/make-edge s :subclass-of t)]
                             [(:id e) e])))
                    parent-pairs)

        ;; --- node-index for round-trip diffing ---
        node-index
        (into {}
              (map (fn [[id node]]
                     [id {:provenance (:provenance node)
                          :original {:label   (:label node)
                                     :kind    (:kind node)
                                     :gloss   (:gloss node)
                                     :props   (:props node)
                                     :parents (into #{} (comp (filter #(= id (first %)))
                                                              (map second)) parent-pairs)}}]))
              real-nodes)

        ;; --- groups (outline tree) ---
        groups
        (reduce
         (fn [gs {:keys [raw path]}]
           (let [id      (get raw "id")
                 cls     (classify-path path)
                 section (:section cls)
                 sec-gid section
                 gs      (update gs sec-gid
                                 (fn [g] (-> (or g {:id sec-gid
                                                    :label (section-label data section)
                                                    :kind :section :parent nil
                                                    :subgroups #{} :members []})
                                             (update :members (fn [m] (if (some #{id} m) m (conj m id)))))))]
             (if-let [sub (:sub cls)]
               (let [sub-gid (str section "/" sub)]
                 (-> gs
                     (update sec-gid update :subgroups conj sub-gid)
                     (update sub-gid
                             (fn [g] (-> (or g {:id sub-gid
                                               :label (sub-label data section cls)
                                               :kind (:sub-kind cls) :parent sec-gid
                                               :subgroups #{} :members []})
                                         (update :members (fn [m] (if (some #{id} m) m (conj m id)))))))))
               gs)))
         {}
         occurrences)]

    {:meta   (let [notes (prompt-notes data)]
               (cond-> {:title      (get-in data ["metadata" "title"] "Untitled ontology")
                        :version    (get-in data ["metadata" "version"])
                        :namespaces (get data "namespaces" {})
                        :format     :geo-reference-json
                        :onteater/geo {:node-index node-index}}
                 (seq notes) (assoc :prompt-notes notes)))
     :nodes  nodes
     :edges  edges
     :groups groups
     :residual raw-str
     :order  (vec (keys real-nodes))}))

(defn parse-str
  "Parse a geo JSON string into a canonical model. Throws a user-readable ex-info
  on malformed JSON."
  [raw-str]
  (let [data (try (js->clj (js/JSON.parse raw-str) :keywordize-keys false)
                  (catch :default e
                    (throw (ex-info "This file is not valid JSON."
                                    {:message (str "JSON parse error: " (.-message e))}))))]
    (when-not (map? data)
      (throw (ex-info "Expected a JSON object at the top level."
                      {:message "This does not look like a geo ontology file."})))
    (build-model data raw-str)))

;; ---------------------------------------------------------------------------
;; Serialize — diff the current model against the parse-time index
;; ---------------------------------------------------------------------------

(defn- node-changed?
  "Has `node` diverged from its parsed `original` (label/kind/gloss/props/parents)?"
  [node original current-parents]
  (or (not= (:label node)  (:label original))
      (not= (:kind node)   (:kind original))
      (not= (:gloss node)  (:gloss original))
      (not= (:props node)  (:props original))
      (not= current-parents (:parents original))))

(defn- compute-plan
  "Diff current model nodes vs the parse-time node-index. Returns
  {:patches [{:id :node :parents}] :deletes [{:id :provenance}] :adds [node]}.
  External stub nodes are ignored (they have no provenance in the source)."
  [model]
  (let [node-index (get-in model [:meta :onteater/geo :node-index])
        current    (:nodes model)
        parents-of (fn [id] (g/parents model id))
        indexed    (set (keys node-index))
        present    (into #{} (comp (remove #(:external? (val %))) (map key)) current)]
    {:deletes (for [id indexed :when (not (contains? current id))]
                {:id id :provenance (get-in node-index [id :provenance])})
     :patches (for [id present
                    :let [node (get current id)
                          idx  (get node-index id)]
                    :when (and idx
                               (node-changed? node (:original idx) (parents-of id)))]
                {:id id :node node :provenance (:provenance idx)
                 :parents (parents-of id) :original (:original idx)})
     :adds    (for [id present :when (not (contains? node-index id))]
                (get current id))}))

(defn- empty-plan? [{:keys [patches deletes adds]}]
  (and (empty? patches) (empty? deletes) (empty? adds)))

(defn- js-get-in
  "Navigate a parsed JS tree by a provenance path (string keys / int indices)."
  [root path]
  (reduce (fn [acc seg] (when acc (if (number? seg) (aget acc seg) (gobj/get acc seg))))
          root path))

(defn- apply-patch! [root {:keys [node provenance parents original]}]
  (doseq [path provenance]
    (when-let [obj (js-get-in root path)]
      (gobj/set obj "label" (:label node))
      (gobj/set obj "kind" (kw->kind (:kind node)))
      (if (str/blank? (:gloss node))
        (gobj/remove obj "gloss")
        (gobj/set obj "gloss" (:gloss node)))
      ;; subClassOf: only rewrite for single-home nodes whose parent set changed,
      ;; so multi-provenance (duplicate-id) nodes keep each path's own value.
      (when (and (= 1 (count provenance)) (not= parents (:parents original)))
        (cond
          (empty? parents)      (gobj/remove obj "subClassOf")
          (= 1 (count parents)) (gobj/set obj "subClassOf" (first parents))
          :else                 (gobj/set obj "subClassOf" (clj->js (vec parents))))))))

(defn- apply-deletes! [root deletes]
  ;; Group deletion targets by their containing array, then splice indices in
  ;; descending order so earlier indices stay valid during removal.
  (let [array-hits (for [{:keys [provenance]} deletes
                         path provenance
                         :when (number? (last path))]
                     {:arr-path (vec (butlast path)) :idx (last path)})]
    (doseq [[arr-path hits] (group-by :arr-path array-hits)]
      (when-let [arr (js-get-in root arr-path)]
        (doseq [idx (sort > (map :idx hits))]
          (.splice arr idx 1))))))

(defn- node->js
  "Build a JS object for a new node with canonical key order (id, label, kind,
  subClassOf, gloss) for clean diffs."
  [node parents]
  (let [o (js-obj)]
    (gobj/set o "id" (:id node))
    (gobj/set o "label" (:label node))
    (gobj/set o "kind" (kw->kind (:kind node)))
    (cond
      (= 1 (count parents)) (gobj/set o "subClassOf" (first parents))
      (seq parents)         (gobj/set o "subClassOf" (clj->js (vec parents))))
    (when-not (str/blank? (:gloss node)) (gobj/set o "gloss" (:gloss node)))
    o))

(defn- apply-adds! [root model adds]
  ;; New nodes are appended to spine.classes by default. If a node
  ;; carries a :props "__container" path hint (set by the canvas "add node" flow),
  ;; that array is used instead.
  (doseq [node adds]
    (let [container (or (get-in node [:props "__container"]) ["spine" "classes"])
          clean     (update node :props dissoc "__container")
          arr       (js-get-in root container)]
      (when (and arr (js/Array.isArray arr))
        (.push arr (node->js clean (g/parents model (:id node))))))))

(defn serialize-model
  "Serialise `model` back to geo JSON. Byte-identical to the source when nothing
  changed; otherwise a minimal in-place patch of the original text."
  [model]
  (let [raw-str (:residual model)
        plan    (compute-plan model)]
    (if (or (nil? raw-str) (empty-plan? plan))
      (or raw-str
          ;; No residual (e.g. a geo model authored from scratch): emit a fresh skeleton.
          (js/JSON.stringify (clj->js {"metadata" {"title" (get-in model [:meta :title])}
                                       "spine" {"classes" []}}) nil 2))
      (let [root (js/JSON.parse raw-str)]
        (doseq [p (:patches plan)] (apply-patch! root p))
        (apply-deletes! root (:deletes plan))
        (apply-adds! root model (:adds plan))
        (js/JSON.stringify root nil 2)))))

;; ---------------------------------------------------------------------------
;; Adapter + detection
;; ---------------------------------------------------------------------------

(defn detect-str
  "Confidence that `raw-str` is a geo reference ontology."
  [raw-str]
  (if-not (fmt/looks-like-json? raw-str)
    0.0
    (let [data (try (js->clj (js/JSON.parse raw-str) :keywordize-keys false)
                    (catch :default _ nil))]
      (if-not (map? data)
        0.0
        (let [has-spine   (contains? data "spine")
              has-modules (contains? data "modules")
              geo-ns      (get-in data ["namespaces" "geo"])
              some-node   (boolean (seq (walk-occurrences data)))]
          (cond
            (and geo-ns (or has-spine has-modules) some-node) 0.95
            (and (or has-spine has-modules) some-node)        0.75
            some-node                                         0.4
            :else                                             0.05))))))

(defrecord GeoFormat []
  fmt/OntologyFormat
  (format-id    [_] :geo-reference-json)
  (display-name [_] "Geo reference ontology (JSON)")
  (detect       [_ raw-str] (detect-str raw-str))
  (parse        [_ raw-str] (parse-str raw-str))
  (serialize    [_ model]   (serialize-model model)))

(defonce ^{:doc "Registers the geo adapter on namespace load."} registered
  (fmt/register! (->GeoFormat)))
