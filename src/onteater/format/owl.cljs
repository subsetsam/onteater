(ns onteater.format.owl
  "Adapter for OWL 2 ontologies serialised in **Turtle** (`.ttl`).

  ## Why Turtle (and not the other OWL syntaxes)

  OWL 2 is an abstract model with several concrete serialisations (RDF/XML, OWL/XML,
  Functional-Style, Manchester, Turtle). Turtle is the most common *readable*
  RDF serialisation of OWL on the web and the one most tools export, yet it is
  tractable to parse by hand: a small tokeniser + recursive-descent parser turns
  it into RDF triples with no external library and no DOM — so the whole thing runs
  headless under `:node-test`, exactly like the geo and native adapters.

  ## Fidelity model

  Onteater has three fidelity tiers (see `onteater.format.core`): geo is *byte
  perfect* via a residual+diff; native is *lossless* over the whole canonical
  model. OWL sits in between — a faithful **importer/exporter over a modelled
  subset**:

    Modelled as first-class (become nodes/edges/labels the UI can edit):
      - Declaration of Class / ObjectProperty / DatatypeProperty /
        AnnotationProperty / NamedIndividual        -> a node (`:kind`)
      - rdfs:subClassOf  <iri>                        -> :subclass-of edge
      - rdfs:subPropertyOf <iri>                      -> :subproperty-of edge
      - rdfs:domain / rdfs:range <iri>                -> :domain / :range edge
      - rdf:type <user-class> (on an individual)      -> :instance-of edge
      - rdfs:label / rdfs:comment                     -> :label / :gloss

    Everything else — anonymous class expressions (`owl:Restriction` blank nodes),
    RDF lists, property characteristics, ontology imports, custom annotation
    predicates, … — is preserved verbatim as a `:residual` block of Turtle and
    re-emitted on save, so a round-trip never silently drops axioms it does not
    model. Prefixes, the base IRI and each entity's exact declaration types are
    remembered under `[:meta :onteater/owl]` so OWL->OWL is stable.

  Because unmodelled axioms are carried as opaque text keyed by the *original*
  IRIs, renaming a node that such an axiom refers to will not rewrite the axiom;
  the native format remains the lossless escape hatch for heavy restructuring.

  ## Export from any format

  `serialize` works on *any* canonical model, not just OWL-originated ones: when
  the `[:meta :onteater/owl]` stash is absent it synthesises prefixes from
  `[:meta :namespaces]`, a default declaration type per node `:kind`, and an
  ontology IRI from the title. That is what powers File -> Export (OWL2)."
  (:require [clojure.string :as str]
            [onteater.model.graph :as g]
            [onteater.format.core :as fmt]))

;; ---------------------------------------------------------------------------
;; Well-known vocabularies
;; ---------------------------------------------------------------------------

(def ^:private rdf  "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(def ^:private rdfs "http://www.w3.org/2000/01/rdf-schema#")
(def ^:private owl  "http://www.w3.org/2002/07/owl#")
(def ^:private xsd  "http://www.w3.org/2001/XMLSchema#")

(def ^:private rdf:type       (str rdf "type"))
(def ^:private rdf:first      (str rdf "first"))
(def ^:private rdf:rest       (str rdf "rest"))
(def ^:private rdf:nil        (str rdf "nil"))
(def ^:private rdfs:label     (str rdfs "label"))
(def ^:private rdfs:comment   (str rdfs "comment"))
(def ^:private rdfs:subClassOf    (str rdfs "subClassOf"))
(def ^:private rdfs:subPropertyOf (str rdfs "subPropertyOf"))
(def ^:private rdfs:domain    (str rdfs "domain"))
(def ^:private rdfs:range     (str rdfs "range"))
(def ^:private owl:Ontology   (str owl "Ontology"))
(def ^:private owl:versionInfo (str owl "versionInfo"))
(def ^:private xsd:string     (str xsd "string"))

;; The default prefix table an RDF/OWL document may rely on without declaring.
(def ^:private default-prefixes
  {"rdf" rdf "rdfs" rdfs "owl" owl "xsd" xsd})

;; rdf:type object IRI -> node :kind for the OWL entity declarations we model.
(def ^:private entity-type->kind
  {(str owl "Class")              :class
   (str rdfs "Class")             :class
   (str owl "ObjectProperty")     :property
   (str owl "DatatypeProperty")   :property
   (str owl "AnnotationProperty") :property
   (str rdf "Property")           :property
   (str owl "NamedIndividual")    :individual})

;; :kind -> the declaration type IRI to emit when a model has no remembered
;; declaration (i.e. exporting a non-OWL model to OWL).
(def ^:private kind->default-type
  {:class      (str owl "Class")
   :property   (str owl "ObjectProperty")
   :individual (str owl "NamedIndividual")})

;; RDF predicate -> model edge :type for the relations we round-trip. (The reverse
;; direction is written out with fixed curies in `node->block`, since rdfs:/owl:
;; are always declared.)
(def ^:private pred->edge-type
  {rdfs:subClassOf    :subclass-of
   rdfs:subPropertyOf :subproperty-of
   rdfs:domain        :domain
   rdfs:range         :range})

;; ===========================================================================
;; Tokeniser
;; ===========================================================================
;;
;; Turtle is scanned with a set of *sticky* (`y`-flag) regexes: each is anchored
;; at the current index so there is no substring copying and no O(n^2) rescan on
;; large files. Whitespace and `# comments` are trivia between tokens.

(defn- re [pat flags] (js/RegExp. pat flags))

(def ^:private re-ws       (re "[ \\t\\r\\n]+" "y"))
(def ^:private re-comment  (re "#[^\\n\\r]*" "y"))
(def ^:private re-iri      (re "<([^>]*)>" "y"))
(def ^:private re-strlongd (re "\"\"\"([\\s\\S]*?)\"\"\"" "y"))
(def ^:private re-strlongs (re "'''([\\s\\S]*?)'''" "y"))
(def ^:private re-strd     (re "\"((?:[^\"\\\\]|\\\\.)*)\"" "y"))
(def ^:private re-strs     (re "'((?:[^'\\\\]|\\\\.)*)'" "y"))
(def ^:private re-lang     (re "@([a-zA-Z]+(?:-[a-zA-Z0-9]+)*)" "y"))
(def ^:private re-dir      (re "@(prefix|base)\\b" "y"))
(def ^:private re-sparql   (re "(PREFIX|BASE)\\b" "iy"))
(def ^:private re-number   (re "[+-]?(?:\\d+\\.\\d+|\\.\\d+|\\d+)(?:[eE][+-]?\\d+)?" "y"))
(def ^:private re-bool     (re "(true|false)\\b" "y"))
(def ^:private re-bnode    (re "_:([A-Za-z0-9_][A-Za-z0-9_.\\-]*)" "y"))
(def ^:private re-pname    (re "([A-Za-z][A-Za-z0-9._\\-]*)?:([A-Za-z0-9_%][A-Za-z0-9_.\\-%]*)?" "y"))
(def ^:private re-a        (re "a(?=[\\s;,.\\]\\)]|$)" "y"))
(def ^:private re-dt       (re "\\^\\^" "y"))

(defn- sticky
  "Try `regex` anchored exactly at index `i` in `s`. Return the JS match object on
  success (with the regex's `lastIndex` advanced past it), else nil."
  [regex s i]
  (set! (.-lastIndex regex) i)
  (let [m (.exec regex s)]
    (when (and m (= (.-index m) i)) m)))

(defn- parse-error [msg]
  (throw (ex-info "This does not look like a valid Turtle/OWL file."
                  {:message (str "Turtle parse error: " msg)})))

(defn- skip-trivia
  "Advance past any whitespace and comments starting at `i`."
  [s i n]
  (loop [i i]
    (cond
      (>= i n) i
      (sticky re-ws s i)      (recur (.-lastIndex re-ws))
      (sticky re-comment s i) (recur (.-lastIndex re-comment))
      :else i)))

(defn- unescape
  "Resolve Turtle string escapes (\\n \\t \\\" \\\\ \\uXXXX …) to their characters."
  [s]
  (-> s
      (str/replace #"\\u([0-9A-Fa-f]{4})"
                   (fn [[_ h]] (js/String.fromCharCode (js/parseInt h 16))))
      (str/replace #"\\U([0-9A-Fa-f]{8})"
                   (fn [[_ h]] (js/String.fromCodePoint (js/parseInt h 16))))
      (str/replace #"\\(.)"
                   (fn [[_ c]]
                     (case c "n" "\n" "t" "\t" "r" "\r" "b" "\b" "f" "\f" c)))))

(defn- read-string-token
  "Read a string literal at `i` (any of the four Turtle quote forms) together with
  an immediately-following language tag or `^^datatype`. Returns `[token next-i]`."
  [s i]
  (let [try-quote (fn [regex] (when-let [m (sticky regex s i)]
                                [(unescape (aget m 1)) (.-lastIndex regex)]))
        [v after] (or (try-quote re-strlongd) (try-quote re-strlongs)
                      (try-quote re-strd) (try-quote re-strs)
                      (parse-error "unterminated string literal"))]
    (if-let [lm (sticky re-lang s after)]
      [{:k :lit :v v :lang (aget lm 1)} (.-lastIndex re-lang)]
      (if (sticky re-dt s after)
        (let [j (.-lastIndex re-dt)]
          (cond
            (sticky re-iri s j)
            (let [m (sticky re-iri s j)]
              [{:k :lit :v v :dt {:iri (aget m 1)}} (.-lastIndex re-iri)])
            (sticky re-pname s j)
            (let [m (sticky re-pname s j)]
              [{:k :lit :v v :dt {:prefix (or (aget m 1) "") :local (or (aget m 2) "")}}
               (.-lastIndex re-pname)])
            :else (parse-error "malformed datatype after ^^")))
        [{:k :lit :v v} after]))))

(defn- lex1
  "Produce one token starting at `i` (trivia already skipped). Returns `[token next-i]`."
  [s i]
  (let [c (.charAt s i)]
    (cond
      (= c "<")
      (if-let [m (sticky re-iri s i)]
        [{:k :iri :v (aget m 1)} (.-lastIndex re-iri)]
        (parse-error "unterminated IRI"))

      (or (= c "\"") (= c "'")) (read-string-token s i)

      (= c "_")
      (if-let [m (sticky re-bnode s i)]
        [{:k :bnode :v (aget m 1)} (.-lastIndex re-bnode)]
        (parse-error "malformed blank node"))

      (= c "@")
      (if-let [m (sticky re-dir s i)]
        [{:k :dir :v (aget m 1)} (.-lastIndex re-dir)]
        (parse-error "unknown @directive"))

      (contains? #{"." ";" "," "[" "]" "(" ")"} c)
      [{:k :punc :v c} (inc i)]

      :else
      (or
       (when-let [m (sticky re-sparql s i)]
         [{:k :dir :v (str/lower-case (aget m 1))} (.-lastIndex re-sparql)])
       (when-let [m (sticky re-number s i)]
         [{:k :lit :v (aget m 0) :numeric true} (.-lastIndex re-number)])
       (when-let [m (sticky re-bool s i)]
         [{:k :lit :v (aget m 1) :numeric true} (.-lastIndex re-bool)])
       (when (sticky re-a s i)
         [{:k :a} (.-lastIndex re-a)])
       (when-let [m (sticky re-pname s i)]
         [{:k :pname :prefix (or (aget m 1) "") :local (or (aget m 2) "")}
          (.-lastIndex re-pname)])
       (parse-error (str "unexpected character '" c "'"))))))

(defn- tokenize [s]
  (let [n (count s)]
    (loop [i (skip-trivia s 0 n) toks (transient [])]
      (if (>= i n)
        (persistent! toks)
        (let [[tok ni] (lex1 s i)]
          (recur (skip-trivia s ni n) (conj! toks tok)))))))

;; ===========================================================================
;; Parser: tokens -> {:triples [...] :prefixes {..} :base str}
;; ===========================================================================
;;
;; A triple is {:s term :p iri-string :o term}. A term is one of
;;   {:t :iri   :v absolute-iri}
;;   {:t :bnode :v id}
;;   {:t :lit   :v str :lang lang? :dt iri? :numeric bool?}
;; Anonymous `[ ... ]` property lists and `( ... )` collections are desugared into
;; fresh blank nodes + their triples, so the output is a flat triple list.

(defn- absolute-iri? [v]
  (boolean (re-find #"^[A-Za-z][A-Za-z0-9+.\-]*:" v)))

(defn- parse-tokens [toks]
  (let [pos      (atom 0)
        prefixes (atom default-prefixes)
        base     (atom nil)
        triples  (atom (transient []))
        bnc      (atom 0)]
    (letfn [(peek1 [] (nth toks @pos nil))
            (adv []   (let [t (nth toks @pos nil)] (swap! pos inc) t))
            (punc? [t v] (and t (= :punc (:k t)) (= v (:v t))))
            (dot-next? [] (punc? (peek1) "."))
            (emit [s p o] (swap! triples conj! {:s s :p p :o o}))
            (fresh-bnode [] {:t :bnode :v (str "g" (swap! bnc inc))})

            (resolve-iri [v] (if (absolute-iri? v) v (str (or @base "") v)))

            (resolve-pname [prefix local]
              (if-let [u (get @prefixes prefix)]
                (str u local)
                ;; Unknown prefix: mint a synthetic namespace so the term still
                ;; resolves and round-trips as a curie rather than being lost.
                (let [u (str "http://onteater.local/ns/"
                             (if (seq prefix) prefix "_") "#")]
                  (swap! prefixes assoc prefix u)
                  (str u local))))

            (dt-iri [dt]
              (when dt
                (if (:iri dt)
                  (resolve-iri (:iri dt))
                  (resolve-pname (:prefix dt) (:local dt)))))

            (token->term [t]
              (case (:k t)
                :iri   {:t :iri :v (resolve-iri (:v t))}
                :a     {:t :iri :v rdf:type}
                :pname {:t :iri :v (resolve-pname (:prefix t) (:local t))}
                :bnode {:t :bnode :v (str "u_" (:v t))} ; namespace user labels
                :lit   (cond-> {:t :lit :v (:v t)}
                         (:lang t)    (assoc :lang (:lang t))
                         (:numeric t) (assoc :numeric true)
                         (:dt t)      (assoc :dt (dt-iri (:dt t))))
                (parse-error (str "unexpected token " (pr-str t)))))

            (verb-start? [t]
              (and t (contains? #{:a :iri :pname} (:k t))))

            (parse-verb []
              (let [t (adv)]
                (case (:k t)
                  :a     rdf:type
                  :iri   (resolve-iri (:v t))
                  :pname (resolve-pname (:prefix t) (:local t))
                  (parse-error "expected a predicate"))))

            (parse-object []
              (let [t (peek1)]
                (cond
                  (punc? t "[") (blank-property-list)
                  (punc? t "(") (collection)
                  (nil? t)      (parse-error "unexpected end of input in object")
                  :else         (token->term (adv)))))

            (object-list [subj verb]
              (emit subj verb (parse-object))
              (loop []
                (when (punc? (peek1) ",")
                  (adv)
                  (emit subj verb (parse-object))
                  (recur))))

            (predicate-object-list [subj]
              (when (verb-start? (peek1))
                (object-list subj (parse-verb))
                (loop []
                  (when (punc? (peek1) ";")
                    (adv)                         ; consume ';'
                    (when (verb-start? (peek1))   ; tolerate a trailing ';'
                      (object-list subj (parse-verb))
                      (recur))))))

            (blank-property-list []
              (adv)                               ; consume '['
              (let [b (fresh-bnode)]
                (predicate-object-list b)
                (if (punc? (peek1) "]")
                  (adv)
                  (parse-error "expected ']'"))
                b))

            (collection []
              (adv)                               ; consume '('
              (loop [items []]
                (cond
                  (punc? (peek1) ")") (do (adv)
                                          (if (empty? items)
                                            {:t :iri :v rdf:nil}
                                            (build-list items)))
                  (nil? (peek1))      (parse-error "unterminated collection")
                  :else               (recur (conj items (parse-object))))))

            (build-list [items]
              ;; items -> rdf:first/rdf:rest chain terminated by rdf:nil
              (let [cells (mapv (fn [_] (fresh-bnode)) items)]
                (dotimes [k (count items)]
                  (emit (nth cells k) rdf:first (nth items k))
                  (emit (nth cells k) rdf:rest
                        (if (< (inc k) (count items))
                          (nth cells (inc k))
                          {:t :iri :v rdf:nil})))
                (first cells)))

            (parse-directive []
              (let [d (adv)]                       ; :dir "prefix" | "base"
                (if (= "prefix" (:v d))
                  (let [pn  (adv)                  ; pname token "pfx:" (local empty)
                        iri (adv)]
                    (when-not (= :pname (:k pn)) (parse-error "malformed @prefix name"))
                    (when-not (= :iri (:k iri))  (parse-error "malformed @prefix IRI"))
                    (swap! prefixes assoc (:prefix pn) (resolve-iri (:v iri))))
                  (let [iri (adv)]
                    (when-not (= :iri (:k iri)) (parse-error "malformed @base IRI"))
                    (reset! base (resolve-iri (:v iri)))))
                (when (dot-next?) (adv))))         ; '.' present for @-form, absent for SPARQL

            (parse-statement []
              (let [subj (parse-object)]           ; subject (never a literal in valid input)
                (predicate-object-list subj)
                (if (dot-next?)
                  (adv)
                  (parse-error "expected '.' at end of statement"))))]

      (loop []
        (when-let [t (peek1)]
          (if (= :dir (:k t))
            (parse-directive)
            (parse-statement))
          (recur)))

      {:triples  (persistent! @triples)
       :prefixes @prefixes
       :base     @base})))

;; ===========================================================================
;; Triples -> canonical model
;; ===========================================================================

(defn- iri-term? [t] (= :iri (:t t)))
(defn- term-key [t] (str (name (:t t)) "␟" (:v t)))
(defn- local-name
  "Last path segment of an IRI (after the final # or /), for a fallback label."
  [iri]
  (let [seg (last (str/split iri #"[#/]"))]
    (if (str/blank? seg) iri seg)))

(defn- best-prefix
  "Longest declared prefix whose namespace IRI is a prefix of `iri` and leaves a
  simple local part. Returns [prefix local] or nil."
  [iri prefixes]
  (->> prefixes
       (keep (fn [[p u]]
               (when (and (seq u) (str/starts-with? iri u))
                 (let [local (subs iri (count u))]
                   (when (re-matches #"[A-Za-z0-9_][A-Za-z0-9_.\-]*" local)
                     [(count u) p local])))))
       (sort-by first >)
       first
       (drop 1)
       vec))

(defn- iri->id
  "Abbreviate a full IRI to a `prefix:local` node id when a declared prefix fits;
  otherwise keep the full IRI (angle-wrapped on output)."
  [iri prefixes]
  (if-let [[p local] (best-prefix iri prefixes)]
    (str p ":" local)
    iri))

(defn- id-module
  "The grouping module for an id: its curie prefix, or nil for a bare IRI."
  [id]
  (when-let [m (re-matches #"([A-Za-z][A-Za-z0-9._\-]*):.*" id)]
    (let [p (second m)]
      (when-not (str/includes? id "//") p))))

;; --- residual (unmodelled triples) rendering -------------------------------

(defn- escape-literal [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")
      (str/replace "\t" "\\t")))

(defn- iri->turtle [iri prefixes]
  (cond
    (= iri rdf:type) "a"
    :else (if-let [[p local] (best-prefix iri prefixes)]
            (str p ":" local)
            (str "<" iri ">"))))

(defn- id->turtle
  "Render a *node id* (which may already be a `prefix:local` curie or a full IRI)
  as a Turtle term. A curie with a declared prefix is emitted verbatim; a full IRI
  is abbreviated or angle-wrapped."
  [id prefixes]
  (let [m (id-module id)]
    (cond
      (and m (contains? prefixes m)) id
      (absolute-iri? id)             (iri->turtle id prefixes)
      :else                          (str "<" id ">"))))

(defn- lit->turtle [{:keys [v lang dt numeric]} prefixes]
  (cond
    numeric v
    :else   (let [q (str "\"" (escape-literal v) "\"")]
              (cond
                lang (str q "@" lang)
                (and dt (not= dt xsd:string)) (str q "^^" (iri->turtle dt prefixes))
                :else q))))

(defn- term->turtle [t prefixes]
  (case (:t t)
    :iri   (iri->turtle (:v t) prefixes)
    :bnode (str "_:" (:v t))
    :lit   (lit->turtle t prefixes)))

(defn- triples->turtle
  "Render leftover triples as flat `s p o .` Turtle lines (indented two spaces)."
  [ts prefixes]
  (->> ts
       (map (fn [{:keys [s p o]}]
              (str "  " (term->turtle s prefixes) " "
                   (iri->turtle p prefixes) " "
                   (term->turtle o prefixes) " .")))
       (str/join "\n")))

;; --- the build ------------------------------------------------------------

(defn- build-model [{:keys [triples prefixes base]}]
  (let [indexed  (map-indexed (fn [i t] (assoc t :i i)) triples)
        by-subj  (group-by (comp term-key :s) indexed)
        ;; subject-key -> the subject term (any representative)
        subj-of  (into {} (map (fn [[k ts]] [k (:s (first ts))])) by-subj)

        ;; ontology header subject(s): typed owl:Ontology.
        ont-key  (some (fn [[k ts]]
                         (when (some #(and (= rdf:type (:p %))
                                           (iri-term? (:o %))
                                           (= owl:Ontology (:v (:o %)))) ts)
                           k))
                       by-subj)
        ont-term (get subj-of ont-key)
        ont-iri  (when (and ont-term (iri-term? ont-term)) (:v ont-term))
        ont-ts   (get by-subj ont-key)
        ont-title   (some (fn [{:keys [p o]}] (when (and (= rdfs:label p) (= :lit (:t o))) (:v o))) ont-ts)
        ont-version (some (fn [{:keys [p o]}] (when (and (= owl:versionInfo p) (= :lit (:t o))) (:v o))) ont-ts)

        consumed (atom (transient #{}))
        eat!     (fn [i] (swap! consumed conj! i))

        ;; The ontology header's modelled triples (type, the used label, the used
        ;; version) are regenerated on save, so consume them here; any *other*
        ;; ontology-level triple (owl:imports, custom annotations) stays in residual.
        _ (doseq [{:keys [i p o]} ont-ts]
            (when (or (and (= rdf:type p) (iri-term? o) (= owl:Ontology (:v o)))
                      (and (= rdfs:label p) (= :lit (:t o)) (= (:v o) ont-title))
                      (and (= owl:versionInfo p) (= :lit (:t o)) (= (:v o) ont-version)))
              (eat! i)))

        ;; Decide the entity kind for an IRI subject and gather its facts.
        subjects (for [[k ts] by-subj
                       :let [s (get subj-of k)]
                       :when (and (iri-term? s) (not= k ont-key))]
                   [k s ts])

        classify (fn [ts]
                   (let [types (into #{} (comp (filter #(= rdf:type (:p %)))
                                               (filter (comp iri-term? :o))
                                               (map (comp :v :o))) ts)
                         has?  (fn [p] (some #(= p (:p %)) ts))
                         kinds (into #{} (keep entity-type->kind) types)]
                     (cond
                       (kinds :class)      :class
                       (kinds :property)   :property
                       (kinds :individual) :individual
                       ;; typed only by a user class -> an individual
                       (some (complement entity-type->kind) types) :individual
                       (has? rdfs:subClassOf)  :class
                       (or (has? rdfs:domain) (has? rdfs:range)
                           (has? rdfs:subPropertyOf)) :property
                       :else nil)))

        acc (reduce
             (fn [{:keys [nodes edges decl] :as a} [_ s ts]]
               (if-let [kind (classify ts)]
                 (let [iri     (:v s)
                       id      (iri->id iri prefixes)
                       ;; first label / comment are consumed; extras fall to residual
                       label-t (some (fn [{:keys [i p o]}]
                                       (when (and (= rdfs:label p) (= :lit (:t o))) [i (:v o)])) ts)
                       gloss-t (some (fn [{:keys [i p o]}]
                                       (when (and (= rdfs:comment p) (= :lit (:t o))) [i (:v o)])) ts)
                       _ (when label-t (eat! (first label-t)))
                       _ (when gloss-t (eat! (first gloss-t)))

                       ;; declaration types (builtin entity types) — consumed
                       decl-types (reduce (fn [dt {:keys [i p o]}]
                                            (if (and (= rdf:type p) (iri-term? o)
                                                     (entity-type->kind (:v o)))
                                              (do (eat! i) (conj dt (:v o)))
                                              dt))
                                          [] ts)

                       ;; edges from modelled predicates with IRI objects — consumed
                       new-edges
                       (reduce
                        (fn [es {:keys [i p o]}]
                          (cond
                            ;; rdf:type -> a *user* class => :instance-of edge
                            (and (= rdf:type p) (iri-term? o)
                                 (not (entity-type->kind (:v o))))
                            (do (eat! i)
                                (conj es (g/make-edge id :instance-of (iri->id (:v o) prefixes))))

                            (and (pred->edge-type p) (iri-term? o))
                            (do (eat! i)
                                (conj es (g/make-edge id (pred->edge-type p) (iri->id (:v o) prefixes))))

                            :else es))
                        [] ts)

                       node {:id id :label (or (second label-t) (local-name iri))
                             :kind kind :gloss (second gloss-t) :props {}
                             :module (id-module id) :external? false :provenance []}]
                   {:nodes (assoc nodes id node)
                    :edges (into edges (map (juxt :id identity)) new-edges)
                    :decl  (assoc decl id decl-types)})
                 a))
             {:nodes {} :edges {} :decl {}}
             subjects)

        real-nodes (:nodes acc)
        edges      (:edges acc)
        decl-types (:decl acc)
        node-ids   (set (keys real-nodes))

        ;; external stubs for any edge endpoint not declared as a node
        endpoints  (into #{} (mapcat (fn [e] [(:source e) (:target e)])) (vals edges))
        external   (into {}
                         (comp (remove node-ids)
                               (map (fn [id]
                                      [id {:id id :label (local-name id) :kind :class
                                           :gloss nil :props {} :module "external"
                                           :external? true :provenance []}])))
                         endpoints)
        nodes      (merge real-nodes external)

        ;; groups: one section per module (curie prefix) listing its members
        groups (reduce (fn [gs [id node]]
                         (if-let [m (:module node)]
                           (update gs m (fn [g]
                                          (-> (or g {:id m :label (str m ": " (get prefixes m m))
                                                     :kind :section :parent nil
                                                     :subgroups #{} :members []})
                                              (update :members conj id))))
                           gs))
                       {}
                       real-nodes)

        ;; leftover triples -> residual Turtle (unmodelled axioms preserved)
        cons-set   (persistent! @consumed)
        residual-ts (remove #(cons-set (:i %)) indexed)
        residual   (when (seq residual-ts) (triples->turtle residual-ts prefixes))]

    {:meta   {:title      (or ont-title "OWL Ontology")
              :version    ont-version
              :namespaces prefixes
              :format     :owl2-turtle
              :onteater/owl {:prefixes prefixes
                             :base base
                             :ontology-iri ont-iri
                             :decl-types decl-types}}
     :nodes  nodes
     :edges  edges
     :groups groups
     :residual residual
     :order  (vec (keys real-nodes))}))

(defn parse-str
  "Parse OWL-in-Turtle text into a canonical model. Throws a user-readable
  ex-info on malformed input."
  [raw-str]
  (build-model (parse-tokens (tokenize (or raw-str "")))))

;; ===========================================================================
;; Model -> Turtle
;; ===========================================================================

(defn- slugify [s]
  (-> (or s "ontology") str/lower-case (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-+|-+$" "")))

(defn- ensure-prefixes
  "Guarantee every curie prefix used by node ids (and the well-known ones) has a
  namespace declared, minting synthetic namespaces for any that don't. Returns the
  augmented prefix map."
  [prefixes model]
  (let [used (into #{} (keep id-module) (keys (:nodes model)))]
    (reduce (fn [pm p]
              (if (contains? pm p)
                pm
                (assoc pm p (str "http://onteater.local/ns/" p "#"))))
            (merge default-prefixes prefixes)
            used)))

(defn- node->block
  "Render one node as a Turtle statement (a `;`-separated predicate list ending
  in ` .`). Returns nil for external stubs (referenced-but-undeclared)."
  [model prefixes decl-types node]
  (when-not (:external? node)
    (let [id       (:id node)
          subj     (id->turtle id prefixes)
          out      (g/out-edges model id)
          by-type  (group-by :type out)
          ;; rdf:type objects: remembered declaration types + instance-of targets
          decl     (if (contains? decl-types id)
                     (get decl-types id)
                     [(kind->default-type (:kind node))])
          decl     (remove nil? decl)
          inst     (map :target (get by-type :instance-of))
          type-strs (concat (map #(iri->turtle % prefixes) decl)
                            (map #(id->turtle % prefixes) inst))
          ;; ordered (predicate, [object-strings]) pairs
          rel-strs (fn [etype]
                     (map #(id->turtle (:target %) prefixes) (get by-type etype)))
          pairs (cond-> []
                  (seq type-strs)
                  (conj ["a" type-strs])
                  (seq (:label node))
                  (conj ["rdfs:label" [(lit->turtle {:t :lit :v (:label node)} prefixes)]])
                  (seq (rel-strs :subclass-of))
                  (conj ["rdfs:subClassOf" (rel-strs :subclass-of)])
                  (seq (rel-strs :subproperty-of))
                  (conj ["rdfs:subPropertyOf" (rel-strs :subproperty-of)])
                  (seq (rel-strs :domain))
                  (conj ["rdfs:domain" (rel-strs :domain)])
                  (seq (rel-strs :range))
                  (conj ["rdfs:range" (rel-strs :range)])
                  (not (str/blank? (:gloss node)))
                  (conj ["rdfs:comment" [(lit->turtle {:t :lit :v (:gloss node)} prefixes)]]))]
      (when (seq pairs)
        (let [body (->> pairs
                        (map (fn [[p objs]] (str p " " (str/join " , " objs))))
                        (str/join " ;\n    "))]
          (str subj " " body " ."))))))

(defn- prefix-lines [prefixes]
  (->> prefixes
       (sort-by first)
       (map (fn [[p u]] (str "@prefix " p ": <" u "> .")))
       (str/join "\n")))

(defn serialize-model
  "Serialise any canonical `model` to OWL-in-Turtle text. When the model came from
  this adapter, prefixes/base/declaration-types and the residual axiom block are
  reproduced; otherwise sensible defaults are synthesised (the Export path)."
  [model]
  (let [owl-meta   (get-in model [:meta :onteater/owl])
        prefixes   (ensure-prefixes (merge (get-in model [:meta :namespaces] {})
                                           (:prefixes owl-meta))
                                    model)
        decl-types (:decl-types owl-meta)
        title      (get-in model [:meta :title])
        ont-iri    (or (:ontology-iri owl-meta)
                       (str "http://onteater.local/ontology/" (slugify title)))
        version    (get-in model [:meta :version])
        header     (str "<" ont-iri "> a owl:Ontology"
                        (when (seq title)   (str " ;\n    rdfs:label \"" (escape-literal title) "\""))
                        (when (seq version) (str " ;\n    owl:versionInfo \"" (escape-literal version) "\""))
                        " .")
        ordered    (let [ord (:order model)
                         seen (set ord)]
                     (concat (keep #(get-in model [:nodes %]) ord)
                             (remove #(seen (:id %)) (g/nodes model))))
        blocks     (keep #(node->block model prefixes decl-types %) ordered)
        residual   (when owl-meta (:residual model))]
    (str/join "\n"
              (remove nil?
                      [(prefix-lines prefixes)
                       ""
                       header
                       ""
                       (str/join "\n\n" blocks)
                       (when (and residual (seq (str/trim residual)))
                         (str "\n# --- preserved axioms (restrictions, lists, annotations) ---\n"
                              residual))]))))

;; ===========================================================================
;; Detection + adapter registration
;; ===========================================================================

(defn detect-str
  "Confidence that `raw-str` is an OWL/Turtle document. Zero for JSON (so the geo
  and native adapters win their own files)."
  [raw-str]
  (let [s (or raw-str "")]
    (if (fmt/looks-like-json? s)
      0.0
      (let [has-prefix (boolean (re-find #"(?i)@prefix|@base|\bPREFIX\b|\bBASE\b" s))
            has-owl    (boolean (re-find #"owl:|www\.w3\.org/2002/07/owl|rdfs:subClassOf|rdf:type" s))]
        (cond
          (and has-prefix has-owl) 0.95
          has-prefix               0.6
          has-owl                  0.5
          :else                    0.0)))))

(defrecord OwlTurtleFormat []
  fmt/OntologyFormat
  (format-id    [_] :owl2-turtle)
  (display-name [_] "OWL 2 ontology (Turtle)")
  (detect       [_ raw-str] (detect-str raw-str))
  (parse        [_ raw-str] (parse-str raw-str))
  (serialize    [_ model]   (serialize-model model)))

(defonce ^{:doc "Registers the OWL/Turtle adapter on namespace load."} registered
  (fmt/register! (->OwlTurtleFormat)))
