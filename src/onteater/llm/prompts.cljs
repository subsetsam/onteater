(ns onteater.llm.prompts
  "Prompt construction and response parsing for the mapping pipeline.
  Pure functions only — no LLM calls, no DOM — so the compaction budgeting, the
  chunking, and especially the response validation are all unit-tested against
  fixtures without a live model (never trust LLM output).

  Pipeline:
    1. `compact-ontology` — the full JSON is far too large/noisy for a prompt, so
       build a compact one-line-per-node schema view grouped by module.
    2. `chunk-scenario` — split oversized scenarios on Markdown/paragraph
       boundaries with overlap; entries are merged downstream.
    3. `mapping-messages` + `mapping-schema` — a /api/chat request constrained to a
       JSON schema (Ollama structured outputs), with the user's forced entries fed
       back as hard constraints.
    4. `parse-response` + `validate-entries` — turn the model's JSON into entries
       and flag every unknown node id / unlocatable excerpt."
  (:require [clojure.string :as str]
            [onteater.model.graph :as g]
            [onteater.model.mapping :as mapping]
            [onteater.model.timeline :as timeline]))

;; --- token budgeting --------------------------------------------------------

(defn estimate-tokens
  "Very rough token estimate (~4 chars/token). Good enough to decide whether to
  fall back to two-stage mapping or to chunk the scenario."
  [s]
  (quot (count (or s "")) 4))

;; --- ontology compaction ----------------------------------------------------
;;
;; The compaction is the schema view the mapping LLM sees; its job is to render
;; the ontology's *shape* — the subclass tree (indentation), the module/subsection
;; organization (real group titles), the upper-level spine first — not a flat
;; line-per-node dump. Glosses are truncated on sentence boundaries with a tiered
;; budget: interior classes orient (first sentence), leaves discriminate (both
;; sentences), scoped views (`:include-ids` + `:full-gloss?`) get everything.

(defn sentence-truncate
  "Truncate `s` to at most `n` chars, cutting at the last sentence boundary past
  n/3 when one exists; otherwise a hard cut with an ellipsis. Authoring guidance
  lives on the other side of this fn: a gloss's FIRST sentence should discriminate
  on its own, because it is what survives every rendering."
  [s n]
  (let [s (str/trim (str s))]
    (if (<= (count s) n)
      s
      (let [head (subs s 0 n)
            cut  (str/last-index-of head ". ")]
        (if (and cut (>= cut (quot n 3)))
          (subs head 0 (inc cut))
          (str (str/trimr (subs head 0 (dec n))) "…"))))))

(defn- node-gloss
  "A node's gloss under the tiered budget: full in scoped views, first-sentence-ish
  for interior classes (they orient; the subtree below discriminates), longer for
  leaves (where the mapping decision actually lands)."
  [model {:keys [id gloss]} {:keys [interior-gloss leaf-gloss full-gloss?]}]
  (when (seq gloss)
    (cond
      full-gloss?                     gloss
      (seq (g/children model id))     (sentence-truncate gloss interior-gloss)
      :else                           (sentence-truncate gloss leaf-gloss))))

(defn- forest-lines
  "Render the class `ids` as an indented subclass forest (2 spaces per level),
  ordered roots-first then depth-first. A node with several included parents
  renders once, under the alphabetically-first one, its other parents noted as
  `⊂ …`; parents outside `ids` (e.g. external bfo:* stubs, or classes in another
  subsection) are always noted so no line loses its place in the hierarchy.
  Non-class kinds (individuals, values) are tagged so the LLM can tell them from
  classes. `(:hidden-children opts)` maps boundary ids to a count of omitted
  descendants (the coarse view's `(+N more …)` marker). Cycle-safe AND
  cycle-complete: members of a subclass cycle have no in-set root, so after the
  walk any unvisited id is appended as a flat line — the compaction must never
  lose a node (graph/validate merely warns about cycles, it doesn't fix them)."
  [model ids opts]
  (let [include (set ids)
        kids-of (fn [id] (sort (filter include (g/children model id))))
        prim    (fn [id] (first (sort (filter include (g/parents model id)))))
        roots   (sort (filter #(nil? (prim %)) include))
        line    (fn [id depth]
                  (let [n       (g/node model id)
                        parents (sort (g/parents model id))
                        noted   (if (zero? depth) parents (remove #{(prim id)} parents))
                        extra   (when (seq noted) (str "⊂ " (str/join ", " noted)))
                        kind    (:kind n)
                        tag     (when (and kind (not (#{:class :property} kind)))
                                  (name kind))
                        hidden  (get (:hidden-children opts) id)
                        more    (when hidden (str "(+" hidden " more specific subclasses not shown)"))]
                    (str (apply str (repeat depth "  "))
                         (str/join " | " (remove nil? [id (:label n) tag extra
                                                       (node-gloss model n opts) more])))))]
    (letfn [(walk [[seen out] id depth]
              (if (or (seen id) (> depth 14))
                [seen out]
                (reduce (fn [acc c] (walk acc c (inc depth)))
                        [(conj seen id) (conj out (line id depth))]
                        (kids-of id))))]
      (let [[seen out] (reduce (fn [acc r] (walk acc r 0)) [#{} []] roots)
            orphans    (sort (remove seen include))]
        (into out (map #(line % 0)) orphans)))))

(defn- section-plan
  "Organize the model's classes for rendering: ordered sections `{:id :label
  :spine? :ids :sub-blocks [{:label :ids}]}`, derived from the model's groups
  (real module/subsection titles) with a plain `:module` grouping fallback for
  models that carry no groups (owl/native). Spine-ish sections sort first —
  the upper level is the reader's orientation."
  [model classes]
  (let [class-ids (into #{} (map :id) classes)
        sections  (filter #(nil? (:parent %)) (g/groups model))
        by-gid    (into {} (map (juxt :id identity)) (g/groups model))
        spine-re  #"(?i)spine|upper"]
    (if (empty? sections)
      (->> classes
           (group-by :module)
           (sort-by (fn [[m _]] (or m "~")))
           (mapv (fn [[m ns]] {:id (or m "—") :label (str "Module " (or m "—"))
                               :spine? false :ids (mapv :id (sort-by :id ns)) :sub-blocks []})))
      (let [planned
            (for [sec (sort-by :id sections)
                  :let [subs      (keep by-gid (sort (:subgroups sec)))
                        sub-blocks (for [sg subs
                                         :let [ids (filterv class-ids (:members sg))]
                                         :when (seq ids)]
                                     {:label (:label sg) :ids ids})
                        covered   (into #{} (mapcat :ids) sub-blocks)
                        rest-ids  (filterv (every-pred class-ids (complement covered))
                                           (:members sec))
                        sub-blocks (cond-> (vec sub-blocks)
                                     (and (seq sub-blocks) (seq rest-ids))
                                     (conj {:label "Other" :ids rest-ids}))
                        all-ids   (filterv class-ids (distinct (:members sec)))]
                  :when (seq all-ids)]
              {:id (:id sec) :label (:label sec)
               :spine? (boolean (re-find spine-re (str (:id sec) " " (:label sec))))
               :ids all-ids
               :sub-blocks (if (> (count sub-blocks) 1) sub-blocks [])})
            covered   (into #{} (mapcat :ids) planned)
            leftovers (into [] (comp (map :id) (remove covered)) classes)
            planned   (cond-> (vec planned)
                        (seq leftovers)
                        (conj {:id "—" :label "Ungrouped" :spine? false
                               :ids leftovers :sub-blocks []}))]
        (vec (sort-by (fn [s] [(if (:spine? s) 0 1) (:id s)]) planned))))))

(defn- relations-lines [props]
  (for [p (sort-by :id props)]
    (str/join " | " (remove nil? [(:id p) (:label p)
                                  (when (seq (:gloss p))
                                    (sentence-truncate (:gloss p) 140))]))))

(defn compact-ontology
  "Build the compact schema view the LLM sees: the upper-level spine first, then
  each module under its real title (subsection titles as sub-headings), classes as
  an indented subclass forest with tiered sentence-boundary glosses, and finally
  the relation properties with their glosses.

  Options:
    :interior-gloss / :leaf-gloss  tiered gloss budgets (default 100/200 chars)
    :gloss-len       back-compat override — sets BOTH tiers (chat uses 60)
    :include-ids     scoped view — render only these class ids; sections with no
                     included class compress to one `omitted` line
    :full-gloss?     no gloss truncation (scoped views, where the count is small)
    :hidden-children {id count} — coarse-view `(+N more …)` markers
    :module-summaries {section-id str} — appended to omitted-section lines"
  [model & [{:keys [gloss-len interior-gloss leaf-gloss include-ids full-gloss?
                    hidden-children module-summaries]
             :or {interior-gloss 100 leaf-gloss 200}}]]
  (let [gopts    {:interior-gloss (or gloss-len interior-gloss)
                  :leaf-gloss     (or gloss-len leaf-gloss)
                  :full-gloss?    (boolean (and full-gloss? (nil? gloss-len)))
                  :hidden-children hidden-children}
        real     (remove :external? (g/nodes model))
        classes  (filter #(not= :property (:kind %)) real)
        props    (filter #(= :property (:kind %)) real)
        include  (some-> include-ids set)
        keep?    (if include #(contains? include %) (constantly true))
        sections (section-plan model classes)
        spine?   (boolean (some :spine? sections))
        body
        (str/join
         "\n"
         (for [{:keys [id label ids sub-blocks]} sections
               :let [in-ids (filterv keep? ids)]]
           (if (empty? in-ids)
             (str "## " label " — omitted (" (count ids) " classes; none matched "
                  "this scenario"
                  ;; the briefing's summaries key on whatever the model echoed —
                  ;; the section id or its full label — accept either
                  (when-let [s (or (get module-summaries id)
                                   (get module-summaries label))]
                    (str ". " s)) ")\n")
             (str "## " label "\n"
                  (if (seq sub-blocks)
                    (str/join
                     (for [{:keys [label ids]} sub-blocks
                           :let [sids (filterv keep? ids)]
                           :when (seq sids)]
                       (str "### " label "\n"
                            (str/join "\n" (forest-lines model sids gopts)) "\n")))
                    (str (str/join "\n" (forest-lines model in-ids gopts)) "\n"))))))]
    (str
     "ONTOLOGY: " (get-in model [:meta :title] "Ontology")
     "\nFormat per line:  id | label | ⊂ parent | gloss — indentation nests "
     "subclasses under their parent (deeper = more specific)."
     (when spine?
       (str "\nThe first section is the upper-level spine: use it to orient, then map "
            "to the most specific class in the modules below."))
     "\n\n" body
     (when (seq props)
       (str "\n## Relations (object properties)\n"
            (str/join "\n" (relations-lines props)) "\n")))))

(defn compaction-fits?
  "Does the compacted ontology fit within `budget` tokens (leaving room for the
  scenario + instructions)? When false the caller should fall back to two-stage
  mapping."
  [model budget]
  (<= (estimate-tokens (compact-ontology model)) budget))

;; --- scenario chunking ------------------------------------------------------

(defn chunk-scenario
  "Split `text` into chunks each under ~`char-budget`, breaking on blank-line
  (paragraph) boundaries and carrying `overlap` characters of context between
  chunks. Small scenarios return a single chunk."
  [text & [{:keys [char-budget overlap] :or {char-budget 12000 overlap 400}}]]
  (let [text (or text "")]
    (if (<= (count text) char-budget)
      [text]
      (let [paras (str/split text #"(\n\s*\n)")]
        (loop [ps paras cur "" out []]
          (if (empty? ps)
            (if (str/blank? cur) out (conj out cur))
            (let [p (first ps)
                  candidate (str cur p)]
              (if (and (seq cur) (> (count candidate) char-budget))
                (recur ps
                       (str (subs cur (max 0 (- (count cur) overlap))))
                       (conj out cur))
                (recur (rest ps) candidate out)))))))))

;; --- request construction ---------------------------------------------------

(def relation-enum ["instance-of" "mentions" "evidence-for"])

(def ^:private node-id-enum-cap
  "Above this many node ids, skip the `node_id` enum constraint — a huge enum
  compiles to a huge GBNF grammar for no marginal benefit."
  2000)

(defn mapping-schema
  "JSON schema for Ollama structured outputs constraining the mapping response.
  Eliminates most parse failures. When `model` is supplied, `node_id` is
  additionally constrained to an enum of the model's real node ids — on
  llama.cpp/GGUF runners (grammar-constrained decoding) an invented id then cannot
  even be decoded. MLX runners ignore the schema entirely; `validate-entries`
  remains the backstop either way. (Cloud strict modes cap enum size;
  providers/strictify-schema drops oversized enums on that path.)

  `restrict-ids`, when given, narrows the enum to those class ids (plus every
  property id) — the staged run's coarse pass uses it so the grammar can't leak
  the very leaf classes the coarse view hides."
  ([] (mapping-schema nil))
  ([model] (mapping-schema model nil))
  ([model restrict-ids]
   (let [ids (when model
               (let [all (into [] (comp (remove :external?) (map :id)) (g/nodes model))
                     ids (if restrict-ids
                           (into (vec restrict-ids)
                                 (comp (remove :external?)
                                       (filter #(= :property (:kind %)))
                                       (map :id))
                                 (g/nodes model))
                           all)]
                 (when (<= (count ids) node-id-enum-cap) (vec (sort (distinct ids))))))]
     {:type "object"
      :properties
      {:entries
       {:type "array"
        :items
        {:type "object"
         :properties {:excerpt    {:type "string"}
                      :occurrence {:type "integer" :minimum 1}
                      :node_id    (if ids {:type "string" :enum ids} {:type "string"})
                      :relation   {:type "string" :enum relation-enum}
                      :confidence {:type "number" :minimum 0 :maximum 1}
                      :rationale  {:type "string"}}
         :required ["excerpt" "node_id" "relation" "confidence" "rationale"]}}
       :unmapped {:type "array" :items {:type "string"}}}
      :required ["entries"]})))

(defn- forced-constraints-text [model session]
  (let [forced (mapping/forced-entries session)]
    (when (seq forced)
      (str "\n\nHARD CONSTRAINTS — the user has FORCED these mappings. You MUST keep "
           "each one exactly as given and echo it back unchanged:\n"
           (str/join "\n"
                     (for [e forced]
                       (str "- \"" (:excerpt e) "\" (occurrence " (:occurrence e) ") -> "
                            (:node-id e) " [" (name (:relation e)) "]")))))))

(defn- accepted-examples-text
  "Up to three entries the user has already ACCEPTED, rendered as in-distribution
  worked examples — they teach excerpt granularity and rationale style for free
  and keep re-runs consistent with the curation so far."
  [session]
  (let [acc (take 3 (mapping/by-status session :accepted))]
    (when (seq acc)
      (str "\n\nEXAMPLES — entries the user has already accepted (match their "
           "granularity and style):\n"
           (str/join "\n"
                     (for [e acc]
                       (str "- \"" (:excerpt e) "\" (occurrence " (:occurrence e) ") -> "
                            (:node-id e) " [" (name (:relation e)) "]"
                            (when (seq (:rationale e))
                              (str " — " (sentence-truncate (:rationale e) 140))))))))))

(defn- notes-text
  "The format-neutral modeling-notes channel: adapters may put the ontology's own
  discipline notes / axiom statements into [:meta :prompt-notes] at parse time
  (see format.geo); rendered verbatim here. Empty for adapters that don't."
  [model]
  (let [notes (get-in model [:meta :prompt-notes])]
    (when (seq notes)
      (str "\n\nMODELING NOTES (from the ontology's own documentation — follow them):\n"
           (str/join "\n" (map #(str "- " %) notes))))))

(defn routing-guide
  "An auto-generated 'how to choose a node' decision procedure, derived from the
  loaded model's structure (occurrent / role / disposition subtrees, discovered
  exactly as the timeline pass discovers them — never hard-coded ids). Encodes the
  two classic mapping failure modes: role/bearer confusion and disposition/agent
  confusion. Degrades to the lone most-specific-subclass bullet when the ontology
  models none of these."
  [model]
  (let [modules-of (fn [ids] (->> ids (keep #(:module (g/node model %)))
                                  (remove #{"external"}) distinct sort))
        fmt-mods   (fn [ids] (let [ms (modules-of ids)]
                               (when (seq ms)
                                 (str " (mostly in: " (str/join ", " (take 5 ms)) ")"))))
        occ   (timeline/occurrent-ids model)
        roles (timeline/role-ids model)
        disp  (timeline/disposition-ids model)
        bullets
        (cond-> []
          (seq occ)
          (into [(str "- Things that HAPPEN — acts, measures being imposed, processes, "
                      "episodes — take occurrent classes" (fmt-mods occ) ".")
                 (str "- Actors, organizations, places, instruments, and other THINGS take "
                      "continuant classes — never type a thing with an occurrent class or "
                      "vice versa.")])
          (seq roles)
          (conj (str "- Role words (sender, target, chokepoint, …) name ROLES borne "
                     "relative to an episode" (fmt-mods roles) " — when the text names the "
                     "role, map the role class, not the bearer's class."))
          (seq disp)
          (conj (str "- Power-words (leverage, dependence, vulnerability, …) name "
                     "DISPOSITIONS of a bearer" (fmt-mods disp) " — never the agent class "
                     "itself."))
          true
          (conj (str "- Prefer the MOST SPECIFIC subclass that fits; if only an abstract "
                     "class fits, use it and say so in the rationale.")))]
    (str "HOW TO CHOOSE A NODE:\n" (str/join "\n" bullets))))

(defn system-prompt
  "The mapping system prompt: role, compacted ontology (optionally scoped/coarse),
  routing guide, modeling notes, curated briefing, instructions, accepted-entry
  examples, and any forced constraints.

  `opts` (all optional): compaction opts passed through to `compact-ontology`
  (`:include-ids`, `:full-gloss?`, `:hidden-children`, `:module-summaries`, gloss
  tiers), plus `:coarse?` (adds the coarse-view rule for the staged run's first
  pass) and `:briefing` (the user-curated briefing text, injected verbatim)."
  ([model session] (system-prompt model session nil))
  ([model session {:keys [coarse? briefing] :as opts}]
   (str
    "You are an expert in applied ontology and knowledge mapping. You map elements of a "
    "scenario onto a fixed ontology.\n\n"
    (compact-ontology model (dissoc opts :coarse? :briefing))
    "\n\n" (routing-guide model)
    (notes-text model)
    (when (seq briefing)
      (str "\n\nONTOLOGY BRIEFING (curated guidance for this ontology — follow it):\n"
           briefing))
    "\n\nTASK: Read the scenario and map its SALIENT elements — actors, instruments, "
    "measures, episodes, flows, dispositions, roles — to ontology node ids above.\n"
    "RULES:\n"
    "- `excerpt` is a SHORT exact quote from the scenario — a few words (e.g. a term "
    "or noun phrase), NOT a whole sentence. Copy the words verbatim; do not paraphrase "
    "or include Markdown symbols like ** or *.\n"
    "- `occurrence` is which occurrence of that quote you mean (1 for the first).\n"
    "- `node_id` MUST be one of the ids listed above. Never invent ids.\n"
    "- `relation` is one of: instance-of, mentions, evidence-for.\n"
    "- `confidence` in [0,1]; `rationale` one concise sentence.\n"
    "- Prefer precise, defensible mappings over exhaustive ones.\n"
    "- List genuinely significant scenario elements you could NOT place in `unmapped`.\n"
    (when coarse?
      (str "- This is a COARSE view of the ontology: lines marked \"(+N more specific "
           "subclasses not shown)\" have finer classes omitted. Choose the best class "
           "LISTED; a refinement pass will narrow your choices afterwards.\n"))
    "\n"
    ;; The wrapper shape is ALSO enforced out-of-band via Ollama structured
    ;; outputs (`format`), but that is grammar-constrained decoding which only
    ;; the llama.cpp runner applies — the MLX runner ignores it. Spelling the
    ;; exact JSON out here makes the run engine-agnostic so MLX models still
    ;; produce parseable output. Keep this in sync with `mapping-schema`.
    "OUTPUT: Respond with ONLY a single JSON object — no prose, no explanation, and "
    "no Markdown code fences — of exactly this form:\n"
    "{\"entries\": [{\"excerpt\": \"...\", \"occurrence\": 1, \"node_id\": \"...\", "
    "\"relation\": \"instance-of|mentions|evidence-for\", \"confidence\": 0.0, "
    "\"rationale\": \"...\"}], \"unmapped\": [\"...\"]}"
    (accepted-examples-text session)
    (forced-constraints-text model session))))

(defn mapping-messages
  "Build the /api/chat message list for one scenario chunk. `opts` are
  `system-prompt` opts (scoping, coarse view, briefing)."
  ([model session chunk-text] (mapping-messages model session chunk-text nil))
  ([model session chunk-text opts]
   [{:role "system" :content (system-prompt model session opts)}
    {:role "user" :content (str "SCENARIO:\n\n" chunk-text)}]))

;; --- request sizing (Ollama num_ctx) ----------------------------------------

(defn messages-num-ctx
  "The power-of-two Ollama `num_ctx` that fits `messages` plus generation
  headroom, capped (default 32768). Without this, Ollama silently truncates at
  its small default window — the model never sees the tail of the system prompt
  (the OUTPUT contract, the forced constraints) or the scenario itself, which
  dominates every other prompt-quality concern. Returns {:num-ctx n :overflow?
  b}; `:overflow?` means even the cap can't hold the estimate and the caller
  should surface a warning."
  [messages & [{:keys [cap headroom] :or {cap 32768 headroom 2048}}]]
  (let [est (+ headroom (transduce (map #(estimate-tokens (:content %))) + 0 messages))]
    (loop [c 4096]
      (if (or (>= c est) (>= c cap))
        {:num-ctx (min c cap) :overflow? (> est cap)}
        (recur (* 2 c))))))

;; --- scenario-scoped compaction (stage 1: lexical) --------------------------

(def ^:private stopwords
  #{"the" "and" "for" "with" "that" "this" "from" "was" "were" "are" "has" "have"
    "had" "its" "their" "into" "over" "under" "between" "after" "before" "when"
    "then" "than" "them" "they" "will" "would" "could" "should" "not" "but" "all"
    "one" "two" "also" "been" "being" "each" "which" "who" "what" "where" "why"
    "how" "his" "her" "our" "your" "any" "some" "such" "may" "can" "more" "most"
    "other" "there" "these" "those" "while" "during" "against" "through" "about"})

(defn- stem
  "Crude plural-stripping stem — enough to make 'chokepoints' hit 'Chokepoint'."
  [w]
  (cond
    (str/ends-with? w "ies")                       (str (subs w 0 (- (count w) 3)) "y")
    (and (str/ends-with? w "s") (> (count w) 3)
         (not (str/ends-with? w "ss")))            (subs w 0 (dec (count w)))
    :else w))

(defn- words [s]
  (into #{}
        (comp (map str/lower-case)
              (remove stopwords)
              (filter #(>= (count %) 3))
              (map stem))
        (re-seq #"[A-Za-z][A-Za-z0-9'-]*" (str s))))

(defn- split-camel [s] (str/replace (str s) #"([a-z])([A-Z])" "$1 $2"))

(defn score-nodes
  "Lexical relevance of every real node to `text`: 3 points per query token
  hitting the label / camelCase-split local id, 1 per gloss hit (both sides
  stemmed). Returns {node-id score} for scores > 0 — the cheap, deterministic
  stage 1 of scoped mapping."
  [model text]
  (let [q (words text)]
    (into {}
          (keep (fn [n]
                  (let [lts (words (str (:label n) " "
                                        (split-camel (timeline/local-name (:id n)))))
                        gts (words (:gloss n))
                        s   (+ (* 3 (count (filter q lts)))
                               (count (filter q gts)))]
                    (when (pos? s) [(:id n) s]))))
          (remove :external? (g/nodes model)))))

(defn structural-closure
  "Close class `ids` over the structure a mapping judgment needs: every subclass
  ancestor (orientation — where does this sit?) and every sibling of an included
  node (contrast — is a sibling the better fit?). External stubs are dropped;
  properties are rendered separately and need no closure."
  [model ids]
  (let [base (set ids)
        anc  (into #{} (mapcat #(g/ancestors model %)) base)
        sibs (into #{} (comp (mapcat #(g/parents model %))
                             (mapcat #(g/children model %)))
                   base)]
    (into #{}
          (remove #(:external? (g/node model %)))
          (concat base anc sibs))))

(defn scoped-ids
  "The class ids a scoped mapping prompt should render for `text`: the top-`top-k`
  lexically-relevant nodes (see `score-nodes`) closed over ancestors + siblings."
  [model text & [{:keys [top-k] :or {top-k 40}}]]
  (let [top (->> (score-nodes model text)
                 (sort-by (juxt (comp - val) key))
                 (take top-k)
                 (map key))]
    (structural-closure model top)))

(defn scoped-opts
  "The `compact-ontology` opts for a scoped run over `text`: the scoped id set,
  fully glossed only while it stays small — a scenario that lexically touches
  most of the ontology would otherwise inflate the 'scoped' view past the full
  one (closure pulls in every sibling)."
  [model text]
  (let [ids (scoped-ids model text)]
    {:include-ids ids :full-gloss? (<= (count ids) 120)}))

;; --- coarse view (staged mapping, pass 1) -----------------------------------

(defn coarse-view
  "The ids visible in the coarse (top-of-hierarchy) rendering: every class within
  `depth` subclass steps of its local roots, plus {boundary-id omitted-descendant-
  count} for the subtrees that were cut — rendered by `compact-ontology` as
  `(+N more specific subclasses not shown)` markers. Returns {:ids set :hidden map}.
  `depth` defaults to 1 (roots + their children): the reference ontologies are
  shallow-and-wide, so anything deeper reproduces nearly the full view."
  [model & [{:keys [depth] :or {depth 1}}]]
  (let [classes (into [] (comp (remove :external?)
                               (filter #(not= :property (:kind %)))
                               (map :id))
                      (g/nodes model))
        cset    (set classes)
        real-parent? (fn [id] (some cset (g/parents model id)))
        roots   (remove real-parent? classes)]
    (loop [level (vec (sort roots)) d 0 seen (set roots)]
      (if (or (>= d depth) (empty? level))
        {:ids seen
         :hidden (into {}
                       (keep (fn [id]
                               (when (seq (remove seen (g/children model id)))
                                 [id (count (remove seen (g/subtree model id)))])))
                       seen)}
        (let [nxt (into [] (comp (mapcat #(sort (g/children model %)))
                                 (remove seen) (distinct))
                        level)]
          (recur nxt (inc d) (into seen nxt)))))))

;; --- strategy gate -----------------------------------------------------------

(def default-ontology-budget
  "Token budget for the ontology block of a mapping prompt before the run falls
  back to scoping / staging. Calibrated so the ~270-node reference ontology still
  runs single-pass :full (its tree compaction is ~11k tokens) while the ~430-node
  v2-scale ontology (~16k) gets scoped/staged; num_ctx scales to fit either, but
  local-model accuracy degrades as the prompt grows — that, not fit, is what this
  budget guards."
  12000)

(defn choose-strategy
  "Pick how to present the ontology for mapping `text`:
    :full   — the whole compacted ontology fits `budget`; one pass
    :scoped — a lexically-scoped view fits; one pass over the scoped view
    :staged — coarse pass over the hierarchy top + per-branch refinement
  Pure; `budget` defaults to `default-ontology-budget`."
  [model text & [budget]]
  (let [budget (or budget default-ontology-budget)]
    (cond
      (compaction-fits? model budget) :full
      (<= (estimate-tokens (compact-ontology model (scoped-opts model text)))
          budget)                     :scoped
      :else                           :staged)))

(defn strategy-opts
  "The `compact-ontology`/`system-prompt` opts for one scenario chunk under
  `strategy` (:full | :scoped | :staged) — the ONE place the strategy→prompt
  mapping lives. Both the app's run events and the eval harness call this, so
  what gets measured is what ships; callers merge session extras (briefing,
  module summaries) on top."
  [model chunk strategy]
  (case strategy
    :scoped (scoped-opts model chunk)
    :staged (let [{:keys [ids hidden]} (coarse-view model)]
              {:include-ids ids :hidden-children hidden :coarse? true})
    nil))

;; --- response parsing + validation ------------------------------------------

(defn ->entry
  "Turn one raw structured-output entry map (string/keyword keys) into a mapping
  entry, tolerating key spelling variants. Public: the eval harness must decode
  the wire shape EXACTLY as the app does (same defaults, same coercions)."
  [raw]
  (let [g #(or (get raw %1) (get raw %2))]
    (mapping/new-entry
     {:excerpt    (str (or (g :excerpt "excerpt") ""))
      :occurrence (int (or (g :occurrence "occurrence") 1))
      :node-id    (str (or (g :node_id "node_id") (get raw :node-id) ""))
      :relation   (keyword (or (g :relation "relation") "mentions"))
      :confidence (let [c (g :confidence "confidence")] (if (number? c) c 0.5))
      :rationale  (str (or (g :rationale "rationale") ""))})))

(defn- balanced-json-object
  "Scan `s` for the first balanced, top-level `{ … }` region and return that
  substring, or nil. Tracks string literals and escapes so braces inside quoted
  values don't unbalance the count. Lets us recover the JSON object a model
  wrapped in prose or a leading `thinking` preamble."
  [s]
  (let [n (count s)]
    (loop [i 0]
      (cond
        (>= i n) nil
        (= \{ (nth s i))
        (loop [j i depth 0 in-str? false escaped? false]
          (if (>= j n)
            nil
            (let [c (nth s j)]
              (cond
                escaped?   (recur (inc j) depth in-str? false)
                (= \\ c)   (recur (inc j) depth in-str? true)
                (= \" c)   (recur (inc j) depth (not in-str?) false)
                in-str?    (recur (inc j) depth in-str? false)
                (= \{ c)   (recur (inc j) (inc depth) in-str? false)
                (= \} c)   (if (= 1 depth)
                             (subs s i (inc j))
                             (recur (inc j) (dec depth) in-str? false))
                :else      (recur (inc j) depth in-str? false)))))
        :else (recur (inc i))))))

(defn extract-json-object
  "Best-effort parse of a model reply into a clj map with keyword keys, or nil.
  Tries a direct parse, then strips Markdown code fences, then recovers the first
  balanced `{ … }` object embedded in surrounding prose/thinking. Never throws.
  This is what lets the mapping run tolerate models (notably Ollama MLX builds)
  that ignore the `format` schema and answer with fenced or prose-wrapped JSON."
  [s]
  (let [try-parse (fn [x] (try (let [v (js->clj (js/JSON.parse x) :keywordize-keys true)]
                                 (when (map? v) v))
                               (catch :default _ nil)))
        stripped  (-> (str s)
                      (str/replace #"(?s)```(?:json)?\s*" "")
                      (str/replace #"```" ""))]
    (or (try-parse s)
        (try-parse stripped)
        (some-> (balanced-json-object stripped) try-parse))))

(defn parse-response
  "Parse a structured-output response (already `js->clj`'d with keyword keys, OR a
  JSON string) into {:entries [...] :unmapped [...] :status <kw>}. Never throws —
  malformed input yields empty results the caller can surface.

  `:status` distinguishes the failure modes so the UI can explain them:
    :ok       parsed a JSON object (entries may still be empty — genuine no-match)
    :no-json  non-blank reply but no parseable object — the model ignored the
              response schema (the classic MLX-runner symptom)
    :empty    blank/nil reply"
  [resp]
  (let [data (cond
               (map? resp) resp
               (and (string? resp) (str/blank? resp)) nil
               (string? resp) (extract-json-object resp)
               :else nil)
        status (cond
                 (map? data) :ok
                 (or (nil? resp) (and (string? resp) (str/blank? resp))) :empty
                 :else :no-json)
        entries (or (:entries data) [])]
    {:entries  (mapv ->entry entries)
     :unmapped (vec (:unmapped data))
     :status   status}))

(defn validate-entries
  "Flag entries whose `:node-id` is not in the model (`:invalid-target`) or whose
  `:excerpt` cannot be located in `scenario-text` (`:excerpt-not-found`). Never
  drops entries — the user sees exactly what came back and what was wrong."
  [model scenario-text entries]
  (mapv (fn [e]
          (let [flags (cond-> #{}
                        (not (g/exists? model (:node-id e))) (conj :invalid-target)
                        (not (mapping/excerpt-locatable? scenario-text e)) (conj :excerpt-not-found))]
            (assoc e :flags flags)))
        entries))

;; --- chat -------------------------------------------------------

(defn- entries-summary
  "A compact, bounded rendering of the current mapping entries for the chat system
  context. Forced entries are marked so the model treats them as fixed."
  [session]
  (let [es (mapping/active-entries session)]
    (if (empty? es)
      "(no entries yet)"
      (str/join "\n"
                (for [e (take 60 es)]
                  (str "- \"" (:excerpt e) "\" -> " (:node-id e)
                       " [" (name (:relation e)) ", " (name (:status e))
                       (when (= :forced (:status e)) ", FORCED — do not change") "]"))))))

(defn- timeline-summary
  "A compact, bounded rendering of the current timeline events + relations for the
  chat context. Forced items are marked; untyped items are shown as gaps so the model
  can reason about them without re-deriving structure."
  [session]
  (let [tl (:timeline session)
        es (timeline/active-events tl)
        rs (timeline/active-relations tl)]
    (if (and (empty? es) (empty? rs))
      "(no timeline yet — the user can run the timeline pass)"
      (str
       (str/join "\n"
                 (for [e (take 50 es)]
                   (str "- event “" (:label e) "” -> " (or (:node-id e) "(untyped gap)")
                        (when (= :forced (:status e)) " [FORCED — do not change]"))))
       "\n"
       (str/join "\n"
                 (for [r (take 50 rs)]
                   (str "- relation " (name (:type r)) " (" (or (:property-id r) "untyped gap") ")")))))))

(defn chat-system-prompt
  "Assemble the chat system context each turn: role, compacted ontology, scenario,
  the CURRENT mapping AND timeline state (forced items highlighted), and the one
  `mapping-update` protocol that covers entries and timeline events/relations (§6.5)."
  [model session]
  (str
   "You are an expert assistant helping a user curate a mapping (and a temporal "
   "timeline of events) from a scenario onto the loaded ontology. Answer questions "
   "about the mapping, the timeline, and the ontology concisely. When the user asks a "
   "structural question (dependencies, chains, why an event is untyped) and computed "
   "structure is supplied in their message, narrate THAT — never re-derive graph "
   "structure from the raw text.\n\n"
   (compact-ontology model {:gloss-len 60})
   "\n\nSCENARIO:\n" (get-in session [:scenario :text])
   "\n\nCURRENT MAPPING:\n" (entries-summary session)
   "\n\nCURRENT TIMELINE:\n" (timeline-summary session)
   "\n\nWhen — and ONLY when — you want to change the mapping or timeline, emit a "
   "fenced code block tagged `mapping-update` containing JSON. Each op has a `target` "
   "of `entry`, `event`, or `relation` and a `value`:\n"
   "```mapping-update\n"
   "{\"ops\": ["
   "{\"op\": \"update\", \"target\": \"entry\", \"value\": {\"excerpt\": \"...\", \"node_id\": \"geo:...\", \"relation\": \"instance-of\", \"confidence\": 0.8, \"rationale\": \"...\"}},"
   "{\"op\": \"add\", \"target\": \"event\", \"value\": {\"label\": \"...\", \"excerpt\": \"...\", \"node_id\": \"geo:...\"|null, \"nearest\": \"geo:...\"|null, \"why_no_fit\": \"...\"|null}},"
   "{\"op\": \"update\", \"target\": \"relation\", \"value\": {\"id\": \"<relation-id>\", \"type\": \"causes\", \"property_id\": \"geo:...\"|null}}"
   "], \"reason\": \"why\"}\n"
   "```\n"
   "Rules: use only node ids that exist above; never modify FORCED items; keep prose "
   "and the block separate. The user reviews every change before it applies."))

(defn chat-messages
  "Full message list for a chat turn: the assembled system context, prior transcript
  (role/content pairs), then the new user message."
  [model session transcript user-text]
  (into [{:role "system" :content (chat-system-prompt model session)}]
        (conj (mapv (fn [m] {:role (name (:role m)) :content (:content m)}) transcript)
              {:role "user" :content user-text})))

;; --- shared value normalizers (timeline extraction + chat updates) ----------

(defn- blank->nil [x]
  (let [s (some-> x str str/trim)]
    (when (and s (seq s) (not= "null" (str/lower-case s))) x)))

(defn- ->when
  "Normalize a raw `when`/temporal map into the model's `:when` shape. A null date
  is a valid answer (the model must never invent dates); `:narrative-index` is the
  ordering of last resort and always present."
  [raw ni]
  (let [g #(or (get raw %1) (get raw %2))]
    {:kind (keyword (or (blank->nil (g :kind "kind")) "unknown"))
     :start (blank->nil (g :start "start"))
     :end   (blank->nil (g :end "end"))
     :precision (some-> (blank->nil (g :precision "precision")) keyword)
     :narrative-index (int (or (g :narrative_index "narrative_index")
                               (get raw :narrative-index) ni))}))

(defn- ->participant [raw]
  (let [g #(or (get raw %1) (get raw %2))]
    {:entity  (str (or (blank->nil (g :entity "entity")) ""))
     :node-id (blank->nil (or (g :node_id "node_id") (get raw :node-id)))
     :role-id (blank->nil (or (g :role_id "role_id") (get raw :role-id)))}))

(defn ->timeline-event
  "Normalize one raw extracted event (string/keyword keys) into an event map for
  `onteater.model.timeline`. A null `node_id` is preserved with its `nearest` +
  `why_no_fit` — a valid, expected answer (a gap), never forced. The LLM's own event
  id rides along as `:local-id` so relation endpoints can be resolved. `idx` supplies
  a default narrative index."
  [raw idx]
  (let [g #(or (get raw %1) (get raw %2))]
    {:local-id    (str (or (g :id "id") (str "e" idx)))
     :label       (str (or (blank->nil (g :label "label")) (blank->nil (g :excerpt "excerpt")) ""))
     :excerpt     (str (or (blank->nil (g :excerpt "excerpt")) ""))
     :occurrence  (int (or (g :occurrence "occurrence") 1))
     :node-id     (blank->nil (or (g :node_id "node_id") (get raw :node-id)))
     :nearest     (blank->nil (g :nearest "nearest"))
     :why-no-fit  (blank->nil (or (g :why_no_fit "why_no_fit") (get raw :why-no-fit)))
     :participants (mapv ->participant (or (g :participants "participants") []))
     :when        (->when (or (g :when "when") {}) idx)
     :confidence  (let [c (g :confidence "confidence")] (if (number? c) c 0.6))
     :rationale   (str (or (blank->nil (g :rationale "rationale")) ""))}))

(defn ->timeline-relation
  "Normalize one raw extracted relation. Endpoints (`source`/`target`) reference the
  LLM's event ids and are resolved downstream. A null `property_id` is preserved (a
  gap). `:type` falls back to `:precedes`."
  [raw]
  (let [g #(or (get raw %1) (get raw %2))
        t (some-> (blank->nil (g :type "type")) keyword)]
    {:source      (blank->nil (or (g :source "source") (g :from "from")))
     :target      (blank->nil (or (g :target "target") (g :to "to")))
     :type        (if (contains? timeline/relation-type-set t) t :precedes)
     :property-id (blank->nil (or (g :property_id "property_id") (get raw :property-id)))
     :confidence  (let [c (g :confidence "confidence")] (if (number? c) c 0.6))
     :rationale   (str (or (blank->nil (g :rationale "rationale")) ""))}))

;; --- chat updates -----------------------------------------------------------

(def ^:private mapping-update-re
  #"(?s)```mapping-update\s*(\{.*?\})\s*```")

(defn- normalize-op
  "Normalize one chat-update op into `{:op :target :value}`. `:target` defaults to
  `:entry` (backward compatible with the entry-only protocol, which keyed the payload
  under `:entry`). For `:event`/`:relation` targets the payload is under `:value`.
  A single diff-card pipeline then serves entries AND timeline items (§6.5)."
  [op]
  (let [target (keyword (or (:target op) "entry"))
        raw    (or (:value op) (:entry op))
        value  (case target
                 :entry    (when raw (->entry raw))
                 :event    (when raw (->timeline-event raw 0))
                 :relation (when raw (->timeline-relation raw))
                 raw)]
    (cond-> {:op (keyword (:op op)) :target target :value value}
      ;; `:entry` alias keeps the entry-only consumers (mapping/apply-op, the chat
      ;; op-card) working unchanged; timeline consumers read `:value`.
      (= :entry target) (assoc :entry value))))

(defn parse-mapping-updates
  "Extract every ```mapping-update``` block from an assistant `content` string.
  Returns {:prose <content with blocks removed> :updates [{:ops [...] :reason s} ...]}.
  Each op is normalized to `{:op :target :value}` (§6.5) — the one protocol that
  covers mapping entries and timeline events/relations. Never throws."
  [content]
  (let [content (or content "")
        matches (re-seq mapping-update-re content)
        updates (keep (fn [[_ json]]
                        (try
                          (let [data (js->clj (js/JSON.parse json) :keywordize-keys true)
                                ops  (mapv normalize-op (:ops data))]
                            (when (seq ops) {:ops ops :reason (:reason data)}))
                          (catch :default _ nil)))
                      matches)
        prose (str/trim (str/replace content mapping-update-re ""))]
    {:prose prose :updates (vec updates)}))

;; ---------------------------------------------------------------------------
;; Timeline extraction pass (§6.7.2) — a second, occurrent-focused request
;; ---------------------------------------------------------------------------
;;
;; Shares the compaction/chunking/structured-output/validation/forced-constraint
;; machinery of the entity pass, but foregrounds the ontology's OCCURRENT
;; vocabulary (process classes, roles, stage states, relation properties) —
;; discovered structurally from the loaded model, never hard-coded. A null
;; node_id/property_id is an explicitly valid answer that routes straight to the
;; gap report (never retried away).

(def ^:private temporal-property-re
  #"(?i)preced|caus|enabl|respond|retaliat|terminat|within|part|before|after|follow|trigger|lead|react")

(defn occurrent-compaction
  "The occurrent-focused compact schema view the timeline pass sees. Foregrounds
  occurrent classes (grouped by module, as an indented subclass forest), role
  classes, stage-machine states, and the relation properties (temporal/causal ones
  first) — with the same tree rendering and tiered sentence-boundary glosses as
  `compact-ontology`. Falls back gracefully when the ontology models no process
  spine (the sections are simply empty and the pass still runs, typing everything
  as gaps)."
  [model & [{:keys [gloss-len] :or {gloss-len 90}}]]
  (let [gopts  {:interior-gloss gloss-len :leaf-gloss (* 2 gloss-len)}
        occ    (timeline/occurrent-ids model)
        roles  (timeline/role-ids model)
        stages (timeline/stage-ids model)
        occ-ids  (->> occ (keep #(g/node model %)) (remove :external?) (map :id))
        role-ids (->> roles (keep #(g/node model %)) (remove :external?) (map :id))
        props      (->> (g/nodes model) (filter #(= :property (:kind %))) (sort-by :id))
        temporal   (filter #(re-find temporal-property-re (str (:id %) " " (:label %))) props)
        section (fn [title lines] (when (seq lines) (str "## " title "\n" (str/join "\n" lines) "\n\n")))
        by-mod  (->> occ-ids (group-by #(:module (g/node model %))) (sort-by first))]
    (str
     "ONTOLOGY OCCURRENT VOCABULARY: " (get-in model [:meta :title] "Ontology")
     "\nType events with the MOST SPECIFIC fitting class. Format:  id | label | "
     "⊂ parent | gloss — indentation nests subclasses under their parent.\n\n"
     (str/join
      (for [[m ids] by-mod]
        (section (str "Occurrent classes — module " (or m "—"))
                 (forest-lines model ids gopts))))
     (section "Role classes (for participants)" (forest-lines model role-ids gopts))
     (when (seq stages)
       (section "Stage-machine states" (sort (map timeline/local-name stages))))
     (section "Relation properties (temporal/causal first)"
              (concat (map #(str % "  ⟵ temporal/causal") (relations-lines temporal))
                      (relations-lines (remove (set temporal) props)))))))

(defn timeline-schema
  "JSON schema for the timeline structured-output response. Best-effort only —
  llama.cpp honours it, MLX ignores it — so the prompt spells the same contract.
  `node_id`/`property_id`/`role_id` are nullable: a null is a valid gap answer."
  []
  (let [nullable-str {:type ["string" "null"]}]
    {:type "object"
     :properties
     {:events
      {:type "array"
       :items {:type "object"
               :properties {:id {:type "string"}
                            :label {:type "string"}
                            :excerpt {:type "string"}
                            :occurrence {:type "integer"}
                            :node_id nullable-str
                            :nearest nullable-str
                            :why_no_fit nullable-str
                            :participants {:type "array"
                                           :items {:type "object"
                                                   :properties {:entity {:type "string"}
                                                                :node_id nullable-str
                                                                :role_id nullable-str}}}
                            :when {:type "object"
                                   :properties {:kind {:type "string"}
                                                :start nullable-str
                                                :end nullable-str
                                                :precision nullable-str
                                                :narrative_index {:type "integer"}}}
                            :confidence {:type "number"}
                            :rationale {:type "string"}}}}
      :relations
      {:type "array"
       :items {:type "object"
               :properties {:source {:type "string"}
                            :target {:type "string"}
                            :type {:type "string"}
                            :property_id nullable-str
                            :confidence {:type "number"}
                            :rationale {:type "string"}}}}}
     :required ["events" "relations"]}))

(defn- forced-timeline-text
  "Echo the user's forced events/relations back as hard constraints (§6.7.2)."
  [session]
  (let [tl (:timeline session)
        fe (timeline/forced-events tl)
        fr (timeline/forced-relations tl)]
    (when (or (seq fe) (seq fr))
      (str "\n\nHARD CONSTRAINTS — the user has FORCED these. Keep each exactly and "
           "echo it back unchanged:\n"
           (str/join "\n" (for [e fe] (str "- event \"" (:label e) "\" -> "
                                           (or (:node-id e) "(untyped)"))))
           (str/join "\n" (for [r fr] (str "- relation " (name (:type r))
                                           " -> " (or (:property-id r) "(untyped)"))))))))

(defn timeline-system-prompt
  "System prompt for the timeline pass: role, occurrent vocabulary, the exact JSON
  contract, and the null-is-valid instruction that relieves force-fit pressure."
  [model session]
  (str
   "You are an expert in applied ontology extracting the TEMPORAL structure of a "
   "scenario: its events (occurrents), who takes part, and the temporal/causal "
   "relations between them — including forks (one cause, several effects), joins "
   "(several causes, one event), and parallel threads.\n\n"
   (occurrent-compaction model)
   (some-> (notes-text model) (str "\n"))
   "\nTASK:\n"
   "- Identify the scenario's salient EVENTS. Give each a short `label` and a SHORT "
   "exact `excerpt` quoted verbatim from the scenario (a few words), plus which "
   "`occurrence` of that quote you mean.\n"
   "- Type each event with the MOST SPECIFIC fitting occurrent class id in `node_id`. "
   "If NONE fits, set `node_id` to null and give `nearest` (closest abstract class) "
   "and a one-line `why_no_fit`. A null is a VALID, EXPECTED answer — do NOT force a "
   "poor fit; a genuine gap is more useful than a wrong type.\n"
   "- List `participants` with the scenario `entity` name, its ontology `node_id` if "
   "known, and a `role_id` role class (null if none fits — also a valid gap).\n"
   "- Give `when` with `kind` (instant|interval|unknown), `start`/`end` dates ONLY if "
   "the scenario states them (never invent dates), `precision` (day|month|year), and a "
   "`narrative_index` (0-based order of appearance in the text).\n"
   "- Give `relations` between events by their `id`s: `source` (the cause/precedent) → "
   "`target` (the consequence), a `type` (precedes|causes|enables|responds-to|part-of|"
   "terminates), and a matching ontology `property_id` (null if none fits — a gap).\n"
   "- Assign each event a stable `id` (e.g. \"e1\",\"e2\") and use those ids in relations.\n\n"
   "OUTPUT: ONLY a single JSON object — no prose, no code fences — of this form:\n"
   "{\"events\":[{\"id\":\"e1\",\"label\":\"...\",\"excerpt\":\"...\",\"occurrence\":1,"
   "\"node_id\":\"geo:...\"|null,\"nearest\":\"geo:...\"|null,\"why_no_fit\":\"...\"|null,"
   "\"participants\":[{\"entity\":\"...\",\"node_id\":\"geo:...\"|null,\"role_id\":\"geo:...\"|null}],"
   "\"when\":{\"kind\":\"instant\",\"start\":\"2025-04-04\"|null,\"end\":null,\"precision\":\"day\"|null,"
   "\"narrative_index\":0},\"confidence\":0.0,\"rationale\":\"...\"}],"
   "\"relations\":[{\"source\":\"e1\",\"target\":\"e2\",\"type\":\"causes\","
   "\"property_id\":\"geo:...\"|null,\"confidence\":0.0,\"rationale\":\"...\"}]}"
   (forced-timeline-text session)))

(defn timeline-messages
  "Build the /api/chat message list for one scenario chunk of the timeline pass."
  [model session chunk-text]
  [{:role "system" :content (timeline-system-prompt model session)}
   {:role "user" :content (str "SCENARIO:\n\n" chunk-text)}])

(defn parse-timeline-response
  "Parse a timeline structured-output response (a CLJS map or a JSON string) into
  `{:events [...] :relations [...] :status kw}`. Tolerant exactly like
  `parse-response`: recovers JSON from code fences / prose / a `thinking` preamble,
  never throws. Events carry `:local-id` (the model's own id) so relation endpoints
  resolve downstream; null typings are preserved as gaps.

  `:status` is `:ok` (parsed an object), `:no-json` (non-blank but unparseable — the
  MLX-runner symptom), or `:empty` (blank/nil)."
  [resp]
  (let [data (cond
               (map? resp) resp
               (and (string? resp) (str/blank? resp)) nil
               (string? resp) (extract-json-object resp)
               :else nil)
        status (cond
                 (map? data) :ok
                 (or (nil? resp) (and (string? resp) (str/blank? resp))) :empty
                 :else :no-json)
        events (vec (map-indexed (fn [i e] (->timeline-event e i)) (or (:events data) [])))
        rels   (mapv ->timeline-relation (or (:relations data) []))]
    {:events events :relations rels :status status}))

(defn validate-timeline
  "Validate + repair a parsed timeline extraction against the model and scenario.
  Flags events (`:invalid-target` when a non-nil `node-id` is unknown;
  `:excerpt-not-found` when the excerpt can't be located) and relations
  (`:invalid-property` when a non-nil `property-id` is unknown). REPAIRS dangling
  relations by dropping any whose `source`/`target` doesn't reference a returned
  event — the classic repair path. Never drops events; gaps (null typings) are
  preserved intact. Returns `{:events [...] :relations [...] :dropped n}`."
  [model scenario-text events relations]
  (let [ids (into #{} (map :local-id) events)
        flagged-events
        (mapv (fn [e]
                (assoc e :flags
                       (cond-> #{}
                         (and (:node-id e) (not (g/exists? model (:node-id e)))) (conj :invalid-target)
                         (not (mapping/excerpt-locatable? scenario-text e)) (conj :excerpt-not-found))))
              events)
        {:keys [kept dropped]}
        (reduce (fn [acc r]
                  (if (and (ids (:source r)) (ids (:target r)))
                    (update acc :kept conj
                            (assoc r :flags (cond-> #{}
                                              (and (:property-id r) (not (g/exists? model (:property-id r))))
                                              (conj :invalid-property))))
                    (update acc :dropped inc)))
                {:kept [] :dropped 0}
                relations)]
    {:events flagged-events :relations kept :dropped dropped}))

;; ---------------------------------------------------------------------------
;; Refinement pass (staged mapping, pass 2) — narrow coarse typings per branch
;; ---------------------------------------------------------------------------
;;
;; After the coarse pass, every proposed entry typed to a class that still has
;; subclasses is a candidate for narrowing. Batches are per-module; each batch's
;; prompt renders ONLY the branches involved (fully glossed, closed over
;; ancestors + sibling branches so "wrong branch: use X instead" stays
;; expressible). Output contract = the mapping entries JSON, so parse-response /
;; validate-entries / merge machinery apply unchanged.

(defn refinable-entries
  "Still-`:proposed` entries whose node exists and has more specific subclasses —
  the candidates for the narrowing pass. Curated entries are never touched."
  [model session]
  (->> (mapping/active-entries session)
       (filter #(= :proposed (:status %)))
       (filter #(and (seq (str (:node-id %)))
                     (g/exists? model (:node-id %))
                     (seq (g/children model (:node-id %)))))))

(defn refine-batches
  "Group refinable entries into per-module batches of ≤ `max-per-batch` (module of
  the entry's current node — one branch neighbourhood per request)."
  [model session & [{:keys [max-per-batch] :or {max-per-batch 8}}]]
  (->> (refinable-entries model session)
       (group-by #(:module (g/node model (:node-id %))))
       (sort-by (fn [[m _]] (str m)))
       (mapcat (fn [[_ es]] (partition-all max-per-batch es)))
       (mapv vec)))

(defn entry->wire
  "Serialize an entry back to the wire shape (`->entry`'s inverse, minus flags).
  Public for the same reason as `->entry`: one wire contract, one encoder."
  [e]
  {:excerpt (:excerpt e) :occurrence (:occurrence e) :node_id (:node-id e)
   :relation (name (:relation e)) :confidence (:confidence e)
   :rationale (:rationale e)})

(defn refine-messages
  "Messages for one refinement batch: a scoped, fully-glossed view of the subtrees
  the batch's entries landed in (plus ancestors and sibling branches via
  `structural-closure`) and the entries to narrow."
  [model session batch]
  (let [ids    (into #{} (mapcat #(g/subtree model (:node-id %))) batch)
        scoped (structural-closure model ids)
        sys (str
             "You are an expert in applied ontology refining a draft scenario mapping. "
             "Each draft entry below is typed with a class that has MORE SPECIFIC "
             "subclasses in the ontology.\n\n"
             (compact-ontology model {:include-ids scoped :full-gloss? true})
             (notes-text model)
             "\n\nTASK: For each entry, re-read its excerpt in the scenario and pick the "
             "MOST SPECIFIC class id above that genuinely fits.\n"
             "RULES:\n"
             "- Keep `excerpt` and `occurrence` EXACTLY as given — never reword them.\n"
             "- If a more specific subclass fits, replace `node_id` with it and update "
             "`confidence` and `rationale`.\n"
             "- If the current class is already the best fit, echo the entry unchanged — "
             "do NOT force a more specific class that doesn't fit.\n"
             "- If an entry sits in the WRONG BRANCH entirely, you may retarget it to any "
             "id listed above; say why in `rationale`.\n"
             "- Never invent ids.\n\n"
             "OUTPUT: ONLY a single JSON object — no prose, no code fences — of exactly "
             "this form:\n"
             "{\"entries\": [{\"excerpt\": \"...\", \"occurrence\": 1, \"node_id\": \"...\", "
             "\"relation\": \"instance-of|mentions|evidence-for\", \"confidence\": 0.0, "
             "\"rationale\": \"...\"}]}")
        user (str "SCENARIO:\n\n" (get-in session [:scenario :text])
                  "\n\nENTRIES TO REFINE:\n"
                  (js/JSON.stringify (clj->js {:entries (mapv entry->wire batch)}) nil 1))]
    [{:role "system" :content sys}
     {:role "user" :content user}]))

(defn apply-refinements
  "Fold one refinement batch's parsed entries back into the session. Each refined
  entry retargets the still-`:proposed` entry with the same (normalized excerpt,
  occurrence); unknown target ids, unmatched entries, no-op echoes, and curated
  entries are all left untouched. Returns the updated session."
  [model session refined]
  (let [ekey (fn [e] [(mapping/normalize-match (:excerpt e)) (or (:occurrence e) 1)])]
    (reduce (fn [s r]
              (let [match (first (filter #(and (= :proposed (:status %))
                                               (= (ekey %) (ekey r)))
                                         (:entries s)))]
                (if (and match
                         (g/exists? model (:node-id r))
                         (not= (:node-id match) (:node-id r)))
                  (mapping/update-entry s (:id match) merge
                                        (select-keys r [:node-id :confidence :rationale]))
                  s)))
            session refined)))

;; ---------------------------------------------------------------------------
;; Ontology briefing (LLM-generated once per ontology, user-curated)
;; ---------------------------------------------------------------------------
;;
;; A one-time meta-prompting pass: the model studies the compacted ontology and
;; produces per-module summaries + confusable-class disambiguation rules. The
;; result is validated (ids must exist), rendered to editable text, reviewed by
;; the user, and thereafter injected into every mapping prompt (`:briefing` opt
;; of `system-prompt`). Never regenerated silently, never trusted unvalidated.

(defn briefing-schema
  "JSON schema for the briefing structured output (best-effort, like all schemas
  here — the prompt spells the same contract)."
  []
  {:type "object"
   :properties
   {:module_summaries {:type "array"
                       :items {:type "object"
                               :properties {:module  {:type "string"}
                                            :summary {:type "string"}}
                               :required ["module" "summary"]}}
    :disambiguations  {:type "array"
                       :items {:type "object"
                               :properties {:use  {:type "string"}
                                            :not  {:type "string"}
                                            :rule {:type "string"}}
                               :required ["use" "rule"]}}
    :cautions         {:type "array" :items {:type "string"}}}
   :required ["module_summaries" "disambiguations"]})

(defn briefing-messages
  "The one-shot briefing request: study the full compaction, produce summaries +
  disambiguation rules as JSON."
  [model]
  [{:role "system"
    :content
    (str "You are an expert in applied ontology preparing a briefing for a colleague "
         "who will map scenario texts onto this ontology.\n\n"
         (compact-ontology model)
         (notes-text model)
         "\n\nTASK: produce\n"
         "- `module_summaries`: for EVERY module section above, one sentence on what "
         "belongs in it;\n"
         "- `disambiguations`: the 5–15 most confusable class pairs — `use` (the id to "
         "prefer), `not` (the id it gets confused with), and a one-line `rule` for "
         "choosing between them;\n"
         "- `cautions`: up to 5 one-line warnings about likely mapping mistakes.\n"
         "Use ONLY class/property ids that appear above. Never invent ids.\n\n"
         "OUTPUT: ONLY a single JSON object — no prose, no code fences — of exactly "
         "this form:\n"
         "{\"module_summaries\":[{\"module\":\"...\",\"summary\":\"...\"}],"
         "\"disambiguations\":[{\"use\":\"...\",\"not\":\"...\",\"rule\":\"...\"}],"
         "\"cautions\":[\"...\"]}")}
   {:role "user" :content "Produce the briefing JSON now."}])

(defn parse-briefing
  "Parse + validate a briefing reply (CLJS map or JSON string; tolerant like
  `parse-response`). Disambiguations whose `use`/`not` reference unknown node ids
  are dropped and counted in `:dropped` — never trust LLM output. Returns
  {:briefing {...}|nil :dropped n :status :ok|:no-json|:empty}."
  [model resp]
  (let [data (cond
               (map? resp) resp
               (and (string? resp) (str/blank? resp)) nil
               (string? resp) (extract-json-object resp)
               :else nil)
        status (cond
                 (map? data) :ok
                 (or (nil? resp) (and (string? resp) (str/blank? resp))) :empty
                 :else :no-json)]
    (if (not= :ok status)
      {:briefing nil :dropped 0 :status status}
      (let [dis   (vec (:disambiguations data))
            valid (filterv #(and (g/exists? model (str (:use %)))
                                 (or (nil? (blank->nil (:not %)))
                                     (g/exists? model (str (:not %)))))
                           dis)
            sums  (filterv #(and (seq (str (:module %))) (seq (str (:summary %))))
                           (vec (:module_summaries data)))]
        {:briefing {:module-summaries sums
                    :disambiguations  valid
                    :cautions         (vec (take 5 (map str (:cautions data))))}
         :dropped (- (count dis) (count valid))
         :status :ok}))))

(defn briefing-text
  "Render a parsed briefing as Markdown-ish text — both the block injected into
  mapping prompts and the editable artifact the user curates."
  [{:keys [module-summaries disambiguations cautions]}]
  (str/trim
   (str
    (when (seq module-summaries)
      (str "Module summaries:\n"
           (str/join "\n" (for [m module-summaries]
                            (str "- " (:module m) ": " (:summary m))))
           "\n"))
    (when (seq disambiguations)
      (str "Disambiguation rules:\n"
           (str/join "\n" (for [d disambiguations]
                            (str "- Prefer " (:use d)
                                 (when (blank->nil (:not d)) (str " over " (:not d)))
                                 ": " (:rule d))))
           "\n"))
    (when (seq cautions)
      (str "Cautions:\n" (str/join "\n" (map #(str "- " %) cautions)) "\n")))))
