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
            [onteater.model.mapping :as mapping]))

;; --- token budgeting --------------------------------------------------------

(defn estimate-tokens
  "Very rough token estimate (~4 chars/token). Good enough to decide whether to
  fall back to two-stage mapping or to chunk the scenario."
  [s]
  (quot (count (or s "")) 4))

;; --- ontology compaction ----------------------------------------------------

(defn- truncate [s n]
  (let [s (str s)]
    (if (> (count s) n) (str (str/trimr (subs s 0 (dec n))) "…") s)))

(defn- node-line
  "One compact line for a node: `id | kind | label | ⊂ parent | gloss(truncated)`."
  [model {:keys [id kind label gloss] :as _node} gloss-len]
  (let [parent (first (sort (g/parents model id)))
        parts  [id (name (or kind :class)) label
                (when parent (str "⊂ " parent))
                (when (seq gloss) (truncate gloss gloss-len))]]
    (str/join " | " (remove nil? parts))))

(defn compact-ontology
  "Build the compact schema view the LLM sees. Groups real (non-external) nodes by
  module, one line each, then lists the relation properties. `gloss-len` truncates
  glosses to keep the prompt small."
  [model & [{:keys [gloss-len] :or {gloss-len 90}}]]
  (let [real (remove :external? (g/nodes model))
        classes (filter #(not= :property (:kind %)) real)
        props   (filter #(= :property (:kind %)) real)
        by-mod  (->> classes (group-by :module) (sort-by first))
        section (fn [title lines]
                  (when (seq lines)
                    (str "## " title "\n" (str/join "\n" lines) "\n")))]
    (str
     "ONTOLOGY: " (get-in model [:meta :title] "Ontology")
     "\nFormat per line:  id | kind | label | ⊂ parent | gloss\n\n"
     (str/join "\n"
               (for [[m nodes] by-mod]
                 (section (str "Module " (or m "—"))
                          (map #(node-line model % gloss-len) (sort-by :id nodes)))))
     (when (seq props)
       (str "\n" (section "Relations (object properties)"
                          (map (fn [p] (str (:id p) " | " (:label p))) (sort-by :id props))))))))

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

(defn mapping-schema
  "JSON schema for Ollama structured outputs constraining the mapping response.
  Eliminates most parse failures."
  []
  {:type "object"
   :properties
   {:entries
    {:type "array"
     :items
     {:type "object"
      :properties {:excerpt    {:type "string"}
                   :occurrence {:type "integer" :minimum 1}
                   :node_id    {:type "string"}
                   :relation   {:type "string" :enum relation-enum}
                   :confidence {:type "number" :minimum 0 :maximum 1}
                   :rationale  {:type "string"}}
      :required ["excerpt" "node_id" "relation" "confidence" "rationale"]}}
    :unmapped {:type "array" :items {:type "string"}}}
   :required ["entries"]})

(defn- forced-constraints-text [model session]
  (let [forced (mapping/forced-entries session)]
    (when (seq forced)
      (str "\n\nHARD CONSTRAINTS — the user has FORCED these mappings. You MUST keep "
           "each one exactly as given and echo it back unchanged:\n"
           (str/join "\n"
                     (for [e forced]
                       (str "- \"" (:excerpt e) "\" (occurrence " (:occurrence e) ") -> "
                            (:node-id e) " [" (name (:relation e)) "]")))))))

(defn system-prompt
  "The mapping system prompt: role, compacted ontology, instructions, and any
  forced constraints."
  [model session]
  (str
   "You are an expert in applied ontology and knowledge mapping. You map elements of a "
   "scenario onto a fixed ontology.\n\n"
   (compact-ontology model)
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
   "- List genuinely significant scenario elements you could NOT place in `unmapped`.\n\n"
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
   (forced-constraints-text model session)))

(defn mapping-messages
  "Build the /api/chat message list for one scenario chunk."
  [model session chunk-text]
  [{:role "system" :content (system-prompt model session)}
   {:role "user" :content (str "SCENARIO:\n\n" chunk-text)}])

;; --- response parsing + validation ------------------------------------------

(defn- ->entry
  "Turn one raw structured-output entry map (string/keyword keys) into a mapping
  entry, tolerating key spelling variants."
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

(defn chat-system-prompt
  "Assemble the chat system context each turn: role, compacted
  ontology, scenario, the CURRENT mapping state (forced entries highlighted), and
  the mapping-update protocol the model must use to propose changes."
  [model session]
  (str
   "You are an expert assistant helping a user curate a mapping from a scenario "
   "onto the loaded ontology. Answer questions about the mapping and the "
   "ontology concisely.\n\n"
   (compact-ontology model {:gloss-len 60})
   "\n\nSCENARIO:\n" (get-in session [:scenario :text])
   "\n\nCURRENT MAPPING:\n" (entries-summary session)
   "\n\nWhen — and ONLY when — you want to change the mapping, emit a fenced code "
   "block tagged `mapping-update` containing JSON of the form:\n"
   "```mapping-update\n"
   "{\"ops\": [{\"op\": \"add|update|remove\", \"entry\": {\"excerpt\": \"...\", "
   "\"node_id\": \"geo:...\", \"relation\": \"instance-of\", \"confidence\": 0.8, "
   "\"rationale\": \"...\"}}], \"reason\": \"why\"}\n"
   "```\n"
   "Rules: use only node ids that exist above; never modify FORCED entries; keep "
   "prose and the block separate. The user reviews every change before it applies."))

(defn chat-messages
  "Full message list for a chat turn: the assembled system context, prior transcript
  (role/content pairs), then the new user message."
  [model session transcript user-text]
  (into [{:role "system" :content (chat-system-prompt model session)}]
        (conj (mapv (fn [m] {:role (name (:role m)) :content (:content m)}) transcript)
              {:role "user" :content user-text})))

(def ^:private mapping-update-re
  #"(?s)```mapping-update\s*(\{.*?\})\s*```")

(defn parse-mapping-updates
  "Extract every ```mapping-update``` block from an assistant `content` string.
  Returns {:prose <content with blocks removed> :updates [{:ops [...] :reason s} ...]}.
  Each op's entry is normalized into a mapping entry. Never throws."
  [content]
  (let [content (or content "")
        matches (re-seq mapping-update-re content)
        updates (keep (fn [[_ json]]
                        (try
                          (let [data (js->clj (js/JSON.parse json) :keywordize-keys true)
                                ops  (mapv (fn [op]
                                             {:op (keyword (:op op))
                                              :entry (when (:entry op) (->entry (:entry op)))})
                                           (:ops data))]
                            (when (seq ops) {:ops ops :reason (:reason data)}))
                          (catch :default _ nil)))
                      matches)
        prose (str/trim (str/replace content mapping-update-re ""))]
    {:prose prose :updates (vec updates)}))
