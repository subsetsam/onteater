(ns onteater.model.mapping
  "The scenario→ontology mapping session model and pure operations.

  A mapping session records how elements of a plain-text/Markdown scenario map onto
  ontology nodes, as proposed by an LLM and curated by the user. Entries are
  anchored to the scenario by **exact excerpt + occurrence index** (never character
  offsets — LLMs cannot count characters, and offsets break under Markdown
  rendering). Sessions persist to their own `*.onteater-mapping.json` file; the
  ontology file is never polluted.

  Session shape:

    {:id           uuid
     :scenario     {:title str :text str :source-file str}
     :ontology-ref {:file str :title str :hash str}   ; guards against stale mappings
     :model        str                                 ; ollama model used
     :entries      [Entry ...]
     :chat         [...]                                ; per-session transcript (§6.5)
     :unmapped     {:scenario-elements [str] :notes str}}

  Entry shape:

    {:id         uuid
     :excerpt    str          ; exact quote from the scenario
     :occurrence int          ; which occurrence of the quote (1-based)
     :node-id    str          ; target ontology node id
     :relation   kw           ; :instance-of | :mentions | :evidence-for
     :confidence number       ; 0..1 from the LLM
     :rationale  str          ; one-paragraph justification
     :status     kw           ; :proposed | :accepted | :rejected | :forced
     :flags      #{kw}         ; validation flags, e.g. :invalid-target :excerpt-not-found
     :history    [Entry ...]}  ; prior versions of this entry

  Everything here is pure data manipulation (uses `random-uuid` for ids); no DOM,
  no LLM calls — those live in the effects/prompts layers."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def relations
  "Supported excerpt→node relation kinds, with UI labels."
  [[:instance-of  "instance of"]
   [:mentions     "mentions"]
   [:evidence-for "evidence for"]])

(defn new-session
  "Create an empty mapping session bound to a scenario and an ontology reference."
  [{:keys [scenario ontology-ref model]}]
  {:id           (random-uuid)
   :scenario     (merge {:title "Untitled scenario" :text "" :source-file nil} scenario)
   :ontology-ref (merge {:file nil :title nil :hash nil} ontology-ref)
   :model        model
   :entries      []
   :chat         []
   ;; Temporal mapping (onteater.model.timeline). Kept inline (rather than
   ;; requiring the timeline ns) so the session model has no upward dependency; the
   ;; timeline ns operates on this `{:events :relations}` map.
   :timeline     {:events [] :relations []}
   :unmapped     {:scenario-elements [] :notes nil}})

(defn new-entry
  "Build an entry, filling defaults. `:status` defaults to :proposed."
  [m]
  (merge {:id (random-uuid) :occurrence 1 :relation :instance-of
          :confidence 0.5 :rationale "" :status :proposed :flags #{} :history []}
         m))

;; --- entry CRUD -------------------------------------------------------------

(defn entry [session id] (first (filter #(= id (:id %)) (:entries session))))

(defn add-entry [session e]
  (update session :entries (fnil conj []) (new-entry e)))

(defn update-entry
  "Apply `f` to the entry with `id`, snapshotting the prior version into :history."
  [session id f & args]
  (update session :entries
          (fn [es]
            (mapv (fn [e]
                    (if (= id (:id e))
                      (let [snapshot (dissoc e :history)]
                        (-> (apply f e args)
                            (update :history (fnil conj []) snapshot)))
                      e))
                  es))))

(defn remove-entry [session id]
  (update session :entries (fn [es] (vec (remove #(= id (:id %)) es)))))

(defn set-status [session id status]
  (update-entry session id assoc :status status))

(defn force-entry
  "Pin an excerpt to a user-chosen node (status :forced). Forced entries survive
  and constrain subsequent LLM runs (§6.3)."
  [session id node-id]
  (update-entry session id assoc :node-id node-id :status :forced :flags #{}))

;; --- queries ----------------------------------------------------------------

(defn entries [session] (:entries session))

(defn by-status [session status]
  (filter #(= status (:status %)) (:entries session)))

(defn forced-entries
  "Entries the user has forced — fed back to the LLM as hard constraints."
  [session]
  (by-status session :forced))

(defn active-entries
  "Entries that count as part of the mapping (everything not rejected)."
  [session]
  (remove #(= :rejected (:status %)) (:entries session)))

(defn entries-for-node [session node-id]
  (filter #(= node-id (:node-id %)) (active-entries session)))

(defn group-by-module
  "Group active entries by the module of their target node (using `node->module`,
  a fn node-id -> module string). Returns an ordered map-like seq of
  [module entries]."
  [session node->module]
  (->> (active-entries session)
       (group-by (fn [e] (or (node->module (:node-id e)) "—")))
       (sort-by first)))

;; --- summary (Level 1) ------------------------------------------------------

(defn- paragraphs [text]
  (->> (str/split (or text "") #"\n\s*\n")
       (map str/trim)
       (remove str/blank?)))

(defn coverage
  "Fraction of scenario paragraphs that contain at least one active entry's
  excerpt. A simple, robust coverage proxy for the Level-1 strip."
  [session]
  (let [paras (paragraphs (get-in session [:scenario :text]))
        exs   (map :excerpt (active-entries session))]
    (if (empty? paras)
      0.0
      (/ (count (filter (fn [p] (some #(and (seq %) (str/includes? p %)) exs)) paras))
         (count paras)))))

(defn summary
  "Headline numbers for the summary strip: entry/node/module counts, status
  breakdown, coverage, and per-module entry counts (for the bar chart)."
  [session node->module]
  (let [active (active-entries session)]
    {:entries   (count active)
     :nodes     (count (distinct (map :node-id active)))
     :modules   (count (distinct (keep #(node->module (:node-id %)) active)))
     :coverage  (coverage session)
     :by-status (frequencies (map :status (:entries session)))
     :per-module (->> active
                      (group-by #(or (node->module (:node-id %)) "—"))
                      (map (fn [[m es]] {:module m :count (count es)}))
                      (sort-by :count >)
                      vec)}))

;; --- excerpt anchoring ------------------------------------------------------

(defn nth-occurrence
  "Character index of the `n`-th (1-based) occurrence of `excerpt` in `text`, or
  nil if it does not occur that many times. Exact substring match."
  [text excerpt n]
  (when (and (seq text) (seq excerpt) (pos? n))
    (loop [from 0 remaining n]
      (let [idx (str/index-of text excerpt from)]
        (cond
          (nil? idx) nil
          (= 1 remaining) idx
          :else (recur (+ idx (count excerpt)) (dec remaining)))))))

(defn normalize-match
  "Normalize text for excerpt matching: drop Markdown emphasis markers and collapse
  whitespace, so an excerpt quoted from the RENDERED prose (e.g. `tariff`) still
  matches the RAW source (e.g. `**tariff**`)."
  [s]
  (-> (or s "")
      (str/replace #"[*_`~]" "")
      (str/replace #"\s+" " ")
      str/trim))

(defn excerpt-locatable?
  "Is an entry's excerpt findable in `text`? Tries an exact match at the stated
  occurrence first, then a Markdown/whitespace-normalized substring match (robust
  to emphasis markers and reflowed whitespace)."
  [text {:keys [excerpt occurrence]}]
  (or (some? (nth-occurrence text excerpt (or occurrence 1)))
      (and (seq excerpt)
           (str/includes? (normalize-match text) (normalize-match excerpt)))))

;; --- merging LLM result sets ------------------------------------------------

(defn- entry-key [e] [(str/trim (str (:excerpt e))) (:node-id e)])

(def curated-statuses
  "Statuses that record a user decision — these entries survive a new mapping run
  and always win over an incoming LLM duplicate."
  #{:forced :accepted :rejected})

(defn merge-entries
  "Merge a freshly-parsed set of `incoming` entries into `existing`, de-duping by
  (excerpt, node-id). EVERY existing entry is preserved — chunks of one run
  accumulate — and an incoming duplicate of any existing entry is dropped, so the
  user's curated decisions are never overwritten. Stale proposals from a previous
  run are cleared once at run start (`clear-proposed`), not here. Returns the
  merged vector."
  [existing incoming]
  (let [existing-keys (into #{} (map entry-key) existing)]
    (->> incoming
         (reduce (fn [{:keys [acc seen]} e]
                   (let [k (entry-key e)]
                     (if (or (seen k) (existing-keys k))
                       {:acc acc :seen seen}
                       {:acc (conj acc (new-entry e)) :seen (conj seen k)})))
                 {:acc [] :seen #{}})
         :acc
         (into (vec existing)))))

(defn clear-proposed
  "Drop all non-curated entries from the session — a new mapping run replaces
  stale proposals, while forced/accepted/rejected entries (the user's decisions)
  survive and constrain the run."
  [session]
  (update session :entries
          (fn [es] (vec (filter #(curated-statuses (:status %)) es)))))

;; --- chat-driven mapping updates --------------------------------

(defn- match-entry
  "Find the active entry an update/remove op refers to: same excerpt (normalized),
  preferring one that also shares the node-id. Returns the entry or nil."
  [session {:keys [excerpt node-id]}]
  (let [nx (normalize-match excerpt)
        cands (filter #(= nx (normalize-match (:excerpt %))) (active-entries session))]
    (or (first (filter #(= node-id (:node-id %)) cands))
        (first cands))))

(defn forced-op-conflict?
  "Would applying `op` modify or remove a FORCED entry? Such ops are rejected
  client-side."
  [session {:keys [op entry]}]
  (and (#{:update :remove} op)
       (when-let [e (match-entry session entry)]
         (= :forced (:status e)))))

(defn apply-op
  "Apply one chat-proposed op to the session. :add inserts a new proposed entry;
  :update retargets/adjusts the matched entry (snapshotting history); :remove drops
  the matched entry. Ops conflicting with forced entries must be filtered by the
  caller first (see `forced-op-conflict?`)."
  [session {:keys [op entry]}]
  (case op
    :add    (add-entry session (assoc entry :status :proposed))
    :update (if-let [e (match-entry session entry)]
              (update-entry session (:id e) merge
                            (select-keys entry [:node-id :relation :confidence :rationale])
                            {:status :proposed})
              (add-entry session (assoc entry :status :proposed)))
    :remove (if-let [e (match-entry session entry)]
              (remove-entry session (:id e))
              session)
    session))

;; --- session file serialization (*.onteater-mapping.json) -------------------

(defn- restore-entry [e]
  (cond-> e
    (:relation e) (update :relation keyword)
    (:status e)   (update :status keyword)
    true          (update :flags #(set (map keyword %)))
    (:history e)  (update :history (fn [hs] (mapv restore-entry hs)))))

(defn session->json
  "Serialize a session to pretty JSON text. clj->js turns keyword values/keys into
  strings and sets into arrays; we only need to stringify uuids first."
  [session]
  (js/JSON.stringify
   (clj->js (walk/postwalk #(if (uuid? %) (str %) %) session))
   nil 2))

(defn- restore-event
  "Re-keywordize a timeline event after JSON round-trip (clj->js stringified its
  keyword values and turned its flag-set into an array). `:node-id`/`:nearest` stay
  strings (or nil, a valid untyped gap)."
  [e]
  (cond-> e
    (:status e) (update :status keyword)
    true        (update :flags #(set (map keyword %)))
    (:when e)   (update :when (fn [w] (cond-> w
                                        (:kind w)      (update :kind keyword)
                                        (:precision w) (update :precision keyword))))
    (:history e) (update :history (fn [hs] (mapv restore-event hs)))))

(defn- restore-relation [r]
  (cond-> r
    (:type r)   (update :type keyword)
    (:status r) (update :status keyword)
    true        (update :flags #(set (map keyword %)))
    (:history r) (update :history (fn [hs] (mapv restore-relation hs)))))

(defn json->session
  "Parse a *.onteater-mapping.json string back into a session (ids stay strings —
  they are only used for identity). Restores entry, chat, and timeline keyword
  fields. Sessions written before the timeline feature simply have no `:timeline`
  key — they load unchanged (no migration needed)."
  [s]
  (let [data (js->clj (js/JSON.parse s) :keywordize-keys true)]
    (-> data
        (update :entries #(mapv restore-entry %))
        (update :timeline (fn [t]
                            (-> (or t {:events [] :relations []})
                                (update :events #(mapv restore-event (or % [])))
                                (update :relations #(mapv restore-relation (or % [])))))))))
