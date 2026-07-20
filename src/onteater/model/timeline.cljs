(ns onteater.model.timeline
  "The temporal-mapping (timeline) domain model and its pure analyses.

  Where `onteater.model.mapping` answers *what maps to what*, this namespace answers
  *what happened, in what order, driven by what*. A timeline lives inside a mapping
  session under `:timeline` and has exactly two collections:

    {:events    [Event ...]
     :relations [Relation ...]}

  Event shape (see PLAN §6.7.1):

    {:id           uuid
     :label        str                 ; short display label
     :excerpt      str  :occurrence int ; anchored exactly as mapping entries (§6.2)
     :node-id      str|nil             ; ontology occurrent class; nil => untyped (a gap)
     :nearest      str                 ; best abstract fit when node-id is nil
     :why-no-fit   str                 ; one line, when node-id is nil
     :participants [{:entity str       ; scenario-level actor name
                     :node-id str       ; the ontology node it maps to (via a mapping entry)
                     :role-id str|nil}] ; role class; nil => unroled (a gap)
     :when         {:kind :instant|:interval|:unknown
                    :start str :end str
                    :precision :day|:month|:year
                    :narrative-index int}   ; order of appearance — ALWAYS present
     :confidence   number :rationale str
     :status       :proposed|:accepted|:rejected|:forced
     :flags        #{kw} :history [Event ...]}

  Relation shape:

    {:id uuid :source event-id :target event-id
     :type        :precedes|:causes|:enables|:responds-to|:part-of|:terminates
     :property-id str|nil              ; ontology property; nil => untyped (a gap)
     :confidence number :rationale str :status kw :flags #{kw} :history [...]}

  Events plus their **non-`:part-of`** relations form a directed graph expected to
  be a DAG but *allowed not to be*: forks, joins, parallel threads, and (rarely)
  cycles are all first-class. `:part-of` is handled separately — it nests events
  into compound episodes and never participates in the dependency graph.

  Everything here is pure (no DOM, no LLM, only data + `random-uuid`). The ordering
  cascade, dependency cones, path enumeration, lane assignment, and the gap report
  are all unit-tested headlessly — this namespace joins the golden domain core.

  ## Ontology discovery (format-agnostic)

  The feature must not hard-code the geo ontology's ids. The occurrent classes,
  role classes, stage states, and relation properties the timeline pass targets are
  *discovered* from the loaded model by structure: occurrent classes are the
  transitive `:subclass-of` descendants of any class whose name reads like a
  process/occurrent/event root; role classes descend from a `Role`-named root; etc.
  These helpers back both the extraction compaction (§6.7.2) and the gap report."
  (:refer-clojure :exclude [ancestors descendants])
  (:require [clojure.string :as str]
            [onteater.model.graph :as g]))

;; ---------------------------------------------------------------------------
;; Ontology vocabulary discovery (pure, structural — no hard-coded ids)
;; ---------------------------------------------------------------------------

(defn local-name
  "The local part of a prefixed id/label — everything after the last ':'.
  `\"geo:Act\"` -> `\"Act\"`, `\"Process\"` -> `\"Process\"`."
  [s]
  (let [s (str s)] (if-let [i (str/last-index-of s ":")] (subs s (inc i)) s)))

(def ^:private occurrent-root-re
  ;; A class reads as an occurrent *root* if its name mentions one of these stems.
  ;; The spine's leaves (geo:Act, geo:Imposition, …) need not match — they are
  ;; pulled in as subclass-descendants of a matching root (e.g. bfo:Process).
  #"(?i)process|occurrent|event|episode|activity|happening|proceeding")

(def ^:private role-root-re #"(?i)(^|[^a-z])role([^a-z]|$)")
(def ^:private stage-re #"(?i)stage|phase")

(def ^:private disposition-root-re
  ;; A class reads as a disposition *root* if its name mentions a power-word stem
  ;; or the BFO term itself. Like the occurrent roots, leaves are pulled in as
  ;; subclass-descendants, so only the family heads need to match. Substring
  ;; matching is a deliberately weak signal (same discipline as
  ;; `occurrent-root-re`): a continuant named e.g. `MilitaryCapability` would
  ;; over-match — acceptable for the advisory routing-guide bullet this feeds,
  ;; which guides, never constrains.
  #"(?i)disposition|leverage|dependenc|vulnerab|resilien|credibil|capabilit")

(defn- name-matches? [re node]
  (boolean (or (re-find re (local-name (:id node)))
               (and (:label node) (re-find re (local-name (:label node)))))))

(defn occurrent-root-ids
  "Ids of classes/stubs whose own name reads like an occurrent/process root."
  [model]
  (into #{} (comp (filter #(not= :property (:kind %)))
                  (filter #(name-matches? occurrent-root-re %))
                  (map :id))
        (g/nodes model)))

(defn occurrent-ids
  "The set of ontology class ids that are occurrents: the transitive
  `:subclass-of` descendants (inclusive) of every occurrent root. Empty when the
  ontology models no process spine."
  [model]
  (into #{} (mapcat #(g/subtree model %)) (occurrent-root-ids model)))

(defn role-ids
  "Ids of role classes: descendants of any class whose name is/contains `Role`."
  [model]
  (let [roots (into #{} (comp (filter #(not= :property (:kind %)))
                              (filter #(name-matches? role-root-re %))
                              (map :id))
                    (g/nodes model))]
    (into #{} (mapcat #(g/subtree model %)) roots)))

(defn disposition-ids
  "Ids of disposition classes — the power-word vocabulary (leverage, dependence,
  vulnerability, …): descendants of any class whose name matches a disposition
  stem. Backs the routing guide's 'power-words are dispositions, not agents'
  bullet; empty when the ontology models none."
  [model]
  (let [roots (into #{} (comp (filter #(not= :property (:kind %)))
                              (filter #(name-matches? disposition-root-re %))
                              (map :id))
                    (g/nodes model))]
    (into #{} (mapcat #(g/subtree model %)) roots)))

(defn stage-ids
  "Ids of stage-machine states: occurrent classes whose name mentions stage/phase.
  A weak but useful signal for the stage-mismatch gap check."
  [model]
  (let [occ (occurrent-ids model)]
    (into #{} (comp (filter #(occ (:id %)))
                    (filter #(name-matches? stage-re %))
                    (map :id))
          (g/nodes model))))

(defn relation-property-ids
  "Ids of every `:property` node — the ontology's relation vocabulary."
  [model]
  (into #{} (comp (filter #(= :property (:kind %))) (map :id)) (g/nodes model)))

;; ---------------------------------------------------------------------------
;; Construction
;; ---------------------------------------------------------------------------

(def relation-types
  "Built-in temporal/causal relation types with UI labels and a coarse family used
  for edge styling (§6.7.3). `:part-of` is nesting, not a dependency edge."
  [[:precedes       "precedes"        :temporal]
   [:causes         "causes"          :causal]
   [:enables        "enables"         :causal]
   [:responds-to    "responds to"     :response]
   [:terminates     "terminates"      :causal]
   [:part-of        "part of"         :nesting]])

(def relation-type-set (into #{} (map first) relation-types))

(defn new-timeline
  "An empty timeline (a session's `:timeline` value)."
  []
  {:events [] :relations []})

(defn new-event
  "Build an event, filling defaults. `:when` always carries a `:narrative-index`
  (the ordering of last resort). `:status` defaults to `:proposed`."
  [m]
  (let [w (merge {:kind :unknown :start nil :end nil :precision nil :narrative-index 0}
                 (:when m))]
    (merge {:id (random-uuid) :label "" :excerpt "" :occurrence 1
            :node-id nil :nearest nil :why-no-fit nil
            :participants [] :confidence 0.5 :rationale ""
            :status :proposed :flags #{} :history []}
           (assoc m :when w))))

(defn new-relation
  "Build a relation, filling defaults. `:type` defaults to `:precedes`."
  [m]
  (merge {:id (random-uuid) :type :precedes :property-id nil
          :confidence 0.5 :rationale "" :status :proposed :flags #{} :history []}
         m))

;; ---------------------------------------------------------------------------
;; Accessors + CRUD (operate on a timeline map)
;; ---------------------------------------------------------------------------

(defn events    [tl] (:events tl))
(defn relations [tl] (:relations tl))
(defn event     [tl id] (first (filter #(= id (:id %)) (:events tl))))
(defn relation  [tl id] (first (filter #(= id (:id %)) (:relations tl))))

(defn active-events
  "Events that count as part of the timeline (everything not `:rejected`)."
  [tl]
  (remove #(= :rejected (:status %)) (:events tl)))

(defn active-relations [tl]
  (remove #(= :rejected (:status %)) (:relations tl)))

(defn forced-events    [tl] (filter #(= :forced (:status %)) (:events tl)))
(defn forced-relations [tl] (filter #(= :forced (:status %)) (:relations tl)))

(defn add-event [tl e] (update tl :events (fnil conj []) (new-event e)))
(defn add-relation [tl r] (update tl :relations (fnil conj []) (new-relation r)))

(defn- update-in-coll
  "Apply `f` to the item with `id` in `coll-key`, snapshotting the prior version
  (minus its own `:history`) into `:history`."
  [tl coll-key id f & args]
  (update tl coll-key
          (fn [xs]
            (mapv (fn [x]
                    (if (= id (:id x))
                      (let [snap (dissoc x :history)]
                        (-> (apply f x args) (update :history (fnil conj []) snap)))
                      x))
                  (or xs [])))))

(defn update-event [tl id f & args] (apply update-in-coll tl :events id f args))
(defn update-relation [tl id f & args] (apply update-in-coll tl :relations id f args))

(defn remove-relation [tl id]
  (update tl :relations (fn [rs] (vec (remove #(= id (:id %)) rs)))))

(defn remove-event
  "Remove an event and cascade-delete every relation touching it."
  [tl id]
  (-> tl
      (update :events (fn [es] (vec (remove #(= id (:id %)) es))))
      (update :relations (fn [rs] (vec (remove #(or (= id (:source %)) (= id (:target %))) rs))))))

(defn set-event-status    [tl id status] (update-event tl id assoc :status status))
(defn set-relation-status [tl id status] (update-relation tl id assoc :status status))

(defn force-event
  "Pin an event's typing to a user-chosen occurrent class (status `:forced`).
  Forced events survive and constrain subsequent extraction runs (§6.7.2)."
  [tl id node-id]
  (update-event tl id assoc :node-id node-id :nearest nil :why-no-fit nil
                :status :forced :flags #{}))

(defn force-relation
  "Pin a relation's type/property (status `:forced`)."
  [tl id type property-id]
  (update-relation tl id assoc :type type :property-id property-id
                   :status :forced :flags #{}))

;; ---------------------------------------------------------------------------
;; The dependency graph (events + non-:part-of relations)
;; ---------------------------------------------------------------------------

(defn dependency-relations
  "Active relations that form the dependency DAG — everything except `:part-of`
  nesting. `source` *depends on nothing*; it is the cause/precedent, `target` the
  consequence (source precedes/causes/enables/… target)."
  [tl]
  (remove #(= :part-of (:type %)) (active-relations tl)))

(defn part-of-relations [tl]
  (filter #(= :part-of (:type %)) (active-relations tl)))

(defn- adjacency
  "Forward adjacency map event-id -> [successor-ids] over `rels` (source->target)."
  [rels]
  (reduce (fn [m r] (update m (:source r) (fnil conj []) (:target r))) {} rels))

(defn- reverse-adjacency
  "Backward adjacency event-id -> [predecessor-ids]."
  [rels]
  (reduce (fn [m r] (update m (:target r) (fnil conj []) (:source r))) {} rels))

(defn cycle-nodes
  "The set of event ids that participate in a cycle of the dependency graph
  (nontrivial strongly-connected components + self-loops). Tarjan's SCC. These are
  badged in the UI and their internal ordering falls back to date/narrative order —
  they are never rejected (feedback loops are real in geoeconomics)."
  [tl]
  (let [rels  (dependency-relations tl)
        ids   (mapv :id (active-events tl))
        succ  (adjacency rels)
        self  (into #{} (comp (filter #(= (:source %) (:target %))) (map :source)) rels)
        state (atom {:idx 0 :stack [] :on-stack #{} :index {} :low {} :sccs []})]
    (letfn [(strong [v]
              (swap! state (fn [s] (-> s (assoc-in [:index v] (:idx s))
                                       (assoc-in [:low v] (:idx s))
                                       (update :idx inc)
                                       (update :stack conj v)
                                       (update :on-stack conj v))))
              (doseq [w (get succ v)]
                (cond
                  (not (contains? (:index @state) w))
                  (do (strong w)
                      (swap! state update-in [:low v] min (get-in @state [:low w])))
                  (contains? (:on-stack @state) w)
                  (swap! state update-in [:low v] min (get-in @state [:index w]))))
              (when (= (get-in @state [:low v]) (get-in @state [:index v]))
                (let [stk (:stack @state)
                      i   (.lastIndexOf (to-array stk) v)
                      comp (subvec stk i)]
                  (swap! state (fn [s] (-> s (assoc :stack (subvec stk 0 i))
                                           (update :on-stack #(reduce disj % comp))
                                           (update :sccs conj comp)))))))]
      (doseq [v ids] (when-not (contains? (:index @state) v) (strong v)))
      (into self
            (comp (filter #(> (count %) 1)) cat)
            (:sccs @state)))))

(defn toposort
  "Cycle-tolerant topological order of the active events over the dependency graph.
  Returns `{:order [event-id ...] :cycles #{cyclic-event-id ...}}`.

  Kahn's algorithm with a deterministic tie-break: among currently-ready events the
  one with the smallest ordering key (date, then narrative-index) is emitted first,
  so parallel threads interleave by time/appearance rather than by hash order. When
  the ready set empties but events remain (a cycle), the remaining event with the
  smallest ordering key is force-emitted to break the deadlock — ordering inside a
  cycle thus falls back to date/narrative order, exactly as §6.7.1 requires. Never
  loops forever, never drops an event."
  [tl]
  (let [evs   (active-events tl)
        ids   (mapv :id evs)
        by-id (into {} (map (juxt :id identity)) evs)
        rels  (dependency-relations tl)
        succ  (adjacency rels)
        cyc   (cycle-nodes tl)
        okey  (fn [id] (let [e (by-id id)
                             w (:when e)]
                         [(or (:start w) "~") (or (:narrative-index w) 0) (str id)]))
        indeg0 (reduce (fn [m r] (update m (:target r) (fnil inc 0)))
                       (zipmap ids (repeat 0)) rels)]
    (loop [indeg indeg0 remaining (set ids) order []]
      (if (empty? remaining)
        {:order order :cycles cyc}
        (let [ready (filter #(and (remaining %) (zero? (get indeg % 0))) remaining)
              pick  (if (seq ready)
                      (first (sort-by okey ready))
                      ;; deadlock (cycle): force the smallest-keyed remaining node
                      (first (sort-by okey remaining)))
              indeg' (reduce (fn [m t] (update m t (fnil dec 0))) indeg (get succ pick))]
          (recur indeg' (disj remaining pick) (conj order pick)))))))

;; ---------------------------------------------------------------------------
;; Dependency cones + paths
;; ---------------------------------------------------------------------------

(defn- reachable
  "Transitive closure of `id` over adjacency `adj` (exclusive of `id`). Cycle-safe."
  [adj id]
  (loop [stack (vec (get adj id)) seen #{}]
    (if (empty? stack)
      seen
      (let [x (peek stack) stack (pop stack)]
        (if (seen x)
          (recur stack seen)
          (recur (into stack (get adj x)) (conj seen x)))))))

(defn ancestors
  "Event ids `id` transitively depends on (its upstream cone). Cycle-safe."
  [tl id]
  (reachable (reverse-adjacency (dependency-relations tl)) id))

(defn descendants
  "Event ids that transitively depend on `id` (its downstream cone). Cycle-safe."
  [tl id]
  (reachable (adjacency (dependency-relations tl)) id))

(defn dependency-cone
  "`{:ancestors #{} :descendants #{}}` for `id` — the dependency-analysis instrument
  (objective 1). Upstream = what it depended on, downstream = what depends on it."
  [tl id]
  {:ancestors (ancestors tl id)
   :descendants (descendants tl id)})

(defn paths-between
  "All simple directed paths from event `a` to event `b` over the dependency graph,
  each a vector of event ids `[a … b]`. Capped at `limit` paths (default 24) so a
  dense graph can't explode. Returns `[]` when unreachable."
  ([tl a b] (paths-between tl a b 24))
  ([tl a b limit]
   (let [succ (adjacency (dependency-relations tl))
         out  (atom [])]
     (letfn [(go [node path visiting]
               (when (< (count @out) limit)
                 (if (= node b)
                   (swap! out conj path)
                   (doseq [n (get succ node)
                           :when (and (not (visiting n)) (< (count @out) limit))]
                     (go n (conj path n) (conj visiting n))))))]
       (when (and a b (not= a b)) (go a [a] #{a}))
       @out))))

(defn relation-between
  "The active dependency relation directly linking `a`->`b`, if any (for labelling a
  path segment with its type/property)."
  [tl a b]
  (first (filter #(and (= a (:source %)) (= b (:target %))) (dependency-relations tl))))

;; ---------------------------------------------------------------------------
;; Entity views
;; ---------------------------------------------------------------------------

(defn event-entities
  "Distinct participant entity names of an event, in participant order."
  [e]
  (into [] (comp (map :entity) (remove str/blank?) (distinct)) (:participants e)))

(defn entities
  "All distinct participant entity names across the active timeline, sorted."
  [tl]
  (->> (active-events tl) (mapcat event-entities) distinct sort vec))

(defn entity-timeline
  "Active events in which `entity` participates, in the timeline's computed order."
  [tl entity]
  (let [order (into {} (map-indexed (fn [i id] [id i])) (:order (toposort tl)))
        touches? (fn [e] (some #(= entity (:entity %)) (:participants e)))]
    (->> (active-events tl)
         (filter touches?)
         (sort-by #(get order (:id %) 0))
         vec)))

(defn entity-dependency-matrix
  "An entity×entity count of event-mediated dependencies: for each active dependency
  relation source->target, every (entity-of-source, entity-of-target) pair with
  distinct entities is tallied. Returns
  `{:entities [name ...] :cells {[from to] count}}` — the heat matrix (objective 1)."
  [tl]
  (let [by-id (into {} (map (juxt :id identity)) (active-events tl))
        cells (reduce
               (fn [m r]
                 (let [src (get by-id (:source r)) tgt (get by-id (:target r))]
                   (reduce (fn [m [a b]]
                             (if (and a b (not= a b)) (update m [a b] (fnil inc 0)) m))
                           m
                           (for [a (event-entities src) b (event-entities tgt)] [a b]))))
               {}
               (dependency-relations tl))]
    {:entities (entities tl) :cells cells}))

;; ---------------------------------------------------------------------------
;; The ordering cascade (dates -> topological order -> narrative index)
;; ---------------------------------------------------------------------------

(defn parse-date
  "Parse an ISO-ish date string into a comparable number `yyyymmdd`, tolerating
  `YYYY`, `YYYY-MM`, and `YYYY-MM-DD`. Missing parts default to the start of the
  period (month/day = 1). Returns nil for blank/unparseable input — never throws."
  [s]
  (when (and s (seq (str/trim (str s))))
    (let [m (re-matches #"(\d{4})(?:-(\d{1,2}))?(?:-(\d{1,2}))?.*" (str/trim (str s)))]
      (when m
        (let [[_ y mo d] m]
          (+ (* 10000 (js/parseInt y 10))
             (* 100 (if mo (js/parseInt mo 10) 1))
             (if d (js/parseInt d 10) 1)))))))

(defn event-date-value
  "Comparable date value for an event's `:when` (its `:start`), or nil if undated."
  [e]
  (parse-date (get-in e [:when :start])))

(defn ordering
  "The ordering cascade → an ordinal slotting of every active event onto an integer
  x-axis. Returns a vector of `{:id :index :dated? :value}` in slot order plus is
  usable directly by the swimlane axis (§6.7.3):

  1. Start from the cycle-tolerant topological order (respects causal/temporal
     relations, and is already date/narrative tie-broken).
  2. Give each event a *time value*: its parsed date if dated; otherwise interpolate
     one strictly between the nearest dated predecessor and successor along the topo
     order (so undated events slot into the ordinal gap between their dated
     neighbours). Leading/trailing undated runs step just outside the known range.
  3. Sort by (time-value, topo-index) and assign contiguous slot indices.

  `:dated?` marks which slots are metric (a real `d3.scaleTime` position) versus
  merely ordinal — the axis renders the two honestly (hatched ordinal segments)."
  [tl]
  (let [order   (:order (toposort tl))
        by-id   (into {} (map (juxt :id identity)) (active-events tl))
        topo-ix (into {} (map-indexed (fn [i id] [id i])) order)
        n       (count order)
        raw     (mapv (fn [id] (event-date-value (by-id id))) order)
        ;; forward floor: greatest dated value at or before position i
        floors  (reductions (fn [acc v] (if v (max (or acc v) v) acc)) nil raw)
        floors  (vec (rest floors))                 ; align to positions 0..n-1
        ;; backward ceil: least dated value at or after position i
        ceils   (->> (reverse raw)
                     (reductions (fn [acc v] (if v (min (or acc v) v) acc)) nil)
                     rest reverse vec)
        values  (mapv (fn [i v]
                        (if v
                          v
                          (let [fl (nth floors i) ce (nth ceils i)]
                            (cond
                              (and fl ce) (+ fl (* (/ (inc (mod i n)) (+ n 1.0)) (- ce fl 0.0)) 0.0001)
                              fl          (+ fl (* 0.001 (inc i)))
                              ce          (- ce (* 0.001 (- n i)))
                              :else       (* 1.0 i)))))       ; wholly undated timeline
                      (range n) raw)
        rows    (map (fn [id v] {:id id :value v :dated? (some? (event-date-value (by-id id)))
                                 :topo (topo-ix id)})
                     order values)
        sorted  (sort-by (juxt :value :topo) rows)]
    (into [] (map-indexed (fn [i r] (assoc (dissoc r :topo) :index i))) sorted)))

;; ---------------------------------------------------------------------------
;; Lane assignment (the y-axis) — §6.7.3 / §12.13(b)
;; ---------------------------------------------------------------------------

(defn- event-lane-key
  "The lane an event belongs to under `grouping`, using `node->module` (id->module)
  for module grouping and `parent-of` (event-id -> containing :part-of event-id) for
  episode grouping. Multi-participant events lane by their PRIMARY (first)
  participant. Returns [key label]."
  [grouping node->module parent-of tl e]
  (case grouping
    :single [:all "All events"]
    :module (let [m (get node->module (:node-id e))] [(or m "—") (or m "—")])
    :episode (let [p (get parent-of (:id e))
                   pe (event tl p)]
               (if pe [p (or (:label pe) "Episode")] [:none "(top level)"]))
    ;; :entity (default)
    (let [ent (first (event-entities e))]
      [(or ent "—") (or ent "(no participant)")])))

(defn assign-lanes
  "Assign active events to lanes for the swimlane view. `opts`:
    :grouping     :entity | :module | :episode | :single   (default :entity)
    :node->module id -> module string (for :module grouping)
    :collapse-threshold  lanes with fewer events than this are marked
                         `:auto-collapsed?` (default 1 — i.e. nothing auto-collapses;
                         the UI raises it for busy scenarios)

  Returns `{:lanes [{:key :label :index :event-ids [...] :auto-collapsed?}] :event->lane {id -> lane-key}}`.
  Lanes are ordered by first appearance in the timeline's computed order, so the
  busiest early actors sit at the top."
  [tl {:keys [grouping node->module collapse-threshold] :or {grouping :entity collapse-threshold 1}}]
  (let [order    (:order (toposort tl))
        order-ix (into {} (map-indexed (fn [i id] [id i])) order)
        parent-of (reduce (fn [m r] (assoc m (:source r) (:target r)))
                          {} (part-of-relations tl))
        evs      (sort-by #(get order-ix (:id %) 0) (active-events tl))
        keyed    (map (fn [e] [e (event-lane-key grouping node->module parent-of tl e)]) evs)
        by-key   (reduce (fn [m [e [k lbl]]]
                           (-> m (update-in [k :event-ids] (fnil conj []) (:id e))
                               (update-in [k :label] #(or % lbl))
                               (update-in [k :first] #(min (or % 1e9) (get order-ix (:id e) 0)))))
                         {} keyed)
        lanes    (->> by-key
                      (sort-by (fn [[_ v]] (:first v)))
                      (map-indexed (fn [i [k v]]
                                     {:key k :label (:label v) :index i
                                      :event-ids (:event-ids v)
                                      :auto-collapsed? (< (count (:event-ids v)) collapse-threshold)})))
        e->lane  (into {} (for [l lanes eid (:event-ids l)] [eid (:key l)]))]
    {:lanes (vec lanes) :event->lane e->lane}))

;; ---------------------------------------------------------------------------
;; Layout — combine ordering (x) + lanes (y) + edge routing (§12.13)
;; ---------------------------------------------------------------------------

(defn route-edges
  "Edge routing for the active dependency relations given each event's `{:index :lane-index}`
  position (`pos`, id -> map). Pure geometry in ORDINAL units — the viz scales to px.

  Each routed edge is `{:id :source :target :type :property-id :status :untyped?
  :x1 :y1 :x2 :y2 :fan}` where `:fan` spreads the many edges leaving one source (or
  entering one target) so a fork/join fans out instead of overdrawing a single line.
  Relations with a nil `:property-id` are marked `:untyped?` (drawn dashed in the
  warning colour — a gap visible in place)."
  [tl pos]
  (let [rels     (dependency-relations tl)
        out-grp  (group-by :source rels)
        fan-of   (fn [r]
                   ;; index of this edge among those sharing its source, centred on 0
                   (let [sibs (get out-grp (:source r))
                         i    (.indexOf (to-array (mapv :id sibs)) (:id r))
                         c    (count sibs)]
                     (if (<= c 1) 0 (- i (/ (dec c) 2.0)))))]
    (into []
          (keep (fn [r]
                  (let [s (get pos (:source r)) t (get pos (:target r))]
                    (when (and s t)
                      {:id (:id r) :source (:source r) :target (:target r)
                       :type (:type r) :property-id (:property-id r) :status (:status r)
                       :untyped? (nil? (:property-id r))
                       :x1 (:index s) :y1 (:lane-index s)
                       :x2 (:index t) :y2 (:lane-index t)
                       :fan (fan-of r)}))))
          rels)))

(defn layout
  "Full pure layout of the timeline in ordinal units, ready for the viz layer to
  scale to pixels. `opts` are `assign-lanes` opts. Returns:

    {:events   [{:id :index :dated? :value :lane :lane-index + the event map}]
     :lanes    [lane ...]                 ; from assign-lanes
     :edges    [routed-edge ...]          ; from route-edges (dependency graph)
     :part-of  [{:child :parent} ...]     ; nesting relations, for container glyphs
     :cycles   #{cyclic-event-id ...}}"
  [tl opts]
  (let [ord        (ordering tl)
        {:keys [lanes event->lane]} (assign-lanes tl opts)
        lane-ix    (into {} (map (juxt :key :index)) lanes)
        by-id      (into {} (map (juxt :id identity)) (active-events tl))
        placed     (mapv (fn [{:keys [id index dated? value]}]
                           (let [lane (get event->lane id)]
                             (merge (get by-id id)
                                    {:id id :index index :dated? dated? :value value
                                     :lane lane :lane-index (get lane-ix lane 0)})))
                         ord)
        pos        (into {} (map (fn [e] [(:id e) (select-keys e [:index :lane-index])])) placed)
        part-of    (mapv (fn [r] {:child (:source r) :parent (:target r)}) (part-of-relations tl))]
    {:events placed :lanes lanes :edges (route-edges tl pos)
     :part-of part-of :cycles (:cycles (toposort tl))}))

;; ---------------------------------------------------------------------------
;; Gap report (objective 2 — measured by pure code, no LLM at report time) §6.7.4
;; ---------------------------------------------------------------------------

(defn- group-by-nearest [items]
  (->> items
       (group-by #(or (:nearest %) "—"))
       (map (fn [[k v]] {:nearest k :count (count v) :items (vec v)}))
       (sort-by :count >)
       vec))

(defn gap-report
  "Measure the ontology's *holes* against the mapped timeline — the completeness
  audit (objective 2). Pure over `tl` + `model`; no LLM in the loop. Returns:

    {:untyped-events   [{:nearest :count :items}]   ; events with node-id = nil
     :untyped-relations[{:nearest :count :items}]   ;   grouped by :type here
     :shallow-typings  [event ...]                  ; typed only to a class that
                                                    ;   still has leaf subclasses
     :unroled-participants [{:event :participant}]  ; participant with role-id = nil
     :coverage {:occurrent {:used :total :unused #{} }
                :properties {:used :total :unused #{}}
                :roles      {:used :total :unused #{}}}}

  Coverage is two-directional signal: classes never exercised hint at
  over-engineering; single classes absorbing many events hint at
  under-differentiation (surfaced as `:overloaded`)."
  [tl model]
  (let [evs        (active-events tl)
        rels       (active-relations tl)
        occ        (occurrent-ids model)
        props      (relation-property-ids model)
        roles      (role-ids model)
        untyped-ev (filter #(nil? (:node-id %)) evs)
        untyped-rel (filter #(nil? (:property-id %)) rels)
        shallow    (filter (fn [e]
                             (when-let [nid (:node-id e)]
                               (and (g/exists? model nid)
                                    (seq (g/children model nid))))) ; has leaf subclasses below it
                           evs)
        unroled    (for [e evs p (:participants e)
                         :when (and (seq (:entity p)) (nil? (:role-id p)))]
                     {:event (:id e) :label (:label e) :participant (:entity p)})
        used-occ   (into #{} (keep :node-id) evs)
        used-props (into #{} (keep :property-id) rels)
        used-roles (into #{} (comp (mapcat :participants) (keep :role-id)) evs)
        by-occ     (frequencies (keep :node-id evs))
        overloaded (->> by-occ (filter (fn [[_ c]] (>= c 3)))
                        (map (fn [[id c]] {:node-id id :count c})) (sort-by :count >) vec)]
    {:untyped-events    (group-by-nearest untyped-ev)
     :untyped-relations (->> untyped-rel
                             (group-by #(name (:type %)))
                             (map (fn [[k v]] {:nearest k :count (count v) :items (vec v)}))
                             (sort-by :count >) vec)
     :shallow-typings   (vec shallow)
     :unroled-participants (vec unroled)
     :coverage {:occurrent  {:used (count (filter occ used-occ)) :total (count occ)
                             :unused (into #{} (remove used-occ) occ)
                             :overloaded overloaded}
                :properties {:used (count (filter props used-props)) :total (count props)
                             :unused (into #{} (remove used-props) props)}
                :roles      {:used (count (filter roles used-roles)) :total (count roles)
                             :unused (into #{} (remove used-roles) roles)}}}))

(defn gap-count
  "Total number of distinct gap findings in a report — the headline the Gaps tab
  badges and the re-run flow compares before/after."
  [report]
  (+ (reduce + (map :count (:untyped-events report)))
     (reduce + (map :count (:untyped-relations report)))
     (count (:shallow-typings report))
     (count (:unroled-participants report))))

(defn gap-report-markdown
  "Render a gap report as Markdown, ready to paste into ontology revision notes
  (the exportable audit of §6.7.4)."
  [report title]
  (let [sec (fn [heading lines] (when (seq lines) (str "\n## " heading "\n" (str/join "\n" lines) "\n")))
        cov (:coverage report)]
    (str "# Gap report — " (or title "scenario") "\n"
         "\n" (gap-count report) " gap finding(s).\n"
         (sec "Untyped events (no fitting occurrent class)"
              (for [g (:untyped-events report) it (:items g)]
                (str "- **" (:label it) "** — nearest `" (:nearest g) "`"
                     (when (:why-no-fit it) (str ": " (:why-no-fit it))))))
         (sec "Untyped relations (no fitting property)"
              (for [g (:untyped-relations report)]
                (str "- " (:count g) "× `" (:nearest g) "` relation(s) with no ontology property")))
         (sec "Shallow typings (typed above available leaf classes)"
              (for [e (:shallow-typings report)] (str "- **" (:label e) "** typed to `" (:node-id e) "`")))
         (sec "Unroled participants"
              (for [u (:unroled-participants report)]
                (str "- **" (:participant u) "** in “" (:label u) "” has no fitting role class")))
         (str "\n## Coverage\n"
              "- Occurrent classes exercised: " (get-in cov [:occurrent :used]) "/" (get-in cov [:occurrent :total]) "\n"
              "- Relation properties exercised: " (get-in cov [:properties :used]) "/" (get-in cov [:properties :total]) "\n"
              "- Role classes exercised: " (get-in cov [:roles :used]) "/" (get-in cov [:roles :total]) "\n"))))

;; ---------------------------------------------------------------------------
;; Chat-driven timeline updates (the shared mapping-update protocol, target=event/relation)
;; ---------------------------------------------------------------------------

(defn- match-event
  "Find the active event an update/remove op refers to: same label (case-insensitive)
  or same excerpt. Returns the event or nil."
  [tl {:keys [label excerpt id]}]
  (or (when id (event tl id))
      (let [nl (some-> label str/lower-case str/trim)]
        (first (filter #(or (and nl (= nl (some-> (:label %) str/lower-case str/trim)))
                            (and (seq excerpt) (= (str/trim excerpt) (str/trim (:excerpt %)))))
                       (active-events tl))))))

(defn forced-op-conflict?
  "Would applying a timeline op modify/remove a FORCED event or relation? Such ops
  are rejected client-side (parallels mapping/forced-op-conflict?)."
  [tl {:keys [op target value]}]
  (and (#{:update :remove} op)
       (case target
         :event (when-let [e (match-event tl value)] (= :forced (:status e)))
         :relation (when-let [r (relation tl (:id value))] (= :forced (:status r)))
         false)))

(defn apply-op
  "Apply one chat-proposed timeline op. `:target` is `:event` or `:relation`.
  Callers must filter forced conflicts first (see `forced-op-conflict?`)."
  [tl {:keys [op target value]}]
  (case [op target]
    [:add :event]    (add-event tl (assoc value :status :proposed))
    [:update :event] (if-let [e (match-event tl value)]
                       (update-event tl (:id e) merge
                                     (select-keys value [:label :node-id :nearest :why-no-fit :confidence :rationale])
                                     {:status :proposed})
                       (add-event tl (assoc value :status :proposed)))
    [:remove :event] (if-let [e (match-event tl value)] (remove-event tl (:id e)) tl)
    [:add :relation] (add-relation tl (assoc value :status :proposed))
    [:update :relation] (if-let [r (relation tl (:id value))]
                          (update-relation tl (:id r) merge
                                           (select-keys value [:type :property-id :confidence :rationale])
                                           {:status :proposed})
                          tl)
    [:remove :relation] (if-let [r (relation tl (:id value))] (remove-relation tl (:id r)) tl)
    tl))

;; ---------------------------------------------------------------------------
;; Merging extraction results (parallels mapping/merge-entries) + validation
;; ---------------------------------------------------------------------------

(defn- event-key [e] [(str/lower-case (str/trim (or (:label e) "")))
                      (str/trim (str (:excerpt e)))])

(defn merge-events
  "Merge freshly-extracted `incoming` events into `existing`, de-duping by
  (label, excerpt). Curated (forced/accepted/rejected) existing events win over an
  incoming duplicate — the pass never overwrites the user's decisions (§6.7.2:
  independently re-runnable without disturbing accepted mappings)."
  [existing incoming]
  (let [curated?   #{:forced :accepted :rejected}
        by-key     (into {} (map (juxt event-key identity)) existing)
        curated-existing (filter #(curated? (:status %)) existing)]
    (->> incoming
         (reduce (fn [{:keys [acc seen]} e]
                   (let [k (event-key e)]
                     (cond
                       (seen k) {:acc acc :seen seen}
                       (curated? (:status (by-key k))) {:acc acc :seen (conj seen k)}
                       :else {:acc (conj acc (new-event e)) :seen (conj seen k)})))
                 {:acc [] :seen #{}})
         :acc
         (into (vec curated-existing)))))

(defn prune-dangling
  "Drop any relation whose `:source`/`:target` is not an event id in the timeline —
  the final integrity pass after merging fresh extraction results into an existing
  timeline (fresh events get fresh ids; a relation pointing at a de-duped event is
  repaired away). Idempotent."
  [tl]
  (let [ids (into #{} (map :id) (:events tl))]
    (update tl :relations (fn [rs] (vec (filter #(and (ids (:source %)) (ids (:target %))) rs))))))

(defn merge-relations
  "Merge extracted relations, resolving each endpoint against the merged event set
  and de-duping by (source,target,type). Relations whose endpoints don't resolve are
  dropped by the *caller's* validation (see events layer); this only de-dupes and
  preserves curated relations."
  [existing incoming]
  (let [curated? #{:forced :accepted :rejected}
        rk (fn [r] [(:source r) (:target r) (:type r)])
        by-key (into {} (map (juxt rk identity)) existing)]
    (->> incoming
         (reduce (fn [{:keys [acc seen]} r]
                   (let [k (rk r)]
                     (cond
                       (seen k) {:acc acc :seen seen}
                       (curated? (:status (by-key k))) {:acc acc :seen (conj seen k)}
                       :else {:acc (conj acc (new-relation r)) :seen (conj seen k)})))
                 {:acc [] :seen #{}})
         :acc
         (into (vec (filter #(curated? (:status %)) existing))))))
