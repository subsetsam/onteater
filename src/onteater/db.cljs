(ns onteater.db
  "The shape of the re-frame application database (`app-db`) and its initial value.

  app-db is one big map. Documenting its schema here — rather than letting it
  accrete implicitly across event handlers — is the single most useful thing for
  keeping a re-frame app of this size tractable. Every key below has a comment
  describing what owns it and what it means.

  Top-level layout:

    {:workspace      :ontology|:scenario     ; which top-level tab is active
     :ontology       { ...ontology workspace state... }
     :scenario       { ...scenario workspace state... }
     :ollama         { ...local Ollama server/model settings... }
     :llm            { ...provider selection + cloud/Azure-Gov settings... }
     :ui             { ...transient shell state: dialogs, toasts, theme... }}

  The canonical ontology model itself (nodes/edges/groups/residual — see
  `onteater.model.graph`) lives at [:ontology :model]. Nothing outside the model
  namespace should reach into its internals; go through model fns and subs.")

(def default-ollama
  "Default Ollama connection + generation settings. `:base-url` and `:model`
  (plus `:options`) are persisted to IndexedDB with the LLM provider settings
  (see :llm/persist-settings) and restored on boot; the volatile keys
  (`:models`, `:status`) are session-only. `:base-url` default matches a stock
  local install."
  {:base-url    "http://localhost:11434"
   :model       nil          ; selected model name, e.g. "llama3.1:70b"
   :models      []           ; last-fetched /api/tags result (vector of maps)
   :status      :unknown     ; :unknown | :ok | :unreachable | :cors | :checking
   :status-msg  nil
   :options     {:temperature 0.2}
   ;; Scaffolded but hidden until real auth exists. The client
   ;; middleware seam is the single place auth will plug in.
   :auth        {:enabled false :scheme nil}})

(def default-llm
  "Multi-provider LLM connection state. `:active` names the provider used for
  mapping/chat runs (the selected Settings tab); Ollama config continues to
  live at the top-level :ollama key for backward compatibility. All
  provider-specific request/response knowledge lives in
  `onteater.llm.providers` — this map is only credentials + choices.

  Security: a live `:api-key` is held in memory as typed and is NEVER written
  to IndexedDB. When `:remember-key?` is true the key is instead encrypted (see
  onteater.io.crypto) under a user passphrase and the ciphertext is stored in
  `:saved`, keyed by a per-selection *slot* id:

    [:cloud <vendor>]        — one blob per Cloud vendor (:anthropic/:openai/:custom)
    [:azgov <auth-scheme>]   — one blob per Azure auth method (:api-key/:bearer)

  `:crypto` is transient session state, never persisted: the in-memory passphrase
  (nil = locked), the decrypted plaintexts, and the active passphrase-prompt."
  {:active :ollama            ; :ollama | :cloud | :azgov
   :cloud  {:vendor    :anthropic     ; :anthropic | :openai | :custom (OpenAI-compatible)
            :base-url  "https://api.anthropic.com" ; derived from :vendor preset unless :custom
            :api-key   ""
            :model     ""             ; chosen model id
            :models    []             ; fetched model-id strings (session-only)
            :remember-key? false
            :status    :unknown       ; :unknown | :checking | :ok | :unreachable | :error
            :status-msg nil}
   :azgov  {:base-url    ""           ; e.g. https://myres.openai.azure.us (or an APIM/proxy URL)
            :deployment  ""           ; Azure deployment name (stands in for the model)
            :api-version "2024-10-21"
            :auth-scheme :api-key     ; :api-key | :bearer (Entra ID token)
            :api-key     ""           ; key or pasted bearer token
            :remember-key? false
            :status      :unknown
            :status-msg  nil}
   ;; slot-id -> encrypted blob {:v :salt :iv :ct}; the only credential data
   ;; that reaches IndexedDB, and only in encrypted form.
   :saved  {}
   ;; transient, in-memory only (never persisted):
   :crypto {:passphrase nil          ; session passphrase; nil = locked
            :unlocked   {}            ; slot-id -> decrypted plaintext key
            :prompt     nil}})        ; {:mode :unlock|:enter|:set :target slot :error msg :pending {...}}

(def default-view-spec
  "The ontology canvas always renders a *view specification*, never the whole
  graph (the core anti-clutter requirement). This is its initial value:
  show modules as collapsed meta-nodes.

    :mode       — :overview | :neighborhood | :subtree | :module | :custom
    :focus      — set of node ids the view is centred on
    :hops       — neighbourhood radius (1–3) for :neighborhood mode
    :edge-types — set of edge :type keywords currently shown
    :kinds      — set of node :kind keywords currently shown
    :collapsed  — set of node/group ids whose descendants are hidden
    :layout     — :force | :tree | :radial | :cluster
    :roots      — chosen root ids for tree/radial layouts"
  {:mode       :overview
   :focus      #{}
   :hops       1
   :edge-types #{:subclass-of :domain :range :module-membership
                 :instance-of :subproperty-of}
   :kinds      #{:class :property :individual :value}
   :collapsed  #{}
   :layout     :cluster
   :roots      #{}})

(def default-db
  "The initial application database, installed by the :app/initialize event
  before any user interaction and used as the recovery baseline."
  {:workspace :ontology

   :ontology  {:model      nil        ; canonical model (onteater.model.graph) or nil
               :file       {:name nil  ; loaded file name
                            :handle nil ; FS Access API handle (Chromium) or nil
                            :format nil ; format-id the file was parsed with
                            :hash   nil}
               :dirty?     false
               :selection  {:node nil :edge nil :pinned #{}}
               :view-spec  default-view-spec
               :force-opts {:link-distance 60 :charge -240}
               ;; :expanded — set of outline group ids currently expanded; kept in
               ;; app-db (not component-local) so graph navigation can reveal nodes
               ;; in the tree. Navigation only adds to it, so 'back' never collapses.
               :outline    {:query "" :filters {} :expanded #{}}
               :undo       []          ; stack of prior models (see history interceptor)
               :redo       []
               :breadcrumbs []}        ; focus history for back-navigation

   :scenario  {:sessions   {}          ; session-id -> mapping session (model.mapping)
               :active     nil         ; active session id
               :raw-text   ""          ; current scenario editor buffer
               :rendered?  false       ; false = Raw editor (default) vs rendered markdown
               :chat-open? false
               :run        {:status :idle :received 0 :error nil}}

   :ollama    default-ollama
   :llm       default-llm

   :ui        {:theme      :auto       ; :auto | :light | :dark
               :dialogs    []          ; stack of open dialog descriptors
               :toasts     []          ; transient notifications
               :menu       nil         ; open menu-bar menu id
               :help-open? false}})
