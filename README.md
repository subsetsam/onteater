# [Onteater](https://github.com/subsetsam/onteater/)

<p align="left">
  <img src="images/onteater_mascot.svg" alt="Onteater mascot — a southern tamandua anteater playing with things that it does not understand" width="200">
</p>

An **offline-first, single-HTML-file** studio for ontology **visualization,
exploration, and editing**, with **LLM-assisted mapping** of plain-text scenarios
onto an ontology via a local [Ollama](https://ollama.com) server, a token-based
cloud provider (Anthropic, OpenAI, or any OpenAI-compatible endpoint), or an
Azure Government-hosted Azure OpenAI deployment.

The shipped artifact — `dist/onteater.html` — is one self-contained file (compiled
app, d3, KaTeX fonts, CSS, all inlined). It opens straight from `file://` and needs
no network access except the LLM calls you explicitly configure (⚙ LLM settings:
Ollama | Cloud | Azure Gov tabs — the selected tab is what mapping and chat use).
API keys stay in memory unless you tick "Remember key on this device".

Built with **HTML + ClojureScript only** (Reagent + re-frame, d3 v7, markdown-it,
KaTeX), compiled with shadow-cljs.

## Two workflows

- **Ontology** — open a JSON ontology, navigate from a module overview into
  focused neighbourhoods (four layouts: clustered, force, tidy tree, radial),
  search and filter, edit any attribute, add/delete nodes and relations, undo/redo,
  and save. Edits to a source file land **in place**; the rest of the file is left
  byte-for-byte untouched. The center pane toggles between the **Graph** view and a
  **Docs** view — the ontology's prose documentation (worked examples, design
  decisions, revision notes, governance, metadata) shown as an accordion and edited
  in place by one generic structured editor, every change undoable and round-tripped
  back into the file on save.
- **Scenario** — paste or upload a scenario (Markdown + LaTeX math), pick a model
  (local Ollama, a cloud provider, or an Azure Gov deployment), and map its
  elements onto the ontology. Every mapping is validated against
  the ontology, shown across three linked levels (summary → board → detail), and
  fully curatable (accept / reject / force). A chat drawer answers questions about
  the mapping and can propose changes as reviewable diffs. The center pane is
  tabbed into three linked views over one session:
  - **Mapping** — the entry board: scenario excerpts typed onto ontology classes,
    each accept/reject/force-able and highlighted back in the scenario text.
    Optionally generate an **ontology briefing** first — a one-time LLM pass over
    the loaded ontology that produces module summaries and disambiguation rules,
    which you can edit and which are then injected into every mapping prompt to
    tune extraction to *this* ontology.
  - **Timeline** — a second extraction pass lifts the mapped elements into a
    temporal/causal graph: dated **events** ordered by a date → dependency →
    narrative cascade, linked by *precedes / causes / enables / responds-to /
    part-of / terminates* **relations**, drawn as a swimlane DAG (lane by entity,
    module, or episode). Forks, joins, parallel threads, and feedback cycles are
    first-class; select an event to read its upstream/downstream dependency cone
    and the paths between any two events. Drag glyph-to-glyph to add a relation.
  - **Gaps** — a pure completeness audit (no LLM) of where the ontology *fails*
    the scenario: untyped events and relations, typings too shallow for the
    available leaf classes, unroled participants, and two-directional class
    coverage (unused classes vs. overloaded ones). Each gap offers *"Draft
    ontology element from this gap…"* (jumps to the Ontology editor with a
    pre-filled add-node dialog); a re-run reports which gaps cleared, and the
    whole report exports as Markdown.

The data model is **format-agnostic**. Onteater ships three adapters:

- the bundled **geo reference JSON** (`examples/galactic-economic-ontology.json`),
  read/written with byte-for-byte round-trip fidelity;
- the **Onteater native JSON** lossless format; and
- **OWL 2 in Turtle** (`.ttl`) — open, save, and **File ▸ Export (OWL2)** from any
  loaded ontology. Classes, object/datatype/annotation properties, individuals,
  `subClassOf`/`subPropertyOf`/`domain`/`range` and `rdf:type` become first-class
  nodes and edges; anything Onteater does not model (restrictions, lists, custom
  annotations) is preserved verbatim and re-emitted on save.

Adding another format (JSON-LD, SKOS, plain node-link JSON) is one new namespace
implementing the `OntologyFormat` protocol.

## Examples

The `examples/` directory holds a self-contained demo set — two ontologies and
five scenarios to map against them.

- **[`galactic-economic-ontology.json`](examples/galactic-economic-ontology.json)** —
  the bundled reference ontology (the *Reference Ontology for Galactic Political
  Economy*). Open it with **File ▸ Open** to explore, search, and edit. This is the
  same ontology the golden round-trip test is built on.
- **[`solar-system.ttl`](examples/solar-system.ttl)** — a small **OWL 2 / Turtle**
  ontology (classes, properties, individuals, and an anonymous `owl:Restriction`)
  for trying **File ▸ Open** on a `.ttl` file and **File ▸ Export (OWL2)**.
- Five scenarios (Markdown + LaTeX) to load in the **Scenario** workflow, then map
  onto the ontology with a local model. The first three are single-episode *worked
  examples*; the last two are longer-form:
  - **[`scenario-naboo-blockade.md`](examples/scenario-naboo-blockade.md)** — Worked
    example A: the Naboo blockade (32 BBY), a coercion episode with a covert patron.
  - **[`scenario-bespin-connector.md`](examples/scenario-bespin-connector.md)** —
    Worked example B: Bespin (3 ABY), connector capture and the unilateral alteration
    of a deal.
  - **[`scenario-alderaan-demonstration.md`](examples/scenario-alderaan-demonstration.md)**
    — Worked example C: Alderaan (0 BBY), the demonstration that refuted its own
    doctrine.
  - **[`scenario-clone-wars.md`](examples/scenario-clone-wars.md)** — Worked example D:
    a campaign-level scenario spanning Episodes II–III: the Clone Wars (22–19 BBY) as 
    a manufactured fragmentation and the war both sides lost.
  - **[`scenario-droid-courier.md`](examples/scenario-droid-courier.md)** — Worked 
    example E: a supplementary scenario (0 BBY, Episode IV): information carriage, 
    the informal droid market, and the observation layer.

## Requirements

- [Node.js](https://nodejs.org) 18+ and a JDK 11+ (for the shadow-cljs build).
- A browser. Full experience in Chromium/Brave/Edge, Firefox, and Safari; the File
  System Access API (true in-place Save) is used where available, with a
  download-based fallback elsewhere.
- For the Scenario workflow, one LLM connection: a running Ollama server, a
  cloud API key (Anthropic / OpenAI / any OpenAI-compatible endpoint), or an
  Azure Government Azure-OpenAI deployment. None is needed to explore or edit an
  ontology.

## Build

```bash
npm install
npm run build         # shadow-cljs release + inline -> dist/onteater.html
```

Then double-click `dist/onteater.html` (or open it from your browser).

### Development

```bash
npm run dev           # shadow-cljs watch with hot reload, http://localhost:8080
npm test              # headless cljs.test (model, formats, prompts, events)
```

## Connecting an LLM provider

The Scenario workflow (mapping and the chat drawer) runs against one of three
provider families. Open **Settings (⚙)** to find a tab for each — **Ollama**,
**Cloud**, and **Azure Gov**. **The selected tab _is_ the active provider**: whatever
tab is showing is what mapping and chat use. A single temperature slider sits below
the tabs and is shared by every provider (Anthropic models ignore it — they reject
sampling parameters).

API keys live in memory for the session only. To keep one across reloads, tick
**"Remember key on this device"** on that provider's tab — the key is then
**encrypted with a passphrase you set** (AES-GCM under a PBKDF2-derived key, via
the browser's Web Crypto API) and only the ciphertext is written to this
browser's IndexedDB; after a reload, enter the passphrase to unlock saved keys.
This protects credentials *at rest* — an app that calls LLMs from page JS must
recover the plaintext at request time, so session-only remains the strongest
option. Non-secret settings (chosen provider, base URLs, models) always persist.

### Ollama (local)

A page served from `file://` has origin `null`, which Ollama rejects by default.
Start the server so it accepts this origin:

```bash
OLLAMA_ORIGINS="*" ollama serve      # or list your specific origin(s)
```

Then on the **Ollama** tab set the base URL (default `http://localhost:11434`),
press **Test connection**, and pick a model. Structured-output support varies by
model; capable instruct models produce the best mappings.

### Cloud (Anthropic / OpenAI / OpenAI-compatible)

On the **Cloud** tab pick a **Provider**:

- **Anthropic** and **OpenAI** come with their base URL preset; just paste an API
  key and press **Test & load models** to populate the model picker (defaults to
  `claude-opus-4-8` / `gpt-4o`).
- **Custom (OpenAI-compatible)** exposes an editable **Base URL** for any gateway
  or proxy that speaks the OpenAI Chat Completions API, plus a free-text model id.

Mapping requests use each vendor's strict structured-output mode (Anthropic
`output_config`, OpenAI `response_format: json_schema`) so results validate against
the ontology schema. Browser-direct Anthropic calls are made with the
`anthropic-dangerous-direct-browser-access` opt-in; most OpenAI-compatible
endpoints must allow the `null` (file://) origin via CORS.

### Azure Gov (Azure OpenAI)

The **Azure Gov** tab targets an Azure Government-hosted Azure OpenAI deployment.
Fill in the **Endpoint** (`https://<resource>.openai.azure.us`), **Deployment
name**, **API version** (default `2024-10-21`), and authenticate with either an
**API key** or an **Entra ID bearer token** (paste one from
`az account get-access-token --resource https://cognitiveservices.azure.us`).
**Test connection** sends a one-word chat to the deployment (there is no
list-deployments call).

## Repository layout

```
src/onteater/
  core.cljs              entry point, mount, global services
  db.cljs                app-db schema + initial state
  events/, events.cljs   re-frame events (ontology, editing, history, mapping,
                         timeline, docs, chat, ollama, multi-provider LLM settings,
                         persist) + registry
  subs/, subs.cljs       re-frame subscriptions (ontology, scenario, timeline) + registry
  model/graph.cljs       canonical ontology model + pure operations
  model/view.cljs        the anti-clutter "what's visible" view model
  model/mapping.cljs     mapping session model + operations
  model/timeline.cljs    temporal/causal timeline model — ordering cascade,
                         dependency cones, lane layout, gap report (pure, tested)
  model/docs.cljs        ontology documentation tree — generic structured editor ops
  format/{core,geo,native,owl}.cljs  format protocol + adapters (geo JSON, native JSON, OWL2 Turtle)
  llm/providers.cljs     pure provider adapters (request/response shapes for
                         Ollama, cloud vendors, Azure Gov)
  llm/{client,prompts}.cljs       provider-agnostic HTTP client + prompt engineering
  viz/{common,graph,tree,timeline}.cljs  d3 rendering + layouts (incl. swimlane DAG)
  io/{file,export,idb}.cljs       file I/O, SVG/PNG export, autosave
  ui/*.cljs              views: shell, workspaces, panes, dialogs, chat, mascot
build/inline.mjs         single-file packager
build/verify-*.mjs       headless-Chromium end-to-end checks
examples/                reference ontology + scenario demos (see Examples)
test/onteater/...        unit tests (incl. the golden round-trip)
```

## Testing

- `npm test` runs the headless unit suite, including the **golden round-trip** on the
  reference ontology (parse → serialize is byte-identical) — the hard gate the editing
  features are built on — and the pure timeline core (ordering cascade, dependency
  cones, lane layout, gap report).
- `build/verify-*.mjs` are end-to-end checks against the built artifact in headless
  Chromium (file:// boot, explore, edit+persist, multi-provider settings, live Ollama
  mapping and chat, and the timeline extraction/gap flow).
- `build/eval-mapping.mjs` is an optional mapping-**quality** harness (metrics, not
  pass/fail): it runs the real prompt pipeline against a live Ollama over the example
  scenarios and reports invalid-target / excerpt-miss / shallow-typing rates per
  strategy. `--report-only` needs no model and just prints prompt-size estimates.

## License

Code is available under the [MIT License](LICENSE). 

The example [ontology](examples/galactic-economic-ontology.json) and scenarios [A](/examples/scenario-alderaan-demonstration.md), [B](/examples/scenario-bespin-connector.md), [C](/examples/scenario-naboo-blockade.md), [D](/examples/scenario-clone-wars.md), and [E](/examples/scenario-droid-courier.md) in the `examples/` directory are for illustrative purposes, and reference characters and events from the first two Star Wars trilogies. Star Wars is the property of [The Walt Disney Company](https://thewaltdisneycompany.com/). This project is not affiliated with or endorsed by Lucasfilm or Disney.