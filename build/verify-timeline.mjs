#!/usr/bin/env node
/*
 * build/verify-timeline.mjs — Milestone 6 (Timeline) acceptance flow, fully
 * offline. Served over http://127.0.0.1; the LLM is mocked with Playwright route
 * interception (the Cloud/Anthropic path, exactly like verify-m7-providers.mjs) so
 * nothing leaves the machine. Runs against the advanced-compiled dist/onteater.html.
 *
 * Scripts the PLAN §8 milestone-6 acceptance criteria over a multi-actor scenario
 * containing a fork and a join:
 *   - the timeline extraction pass runs and lanes + fork/join edges render
 *   - an event the ontology can't type shows a "?" glyph on the timeline
 *   - dependency-cone mode dims events outside the selection's cone
 *   - that untyped event appears in the Gap report
 *   - "Draft ontology element from this gap…" pre-fills the add-node dialog in the
 *     ontology workspace (use case A) with label + suggested parent from `nearest`
 *   - undo reverts an applied timeline change (Accept)
 *
 * E2E gotchas per CLAUDE.md: delete the FS pickers in an init script, serve over
 * http://127.0.0.1, page.click + waitForFunction for re-frame async renders.
 */
import { chromium } from "playwright-core";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";
import { createServer } from "node:http";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ARTPATH = resolve(__dirname, "../dist/onteater.html");
const html = readFileSync(ARTPATH, "utf8");
const SAMPLE_ONTO = resolve(__dirname, "../examples/galactic-economic-ontology.json");

const errors = [];
const fail = (m) => errors.push(m);
const ok = (m) => console.log("  " + m);
const IGNORE = [/React DevTools/i, /favicon/i, /Failed to load resource/i, /net::ERR/i,
                /Access to fetch/i, /CORS/i, /ERR_FAILED/i, /127\.0\.0\.1:1/];

const server = createServer((_req, res) => {
  res.writeHead(200, { "Content-Type": "text/html" });
  res.end(html);
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const httpUrl = `http://127.0.0.1:${server.address().port}/`;

const DUMMY_KEY = "sk-dummy-not-a-real-key";

// A multi-actor scenario: a fork (US action → two responses) and a join
// (two responses → market destabilisation). Each excerpt appears verbatim below.
const SCEN_TEXT =
  "The US imposes controls on hyperdrive components in April 2025. In response, the " +
  "Sector restricts exports of raw ore. Meanwhile, the Allies coordinate a joint " +
  "stance against the embargo. As a result, the Markets destabilize across the region.";

// The mocked timeline extraction: e1 forks to e2 and e3; e2 and e3 join at e4.
// e3 is deliberately UNTYPED (node_id null) — an ontology gap with nearest + why.
const TIMELINE_JSON = JSON.stringify({
  events: [
    { id: "e1", label: "US imposes controls", excerpt: "imposes controls", occurrence: 1,
      node_id: "geo:Imposition", participants: [{ entity: "United States", role_id: "geo:SenderRole" }],
      when: { kind: "instant", start: "2025-04-04", precision: "day", narrative_index: 0 },
      confidence: 0.9, rationale: "An imposition of controls." },
    { id: "e2", label: "Sector restricts", excerpt: "restricts exports", occurrence: 1,
      node_id: "geo:Imposition", participants: [{ entity: "Sector", role_id: null }],
      when: { kind: "unknown", start: null, narrative_index: 1 }, confidence: 0.7, rationale: "A retaliatory restriction." },
    { id: "e3", label: "Allies coordinate", excerpt: "coordinate", occurrence: 1,
      node_id: null, nearest: "geo:Act", why_no_fit: "no coalition-coordination class exists",
      participants: [{ entity: "Allies", role_id: null }],
      when: { kind: "unknown", start: null, narrative_index: 2 }, confidence: 0.5, rationale: "Coalition forming." },
    { id: "e4", label: "Markets destabilize", excerpt: "Markets destabilize", occurrence: 1,
      node_id: "geo:EconomicProcess", participants: [{ entity: "Markets", role_id: null }],
      when: { kind: "unknown", start: null, narrative_index: 3 }, confidence: 0.6, rationale: "Downstream effect." },
  ],
  relations: [
    { source: "e1", target: "e2", type: "causes", property_id: "geo:respondsTo", confidence: 0.7, rationale: "" },
    { source: "e1", target: "e3", type: "causes", property_id: null, confidence: 0.6, rationale: "" }, // untyped rel (gap)
    { source: "e2", target: "e4", type: "enables", property_id: null, confidence: 0.6, rationale: "" },
    { source: "e3", target: "e4", type: "enables", property_id: "geo:respondsTo", confidence: 0.6, rationale: "" },
  ],
});

const cors = { "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Headers": "*", "Access-Control-Allow-Methods": "*" };

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1280, height: 860 } });
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

try {
  await page.addInitScript(() => {
    try { delete window.showOpenFilePicker; delete window.showSaveFilePicker; } catch (_) {}
  });

  // Mock the Anthropic path: models list, CORS preflight, and /v1/messages —
  // returning the timeline JSON for the temporal pass (system prompt says TEMPORAL),
  // an empty entry set for any other request.
  await page.route("https://api.anthropic.com/**", async (route) => {
    const req = route.request();
    if (req.method() === "OPTIONS") return route.fulfill({ status: 204, headers: cors });
    if (req.url().endsWith("/v1/models")) {
      return route.fulfill({ status: 200, headers: cors, contentType: "application/json",
        body: JSON.stringify({ data: [{ id: "claude-opus-4-8" }] }) });
    }
    if (req.url().endsWith("/v1/messages")) {
      const sys = req.postDataJSON()?.system || "";
      const text = /TEMPORAL/.test(sys) ? TIMELINE_JSON : JSON.stringify({ entries: [], unmapped: [] });
      return route.fulfill({ status: 200, headers: cors, contentType: "application/json",
        body: JSON.stringify({ content: [{ type: "text", text }], stop_reason: "end_turn",
          usage: { input_tokens: 1, output_tokens: 1 } }) });
    }
    return route.fulfill({ status: 404, headers: cors, body: "unmocked" });
  });

  await page.goto(httpUrl, { waitUntil: "load", timeout: 20000 });

  // --- load the ontology (picker fallback) -----------------------------------
  const [chooser] = await Promise.all([page.waitForEvent("filechooser"), page.click("button.btn-primary")]);
  await chooser.setFiles(SAMPLE_ONTO);
  await page.waitForSelector(".graph-canvas g.node");
  ok("ontology loaded");

  // --- configure the mocked Anthropic provider -------------------------------
  await page.click(".menubar-actions .icon-btn[title='LLM settings']");
  await page.waitForSelector(".settings-dialog");
  await page.click(".settings-tab:has-text('Cloud')");
  await page.fill(".settings-dialog input[type=password]", DUMMY_KEY);
  await page.waitForFunction((k) => document.querySelector(".settings-dialog input[type=password]")?.value === k, DUMMY_KEY);
  await page.click(".dialog-title");
  await page.click(".llm-test");
  await page.waitForSelector(".conn-ok", { timeout: 8000 });
  await page.waitForSelector("select.cloud-model", { timeout: 3000 });
  await page.click(".dialog-actions .btn"); // Done
  ok("provider configured (mocked Anthropic)");

  // --- scenario + timeline pass ----------------------------------------------
  await page.click(".workspace-tabs .tab:has-text('Scenario')");
  await page.waitForSelector(".scenario-input");
  await page.fill(".scenario-input", SCEN_TEXT);
  await page.click(".center-tab:has-text('Timeline')");
  await page.click("button:has-text('Run timeline pass')");
  await page.waitForSelector(".tl-event", { timeout: 15000 });

  // lanes + events + fork/join edges
  const nLanes = await page.$$eval(".tl-lane", (n) => n.length);
  const nEvents = await page.$$eval(".tl-event", (n) => n.length);
  const nEdges = await page.$$eval(".tl-edge", (n) => n.length);
  if (nEvents !== 4) fail(`expected 4 events, got ${nEvents}`);
  if (nLanes < 4) fail(`expected >=4 entity lanes, got ${nLanes}`);
  if (nEdges < 4) fail(`expected >=4 fork/join edges, got ${nEdges}`);
  ok(`timeline rendered: ${nEvents} events, ${nLanes} lanes, ${nEdges} edges`);

  // the run controls keep an elapsed-time line after the pass completes
  await page.waitForFunction(() => /elapsed time:/.test(document.querySelector(".tl-run")?.textContent || ""),
    null, { timeout: 4000 }).catch(() => fail("no elapsed-time line shown after the timeline pass"));
  ok("elapsed-time shown after the timeline pass completes");

  // the untyped event shows a "?" glyph
  const qGlyphs = await page.$$eval(".tl-glyph-q", (ns) => ns.map((n) => n.textContent).filter((t) => t === "?"));
  if (qGlyphs.length !== 1) fail(`expected exactly one "?" (untyped) glyph, got ${qGlyphs.length}`);
  ok(`untyped event shows "?" glyph on the timeline`);

  // untyped relation drawn dashed in the warning colour (a gap visible in place)
  const nUntypedEdge = await page.$$eval(".tl-edge-untyped", (n) => n.length);
  if (nUntypedEdge < 1) fail("expected at least one untyped (dashed warning) relation edge");

  // --- dependency-cone mode dims events outside the cone ---------------------
  // Select e2 (Sector): cone = {e1, e2, e4}; e3 (Allies) is outside → dimmed.
  await page.click(".tl-event:has-text('Sector')");
  await page.click(".chip:has-text('Cone')");
  await page.waitForFunction(() => document.querySelectorAll(".tl-event.tl-dimmed").length >= 1, null, { timeout: 4000 })
    .catch(() => fail("cone mode did not dim any out-of-cone event"));
  const dimmedHasAllies = await page.$$eval(".tl-event.tl-dimmed",
    (ns) => ns.some((n) => /Allies/.test(n.textContent)));
  if (!dimmedHasAllies) fail("the out-of-cone event (Allies) was not the dimmed one");
  ok("dependency-cone mode dims non-ancestors/descendants");
  await page.click(".chip:has-text('Cone')"); // cone off

  // --- undo reverts an applied timeline change (Accept) ----------------------
  await page.click(".tl-event:has-text('Markets')");
  await page.waitForSelector(".tl-detail .detail-actions");
  await page.click(".tl-detail .detail-actions .btn:has-text('Accept')");
  await page.waitForFunction(
    () => { const b = [...document.querySelectorAll(".tl-detail .detail-actions .btn")].find((x) => /Accept/.test(x.textContent));
            return b && b.classList.contains("btn-primary"); }, null, { timeout: 4000 })
    .catch(() => fail("Accept did not mark the event accepted"));
  await page.keyboard.press(process.platform === "darwin" ? "Meta+z" : "Control+z");
  await page.waitForFunction(
    () => { const b = [...document.querySelectorAll(".tl-detail .detail-actions .btn")].find((x) => /Accept/.test(x.textContent));
            return b && !b.classList.contains("btn-primary"); }, null, { timeout: 4000 })
    .catch(() => fail("undo did not revert the applied timeline change"));
  ok("undo reverts an applied timeline change");

  // --- Gap report: the untyped event appears; draft-from-gap pre-fills A ------
  await page.click(".center-tab:has-text('Gaps')");
  await page.waitForSelector(".gap-card");
  const gapHasAllies = await page.$$eval(".gap-card .gap-label", (ns) => ns.some((n) => /Allies/.test(n.textContent)));
  if (!gapHasAllies) fail("the untyped event did not appear in the gap report");
  ok("untyped event appears in the Gap report");

  // Draft ontology element from that gap → add-node dialog, prefilled, in use case A
  await page.click(".gap-card button:has-text('Draft ontology element')");
  await page.waitForSelector(".add-node-dialog", { timeout: 4000 });
  const draftLabel = await page.inputValue(".add-node-dialog .dialog-field input");
  if (!/Allies/.test(draftLabel)) fail(`add-node dialog label not pre-filled from the gap: "${draftLabel}"`);
  const parentVal = await page.$eval(".add-node-dialog .dialog-field:nth-of-type(2) input", (i) => i.value);
  if (parentVal !== "geo:Act") fail(`suggested parent not pre-filled from 'nearest': "${parentVal}"`);
  const wsAfter = await page.$eval(".workspace-tabs .tab-active", (t) => t.textContent).catch(() => "");
  if (!/Ontology/.test(wsAfter)) fail(`draft-from-gap did not switch to the Ontology workspace: "${wsAfter}"`);
  ok("draft-from-gap pre-fills the add-node dialog in the ontology workspace");

  // Commit the drafted class and confirm it lands in the ontology model.
  await page.click(".add-node-dialog .btn-primary:has-text('Create class')");
  await page.waitForFunction(() => !document.querySelector(".add-node-dialog"), null, { timeout: 4000 });
  await page.waitForSelector(".graph-canvas g.node.selected", { timeout: 5000 })
    .catch(() => fail("drafted class was not created + focused in the ontology canvas"));
  ok("drafted class created in the ontology workspace");

} catch (e) {
  fail("exception: " + (e && e.stack ? e.stack : e));
}

await browser.close();
server.close();

if (errors.length === 0) {
  console.log("✓ Timeline (M6) passed: extraction, lanes + fork/join, ? glyph, cone dimming, gap report, draft-from-gap, and undo all behave");
  process.exit(0);
} else {
  console.error("✗ Timeline verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
