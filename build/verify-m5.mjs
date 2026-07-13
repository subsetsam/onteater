#!/usr/bin/env node
/*
 * build/verify-m5.mjs — live scenario→ontology mapping against REAL Ollama.
 * Served over http (Ollama accepts localhost origins). Loads the ontology, opens
 * the Scenario tab, selects a small fast model, pastes the sample scenario, runs
 * the mapping (structured output + streaming), and verifies entries appear, the
 * scenario shows excerpt underlines, and accept/force curation works. Also checks
 * the mapping-session file round-trips.
 */
import { chromium } from "playwright-core";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";
import { createServer } from "node:http";

const __dirname = dirname(fileURLToPath(import.meta.url));
const html = readFileSync(resolve(__dirname, "../dist/onteater.html"), "utf8");
const SAMPLE_ONTO = resolve(__dirname, "../examples/galactic-economic-ontology.json");
const SCENARIO = readFileSync(resolve(__dirname, "../examples/scenario-droid-courier.md"), "utf8");

// Prefer a small, fast local model that honors Ollama structured outputs.
const PREFERRED = ["gemma4:e4b", "nemotron-3-nano:4b-bf16", "gpt-oss:20b", "gemma4:e4b-mlx-bf16"];

const errors = [];
const fail = (m) => errors.push(m);
const IGNORE = [/React DevTools/i, /favicon/i, /Failed to load resource/i];

const server = createServer((_r, res) => { res.writeHead(200, { "Content-Type": "text/html" }); res.end(html); });
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const url = `http://127.0.0.1:${server.address().port}/`;

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1500, height: 950 } });
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

try {
  // Force the <input>/download fallback so pickers are automatable in headless.
  await page.addInitScript(() => {
    try { delete window.showOpenFilePicker; delete window.showSaveFilePicker; } catch (_) {}
  });
  await page.goto(url, { waitUntil: "load", timeout: 20000 });

  // Load the ontology.
  const [chooser] = await Promise.all([page.waitForEvent("filechooser"), page.click("button.btn-primary")]);
  await chooser.setFiles(SAMPLE_ONTO);
  await page.waitForSelector(".graph-canvas g.node");

  // Configure a model via Settings.
  await page.click(".menubar-actions .icon-btn[title='LLM settings']");
  await page.click(".settings-row .btn-primary");
  await page.waitForSelector(".conn-ok", { timeout: 12000 });
  const models = await page.$$eval(".settings-dialog select option", (o) => o.map((x) => x.value));
  const model = PREFERRED.find((m) => models.includes(m)) || models[0];
  await page.selectOption(".settings-dialog select", model);
  console.log(`  using model: ${model}`);
  await page.click(".dialog-actions .btn");

  // Scenario tab: paste scenario.
  await page.click(".workspace-tabs .tab:has-text('Scenario')");
  await page.waitForSelector(".scenario-input");
  await page.fill(".scenario-input", SCENARIO);

  // Run mapping (allow up to 3 min for a small local model).
  await page.click(".scenario-actions .btn-primary");
  await page.waitForSelector(".entry-card", { timeout: 180000 });
  const nEntries = await page.$$eval(".entry-card", (n) => n.length);
  console.log(`  mapping produced ${nEntries} entries`);
  if (nEntries < 1) fail("mapping produced no entries");

  // Summary strip populated.
  const stat = await page.textContent(".summary-heads .stat-num");
  if (!/\d/.test(stat)) fail("summary strip empty");

  // Scenario underlines: switch to Rendered and expect excerpt marks.
  await page.click(".seg-toggle .chip:has-text('Rendered')");
  await page.waitForTimeout(800);
  const marks = await page.$$eval("mark.excerpt", (m) => m.length);
  console.log(`  scenario shows ${marks} excerpt underline(s)`);
  if (marks < 1) fail("no excerpt underlines rendered (anchoring failed)");

  // Level 3: select an entry, accept it, then force it to a chosen node.
  await page.click(".entry-card");
  await page.waitForSelector(".entry-detail");
  await page.click(".detail-actions .btn:has-text('Accept')");
  await page.fill(".force-picker .insp-input", "Leverage");
  await page.waitForSelector(".force-result", { timeout: 4000 });
  await page.click(".force-result");
  // forced entry now shows the lock status somewhere on the board
  await page.waitForTimeout(300);
  const locked = await page.$$eval(".entry-status.status-forced", (n) => n.length);
  if (locked < 1) fail("force did not mark an entry as forced");

  // Session save round-trips to a file we can parse (fallback download path).
  const [dl] = await Promise.all([
    page.waitForEvent("download"),
    page.click(".session-bar .chip:has-text('Save')"),
  ]);
  const saved = JSON.parse(readFileSync(await dl.path(), "utf8"));
  if (!saved.entries || saved.entries.length < 1) fail("saved session has no entries");
  if (!saved.entries.some((e) => e.status === "forced")) fail("forced entry not persisted in session file");
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();
server.close();

if (errors.length === 0) {
  console.log("✓ M5 passed: live mapping run → entries + summary + underlines → accept/force → session file round-trip");
  process.exit(0);
} else {
  console.error("✗ M5 verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
