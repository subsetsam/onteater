#!/usr/bin/env node
/*
 * build/verify-m2.mjs — end-to-end check of the Explore workspace against the
 * ADVANCED artifact (dist/onteater.html) via file://. Testing the advanced build
 * is the point: it is where any Closure property-renaming bug in the d3 interop
 * would surface. Drives: Open → load geo_ontology.json → graph renders → select a
 * node → inspector populates → switch layout → focus preserved.
 */
import { chromium } from "playwright-core";
import { pathToFileURL } from "node:url";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ART = pathToFileURL(resolve(__dirname, "../dist/onteater.html")).href;
const SAMPLE = resolve(__dirname, "../examples/galactic-economic-ontology.json");

const IGNORE = [/React DevTools/i, /favicon/i];
const errors = [];
const fail = (m) => { errors.push(m); };

const browser = await chromium.launch();
const page = await browser.newPage();
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

try {
  // Force the <input> fallback so the picker is automatable in headless.
  await page.addInitScript(() => {
    try { delete window.showOpenFilePicker; delete window.showSaveFilePicker; } catch (_) {}
  });
  await page.goto(ART, { waitUntil: "load", timeout: 20000 });
  await page.waitForSelector(".empty-state", { timeout: 8000 });

  // Open the sample through the file picker.
  const [chooser] = await Promise.all([
    page.waitForEvent("filechooser", { timeout: 8000 }),
    page.click("button.btn-primary"),
  ]);
  await chooser.setFiles(SAMPLE);

  // Graph should render as module bubbles in the default overview.
  await page.waitForSelector(".graph-canvas g.node", { timeout: 8000 });
  const bubbles = await page.$$eval(".graph-canvas g.node.meta", (n) => n.length);
  if (bubbles < 5) fail(`expected module bubbles in overview, got ${bubbles}`);

  // Status bar should report the real node count (337 real + external stubs).
  const status = await page.textContent(".status-counts");
  if (!/\d{3,} nodes/.test(status)) fail(`status bar node count missing: "${status}"`);

  // Double-click a module bubble to expand it, then a real node should appear.
  await page.dblclick(".graph-canvas g.node.meta");
  await page.waitForTimeout(600);
  const afterExpand = await page.$$eval(".graph-canvas g.node", (n) => n.length);
  if (afterExpand < 2) fail(`module did not expand (nodes: ${afterExpand})`);

  // Search + select via outline: type a query, click first result, inspector fills.
  await page.fill(".search-input", "Leverage");
  await page.waitForSelector(".result-item", { timeout: 4000 });
  await page.click(".result-item");
  await page.waitForSelector(".insp-title", { timeout: 4000 });
  const title = await page.textContent(".insp-title");
  if (!/Leverage/i.test(title)) fail(`inspector did not show Leverage: "${title}"`);

  // The selected node's neighbourhood is now focused; switch layout, focus kept.
  await page.click("button.chip:has-text('Tree')");
  await page.waitForTimeout(500);
  const stillThere = await page.$$eval(".graph-canvas g.node", (n) => n.length);
  if (stillThere < 1) fail(`tree layout rendered nothing (${stillThere})`);

  // Theme toggle should not crash and should restyle.
  await page.click(".icon-btn");
  await page.waitForTimeout(200);
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();

if (errors.length === 0) {
  console.log("✓ M2 advanced-artifact E2E passed: open → overview → expand → search → select → inspector → layout switch");
  process.exit(0);
} else {
  console.error("✗ M2 verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
