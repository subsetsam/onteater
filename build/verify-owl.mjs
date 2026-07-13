#!/usr/bin/env node
/*
 * build/verify-owl.mjs — end-to-end check of OWL 2 / Turtle support against the
 * ADVANCED artifact (dist/onteater.html). Drives the real UI:
 *   1. Open examples/solar-system.ttl → auto-detected as OWL, graph renders.
 *   2. File → Export ontology (OWL2 Turtle) → capture the download, assert it is
 *      valid Turtle carrying the modelled entities AND the preserved restriction.
 * Runs against the advanced build so any Closure property-renaming bug surfaces.
 */
import { chromium } from "playwright-core";
import { pathToFileURL } from "node:url";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ART = pathToFileURL(resolve(__dirname, "../dist/onteater.html")).href;
const SAMPLE = resolve(__dirname, "../examples/solar-system.ttl");

const IGNORE = [/React DevTools/i, /favicon/i];
const errors = [];
const fail = (m) => { errors.push(m); };

const browser = await chromium.launch();
const page = await browser.newPage();
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

try {
  // Force the <input>/download fallback so both picker and save are automatable.
  await page.addInitScript(() => {
    try { delete window.showOpenFilePicker; delete window.showSaveFilePicker; } catch (_) {}
  });
  await page.goto(ART, { waitUntil: "load", timeout: 20000 });
  await page.waitForSelector(".empty-state", { timeout: 8000 });

  // Open the Turtle sample through the file picker.
  const [chooser] = await Promise.all([
    page.waitForEvent("filechooser", { timeout: 8000 }),
    page.click("button.btn-primary"),
  ]);
  await chooser.setFiles(SAMPLE);

  // Graph should render — the .ttl was recognised without any extension hint.
  await page.waitForSelector(".graph-canvas g.node", { timeout: 8000 });
  const status = await page.textContent(".status-counts");
  if (!/\d+ nodes/.test(status)) fail(`status bar node count missing: "${status}"`);

  // File → Export ontology (OWL2 Turtle), capturing the triggered download.
  await page.click("button.menu-trigger:has-text('File')");
  const [download] = await Promise.all([
    page.waitForEvent("download", { timeout: 8000 }),
    page.click(".menu-item:has-text('Export ontology (OWL2 Turtle)')"),
  ]);
  if (!/\.ttl$/.test(download.suggestedFilename())) {
    fail(`export filename not .ttl: "${download.suggestedFilename()}"`);
  }
  const path = await download.path();
  const ttl = readFileSync(path, "utf8");

  const must = ["@prefix owl:", "owl:Class", "owl:ObjectProperty", "owl:DatatypeProperty",
                "rdfs:subClassOf", "owl:Restriction", "owl:someValuesFrom"];
  for (const m of must) if (!ttl.includes(m)) fail(`exported Turtle missing "${m}"`);
  // A curie must never be emitted angle-wrapped (the classic id->turtle bug).
  if (/<[a-z]+:[A-Za-z]/.test(ttl)) fail("exported Turtle has an angle-wrapped curie");
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();

if (errors.length === 0) {
  console.log("✓ OWL E2E passed: open .ttl (auto-detect) → render → Export OWL2 → valid Turtle w/ preserved axioms");
  process.exit(0);
} else {
  console.error("✗ OWL verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
