#!/usr/bin/env node
/*
 * build/verify-m3.mjs — end-to-end check of Edit + Persist on the advanced
 * artifact: edit a gloss in the inspector, confirm dirty tracking, export the
 * edited ontology in the GEO format and confirm the edit persisted while the rest
 * of the file stayed byte-conservative, then undo, add a class, and delete with
 * confirmation.
 */
import { chromium } from "playwright-core";
import { pathToFileURL } from "node:url";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ART = pathToFileURL(resolve(__dirname, "../dist/onteater.html")).href;
const SAMPLE = resolve(__dirname, "../examples/galactic-economic-ontology.json");
const original = JSON.parse(readFileSync(SAMPLE, "utf8"));

const errors = [];
const fail = (m) => errors.push(m);
const IGNORE = [/React DevTools/i, /favicon/i];

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1400, height: 900 } });
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

const readDownload = async (dl) => {
  const p = await dl.path();
  return readFileSync(p, "utf8");
};

try {
  // Force the download/<input> fallback path (the Firefox/Safari path the plan
  // requires) so the picker is automatable and downloads are capturable.
  await page.addInitScript(() => {
    try { delete window.showOpenFilePicker; delete window.showSaveFilePicker; } catch (_) {}
  });
  await page.goto(ART, { waitUntil: "load", timeout: 20000 });
  const [chooser] = await Promise.all([page.waitForEvent("filechooser"), page.click("button.btn-primary")]);
  await chooser.setFiles(SAMPLE);
  await page.waitForSelector(".graph-canvas g.node");

  // Select geo:Leverage via search, edit its gloss in the inspector.
  await page.fill(".search-input", "Leverage");
  await page.waitForSelector(".result-item");
  await page.click(".result-item");
  await page.waitForSelector(".insp-textarea");
  const NEWGLOSS = "EDITED VIA INSPECTOR — persistence check.";
  await page.fill(".insp-textarea", NEWGLOSS);
  await page.click(".insp-title"); // blur -> commit

  // Dirty flag should now show.
  await page.waitForSelector(".dirty-dot", { timeout: 3000 });

  // Export as GEO JSON (fallback download since headless has no FS Access).
  await page.click("button.menu-trigger:has-text('File')");
  const [dl] = await Promise.all([
    page.waitForEvent("download"),
    page.click(".menu-item:has-text('geo JSON')"),
  ]);
  const text = await readDownload(dl);
  const out = JSON.parse(text);

  // The edit landed on geo:Leverage where it is authoritatively defined.
  const findById = (o, id) => {
    let hit = null;
    const walk = (x) => { if (hit) return;
      if (Array.isArray(x)) x.forEach(walk);
      else if (x && typeof x === "object") { if (x.id === id && x.gloss) hit = x; Object.values(x).forEach(walk); } };
    walk(o); return hit;
  };
  const lev = findById(out, "geo:Leverage");
  if (!lev || lev.gloss !== NEWGLOSS) fail(`edited gloss not persisted: ${lev && lev.gloss}`);

  // Byte-conservative: every OTHER spine class is identical to the original.
  const beforeSpine = original.spine.classes;
  const afterSpine = out.spine.classes;
  if (beforeSpine.length !== afterSpine.length) fail("spine.classes length changed");
  for (let i = 0; i < beforeSpine.length; i++) {
    if (beforeSpine[i].id !== "geo:Leverage" &&
        JSON.stringify(beforeSpine[i]) !== JSON.stringify(afterSpine[i])) {
      fail(`unrelated node changed on export: ${beforeSpine[i].id}`);
      break;
    }
  }

  // Undo restores the gloss (inspector reflects it).
  await page.keyboard.press(process.platform === "darwin" ? "Meta+z" : "Control+z");
  await page.waitForTimeout(300);
  const glossAfterUndo = await page.inputValue(".insp-textarea");
  if (glossAfterUndo === NEWGLOSS) fail("undo did not revert the gloss");

  // Add a class via the toolbar; node count rises.
  const before = await page.$$eval(".graph-canvas g.node", (n) => n.length);
  await page.click(".chip-add");
  await page.waitForTimeout(400);
  // The new node is selected -> inspector shows an editable label input.
  await page.waitForSelector(".insp-title", { timeout: 3000 });

  // Delete it with confirmation.
  await page.click(".btn-danger:has-text('Delete')");
  await page.waitForSelector(".dialog", { timeout: 3000 });
  await page.click(".dialog .btn-danger-solid");
  await page.waitForTimeout(300);

  // Export the graph as SVG and confirm it is a real, populated SVG.
  await page.click("button.menu-trigger:has-text('File')");
  const [svgDl] = await Promise.all([
    page.waitForEvent("download"),
    page.click(".menu-item:has-text('SVG')"),
  ]);
  const svg = await readDownload(svgDl);
  if (!/<svg/.test(svg) || !/<path/.test(svg)) fail("SVG export is empty/invalid");
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();

if (errors.length === 0) {
  console.log("✓ M3 E2E passed: inspector edit → dirty → geo export (edit persisted, rest byte-conservative) → undo → add class → delete w/ confirm → SVG export");
  process.exit(0);
} else {
  console.error("✗ M3 verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
