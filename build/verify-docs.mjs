#!/usr/bin/env node
/*
 * build/verify-docs.mjs — end-to-end check of the Docs view on the advanced
 * artifact: toggle Graph|Docs, edit a worked-example name in the generic
 * structured editor, confirm dirty tracking, export geo JSON and confirm the
 * edit persisted while the rest of the file (including top-level key order)
 * stayed byte-conservative; then remove a whole section with confirmation,
 * undo it back, and add a brand-new section.
 */
import { chromium } from "playwright-core";
import { pathToFileURL } from "node:url";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ART = pathToFileURL(resolve(__dirname, "../dist/onteater.html")).href;
// Optionally pass another geo file as argv[2] (it needs worked_examples + governance).
const SAMPLE = resolve(process.argv[2] ?? resolve(__dirname, "../examples/galactic-economic-ontology.json"));
const original = JSON.parse(readFileSync(SAMPLE, "utf8"));

const errors = [];
const fail = (m) => errors.push(m);
const IGNORE = [/React DevTools/i, /favicon/i];

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1400, height: 900 } });
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

const readDownload = async (dl) => readFileSync(await dl.path(), "utf8");

const exportGeo = async () => {
  await page.click("button.menu-trigger:has-text('File')");
  const [dl] = await Promise.all([
    page.waitForEvent("download"),
    page.click(".menu-item:has-text('geo JSON')"),
  ]);
  return JSON.parse(await readDownload(dl));
};

try {
  await page.addInitScript(() => {
    try { delete window.showOpenFilePicker; delete window.showSaveFilePicker; } catch (_) {}
  });
  await page.goto(ART, { waitUntil: "load", timeout: 20000 });
  const [chooser] = await Promise.all([page.waitForEvent("filechooser"), page.click("button.btn-primary")]);
  await chooser.setFiles(SAMPLE);
  await page.waitForSelector(".graph-canvas g.node");

  // Switch the center pane to Docs; the sections accordion appears.
  await page.click(".center-view-bar .chip:has-text('Docs')");
  await page.waitForSelector(".docs-section");
  const sectionCount = await page.$$eval(".docs-section", (n) => n.length);
  if (sectionCount < 5) fail(`expected >=5 docs sections, saw ${sectionCount}`);

  // Expand Worked examples and edit the first example's name. Re-frame renders
  // async, so wait for the target field to materialise before touching it.
  await page.click(".docs-section-head:has-text('Worked examples')");
  const oldName = original.worked_examples.examples[0].name;
  const NEWNAME = "EDITED VIA DOCS VIEW — persistence check.";
  await page.waitForFunction(
    (oldV) => [...document.querySelectorAll(".docs-section-body input")].some((i) => i.value === oldV),
    oldName, { timeout: 3000 });
  const edited = await page.evaluate(([oldV, newV]) => {
    const inp = [...document.querySelectorAll(".docs-section-body input")]
      .find((i) => i.value === oldV);
    if (!inp) return false;
    inp.focus();
    // React uncontrolled input: set via the native setter so blur sees the change.
    const set = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, "value").set;
    set.call(inp, newV);
    inp.dispatchEvent(new Event("input", { bubbles: true }));
    inp.blur();
    return true;
  }, [oldName, NEWNAME]);
  if (!edited) fail(`could not find worked-example input with value "${oldName}"`);

  // Dirty flag should now show; export and diff against the original.
  await page.waitForSelector(".dirty-dot", { timeout: 3000 });
  const out = await exportGeo();
  if (out.worked_examples.examples[0].name !== NEWNAME)
    fail(`docs edit not persisted: ${out.worked_examples.examples[0].name}`);
  if (JSON.stringify(Object.keys(out)) !== JSON.stringify(Object.keys(original)))
    fail("top-level key order changed on export");
  const restBefore = { ...original, worked_examples: null };
  const restAfter = { ...out, worked_examples: null };
  if (JSON.stringify(restBefore) !== JSON.stringify(restAfter))
    fail("content outside worked_examples changed on export");
  if (JSON.stringify(Object.keys(out.worked_examples.examples[0])) !==
      JSON.stringify(Object.keys(original.worked_examples.examples[0])))
    fail("worked-example item key order changed");

  // Remove the Governance section (confirm dialog), then undo restores it.
  await page.click(".docs-section-head:has-text('Governance') .prop-remove");
  await page.waitForSelector(".dialog", { timeout: 3000 });
  await page.click(".dialog .btn-danger-solid");
  await page.waitForTimeout(300);
  const afterRemove = await exportGeo();
  if ("governance" in afterRemove) fail("removed section still present on export");
  await page.keyboard.press(process.platform === "darwin" ? "Meta+z" : "Control+z");
  await page.waitForTimeout(300);
  const govBack = await page.$(".docs-section-head:has-text('Governance')");
  if (!govBack) fail("undo did not restore the removed section");

  // Add a brand-new section; it lands as a new top-level key.
  await page.fill(".docs-add-key", "curation_notes");
  await page.click(".docs-add-row .btn:has-text('Add section')");
  await page.waitForSelector(".docs-section-head:has-text('Curation notes')");
  const afterAdd = await exportGeo();
  if (!("curation_notes" in afterAdd)) fail("added section missing from export");
  if ("governance" in afterAdd === false) fail("undone section missing after add");
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();

if (errors.length === 0) {
  console.log("✓ Docs E2E passed: Graph|Docs toggle → edit worked example → dirty → geo export (edit persisted, key order + rest byte-conservative) → remove section w/ confirm → undo restores → add section");
  process.exit(0);
} else {
  console.error("✗ Docs verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
