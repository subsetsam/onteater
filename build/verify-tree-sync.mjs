#!/usr/bin/env node
/*
 * build/verify-tree-sync.mjs — the left-pane tree-view stays in sync with graph
 * navigation: double-clicking a module bubble expands that module in the tree;
 * double-clicking a node reveals it (ancestors expanded, leaf selected + scrolled
 * into view); and pressing the canvas 'back' button does NOT collapse the tree.
 */
import { chromium } from "playwright-core";
import { pathToFileURL } from "node:url";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ART = pathToFileURL(resolve(__dirname, "../dist/onteater.html")).href;
const SAMPLE = resolve(__dirname, "../examples/galactic-economic-ontology.json");

const errors = [];
const fail = (m) => errors.push(m);
const IGNORE = [/React DevTools/i, /favicon/i];

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1400, height: 950 } });
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

const openLists = () => page.$$eval(".tree-children", (n) => n.length);
const selectedMemberLabel = () =>
  page.$eval(".tree-member.tree-selected .tree-label", (n) => n.textContent).catch(() => null);

// Pick an on-screen, hittable node (optionally only meta bubbles); return its
// centre, id and graph label.
function pickNode(metaOnly) {
  return page.evaluate((meta) => {
    const svg = document.querySelector(".graph-canvas").getBoundingClientRect();
    const sel = meta ? "g.node.meta[data-id]" : "g.node:not(.meta)[data-id]";
    for (const g of document.querySelectorAll(".graph-canvas " + sel)) {
      const r = g.querySelector("path").getBoundingClientRect();
      const x = r.x + r.width / 2, y = r.y + r.height / 2;
      if (x < svg.x + 70 || x > svg.right - 70 || y < svg.y + 70 || y > svg.bottom - 70) continue;
      const el = document.elementFromPoint(x, y);
      const hit = el && el.closest && el.closest("g.node");
      if (hit) return { id: hit.getAttribute("data-id"), x, y,
                        label: (hit.querySelector(".node-label") || {}).textContent || "" };
    }
    return { id: null };
  }, !!metaOnly);
}

// Wait until the (sim) layout stops moving a node, so dblclick lands on it.
async function settle(sampleId) {
  let prev = null, stable = 0;
  for (let i = 0; i < 40 && stable < 3; i++) {
    await page.waitForTimeout(200);
    const t = await page.getAttribute(`.graph-canvas g.node[data-id="${sampleId}"]`, "transform").catch(() => null);
    if (t && prev === t) stable++; else stable = 0;
    prev = t;
  }
}

try {
  await page.addInitScript(() => { try { delete window.showOpenFilePicker; } catch (_) {} });
  await page.goto(ART, { waitUntil: "load", timeout: 20000 });
  const [chooser] = await Promise.all([page.waitForEvent("filechooser"), page.click("button.btn-primary")]);
  await chooser.setFiles(SAMPLE);
  await page.waitForSelector(".graph-canvas g.node");
  await page.waitForSelector(".tree-group");

  // 1) Tree starts collapsed.
  if ((await openLists()) !== 0) fail("tree did not start collapsed");

  // 2) Double-click a module bubble -> that module expands in the tree.
  const meta = await pickNode(true);
  if (!meta.id) fail("no on-screen module bubble found");
  else {
    await page.mouse.click(meta.x, meta.y, { clickCount: 2 });
    await page.waitForTimeout(500);
    if ((await openLists()) < 1) fail("double-clicking a module bubble did not expand the tree");
    else console.log(`  module bubble "${meta.label.trim()}" -> tree expanded (${await openLists()} open lists)`);
  }

  // 3) Double-click a real node -> it is revealed + selected in the tree.
  await settle(await pickNode(false).then((p) => p.id));
  const node = await pickNode(false);
  if (!node.id) { fail("no on-screen node to double-click"); }
  else {
    await page.mouse.click(node.x, node.y, { clickCount: 2 });
    await page.waitForTimeout(600);
    const label = await selectedMemberLabel();
    const graphLabel = node.label.replace(/…\s*$/, "").trim();
    if (!label) fail("no selected member appeared in the tree after double-click");
    else if (graphLabel && !label.trim().startsWith(graphLabel))
      fail(`revealed the wrong node: tree "${label}" vs graph "${node.label}"`);
    else console.log(`  node "${node.label.trim()}" -> revealed + selected in tree as "${label.trim()}"`);
  }

  const openBefore = await openLists();
  const selBefore = await selectedMemberLabel();

  // 4) Back must NOT collapse the tree.
  await page.click(".chip-back");
  await page.waitForTimeout(500);
  const openAfter = await openLists();
  const selAfter = await selectedMemberLabel();
  if (openAfter < openBefore) fail(`'back' collapsed the tree (${openBefore} -> ${openAfter} open lists)`);
  else if (selBefore && selAfter !== selBefore) fail("'back' lost the revealed selection in the tree");
  else console.log(`  after 'back': tree still expanded (${openAfter} lists), selection kept ("${(selAfter || "").trim()}")`);
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();

if (errors.length === 0) {
  console.log("✓ tree-view syncs with graph navigation; 'back' does not collapse it");
  process.exit(0);
} else {
  console.error("✗ tree-sync verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
