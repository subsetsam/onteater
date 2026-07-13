#!/usr/bin/env node
/*
 * build/verify-drag.mjs — node dragging works in every layout, and dragged
 * positions persist across redraws. Runs against the advanced artifact via
 * file:// (no Ollama needed).
 *
 * Nodes can overlap, so we always identify (via elementFromPoint) which node is
 * actually under the cursor and measure THAT node's movement — otherwise we might
 * click one node but measure a different, un-dragged one sitting at the same spot.
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
const page = await browser.newPage({ viewport: { width: 1500, height: 950 } });
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

const xformOf = (id) => page.getAttribute(`.graph-canvas g.node[data-id="${id}"]`, "transform");
const parse = (t) => {
  const m = /translate\(([-\d.]+)[ ,]([-\d.]+)\)/.exec(t || "");
  return m ? { x: parseFloat(m[1]), y: parseFloat(m[2]) } : null;
};
// Pick the first node whose shape-centre is comfortably inside the canvas AND is
// the topmost element there (hittable). Force layouts can fling nodes off-screen,
// so we can't just take the first node in DOM order. One round trip so a moving
// node can't slip between reading and hit-testing.
function pickNode() {
  return page.evaluate(() => {
    const svg = document.querySelector(".graph-canvas").getBoundingClientRect();
    for (const g of document.querySelectorAll(".graph-canvas g.node[data-id]")) {
      const r = g.querySelector("path").getBoundingClientRect();
      const x = r.x + r.width / 2, y = r.y + r.height / 2;
      if (x < svg.x + 60 || x > svg.right - 60 || y < svg.y + 60 || y > svg.bottom - 60) continue;
      const el = document.elementFromPoint(x, y);
      const hit = el && el.closest && el.closest("g.node");
      if (hit) return { id: hit.getAttribute("data-id"), x, y };
    }
    return { id: null };
  });
}

const firstNodeId = () => page.$eval(".graph-canvas g.node[data-id]", (n) => n.getAttribute("data-id"));

// Drag whatever node is at (x,y) by (dx,dy), measuring that node's movement.
async function dragSomeNode(dx, dy) {
  const { id, x, y } = await pickNode();
  if (!id) return { id: null };
  const before = parse(await xformOf(id));
  await page.mouse.move(x, y);
  await page.mouse.down();
  await page.mouse.move(x + dx * 0.4, y + dy * 0.4, { steps: 6 });
  await page.mouse.move(x + dx, y + dy, { steps: 6 });
  await page.mouse.up();
  await page.waitForTimeout(250);
  const after = parse(await xformOf(id));
  return { id, before, after };
}

try {
  await page.addInitScript(() => { try { delete window.showOpenFilePicker; } catch (_) {} });
  await page.goto(ART, { waitUntil: "load", timeout: 20000 });
  const [chooser] = await Promise.all([page.waitForEvent("filechooser"), page.click("button.btn-primary")]);
  await chooser.setFiles(SAMPLE);
  await page.waitForSelector(".graph-canvas g.node");
  await page.dblclick(".graph-canvas g.node.meta"); // expand a module -> many nodes
  await page.waitForTimeout(800);

  // --- Tree layout (static): the node under the cursor follows the drag ---
  await page.click("button.chip:has-text('Tree')");
  await page.waitForTimeout(700);
  const t = await dragSomeNode(150, 95);
  if (!t.id || !t.before || !t.after) fail("tree: no node under cursor to drag");
  else {
    const mdx = t.after.x - t.before.x, mdy = t.after.y - t.before.y;
    if (Math.abs(mdx - 150) > 45 || Math.abs(mdy - 95) > 45)
      fail(`tree: node ${t.id} did not follow the drag (moved ${mdx.toFixed(0)},${mdy.toFixed(0)}, expected ~150,95)`);
    else console.log(`  tree: node ${t.id} moved ${mdx.toFixed(0)},${mdy.toFixed(0)} ✓`);

    // --- Persistence: a redraw (selecting another node) must NOT reset the drag ---
    const posAfterDrag = parse(await xformOf(t.id));
    const otherId = await page.$$eval(".graph-canvas g.node[data-id]",
      (ns, drag) => ns.map((n) => n.getAttribute("data-id")).find((i) => i !== drag), t.id);
    await page.click(`.graph-canvas g.node[data-id="${otherId}"]`);
    await page.waitForTimeout(400);
    const posAfterRedraw = parse(await xformOf(t.id));
    if (!posAfterRedraw || Math.abs(posAfterRedraw.x - posAfterDrag.x) > 3 || Math.abs(posAfterRedraw.y - posAfterDrag.y) > 3)
      fail("tree: dragged position did not persist across a redraw");
    else console.log("  tree: dragged position persisted across redraw ✓");
  }

  // --- Force layout (dynamic): the node under the cursor moves substantially ---
  await page.click("button.chip:has-text('Force')");
  // Wait until the sim is actually still (a node's position stops changing),
  // otherwise it drifts out from under the cursor before mousedown lands.
  {
    const id = await firstNodeId();
    let prev = null, stable = 0;
    for (let i = 0; i < 40 && stable < 3; i++) {
      await page.waitForTimeout(250);
      const p = parse(await xformOf(id));
      if (prev && Math.hypot(p.x - prev.x, p.y - prev.y) < 0.5) stable++;
      else stable = 0;
      prev = p;
    }
  }
  const f = await dragSomeNode(200, 130);
  if (!f.id || !f.before || !f.after) fail("force: no node under cursor to drag");
  else {
    const dist = Math.hypot(f.after.x - f.before.x, f.after.y - f.before.y);
    if (dist < 80) fail(`force: node ${f.id} barely moved (${dist.toFixed(0)}px)`);
    else console.log(`  force: node ${f.id} moved ${dist.toFixed(0)}px ✓`);
  }
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();

if (errors.length === 0) {
  console.log("✓ drag verified: nodes are draggable in tree and force layouts, and drags persist across redraws");
  process.exit(0);
} else {
  console.error("✗ drag verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
