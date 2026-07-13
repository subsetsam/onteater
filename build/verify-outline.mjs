#!/usr/bin/env node
/*
 * build/verify-outline.mjs — the left-panel outline tree-view: modules/spine/
 * relations expand into subgroups and members, with no duplicate members, and
 * Expand all / Collapse all work.
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

const groupHeadWith = (text) =>
  page.locator(`.tree-group-head`, { hasText: text }).first();

try {
  await page.addInitScript(() => { try { delete window.showOpenFilePicker; } catch (_) {} });
  await page.goto(ART, { waitUntil: "load", timeout: 20000 });
  const [chooser] = await Promise.all([page.waitForEvent("filechooser"), page.click("button.btn-primary")]);
  await chooser.setFiles(SAMPLE);
  await page.waitForSelector(".graph-canvas g.node");
  await page.waitForSelector(".tree-group");

  // Nothing expanded initially.
  if ((await page.$$(".tree-children")).length !== 0) fail("tree starts expanded (should be collapsed)");

  // Expand the INS module by clicking its row.
  await groupHeadWith("Module INS").click();
  await page.waitForTimeout(200);

  // It should reveal its subgroups (families) but NOT dump all 93 members flat.
  const insMembersFlat = await page.$$eval(
    ".tree-group:has(> .tree-group-head:has-text('Module INS')) > .tree-children > .tree-member",
    (n) => n.length
  ).catch(() => -1);
  const insSubgroups = await page.$$eval(
    ".tree-group:has(> .tree-group-head:has-text('Module INS')) > .tree-children > .tree-group",
    (n) => n.length
  ).catch(() => -1);
  console.log(`  INS expanded -> ${insSubgroups} subgroups, ${insMembersFlat} direct members`);
  if (insSubgroups < 5) fail(`INS should show its family subgroups (got ${insSubgroups})`);
  if (insMembersFlat > 5) fail(`INS dumped ${insMembersFlat} members flat (duplicate-member bug not fixed)`);

  // Expand a family (INS "Trade and route measures") -> it should show member leaves.
  await groupHeadWith("Trade and route measures").click();
  await page.waitForTimeout(200);
  const familyMembers = await page.$$eval(".tree-member", (n) => n.length);
  if (familyMembers < 3) fail(`family "Trade and route measures" showed no members (got ${familyMembers})`);
  else console.log(`  family "Trade and route measures" expanded -> ${familyMembers} member leaves visible`);

  // A section with no subgroups (Spine) expands directly to member leaves.
  await groupHeadWith("Spine").click();
  await page.waitForTimeout(200);
  const afterSpine = await page.$$eval(".tree-member", (n) => n.length);
  if (afterSpine <= familyMembers) fail("Spine did not expand to show its member classes");
  else console.log(`  Spine expanded -> more members visible (${afterSpine})`);

  // Clicking a member leaf selects it (inspector fills).
  await page.click(".tree-member");
  await page.waitForSelector(".insp-title", { timeout: 3000 });

  // Expand all / Collapse all.
  await page.click(".tree-toolbtn:has-text('Expand all')");
  await page.waitForTimeout(300);
  const allExpanded = await page.$$eval(".tree-children", (n) => n.length);
  if (allExpanded < 20) fail(`Expand all did not open the tree (${allExpanded} open lists)`);
  else console.log(`  Expand all -> ${allExpanded} expanded lists`);
  await page.click(".tree-toolbtn:has-text('Collapse all')");
  await page.waitForTimeout(200);
  if ((await page.$$(".tree-children")).length !== 0) fail("Collapse all did not close the tree");

  // The hover 'show on canvas' button still opens a module view.
  await groupHeadWith("Module POW").hover();
  await page.locator(".tree-group-head", { hasText: "Module POW" }).locator(".tree-focus").click();
  await page.waitForTimeout(400);
  const crumb = await page.textContent(".breadcrumbs");
  if (!/POW|module/i.test(crumb || "")) fail("show-on-canvas did not open the module view");
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();

if (errors.length === 0) {
  console.log("✓ outline tree-view: groups expand into subgroups+members (no duplicates), expand/collapse-all, and show-on-canvas all work");
  process.exit(0);
} else {
  console.error("✗ outline verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
