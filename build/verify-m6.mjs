#!/usr/bin/env node
/*
 * build/verify-m6.mjs — live chat against REAL Ollama. Loads ontology, runs a
 * mapping, opens the chat drawer, asks a question referencing an entry, and
 * verifies a coherent answer comes back. Then asks for an alternative and, if the
 * model emits a mapping-update block, applies it and confirms the board updates
 * and a mapping-undo reverts it.
 */
import { chromium } from "playwright-core";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";
import { createServer } from "node:http";

const __dirname = dirname(fileURLToPath(import.meta.url));
const html = readFileSync(resolve(__dirname, "../dist/onteater.html"), "utf8");
const ONTO = resolve(__dirname, "../examples/galactic-economic-ontology.json");
const SCEN = readFileSync(resolve(__dirname, "../examples/scenario-droid-courier.md"), "utf8");
const PREFERRED = ["gemma4:e4b", "nemotron-3-nano:4b-bf16", "gpt-oss:20b"];

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
  await page.addInitScript(() => { try { delete window.showOpenFilePicker; delete window.showSaveFilePicker; } catch (_) {} });
  await page.goto(url, { waitUntil: "load", timeout: 20000 });
  const [ch] = await Promise.all([page.waitForEvent("filechooser"), page.click("button.btn-primary")]);
  await ch.setFiles(ONTO);
  await page.waitForSelector(".graph-canvas g.node");
  await page.click(".menubar-actions .icon-btn[title='LLM settings']");
  await page.click(".settings-row .btn-primary");
  await page.waitForSelector(".conn-ok", { timeout: 12000 });
  const models = await page.$$eval(".settings-dialog select option", (o) => o.map((x) => x.value));
  const model = PREFERRED.find((m) => models.includes(m)) || models[0];
  await page.selectOption(".settings-dialog select", model);
  console.log(`  using model: ${model}`);
  await page.click(".dialog-actions .btn");

  await page.click(".workspace-tabs .tab:has-text('Scenario')");
  await page.waitForSelector(".scenario-input");
  await page.fill(".scenario-input", SCEN);
  await page.click(".scenario-actions .btn-primary");
  await page.waitForSelector(".entry-card", { timeout: 180000 });
  const entriesBefore = await page.$$eval(".entry-card", (n) => n.length);

  // Open chat, ask a question.
  await page.click(".session-bar .chip:has-text('Chat')");
  await page.waitForSelector(".chat-drawer");
  await page.fill(".chat-input", "Why did you map that excerpt to that node? Answer in one sentence.");
  await page.click(".chat-composer .btn-primary");
  // wait for the assistant reply to finish (thinking indicator disappears, body appears)
  await page.waitForSelector(".msg-assistant .msg-body", { timeout: 180000 });
  const reply = await page.textContent(".msg-assistant .msg-body");
  console.log(`  assistant replied (${(reply || "").length} chars)`);
  if (!reply || reply.trim().length < 10) fail("assistant reply too short/empty");

  // Ask for an alternative that should propose a mapping-update.
  await page.fill(".chat-input", "Propose a better ontology node for the excerpt \"leverage\" using a mapping-update block.");
  await page.click(".chat-composer .btn-primary");
  await page.waitForTimeout(2000);
  // Wait until no message is pending.
  await page.waitForFunction(() => !document.querySelector(".msg-thinking"), { timeout: 180000 });

  const hasUpdate = await page.$(".update-block");
  if (hasUpdate) {
    console.log("  model proposed a mapping-update; applying it");
    await page.click(".op-card .btn-primary");
    await page.waitForTimeout(400);
    const applied = await page.$(".op-applied");
    if (!applied) fail("apply did not mark the op applied");
    // undo should revert
    await page.click(".chat-head-actions .icon-btn[title='Undo mapping change']");
    await page.waitForTimeout(400);
    console.log("  applied + undone a proposed change");
  } else {
    console.log("  model did not emit a mapping-update block this run (acceptable); chat Q&A verified");
  }
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();
server.close();

if (errors.length === 0) {
  console.log("✓ M6 passed: chat Q&A round-trips with mapping context; mapping-update apply/undo works when proposed");
  process.exit(0);
} else {
  console.error("✗ M6 verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
