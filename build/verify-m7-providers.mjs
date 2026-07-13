#!/usr/bin/env node
/*
 * build/verify-m7-providers.mjs — multi-provider LLM settings, NO live cloud calls.
 * Served over http://127.0.0.1 (IndexedDB persistence needs a stable origin).
 *
 * Covers:
 *  - the tabbed LLM-settings dialog (Ollama | Cloud | Azure Gov), Ollama default
 *  - Cloud tab: Anthropic preset base-url (read-only), vendor switch to Custom,
 *    free-text model fallback, Test button reaching :checking → unreachable/error
 *    against a dead local port (never a real provider)
 *  - Azure Gov tab: CORS guidance help text present
 *  - persistence: non-secret fields survive a reload; an un-remembered API key
 *    does NOT; ticking "Remember" now opens a set-passphrase dialog and stores
 *    ciphertext (never plaintext) — the full unlock cycle is in verify-m8
 *
 * Part B mocks api.anthropic.com with Playwright route interception and runs a
 * REAL mapping run through the Cloud/Anthropic path end-to-end: it asserts the
 * exact wire format the app sends (x-api-key / anthropic-version /
 * anthropic-dangerous-direct-browser-access headers; system hoisted out of
 * messages; max_tokens present; NO temperature; strict output_config schema)
 * and that a canned Messages-API response flows through parsing into entry
 * cards. Nothing leaves the machine.
 *
 * Optional manual smoke (not run here): with a real key, set the Cloud tab to
 * Anthropic/OpenAI, paste the key, Test & load models, and run a one-chunk
 * mapping. This script deliberately makes no external network calls.
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
const IGNORE = [/React DevTools/i, /favicon/i, /Failed to load resource/i, /net::ERR/i,
                /Access to fetch/i, /CORS/i, /ERR_FAILED/i, /127\.0\.0\.1:1/];

const server = createServer((_req, res) => {
  res.writeHead(200, { "Content-Type": "text/html" });
  res.end(html);
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const httpUrl = `http://127.0.0.1:${server.address().port}/`;

const DUMMY_KEY = "sk-dummy-not-a-real-key";
const DEAD_URL = "http://127.0.0.1:1"; // nothing listens on port 1 — fetch fails fast

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1200, height: 800 } });
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

const openSettings = async () => {
  await page.click(".menubar-actions .icon-btn[title='LLM settings']");
  await page.waitForSelector(".settings-dialog");
};
// blur the focused field so its on-blur dispatch fires, then let re-frame settle
const blur = async () => { await page.click(".dialog-title"); await page.waitForTimeout(150); };
const activeTab = () => page.textContent(".settings-tab-active");

try {
  // Force the <input>/download picker fallback (needed by Part B's ontology load).
  await page.addInitScript(() => {
    try { delete window.showOpenFilePicker; delete window.showSaveFilePicker; } catch (_) {}
  });
  await page.goto(httpUrl, { waitUntil: "load", timeout: 20000 });

  // --- dialog + tabs ---------------------------------------------------------
  await openSettings();
  const tabs = await page.$$eval(".settings-tab", (ts) => ts.map((t) => t.textContent));
  if (tabs.join("|") !== "Ollama|Cloud|Azure Gov") fail(`unexpected tabs: ${tabs.join("|")}`);
  if ((await activeTab()) !== "Ollama") fail("Ollama is not the default active tab");
  const ollamaUrl = await page.inputValue(".settings-row input.insp-input");
  if (!/localhost:11434/.test(ollamaUrl)) fail(`Ollama base-url default wrong: ${ollamaUrl}`);

  // --- Cloud tab: Anthropic preset -------------------------------------------
  await page.click(".settings-tab:has-text('Cloud')");
  if ((await page.inputValue("select.cloud-vendor")) !== "anthropic") fail("cloud vendor default is not anthropic");
  if ((await page.inputValue(".cloud-base-url")) !== "https://api.anthropic.com") fail("anthropic preset base-url missing");
  if (!(await page.$(".cloud-base-url[readonly]"))) fail("preset base-url is not read-only");
  if (!(await page.$("input.cloud-model"))) fail("free-text model input missing when no models fetched");

  // --- Custom vendor + dead endpoint: test-connection error path -------------
  await page.selectOption("select.cloud-vendor", "custom");
  await page.fill(".cloud-base-url", DEAD_URL); await blur();
  await page.fill(".settings-dialog input[type=password]", DUMMY_KEY); await blur();
  await page.fill("input.cloud-model", "test-model"); await blur();
  await page.click(".llm-test");
  await page.waitForSelector(".conn-unreachable, .conn-error", { timeout: 8000 });
  console.log("  cloud: dead endpoint surfaced unreachable/error status");

  // --- Azure Gov tab: CORS guidance -------------------------------------------
  await page.click(".settings-tab:has-text('Azure Gov')");
  const help = await page.textContent(".settings-help");
  if (!/Azure endpoints usually block browser calls/.test(help || "")) fail(`azgov CORS guidance missing: "${help}"`);
  await page.fill(".azgov-endpoint", "https://myres.openai.azure.us"); await blur();

  // --- persistence: reload without remembering the key ------------------------
  await page.click(".settings-tab:has-text('Cloud')");
  await page.waitForTimeout(700); // let the IndexedDB write land
  await page.reload({ waitUntil: "load" });
  await openSettings();
  await page.waitForFunction(
    () => document.querySelector("select.cloud-vendor")?.value === "custom",
    null, { timeout: 5000 }
  ).catch(() => fail("vendor 'custom' not restored after reload"));
  if ((await activeTab()) !== "Cloud") fail("active provider (Cloud) not restored after reload");
  if ((await page.inputValue(".cloud-base-url")) !== DEAD_URL) fail("custom base-url not restored");
  if ((await page.inputValue("input.cloud-model")) !== "test-model") fail("model not restored");
  const key1 = await page.inputValue(".settings-dialog input[type=password]");
  if (key1 !== "") fail(`un-remembered API key survived reload: "${key1}"`);
  console.log("  persistence: non-secret fields restored; key correctly forgotten");

  // --- persistence: remember-key opt-in now ENCRYPTS (see verify-m8) ----------
  // Ticking "Remember" no longer stores plaintext; it opens a set-passphrase
  // dialog and persists only ciphertext. (The full unlock/load cycle lives in
  // build/verify-m8-key-encryption.mjs; here we just guard the opt-in contract.)
  await page.fill(".settings-dialog input[type=password]", DUMMY_KEY); await blur();
  await page.click(".settings-check input[type=checkbox]");
  await page.waitForSelector(".crypto-dialog", { timeout: 4000 });
  if ((await page.textContent(".crypto-dialog .dialog-title")) !== "Set a passphrase")
    fail("remember-key opt-in did not open the set-passphrase dialog");
  await page.fill(".crypto-pw", "pw-m7-passphrase");
  await page.fill(".crypto-pw2", "pw-m7-passphrase");
  await page.click(".crypto-submit");
  await page.waitForSelector(".crypto-dialog", { state: "detached", timeout: 4000 });
  await page.waitForTimeout(700);
  const rec = await page.evaluate(() => new Promise((res) => {
    const req = indexedDB.open("onteater", 1);
    req.onsuccess = () => {
      const g = req.result.transaction("kv", "readonly").objectStore("kv").get("llm-settings");
      g.onsuccess = () => res(g.result || ""); g.onerror = () => res("");
    };
    req.onerror = () => res("");
  }));
  if (rec.includes(DUMMY_KEY)) fail("plaintext key written to IndexedDB after opt-in");
  if (!rec.includes("[:cloud :custom]")) fail("encrypted blob for the slot not persisted");
  console.log("  persistence: opt-in encrypts the key (no plaintext at rest)");

  // --- Ollama tab untouched by all of the above --------------------------------
  await page.click(".settings-tab:has-text('Ollama')");
  await page.waitForFunction(  // tab switch is an async dispatch → render
    () => document.querySelector(".settings-tab-active")?.textContent === "Ollama",
    null, { timeout: 3000 }
  );
  const ollamaUrl2 = await page.inputValue(".settings-row input.insp-input");
  if (!/localhost:11434/.test(ollamaUrl2)) fail(`Ollama base-url disturbed: ${ollamaUrl2}`);
  await page.click(".dialog-actions .btn"); // Done

  // ===========================================================================
  // Part B — mocked Anthropic: full mapping run through the Cloud path.
  // ===========================================================================
  const SCEN_TEXT = "Princess Leia entrusted the secret plans to an astromech droid.";
  const MAPPING_JSON = JSON.stringify({
    entries: [
      { excerpt: "secret plans", occurrence: 1, node_id: "geo:InformationArtifact",
        relation: "mentions", confidence: 0.9, rationale: "The plans are an information artifact." },
      { excerpt: "astromech droid", occurrence: 1, node_id: "geo:Agent",
        relation: "instance-of", confidence: 0.8, rationale: "The droid acts as a carrier agent." },
    ],
    unmapped: [],
  });
  const cors = { "Access-Control-Allow-Origin": "*",
                 "Access-Control-Allow-Headers": "*",
                 "Access-Control-Allow-Methods": "*" };
  let capturedChat = null;
  await page.route("https://api.anthropic.com/**", async (route) => {
    const req = route.request();
    if (req.method() === "OPTIONS") {                     // CORS preflight
      return route.fulfill({ status: 204, headers: cors });
    }
    if (req.url().endsWith("/v1/models") && req.method() === "GET") {
      return route.fulfill({
        status: 200, headers: cors, contentType: "application/json",
        body: JSON.stringify({ data: [{ id: "claude-haiku-4-5" }, { id: "claude-opus-4-8" }] }),
      });
    }
    if (req.url().endsWith("/v1/messages") && req.method() === "POST") {
      capturedChat = { headers: req.headers(), body: req.postDataJSON() };
      return route.fulfill({
        status: 200, headers: cors, contentType: "application/json",
        body: JSON.stringify({
          content: [{ type: "text", text: MAPPING_JSON }],
          stop_reason: "end_turn", usage: { input_tokens: 1, output_tokens: 1 },
        }),
      });
    }
    return route.fulfill({ status: 404, headers: cors, body: "unmocked" });
  });

  // Load the ontology (picker fallback), then configure Anthropic in Settings.
  const [chooser] = await Promise.all([page.waitForEvent("filechooser"), page.click("button.btn-primary")]);
  await chooser.setFiles(SAMPLE_ONTO);
  await page.waitForSelector(".graph-canvas g.node");

  await openSettings();
  await page.click(".settings-tab:has-text('Cloud')");
  await page.selectOption("select.cloud-vendor", "anthropic");
  // A vendor switch clears the live key field (it selects a different saved
  // slot); wait for that remount to settle, then fill and confirm it stuck so
  // the on-blur dispatch carries the real value.
  await page.waitForFunction(
    () => document.querySelector(".settings-dialog input[type=password]")?.value === "",
    null, { timeout: 3000 });
  await page.fill(".settings-dialog input[type=password]", DUMMY_KEY);
  await page.waitForFunction(
    (k) => document.querySelector(".settings-dialog input[type=password]")?.value === k,
    DUMMY_KEY, { timeout: 2000 });
  await blur();
  await page.click(".llm-test");
  await page.waitForSelector(".conn-ok", { timeout: 8000 });
  await page.waitForSelector("select.cloud-model", { timeout: 3000 });
  const chosen = await page.inputValue("select.cloud-model");
  if (chosen !== "claude-opus-4-8") fail(`vendor default model not preselected: "${chosen}"`);
  console.log("  anthropic(mock): key accepted, models listed, default preselected");
  await page.click(".dialog-actions .btn"); // Done

  // Scenario tab: paste text, run the mapping through the mocked endpoint.
  await page.click(".workspace-tabs .tab:has-text('Scenario')");
  await page.waitForSelector(".scenario-input");
  await page.fill(".scenario-input", SCEN_TEXT);
  await page.click(".scenario-actions .btn-primary");
  await page.waitForSelector(".entry-card", { timeout: 15000 });
  const nEntries = await page.$$eval(".entry-card", (n) => n.length);
  if (nEntries !== 2) fail(`expected 2 entries from mocked mapping, got ${nEntries}`);
  console.log(`  anthropic(mock): mapping run produced ${nEntries} entries`);

  // Wire-format assertions on the captured /v1/messages request.
  if (!capturedChat) fail("no /v1/messages request captured");
  else {
    const h = capturedChat.headers, b = capturedChat.body;
    if (h["x-api-key"] !== DUMMY_KEY) fail("x-api-key header wrong/missing");
    if (h["anthropic-version"] !== "2023-06-01") fail("anthropic-version header wrong/missing");
    if (h["anthropic-dangerous-direct-browser-access"] !== "true") fail("browser-access header missing");
    if (b.model !== "claude-opus-4-8") fail(`body.model wrong: ${b.model}`);
    if (typeof b.max_tokens !== "number") fail("body.max_tokens missing");
    if ("temperature" in b) fail("temperature sent to Anthropic (models reject sampling params)");
    if (typeof b.system !== "string" || b.system.length < 100) fail("system prompt not hoisted to top level");
    if (!Array.isArray(b.messages) || b.messages.some((m) => m.role === "system"))
      fail("system message left inside messages[]");
    if (b.output_config?.format?.type !== "json_schema") fail("output_config.format missing");
    if (b.output_config?.format?.schema?.additionalProperties !== false)
      fail("schema not strictified (additionalProperties)");
    console.log("  anthropic(mock): wire format verified (headers, system hoist, strict schema, no temperature)");
  }
} catch (e) {
  fail("exception: " + e.message);
}

await browser.close();
server.close();

if (errors.length === 0) {
  console.log("✓ M7 passed: provider tabs, error surfacing, opt-in key persistence, and the mocked-Anthropic mapping run all behave");
  process.exit(0);
} else {
  console.error("✗ M7 verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
