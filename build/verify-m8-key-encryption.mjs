#!/usr/bin/env node
/*
 * build/verify-m8-key-encryption.mjs — passphrase-encrypted credential storage.
 * Served over http://127.0.0.1 (a secure context, so crypto.subtle is available;
 * IndexedDB persistence needs a stable origin).
 *
 * Covers the full spec flow, with NO network calls:
 *  Part A — save two credentials, each under its own slot, encrypted with one
 *           passphrase: a Cloud/Anthropic API key (via the set-passphrase
 *           dialog) and an Azure Gov Bearer token (silent, passphrase already
 *           in memory). Then assert the IndexedDB `llm-settings` record holds
 *           only ciphertext (ct/iv/salt) — never the plaintext key or token.
 *  Part B — reload (locks the session). No boot prompt; key fields blank. Load
 *           the Anthropic key via the "Load saved API key" dropdown: Cancel
 *           leaves the field blank; a wrong passphrase is rejected; the correct
 *           passphrase fills it. Then the Azure Bearer token loads with NO
 *           second prompt (one unlock reveals every slot).
 *  Part C — a file:// secure-context probe: open the built artifact from file://
 *           and run a raw WebCrypto AES-GCM round-trip in-page, proving the
 *           shipped single-file mode has crypto.subtle (the flagged risk).
 */
import { chromium } from "playwright-core";
import { resolve, dirname } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { readFileSync } from "node:fs";
import { createServer } from "node:http";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ARTPATH = resolve(__dirname, "../dist/onteater.html");
const html = readFileSync(ARTPATH, "utf8");

const errors = [];
const fail = (m) => errors.push(m);
const IGNORE = [/React DevTools/i, /favicon/i, /Failed to load resource/i, /net::ERR/i];

const ANTHROPIC_KEY = "sk-ant-PLAINTEXT-do-not-store-11";
const AZURE_TOKEN = "eyJ-bearer-PLAINTEXT-do-not-store-11";
const PASSPHRASE = "correct horse battery staple";

const server = createServer((_req, res) => {
  res.writeHead(200, { "Content-Type": "text/html" });
  res.end(html);
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const httpUrl = `http://127.0.0.1:${server.address().port}/`;

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1200, height: 800 } });
page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail("console: " + m.text()); });
page.on("pageerror", (e) => fail("pageerror: " + e.message));

const openSettings = async () => {
  await page.click(".menubar-actions .icon-btn[title='LLM settings']");
  await page.waitForSelector(".settings-dialog");
};
const blur = async () => { await page.click(".dialog-title"); await page.waitForTimeout(150); };

// Read the persisted llm-settings EDN string straight out of IndexedDB.
const readLlmSettings = () => page.evaluate(() => new Promise((res, rej) => {
  const req = indexedDB.open("onteater", 1);
  req.onsuccess = () => {
    try {
      const tx = req.result.transaction("kv", "readonly");
      const g = tx.objectStore("kv").get("llm-settings");
      g.onsuccess = () => res(g.result || null);
      g.onerror = () => rej(g.error);
    } catch (e) { rej(e); }
  };
  req.onerror = () => rej(req.error);
}));

const waitForSaved = async (needle) => {
  await page.waitForFunction((n) => new Promise((res) => {
    const req = indexedDB.open("onteater", 1);
    req.onsuccess = () => {
      try {
        const g = req.result.transaction("kv", "readonly").objectStore("kv").get("llm-settings");
        g.onsuccess = () => res(typeof g.result === "string" && g.result.includes(n));
        g.onerror = () => res(false);
      } catch (_) { res(false); }
    };
    req.onerror = () => res(false);
  }), needle, { timeout: 6000 });
};

try {
  await page.addInitScript(() => {
    try { delete window.showOpenFilePicker; delete window.showSaveFilePicker; } catch (_) {}
  });
  await page.goto(httpUrl, { waitUntil: "load", timeout: 20000 });

  // Sanity: 127.0.0.1 is a secure context, so the crypto is actually exercised.
  const secure = await page.evaluate(() => window.isSecureContext && !!window.crypto?.subtle);
  if (!secure) fail("crypto.subtle unavailable over http://127.0.0.1 (should be a secure context)");

  // ===========================================================================
  // Part A — save two credentials, encrypted at rest.
  // ===========================================================================
  await openSettings();

  // Cloud / Anthropic: enter key, tick Remember → set-passphrase dialog.
  await page.click(".settings-tab:has-text('Cloud')");
  await page.fill(".settings-dialog input[type=password]", ANTHROPIC_KEY); await blur();
  await page.click(".settings-check input[type=checkbox]");
  await page.waitForSelector(".crypto-dialog", { timeout: 4000 });
  if ((await page.textContent(".crypto-dialog .dialog-title")) !== "Set a passphrase")
    fail("first save did not open the set-passphrase dialog");
  await page.fill(".crypto-pw", PASSPHRASE);
  await page.fill(".crypto-pw2", PASSPHRASE);
  await page.click(".crypto-submit");
  await page.waitForSelector(".crypto-dialog", { state: "detached", timeout: 4000 });
  await waitForSaved("[:cloud :anthropic]");
  console.log("  A: Anthropic key saved via set-passphrase dialog");

  // Azure Gov / Bearer: passphrase already in memory → silent encrypt, no dialog.
  await page.click(".settings-tab:has-text('Azure Gov')");
  await page.selectOption("select.azgov-auth", "bearer");
  await page.fill(".settings-dialog input[type=password]", AZURE_TOKEN); await blur();
  await page.click(".settings-check input[type=checkbox]");
  await page.waitForTimeout(400);
  if (await page.$(".crypto-dialog")) fail("Azure save re-prompted for a passphrase (should reuse the session one)");
  await waitForSaved("[:azgov :bearer]");
  console.log("  A: Azure Bearer token saved silently (session passphrase reused)");

  // Assert the stored record is ciphertext-only.
  const stored = await readLlmSettings();
  if (!stored) fail("no llm-settings persisted");
  else {
    if (stored.includes(ANTHROPIC_KEY)) fail("PLAINTEXT Anthropic key found in IndexedDB!");
    if (stored.includes(AZURE_TOKEN)) fail("PLAINTEXT Azure token found in IndexedDB!");
    if (!stored.includes("[:cloud :anthropic]") || !stored.includes("[:azgov :bearer]"))
      fail("expected both saved slots in the record");
    if (!(stored.includes(":ct") && stored.includes(":iv") && stored.includes(":salt")))
      fail("ciphertext fields (:ct/:iv/:salt) missing from saved blobs");
    console.log("  A: IndexedDB holds only ciphertext — no plaintext key/token");
  }
  await page.click(".dialog-actions .btn"); // Done

  // ===========================================================================
  // Part B — reload (locks), then load on demand.
  // ===========================================================================
  await page.reload({ waitUntil: "load" });
  if (await page.$(".crypto-dialog")) fail("a passphrase dialog auto-opened at boot (should not)");
  await openSettings();
  await page.click(".settings-tab:has-text('Cloud')");
  // Anthropic is the default vendor; field must start blank (locked).
  if ((await page.inputValue(".settings-dialog input[type=password]")) !== "")
    fail("Anthropic key field not blank after reload");
  if (!(await page.$(".secret-menu"))) fail("'Load saved' dropdown missing for a slot with a saved key");

  const openLoadMenu = async () => {
    await page.click(".secret-menu-btn");
    await page.click(".secret-load");
  };

  // Cancel leaves the field blank.
  await openLoadMenu();
  await page.waitForSelector(".crypto-dialog");
  await page.click(".crypto-dialog button:has-text('Cancel')");
  await page.waitForSelector(".crypto-dialog", { state: "detached" });
  if ((await page.inputValue(".settings-dialog input[type=password]")) !== "")
    fail("Cancel did not leave the key field blank");
  console.log("  B: Cancel on the unlock prompt left the field blank");

  // Wrong passphrase is rejected; field stays blank.
  await openLoadMenu();
  await page.waitForSelector(".crypto-dialog");
  await page.fill(".crypto-pw", "not the passphrase");
  await page.click(".crypto-submit");
  await page.waitForSelector(".crypto-dialog .conn-detail", { timeout: 4000 });
  if (!/Incorrect passphrase/.test(await page.textContent(".crypto-dialog .conn-detail")))
    fail("wrong passphrase did not surface an error");
  // Correct passphrase fills the key.
  await page.fill(".crypto-pw", PASSPHRASE);
  await page.click(".crypto-submit");
  await page.waitForSelector(".crypto-dialog", { state: "detached", timeout: 4000 });
  await page.waitForFunction(
    (k) => document.querySelector(".settings-dialog input[type=password]")?.value === k,
    ANTHROPIC_KEY, { timeout: 4000 }
  ).catch(() => fail("correct passphrase did not fill the Anthropic key"));
  console.log("  B: wrong passphrase rejected; correct one filled the Anthropic key");

  // Azure Bearer token loads with NO second prompt (session now unlocked).
  // The auth-scheme (a non-secret preference) was restored to :bearer, so first
  // switch to the API-key scheme to prove the dropdown is gated per-scheme...
  await page.click(".settings-tab:has-text('Azure Gov')");
  await page.selectOption("select.azgov-auth", "api-key");
  await page.waitForFunction(() => !document.querySelector(".secret-menu"), null, { timeout: 3000 })
    .catch(() => fail("'Load saved' shown for the un-saved API-key scheme"));
  // ...then back to Bearer, which does have a saved blob.
  await page.selectOption("select.azgov-auth", "bearer");
  await page.waitForSelector(".secret-menu");
  await page.click(".secret-menu-btn");
  await page.click(".secret-load");
  await page.waitForTimeout(300);
  if (await page.$(".crypto-dialog")) fail("Azure token load re-prompted (already unlocked this session)");
  await page.waitForFunction(
    (t) => document.querySelector(".settings-dialog input[type=password]")?.value === t,
    AZURE_TOKEN, { timeout: 4000 }
  ).catch(() => fail("Azure Bearer token did not fill without a prompt"));
  console.log("  B: Azure Bearer token filled with no second prompt");
} catch (e) {
  fail("exception: " + e.message);
}

// ===========================================================================
// Part C — file:// secure-context probe (the shipped mode).
// ===========================================================================
try {
  const filePage = await browser.newPage();
  await filePage.goto(pathToFileURL(ARTPATH).href, { waitUntil: "load", timeout: 20000 });
  const roundTrip = await filePage.evaluate(async () => {
    if (!(window.isSecureContext && window.crypto?.subtle)) return "no-subtle";
    const enc = new TextEncoder();
    const salt = crypto.getRandomValues(new Uint8Array(16));
    const iv = crypto.getRandomValues(new Uint8Array(12));
    const base = await crypto.subtle.importKey("raw", enc.encode("pw"), { name: "PBKDF2" }, false, ["deriveKey"]);
    const key = await crypto.subtle.deriveKey(
      { name: "PBKDF2", salt, iterations: 1000, hash: "SHA-256" },
      base, { name: "AES-GCM", length: 256 }, false, ["encrypt", "decrypt"]);
    const ct = await crypto.subtle.encrypt({ name: "AES-GCM", iv }, key, enc.encode("secret"));
    const pt = await crypto.subtle.decrypt({ name: "AES-GCM", iv }, key, ct);
    return new TextDecoder().decode(pt);
  });
  if (roundTrip !== "secret") fail(`file:// WebCrypto round-trip failed: got "${roundTrip}"`);
  else console.log("  C: file:// is a secure context — WebCrypto AES-GCM round-trip works");
  await filePage.close();
} catch (e) {
  fail("file:// probe exception: " + e.message);
}

await browser.close();
server.close();

if (errors.length === 0) {
  console.log("✓ M8 passed: credentials encrypted at rest, per-slot on-demand unlock, one-passphrase reveal, file:// crypto OK");
  process.exit(0);
} else {
  console.error("✗ M8 verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
