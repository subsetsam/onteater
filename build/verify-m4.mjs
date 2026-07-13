#!/usr/bin/env node
/*
 * build/verify-m4.mjs — Ollama plumbing against the REAL local server.
 * Part A: served over http://127.0.0.1 (an origin Ollama accepts by default) —
 *         Test connection lists the real installed models.
 * Part B: loaded via file:// (origin null) — Test connection surfaces the CORS
 *         guidance rather than failing silently.
 */
import { chromium } from "playwright-core";
import { pathToFileURL } from "node:url";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";
import { createServer } from "node:http";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ARTPATH = resolve(__dirname, "../dist/onteater.html");
const html = readFileSync(ARTPATH, "utf8");

const errors = [];
const fail = (m) => errors.push(m);
const IGNORE = [/React DevTools/i, /favicon/i, /Failed to load resource/i, /net::ERR/i,
                /Access to fetch/i, /CORS/i, /ERR_FAILED/i, /11434/];

// Serve the single file on a random localhost port.
const server = createServer((_req, res) => {
  res.writeHead(200, { "Content-Type": "text/html" });
  res.end(html);
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const port = server.address().port;
const httpUrl = `http://127.0.0.1:${port}/`;

const browser = await chromium.launch();

async function testConnection(url, { expectOk }) {
  const page = await browser.newPage({ viewport: { width: 1200, height: 800 } });
  page.on("console", (m) => { if (m.type() === "error" && !IGNORE.some((r) => r.test(m.text()))) fail(`[${expectOk ? "http" : "file"}] console: ` + m.text()); });
  page.on("pageerror", (e) => fail(`[${expectOk ? "http" : "file"}] pageerror: ` + e.message));
  try {
    await page.goto(url, { waitUntil: "load", timeout: 20000 });
    await page.click(".menubar-actions .icon-btn[title='LLM settings']");
    await page.waitForSelector(".settings-dialog");
    await page.click(".settings-row .btn-primary"); // Test connection
    if (expectOk) {
      await page.waitForSelector(".conn-ok", { timeout: 12000 });
      const opts = await page.$$eval(".settings-dialog select option", (o) => o.map((x) => x.value));
      if (opts.length < 1) fail("http: no models listed after successful connection");
      else console.log(`  http: connected, models = [${opts.join(", ")}]`);
    } else {
      // file:// -> expect the unreachable/CORS state with guidance mentioning OLLAMA_ORIGINS.
      await page.waitForSelector(".conn-unreachable, .conn-error", { timeout: 12000 });
      const detail = await page.textContent(".conn-detail");
      if (!/OLLAMA_ORIGINS/.test(detail || "")) fail(`file://: CORS guidance missing: "${detail}"`);
      else console.log("  file://: surfaced CORS guidance as expected");
    }
  } catch (e) {
    fail(`[${expectOk ? "http" : "file"}] exception: ` + e.message);
  }
  await page.close();
}

await testConnection(httpUrl, { expectOk: true });
await testConnection(pathToFileURL(ARTPATH).href, { expectOk: false });

await browser.close();
server.close();

if (errors.length === 0) {
  console.log("✓ M4 passed: real Ollama models listed over http; CORS guidance shown over file://");
  process.exit(0);
} else {
  console.error("✗ M4 verification FAILED:");
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
