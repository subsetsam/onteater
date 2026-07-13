#!/usr/bin/env node
/*
 * build/verify-file-url.mjs — headless file:// smoke test for the artifact.
 *
 * file:// breakages (absolute paths, workers, import(),
 * CSP quirks) only surface in a real browser, not localhost. This script opens
 * dist/onteater.html via the file:// protocol in headless Chromium, captures
 * console errors and page errors, and asserts the ClojureScript app actually
 * booted and mounted its shell. Run after every `npm run build`.
 *
 * Usage: node build/verify-file-url.mjs [--selector ".menubar"] [--expect "text"]
 * Exit 0 = booted clean; non-zero = a console/page error or a missing mount.
 */
import { chromium } from "playwright-core";
import { pathToFileURL } from "node:url";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ARTIFACT = resolve(__dirname, "../dist/onteater.html");
const url = pathToFileURL(ARTIFACT).href;

// Ignore benign noise; fail on everything else.
const IGNORE = [/Download the React DevTools/i, /favicon\.ico/i];

const args = process.argv.slice(2);
const selArg = args.indexOf("--selector");
const SELECTOR = selArg >= 0 ? args[selArg + 1] : ".app-root .menubar .brand-name";
const expArg = args.indexOf("--expect");
const EXPECT = expArg >= 0 ? args[expArg + 1] : "Onteater";

const errors = [];

const browser = await chromium.launch();
const page = await browser.newPage();

page.on("console", (msg) => {
  if (msg.type() === "error") {
    const t = msg.text();
    if (!IGNORE.some((re) => re.test(t))) errors.push(`console.error: ${t}`);
  }
});
page.on("pageerror", (err) => errors.push(`pageerror: ${err.message}`));

let mounted = false;
let text = "";
try {
  await page.goto(url, { waitUntil: "load", timeout: 20000 });
  await page.waitForSelector(SELECTOR, { timeout: 8000 });
  mounted = true;
  text = (await page.textContent(SELECTOR)) || "";
} catch (e) {
  errors.push(`mount check failed for selector ${SELECTOR}: ${e.message}`);
}

await browser.close();

const okText = text.includes(EXPECT);
if (errors.length === 0 && mounted && okText) {
  console.log(`✓ file:// boot OK — "${SELECTOR}" present, text contains "${EXPECT}"`);
  process.exit(0);
} else {
  console.error("✗ file:// verification FAILED");
  if (!mounted) console.error(`  - app did not mount (selector ${SELECTOR} absent)`);
  if (mounted && !okText)
    console.error(`  - selector present but text "${text.trim().slice(0, 60)}" lacks "${EXPECT}"`);
  for (const e of errors) console.error("  - " + e);
  process.exit(1);
}
