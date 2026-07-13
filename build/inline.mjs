#!/usr/bin/env node
/*
 * build/inline.mjs — Onteater single-file packager.
 *
 * Takes the advanced-compiled application JS, the app CSS, and any vendored CSS
 * (KaTeX, with its web-font files folded in as base64 data: URIs), and inlines
 * all of it into resources/index.template.html, emitting dist/onteater.html — the
 * deliverable: one self-contained page that opens from file:// with no network
 * access except the Ollama calls the user makes.
 *
 * This is intentionally a plain Node script (build tooling, not shipped code):
 * the "HTML + ClojureScript only" constraint governs what SHIPS in the artifact,
 * not the build pipeline. Run after `shadow-cljs release app`:
 *     node build/inline.mjs
 *
 * Design notes:
 * - `</script>` inside the JS is escaped to `<\/script>` so the inlined script
 *   block cannot be terminated early by string/regex literals in the code.
 * - CSS `url(...)` references to local font/image files are rewritten to data:
 *   URIs so nothing is fetched at runtime. Remote url()s are left as-is (they
 *   degrade gracefully offline, e.g. remote scenario images).
 * - After assembly the output is scanned for tell-tale runtime network hooks
 *   (external <script src>, <link href> to http(s), importScripts, `import(`)
 *   and the build fails loudly if any remain — this is the file:// safety net.
 */
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(__dirname, "..");

const TEMPLATE = join(ROOT, "resources/index.template.html");
const APP_JS = join(ROOT, "resources/public/js/main.js");
const APP_CSS = join(ROOT, "resources/public/css/onteater.css");
const OUT_DIR = join(ROOT, "dist");
const OUT = join(OUT_DIR, "onteater.html");

const MIME = {
  woff2: "font/woff2",
  woff: "font/woff",
  ttf: "font/ttf",
  eot: "application/vnd.ms-fontobject",
  svg: "image/svg+xml",
  png: "image/png",
  jpg: "image/jpeg",
  jpeg: "image/jpeg",
  gif: "image/gif",
};

function read(path) {
  return readFileSync(path, "utf8");
}

/** Rewrite local url(...) refs in a CSS string to base64 data: URIs.
 *  `baseDir` resolves relative refs. Remote (http/https/data) refs are kept. */
function inlineCssAssets(css, baseDir) {
  return css.replace(/url\(\s*(['"]?)([^'")]+)\1\s*\)/g, (whole, _q, ref) => {
    const clean = ref.split(/[?#]/)[0].trim();
    if (/^(data:|https?:|\/\/)/.test(clean)) return whole; // leave remote/data as-is
    const assetPath = resolve(baseDir, clean);
    if (!existsSync(assetPath)) {
      console.warn(`  ! css asset not found, leaving as-is: ${clean}`);
      return whole;
    }
    const ext = clean.split(".").pop().toLowerCase();
    const mime = MIME[ext] || "application/octet-stream";
    const b64 = readFileSync(assetPath).toString("base64");
    return `url("data:${mime};base64,${b64}")`;
  });
}

/** Collect CSS to inline: the app stylesheet plus any vendored stylesheets that
 *  exist (KaTeX). Each is returned already asset-inlined. */
function collectStyles() {
  const parts = [];
  parts.push({ label: "onteater.css", css: read(APP_CSS), dir: dirname(APP_CSS) });

  const katexCss = join(ROOT, "node_modules/katex/dist/katex.min.css");
  if (existsSync(katexCss)) {
    parts.push({ label: "katex.min.css", css: read(katexCss), dir: dirname(katexCss) });
  }

  return parts
    .map(({ label, css, dir }) => {
      const inlined = inlineCssAssets(css, dir);
      return `/* === ${label} === */\n${inlined}`;
    })
    .join("\n");
}

function assertSelfContained(html) {
  const violations = [];
  // External resource references that would require network at load.
  const scriptSrc = /<script[^>]+\bsrc\s*=\s*["']?(?!data:)/i;
  const linkHref = /<link[^>]+href\s*=\s*["']?(?!data:)[^"'>]*\.(css|js)/i;
  if (scriptSrc.test(html)) violations.push("external <script src> present");
  if (linkHref.test(html)) violations.push("external <link href> to css/js present");
  if (/\bimportScripts\s*\(/.test(html)) violations.push("importScripts() present");
  if (/https?:\/\/[^"'\s)]+\.(js|css|woff2?|ttf)/i.test(html))
    violations.push("remote asset URL present");
  if (violations.length) {
    throw new Error(
      "Artifact is not self-contained:\n  - " + violations.join("\n  - ")
    );
  }
}

function main() {
  if (!existsSync(APP_JS)) {
    console.error(
      `Compiled JS not found at ${APP_JS}.\n` +
        `Run \`npx shadow-cljs release app\` (or \`compile app\`) first.`
    );
    process.exit(1);
  }

  const template = read(TEMPLATE);
  const styles = collectStyles();
  const js = read(APP_JS).replace(/<\/script>/gi, "<\\/script>");

  let html = template
    .replace("<!-- ONTEATER:STYLES -->", `<style>\n${styles}\n</style>`)
    .replace("<!-- ONTEATER:SCRIPT -->", `<script>\n${js}\n</script>`);

  assertSelfContained(html);

  if (!existsSync(OUT_DIR)) mkdirSync(OUT_DIR, { recursive: true });
  writeFileSync(OUT, html, "utf8");

  const kb = (Buffer.byteLength(html, "utf8") / 1024).toFixed(0);
  console.log(`✓ wrote ${OUT} (${kb} KB, self-contained)`);
}

main();
