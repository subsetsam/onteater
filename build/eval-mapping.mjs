#!/usr/bin/env node
// Mapping-quality eval harness (PROMPT_PLAN P0.1). Runs the REAL mapping
// pipeline (compiled from src via the :eval shadow build) against a live
// Ollama, over the example scenarios × an ontology, and reports per run:
//
//   entries · invalid-target% (invented ids) · excerpt-not-found% (paraphrased
//   quotes) · shallow% (typed to a class that still has subclasses) · mean
//   confidence · per-chunk parse statuses
//
// No gold data needed — the metrics come straight from validate-entries.
// NOT part of `npm test` (needs a live model).
//
// Usage:
//   node build/eval-mapping.mjs [options]
//     --base-url  http://127.0.0.1:11434   Ollama base URL
//     --model     llama3.1                 Ollama model (required for LLM runs)
//     --ontology  examples/galactic-economic-ontology.json  (repeatable)
//     --scenario  examples/scenario-naboo-blockade.md       (repeatable;
//                 default: every examples/scenario-*.md)
//     --strategy  auto|full|scoped|staged  (default auto)
//     --seeds     1                        repeat runs per pair
//     --report-only                        just print prompt-size report (no LLM)
//
// Examples:
//   node build/eval-mapping.mjs --report-only
//   node build/eval-mapping.mjs --model qwen2.5:14b --strategy staged --seeds 3

import { readFileSync, readdirSync, existsSync } from 'node:fs';
import { execSync } from 'node:child_process';
import { resolve, basename } from 'node:path';
import { request as httpRequest } from 'node:http';

const root = resolve(new URL('..', import.meta.url).pathname);

// --- args -------------------------------------------------------------------
const args = process.argv.slice(2);
const opt = (name, dflt) => {
  const i = args.indexOf(`--${name}`);
  return i >= 0 ? args[i + 1] : dflt;
};
const optAll = (name) =>
  args.flatMap((a, i) => (a === `--${name}` ? [args[i + 1]] : []));
const flag = (name) => args.includes(`--${name}`);

const baseUrl = (opt('base-url', 'http://127.0.0.1:11434')).replace(/\/+$/, '');
const model = opt('model', null);
const strategy = opt('strategy', 'auto');
const seeds = parseInt(opt('seeds', '1'), 10);
const reportOnly = flag('report-only');

const ontologies = optAll('ontology').length
  ? optAll('ontology')
  : [resolve(root, 'examples/galactic-economic-ontology.json')];
const scenarios = optAll('scenario').length
  ? optAll('scenario')
  : readdirSync(resolve(root, 'examples'))
      .filter((f) => /^scenario-.*\.md$/.test(f))
      .map((f) => resolve(root, 'examples', f));

// --- compile + load the :eval library --------------------------------------
const evalJs = resolve(root, 'target/eval.js');
if (!existsSync(evalJs)) {
  console.error('target/eval.js missing — compiling (npx shadow-cljs compile eval)…');
  execSync('npx shadow-cljs compile eval', { cwd: root, stdio: 'inherit' });
}
const libMod = await import(evalJs); // CommonJS build — exports land on default
const lib = libMod.default ?? libMod;

// --- ollama transport --------------------------------------------------------
// node:http rather than fetch: local models can take many minutes per response
// and fetch's built-in undici headers/body timeouts (~5 min) kill the request
// mid-generation.
function chat(path, body) {
  return new Promise((resolvP, reject) => {
    const u = new URL(baseUrl + path);
    const req = httpRequest(
      { hostname: u.hostname, port: u.port, path: u.pathname, method: 'POST',
        headers: { 'content-type': 'application/json' } },
      (res) => {
        let data = '';
        res.on('data', (c) => (data += c));
        res.on('end', () => {
          if (res.statusCode !== 200)
            return reject(new Error(`${path} -> HTTP ${res.statusCode}: ${data}`));
          try {
            const msg = JSON.parse(data).message ?? {};
            resolvP(msg.content?.trim() ? msg.content : (msg.thinking ?? ''));
          } catch (e) { reject(e); }
        });
      }
    );
    req.setTimeout(0);
    req.on('error', reject);
    req.end(JSON.stringify(body));
  });
}

// --- run ---------------------------------------------------------------------
const fmtMetrics = (m) =>
  `entries=${m.entries}  invalid-target=${m['invalid-target']} (${m['invalid-target-pct']}%)  ` +
  `excerpt-miss=${m['excerpt-not-found']} (${m['excerpt-not-found-pct']}%)  ` +
  `shallow=${m.shallow} (${m['shallow-pct']}%)  conf=${m['mean-confidence']}  ` +
  `statuses=[${m.statuses.join(',')}]`;

for (const ontPath of ontologies) {
  const ont = lib.parseOntology(readFileSync(ontPath, 'utf8'));
  console.log(`\n=== Ontology: ${basename(ontPath)} ===`);

  for (const scPath of scenarios) {
    const text = readFileSync(scPath, 'utf8');
    const report = JSON.parse(lib.promptReport(ont, text));
    console.log(
      `\n--- ${basename(scPath)}  (auto→${report.auto}; tokens full=${report.full.tokens} ` +
        `scoped=${report.scoped.tokens} staged=${report.staged.tokens})`
    );
    if (reportOnly) continue;
    if (!model) {
      console.error('No --model given; use --report-only for the size report.');
      process.exit(1);
    }

    for (let seed = 0; seed < seeds; seed++) {
      const t0 = Date.now();
      const { strategy: used, requests } = JSON.parse(
        lib.buildBodies(ont, text, strategy, model)
      );
      const contents = [];
      for (const r of requests) contents.push(await chat(r.path, r.body));
      let state = JSON.parse(lib.evaluate(ont, text, JSON.stringify(contents)));

      let refineNote = '';
      if (used === 'staged') {
        const plan = JSON.parse(
          lib.refineBodies(ont, text, JSON.stringify(state.entries), model)
        );
        const rContents = [];
        for (const r of plan.requests) rContents.push(await chat(r.path, r.body));
        state = JSON.parse(
          lib.applyRefinements(
            ont, text, JSON.stringify(state.entries), JSON.stringify(rContents)
          )
        );
        refineNote = `  (+${plan.requests.length} refine calls)`;
      }

      const secs = ((Date.now() - t0) / 1000).toFixed(1);
      console.log(
        `  [${used}${seeds > 1 ? ` seed ${seed + 1}` : ''}] ${fmtMetrics(state.metrics)}  ` +
          `${secs}s${refineNote}`
      );
    }
  }
}
console.log();
