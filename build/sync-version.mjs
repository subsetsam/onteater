#!/usr/bin/env node
// Sync package.json "version" from the single source of truth (src/onteater/version.txt).
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";
const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const version = readFileSync(join(ROOT, "src/onteater/version.txt"), "utf8").trim();
const pkgPath = join(ROOT, "package.json");
const pkg = JSON.parse(readFileSync(pkgPath, "utf8"));
if (pkg.version !== version) {
  pkg.version = version;
  writeFileSync(pkgPath, JSON.stringify(pkg, null, 2) + "\n", "utf8");
  console.log(`✓ package.json version -> ${version}`);
} else {
  console.log(`✓ package.json version already ${version}`);
}
