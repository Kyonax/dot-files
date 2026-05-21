#!/usr/bin/env node
// set-template-fields.mjs <template_id|mixin_key> --src <config.json> [--version N] [--mode replace|merge] [--confirm]
//
// Updates the `config` array on a templateVersion — i.e., the editable field
// schema a Tophat author sees when configuring this template's settings.
//
// Modes:
//   --mode replace  (default) — Replace the entire config array with the new one.
//                                The src file's array becomes the new schema.
//   --mode merge              — Upsert by `name`. Matching entries (by top-level
//                                `name`) are replaced; new ones are appended;
//                                existing fields not in src are preserved.
//
// The src file is a JSON array of field descriptors. Each entry's shape is
// documented in rules/template-field-schema.md. Minimum keys per field:
//
//   { "name": "title", "type": "text", "options": {} }
//
// Optional keys per field: "default", "helpText", "selectOptions",
// "fieldConfig" (when type is "object"), "locked".
//
// Standard workflow:
//   1.  node get-template-fields.mjs <template> --json > /tmp/<template>.json
//   2.  Edit /tmp/<template>.json by hand or with another script.
//   3.  node set-template-fields.mjs <template> --src /tmp/<template>.json   # dry-run
//   4.  node set-template-fields.mjs <template> --src /tmp/<template>.json --confirm
//
// Examples:
//   node set-template-fields.mjs hero-static --src ./hero-fields.json
//   node set-template-fields.mjs hero-static --src ./hero-fields.json --mode merge --confirm
//
// IMPORTANT (per rules/safety-and-conventions.md):
//   - Backups go to ./cms-backups/templateVersion/<template_id>/<stamp>.json
//   - This is local-only. Production replication = Tophat editor.
//   - The dev-server in-process cache may need a restart to pick up the change.
//
// See: rules/template-field-schema.md (allowed types + options).

import { readFileSync, mkdirSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import {
  parseArgs,
  applyMongoFlags,
  findTemplateById,
  findTemplateByMixinKey,
  findTemplateVersion,
  mongoJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options, flags } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'set-template-fields.mjs <template_id|mixin_key> --src <config.json> [--mode replace|merge] [--confirm]');

const arg = positional[0];
const srcPath = options['--src'];
if (!srcPath) {
  die('--src <config.json> is required');
}

const mode = options['--mode'] || 'replace';
if (mode !== 'replace' && mode !== 'merge') {
  die(`--mode must be 'replace' or 'merge' (got "${mode}")`);
}

let template = null;
if (/^\d+$/.test(arg)) {
  template = findTemplateById(Number(arg));
} else {
  template = findTemplateByMixinKey(arg);
}
if (!template) {
  die(`No template found for "${arg}"`);
}

const version = options['--version']
  ? Number(options['--version'])
  : template.published_version;

const tv = findTemplateVersion(template._id, version);
if (!tv) {
  die(`No templateVersion v${version} for template _id ${template._id}`);
}

const incoming = JSON.parse(readFileSync(srcPath, 'utf8'));
if (!Array.isArray(incoming)) {
  die(`--src file must contain a JSON array of field descriptors (got ${typeof incoming})`);
}

// Normalise: every field gets default keys so the Tophat editor renders it.
function normaliseField(f) {
  const out = {
    name: f.name,
    type: f.type,
    options: f.options || {},
    locked: f.locked === true,
    default: f.default ?? null,
    selectOptions: Array.isArray(f.selectOptions) ? f.selectOptions : [],
  };
  if (f.helpText) {
    out.helpText = f.helpText;
  }
  if (Array.isArray(f.fieldConfig)) {
    out.fieldConfig = f.fieldConfig.map(normaliseField);
  }
  return out;
}

function validateField(f, path = '') {
  const fp = path ? `${path}.${f?.name || '?'}` : f?.name || '?';
  if (!f || typeof f !== 'object') {
    throw new Error(`Field at "${fp}" is not an object`);
  }
  if (typeof f.name !== 'string' || !f.name) {
    throw new Error(`Field at "${fp}" missing string "name"`);
  }
  if (typeof f.type !== 'string' || !f.type) {
    throw new Error(`Field "${fp}" missing string "type"`);
  }
  if (Array.isArray(f.fieldConfig)) {
    f.fieldConfig.forEach((sub) => validateField(sub, fp));
  }
}

try {
  incoming.forEach((f) => validateField(f));
} catch (e) {
  die(`Validation failed: ${e.message}`);
}

const normalised = incoming.map(normaliseField);

let nextConfig;
if (mode === 'replace') {
  nextConfig = normalised;
} else {
  // merge: upsert by top-level name, preserve unmentioned existing fields.
  const byName = new Map();
  (tv.config || []).forEach((f) => byName.set(f.name, f));
  normalised.forEach((f) => byName.set(f.name, f));
  nextConfig = Array.from(byName.values());
}

const before = tv.config || [];
const beforeNames = before.map((f) => f.name);
const afterNames = nextConfig.map((f) => f.name);
const added = afterNames.filter((n) => !beforeNames.includes(n));
const removed = beforeNames.filter((n) => !afterNames.includes(n));
const kept = afterNames.filter((n) => beforeNames.includes(n));

console.log(`Template: ${template.mixin_key} (_id=${template._id})`);
console.log(`templateVersion v${version} _id=${tv._id}`);
console.log(`Mode: ${mode}`);
console.log(`Fields before: ${before.length} (${beforeNames.join(', ')})`);
console.log(`Fields after:  ${nextConfig.length} (${afterNames.join(', ')})`);
console.log(`  added:   ${added.length ? added.join(', ') : '(none)'}`);
console.log(`  removed: ${removed.length ? removed.join(', ') : '(none)'}`);
console.log(`  kept:    ${kept.length ? kept.join(', ') : '(none)'}`);

if (!flags.has('--confirm')) {
  console.log('');
  console.log('DRY RUN — pass --confirm to apply.');
  process.exit(0);
}

const backupDir =
  options['--backup-dir'] || join(process.cwd(), 'cms-backups', 'templateVersion', String(template._id));
const stamp = new Date().toISOString().replace(/[:.]/g, '-');
mkdirSync(backupDir, { recursive: true });
const backupFile = join(backupDir, `${stamp}-v${version}.json`);
writeFileSync(backupFile, JSON.stringify(tv, null, 2));
console.log(`backup → ${backupFile}`);

const upd = mongoJson(`
  const r = db.templateVersion.updateOne(
    {_id: ${tv._id}},
    {$set: {config: ${JSON.stringify(nextConfig)}, updated_at: new Date(), updated_by: "tophat-tools/set-template-fields"}}
  );
  return {matched: r.matchedCount, modified: r.modifiedCount};
`);

console.log(`templateVersion updated: matched=${upd.matched} modified=${upd.modified}`);
console.log('NOTE: dev-server in-process cache may need a restart for editor changes to surface.');
console.log('NOTE: Direct DB writes are NOT a shipping mechanism — replicate in Tophat for production.');
