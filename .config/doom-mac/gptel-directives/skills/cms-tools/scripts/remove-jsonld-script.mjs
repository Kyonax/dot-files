#!/usr/bin/env node
// remove-jsonld-script.mjs <content_id> --variation <key> [--auto-gen | --index N | --where field=value] [--version N] [--confirm]
//
// Removes one or more entries from a contentVersion's
// `renderOptions.additionalScripts[]`. Mirrors the removal to
// `production_content[contentId].variations.<platform>[].renderOptions.additionalScripts[]`
// AND `stage_content[contentId].variations.<platform>[]...` so the dev server's
// denormalized read path stays consistent.
//
// Use cases:
//   - Clean up an R1 auto-gen FAQPage entry after migrating to template-level
//     Pug (the duplicate-FAQPage scenario from DOTCOMPB-7929).
//   - Remove a stale script that survived a content migration.
//   - Surgical removal during JSON-LD experiments.
//
// IMPORTANT:
//   - This is a DB write. Not a shipping mechanism. Tophat's auto-gen pipeline
//     regenerates entries with `generatedAutomatically: true` on the next save
//     of the affected content+variation IF the upstream `addFaqMetadata` (or
//     similar) flag is still set. Disable the flag in Tophat editor for a
//     permanent fix, OR re-run this script after each save.
//   - Restart Vite or publish from Tophat for the change to land in raw HTML.
//
// Required:
//   --variation <key>     Which variationKey to update (A, B, C, ...)
//   And exactly ONE of:
//     --auto-gen          Remove every entry where generatedAutomatically === true
//     --index N           Remove entry at numeric index N (after dry-run preview)
//     --where field=value Remove entries where <field> exactly equals <value>
//                         (single primitive equality; dotted paths e.g. metadata.type=faq supported)
//
// Optional:
//   --version <N>         Specific contentVersion.version (default: published_version)
//   --no-mirror           Skip production_content / stage_content mirroring
//                         (use when you only want to update the canonical row;
//                          live render won't reflect until next publish)
//   --confirm             Apply (default: dry-run)
//
// Examples:
//   # See what would be removed (dry-run)
//   node remove-jsonld-script.mjs 2350 --variation A --auto-gen
//
//   # Apply
//   node remove-jsonld-script.mjs 2350 --variation A --auto-gen --confirm
//
//   # Remove by index after inspecting via get-cms-additional-scripts.mjs
//   node remove-jsonld-script.mjs 2350 --variation A --index 1 --confirm
//
//   # Remove every entry tagged with metadata.type = "faq"
//   node remove-jsonld-script.mjs 2350 --variation A --where metadata.type=faq --confirm
//
// See: rules/json-ld-management.md.

import { mkdirSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import {
  parseArgs,
  applyMongoFlags,
  findContentById,
  mongoJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options, flags } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'remove-jsonld-script.mjs <content_id> --variation X [--auto-gen | --index N | --where k=v] [...]');

const contentId = Number(positional[0]);
const variation = options['--variation'];
if (!variation) {
  die('--variation is required');
}

const indexArg = options['--index'];
const whereArg = options['--where'];
const useAutoGen = flags.has('--auto-gen');
const modes = [useAutoGen, indexArg !== undefined, whereArg !== undefined].filter(Boolean);
if (modes.length !== 1) {
  die('Exactly one of --auto-gen / --index N / --where field=value is required.');
}

const content = findContentById(contentId, { _id: 1, uri: 1, published_version: 1 });
if (!content) {
  die(`content_id ${contentId} not found`);
}
const targetVersion = options['--version']
  ? Number(options['--version'])
  : content.published_version;

const cv = mongoJson(`
  return db.contentVersion.findOne(
    {content_id: ${contentId}, version: ${targetVersion}, variationKey: ${JSON.stringify(variation)}},
    {_id: 1, renderOptions: 1}
  );
`);
if (!cv) {
  die(`No contentVersion at content_id=${contentId} version=${targetVersion} variationKey=${variation}`);
}

const scripts = cv.renderOptions?.additionalScripts || [];

// Resolve which entries match.
function getPath(obj, dotted) {
  return dotted.split('.').reduce((acc, k) => (acc == null ? acc : acc[k]), obj);
}
let matchPredicate;
let removalDescription;
if (useAutoGen) {
  matchPredicate = (s) => s && s.generatedAutomatically === true;
  removalDescription = 'generatedAutomatically === true';
} else if (indexArg !== undefined) {
  const idx = Number(indexArg);
  matchPredicate = (_s, i) => i === idx;
  removalDescription = `index === ${idx}`;
} else {
  const [field, ...rest] = whereArg.split('=');
  const value = rest.join('=');
  matchPredicate = (s) => String(getPath(s, field)) === value;
  removalDescription = `${field} === ${JSON.stringify(value)}`;
}

const matched = scripts
  .map((s, i) => ({ s, i }))
  .filter(({ s, i }) => matchPredicate(s, i));

console.log(`content_id=${contentId} uri=${content.uri} v${targetVersion} variation=${variation}`);
console.log(`  total additionalScripts: ${scripts.length}`);
console.log(`  matching (${removalDescription}): ${matched.length}`);
matched.forEach(({ s, i }) => {
  const text = (s.text || s.body || '').slice(0, 60).replace(/\n/g, ' ');
  console.log(`    [${i}] type=${s.type} autoGen=${s.generatedAutomatically === true} text=${JSON.stringify(text)}`);
});

if (matched.length === 0) {
  console.log('No matches — nothing to do.');
  process.exit(0);
}

if (!flags.has('--confirm')) {
  console.log('DRY RUN — pass --confirm to apply.');
  process.exit(0);
}

// Backup before write — full contentVersion AND the production_content/stage_content
// snapshots so we can recover the denormalized state too.
const backupDir = options['--backup-dir'] || join(process.cwd(), 'cms-backups', 'jsonld', String(contentId));
const stamp = new Date().toISOString().replace(/[:.]/g, '-');
mkdirSync(backupDir, { recursive: true });
const denormSnapshot = mongoJson(`
  return {
    production: db.production_content.findOne({_id: ${contentId}}, {_id:1, variations:1}),
    staged: db.stage_content.findOne({_id: ${contentId}}, {_id:1, variations:1})
  };
`);
const backupFile = join(backupDir, `${stamp}-${variation}-remove.json`);
writeFileSync(backupFile, JSON.stringify({ contentVersion: cv, denorm: denormSnapshot }, null, 2));
console.log(`backup → ${backupFile}`);

// Build the $pull filter expressing the SAME match logic. For dotted paths we
// use the standard Mongo dotted-key support inside $pull.
let pullFilter;
if (useAutoGen) {
  pullFilter = { generatedAutomatically: true };
} else if (indexArg !== undefined) {
  // Mongo can't $pull by index directly. Instead, $unset that slot to null,
  // then $pull nulls. Two ops in one update.
  pullFilter = { __unsupported__: true };
} else {
  const [field, ...rest] = whereArg.split('=');
  const value = rest.join('=');
  pullFilter = { [field]: value };
}

// 1) contentVersion update
if (indexArg !== undefined) {
  const idx = Number(indexArg);
  mongoJson(`
    db.contentVersion.updateOne(
      {_id: ${cv._id}},
      {$unset: {"renderOptions.additionalScripts.${idx}": 1}, $set: {updated_at: new Date()}}
    );
    db.contentVersion.updateOne(
      {_id: ${cv._id}},
      {$pull: {"renderOptions.additionalScripts": null}}
    );
    return {ok: 1};
  `);
} else {
  mongoJson(`
    db.contentVersion.updateOne(
      {_id: ${cv._id}},
      {$pull: {"renderOptions.additionalScripts": ${JSON.stringify(pullFilter)}}, $set: {updated_at: new Date()}}
    );
    return {ok: 1};
  `);
}

// 2) Mirror to production_content + stage_content (unless --no-mirror)
const mirrors = flags.has('--no-mirror') ? [] : ['production_content', 'stage_content'];
mirrors.forEach((coll) => {
  if (indexArg !== undefined) {
    const idx = Number(indexArg);
    mongoJson(`
      db.${coll}.updateMany(
        {_id: ${contentId}, "variations.desktop.variationKey": ${JSON.stringify(variation)}},
        {$unset: {[\`variations.desktop.$.renderOptions.additionalScripts.${idx}\`]: 1}}
      );
      db.${coll}.updateMany(
        {_id: ${contentId}, "variations.desktop.variationKey": ${JSON.stringify(variation)}},
        {$pull: {"variations.desktop.$.renderOptions.additionalScripts": null}}
      );
      return {ok: 1};
    `);
  } else {
    mongoJson(`
      db.${coll}.updateMany(
        {_id: ${contentId}, "variations.desktop.variationKey": ${JSON.stringify(variation)}},
        {$pull: {"variations.desktop.$.renderOptions.additionalScripts": ${JSON.stringify(pullFilter)}}}
      );
      return {ok: 1};
    `);
  }
});

const after = mongoJson(`
  return (db.contentVersion.findOne({_id: ${cv._id}}, {renderOptions: 1}).renderOptions.additionalScripts || []).length;
`);
console.log(`AFTER: contentVersion.additionalScripts.length = ${after}`);
if (mirrors.length === 0) {
  console.log('NOTE: --no-mirror — production_content / stage_content were NOT updated. Live render still emits old entries until Tophat publish.');
} else {
  console.log(`NOTE: mirrored to ${mirrors.join(' + ')}. Dev server still caches per-handler — restart Vite or publish from Tophat to surface the change.`);
}
if (useAutoGen) {
  console.log('NOTE: --auto-gen removes entries with generatedAutomatically:true. Tophat may regenerate them on next save if upstream flags (e.g. addFaqMetadata) are still set. Disable those flags in Tophat editor for a permanent fix.');
}
