#!/usr/bin/env node
// add-jsonld-script.mjs <content_id> --variation <key> --src <file> [--type ld+json] [--header] [--force-interpolation] [--confirm]
//
// Appends a JSON-LD (or any inline) script descriptor to a contentVersion's
// renderOptions.additionalScripts[]. Reads the script body from --src so you
// can author multi-line Pug templates in a real editor.
//
// IMPORTANT (from JSON-LD session):
//   - Direct DB writes are NOT a shipping mechanism. Production fix path is
//     Tophat editor (or Phase C code residue when CMS isn't usable). Use this
//     script ONLY for local diagnosis — verify on dev, then re-do via Tophat.
//   - The dev server caches per-handler; you may need to restart Vite or
//     publish from Tophat for the change to surface in raw HTML.
//   - Per-variation: each (content_id, version, variationKey) is a separate
//     contentVersion doc. To make a script render across A/B/C you must add
//     it to ALL the variations.
//
// Required flags:
//   --variation <key>           Which variationKey to update (A, B, C, ...)
//   --src <path>                File whose contents become the script body
//
// Optional flags:
//   --type <mime>               Default: ld+json
//   --header                    Set inHeader: true (default false)
//   --force-interpolation       Set forceInterpolation: true (default false)
//   --is-url                    Set isUrl: true (script src instead of inline)
//   --add-body-load             Set addBodyLoadScript: true
//   --version <N>               Specific version (default: published_version)
//   --confirm                   Apply (default: dry-run)
//
// Examples:
//   node add-jsonld-script.mjs 2350 --variation A --src ./faq-pug.pug --header --force-interpolation
//   node add-jsonld-script.mjs 2350 --variation A --src ./faq-pug.pug --header --force-interpolation --confirm
//
// See: rules/json-ld-management.md.

import { readFileSync, mkdirSync, writeFileSync } from 'node:fs';
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
requirePositional(positional, 1, 'add-jsonld-script.mjs <content_id> --variation X --src file [...]');

const contentId = Number(positional[0]);
const variation = options['--variation'];
const srcFile = options['--src'];
if (!variation) {
  die('--variation is required');
}
if (!srcFile) {
  die('--src is required');
}

const body = readFileSync(srcFile, 'utf8');
const type = options['--type'] || 'ld+json';
const inHeader = flags.has('--header');
const forceInterpolation = flags.has('--force-interpolation');
const isUrl = flags.has('--is-url');
const addBodyLoadScript = flags.has('--add-body-load');

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

const existing = cv.renderOptions?.additionalScripts || [];
const newScript = {
  type,
  inHeader,
  forceInterpolation,
  isUrl,
  addBodyLoadScript,
  body,
  generatedAutomatically: false,
  source: 'tophat-tools/add-jsonld-script.mjs',
};

console.log(`content_id=${contentId} v${targetVersion} variation=${variation}`);
console.log(`  existing scripts: ${existing.length}`);
console.log(`  appending: type=${type} header=${inHeader} forceInterp=${forceInterpolation} isUrl=${isUrl}`);
console.log(`  body length: ${body.length} chars`);

if (!flags.has('--confirm')) {
  console.log('DRY RUN — pass --confirm to apply.');
  process.exit(0);
}

const backupDir = options['--backup-dir'] || join(process.cwd(), 'cms-backups', 'jsonld', String(contentId));
const stamp = new Date().toISOString().replace(/[:.]/g, '-');
mkdirSync(backupDir, { recursive: true });
const backupFile = join(backupDir, `${stamp}-${variation}.json`);
writeFileSync(backupFile, JSON.stringify(cv, null, 2));
console.log(`backup → ${backupFile}`);

mongoJson(`
  db.contentVersion.updateOne(
    {_id: ${cv._id}},
    {$push: {"renderOptions.additionalScripts": ${JSON.stringify(newScript)}}, $set: {updated_at: new Date()}}
  );
  return {ok: 1};
`);

const after = mongoJson(`
  return (db.contentVersion.findOne({_id: ${cv._id}}, {renderOptions: 1}).renderOptions.additionalScripts || []).length;
`);
console.log(`AFTER: additionalScripts.length = ${after}`);
console.log('NOTE: dev server in-process content cache may not reflect this until restart or Tophat publish.');
console.log('NOTE: per-variation only — re-run for B/C/D if the script must render across all A/B/C variations.');
