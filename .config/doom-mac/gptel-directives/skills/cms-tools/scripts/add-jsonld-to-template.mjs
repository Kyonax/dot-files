#!/usr/bin/env node
// add-jsonld-to-template.mjs <template_id|mixin_key> --src <file> [--type ld+json] [--header] [--force-interpolation] [--version N] [--confirm]
//
// Appends a JSON-LD (or any inline) script descriptor to a templateVersion's
// `additionalScripts[]`. Sibling to `add-jsonld-script.mjs` (which targets
// `contentVersion.renderOptions.additionalScripts[]`).
//
// Why template-level: when a CMS partial / component template renders, the
// scriptsUtils pipeline (mr_modules/cms/lib/scriptsUtils.js + htmlRenderer.js)
// processes BOTH the content's renderOptions.additionalScripts AND the template's
// additionalScripts. Configuring JSON-LD ONCE on a reusable template (e.g.
// `faqs-with-icons-pro`, template_id 1375) means every content doc that mounts
// that template gets the JSON-LD with no per-content edits.
//
// Field path nuance:
//   - Content scripts:  contentVersion.renderOptions.additionalScripts[]
//   - Template scripts: templateVersion.additionalScripts[]   (NOT under renderOptions)
//
// IMPORTANT (mirrors add-jsonld-script.mjs):
//   - Direct DB writes are NOT a shipping mechanism. Production fix path is
//     Tophat editor. Use this for local diagnosis — verify on dev (raw HTML),
//     then re-do via Tophat editor.
//   - Restart Vite or publish from Tophat for the change to surface in raw HTML.
//   - One templateVersion = many bindings. ALL content docs that bind this
//     template at this version will pick up the script.
//
// Required flags:
//   --src <path>                File whose contents become the script body
//
// Optional flags:
//   --type <mime>               Default: ld+json
//   --header                    Set inHeader: true (default false)
//   --force-interpolation       Set forceInterpolation: true (default false)
//   --is-url                    Set isUrl: true (script src instead of inline)
//   --add-body-load             Set addBodyLoadScript: true
//   --version <N>               Target templateVersion.version (default: template.published_version)
//   --confirm                   Apply (default: dry-run)
//
// Examples:
//   node add-jsonld-to-template.mjs 1375 --src ./faqpage-partial.pug --header --force-interpolation
//   node add-jsonld-to-template.mjs faqs-with-icons-pro --src ./faqpage-partial.pug --header --force-interpolation --confirm
//
// See: rules/json-ld-management.md.

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
requirePositional(positional, 1, 'add-jsonld-to-template.mjs <template_id|mixin_key> --src file [...]');

const arg = positional[0];
const srcFile = options['--src'];
if (!srcFile) {
  die('--src is required');
}

const body = readFileSync(srcFile, 'utf8');
const type = options['--type'] || 'ld+json';
const inHeader = flags.has('--header');
const forceInterpolation = flags.has('--force-interpolation');
const isUrl = flags.has('--is-url');
const addBodyLoadScript = flags.has('--add-body-load');

let template;
if (/^\d+$/.test(arg)) {
  template = findTemplateById(Number(arg));
} else {
  template = findTemplateByMixinKey(arg);
}
if (!template) {
  die(`No template found for "${arg}" (tried _id and mixin_key)`);
}

const targetVersion = options['--version']
  ? Number(options['--version'])
  : template.published_version;
if (!targetVersion) {
  die(`No published_version on template ${template._id} (${template.mixin_key}); pass --version explicitly`);
}

const tv = findTemplateVersion(template._id, targetVersion);
if (!tv) {
  die(`No templateVersion at template_id=${template._id} version=${targetVersion}`);
}

const existing = tv.additionalScripts || [];
// Field name matches the Tophat editor + scriptsUtils.js read path. See the
// patch on add-jsonld-script.mjs for the body/text bug history.
//
// `_editorOptions.mode` tells Tophat's embedded Ace editor which language to
// highlight + validate. Without it the editor defaults to JSON and flags Pug
// source with "Unexpected 'i'" on the leading `if`. Set Pug for interpolated
// scripts, JSON for static.
const newScript = {
  type,
  isUrl,
  inHeader,
  forceInterpolation,
  addBodyLoadScript,
  text: body,
  _editorOptions: { mode: forceInterpolation ? 'pug' : 'json', theme: 'monokai', maxLines: 300 },
  _editorHasErrors: false,
};

console.log(`template_id=${template._id} (${template.mixin_key}) — type=${template.type}`);
console.log(`templateVersion _id=${tv._id} v${targetVersion}`);
console.log(`  existing additionalScripts: ${existing.length}`);
console.log(`  appending: type=${type} header=${inHeader} forceInterp=${forceInterpolation} isUrl=${isUrl}`);
console.log(`  body length: ${body.length} chars`);

if (!flags.has('--confirm')) {
  console.log('DRY RUN — pass --confirm to apply.');
  process.exit(0);
}

const backupDir = options['--backup-dir'] || join(process.cwd(), 'cms-backups', 'jsonld', `template-${template._id}`);
const stamp = new Date().toISOString().replace(/[:.]/g, '-');
mkdirSync(backupDir, { recursive: true });
const backupFile = join(backupDir, `${stamp}-v${targetVersion}.json`);
writeFileSync(backupFile, JSON.stringify(tv, null, 2));
console.log(`backup → ${backupFile}`);

mongoJson(`
  db.templateVersion.updateOne(
    {_id: ${tv._id}},
    {$push: {additionalScripts: ${JSON.stringify(newScript)}}, $set: {updated_at: new Date()}}
  );
  return {ok: 1};
`);

const after = mongoJson(`
  return (db.templateVersion.findOne({_id: ${tv._id}}, {additionalScripts: 1}).additionalScripts || []).length;
`);
console.log(`AFTER: templateVersion.additionalScripts.length = ${after}`);
console.log('NOTE: dev server caches templates per request handler — restart Vite or publish from Tophat for the change to surface in raw HTML.');
console.log(`NOTE: every content doc bound to template ${template._id} v${targetVersion} will now emit this script.`);
