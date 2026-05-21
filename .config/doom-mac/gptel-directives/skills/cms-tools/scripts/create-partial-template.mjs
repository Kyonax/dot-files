#!/usr/bin/env node
// create-partial-template.mjs --src <spec.json> [--type partial|component] [--target-platform desktop|mobile] [--confirm]
//
// Inserts a new partial *template* (the jade-owning template, NOT the content).
// Idempotent: if a template with the same mixin_key already exists, the script
// reports the existing _id and exits 0 without mutating.
//
// Counterpart to create-partial-content.mjs — both are usually run together to
// scaffold a new partial from zero. Run this first; the content's
// `contentVersion.templateKey` will point at the template's mixin_key.
//
// SPEC FILE (--src) — required. Minimal shape:
//
//   {
//     "mixin_key": "partial-marketing-lp-offer-callout",
//     "name": "Marketing LP Offer Callout",
//     "jade": "offer-callout(\n  :copy=`'${settings.copy}'`\n  :promo-code=`'${settings.promoCode}'`)",
//     "config": [
//       { "name": "copy", "type": "textarea", "options": { "required": true, "rows": 4 }, "helpText": "Offer body." },
//       { "name": "promoCode", "type": "text", "options": { "required": true } }
//     ]
//   }
//
// Optional spec keys:
//   "type"            (default "component") — see rules/partials.md §2.
//   "targetPlatform"  (default "desktop")
//   "isStatic"        (default true; isStatic=false reserved for Tophat-WYSIWYG flow)
//   "component_list"  (default [])
//   "additionalScripts" (default [])
//   "include_list"    (default [])
//   "partialConfig"   (default [])
//
// CLI flag overrides:
//   --type, --target-platform     override the spec values.
//   --container, --db             standard mongo target flags.
//   --confirm                     apply the insert; without it, dry-run.
//
// EXAMPLES:
//   node create-partial-template.mjs --src ./offer-callout-template.json
//   node create-partial-template.mjs --src ./offer-callout-template.json --confirm
//
// SAFETY (per rules/safety-and-conventions.md):
//   - Dry-run by default.
//   - On --confirm, backs up the next-allocated _ids and any pre-existing template
//     with the same mixin_key (idempotent re-run safety).
//   - Direct DB writes are LOCAL-ONLY. Production replication = Tophat editor.
//   - The dev-server in-process cache may need a restart for the editor to surface
//     the new template; the runtime partial pipeline picks it up immediately.
//
// See: rules/partials.md (full partial mechanism reference)
//      rules/template-field-schema.md (allowed config[] field types)

import { readFileSync, mkdirSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import {
  parseArgs,
  applyMongoFlags,
  findTemplateByMixinKey,
  mongoJson,
  die,
} from './lib/mongo.mjs';

const { options, flags } = parseArgs(process.argv);
applyMongoFlags(options);

const srcPath = options['--src'];
if (!srcPath) {
  die('--src <spec.json> is required. See header for spec shape.');
}

let spec;
try {
  spec = JSON.parse(readFileSync(srcPath, 'utf8'));
} catch (e) {
  die(`failed to read/parse --src "${srcPath}": ${e.message}`);
}

const mixinKey = spec.mixin_key;
if (typeof mixinKey !== 'string' || !mixinKey) {
  die('spec.mixin_key (string) is required');
}
const name = spec.name || mixinKey;
const jade = typeof spec.jade === 'string' ? spec.jade : '';
const type = options['--type'] || spec.type || 'component';
const targetPlatform = options['--target-platform'] || spec.targetPlatform || 'desktop';
const isStatic = spec.isStatic !== undefined ? !!spec.isStatic : true;
const componentList = Array.isArray(spec.component_list) ? spec.component_list : [];
const additionalScripts = Array.isArray(spec.additionalScripts) ? spec.additionalScripts : [];
const includeList = Array.isArray(spec.include_list) ? spec.include_list : [];
const partialConfig = Array.isArray(spec.partialConfig) ? spec.partialConfig : [];
const config = Array.isArray(spec.config) ? spec.config : [];

// Normalise config[] the same way set-template-fields does — so Tophat editor renders.
// CRITICAL: every field MUST carry `options.xsClass` (Bootstrap grid class). Without it
// the Tophat content-edit form renderer either gives the field zero width or skips it
// entirely — `link`-type fields are the most visibly broken case. Default to col-xs-12.
function normaliseField(f) {
  const options = { ...(f.options || {}) };
  if (!options.xsClass) {
    options.xsClass = 'col-xs-12';
  }
  const out = {
    name: f.name,
    type: f.type,
    options,
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
  config.forEach((f) => validateField(f));
} catch (e) {
  die(`config[] validation failed: ${e.message}`);
}
const normalisedConfig = config.map(normaliseField);

// Idempotency check.
const existing = findTemplateByMixinKey(mixinKey);
if (existing) {
  console.log(`Template already exists for mixin_key="${mixinKey}":`);
  console.log(`  _id=${existing._id} type=${existing.type} name=${JSON.stringify(existing.name)}`);
  console.log(`  edit_version=${existing.edit_version} published_version=${existing.published_version}`);
  console.log('');
  console.log('No insert performed (idempotent). To modify, use set-template-fields.mjs');
  console.log('or update the templateVersion record directly.');
  process.exit(0);
}

console.log('Will INSERT:');
console.log(`  template.mixin_key       = "${mixinKey}"`);
console.log(`  template.name            = "${name}"`);
console.log(`  template.type            = "${type}"`);
console.log(`  template.targetPlatform  = "${targetPlatform}"`);
console.log(`  templateVersion.version  = 1`);
console.log(`  templateVersion.jade     = ${JSON.stringify(jade).slice(0, 80)}${jade.length > 80 ? '…' : ''}`);
console.log(`  templateVersion.config[] = ${normalisedConfig.length} fields (${normalisedConfig.map((f) => f.name).join(', ') || '(none)'})`);
console.log(`  templateVersion.isStatic = ${isStatic}`);

if (!flags.has('--confirm')) {
  console.log('');
  console.log('DRY RUN — pass --confirm to apply.');
  process.exit(0);
}

// Allocate _ids via counters (atomic findOneAndUpdate $inc).
const allocated = mongoJson(`
  const t  = db.counters.findOneAndUpdate({_id: "template"},        {$inc: {seq: 1}}, {returnDocument: "after"});
  const tv = db.counters.findOneAndUpdate({_id: "templateVersion"}, {$inc: {seq: 1}}, {returnDocument: "after"});
  return { templateId: t.seq, templateVersionId: tv.seq };
`);
if (!allocated || !allocated.templateId || !allocated.templateVersionId) {
  die(`counter allocation failed: ${JSON.stringify(allocated)}`);
}

// Backup the allocation (lets a manual rollback re-decrement if needed).
const backupDir = options['--backup-dir'] || join(process.cwd(), 'cms-backups', 'template', mixinKey);
const stamp = new Date().toISOString().replace(/[:.]/g, '-');
mkdirSync(backupDir, { recursive: true });
const backupFile = join(backupDir, `${stamp}-create.json`);
writeFileSync(
  backupFile,
  JSON.stringify(
    {
      operation: 'create-partial-template',
      timestamp: stamp,
      spec,
      allocated,
      note: 'Pre-insert allocation record. To rollback, db.template.deleteOne({_id: templateId}), db.templateVersion.deleteOne({_id: templateVersionId}), and decrement the counters.',
    },
    null,
    2
  )
);
console.log(`backup → ${backupFile}`);

const auditUser = options['--audit-user'] || 'tophat-tools/create-partial-template';

const result = mongoJson(`
  const now = new Date();
  const templateDoc = {
    _id: ${allocated.templateId},
    type: ${JSON.stringify(type)},
    mixin_key: ${JSON.stringify(mixinKey)},
    name: ${JSON.stringify(name)},
    targetPlatform: ${JSON.stringify(targetPlatform)},
    image: null,
    imageRefs: [],
    edit_version: 1,
    staged_version: 1,
    published_version: 1,
    is_archived: false,
    created_at: now,
    updated_at: now,
    created_by: ${JSON.stringify(auditUser)},
    updated_by: ${JSON.stringify(auditUser)}
  };
  const tvDoc = {
    _id: ${allocated.templateVersionId},
    template_id: ${allocated.templateId},
    version: 1,
    isStatic: ${isStatic ? 'true' : 'false'},
    staticTemplatePath: null,
    getter: null,
    ngBodyCtl: null,
    loadControllerFile: false,
    jade: ${JSON.stringify(jade)},
    config: ${JSON.stringify(normalisedConfig)},
    component_list: ${JSON.stringify(componentList)},
    modal_list: [],
    styl: "",
    additionalScripts: ${JSON.stringify(additionalScripts)},
    include_list: ${JSON.stringify(includeList)},
    partialConfig: ${JSON.stringify(partialConfig)},
    partialData: {},
    previewData: {},
    created_at: now,
    updated_at: now,
    created_by: ${JSON.stringify(auditUser)},
    updated_by: ${JSON.stringify(auditUser)}
  };
  const t  = db.template.insertOne(templateDoc);
  const tv = db.templateVersion.insertOne(tvDoc);
  return { templateInsertedId: t.insertedId, templateVersionInsertedId: tv.insertedId };
`);

console.log('');
console.log(`template inserted          _id=${result.templateInsertedId}`);
console.log(`templateVersion inserted   _id=${result.templateVersionInsertedId}`);
console.log('');
console.log('NOTE: dev-server in-process cache may need a restart for editor changes to surface.');
console.log('NOTE: runtime partial pipeline (cms-partial) picks up new templates immediately on next request.');
console.log('NOTE: Direct DB writes are NOT a shipping mechanism — replicate in Tophat for production.');
console.log('');
console.log('Next step: create the paired partial content with create-partial-content.mjs');
console.log(`  contentVersion.templateKey must equal "${mixinKey}"`);
