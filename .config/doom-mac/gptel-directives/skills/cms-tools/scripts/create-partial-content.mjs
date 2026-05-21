#!/usr/bin/env node
// create-partial-content.mjs --src <spec.json> [--language en] [--target-platform desktop] [--confirm]
//
// Inserts a new partial *content* (and its initial contentVersion). The
// `content.mixin_key` is the runtime key callers pass to <cms-partial mixin-key="...">.
// The `contentVersion.templateKey` must reference an existing partial template's
// mixin_key (create that first with create-partial-template.mjs).
//
// Idempotent: if a content document with the same mixin_key already exists, the
// script reports the existing _id and exits 0 without mutating.
//
// SPEC FILE (--src) — required. Minimal shape:
//
//   {
//     "mixin_key": "partial-marketing-lp-offer-callout",
//     "name": "Marketing LP Offer Callout",
//     "templateKey": "partial-marketing-lp-offer-callout",
//     "templateData": {
//       "copy": "20% off your first appointment with code WELCOME20.",
//       "promoCode": "WELCOME20",
//       "promoName": "Welcome 20"
//     }
//   }
//
// Optional spec keys:
//   "content_type_id"   (default 5 — Partials. Verify against existing partials.)
//   "language"          (default "en")
//   "targetPlatform"    (default "desktop")
//   "variationKey"      (default "A" — must follow A/B/C convention; "default" breaks Tophat UI initPlatforms)
//   "variationName"     (default "default" — lowercase; matches 607/608 existing partials)
//   "folderId"          (default 2 — the "Website" folder; null leaves the partial unreachable in the content tree)
//   "experimentId"      (default null) — runtime experiment id, NOT doc _id.
//   "variationId"       (default null) — runtime variation id.
//   "weight"            (default null) — used only when experimentId is set.
//   "uri"               (default null) — partials don't route, so leave null.
//   "title"             (default null) — generally unused for partials.
//
// CLI flag overrides:
//   --language, --target-platform     override the spec values.
//   --no-template-check               skip the "templateKey must exist" check.
//                                     Use only when you intend to create the
//                                     template later (unusual; reverses the
//                                     recommended order).
//   --container, --db                 standard mongo target flags.
//   --confirm                         apply the insert; without it, dry-run.
//
// EXAMPLES:
//   node create-partial-content.mjs --src ./offer-callout-content.json
//   node create-partial-content.mjs --src ./offer-callout-content.json --confirm
//
// SAFETY (per rules/safety-and-conventions.md):
//   - Dry-run by default.
//   - On --confirm, backs up the allocation record.
//   - Refuses by default if templateKey doesn't resolve to an existing template
//     (because the partial would 404 at runtime). Override with --no-template-check.
//   - Direct DB writes are LOCAL-ONLY. Production replication = Tophat editor.
//
// See: rules/partials.md (full partial mechanism reference)
//      rules/cms-data-model.md (content / contentVersion schemas)

import { readFileSync, mkdirSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import {
  parseArgs,
  applyMongoFlags,
  findContentByMixinKey,
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
const templateKey = spec.templateKey;
if (typeof templateKey !== 'string' || !templateKey) {
  die('spec.templateKey (string) is required — must match an existing partial template\'s mixin_key');
}
const name = spec.name || mixinKey;
const language = options['--language'] || spec.language || 'en';
const targetPlatform = options['--target-platform'] || spec.targetPlatform || 'desktop';
const variationKey = spec.variationKey || 'A';
const variationName = spec.variationName || 'default';
const folderId = spec.folderId ?? 2;
const experimentId = spec.experimentId ?? null;
const variationId = spec.variationId ?? null;
const weight = spec.weight ?? null;
const uri = spec.uri ?? null;
const title = spec.title ?? null;
const contentTypeId = spec.content_type_id ?? 5;
const templateData = spec.templateData && typeof spec.templateData === 'object' ? spec.templateData : {};

// Idempotency check on content mixin_key.
const existingContent = findContentByMixinKey(mixinKey);
if (existingContent) {
  console.log(`Content already exists for mixin_key="${mixinKey}":`);
  console.log(`  _id=${existingContent._id} name=${JSON.stringify(existingContent.name)}`);
  console.log(`  edit_version=${existingContent.edit_version} published_version=${existingContent.published_version}`);
  console.log('');
  console.log('No insert performed (idempotent). To modify templateData, update');
  console.log('the contentVersion record directly (consider adding update-partial-content.mjs).');
  process.exit(0);
}

// Template-existence check (so the partial doesn't 404 at runtime).
if (!flags.has('--no-template-check')) {
  const linkedTemplate = findTemplateByMixinKey(templateKey);
  if (!linkedTemplate) {
    die(
      `templateKey "${templateKey}" does not resolve to any existing template. ` +
        `Run create-partial-template.mjs first (or pass --no-template-check to skip this guard).`
    );
  }
  console.log(`Template check: templateKey="${templateKey}" → _id=${linkedTemplate._id} (${linkedTemplate.type})`);
}

console.log('Will INSERT:');
console.log(`  content.mixin_key            = "${mixinKey}"`);
console.log(`  content.name                 = "${name}"`);
console.log(`  content.content_type_id      = ${contentTypeId}`);
console.log(`  contentVersion.templateKey   = "${templateKey}"`);
console.log(`  contentVersion.variationKey  = "${variationKey}"`);
console.log(`  contentVersion.variationName = "${variationName}"`);
console.log(`  content.folder_id            = ${folderId}`);
console.log(`  contentVersion.experimentId  = ${experimentId === null ? 'null' : experimentId}`);
console.log(`  contentVersion.variationId   = ${variationId === null ? 'null' : variationId}`);
console.log(`  contentVersion.language      = "${language}"`);
console.log(`  contentVersion.targetPlatform= "${targetPlatform}"`);
console.log(`  contentVersion.templateData  = ${JSON.stringify(templateData).slice(0, 120)}${JSON.stringify(templateData).length > 120 ? '…' : ''}`);

if (!flags.has('--confirm')) {
  console.log('');
  console.log('DRY RUN — pass --confirm to apply.');
  process.exit(0);
}

// Allocate _ids via counters.
const allocated = mongoJson(`
  const c  = db.counters.findOneAndUpdate({_id: "content"},        {$inc: {seq: 1}}, {returnDocument: "after"});
  const cv = db.counters.findOneAndUpdate({_id: "contentVersion"}, {$inc: {seq: 1}}, {returnDocument: "after"});
  return { contentId: c.seq, contentVersionId: cv.seq };
`);
if (!allocated || !allocated.contentId || !allocated.contentVersionId) {
  die(`counter allocation failed: ${JSON.stringify(allocated)}`);
}

const backupDir = options['--backup-dir'] || join(process.cwd(), 'cms-backups', 'content', mixinKey);
const stamp = new Date().toISOString().replace(/[:.]/g, '-');
mkdirSync(backupDir, { recursive: true });
const backupFile = join(backupDir, `${stamp}-create.json`);
writeFileSync(
  backupFile,
  JSON.stringify(
    {
      operation: 'create-partial-content',
      timestamp: stamp,
      spec,
      allocated,
      note: 'Pre-insert allocation record. To rollback, db.content.deleteOne({_id: contentId}), db.contentVersion.deleteOne({_id: contentVersionId}), and decrement the counters.',
    },
    null,
    2
  )
);
console.log(`backup → ${backupFile}`);

const auditUser = options['--audit-user'] || 'tophat-tools/create-partial-content';

const result = mongoJson(`
  const now = new Date();
  const contentDoc = {
    _id: ${allocated.contentId},
    content_type_id: ${contentTypeId},
    name: ${JSON.stringify(name)},
    uri: ${uri === null ? 'null' : JSON.stringify(uri)},
    mixin_key: ${JSON.stringify(mixinKey)},
    takesUrlParameters: false,
    doNotAddToSitemap: true,
    urlParameterList: [],
    edit_version: 1,
    staged_version: 1,
    published_version: 1,
    is_archived: false,
    issue_redirect: false,
    suppress_cache: false,
    redirect_uri: null,
    product_type_id: null,
    folder_id: ${folderId === null ? 'null' : folderId},
    imageRefs: [],
    siteSearchKeywordBoost: [],
    created_at: now,
    updated_at: now,
    created_by: ${JSON.stringify(auditUser)},
    updated_by: ${JSON.stringify(auditUser)}
  };
  const cvDoc = {
    _id: ${allocated.contentVersionId},
    content_id: ${allocated.contentId},
    version: 1,
    language: ${JSON.stringify(language)},
    targetPlatform: ${JSON.stringify(targetPlatform)},
    variationKey: ${JSON.stringify(variationKey)},
    variationName: ${JSON.stringify(variationName)},
    audienceKey: null,
    audienceName: null,
    noRobots: false,
    hideSitewideBanner: false,
    hideStickyPromoDrawer: false,
    requireSoftLogin: false,
    simpleNav: false,
    simpleFooter: false,
    depthTracking: false,
    hideFreeShippingBanner: false,
    hideChatWidget: false,
    advisorRequired: false,
    isVueSsr: false,
    title: ${title === null ? 'null' : JSON.stringify(title)},
    parameterMetadata: {},
    description: null,
    stagingMessage: null,
    shortDescription: null,
    metaTitle: null,
    metaDescription: null,
    metaKeywords: null,
    topics: [],
    dyData: {},
    renderOptions: { additionalScripts: [], cdAttributesToInject: [] },
    cacheOptions: { queryParams: { whitelist: [] } },
    experimentId: ${experimentId === null ? 'null' : experimentId},
    variationId: ${variationId === null ? 'null' : variationId},
    weight: ${weight === null ? 'null' : weight},
    templateKey: ${JSON.stringify(templateKey)},
    templateVersion: 1,
    contentTypeData: {},
    templateData: ${JSON.stringify(templateData)},
    templateRefs: [],
    created_at: now,
    updated_at: now,
    created_by: ${JSON.stringify(auditUser)},
    updated_by: ${JSON.stringify(auditUser)}
  };
  const c  = db.content.insertOne(contentDoc);
  const cv = db.contentVersion.insertOne(cvDoc);
  return { contentInsertedId: c.insertedId, contentVersionInsertedId: cv.insertedId };
`);

console.log('');
console.log(`content inserted          _id=${result.contentInsertedId}`);
console.log(`contentVersion inserted   _id=${result.contentVersionInsertedId}`);
console.log('');
console.log('NOTE: dev-server in-process cache may need a restart for editor changes to surface.');
console.log('NOTE: runtime partial pipeline (cms-partial) picks up new content immediately on next request.');
console.log('NOTE: Direct DB writes are NOT a shipping mechanism — replicate in Tophat for production.');
console.log('');
console.log(`Verify with: node ${process.argv[1].replace(/create-partial-content/, 'inspect-partial')} ${mixinKey}`);
console.log(`REST round-trip: curl -s 'http://localhost:3000/api/cmsSvc/getPartial?mixinKey=${encodeURIComponent(mixinKey)}' | jq '.data | {html, css}'`);
