#!/usr/bin/env node
// migrate-content-experiment.mjs <content_id> --from-config <file> [--confirm]
//
// Re-binds a content_id from one experiment to another, materialising a new
// contentVersion (next version number) per target variation. Generalised from
// the canonical .tasks/DOTCOMPB-8120/cms-migrate.mjs script.
//
// The --from-config file is a JSON descriptor that tells the script:
//   - Which content_id to migrate
//   - Which existing variationKey to use as the source for each new variation
//   - Per-variation overrides (componentList, settings, weights)
//   - The new experiment + variation IDs to bind to
//
// Schema (see rules/content-migration.md for the full spec):
//
//   {
//     "contentId": 3117,
//     "fromVersion": 54,
//     "toVersion": 55,
//     "newExperimentId": 177809681959624,
//     "newExperimentDocId": 504,
//     "variations": [
//       {
//         "key": "A",
//         "name": "default",
//         "weight": 0,
//         "newVariationId": 1778096819596240,
//         "sourceVariationKey": "A",
//         "componentList": "inherit"
//       },
//       {
//         "key": "B",
//         "name": "b",
//         "weight": 10000,
//         "newVariationId": 1778096819596241,
//         "sourceVariationKey": "A",
//         "componentList": [
//           { "mixin_key": "site-message-carousel", "from": "A" },
//           { "mixin_key": "location-specific-colorbar-v2",
//             "settings": { "title": "...", "subtitle": "...", "colorbarTitle": "..." },
//             "templateVersion": 1, "templateType": "component" },
//           { "mixin_key": "letter-from-amy", "from": "A" },
//           { "mixin_key": "party-confetti", "from": "A" },
//           { "mixin_key": "hcb-landing-sticky", "from": "A" }
//         ]
//       }
//     ],
//     "publish": true
//   }
//
// Steps performed (idempotent — re-running drops + re-creates the target version):
//   1. Validate the source variation exists at fromVersion.
//   2. Backup content + contentVersions to ./cms-backups/<contentId>/<stamp>/snapshot.json.
//   3. For each target variation: clone the source contentVersion, override
//      experiment/variation IDs + weight + componentList per config.
//   4. If publish=true, set content.{edit_version,staged_version,published_version}=toVersion.
//
// Examples:
//   node migrate-content-experiment.mjs 3117 --from-config ./mig-3117.json
//   node migrate-content-experiment.mjs 3117 --from-config ./mig-3117.json --confirm
//
// See: rules/content-migration.md and the canonical .tasks/DOTCOMPB-8120/cms-migrate.mjs.

import { readFileSync, mkdirSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import {
  parseArgs,
  applyMongoFlags,
  mongoJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options, flags } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'migrate-content-experiment.mjs <content_id> --from-config <file> [--confirm]');

const contentIdArg = Number(positional[0]);
const cfgPath = options['--from-config'];
if (!cfgPath) {
  die('--from-config <file> is required');
}
const cfg = JSON.parse(readFileSync(cfgPath, 'utf8'));
if (cfg.contentId !== contentIdArg) {
  die(`config contentId (${cfg.contentId}) does not match positional content_id (${contentIdArg})`);
}

const { contentId, fromVersion, toVersion, newExperimentId, newExperimentDocId, variations, publish } = cfg;

console.log(`Migrating content_id=${contentId} v${fromVersion} → v${toVersion}`);
console.log(`  → new experiment _id=${newExperimentDocId} experimentId=${newExperimentId}`);
console.log(`  → ${variations.length} target variations`);

if (!flags.has('--confirm')) {
  console.log('DRY RUN — pass --confirm to apply.');
  variations.forEach((v) => {
    console.log(`    [${v.key}] name=${v.name} weight=${v.weight} sourceVariationKey=${v.sourceVariationKey} componentList=${Array.isArray(v.componentList) ? v.componentList.length + ' blocks' : v.componentList}`);
  });
  process.exit(0);
}

// Step 1: validate source variation.
const validate = mongoJson(`
  return variations.map(v => db.contentVersion.findOne({content_id: ${contentId}, version: ${fromVersion}, variationKey: v.key}, {_id:1}));
`.replace('variations', JSON.stringify(variations.map((v) => ({ key: v.sourceVariationKey })))));
if (validate.some((r) => !r)) {
  die('At least one source variation does not exist at fromVersion. Aborting.');
}

// Step 2: backup.
const backupDir = options['--backup-dir'] || join(process.cwd(), 'cms-backups', String(contentId));
const stamp = new Date().toISOString().replace(/[:.]/g, '-');
const dir = join(backupDir, stamp);
mkdirSync(dir, { recursive: true });
const snapshot = {
  metadata: { contentId, timestamp: stamp, sourceConfig: cfg },
  content: mongoJson(`return db.content.findOne({_id: ${contentId}});`),
  contentVersions: mongoJson(`return db.contentVersion.find({content_id: ${contentId}}).toArray();`),
};
writeFileSync(join(dir, 'snapshot.json'), JSON.stringify(snapshot, null, 2));
console.log(`backup → ${join(dir, 'snapshot.json')}`);

// Step 3: drop any existing rows at toVersion (idempotency), then materialise targets.
mongoJson(`
  db.contentVersion.deleteMany({content_id: ${contentId}, version: ${toVersion}});
  return {ok: 1};
`);

for (const v of variations) {
  const source = mongoJson(`
    return db.contentVersion.findOne({content_id: ${contentId}, version: ${fromVersion}, variationKey: ${JSON.stringify(v.sourceVariationKey)}});
  `);
  const cloned = { ...source };
  delete cloned._id;

  const newId = mongoJson(`
    const c = db.counters.findAndModify({query: {_id: "contentVersion"}, update: {$inc: {seq: 1}}, new: true});
    return c.seq;
  `);
  cloned._id = newId;
  cloned.version = toVersion;
  cloned.variationKey = v.key;
  cloned.variationName = v.name;
  cloned.audienceKey = null;
  cloned.audienceName = null;
  cloned.experimentId = newExperimentId;
  cloned.variationId = v.newVariationId;
  cloned.weight = v.weight;
  cloned.created_at = new Date().toISOString();
  cloned.updated_at = new Date().toISOString();
  cloned.created_by = 'tophat-tools/migrate-content-experiment';
  cloned.updated_by = 'tophat-tools/migrate-content-experiment';

  if (Array.isArray(v.componentList)) {
    const sourceList = source.templateData?.componentList || [];
    const byKey = {};
    sourceList.forEach((c) => {
      byKey[c.mixin_key] = c;
    });
    const newList = v.componentList.map((entry) => {
      if (entry.from === 'A' || entry.from) {
        const sourceKey = entry.from === true ? entry.mixin_key : (entry.mixin_key || entry.from);
        const ref = byKey[sourceKey];
        if (!ref) {
          throw new Error(`source componentList missing mixin_key="${sourceKey}"`);
        }
        return JSON.parse(JSON.stringify(ref));
      }
      // Inline new entry — must specify settings + templateVersion + templateType
      return {
        mixin_key: entry.mixin_key,
        settings: entry.settings || {},
        templateVersion: entry.templateVersion ?? 1,
        templateType: entry.templateType || 'component',
      };
    });
    cloned.templateData = {
      ...(source.templateData || {}),
      componentList: newList,
    };
  }

  mongoJson(`db.contentVersion.insertOne(${JSON.stringify(cloned)}); return {ok: 1};`);
  console.log(`  inserted variation ${v.key} (newId=${newId})`);
}

if (publish) {
  mongoJson(`
    db.content.updateOne(
      {_id: ${contentId}},
      {$set: {edit_version: ${toVersion}, staged_version: ${toVersion}, published_version: ${toVersion}, updated_at: new Date(), updated_by: "tophat-tools/migrate-content-experiment"}}
    );
    return {ok: 1};
  `);
  console.log(`content.{edit,staged,published}_version → ${toVersion}`);
}

const after = mongoJson(`
  return {
    content: db.content.findOne({_id: ${contentId}}, {edit_version:1, staged_version:1, published_version:1}),
    versions: db.contentVersion.find({content_id: ${contentId}, version: ${toVersion}}, {variationKey:1, variationName:1, weight:1, experimentId:1, _id:0}).toArray()
  };
`);
console.log('AFTER:', JSON.stringify(after, null, 2));
console.log('MIGRATION COMPLETE.');
console.log('NOTE: dev-server in-process content cache may not reflect this until restart or Tophat publish.');
