#!/usr/bin/env node
// restore-content.mjs <snapshot.json|snapshot-dir> [--confirm]
//
// Restores a content_id's footprint from a backup-content.mjs snapshot. By
// default this is a DRY RUN — pass --confirm to actually apply writes.
//
// Restored:
//   - db.content     (replaceOne with upsert)
//   - db.contentVersion  (deleteMany on contentId, then insertMany)
//   - db.production_content + db.stage_content (replaceOne with upsert if present)
//
// NOT restored automatically (because they may be shared / risky):
//   - experiments
//   - counters
//   - templates / templateVersions
//
// Examples:
//   node restore-content.mjs ./cms-backups/3117/2026-05-06T20-00-00.000Z/snapshot.json
//   node restore-content.mjs ./cms-backups/3117/2026-05-06T20-00-00.000Z --confirm
//
// See: rules/safety-and-conventions.md.

import { readFileSync, statSync } from 'node:fs';
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
requirePositional(positional, 1, 'restore-content.mjs <snapshot.json|snapshot-dir> [--confirm]');

let snapshotPath = positional[0];
try {
  if (statSync(snapshotPath).isDirectory()) {
    snapshotPath = join(snapshotPath, 'snapshot.json');
  }
} catch (e) {
  die(`Could not stat "${snapshotPath}": ${e.message}`);
}

const dump = JSON.parse(readFileSync(snapshotPath, 'utf8'));
const contentId = dump.metadata?.contentId ?? dump.content?._id;
if (!contentId) {
  die('Snapshot missing metadata.contentId / content._id');
}

const cvCount = (dump.contentVersions || []).length;

if (!flags.has('--confirm')) {
  console.log('DRY RUN — pass --confirm to apply.');
  console.log(`Would restore content_id ${contentId}:`);
  console.log(`  - replace db.content (1 doc)`);
  console.log(`  - replace ${cvCount} db.contentVersion docs`);
  console.log(`  - replace db.production_content (${dump.productionContent ? '1 doc' : 'none in snapshot'})`);
  console.log(`  - replace db.stage_content (${dump.stageContent ? '1 doc' : 'none in snapshot'})`);
  console.log('  experiments / counters / templates are NOT restored — restore manually if needed.');
  process.exit(0);
}

console.log('--- step 1: replace content ---');
mongoJson(`
  db.content.replaceOne({_id: ${contentId}}, ${JSON.stringify(dump.content)}, {upsert: true});
  return {ok: 1};
`);

console.log('--- step 2: rewrite contentVersion ---');
mongoJson(`
  db.contentVersion.deleteMany({content_id: ${contentId}});
  return {ok: 1};
`);
for (const cv of dump.contentVersions || []) {
  mongoJson(`db.contentVersion.insertOne(${JSON.stringify(cv)}); return {ok: 1};`);
}

if (dump.productionContent) {
  console.log('--- step 3: replace production_content ---');
  mongoJson(`
    db.production_content.replaceOne({_id: ${contentId}}, ${JSON.stringify(dump.productionContent)}, {upsert: true});
    return {ok: 1};
  `);
}
if (dump.stageContent) {
  console.log('--- step 4: replace stage_content ---');
  mongoJson(`
    db.stage_content.replaceOne({_id: ${contentId}}, ${JSON.stringify(dump.stageContent)}, {upsert: true});
    return {ok: 1};
  `);
}

console.log(`RESTORE COMPLETE for content_id ${contentId}`);
console.log('NOTE: dev-server in-process content cache may not pick this up until restart or Tophat publish.');
