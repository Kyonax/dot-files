#!/usr/bin/env node
// backup-content.mjs <content_id> [--out <dir>]
//
// Writes a complete JSON snapshot of one content_id's footprint to disk:
//   - The content document
//   - Every contentVersion record (all versions, all variations)
//   - The production_content + stage_content denormalized copies
//   - Every experiment referenced by any of the contentVersion records
//   - The relevant counters (so a restore can preserve _id allocation)
//
// Snapshots go under <out>/<content_id>/<ISO-stamp>/snapshot.json by default.
// `restore-content.mjs` reads the snapshot back.
//
// Examples:
//   node backup-content.mjs 3117
//   node backup-content.mjs 3117 --out /tmp/cms-backups
//
// See: rules/safety-and-conventions.md.

import { mkdirSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import {
  parseArgs,
  applyMongoFlags,
  mongoJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'backup-content.mjs <content_id> [--out <dir>]');

const contentId = Number(positional[0]);
const outRoot = options['--out'] || join(process.cwd(), 'cms-backups');

const content = mongoJson(`return db.content.findOne({_id: ${contentId}});`);
if (!content) {
  die(`content_id ${contentId} not found`);
}

const versions = mongoJson(`return db.contentVersion.find({content_id: ${contentId}}).toArray();`);
const expIds = Array.from(
  new Set((versions || []).map((v) => v.experimentId).filter((x) => x != null))
);

const experiments = expIds.length
  ? mongoJson(`return db.experiment.find({experimentId: {$in: ${JSON.stringify(expIds)}}}).toArray();`)
  : [];

const productionContent = mongoJson(
  `return db.production_content.findOne({_id: ${contentId}});`
);
const stageContent = mongoJson(`return db.stage_content.findOne({_id: ${contentId}});`);

const counters = mongoJson(`
  return {
    contentVersion: db.counters.findOne({_id: "contentVersion"}),
    template: db.counters.findOne({_id: "template"}),
    templateVersion: db.counters.findOne({_id: "templateVersion"})
  };
`);

const stamp = new Date().toISOString().replace(/[:.]/g, '-');
const dir = join(outRoot, String(contentId), stamp);
mkdirSync(dir, { recursive: true });

const snapshot = {
  metadata: {
    contentId,
    timestamp: stamp,
    schemaVersion: 1,
  },
  content,
  contentVersions: versions,
  productionContent,
  stageContent,
  experiments,
  counters,
};

const file = join(dir, 'snapshot.json');
writeFileSync(file, JSON.stringify(snapshot, null, 2));
console.log(`backup written → ${file}`);
console.log(`  contentVersions: ${(versions || []).length}`);
console.log(`  experiments referenced: ${expIds.length}`);
