#!/usr/bin/env node
// set-variant-weight.mjs <experiment_doc_id|name> <variation_name> <weight> [--confirm]
//
// Updates the `weight` field of one variation in an experiment doc AND on
// every contentVersion bound to that experiment+variation. Both copies must
// match — the CMS reads weights from `contentVersion`, but Tophat displays
// them from `experiment.variations`.
//
// `weight` is an integer 0-10000 (parts per 10000 — i.e., 5000 = 50%).
//
// Examples:
//   node set-variant-weight.mjs 504 default 5000        # dry-run
//   node set-variant-weight.mjs 504 default 5000 --confirm
//   node set-variant-weight.mjs LocationSpecificSiteRevolution b 5000 --confirm
//
// See: rules/experiment-management.md.

import { mkdirSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import {
  parseArgs,
  applyMongoFlags,
  findExperiment,
  findExperimentByName,
  mongoJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options, flags } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 3, 'set-variant-weight.mjs <experiment_doc_id|name> <variation_name> <weight> [--confirm]');

const expArg = positional[0];
const variationName = positional[1];
const weight = Number(positional[2]);
if (!Number.isInteger(weight) || weight < 0 || weight > 10000) {
  die(`weight must be an integer 0-10000 (got "${positional[2]}")`);
}

let exp = null;
if (/^\d+$/.test(expArg)) {
  exp = findExperiment(Number(expArg));
} else {
  exp = findExperimentByName(expArg);
}
if (!exp) {
  die(`No experiment found for "${expArg}"`);
}

const variation = (exp.variations || []).find((v) => v.name === variationName);
if (!variation) {
  die(
    `experiment "${exp.name}" has no variation named "${variationName}" (found: ${(exp.variations || [])
      .map((v) => v.name)
      .join(', ')})`
  );
}

console.log(`Experiment _id=${exp._id} name="${exp.name}"`);
console.log(`  variation "${variationName}": ${variation.weight} → ${weight}`);

if (variation.weight === weight) {
  console.log('No change — already at desired weight.');
  process.exit(0);
}

if (!flags.has('--confirm')) {
  console.log('DRY RUN — pass --confirm to apply.');
  console.log('Will update:');
  console.log(`  - experiment.variations[name="${variationName}"].weight`);
  console.log(`  - every contentVersion {experimentId: ${exp.experimentId}, variationId: ${variation.variationId}}.weight`);
  process.exit(0);
}

const backupDir = options['--backup-dir'] || join(process.cwd(), 'cms-backups', 'experiment', String(exp._id));
const stamp = new Date().toISOString().replace(/[:.]/g, '-');
mkdirSync(backupDir, { recursive: true });
const backupFile = join(backupDir, `${stamp}-weight.json`);
writeFileSync(backupFile, JSON.stringify(exp, null, 2));
console.log(`backup → ${backupFile}`);

mongoJson(`
  db.experiment.updateOne(
    {_id: ${exp._id}, "variations.name": ${JSON.stringify(variationName)}},
    {$set: {"variations.$.weight": ${weight}, updated_at: new Date()}}
  );
  return {ok: 1};
`);

const cvUpdate = mongoJson(`
  const r = db.contentVersion.updateMany(
    {experimentId: ${exp.experimentId}, variationId: ${variation.variationId}},
    {$set: {weight: ${weight}, updated_at: new Date()}}
  );
  return {matched: r.matchedCount, modified: r.modifiedCount};
`);

console.log(`contentVersion docs updated: ${cvUpdate.modified}/${cvUpdate.matched}`);

const after = mongoJson(`return db.experiment.findOne({_id: ${exp._id}}, {variations: 1});`);
console.log('AFTER (experiment.variations):', JSON.stringify(after.variations, null, 2));
