#!/usr/bin/env node
// set-experiment-status.mjs <experiment_doc_id|name> <Running|Paused|Stopped> [--confirm]
//
// Updates `experiment.status` on a single experiment document. Dry-run by
// default — pass --confirm to apply. Always backs up the experiment doc first.
//
// Examples:
//   node set-experiment-status.mjs 504 Running
//   node set-experiment-status.mjs LocationSpecificSiteRevolution Paused --confirm
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
requirePositional(positional, 2, 'set-experiment-status.mjs <experiment_doc_id|name> <Running|Paused|Stopped> [--confirm]');

const arg = positional[0];
const newStatus = positional[1];
const VALID = ['Running', 'Paused', 'Stopped'];
if (!VALID.includes(newStatus)) {
  die(`status must be one of ${VALID.join(', ')} (got "${newStatus}")`);
}

let exp = null;
if (/^\d+$/.test(arg)) {
  exp = findExperiment(Number(arg));
} else {
  exp = findExperimentByName(arg);
}
if (!exp) {
  die(`No experiment found for "${arg}"`);
}

console.log(`Experiment _id=${exp._id} name="${exp.name}"`);
console.log(`  current status: ${exp.status}`);
console.log(`  desired status: ${newStatus}`);

if (exp.status === newStatus) {
  console.log('No change — already in desired state.');
  process.exit(0);
}

if (!flags.has('--confirm')) {
  console.log('DRY RUN — pass --confirm to apply.');
  process.exit(0);
}

const backupDir = options['--backup-dir'] || join(process.cwd(), 'cms-backups', 'experiment', String(exp._id));
const stamp = new Date().toISOString().replace(/[:.]/g, '-');
mkdirSync(backupDir, { recursive: true });
const backupFile = join(backupDir, `${stamp}.json`);
writeFileSync(backupFile, JSON.stringify(exp, null, 2));
console.log(`backup → ${backupFile}`);

const result = mongoJson(`
  db.experiment.updateOne(
    {_id: ${exp._id}},
    {$set: {status: ${JSON.stringify(newStatus)}, updated_at: new Date()}}
  );
  return db.experiment.findOne({_id: ${exp._id}}, {status: 1, name: 1, _id: 1});
`);
console.log('AFTER:', JSON.stringify(result));
