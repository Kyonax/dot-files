#!/usr/bin/env node
// inspect-experiment.mjs <experiment_doc_id|name>
//
// Read-only. Prints an experiment doc + its variations (name, weight,
// experimentId, variationId, locked flag) and lists every contentVersion
// currently bound to it.
//
// Two ways to identify an experiment:
//   - Numeric `_id` (small int — e.g. 504)
//   - Name string (e.g. LocationSpecificSiteRevolution)
//
// Note: there are TWO IDs to keep straight:
//   - `experiment._id` is the doc ID (small integer, e.g. 504).
//   - `experiment.experimentId` is the runtime ID (large int, e.g.
//     177809681959624). This is the one that appears in cookies, in the
//     `?xid=` URL override, and in `contentVersion.experimentId`.
//
// Examples:
//   node inspect-experiment.mjs 504
//   node inspect-experiment.mjs LocationSpecificSiteRevolution
//
// See: rules/experiment-management.md.

import {
  parseArgs,
  applyMongoFlags,
  findExperiment,
  findExperimentByName,
  mongoJson,
  printJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'inspect-experiment.mjs <experiment_doc_id|name>');

const arg = positional[0];
let exp = null;
if (/^\d+$/.test(arg)) {
  exp = findExperiment(Number(arg));
} else {
  exp = findExperimentByName(arg);
}

if (!exp) {
  die(`No experiment found for "${arg}" (tried _id and name)`);
}

// Find every contentVersion bound to this experiment's runtime experimentId.
const bindings = mongoJson(`
  return db.contentVersion.find({experimentId: ${exp.experimentId}}, {
    content_id: 1,
    version: 1,
    variationKey: 1,
    variationName: 1,
    variationId: 1,
    weight: 1,
    _id: 0
  }).toArray();
`);

// Group bindings by content_id for readability.
const byContent = {};
(bindings || []).forEach((b) => {
  const k = `${b.content_id}/v${b.version}`;
  if (!byContent[k]) {
    byContent[k] = [];
  }
  byContent[k].push({
    variationKey: b.variationKey,
    variationName: b.variationName,
    variationId: b.variationId,
    weight: b.weight,
  });
});

printJson({
  experiment: {
    _id: exp._id,
    name: exp.name,
    status: exp.status,
    experimentId: exp.experimentId,
    targetPlatform: exp.targetPlatform,
    audienceTargeting: exp.audienceTargeting,
    variations: exp.variations,
  },
  totalBindings: (bindings || []).length,
  contentBindings: byContent,
});
