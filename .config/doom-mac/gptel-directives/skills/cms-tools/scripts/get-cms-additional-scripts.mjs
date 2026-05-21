#!/usr/bin/env node
// get-cms-additional-scripts.mjs <content_id> [--variation A] [--version N]
//
// Read-only. Returns the `renderOptions.additionalScripts[]` array stored on
// a contentVersion — i.e., the JSON-LD / inline scripts that Tophat will
// inject into the page. Use this together with inspect-jsonld.mjs to compare
// "what's stored in CMS" vs "what reaches the rendered HTML".
//
// Per JSON-LD session findings:
//   - additionalScripts also has denormalized copies in
//     production_content[contentId].variations.<platform>[].renderOptions.additionalScripts
//   - The dev server uses a per-handler in-memory cache; raw DB writes may
//     not show up until a Tophat publish event or a server restart.
//
// Examples:
//   node get-cms-additional-scripts.mjs 2349
//   node get-cms-additional-scripts.mjs 2350 --variation A
//
// See: rules/json-ld-management.md.

import {
  parseArgs,
  applyMongoFlags,
  findContentById,
  findContentVersions,
  mongoJson,
  printJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'get-cms-additional-scripts.mjs <content_id> [--variation X] [--version N]');

const contentId = Number(positional[0]);
const variation = options['--variation'] || null;
const explicitVersion = options['--version'] ? Number(options['--version']) : null;

const content = findContentById(contentId, { _id: 1, uri: 1, published_version: 1 });
if (!content) {
  die(`content_id ${contentId} not found`);
}

const targetVersion = explicitVersion ?? content.published_version;
const versions = findContentVersions(contentId, targetVersion, {
  variationKey: 1,
  variationName: 1,
  renderOptions: 1,
  _id: 0,
});

let cvFiltered = versions || [];
if (variation) {
  cvFiltered = cvFiltered.filter((v) => v.variationKey === variation);
}

const fromContentVersion = cvFiltered.map((v) => ({
  variationKey: v.variationKey,
  variationName: v.variationName,
  additionalScripts: v.renderOptions?.additionalScripts || [],
}));

// Also dump the denormalized copy from production_content / stage_content so
// you can spot drift.
const denorm = mongoJson(`
  const pc = db.production_content.findOne({_id: ${contentId}}, {variations:1});
  const sc = db.stage_content.findOne({_id: ${contentId}}, {variations:1});
  return {production: pc, staged: sc};
`);

function summariseDenorm(doc) {
  if (!doc || !doc.variations) {
    return null;
  }
  const out = {};
  Object.keys(doc.variations).forEach((platform) => {
    out[platform] = (doc.variations[platform] || []).map((v) => ({
      variationKey: v.variationKey,
      variationName: v.variationName,
      additionalScripts: v.renderOptions?.additionalScripts || [],
    }));
  });
  return out;
}

printJson({
  contentId,
  uri: content.uri,
  inspectedVersion: targetVersion,
  fromContentVersion,
  fromProductionContent: summariseDenorm(denorm.production),
  fromStageContent: summariseDenorm(denorm.staged),
});
