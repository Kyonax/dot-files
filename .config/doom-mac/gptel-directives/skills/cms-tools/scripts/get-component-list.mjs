#!/usr/bin/env node
// get-component-list.mjs <content_id> [--variation A|B|...] [--version N]
//
// Read-only. Returns a flat ordered list of mixin_keys + their settings for
// one variation of one content version. Useful when you need the exact block
// list rendered on a page (e.g., to compare V1 vs V2 layouts on the same URL).
//
// If --variation is omitted, returns the list for every variation in the
// requested version, keyed by variationKey.
//
// Examples:
//   node get-component-list.mjs 3117 --variation B --version 55
//   node get-component-list.mjs 3117                 # all variations, latest version
//
// See: rules/cms-data-model.md (componentList shape).

import {
  parseArgs,
  applyMongoFlags,
  findContentById,
  findContentVersions,
  printJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'get-component-list.mjs <content_id> [--variation X] [--version N]');

const contentId = Number(positional[0]);
const variationKey = options['--variation'] || null;
const explicitVersion = options['--version'] ? Number(options['--version']) : null;

const content = findContentById(contentId, {
  _id: 1,
  uri: 1,
  published_version: 1,
});
if (!content) {
  die(`content_id ${contentId} not found`);
}
const targetVersion = explicitVersion ?? content.published_version;

const versions = findContentVersions(contentId, targetVersion, {
  variationKey: 1,
  variationName: 1,
  weight: 1,
  experimentId: 1,
  variationId: 1,
  templateData: 1,
  _id: 0,
});

if (!versions || versions.length === 0) {
  die(`No contentVersion records for content_id ${contentId} version ${targetVersion}`);
}

function summariseList(list) {
  return (list || []).map((c, i) => ({
    index: i,
    mixin_key: c.mixin_key,
    templateType: c.templateType,
    templateVersion: c.templateVersion,
    settings: c.settings,
  }));
}

if (variationKey) {
  const v = versions.find((x) => x.variationKey === variationKey);
  if (!v) {
    die(
      `No variation "${variationKey}" on content_id ${contentId} v${targetVersion} (found: ${versions
        .map((x) => x.variationKey)
        .join(', ')})`
    );
  }
  printJson({
    contentId,
    uri: content.uri,
    version: targetVersion,
    variation: variationKey,
    componentList: summariseList(v.templateData?.componentList),
  });
} else {
  const out = {};
  versions.forEach((v) => {
    out[v.variationKey] = {
      variationName: v.variationName,
      weight: v.weight,
      experimentId: v.experimentId,
      componentList: summariseList(v.templateData?.componentList),
    };
  });
  printJson({
    contentId,
    uri: content.uri,
    version: targetVersion,
    variations: out,
  });
}
