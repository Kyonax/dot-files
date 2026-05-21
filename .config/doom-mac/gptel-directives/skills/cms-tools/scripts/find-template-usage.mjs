#!/usr/bin/env node
// find-template-usage.mjs <mixin_key>
//
// Read-only. Finds every contentVersion whose `templateData.componentList`
// includes a component with the given `mixin_key`. Useful when you need to
// know "which CMS pages will be affected if I change this template?"
//
// Returns a list of { content_id, uri, version, variations: [keys...] }
// grouped by content. Live (published) bindings come first.
//
// Examples:
//   node find-template-usage.mjs location-specific-colorbar-v2
//   node find-template-usage.mjs hcb-landing-sticky
//
// See: rules/cms-data-model.md.

import {
  parseArgs,
  applyMongoFlags,
  mongoJson,
  printJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'find-template-usage.mjs <mixin_key>');

const mixinKey = positional[0];

// Find all contentVersion docs that embed this mixin_key in their componentList.
const versions = mongoJson(`
  return db.contentVersion.find(
    { 'templateData.componentList.mixin_key': ${JSON.stringify(mixinKey)} },
    { content_id: 1, version: 1, variationKey: 1, variationName: 1, _id: 0 }
  ).toArray();
`);

if (!versions || versions.length === 0) {
  printJson({ mixinKey, totalBindings: 0, byContent: {} });
  process.exit(0);
}

// Group by content_id.
const byContentId = {};
versions.forEach((v) => {
  if (!byContentId[v.content_id]) {
    byContentId[v.content_id] = { versions: {} };
  }
  if (!byContentId[v.content_id].versions[v.version]) {
    byContentId[v.content_id].versions[v.version] = [];
  }
  byContentId[v.content_id].versions[v.version].push(v.variationKey);
});

// Annotate with URI + published_version so we can tell which usages are live.
const ids = Object.keys(byContentId).map(Number);
const contents = mongoJson(`
  return db.content.find(
    { _id: { $in: ${JSON.stringify(ids)} } },
    { _id: 1, uri: 1, name: 1, published_version: 1 }
  ).toArray();
`);

const out = {};
contents.forEach((c) => {
  const entry = byContentId[c._id];
  const liveVersions = Object.keys(entry.versions).map(Number).sort((a, b) => b - a);
  out[c._id] = {
    contentId: c._id,
    name: c.name,
    uri: c.uri,
    publishedVersion: c.published_version,
    isLive: !!entry.versions[c.published_version],
    liveVariations: entry.versions[c.published_version] || [],
    allVersions: liveVersions.map((ver) => ({
      version: ver,
      variations: entry.versions[ver],
      isCurrent: ver === c.published_version,
    })),
  };
});

const sorted = Object.values(out).sort((a, b) => Number(b.isLive) - Number(a.isLive));

printJson({
  mixinKey,
  totalBindings: versions.length,
  totalContent: sorted.length,
  bindings: sorted,
});
