#!/usr/bin/env node
// inspect-content-by-uri.mjs <uri>
//
// Read-only. Resolves a URI -> content document, walking parent paths and
// honouring `takesUrlParameters`. Then delegates to inspect-content for
// the full variation breakdown.
//
// Examples:
//   node inspect-content-by-uri.mjs /colorbar/location-specific
//   node inspect-content-by-uri.mjs /colorbar/locations/hillsboro
//   node inspect-content-by-uri.mjs /shop/brown
//
// See: rules/cms-data-model.md (URI resolution rules).

import {
  parseArgs,
  applyMongoFlags,
  findContentByUri,
  findContentVersions,
  printJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'inspect-content-by-uri.mjs <uri> [--version <N>]');

const uri = positional[0];
const resolved = findContentByUri(uri);
if (!resolved) {
  die(`Could not resolve URI "${uri}" to a content document (tried exact + parent walk).`);
}

const targetVersion = options['--version']
  ? Number(options['--version'])
  : null;

const variations = findContentVersions(resolved._id, targetVersion, {
  variationKey: 1,
  variationName: 1,
  experimentId: 1,
  variationId: 1,
  weight: 1,
  audienceKey: 1,
  audienceName: 1,
  _id: 1,
  version: 1,
  'templateData.componentList.mixin_key': 1,
});

// Pick the latest version if not pinned.
const latestVersion = variations.length
  ? Math.max(...variations.map((v) => v.version))
  : null;

const filtered = variations.filter((v) => v.version === (targetVersion ?? latestVersion));

const summary = filtered.map((v) => ({
  _id: v._id,
  variationKey: v.variationKey,
  variationName: v.variationName,
  experimentId: v.experimentId,
  variationId: v.variationId,
  weight: v.weight,
  audienceKey: v.audienceKey,
  audienceName: v.audienceName,
  componentMixinKeys: (v.templateData?.componentList || []).map((c) => c.mixin_key),
}));

printJson({
  resolved,
  tophatLink: `http://localhost:4000/#/cms/content/edit/${resolved._id}`,
  inspectedVersion: targetVersion ?? latestVersion,
  variations: summary,
});
