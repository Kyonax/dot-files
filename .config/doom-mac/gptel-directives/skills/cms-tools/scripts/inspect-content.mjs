#!/usr/bin/env node
// inspect-content.mjs <content_id>
//
// Read-only. Prints the content doc + all contentVersion variations for the
// latest version (or a specific version via --version <N>). Use this as the
// first step when investigating any CMS-driven URL — the result tells you
// which experiment is bound, which variations exist, and which weights are
// active.
//
// Output: pretty-printed JSON with three keys: { content, currentVersion, variations }.
//
// See: rules/cms-data-model.md (collection schemas).

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
requirePositional(positional, 1, 'inspect-content.mjs <content_id> [--version <N>]');

const contentId = Number(positional[0]);
if (!Number.isInteger(contentId)) {
  die(`content_id must be an integer; got "${positional[0]}"`);
}

const content = findContentById(contentId, {
  _id: 1,
  name: 1,
  uri: 1,
  edit_version: 1,
  staged_version: 1,
  published_version: 1,
  takesUrlParameters: 1,
  template_id: 1,
});

if (!content) {
  die(`No content document found with _id=${contentId}`);
}

const targetVersion = options['--version']
  ? Number(options['--version'])
  : content.published_version;

const variations = findContentVersions(contentId, targetVersion, {
  variationKey: 1,
  variationName: 1,
  experimentId: 1,
  variationName: 1,
  variationId: 1,
  weight: 1,
  audienceKey: 1,
  audienceName: 1,
  _id: 1,
  version: 1,
  'templateData.componentList.mixin_key': 1,
  'templateData.componentList.templateVersion': 1,
});

const summary = (variations || []).map((v) => ({
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

printJson({ content, currentVersion: targetVersion, variations: summary });
