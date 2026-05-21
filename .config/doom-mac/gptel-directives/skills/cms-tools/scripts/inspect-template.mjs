#!/usr/bin/env node
// inspect-template.mjs <template_id|mixin_key>
//
// Read-only. Looks up a template by either numeric _id or string mixin_key,
// then prints:
//   - The template document (metadata, version pointers).
//   - The active templateVersion (jade source, component_list, etc.).
//
// Use this to answer questions like:
//   - "What pug does this CMS sub-template emit?"
//   - "Which child components does this template embed?"
//   - "What was the templateVersion that's actually live right now?"
//
// Examples:
//   node inspect-template.mjs 1650
//   node inspect-template.mjs location-specific-colorbar-v2
//
// See: rules/cms-data-model.md (template/templateVersion schemas).

import {
  parseArgs,
  applyMongoFlags,
  findTemplateById,
  findTemplateByMixinKey,
  findTemplateVersion,
  printJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'inspect-template.mjs <template_id|mixin_key>');

const arg = positional[0];

let template = null;
if (/^\d+$/.test(arg)) {
  template = findTemplateById(Number(arg));
} else {
  template = findTemplateByMixinKey(arg);
}

if (!template) {
  die(`No template found for "${arg}" (tried _id and mixin_key)`);
}

const requestedVersion = options['--version']
  ? Number(options['--version'])
  : template.published_version;

const tv = findTemplateVersion(template._id, requestedVersion);

const tvSummary = tv
  ? {
      _id: tv._id,
      template_id: tv.template_id,
      version: tv.version,
      isStatic: tv.isStatic,
      jade: tv.jade,
      component_list: tv.component_list,
      additionalScripts: tv.additionalScripts,
      include_list: tv.include_list,
      partialConfig: tv.partialConfig,
    }
  : null;

printJson({
  template: {
    _id: template._id,
    type: template.type,
    mixin_key: template.mixin_key,
    name: template.name,
    targetPlatform: template.targetPlatform,
    edit_version: template.edit_version,
    staged_version: template.staged_version,
    published_version: template.published_version,
    is_archived: template.is_archived,
  },
  inspectedVersion: requestedVersion,
  templateVersion: tvSummary,
});
