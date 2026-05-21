#!/usr/bin/env node
// inspect-partial.mjs <content_mixin_key> [--version N] [--json]
//
// Read-only. One-shot dump of a CMS partial's full footprint:
//   - The content document       (collection: content, lookup by mixin_key)
//   - The active contentVersion  (the live templateData + variation metadata)
//   - The paired template        (collection: template, lookup by contentVersion.templateKey)
//   - The active templateVersion (jade + config[] field schema + component_list)
//
// A partial requires all four to coexist; if any is missing, the partial 404s at
// runtime. This script collapses 3-4 separate inspect calls into one read.
//
// Examples:
//   node inspect-partial.mjs partial-urm-perks
//   node inspect-partial.mjs partial-hellospring-mar-25-rcd-banner
//   node inspect-partial.mjs partial-marketing-lp-offer-callout --json
//   node inspect-partial.mjs partial-urm-perks --version 2
//
// Output shape (default — human-readable):
//   {
//     content: { _id, mixin_key, name, edit_version, staged_version, published_version },
//     contentVersion: { _id, version, templateKey, variationKey, variationName, experimentId, variationId, templateData },
//     template: { _id, type, mixin_key, name, edit_version, staged_version, published_version },
//     templateVersion: { _id, version, isStatic, jade, config[], component_list, … },
//     diagnostics: { contentExists, contentVersionExists, templateExists, templateVersionExists, ready }
//   }
//
// Use --json to emit a single-line JSON blob suitable for jq piping.
//
// See: rules/partials.md  (full partial mechanism reference)
//      rules/cms-data-model.md  (collection schemas)

import {
  parseArgs,
  applyMongoFlags,
  findContentByMixinKey,
  findContentVersions,
  findTemplateByMixinKey,
  findTemplateVersion,
  printJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options, flags } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'inspect-partial.mjs <content_mixin_key> [--version N] [--json]');

const contentMixinKey = positional[0];
const asJson = flags.has('--json');

const content = findContentByMixinKey(contentMixinKey);
if (!content) {
  die(
    `No content found with mixin_key="${contentMixinKey}". Hint: the *content* mixin_key is the runtime key callers pass to <cms-partial mixin-key="...">. Check for a typo, or that the content has been created (and not just the template).`
  );
}

const versionsRequested = options['--version']
  ? Number(options['--version'])
  : content.published_version || content.edit_version || 1;

const contentVersions = findContentVersions(content._id, versionsRequested);
const contentVersion = (contentVersions || [])[0] || null;

let template = null;
let templateVersion = null;
if (contentVersion && contentVersion.templateKey) {
  template = findTemplateByMixinKey(contentVersion.templateKey);
  if (template) {
    const tvVersion = template.published_version || template.edit_version || 1;
    templateVersion = findTemplateVersion(template._id, tvVersion);
  }
}

const out = {
  content: {
    _id: content._id,
    mixin_key: content.mixin_key,
    name: content.name,
    edit_version: content.edit_version,
    staged_version: content.staged_version,
    published_version: content.published_version,
    is_archived: content.is_archived,
    content_type_id: content.content_type_id,
  },
  contentVersion: contentVersion
    ? {
        _id: contentVersion._id,
        version: contentVersion.version,
        templateKey: contentVersion.templateKey,
        templateVersion: contentVersion.templateVersion,
        variationKey: contentVersion.variationKey,
        variationName: contentVersion.variationName,
        language: contentVersion.language,
        targetPlatform: contentVersion.targetPlatform,
        experimentId: contentVersion.experimentId,
        variationId: contentVersion.variationId,
        weight: contentVersion.weight,
        templateData: contentVersion.templateData,
      }
    : null,
  template: template
    ? {
        _id: template._id,
        type: template.type,
        mixin_key: template.mixin_key,
        name: template.name,
        targetPlatform: template.targetPlatform,
        edit_version: template.edit_version,
        staged_version: template.staged_version,
        published_version: template.published_version,
        is_archived: template.is_archived,
      }
    : null,
  templateVersion: templateVersion
    ? {
        _id: templateVersion._id,
        template_id: templateVersion.template_id,
        version: templateVersion.version,
        isStatic: templateVersion.isStatic,
        jade: templateVersion.jade,
        config: templateVersion.config || [],
        component_list: templateVersion.component_list || [],
        additionalScripts: templateVersion.additionalScripts || [],
        include_list: templateVersion.include_list || [],
        partialConfig: templateVersion.partialConfig || [],
      }
    : null,
  diagnostics: {
    contentExists: !!content,
    contentVersionExists: !!contentVersion,
    templateKeyPresent: !!(contentVersion && contentVersion.templateKey),
    templateExists: !!template,
    templateVersionExists: !!templateVersion,
    ready: !!(content && contentVersion && template && templateVersion),
  },
};

if (asJson) {
  process.stdout.write(JSON.stringify(out));
  process.stdout.write('\n');
} else {
  printJson(out);
}

if (!out.diagnostics.ready) {
  console.error('');
  console.error('WARNING: partial is not fully wired. Missing piece(s):');
  if (!out.diagnostics.contentVersionExists) {
    console.error(`  - contentVersion v${versionsRequested} for content_id ${content._id}`);
  }
  if (!out.diagnostics.templateKeyPresent) {
    console.error('  - contentVersion.templateKey (no template binding)');
  }
  if (!out.diagnostics.templateExists && out.diagnostics.templateKeyPresent) {
    console.error(`  - template with mixin_key "${contentVersion.templateKey}"`);
  }
  if (!out.diagnostics.templateVersionExists && out.diagnostics.templateExists) {
    console.error(`  - templateVersion for template_id ${template._id}`);
  }
  process.exit(3);
}
