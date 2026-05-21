#!/usr/bin/env node
// get-template-jade.mjs <template_id|mixin_key> [--version N]
//
// Read-only. Prints ONLY the jade (Pug) source for a given template, on
// stdout, with no JSON wrapping. Useful when you want to pipe into a file
// or grep the template body.
//
// Examples:
//   node get-template-jade.mjs location-specific-colorbar-v2
//   node get-template-jade.mjs 1650 --version 1
//
// For full metadata, use inspect-template.mjs instead.

import {
  parseArgs,
  applyMongoFlags,
  findTemplateById,
  findTemplateByMixinKey,
  findTemplateVersion,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'get-template-jade.mjs <template_id|mixin_key> [--version N]');

const arg = positional[0];

let template = null;
if (/^\d+$/.test(arg)) {
  template = findTemplateById(Number(arg));
} else {
  template = findTemplateByMixinKey(arg);
}

if (!template) {
  die(`No template found for "${arg}"`);
}

const version = options['--version']
  ? Number(options['--version'])
  : template.published_version;

const tv = findTemplateVersion(template._id, version);
if (!tv) {
  die(`No templateVersion v${version} for template _id ${template._id}`);
}

if (!tv.jade) {
  die(`templateVersion v${version} for template _id ${template._id} has no jade source (isStatic=${tv.isStatic})`);
}

process.stdout.write(tv.jade);
if (!tv.jade.endsWith('\n')) {
  process.stdout.write('\n');
}
