#!/usr/bin/env node
// get-template-fields.mjs <template_id|mixin_key> [--version N] [--flat] [--json]
//
// Read-only. Prints the field schema of a template (the `config` array on its
// templateVersion) — i.e., the editable fields a Tophat author sees when they
// open this template's settings panel. Each field includes its type, options,
// helpText, default, selectOptions, and any nested fieldConfig.
//
// Output modes:
//   default  → tabular, indented, human-readable (good for one-off inspection)
//   --flat   → flat table, every leaf-and-inner field in dot-path notation
//   --json   → raw JSON of the config array (good for piping or as a starting
//              point for set-template-fields.mjs --src ...)
//
// Examples:
//   node get-template-fields.mjs 17
//   node get-template-fields.mjs hero-static
//   node get-template-fields.mjs home-tabs-vs --flat
//   node get-template-fields.mjs location-specific-colorbar-v2 --json > /tmp/lscv2-fields.json
//
// See: rules/template-field-schema.md.

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

const { positional, options, flags } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'get-template-fields.mjs <template_id|mixin_key> [--version N] [--flat] [--json]');

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

const config = Array.isArray(tv.config) ? tv.config : [];

if (flags.has('--json')) {
  printJson(config);
  process.exit(0);
}

function isRequired(f) {
  return !!(f.options && f.options.required);
}

function flattenFields(arr, path = '') {
  const out = [];
  if (!Array.isArray(arr)) {
    return out;
  }
  for (const f of arr) {
    const fpath = path ? `${path}.${f.name}` : f.name;
    out.push({
      path: fpath,
      type: f.type,
      required: isRequired(f),
      default: f.default,
      helpText: f.helpText || null,
      options: f.options || {},
      selectOptions: f.selectOptions || [],
      hasFieldConfig: Array.isArray(f.fieldConfig) && f.fieldConfig.length > 0,
    });
    if (Array.isArray(f.fieldConfig)) {
      out.push(...flattenFields(f.fieldConfig, fpath));
    }
  }
  return out;
}

if (flags.has('--flat')) {
  const flat = flattenFields(config);
  // Print as a JSON array of {path, type, required, default, helpText} so it's
  // greppable yet structured.
  printJson({
    template: {
      _id: template._id,
      mixin_key: template.mixin_key,
      name: template.name,
      version,
    },
    fieldCount: flat.length,
    fields: flat,
  });
  process.exit(0);
}

// Default: indented human-readable view.
function fmtDefault(v) {
  if (v === null || v === undefined) {
    return '';
  }
  if (typeof v === 'string') {
    return JSON.stringify(v);
  }
  return JSON.stringify(v);
}

function describeField(f, depth) {
  const indent = '  '.repeat(depth);
  const req = isRequired(f) ? ' (required)' : '';
  const def = f.default !== undefined && f.default !== null ? ` default=${fmtDefault(f.default)}` : '';
  const lines = [`${indent}${f.name}: ${f.type}${req}${def}`];
  if (f.helpText) {
    lines.push(`${indent}  help: ${f.helpText}`);
  }
  const opts = Object.keys(f.options || {}).filter((k) => k !== 'xsClass');
  if (opts.length > 0) {
    lines.push(`${indent}  options: ${JSON.stringify(f.options, null, 0)}`);
  }
  if (Array.isArray(f.selectOptions) && f.selectOptions.length > 0) {
    const choices = f.selectOptions.map((o) => `${o.value}=${JSON.stringify(o.label)}`).join(', ');
    lines.push(`${indent}  choices: ${choices}`);
  }
  if (Array.isArray(f.fieldConfig) && f.fieldConfig.length > 0) {
    f.fieldConfig.forEach((sub) => lines.push(describeField(sub, depth + 1)));
  }
  return lines.join('\n');
}

console.log(`Template: ${template.mixin_key} (_id=${template._id}) "${template.name}"`);
console.log(`Version: ${version}`);
console.log(`Field count: ${config.length} top-level (${flattenFields(config).length} total incl. nested)`);
console.log('');
console.log('Fields:');
console.log(config.map((f) => describeField(f, 1)).join('\n'));
