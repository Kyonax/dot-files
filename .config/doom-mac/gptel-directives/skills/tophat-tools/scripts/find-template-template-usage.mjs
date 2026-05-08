#!/usr/bin/env node
// find-template-template-usage.mjs <mixin_key|template_id>
//
// Read-only. Finds every OTHER template that references the given template
// (the inverse question of `find-template-usage.mjs`, which tells you which
// content uses a mixin_key).
//
// A template references another in three places:
//
//   1. As a config-field option:  templateVersion.config[].options.template === "<mixin>"
//   2. As a config-field whitelist: templateVersion.config[].options.templates[] includes "<mixin>"
//   3. As a tag inside the jade source: templateVersion.jade contains "<mixin>" (kebab-case match)
//
// (1) and (2) mean: "When editing this template's content, an author can drop
// the target template into this slot." (3) means: "This template's pug body
// directly embeds the target template's tag — no editor choice involved."
//
// Output groups results by the referencing template + how it references.
//
// Examples:
//   node find-template-template-usage.mjs location-specific-colorbar-v2
//   node find-template-template-usage.mjs hcb-landing-sticky
//   node find-template-template-usage.mjs 1650
//
// See: rules/template-field-schema.md and rules/code-locator-scripts.md.

import {
  parseArgs,
  applyMongoFlags,
  findTemplateById,
  findTemplateByMixinKey,
  mongoJson,
  printJson,
  die,
  requirePositional,
} from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
applyMongoFlags(options);
requirePositional(positional, 1, 'find-template-template-usage.mjs <mixin_key|template_id>');

const arg = positional[0];

let target = null;
if (/^\d+$/.test(arg)) {
  target = findTemplateById(Number(arg));
} else {
  target = findTemplateByMixinKey(arg);
}
if (!target) {
  die(`No template found for "${arg}"`);
}

const mixinKey = target.mixin_key;
const targetTemplateId = target._id;

// (1) + (2): config-field references. Scan every templateVersion.config[] +
// nested fieldConfig[]. We only consider the latest published version of each
// template to avoid noise from old versions.
//
// (3): jade embeds. A simple substring match for "<mixinKey" or "(mixinKey "
// (Pug syntax for a tag with attributes) is enough — false positives are rare
// because mixin keys are kebab-case and unique.

const results = mongoJson(`
  const targetMixin = ${JSON.stringify(mixinKey)};

  // Resolve the published_version per template so we only scan live config.
  const tmpls = db.template.find({ is_archived: { $ne: true } }, {_id:1, mixin_key:1, name:1, published_version:1}).toArray();
  const liveTvIds = tmpls.map(t => ({template_id: t._id, version: t.published_version}));

  const out = [];
  liveTvIds.forEach(({template_id, version}) => {
    if (template_id === ${targetTemplateId}) return; // skip self
    const tv = db.templateVersion.findOne(
      {template_id: template_id, version: version},
      {template_id:1, version:1, jade:1, config:1, _id:0}
    );
    if (!tv) return;

    const tmpl = tmpls.find(t => t._id === template_id);
    const refs = [];

    // (3) jade embed
    if (tv.jade && tv.jade.includes(targetMixin)) {
      // Lightweight Pug-style match — kebab-case tag with optional attrs.
      const re1 = new RegExp("(^|[^a-z0-9-])" + targetMixin.replace(/[-]/g, "[-]") + "([^a-z0-9-]|$)");
      if (re1.test(tv.jade)) {
        refs.push({ kind: "jadeEmbed" });
      }
    }

    // (1) + (2) config-field references — walk including nested fieldConfig.
    function walkConfig(arr, path) {
      if (!Array.isArray(arr)) return;
      arr.forEach(f => {
        if (!f) return;
        const opts = f.options || {};
        if (f.type === "component" || f.type === "object") {
          if (opts.template === targetMixin) {
            refs.push({ kind: "configFieldDefault", fieldPath: path + f.name, fieldType: f.type });
          }
          if (Array.isArray(opts.templates) && opts.templates.includes(targetMixin)) {
            refs.push({ kind: "configFieldWhitelist", fieldPath: path + f.name, fieldType: f.type });
          }
        }
        if (Array.isArray(f.fieldConfig)) {
          walkConfig(f.fieldConfig, path + f.name + ".");
        }
      });
    }
    walkConfig(tv.config, "");

    if (refs.length > 0) {
      out.push({
        template_id: tmpl._id,
        mixin_key: tmpl.mixin_key,
        name: tmpl.name,
        published_version: tmpl.published_version,
        references: refs
      });
    }
  });

  return out;
`);

const sorted = (results || []).sort((a, b) => {
  // jadeEmbed > configFieldWhitelist > configFieldDefault for visibility
  const score = (e) => Math.max(...e.references.map((r) => (r.kind === 'jadeEmbed' ? 3 : r.kind === 'configFieldWhitelist' ? 2 : 1)));
  return score(b) - score(a);
});

const counts = {
  jadeEmbed: 0,
  configFieldDefault: 0,
  configFieldWhitelist: 0,
};
sorted.forEach((r) => r.references.forEach((ref) => (counts[ref.kind]++)));

printJson({
  target: {
    _id: targetTemplateId,
    mixin_key: mixinKey,
    name: target.name,
  },
  totalReferencingTemplates: sorted.length,
  byReferenceKind: counts,
  referencingTemplates: sorted,
});
