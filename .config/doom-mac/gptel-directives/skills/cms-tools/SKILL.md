---
name: cms-tools
description: >-
  Operational toolkit for Madison Reed's Tophat CMS (MongoDB-backed). Inspect
  content, templates, experiments, partials, JSON-LD, and mixin_key bindings
  via structured Node scripts. Mutate with dry-run + backup safety rails.
  Covers: inspect-content, inspect-content-by-uri, inspect-experiment,
  inspect-partial, find-template-usage, inspect-jsonld (per A/B variation),
  experiment status/weight management, JSON-LD authoring (R1/R2/R3 paths),
  content migration (rebind content_id between experiments), partial scaffolding
  (create-partial-template + create-partial-content). MCP-aware: tophat-cms
  channel provides CRUD against local CMS API without UI clicks.
  Trigger: tophat, cms, contentVersion, mixin_key, experiment, additionalScripts,
  JSON-LD, content_id, templateVersion, production_content, 'what does this URL
  render', 'migrate content', 'rebind experiment', 'scaffold partial'.
user-invocable: true
metadata:
  openclaw:
    emoji: 🗄️
    os: [darwin]
    requires:
      bins: [docker, node]
---

# Tophat Tools Skill Guide

Comprehensive operational toolkit for Madison Reed's Tophat CMS. Provides:

* A shared mongosh wrapper (`scripts/lib/mongo.mjs`) so every script talks to the DB the same way.
* Read-only inspection scripts that emit pretty JSON for `jq`-pipeable workflows.
* Mutation scripts that always dry-run and back up before writing.
* Code-locator scripts that translate between CMS metadata (mixin_key, URI) and the codebase (Vue components, Express routes).
* Reference rules covering the data model, scripts, JSON-LD storage paths, experiment management, content migration, and safety conventions.

## When to Apply

Reference this skill when:

* Investigating "what does this URL render?" / "what's bound to this experiment?" / "what code runs for this CMS block?"
* Auditing or authoring JSON-LD (FAQPage, HairSalon, Product, BreadcrumbList) — knowing which of the R1/R2/R3 paths to use.
* Re-binding a content_id between experiments (the DOTCOMPB-8120 pattern).
* Tuning experiment weights or pausing/running experiments.
* Reproducing a production CMS state locally for debugging.
* Any time you'd otherwise be writing one-off `docker exec mr-mongo mongosh cms --eval '…'` queries.

## Quick Reference

| Rule                         | Description                                                                                                                              |
|------------------------------|------------------------------------------------------------------------------------------------------------------------------------------|
| `cms-data-model`             | Mongo collections (`content`, `contentVersion`, `template`, `templateVersion`, `experiment`, `production_content`, `stage_content`, `counters`); the runtime-experimentId vs doc-id trap; denormalisation paths; URI resolution rules; dev-server cache caveats. |
| `template-field-schema`      | All 23 field types in `templateVersion.config[]` (text, html, textarea, boolean, number, link, image, croppedImage, staticImage, staticCroppedImage, component, object, partial, sectionHeader, dateTime, icon, product, productPrice, productType, promotion, featuredReview, specificReview, select); per-type `options` keys (required, xsClass/smClass/mdClass/lgClass, allowMultiples, template, templates, aspectRatio, crops, rows, min/max, helpText); nested `fieldConfig` for object fields; selectOptions modern vs legacy shape; authoring workflow. |
| `inspection-scripts`         | Read-only scripts (`inspect-content`, `inspect-content-by-uri`, `inspect-template`, `inspect-partial`, `inspect-experiment`, `get-component-list`, `get-template-jade`, `get-template-fields`, `find-template-usage`, `find-template-template-usage`, `get-cms-additional-scripts`, `inspect-jsonld`, `find-cms-component-code`, `find-route`) and the diagnostic flows that combine them. `inspect-partial` is the one-shot read for a CMS partial — dumps the content + active contentVersion + paired template + active templateVersion in a single JSON blob with a `diagnostics` block flagging missing pieces. |
| `experiment-management`      | A/B test scripts (`set-experiment-status`, `set-variant-weight`); the two-place weight rule (experiment.variations + contentVersion); variation NAME vs KEY; locked variations; audience targeting; runtime-id vs doc-id distinction. |
| `json-ld-management`         | JSON-LD storage on `additionalScripts[]`; the three render paths (R1 Tophat auto-gen, R2 hand-authored Pug with `forceInterpolation`, R3 route-handler push); per-variation reality (each A/B/C is a separate doc); raw-HTML verification rule; `add-jsonld-script.mjs` and `inspect-jsonld.mjs`; schema-by-schema recipes (FAQPage, HairSalon, BreadcrumbList, Product). |
| `content-migration`          | Re-binding a content_id between experiments via `migrate-content-experiment.mjs`; JSON config schema; idempotent re-runs; companion steps (sub-template creation, old-experiment pause, production replication via Tophat). |
| `code-locator-scripts`       | Kebab-case-to-PascalCase contract; `find-cms-component-code.mjs` (mixin_key → Vue file + global registration line); `find-route.mjs` (URI → Express handler); `find-template-template-usage.mjs` (mixin_key → other templates that reference it via config-field option or jade embed); `set-template-fields.mjs` (programmatic config schema edits); workflows for renaming a mixin_key. |
| `safety-and-conventions`     | Five mandatory rules for every mutation: dry-run by default with `--confirm`, backup to `./cms-backups/`, idempotency, "DB writes are NOT a shipping mechanism", explicit container/db flags. Audit-field discipline. Verification = raw HTML, never Mongo. |
| `partials`                   | CMS partials (`mixin_key`-addressable HTML+CSS blocks loaded at runtime via `<cms-partial mixin-key="…">`). The three moving parts (partial **template** + paired **content** + Vue **mount point**); template `type` semantics (`partial` / `component` / `container` / `layout` — at runtime they all render the same via the partial pipeline); render flow end-to-end (page jade → `CMSPartial.vue` → `cms/loadPartial` → `vueCmsSvc.getPartial` → `mr_modules/webservices/cmsSvc.js` → `htmlRenderer.renderPartialContent` → `loader.loadPageAsync` → `renderContainerAsync` + `compileStylusAsync`); the `partial-loader` (template 1319) and `partial-preview` (1293) meta-templates and when to use each; the **global-component resolution requirement** for any Vue tag inside a partial's jade — must be registered in BOTH `mrVueApp.js` AND `registerGlobalsSsr.js` (or use `CmsPartialSsr.vue`'s curated local components: `EmailCaptureBlock`, `StoreValue`, `MrBtn`, `MrIcon`); 7-step scaffolding recipe for building a partial from zero (Vue component → global registration on client AND SSR → partial template insertion via `create-partial-template.mjs` → partial content + contentVersion insertion via `create-partial-content.mjs` → mount in parent template's jade → `inspect-partial.mjs` + REST/page test → Tophat production replication hand-off); three dedicated scripts shipped in `tophat-tools v1.1.0` (`inspect-partial.mjs` for one-shot four-document read with `diagnostics` block flagging missing pieces; `create-partial-template.mjs` and `create-partial-content.mjs` for idempotent, dry-run-by-default, backup-before-write inserts with the standard `--confirm` discipline); SSR considerations (`serverPrefetch` runs on the server, `mounted` only on the client, partial CSS injected as inline `<style>` works on both); experiment + audience tracking inside partials (`content.experimentId` → `Experiment Viewed` event on mount); rename hazards across `content.mixin_key` and `template.mixin_key`; diagnostic recipes ("partial doesn't appear" → 404 vs empty html; "raw `<offer-callout>` text visible" → global registration missing; "settings.foo undefined" → templateData/config mismatch; "experiment fires twice" → `hasTrackedExperiment` gate); reference partials in this codebase (`thick-banner-v4` reusable 1:many, `sugg-limitless-pro-template` dedicated 1:1 — only `type=partial` template, `partial-urm-perks` jade-embedded from `hcb-founder-membership`, `partial-take-quiz-blog` via `cms-partial-ssr` in blog layout 1170, `partial-loader` 1319 for CMS-configurable mixin keys, `ColorKitPdpV2.vue:35` for Vue-bound mixin key). |

## Architecture (`AGENTS.md`)

See `AGENTS.md` for the architectural rationale: why this skill exists (token economics across the JSON-LD / CMS investigation pattern), the two-agent model the rule files follow, the script categories (inspect / mutate / code-locator), extension points for future iterations, and the boundary lines (this is not a Tophat replacement, not an SEO auditor, not a content authoring tool).

## Common Workflows (cheat sheet)

```sh
# What renders on this URL?
node scripts/inspect-content-by-uri.mjs /colorbar/location-specific
node scripts/get-component-list.mjs <contentId> --variation B
node scripts/find-cms-component-code.mjs <mixin_key>

# What does the SEO crawler actually see (per A/B/C)?
node scripts/inspect-jsonld.mjs http://localhost:3000<uri>
node scripts/inspect-jsonld.mjs http://localhost:3000<uri> --variation A
node scripts/inspect-jsonld.mjs http://localhost:3000<uri> --variation B

# Where will my template change ripple?
node scripts/find-template-usage.mjs <mixin_key>             # → content bindings
node scripts/find-template-template-usage.mjs <mixin_key>    # → other templates that reference this one

# What fields does this template expose to the editor?
node scripts/get-template-fields.mjs <template>              # default human-readable
node scripts/get-template-fields.mjs <template> --flat       # all leaves in dot-path notation
node scripts/get-template-fields.mjs <template> --json > /tmp/fields.json   # JSON for piping/editing

# Edit a template's field schema (replace or merge mode).
node scripts/set-template-fields.mjs <template> --src /tmp/fields.json
node scripts/set-template-fields.mjs <template> --src /tmp/fields.json --confirm
node scripts/set-template-fields.mjs <template> --src /tmp/fields.json --mode merge --confirm

# Pause an experiment, tune weights.
node scripts/set-experiment-status.mjs <id|name> Paused --confirm
node scripts/set-variant-weight.mjs <id|name> <variation_name> 5000 --confirm

# Migrate a content_id from one experiment to another (DOTCOMPB-8120 pattern).
node scripts/backup-content.mjs <contentId>
node scripts/migrate-content-experiment.mjs <contentId> --from-config ./cfg.json
node scripts/migrate-content-experiment.mjs <contentId> --from-config ./cfg.json --confirm
# If something goes wrong:
node scripts/restore-content.mjs ./cms-backups/<contentId>/<stamp>/snapshot.json --confirm

# Scaffold a CMS partial from zero (template + content + verification).
# Authoring order matters: template first (content's templateKey check refuses
# if the template hasn't landed). Both scripts are idempotent on re-run.
node scripts/inspect-partial.mjs <content_mixin_key>                                    # one-shot read of all four docs
node scripts/create-partial-template.mjs --src .tasks/<TICKET>/<name>-template.json     # dry-run
node scripts/create-partial-template.mjs --src .tasks/<TICKET>/<name>-template.json --confirm
node scripts/create-partial-content.mjs  --src .tasks/<TICKET>/<name>-content.json      # dry-run (template-existence check)
node scripts/create-partial-content.mjs  --src .tasks/<TICKET>/<name>-content.json --confirm
curl -s 'http://localhost:3000/api/cmsSvc/getPartial?mixinKey=<content_mixin_key>' | jq '.data | {html, css}'
```

## Cross-Skill References

* `seo-web-quality` — the rules for *what* JSON-LD should look like (schema vocabularies, on-page SEO standards). This skill is *how* you implement those rules in the MR-specific Tophat plumbing.
* `mr-dotcom-dev` — the Vue / Vuex / Pug / Stylus conventions for the consumer site. Component-side knowledge that complements this skill's CMS-side knowledge.
* `pr-scribe` — the PR-authoring conventions. When you ship a Tophat-related fix, the PR body usually includes a *Special Deployment Requirements* section describing the manual Tophat steps Carley needs to apply.
