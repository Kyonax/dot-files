---
name: tophat-tools
description: Madison Reed Tophat / CMS operations toolkit. Scripts and reference docs for inspecting, auditing, and (with safety rails) mutating the Mongo-backed CMS that powers the consumer site. Use when working with Tophat content, contentVersion, template, templateVersion, experiment, production_content, additionalScripts, mixin_key, JSON-LD, A/B variations, weight changes, content migration, or any task that requires translating between CMS metadata and the actual Vue/Express codebase. Wraps `docker exec mr-mongo mongosh cms` queries into structured, JSON-emitting Node scripts so investigations don't burn tokens on raw mongosh output.
metadata:
  author: Kyo
  version: "1.0.0"
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
| `inspection-scripts`         | Read-only scripts (`inspect-content`, `inspect-content-by-uri`, `inspect-template`, `inspect-experiment`, `get-component-list`, `get-template-jade`, `get-template-fields`, `find-template-usage`, `find-template-template-usage`, `get-cms-additional-scripts`, `inspect-jsonld`, `find-cms-component-code`, `find-route`) and the diagnostic flows that combine them. |
| `experiment-management`      | A/B test scripts (`set-experiment-status`, `set-variant-weight`); the two-place weight rule (experiment.variations + contentVersion); variation NAME vs KEY; locked variations; audience targeting; runtime-id vs doc-id distinction. |
| `json-ld-management`         | JSON-LD storage on `additionalScripts[]`; the three render paths (R1 Tophat auto-gen, R2 hand-authored Pug with `forceInterpolation`, R3 route-handler push); per-variation reality (each A/B/C is a separate doc); raw-HTML verification rule; `add-jsonld-script.mjs` and `inspect-jsonld.mjs`; schema-by-schema recipes (FAQPage, HairSalon, BreadcrumbList, Product). |
| `content-migration`          | Re-binding a content_id between experiments via `migrate-content-experiment.mjs`; JSON config schema; idempotent re-runs; companion steps (sub-template creation, old-experiment pause, production replication via Tophat). |
| `code-locator-scripts`       | Kebab-case-to-PascalCase contract; `find-cms-component-code.mjs` (mixin_key → Vue file + global registration line); `find-route.mjs` (URI → Express handler); `find-template-template-usage.mjs` (mixin_key → other templates that reference it via config-field option or jade embed); `set-template-fields.mjs` (programmatic config schema edits); workflows for renaming a mixin_key. |
| `safety-and-conventions`     | Five mandatory rules for every mutation: dry-run by default with `--confirm`, backup to `./cms-backups/`, idempotency, "DB writes are NOT a shipping mechanism", explicit container/db flags. Audit-field discipline. Verification = raw HTML, never Mongo. |

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
```

## Cross-Skill References

* `seo-web-quality` — the rules for *what* JSON-LD should look like (schema vocabularies, on-page SEO standards). This skill is *how* you implement those rules in the MR-specific Tophat plumbing.
* `mr-dotcom-dev` — the Vue / Vuex / Pug / Stylus conventions for the consumer site. Component-side knowledge that complements this skill's CMS-side knowledge.
* `pr-scribe` — the PR-authoring conventions. When you ship a Tophat-related fix, the PR body usually includes a *Special Deployment Requirements* section describing the manual Tophat steps Carley needs to apply.
