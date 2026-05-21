---
title: Content Migration — Re-binding a content_id Between Experiments
impact: HIGH
impactDescription: Spec for the migration script that retires one experiment + variations and binds a content_id to a new one. This is exactly what the DOTCOMPB-8120 cms-migrate.mjs did for content_id 3117. Misuse can leave a page bound to a paused experiment, serving the wrong layout, or with mismatched weights.
tags: migration, content-migration, experiment-rebind, contentVersion, version-bump, publish, componentList, location-specific-colorbar-v2, dotcompb-8120, cms-migrate, idempotent, backup, restore, atomic
---

# Content Migration — Re-binding a content_id Between Experiments

## When to use

You need this when:

1. An old experiment is being retired (status → `Paused`) and a new experiment takes its place.
2. The new experiment has a different variation layout (different blocks, different weights, different settings).
3. You need a reproducible, idempotent local apply that mirrors what Carley will do in the Tophat editor on staging/prod.

Example: DOTCOMPB-8120 retired *New Messaging Test July 2025* (5 variations on content_id 3117 v54) in favour of *LocationSpecificSiteRevolution* (2 variations on v55, with a slim 5-block layout at variant B). The canonical implementation is at `.tasks/DOTCOMPB-8120/cms-migrate.mjs` in the MR repo.

The generalised version of that script lives in this skill at `scripts/migrate-content-experiment.mjs`. It takes a JSON config file describing the source and target.

## Config schema

```json
{
  "contentId": 3117,
  "fromVersion": 54,
  "toVersion": 55,
  "newExperimentId": 177809681959624,
  "newExperimentDocId": 504,
  "publish": true,
  "variations": [
    {
      "key": "A",
      "name": "default",
      "weight": 0,
      "newVariationId": 1778096819596240,
      "sourceVariationKey": "A",
      "componentList": "inherit"
    },
    {
      "key": "B",
      "name": "b",
      "weight": 10000,
      "newVariationId": 1778096819596241,
      "sourceVariationKey": "A",
      "componentList": [
        { "mixin_key": "site-message-carousel",          "from": "A" },
        {
          "mixin_key": "location-specific-colorbar-v2",
          "settings": {
            "title": "Salon results without salon cost & time",
            "subtitle": "Let our pros do it for you. Get roots, all over color, highlights, and more.",
            "colorbarTitle": "Locations near you"
          },
          "templateVersion": 1,
          "templateType": "component"
        },
        { "mixin_key": "letter-from-amy",                 "from": "A" },
        { "mixin_key": "party-confetti",                  "from": "A" },
        { "mixin_key": "hcb-landing-sticky",              "from": "A" }
      ]
    }
  ]
}
```

| Field                     | Meaning                                                                                |
|---------------------------|----------------------------------------------------------------------------------------|
| `contentId`               | The content_id to migrate.                                                              |
| `fromVersion`             | The current `published_version` you're migrating off of.                                |
| `toVersion`               | The new version number to materialise. Usually `fromVersion + 1`.                       |
| `newExperimentId`         | The new experiment's runtime ID. Get from `inspect-experiment.mjs`.                     |
| `newExperimentDocId`      | The new experiment's `_id`. Documentation only; not used by the writer.                 |
| `publish`                 | When `true`, bumps `content.{edit,staged,published}_version` to `toVersion`.            |
| `variations[].key`        | New variationKey (A/B/C/...).                                                           |
| `variations[].name`       | New variationName (matches `experiment.variations[].name`).                             |
| `variations[].weight`     | 0-10000 parts per 10000.                                                                 |
| `variations[].newVariationId` | The new experiment's runtime variation ID for this slot.                            |
| `variations[].sourceVariationKey` | Which existing variation at `fromVersion` to clone from (usually "A" / default).  |
| `variations[].componentList` | `"inherit"` to copy verbatim; or an array of explicit blocks. Each block is either: <br>• `{ mixin_key, from: "A" }` — copy the source block at this position, or <br>• `{ mixin_key, settings, templateVersion, templateType }` — inline new block. |

## How the script runs

1. **Validate** every source variation exists at `fromVersion`. Aborts if any are missing.
2. **Backup** to `./cms-backups/<contentId>/<ISO-stamp>/snapshot.json` (full content + every contentVersion).
3. **Drop** any pre-existing rows at `toVersion` (`db.contentVersion.deleteMany`). This makes re-runs idempotent — useful when iterating on the config.
4. **Materialise** each target variation by:
   * Cloning the source contentVersion doc.
   * Allocating a fresh `_id` via `db.counters.findAndModify({_id:"contentVersion"}, {$inc:{seq:1}})`.
   * Overriding `version`, `variationKey`, `variationName`, `experimentId`, `variationId`, `weight`.
   * Rewriting `templateData.componentList` per the config (verbatim copy via `from`, or inline new blocks).
   * `db.contentVersion.insertOne(cloned)`.
5. **Publish** (if `publish: true`) — sets `content.edit_version`, `staged_version`, `published_version` to `toVersion`.
6. **Verify** — prints the new variations and the bumped content version.

## Companion steps (out of script — manual or separate scripts)

* **Pause the old experiment** so it stops serving in editor previews:
  ```sh
  node set-experiment-status.mjs <oldExperimentDocId> Paused --confirm
  ```
* **Sub-template setup** — if the new componentList references a `mixin_key` that doesn't yet have a `template` doc, the script will fail. Create the sub-template first (extension hook: `create-subtemplate.mjs` is a candidate for the next skill iteration; for now, the canonical `cms-migrate.mjs` does this inline at "step 1: ensure new sub-template exists").
* **Production replication** — Carley does this manually in Tophat. The local migration is the pre-flight: it proves the config is valid and the layout renders correctly on dev before she touches staging/prod.

## Idempotency + recovery

Each `migrate-content-experiment.mjs` run drops + re-creates the rows at `toVersion`. Re-running with a tweaked config replays cleanly.

If the migration goes wrong:

```sh
# List backups
ls ./cms-backups/<contentId>/

# Restore (dry-run by default)
node restore-content.mjs ./cms-backups/<contentId>/<ISO-stamp>/snapshot.json
node restore-content.mjs ./cms-backups/<contentId>/<ISO-stamp>/snapshot.json --confirm
```

`restore-content.mjs` rolls back content + contentVersion + production_content + stage_content. It does **not** auto-restore experiments or counters — those are intentionally left for manual review.

## Verifying the migration landed

After `--confirm`:

```sh
# 1. Mongo state.
node inspect-content.mjs <contentId>
# Confirm: published_version=toVersion, exactly N variations, weights match.

# 2. The new experiment is bound and Running.
node inspect-experiment.mjs <newExperimentDocId>
# Confirm: bindings list shows your contentId at toVersion.

# 3. The page renders the new layout (the empty fragment thing if your V2
#    branch is empty is OK).
curl -sS http://localhost:3000<uri> | grep -E "data-cms-tid|location-specific-colorbar-v2"
```

If the page still renders the old layout: restart Vite. The dev server caches per request handler.

## Reference implementation

* `.tasks/DOTCOMPB-8120/cms-migrate.mjs` — the original, content-3117-specific script. Inline sub-template creation, hardcoded experiment IDs, three subcommands (`inspect`, `backup`, `migrate --confirm`, `restore <stamp>`). Use this as the worked example when extending the generalised script.
* See `rules/cms-data-model.md` for the contentVersion schema this all targets.
