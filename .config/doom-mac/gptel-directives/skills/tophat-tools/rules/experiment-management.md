---
title: Experiment & Variation Management Scripts
impact: HIGH
impactDescription: Defines the right way to inspect and edit Tophat experiments + their contentVersion bindings. Misuse here typically produces "the editor showed weight 50 but the page kept serving variant A 100%" symptoms because the two copies of weight are out of sync.
tags: experiment, variation, weight, status, paused, running, locationSpecificSiteRevolution, bookingFlowSiteRevolution, contentVersion, runtime-experiment-id, doc-id, A/B/C, locked, sticky, cookie
---

# Experiment & Variation Management Scripts

## The two-IDs trap

Every experiment has **two** identifiers. Confusing them is the single most common bug:

| Field                    | Where stored                  | Used for                                                    |
|--------------------------|-------------------------------|-------------------------------------------------------------|
| `experiment._id`         | `experiment` collection       | Tophat editor URLs (`/cms/experiment/edit/<_id>`).          |
| `experiment.experimentId`| `experiment` + `contentVersion` | Cookies, `?xid=` URL override, render-time allocation.    |

**The runtime `experimentId` is what `contentVersion.experimentId` references.** When you migrate a content_id from one experiment to another, you update `contentVersion.experimentId` to the *new* runtime ID — not the doc `_id`.

`inspect-experiment.mjs` accepts either form (numeric → tries `_id`; string → tries `name`).

## Two-place weight rule

`weight` lives in two places:

1. `experiment.variations[].weight` — Tophat editor reads this.
2. `contentVersion.weight` (per variationKey) — render-time allocation reads this.

These must match. The Tophat editor publishes both atomically; raw mongo writes don't. **Always use `set-variant-weight.mjs`** — it writes both copies in one run.

## Scripts

### `inspect-experiment.mjs <id|name>`

Read-only. Prints the experiment doc + every contentVersion bound to its runtime experimentId, grouped by content_id+version. See `rules/inspection-scripts.md`.

### `set-experiment-status.mjs <id|name> <Running|Paused|Stopped> [--confirm]`

Sets `experiment.status`. Backs up the doc to `./cms-backups/experiment/<_id>/<stamp>.json` before writing.

```sh
# Pause an experiment (e.g., when retiring it after migration).
node set-experiment-status.mjs 475 Paused --confirm

# Roll out a new test.
node set-experiment-status.mjs LocationSpecificSiteRevolution Running --confirm
```

**Side effects:** the dev server in-process cache may not pick this up until restart or a Tophat publish event. If you're testing locally and the change doesn't surface, restart Vite.

### `set-variant-weight.mjs <id|name> <variation_name> <weight 0-10000> [--confirm]`

Updates **both** `experiment.variations[name=...].weight` and every matching `contentVersion.weight` in one shot. Dry-run by default.

```sh
# 50/50 split.
node set-variant-weight.mjs 504 default 5000 --confirm
node set-variant-weight.mjs 504 b       5000 --confirm

# 100% rollout to B (used for forced-validation testing).
node set-variant-weight.mjs LocationSpecificSiteRevolution default 0 --confirm
node set-variant-weight.mjs LocationSpecificSiteRevolution b 10000 --confirm
```

The script:
1. Loads the experiment, finds the variation by `name` (matches `experiment.variations[].name`, NOT `variationKey`).
2. Backs up the experiment doc to `./cms-backups/experiment/<_id>/<stamp>-weight.json`.
3. Writes `experiment.variations.$.weight`.
4. `updateMany` over `db.contentVersion` matching `{experimentId: <runtime>, variationId: <runtime>}`.
5. Prints the after-state for both.

**Variation NAME vs KEY:**
* `experiment.variations[].name` → "default", "b", "test", "services-prioritized" (editor-friendly)
* `contentVersion.variationKey`  → "A", "B", "C", "D", "E" (positional)
* `contentVersion.variationName` → mirrors `experiment.variations[].name`

The script keys by `name` because that's the stable editor-facing identifier. Cross-walk via `inspect-experiment.mjs` if you only know the key.

### Migrating a content_id between experiments

See `rules/content-migration.md` and `migrate-content-experiment.mjs`. Short version: the migration produces a new contentVersion (next version number) per target variation and rebinds `experimentId`/`variationId` to the new experiment, then bumps `content.published_version`. Old experiment stays in the DB — pause it with `set-experiment-status.mjs` rather than deleting it.

## Locked variations

`experiment.variations[].locked: true` is a Tophat editor flag: the variation is preserved across publishes (so it doesn't get re-numbered). Locked variations can still serve traffic at `weight=0`. The `cms-migrate.mjs` canonical example retires locked variations during migration — they are present on the source experiment but not carried over to the target.

## Audience-targeted variations

`experiment.variations[]` may include `audienceTargeting` (region, customer-state, etc.). Audience targeting routes a variation to a specific cohort regardless of weight. **The skill doesn't yet ship a script for audience editing** — manage from Tophat or extend `set-variant-weight.mjs` with an `--audience-key` flag if you need it.

## Verifying experiment changes

After any write, verify against the rendered page (the SEO crawler view):

```sh
# Probe each A/B/C explicitly via the ?v=&xid= overrides.
node inspect-jsonld.mjs http://localhost:3000/colorbar/location-specific --variation A
node inspect-jsonld.mjs http://localhost:3000/colorbar/location-specific --variation B
```

If the rendered HTML disagrees with what `inspect-experiment.mjs` reports, the dev-server in-process cache is stale. Restart Vite (`npm run dev`) and re-probe.

## Code references

* Server-side allocation: `mr_modules/cms/lib/loaders.js#getContentVariation`
* Cookie sticky logic: `website/src/routing/before.js` + `contexts.js`
* Pre-allocated experiment middleware example: `website/src/routing/views/bookingSiteRevolutionMiddleWare.js:6` (forces a variant via cookie at SSR time)
* Frontend exposure tracking: `website/src/vuescripts/mixins/menuMixin.js:13` (`mix_trackExperimentViewed`)
