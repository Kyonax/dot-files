---
title: Read-Only CMS Inspection Scripts
impact: HIGH
impactDescription: First-stop reference for diagnosing any CMS-driven URL or template question. These scripts are pure reads — safe to run anywhere, anytime — and answer 80%+ of "what does the CMS think this URL is?" questions without burning tokens on raw mongosh queries.
tags: inspect, read-only, content, contentVersion, template, templateVersion, experiment, componentList, jade, mixin_key, uri, variation, weight, find, usage, audit
---

# Read-Only CMS Inspection Scripts

All scripts live under `scripts/` in this skill and use the shared `lib/mongo.mjs` wrapper. They print structured JSON to stdout (so you can pipe to `jq`, `grep`, or write to a file). Pass `--container <name>` and `--db <name>` to override the defaults (`mr-mongo` / `cms`).

## Quick reference

| Script                          | Answers                                                                                              |
|---------------------------------|------------------------------------------------------------------------------------------------------|
| `inspect-content.mjs`           | Given `<content_id>`: full doc + variation breakdown + componentList mixin keys per variation.       |
| `inspect-content-by-uri.mjs`    | Given a URL path: resolves to a content_id (with parent walk-up + `takesUrlParameters` fallback).    |
| `inspect-template.mjs`          | Given a `<template_id>` or `<mixin_key>`: template metadata + active templateVersion (jade source).  |
| `inspect-experiment.mjs`        | Given an experiment `_id` or name: variations, status, and every contentVersion bound to it.         |
| `get-component-list.mjs`        | Given `<content_id>` (+ optional `--variation`/`--version`): flat ordered list of mixin_keys + settings. |
| `get-template-jade.mjs`         | Given a template id or mixin_key: prints **only** the jade source (no JSON wrapper) for piping.       |
| `find-template-usage.mjs`       | Given a `<mixin_key>`: every contentVersion that embeds it, grouped by content + version.            |
| `get-cms-additional-scripts.mjs`| Given `<content_id>`: additionalScripts (JSON-LD) from contentVersion + production_content + stage_content (drift check). |
| `inspect-jsonld.mjs`            | Given a URL: fetches raw HTML and prints every `<script type="application/ld+json">` block + types + active experiment. Probes a specific variation via `--variation X`. |
| `find-cms-component-code.mjs`   | Given a `<mixin_key>`: locates the Vue component file in the codebase + its global registration line. |
| `find-route.mjs`                | Given a URI: greps the Express routing layer for handlers; warns when the URI is served via the CMS catch-all. |

## Standard flags (every script)

| Flag                | Default     | Purpose                                            |
|---------------------|-------------|----------------------------------------------------|
| `--container <name>`| `mr-mongo`  | Docker container running mongosh.                  |
| `--db <name>`       | `cms`       | Database name.                                     |
| `--version <N>`     | published   | Pin the contentVersion / templateVersion to read.  |
| `--variation <key>` | all         | Restrict to one A/B variation (where applicable).  |
| `--repo <path>`     | auto-detect | Repo root for `find-cms-component-code` / `find-route`. |

## Common diagnostic flows

### "What renders on this URL?"

```sh
# 1. Resolve URL → content + variations live right now.
node inspect-content-by-uri.mjs /colorbar/location-specific

# 2. See the exact block list for the served variation.
node get-component-list.mjs 3117 --variation B

# 3. For one of those mixin_keys, find the Vue code.
node find-cms-component-code.mjs location-specific-colorbar-v2

# 4. Or get the CMS sub-template's pug body.
node get-template-jade.mjs location-specific-colorbar-v2
```

### "Did the JSON-LD I added to Tophat actually take effect?"

```sh
# 1. What does Mongo think is configured?
node get-cms-additional-scripts.mjs 2350 --variation A

# 2. What does an SEO crawler ACTUALLY see?
node inspect-jsonld.mjs http://localhost:3000/colorbar/locations/hillsboro --variation A
node inspect-jsonld.mjs http://localhost:3000/colorbar/locations/hillsboro --variation B
node inspect-jsonld.mjs http://localhost:3000/colorbar/locations/hillsboro --variation C

# 3. Drift between production_content (renderable) and contentVersion (editable)?
#    get-cms-additional-scripts.mjs prints all three copies — diff them.
```

### "Where will my template change ripple?"

```sh
# 1. Every page that embeds this mixin_key.
node find-template-usage.mjs hcb-landing-sticky

# 2. Output is grouped by content_id, with publishedVersion + isLive flag.
#    Pages where isLive=true and the mixin_key is in liveVariations are
#    affected on the next render after a publish.
```

### "What are the experiment weights right now?"

```sh
node inspect-experiment.mjs LocationSpecificSiteRevolution
# Returns: experiment doc + every contentVersion bound to it (per content_id, per version, per variation).
```

## Output format

All inspection scripts emit pretty-printed JSON on stdout. Use `jq` for filtering:

```sh
node inspect-content-by-uri.mjs /colorbar/location-specific | jq '.variations[] | {key: .variationKey, weight: .weight, blocks: .componentMixinKeys}'
```

`get-template-jade.mjs` is the one exception — it writes the raw pug body (no JSON wrapping) so you can redirect:

```sh
node get-template-jade.mjs location-specific-colorbar-v2 > /tmp/lscv2.pug
```
