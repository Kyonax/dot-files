---
title: Tophat / MR CMS Mongo Data Model
impact: HIGH
impactDescription: Authoritative reference for the Mongo collections that back Tophat. Without this map you can't write correct queries, you'll read the wrong copy of denormalised data, or you'll edit one variation and miss the other A/B/C records.
tags: cms, mongo, schema, content, contentVersion, template, templateVersion, experiment, production_content, stage_content, additionalScripts, componentList, variations, mixin_key, denormalisation, cache
---

# Tophat / MR CMS Mongo Data Model

## Where it lives

* MongoDB container: `mr-mongo` (Docker Compose service from `npm run mongo`)
* Database: **`cms`** — *not* `madisonreed`
* Shell: `docker exec -it mr-mongo mongosh cms`
* Tophat editor: `http://localhost:4000/#/cms/content/edit/<content_id>`

All scripts in this skill use `docker exec mr-mongo mongosh cms` and parse stdout via `EJSON.stringify(...)` so the output round-trips as JSON.

## Collection map

| Collection             | Purpose                                                                                       |
|------------------------|-----------------------------------------------------------------------------------------------|
| `content`              | One doc per CMS-managed URL (page).                                                           |
| `contentVersion`       | Per-version, per-A/B-variation copy of templateData, renderOptions, etc.                      |
| `template`             | One doc per CMS template / sub-template (mixin_key registry, version pointers).               |
| `templateVersion`      | Per-version body of a template — jade source, component_list, partial config.                 |
| `experiment`           | A/B test definitions: name, status, variations array, runtime experimentId.                   |
| `production_content`   | Denormalised render-time copy of content + active variations.                                 |
| `stage_content`        | Denormalised staged copy (same shape as production_content).                                  |
| `counters`             | Auto-increment sequences (`_id` allocation for content, contentVersion, template, etc.).      |

## `content`

```js
{
  _id: 3117,                       // numeric content_id
  name: "Salon-Quality Hair Color Landing Page",
  uri: "/colorbar/location-specific",
  takesUrlParameters: false,       // when true, child URIs walk-up resolve here
  template_id: <id>,               // top-level template
  edit_version: 55,
  staged_version: 55,
  published_version: 55,           // <- read-time pointer used by the renderer
  created_at, updated_at, created_by, updated_by
}
```

**Read-time:** the renderer picks `production_content[contentId].variations.<platform>[]`. At publish time, Tophat denormalises from `contentVersion` into `production_content`. **Direct DB writes to `contentVersion` may not surface until a Tophat publish or dev-server restart.**

## `contentVersion`

One doc **per (content_id, version, variationKey)**. A page on an experiment with 3 variations has 3 contentVersion records per published version.

```js
{
  _id: <auto>,
  content_id: 3117,
  version: 55,
  variationKey: "B",                       // A/B/C/...
  variationName: "b",                      // editor-friendly label
  experimentId: 177809681959624,           // RUNTIME experimentId (not _id!)
  variationId: 1778096819596241,           // runtime variation ID
  weight: 10000,                           // parts per 10000 (10000 = 100%)
  audienceKey: null,
  audienceName: null,
  templateData: {
    componentList: [
      { mixin_key: "site-message-carousel", settings: {...}, templateVersion: 1, templateType: "component" },
      { mixin_key: "location-specific-colorbar-v2", settings: { title: "...", subtitle: "...", colorbarTitle: "..." }, ... },
      ...
    ],
    _lockedFields: {}
  },
  renderOptions: {
    additionalScripts: [
      { type: "ld+json", inHeader: true, forceInterpolation: true, isUrl: false, body: "..." }
    ]
  },
  created_at, updated_at, created_by, updated_by
}
```

**Critical realities (from JSON-LD session):**
* `experimentId` here is the *runtime* ID (large int, e.g. `177809681959624`), **not** `experiment._id` (small int, e.g. `504`).
* Each variation is a *separate doc.* An edit that should affect "the page" must be replayed across every variationKey.
* Tophat's "Advanced Config / Scripts" panel saves to whichever variation is currently selected in the editor — **not** all variations. The most common reason a Tophat save "doesn't reflect" is "edited variation A, hit variation B." Use `inspect-jsonld.mjs <url> --variation B` to confirm what each variation actually serves.

## `template` + `templateVersion`

```js
// template
{
  _id: 1650,
  type: "component",                    // 'component' or 'page'
  mixin_key: "location-specific-colorbar-v2",
  name: "Location Specific Colorbar V2 (Site Revolution)",
  targetPlatform: "desktop",
  edit_version: 1, staged_version: 1, published_version: 1,
  is_archived: false,
  ...
}

// templateVersion
{
  _id: <auto>,
  template_id: 1650,
  version: 1,
  isStatic: true,                       // true = jade body authored here; false = points at static path
  jade: "location-specific-colorbar-v2(:cms-settings='!{JSON.stringify(settings)}')",
  component_list: [],                   // child sub-templates (not the same as componentList in contentVersion)
  modal_list: [],
  config: [],
  styl: "",
  additionalScripts: [],
  partialConfig: [],
  partialData: {},
  previewData: {},
  include_list: []
}
```

**The `mixin_key` is the contract** between the CMS partial and the global Vue tag. When you create or rename a sub-template, the corresponding global Vue component must register under the same kebab-case name in `mrVueApp.js` and `registerGlobalsSsr.js`.

## `experiment`

```js
{
  _id: 504,                          // EDITOR doc ID (small int)
  name: "LocationSpecificSiteRevolution",
  status: "Running",                 // "Running" | "Paused" | "Stopped"
  experimentId: 177809681959624,     // RUNTIME ID (used in cookies, ?xid=, contentVersion)
  targetPlatform: "desktop",
  audienceTargeting: {...},
  variations: [
    { name: "default", weight: 0,     experimentId: <runtime>, variationId: 1778096819596240, locked: false },
    { name: "b",       weight: 10000, experimentId: <runtime>, variationId: 1778096819596241, locked: false }
  ],
  created_at, updated_at, created_by, updated_by
}
```

`weight` is **parts per 10000** (so 5000 = 50%). The CMS reads weights at allocation time from `contentVersion` (denormalised), but the Tophat UI shows them from `experiment.variations`. **Both must match** — `set-variant-weight.mjs` writes both.

## `production_content` / `stage_content`

Denormalised render-time documents. One per content_id. Shape:

```js
{
  _id: 3117,
  variations: {
    desktop: [
      { variationKey: "A", variationName: "default", experimentId, variationId, weight,
        templateData: {...}, renderOptions: { additionalScripts: [...] } },
      { variationKey: "B", ... }
    ],
    mobile: [...]
  }
}
```

The dev server picks variations from this collection at request time (cookie / experiment-allocation logic decides which one is served). When you write to `contentVersion` directly you must also update the matching record under `production_content` / `stage_content` for the change to surface — the canonical migration script (`migrate-content-experiment.mjs`) does this via Tophat's publish mechanism, not raw writes.

**Dev-server cache caveat:** the Vue dev server (`npm run dev`) caches the per-handler content tree per request handler. Direct DB writes to `contentVersion` / `production_content` are not reliably picked up until either (a) the dev server restarts or (b) Tophat's publish event fires. This invalidates raw DB writes as a verification mechanism for "did Tophat config land?"

## `counters`

```js
{ _id: "contentVersion", seq: 12345 }
{ _id: "template",       seq: 1700 }
{ _id: "templateVersion", seq: 19000 }
```

Use `findAndModify({_id:"<name>"}, {$inc:{seq:1}}, {new:true})` to allocate the next ID.

## URI → content resolution

When a URL request comes in:

1. Exact `uri` match.
2. If no exact match, walk parent paths (e.g. `/shop/brown` → `/shop`) and require `takesUrlParameters: true` on the parent.
3. Fall through to a 404 / catch-all.

`inspect-content-by-uri.mjs` and `lib/mongo.mjs#findContentByUri` implement step 1 + 2.

## Cross-references in this codebase

* `mr_modules/cms/lib/loaders.js` — `getContent`, `getContentVariation` (the `?v=` / `?xid=` overrides for QA).
* `tophat/src/views/ngpartials/cms/content/edit.pug` — Tophat editor UI source for the additionalScripts panel.
* `tophat/src/ngscripts/cms/ContentEditCtl.js` — Tophat publish flow + `getAdditionalScriptForFAQs` auto-gen.
* `website/src/vuescripts/components/CMSPartial/CMSPartial.vue` — runtime resolver for partial mixin_keys (HTML compiled at request time).
* `website/src/vuescripts/mrVueApp.js` + `ssr/registerGlobalsSsr.js` — global Vue registration (the kebab-case tag a CMS template embeds).
