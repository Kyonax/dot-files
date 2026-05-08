---
title: JSON-LD Management — Inspection, Verification, Authoring
impact: HIGH
impactDescription: How JSON-LD schemas are stored in CMS, the three R1/R2/R3 paths from save-time to request-time, and the scripts that inspect + author additionalScripts entries. Misuse here ships JSON-LD that looks correct in Mongo but never reaches the SEO crawler — wasting the SEO investment without anyone noticing.
tags: jsonld, schema-org, seo, additionalScripts, faqpage, hairsalon, breadcrumblist, pug, forceInterpolation, generatedAutomatically, R1, R2, R3, dp-002, dp-003, ad-006, validation, raw-html, crawler
---

# JSON-LD Management

## Storage path

JSON-LD lives on `contentVersion.renderOptions.additionalScripts[]`. The denormalised render-time copy lives on `production_content[contentId].variations.<platform>[].renderOptions.additionalScripts[]` (and the staged equivalent).

```js
{
  type: "ld+json",          // mime type fragment
  inHeader: true,           // <script> goes in <head> (vs end of <body>)
  forceInterpolation: false, // false = static JSON; true = Pug-driven (#{...} / !{...} / each)
  isUrl: false,             // true = src=url instead of inline body
  addBodyLoadScript: false, // separate body-load entry point — usually false for JSON-LD
  body: "{ \"@context\": \"https://schema.org\", \"@type\": \"FAQPage\", ... }",
  generatedAutomatically: false,  // true ONLY when Tophat auto-gen produced this (R1)
  source: "tophat-tools/add-jsonld-script.mjs"  // optional audit field
}
```

**Tophat checkbox cheat-sheet (UI source: `tophat/src/views/ngpartials/cms/content/edit.pug:~600`):**
* Type → `ld+json`
* Is URL → `false`
* In header → `true` (most JSON-LD belongs in `<head>`)
* Force interpolation → `false` (static JSON) or `true` (Pug-driven, runtime values)
* Add body load script → `false`

## The three render paths (R1/R2/R3)

| Path | Where it's authored                   | Trigger                                                                                  | When to use                                                                  |
|------|---------------------------------------|------------------------------------------------------------------------------------------|------------------------------------------------------------------------------|
| **R1** | Tophat auto-gen flag                | `addFaqMetadata: true` on a `componentList` block triggers `ContentEditCtl.getAdditionalScriptForFAQs` at save time; sets `generatedAutomatically: true`. | FAQ pages with stock structure (e.g. `/colorbar/locations` directory).      |
| **R2** | Tophat hand-authored Pug            | `forceInterpolation: true` + Pug body (`#{...}`, `!{...}`, `each`).                       | Schemas that need request-time interpolation (e.g. per-slug HairSalon).      |
| **R3** | Code (route handler)                | `pushJsonLdToContent(content, schema, metadata)` after CMS load.                          | Schemas where Tophat can't reach the data (e.g. `productJsonLd.js` PDP).     |

**Auto-gen invariant:** never set `generatedAutomatically: true` from a route-handler push. That flag is reserved for Tophat auto-gen and triggers a wipe on the next editor save.

## Verification = raw HTML, never Mongo

The harness from the JSON-LD session enforces this rule: **the only way to verify a JSON-LD change took effect is to fetch the URL and read what an SEO crawler reads.** Mongo and Tophat both lie (denormalisation drift, dev-server cache, per-variation editor save). `inspect-jsonld.mjs` implements the verification path.

```sh
# What does Mongo say is configured?
node get-cms-additional-scripts.mjs 2350 --variation A

# What does an SEO crawler actually see?
node inspect-jsonld.mjs http://localhost:3000/colorbar/locations/hillsboro
node inspect-jsonld.mjs http://localhost:3000/colorbar/locations/hillsboro --variation A
node inspect-jsonld.mjs http://localhost:3000/colorbar/locations/hillsboro --variation B
node inspect-jsonld.mjs http://localhost:3000/colorbar/locations/hillsboro --variation C
```

If Mongo has the script but the rendered HTML doesn't:
1. Restart Vite (per-handler in-process cache) — most common cause locally.
2. Confirm you edited the variation that's actually serving you (cookie-sticky for 30 days).
3. Confirm the script's `inHeader` / `type` checkboxes are correct — wrong type emits as a regular `<script>` and is ignored by crawlers.
4. Re-publish from Tophat — denormalisation from `contentVersion` to `production_content` happens at publish, not at write.

## Per-variation reality

Every contentVersion variation is a separate doc. A page with 3 variations has 3 contentVersion records, each with its own `additionalScripts[]`. A Tophat save only updates the variation currently selected in the editor.

**Common failure mode (corrected by the JSON-LD session F23 finding):** the editor configures variation A; the slug request hits variation B; the audit reports BROKEN-IN-CMS even though Mongo "has the script." The fix is to add the script to **every** variation that should serve it.

`add-jsonld-script.mjs` is intentionally per-variation — pass `--variation A`, then `--variation B`, etc. The script prints a reminder note after each run.

## Scripts

### `inspect-jsonld.mjs <url> [--variation X]`

Read-only. Fetches a URL (raw HTML, no JS, no cookies), extracts every `<script type="application/ld+json">`, parses each, lists `@type`s, flags JSON validity issues (parse errors, concatenated objects, trailing commas), and detects the active experiment from page markers. With `--variation`, re-fetches with `?v=<key>&xid=<experimentId>` overrides.

### `get-cms-additional-scripts.mjs <content_id> [--variation X] [--version N]`

Read-only. Dumps `additionalScripts[]` from the contentVersion **and** the denormalised `production_content` / `stage_content` copies for the same content_id. The three lists should match — diff them to spot drift.

### `add-jsonld-script.mjs <content_id> --variation X --src <file> [...]`

Append-only mutation. Reads the script body from a file (so Pug source is editable) and pushes a new entry onto `contentVersion.renderOptions.additionalScripts[]`. Dry-run unless `--confirm`.

```sh
# Local diagnosis only — DB writes don't ship.
node add-jsonld-script.mjs 2350 --variation A --src ./hairsalon-pug.pug --header --force-interpolation --confirm
node add-jsonld-script.mjs 2350 --variation B --src ./hairsalon-pug.pug --header --force-interpolation --confirm
node add-jsonld-script.mjs 2350 --variation C --src ./hairsalon-pug.pug --header --force-interpolation --confirm
```

**Direct DB writes are NOT a shipping mechanism.** They will be wiped on the next Tophat publish. Use this for diagnosis only — proving the Pug source compiles + emits — then re-do via Tophat editor for production.

## Schema-by-schema recipes (R1 vs R2 vs R3)

| Schema           | Default path | Notes                                                                                         |
|------------------|--------------|-----------------------------------------------------------------------------------------------|
| `FAQPage`        | R1 (preferred) → R2 fallback | `addFaqMetadata: true` on the FAQ component handles most cases. Slug pages need R2 per variation (lesson learned 2026-04-30). |
| `HairSalon`      | R2          | Per-slug interpolation (`#{location.name}`, `each` over hours) — only Pug can pull request-time data. |
| `BreadcrumbList` | R2          | Shared content_id (e.g. `/shop/*` → 2686). One R2 entry can cover all children if Pug interpolates `#{shadeFamilyName}`. |
| `Product`        | R3          | PDP shape lives outside CMS. See `productJsonLd.js` builder. Push from route handler post-load. |
| `Organization`   | R1 (sitewide) | Authored once on the site-wide template; auto-emitted everywhere.                            |

## Validity checks `inspect-jsonld.mjs` flags

* `json-parse-error: <msg>` — JSON.parse failed; usually unescaped quotes in interpolated content.
* `multiple-top-level-objects-concatenated (N)` — two `}{` JSON objects glued together; wrap in `[]`.
* `trailing-commas: N` — invalid JSON; common when you copy/paste from JS object literals.

The harness from the JSON-LD session has a richer `validators.mjs` (`/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/validators.mjs`) — pull that in if you need full schema-vocabulary validation (required-fields, type checks).

## Cross-references

* Tophat editor surface: `tophat/src/views/ngpartials/cms/content/edit.pug:~600`
* Auto-gen controller: `tophat/src/ngscripts/cms/ContentEditCtl.js#getAdditionalScriptForFAQs`
* CMS partial runtime: `website/src/vuescripts/components/CMSPartial/CMSPartial.vue` (`serverPrefetch` + Vue runtime compile of partial HTML)
* Route-handler push helper: `mr_modules/cms/lib/contentLoader.js#pushJsonLdToContent`
* Reference implementations: `/colorbar/locations` (R1 directory FAQPage) and `/colorbar/locations/<slug>` (R2 per-variation HairSalon — the canonical R2 example)
