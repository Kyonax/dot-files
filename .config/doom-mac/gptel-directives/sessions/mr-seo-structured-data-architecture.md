# MR SEO Structured Data Architecture Memory

## 1. Summary

**Domain:** Madison Reed website — server-rendered structured data (JSON-LD) for SEO. Schema injection mechanism, page-template wiring, sanitization rules, and the route-handler vs CMS-additionalScripts split.

**Project:** Madison Reed dotcom monorepo (`/Volumes/dev-partition/github-madison-reed/the-code`)

**Last updated:** 2026-04-30 evening (v3 — added dp-004 idempotent push, dp-005 server-side CMS-data port, cl-003 slugged-child gap, rr-005 BROKEN-IN-CMS detection methodology). v2 (2026-04-30 morning — live DB audit invalidated ad-001; added ad-005, ad-006, dp-002, dp-003, cl-002, rr-003, rr-004).

**Source sessions:**
- `dotcompb-7929-json-ld-non-pdp.md` (2026-04-29) — Phase 0 validation findings, refined ACs, mechanism decision, builder utility design
- `dotcompb-7929-json-ld-non-pdp.md` (2026-04-30 morning) — live MongoDB audit; superseded ad-001; documented Tophat auto-gen + Pug-interpolated additionalScripts + helper-based route-handler push patterns
- `dotcompb-7929-json-ld-non-pdp.md` (2026-04-30 evening, v8-v13 plan refinements) — slugged-child gap (cl-003); idempotent push helper (dp-004); server-side port of CMS auto-gen logic (dp-005); BROKEN-IN-CMS auto-detection methodology (rr-005); audit-first execution discipline; harness with CMS state inspection

**Referenced by:**
- `dotcompb-7929-json-ld-non-pdp.md` (2026-04-29 through 2026-04-30 evening)

**Knowledge categories:**
- Architecture Decisions: 6 entries (1 superseded)
- Design Patterns: 5 entries (dp-001 through dp-005)
- Shared State & Data Flow: 0 entries
- Constraints & Limitations: 3 entries (cl-001, cl-002, cl-003)
- Reusable References: 5 entries (rr-001 through rr-005)

---

## 2. Architecture Decisions

### ad-001: pug-skeleton-jsonld-injection-pattern
**Date:** 2026-04-29 | **Status:** SUPERSEDED by ad-005 (2026-04-30) — DO NOT USE
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

> **2026-04-30 update:** Live DB audit discovered the unified `additionalScripts → header.scripts → vue-layout-ssr.pug:24-26 each scriptDefinition` pipeline already emits ALL JSON-LD in production (15 URIs). Adding new per-schema Pug `if` blocks would have been unnecessary indirection. See ad-005 for the corrected mechanism. This entry kept for traceability — do not implement it.

**Context:** Need to inject `<script type="application/ld+json">` blocks into the `<head>` of MR pages for crawler visibility. Existing PDPs build the data server-side but inject the script tag client-side via `document.head.appendChild` in `website/src/vuescripts/utilities/productJsonLdUtils.js:31-36` — this is invisible to `curl` and to crawlers that don't execute JS reliably.

**Decision:** Use Pug-skeleton `if content.<schemaName>JsonLd` blocks at `website/src/views/desktop/vue-skeleton-ssr.pug:149+`. Each schema type gets one slot (`content.productJsonLd`, `content.faqPageJsonLd`, `content.breadcrumbListJsonLd`, etc.), populated by the route handler. The Pug skeleton emits `script(type="application/ld+json")!= JSON.stringify(content.<slot>)` for each populated slot.

**Existing emission (line 149-150):**
```pug
if content && content.productJsonLd
  script#pdp-json-ld(type="application/ld+json")!= JSON.stringify(content.productJsonLd)
```

**Pattern to follow for new schemas:**
```pug
if content && content.faqPageJsonLd
  script#pdp-faq-json-ld(type="application/ld+json")!= JSON.stringify(content.faqPageJsonLd)
if content && content.breadcrumbListJsonLd
  script#breadcrumb-list-json-ld(type="application/ld+json")!= JSON.stringify(content.breadcrumbListJsonLd)
```

**Alternatives considered:**
- `res.locals.jsonLdScripts` array indirection — rejected: Pug already handles per-schema `if` blocks natively; the array adds ceremony without value.
- Vue head plugin (`@unhead/vue` or similar) — rejected: Vue components don't own `<head>` here; Pug does. Adds a dependency without a need.
- CMS `additionalScripts` for everything — partially used (see ad-002) but not for route-handler-driven pages, since they need request-scoped data that CMS-time configuration can't see.

**Consequences:**
- All route-handler-driven pages (PDP, HCB Location V2, VideoChat, Shade Shop, future ticket types) inject schemas via the same mechanism.
- Any new schema type added to MR follows the same pattern: pick a slot name (`content.<schemaName>JsonLd`), populate it server-side, add a Pug `if` block.
- Sibling skeletons (mobile, non-SSR variants) must mirror the same `if` blocks — search for `if content && content.productJsonLd` to find them all.
- Client-side `updateFaqPageJsonLdScript()` becomes obsolete; convert to no-op + dedup defensive helper.

---

### ad-002: route-handler-vs-cms-additionalscripts-split
**Date:** 2026-04-29 | **Status:** active (validated)
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Context:** Two distinct page categories need structured data: (a) route-handler-driven pages (PDP, HCB Location V2, VideoChat, Shade Shop) where schema is built from request-scoped data (route params, `cmsSettings.faqs`, `req.metaData`), and (b) pure CMS-driven pages (some `/help` sub-pages, blog posts, marketing pages) where schema can be configured per-template-instance in CMS without code involvement.

**Decision:** Split mechanism by page category.
- **Route-handler-driven pages:** route handler computes schema from request data, sets `content.<slot>JsonLd`, Pug skeleton emits via ad-001 pattern.
- **CMS-driven pages without route-handler customization:** use existing `mr_modules/cms/lib/scriptsUtils.js:30-79` mechanism — CMS template configures `additionalScripts: [{ type: 'ld+json', text: <stringified JSON> }]`. `scriptsUtils` compiles to a Pug `script(type="application/ld+json")` block emitted into `headerScripts` (head) or `bodyScripts`.

**Alternatives considered:**
- One mechanism for everything — rejected: route-handler pages need request-scoped schema (e.g., breadcrumb names from route params, FAQ data from `req.metaData`) that CMS-time configuration can't supply.
- Always go through CMS `additionalScripts` — rejected: requires duplicating route-handler schema logic into CMS configuration UI; brittle and hard to test.

**Consequences:**
- New ticket: identify whether the affected page is route-handler-driven or pure CMS. Route-handler → set `content.<slot>JsonLd`. Pure CMS → configure `additionalScripts`.
- The two mechanisms can coexist on the same page (e.g., a CMS-driven page could carry both an `additionalScripts`-configured schema AND a route-handler-injected one if needed).
- `scriptsUtils.js:30-79` stays unchanged — it already supports `type="ld+json"`.

**Empirical validation (2026-04-29):** `/colorbar/locations` directory page emits a `FAQPage` JSON-LD block in raw HTML via the CMS `additionalScripts` path — proven by `curl -sS http://localhost:3000/colorbar/locations | grep -A 5 'application/ld+json'`. This is the canonical reference example: the CMS template for this URL has the FAQPage schema configured under `additionalScripts: [{type: 'ld+json', text: ...}]`. Inspect this template in Tophat to model Phase 3.4 implementations.

---

### ad-003: faq-answer-html-allowlist
**Date:** 2026-04-29 | **Status:** active
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Context:** CMS FAQ answers contain rich HTML (links, lists, bold, paragraphs). The existing `buildFaqPageJsonLd` in `mr_modules/cms/lib/productJsonLd.js:794` uses a blanket `stripTags()` that removes ALL HTML — losing meaningful semantic markup. schema.org `Answer.text` allows safe HTML but disallows scripts and event handlers.

**Decision:** Implement an allowlist sanitizer at `mr_modules/cms/lib/jsonLd/sanitize.js` exporting `sanitizeFaqAnswerHtml(html)`. Allowlist: `a` (with `href`), `br`, `ol`, `ul`, `li`, `p`, `strong`, `em`. Strip everything else (including `<script>`, `<style>`, `<iframe>`, event-handler attributes like `onclick=`).

**Alternatives considered:**
- Keep blanket `stripTags()` — rejected: loses meaningful HTML in answers; degrades search experience.
- Allow ALL HTML — rejected: security risk (XSS via `<script>` or event handlers), schema.org spec discourages.
- Use `DOMPurify` — possible if added; otherwise use existing `sanitize-html` if already in `package.json`. Search before adding new dependency.

**Consequences:**
- Single source of truth for the allowlist — never inlined into the schema builder.
- `Question.name` separately uses full `stripTags()` (plain text per schema.org spec).
- New question/answer-bearing schemas (e.g., HowTo `step.text`) can reuse the same sanitizer or define their own allowlist module by the same pattern.

---

### ad-005: unified-additionalscripts-pipeline (SUPERSEDES ad-001)
**Date:** 2026-04-30 | **Status:** active (validated against production DB + raw HTML curl on 15 URIs)
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Context:** Earlier ad-001 proposed adding new Pug `if content.<schema>JsonLd` blocks at `website/src/views/desktop/vue-skeleton-ssr.pug:149-150` for each new schema type. Live MongoDB audit on 2026-04-30 found that EVERY production JSON-LD block already flows through a different, unified pipeline:

```
cms.contentVersion.renderOptions.additionalScripts[]
    ↓
mr_modules/cms/lib/scriptsUtils.js:addScriptsDefsInto()  (compiles each through Pug → _htmlScript)
    ↓
locals.header.scripts[]
    ↓
website/src/views/desktop/vue-layout-ssr.pug:24-26
website/src/views/desktop/vue-layout.pug:24-26
website/src/views/desktop/vue-layout-no-header-footer.pug:24-26
    each scriptDefinition in header.scripts
      | !{scriptDefinition._htmlScript}
    ↓
<script type="application/ld+json">…</script> in <head>
```

The line 149-150 `productJsonLd` block exists too, but it's a hardcoded shortcut for ONE schema — not a pattern to extend. All 15 URIs with JSON-LD in production use the unified pipeline (verified 2026-04-30 via live DB query).

**Decision:** All new structured-data injection — auto-generated, hand-authored, or route-handler-pushed — flows through `content.renderOptions.additionalScripts[]`. Do NOT add new per-schema Pug `if` blocks. Three writers may produce entries:

1. **Tophat auto-gen at save** (see dp-002) — for FAQPage from any CMS content with a `componentList` containing a flagged FAQ component.
2. **Hand-authored Pug-interpolated** (see dp-003) — `additionalScripts: [{type:'ld+json', forceInterpolation:true, text:'<Pug src with #{metaData.x}>'}]` for per-URL dynamic schemas.
3. **Route-handler push** (see ad-006) — code calls `pushJsonLdToContent(content, schema, metadata)` after CMS content load and before `htmlRenderer.renderContent`.

**Alternatives rejected:**
- New per-schema Pug `if` blocks (ad-001 v1) — strictly worse: duplicates the existing `each scriptDefinition` iterator; requires a new slot per schema type forever.
- `res.locals.jsonLdScripts` array indirection — same problem.
- Vue head plugin (`@unhead/vue`) — adds a dependency; `<head>` is owned by Pug here.

**Empirical evidence (2026-04-30):**
- 15 URIs have `renderOptions.additionalScripts[].type === 'ld+json'`
- 14 use Tophat auto-gen (`metadata: {type:'faq'}`, `generatedAutomatically: true`)
- 1 uses Pug-interpolated (`/colorbar/locations/` for HairSalon/Review per location, with `forceInterpolation: true`)
- All emit raw HTML correctly — verified via `curl … | grep -c 'application/ld+json'`

**Consequences:**
- Adding new schema types (Article, LocalBusiness, Organization, etc.) requires zero pipeline changes — only a new builder + a writer.
- One mechanism, one debugging surface, one place to look in the DB (`renderOptions.additionalScripts`).
- ad-001 reference in any prior plan is stale — replace with ad-005.

---

### ad-006: route-handler-push-helper (pushJsonLdToContent)
**Date:** 2026-04-30 | **Status:** active (planned for DOTCOMPB-7929 Phase 2)
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Context:** Route handlers and middleware that need to inject JSON-LD must lazy-create `content.renderOptions` and `content.renderOptions.additionalScripts`, then push a properly-shaped entry. Without a helper, every call site repeats the same 5-line boilerplate.

**Decision:** Add a small helper at `mr_modules/cms/lib/jsonLd/index.js`:

```js
exports.pushJsonLdToContent = function pushJsonLdToContent(content, schemaObject, metadata) {
  if (!schemaObject) {
    return;
  }
  content.renderOptions = content.renderOptions || {};
  content.renderOptions.additionalScripts = content.renderOptions.additionalScripts || [];
  content.renderOptions.additionalScripts.push({
    type: 'ld+json',
    inHeader: true,
    isUrl: false,
    text: JSON.stringify(schemaObject),
    metadata: metadata || {},
  });
};
```

**Critical flag invariant:** NEVER set `generatedAutomatically: true` on entries pushed from code. That flag is owned by Tophat's auto-gen path (`tophat/src/ngscripts/cms/ContentEditCtl.js:861`) which wipes ALL such entries on the next CMS editor save. Code-pushed entries with that flag would be silently deleted whenever an editor re-saves the page.

**Consumers:**
- `mr_modules/cms/lib/router.js:462` (PDP — replaces `content.faqPageJsonLd = …` slot)
- `mr_modules/cms/lib/jsonLd/routeScoped.js` (new — Shade Shop & Shop Category breadcrumbs)
- Future schema types

**Alternatives considered:**
- Inline boilerplate at every call site — rejected: 5 lines × N call sites = drift risk.
- Setter on `content` (e.g., `content.addJsonLd(schema)`) — rejected: would require changing the `content` object's shape across the entire CMS pipeline.

**Consequences:**
- One canonical writer for code-driven schemas. Updates to entry shape (e.g., adding a new field) happen in one place.

---

### ad-004: schema-builders-in-jsonld-subdir
**Date:** 2026-04-29 | **Status:** active
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Context:** MR has one existing schema builder file: `mr_modules/cms/lib/productJsonLd.js` (816 lines, mixes Product, Review, Offer, FAQPage, BuyAction, ProductGroup builders). Adding more schema builders (BreadcrumbList, future Article/HowTo/etc.) directly in this file makes it sprawl. Adding them as separate top-level files in `mr_modules/cms/lib/` clutters that directory.

**Decision:** Create `mr_modules/cms/lib/jsonLd/` subdirectory with one file per schema concern:
- `sanitize.js` — `sanitizeFaqAnswerHtml(html)`
- `faqPage.js` — `buildFaqPageJsonLd({ faqs, pageUrl, about })` (generalized)
- `breadcrumbList.js` — `buildBreadcrumbListJsonLd({ items })`
- `index.js` — barrel re-exports

`mr_modules/cms/lib/productJsonLd.js` keeps its product-specific schemas (Product, Review, Offer, ProductGroup, BuyAction) and re-exports `buildFaqPageJsonLd` from `jsonLd/` for backward compatibility (callers in `router.js:462` and `productCatalog.js:1995` unchanged).

**Alternatives considered:**
- Extend `productJsonLd.js` to ~1500 lines — rejected: file becomes unreadable; mixes concerns.
- Flat top-level files (`mr_modules/cms/lib/faqPage.js`, etc.) — rejected: clutters `cms/lib/` (already 30+ files).
- Move EVERYTHING (incl Product) to `jsonLd/` — possible but increases the diff size of the ticket; defer.

**Consequences:**
- New schema types follow the pattern: add `{schemaType}.js` to `jsonLd/`, re-export from `index.js`, called by route handler.
- Tests co-located: `jsonLd/{schemaType}.test.js`.
- `productJsonLd.js` shrinks over time as legacy code is migrated to `jsonLd/`.

---

## 3. Design Patterns

### dp-001: pure-schema-builder-functions
**Date:** 2026-04-29 | **Status:** active
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Name:** Pure Schema Builder Functions

**When to use:** Any new schema.org JSON-LD type that needs to be emitted from MR pages.

**Structure:**
- One file per schema type at `mr_modules/cms/lib/jsonLd/{schemaType}.js`.
- One exported function per schema, named `build{SchemaType}JsonLd({ ...inputs })`.
- Inputs are plain data (arrays, strings, numbers) — no `req`, no globals, no I/O.
- Returns the JSON object on success, or `null` when content is empty/insufficient.
- Sanitization is delegated to the dedicated sanitizer module (e.g., `sanitize.js`).
- All builders are pure: same inputs → same output, no side effects.

**Why:**
- **Testable:** unit tests pass plain data, assert returned object shape.
- **Composable:** route handlers call the builder, set `content.<slot>JsonLd`, Pug emits.
- **Reusable across pages:** the same `buildFaqPageJsonLd` works for PDP, HCB, VideoChat, CMS pages.
- **No coupling to request lifecycle:** builders don't need `req`, so they can run in tests, scripts, batch jobs, etc.

**Validated in:**
- `mr_modules/cms/lib/productJsonLd.js:774-816` (`buildFaqPageJsonLd` — current implementation, will be generalized).
- Phase 1 of DOTCOMPB-7929 will validate by introducing `breadcrumbList.js`, `sanitize.js`.

**Anti-pattern:** Schema generation inline in components or inside Express middleware. This couples schema logic to render lifecycle (untestable in isolation), and duplicates logic across pages.

---

### dp-002: tophat-auto-gen-via-addfaqmetadata-flag
**Date:** 2026-04-30 | **Status:** active (in production for 14/15 JSON-LD-bearing URIs)
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Name:** Tophat FAQPage auto-generation at CMS save time

**When to use:** Any CMS-driven page whose template has a `componentList` field, where FAQ data is editor-authorable (questions and answers typed directly into Tophat by content editors).

**How it works:**

`tophat/src/ngscripts/cms/ContentEditCtl.js:1377-1582` runs at SAVE time, BEFORE the REST call that persists the doc:

1. `saveContent()` (line 833) iterates every variation of the content.
2. **Dedup pass** (line 861): removes ALL existing entries with `generatedAutomatically: true`. Re-saves never accumulate duplicates.
3. **Detection** (line 1418-1423): iterates `content.templateData.componentList`. For each component with `settings.addFaqMetadata === true`, extracts FAQ data.
4. **Three input shapes supported** (lines 1426-1463):
   - Default: `settings.questions[]` + `settings.answers[]` (parallel arrays). Used by `how-to-question-accordion-vue` (10/10 production cases).
   - Mapped: `settings.faqMetadataFields: {questions: 'q', answers: 'a'}` overrides the field names. **0 production usage.**
   - HCB nested: `settings.faqs: [{question: {mainQuestion}, answer}]`. Used by `hcb-location-page` (1/1).
   - Blog: `mixin_key === 'post-faq'` + `settings.faq[]`. Used by 11 blog posts.
5. **Output** (lines 1476-1486): pushes one entry into `variation.renderOptions.additionalScripts` with the canonical shape (see rr-003).

**Editor recipe (zero code):**
1. Open Tophat → CMS → Content → search target URI → Components tab
2. Add component `how-to-question-accordion-vue` (or any FAQ-bearing component)
3. Fill `questions[]` and `answers[]` (parallel arrays, equal length, HTML allowed in answers)
4. Tick `addFaqMetadata: true`
5. Save. Done — JSON-LD is baked into the doc.

**Critical caveat:** Auto-gen runs ONLY on save. Pre-existing content docs need to be re-saved by an editor before the FAQ script appears. **No retroactive generation server-side.**

**Anti-pattern:** Hand-writing JSON-LD in Tophat's Advanced Config → Additional Scripts. Editors should never see/touch the raw JSON — let auto-gen own it. Any hand-authored entry survives because it lacks `generatedAutomatically: true`, but that's an escape hatch for dp-003 (Pug-interpolated dynamic schemas), not for plain FAQ data.

**Validated routes (2026-04-30):** `/colorbar/locations`, `/colorbar/locations/`, `/hair-color-chart`, `/how-to-color-your-hair`, `/how-to-highlight`, 11 `/blog/*` posts.

---

### dp-003: pug-interpolated-additionalscripts-for-dynamic-schemas
**Date:** 2026-04-30 | **Status:** active (1 production usage — `/colorbar/locations/`)
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Name:** Per-URL dynamic JSON-LD via Pug-interpolated CMS additionalScripts

**When to use:** A page needs JSON-LD whose values come from request-scoped data (URL params, location lookup, product lookup) BUT the schema authoring should still live in CMS for editor visibility, NOT in code.

**How it works:**

`mr_modules/cms/lib/scriptsUtils.js:71-79` builds the Pug source for each `additionalScripts` entry. By default the Pug source ends with a trailing `.` which makes the body literal text. **If the entry has `forceInterpolation: true`, the trailing `.` is omitted** — Pug then interpolates `#{var}` references against the request-scoped locals (see line 134: `pug.compile(...)(locals)`).

**Example (verbatim from `/colorbar/locations/` content_id 2350):**

```pug
script(type="application/ld+json")
  | {
  |   "@context": "https://schema.org",
  |   "@type": "HairSalon",
  |   "name": "#{metaData.name}",
  |   "geo": {
  |     "@type": "GeoCoordinates",
  |     "latitude": #{metaData.geo.lat},
  |     "longitude": #{metaData.geo.lon}
  |   },
  |   "review": [...]
  | }
```

The Express middleware at `website/src/routing/views.js:1593-1607` populates `req.metaData` from `colorbarCache.getLocation(urlKey)`. By the time `scriptsUtils` compiles the Pug, `metaData.name`, `metaData.geo.lat`, etc. are populated — different per `urlKey`, all from one CMS-authored template.

**Editor recipe:**
1. In Tophat, open the content with parameterized routing.
2. Advanced Config → Additional Scripts → New entry.
3. Type: `ld+json`. `forceInterpolation: true`. `inHeader: true`.
4. Paste Pug source with `#{metaData.x}` references.
5. Add an Express middleware (CODE side) to populate `req.metaData.x` per request.

**When NOT to use:**
- Schema is fully static → use dp-002 (auto-gen).
- Data is ONLY available in code (e.g., from a Magento product lookup that CMS can't see) → use ad-006 helper instead.
- Editors don't need to see/edit the schema → use ad-006.

**Anti-pattern:** Combining `forceInterpolation: true` with editor-typed natural-language fields. Pug interpolation runs on EVERY field, so an editor typing `#{name}` in a description field would crash the render. Reserve this pattern for schema-only entries authored by developers.

---

## 4. Shared State & Data Flow

*(no entries yet — schema-related state is request-scoped, not application-scoped)*

---

## 5. Constraints & Limitations

### cl-001: hcb-location-v2-faq-data-not-in-vuex
**Date:** 2026-04-29 | **Status:** active
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Scope:** HCB Location V2 page (`/colorbar/locations/:urlKey`) and its FAQ schema generation.

**Constraint:** The FAQ data rendered by `<HairColorBarLocationFAQs>` on `HcbLocationPageV2.vue` is NOT stored in Vuex and is NOT fetched in `serverPrefetch`. It is only available at server-render time via `req.metaData` → `cmsSettings.faqs` prop. Client-side code paths cannot access it directly.

**Cause:** `colorbarCache.getLocation(urlKey)` runs in the Express route handler and populates `req.metaData`. The route handler then calls `res.render(template, context)`, passing `cmsSettings` (which includes `faqs`) as a prop. There is no Vuex `colorbar` module action that mirrors this fetch.

**Workaround:** Schema generation MUST run in the route handler (or a function called by the route handler), using the same `req.metaData` data. Cannot lazy-load schema on client.

**Impact:** If you try to generate schema in a Vue component lifecycle hook (`mounted`, `serverPrefetch`), the FAQ data will be `undefined` and the schema will be empty/missing. Always generate in the route handler and inject via `content.faqPageJsonLd`.

**Validation:** Live test 2026-04-29 — `curl http://localhost:3000/colorbar/locations/towson` returns valid `HairSalon` JSON-LD (server-side built) and renders FAQ HTML, but no Vuex `colorbar` action populates `faqs` to the store. Component receives via prop only.

---

### cl-002: specific-shop-product-config-template-lacks-componentlist
**Date:** 2026-04-30 | **Status:** active
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Scope:** All `/shop/*` URLs — both the 4 Shade Shop PLPs (`/shop/{brown,blonde,red,black}`) and shop category pages (`/shop/all-hair-color`, `/shop/permanent-color`, `/shop/men`, `/shop/gloss`, `/shop/hair-care`, `/shop/root-touch-up-kit`, `/shop/color-reviving-gloss`).

**Constraint:** All `/shop/*` URLs route through a single CMS content document at `/shop/` (with trailing slash) that uses `takesUrlParameters: true` (5 keys `keyA..keyE`) and `templateKey: 'specific-shop-product-config'` (template_id 1327). This template's `config[]` has only 4 fixed fields:
- `showCategoryHeader: boolean`
- `topCustomComponents: component`
- `belowCategoryHeaderComponents: component`
- `bottomCustomComponents: component`

**No `componentList` field exists.** Editors cannot add a `how-to-question-accordion-vue` (or any other FAQ-bearing component) to drive Tophat's auto-gen pipeline (dp-002). The `additionalScripts` array is also not surfaced in this template's Tophat editor UI.

**Cause:** The Shade Shop redesign (DOTCOMPB-7466, PR #20512) introduced this template specifically for shade pages — it's a tailored shape, not a generic component list.

**Workaround:** Use ad-006 (route-handler push) — Express middleware runs after CMS content load, computes the schema from request params (`/shop/:slug` → label lookup), and pushes a `BreadcrumbList` entry onto `content.renderOptions.additionalScripts` via `pushJsonLdToContent`. No Tophat schema change required, no editor work required.

**Validation:** Verified 2026-04-30 via live MongoDB query of the `cms.template` collection (template_id 1327) and `cms.contentVersion` for `/shop/`.

**Impact on DOTCOMPB-7945 (Shade Shop BreadcrumbList bug):** This is the entire reason the bug exists. The fix CANNOT be a Tophat-side change alone — code must run.

---

## 6. Reusable References

### rr-001: faqpage-jsonld-shape
**Date:** 2026-04-29
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Name:** Canonical `FAQPage` JSON-LD shape

**Source:** schema.org/FAQPage spec (https://schema.org/FAQPage) + Google Rich Results requirements.

**Shape:**
```json
{
  "@context": "https://schema.org",
  "@type": "FAQPage",
  "mainEntity": [
    {
      "@type": "Question",
      "name": "Question text — plain text, no HTML",
      "acceptedAnswer": {
        "@type": "Answer",
        "text": "Answer text — sanitized HTML from allowlist [a, br, ol, ul, li, p, strong, em]"
      }
    }
  ],
  "about": {
    "@id": "https://www.madison-reed.com/<page-url>#product"
  }
}
```

**Required keys:** `@context`, `@type`, `mainEntity` (non-empty array). Each `mainEntity` entry requires `@type`, `name`, `acceptedAnswer.@type`, `acceptedAnswer.text`.

**Optional keys:** `about` — links the FAQPage to a Product or other entity (used on PDPs, optional on non-PDP pages).

**Consumers:**
- `mr_modules/cms/lib/productJsonLd.js:774-816` — current `buildFaqPageJsonLd` (PDP-specific).
- Future `mr_modules/cms/lib/jsonLd/faqPage.js` — generalized builder.
- All non-PDP pages that need FAQPage emission (HCB Location V2, VideoChat, CMS-driven FAQ pages).

**Validation rules:**
- `mainEntity` must NOT be empty array (Rich Results flags empty).
- `Question.name` must be plain text (no HTML — full strip).
- `Answer.text` must contain only allowlisted tags (`a`, `br`, `ol`, `ul`, `li`, `p`, `strong`, `em`) + `href` attribute on `a`.
- No event handlers (`onclick=`, etc.). No `<script>`, `<style>`, `<iframe>`.

---

### rr-002: breadcrumblist-jsonld-shape
**Date:** 2026-04-29
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Name:** Canonical `BreadcrumbList` JSON-LD shape

**Source:** schema.org/BreadcrumbList spec (https://schema.org/BreadcrumbList) + Google Rich Results requirements.

**Shape:**
```json
{
  "@context": "https://schema.org",
  "@type": "BreadcrumbList",
  "itemListElement": [
    { "@type": "ListItem", "position": 1, "name": "Home",              "item": "https://www.madison-reed.com/" },
    { "@type": "ListItem", "position": 2, "name": "Shop All Products", "item": "https://www.madison-reed.com/shop-all" },
    { "@type": "ListItem", "position": 3, "name": "{Color} Shades",    "item": "https://www.madison-reed.com/shop/{color}" }
  ]
}
```

**Required keys:** `@context`, `@type`, `itemListElement` (non-empty array). Each `itemListElement` requires `@type: ListItem`, `position` (1-based, sequential, no gaps), `name` (plain text), `item` (absolute https URL).

**Validation rules:**
- `position` values must be 1, 2, 3, ... with no gaps.
- `name` must be plain text — no HTML.
- `item` must be an absolute https URL — no relative paths.
- `itemListElement` must NOT be empty.

**Consumers (planned, DOTCOMPB-7945):**
- Future `mr_modules/cms/lib/jsonLd/breadcrumbList.js` — `buildBreadcrumbListJsonLd({ items })`.
- Shade Shop PLP route handler (4 URLs: `/shop/{brown,blonde,red,black}`).
- Open: `/shop-all` (parent breadcrumb — F10.1 stakeholder question).
- Future: any page needing breadcrumb structured data.

**Note:** MR has an existing `Breadcrumbs.vue` component (`website/src/vuescripts/components/Breadcrumbs/Breadcrumbs.vue`) that emits microdata (`itemscope itemtype="https://schema.org/BreadcrumbList"`) — this is a separate mechanism from JSON-LD. Stakeholder decision (P0.5) determines whether to keep both or remove microdata where JSON-LD is added.

---

### rr-003: tophat-auto-gen-additionalscripts-entry-shape
**Date:** 2026-04-30
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Name:** Canonical Tophat-auto-generated `additionalScripts` entry shape

**Where stored:** `cms.contentVersion {content_id, version}.renderOptions.additionalScripts[]`

**Shape:**
```js
{
  type: 'ld+json',
  inHeader: true,
  isUrl: false,
  addBodyLoadScript: false,
  text: '<JSON.stringify(<schema object>)>',
  metadata: { type: 'faq' },          // or 'breadcrumb', etc. — type discriminator for filters
  generatedAutomatically: true        // ⚠️ flag-of-doom — see warning below
}
```

**Required keys (when writing):** `type`, `inHeader`, `text`. Other keys default to safe values in `scriptsUtils.js`.

**Optional keys:**
- `forceInterpolation` (boolean) — when `true`, the `text` is treated as Pug source and interpolated against request locals (see dp-003). Used ONLY for hand-authored dynamic schemas.
- `metadata.type` — informal discriminator used by Tophat UI to filter/dedup. Common values: `faq`, `breadcrumb`, `product`.

**⚠️ Critical: `generatedAutomatically: true` is destructive.**

`tophat/src/ngscripts/cms/ContentEditCtl.js:861` runs at every save:
```js
variation.renderOptions.additionalScripts =
  variation.renderOptions.additionalScripts.filter(as => !as.generatedAutomatically);
```
Any entry with this flag is wiped on the next editor save. Code-pushed entries (ad-006) MUST NOT set this flag — otherwise re-saves silently delete them.

**Consumers:**
- `mr_modules/cms/lib/scriptsUtils.js:addScriptsDefsInto()` reads these entries at render time.
- Tophat UI `ContentEditCtl.js:1377-1582` writes them at save time (auto-gen).
- `pushJsonLdToContent` helper (ad-006) writes them from route handlers.

**Validation rules:**
- `text` must be valid JSON (parseable with `JSON.parse`) when `forceInterpolation` is false/absent.
- `text` must be valid Pug source (compilable) when `forceInterpolation: true`.
- `inHeader: true` puts the script in `<head>`; `false` puts it before `</body>`. Always use `true` for JSON-LD.

---

### rr-004: cms-diagnostic-mongo-one-liner
**Date:** 2026-04-30
**Source:** `dotcompb-7929-json-ld-non-pdp.md`

**Name:** Canonical Mongo diagnostic for any CMS-driven URI

**Use:** Inspect what's currently configured for any URI on the local MR dev environment — without Tophat auth.

**Pre-req:** `mr-mongo` Docker container running (`docker ps | grep mr-mongo`). Database name is `cms` (NOT `madisonreed`).

**Command:**
```sh
docker exec mr-mongo mongosh cms --quiet --eval '
  const c = db.content.findOne({uri:"PUT_URI_HERE"});
  if (!c) { print("NOT FOUND"); quit(); }
  const cv = db.contentVersion.findOne({content_id:c._id, version:c.published_version});
  print("templateKey: " + cv.templateKey);
  print("isVueSsr: " + cv.isVueSsr);
  print("scripts count: " + (cv.renderOptions?.additionalScripts?.length || 0));
  print("scripts types: " + JSON.stringify((cv.renderOptions?.additionalScripts||[]).map(s => ({type:s.type, metadata:s.metadata, gen:s.generatedAutomatically, fi:s.forceInterpolation}))));
  print("components: " + (cv.templateData?.componentList||[]).map(x => x.mixin_key + (x.settings?.addFaqMetadata?" [FAQ✓]":"")).join(", "));
'
```

**Output keys:**
- `templateKey` — name of the template; cross-reference with `db.template.findOne({mixin_key: <key>})` to inspect.
- `isVueSsr` — `true` for SSR-rendered pages; `false` for legacy non-SSR.
- `scripts count` / `scripts types` — what's in `additionalScripts`. Quick diagnostic for ad-005 / dp-002 / dp-003.
- `components` — what's in `templateData.componentList`. Components flagged `addFaqMetadata: true` get the `[FAQ✓]` annotation.

**Companion query — list all routes with JSON-LD today:**
```sh
docker exec mr-mongo mongosh cms --quiet --eval '
  db.contentVersion.aggregate([
    {$match:{"renderOptions.additionalScripts.type":"ld+json"}},
    {$lookup:{from:"content", localField:"content_id", foreignField:"_id", as:"c"}},
    {$project:{uri:{$arrayElemAt:["$c.uri",0]}, scripts:"$renderOptions.additionalScripts"}}
  ]).forEach(d => print(d.uri + " → " + d.scripts.map(s => s.metadata?.type || s.type).join(",")));
'
```

**When to run:**
- Before planning any per-route work: confirms current state.
- After a Tophat editor save: confirms auto-gen wrote the entry.
- After a code change: confirms `pushJsonLdToContent` ran.
- Whenever a curl-grep mismatches DB state: localizes the bug to render path vs storage.

**Consumers:**
- DOTCOMPB-7929 Phase 0 discovery (P0.6).
- DOTCOMPB-7929 Phase 3 verification (per-task DB check).
- Any future SEO ticket touching CMS structured data.


---

## (NEW v3 ENTRIES — added 2026-04-30 evening from DOTCOMPB-7929 v8-v13 refinement)

### dp-004: idempotent-route-handler-push
**Date:** 2026-04-30 | **Status:** active
**Source:** `dotcompb-7929-json-ld-non-pdp.md` (v10 plan refinement)

**Context:** When code-bypassing a CMS-pipeline gap (e.g., F23 / cl-003 slugged-child gap), the route-handler push must coexist with the existing CMS auto-gen path. If both emit the same `@type`, the page gets duplicate FAQPage blocks (Google warns / picks one randomly). If the route-handler push runs unconditionally, it can overwrite editor-driven content.

**Decision:** `pushJsonLdToContent(content, schemaObject, metadata)` is **idempotent** — it skips when an entry of the same `@type` (same schema) is already present in `content.renderOptions.additionalScripts[]`. Implementation:

```javascript
exports.pushJsonLdToContent = function pushJsonLdToContent(content, schemaObject, metadata) {
  if (!schemaObject) {
    return; // ad-006 / AC6 — empty input must not emit
  }
  const newType = schemaObject['@type'];
  content.renderOptions = content.renderOptions || {};
  content.renderOptions.additionalScripts = content.renderOptions.additionalScripts || [];
  // Idempotency check: skip if same @type already in array
  const exists = content.renderOptions.additionalScripts.some((s) => {
    if (s.type !== 'ld+json' || !s.text) {
      return false;
    }
    try {
      const parsed = JSON.parse(s.text);
      return parsed['@type'] === newType;
    } catch (err) {
      return false; // malformed CMS entry — push anyway
    }
  });
  if (exists) {
    return;
  }
  content.renderOptions.additionalScripts.push({
    type: 'ld+json',
    inHeader: true,
    isUrl: false,
    text: JSON.stringify(schemaObject),
    metadata: metadata || {},
    // generatedAutomatically intentionally omitted — see ad-006
  });
};
```

**When to use:**
- Route-handler push co-exists with editor-driven CMS scripts (e.g., HCB Location detail: HairSalon hand-authored + FAQPage R3 push)
- Multiple route handlers / middleware might run on the same request (e.g., shared `productRouter` covering `/product/:slug` and `/bundle/perfect-pair/:slug`)
- Verifying defenses against double-emission

**Anti-patterns:**
- Don't add `forceInterpolation: true` defensively — code-built schemas are already final JSON, never Pug source.
- Don't set `generatedAutomatically: true` — that's reserved for Tophat auto-gen and triggers wipe on next editor save (ad-006).
- Don't replace existing entries — only skip. Overwriting destroys editor-driven content silently.

**Consequences:**
- Existing R1 emissions on `/colorbar/locations` directory + 11 blog posts continue working post-deploy.
- Slugged children of `/colorbar/locations/` (where R1 silently drops FAQPage per cl-003) get the FAQPage from the route-handler push.
- Future code-handlers can push without checking what CMS already has — the helper handles deduplication.

---

### dp-005: server-side-port-of-cms-auto-gen
**Date:** 2026-04-30 | **Status:** active
**Source:** `dotcompb-7929-json-ld-non-pdp.md` (v10 P1.8 + P3.4)

**Context:** The Tophat auto-gen path (dp-002) reads `content.templateData.componentList[]` looking for components with `settings.addFaqMetadata === true` and produces a FAQPage entry at SAVE time. When the runtime pipeline silently drops that entry (e.g., cl-003 slugged-child gap), we need to emit the same schema from server-side code without losing the editor-authored data semantics.

**Decision:** Port the data-extraction logic of `tophat/src/ngscripts/cms/ContentEditCtl.js:getFAQsFromQuestionsAnswersPair` (lines 1413-1463) to a server-side module:

```javascript
// mr_modules/cms/lib/jsonLd/extractFaqs.js
exports.extractFaqsFromContent = function extractFaqsFromContent(content) {
  if (!content || !content.templateData || !Array.isArray(content.templateData.componentList)) {
    return [];
  }
  const faqs = [];
  content.templateData.componentList.forEach(({settings, mixin_key}) => {
    if (!settings || !settings.addFaqMetadata) {
      return;
    }
    const fields = {
      questions: settings.faqMetadataFields?.questions || 'questions',
      answers: settings.faqMetadataFields?.answers || 'answers',
    };
    let questions = settings[fields.questions];
    let answers = settings[fields.answers];
    // HCB shape: { faqs: [{ question: { mainQuestion }, answer }, ...] }
    if (!questions && Array.isArray(settings.faqs)) {
      questions = settings.faqs.map((f) => f.question.mainQuestion);
      answers = settings.faqs.map((f) => f.answer);
    }
    if (!Array.isArray(questions) || !Array.isArray(answers)) {
      return;
    }
    if (questions.length !== answers.length) {
      throw new Error(`[extractFaqs] component ${mixin_key} has ${questions.length} questions but ${answers.length} answers`);
    }
    questions.forEach((q, i) => {
      faqs.push({question: q, answer: answers[i] || ''});
    });
  });
  return faqs;
};
```

**Why port instead of fix the pipeline:**
- The pipeline issue (cl-003) is in shared code (`scriptsUtils.js` + `htmlRenderer.js`); a fix has site-wide blast radius.
- The server-side port is additive — it doesn't touch existing emissions.
- The CMS data structure is stable (`componentList` + `addFaqMetadata` flag have been live in production for years).
- Same source of truth: editor edits ONE place (Tophat component), code reads SAME place at runtime.

**When to apply this pattern (general):**
- CMS-driven pipeline drops content silently and root cause investigation has high cost.
- The CMS data structure is stable and well-defined.
- The R3 push is straightforward (route handler exists, content is loaded before render).

**When NOT to apply:**
- The CMS data isn't stable (frequent shape changes) — port creates duplication that drifts.
- The CMS pipeline gap affects content editors directly (e.g., they save and don't see the change in preview) — fix the pipeline.
- The schema needs data not in `componentList` (e.g., Magento product data) — that's pure R3, no port needed.

**Consequences:**
- HCB Location detail (cl-003) fixed via P3.4 without touching shared code.
- Pattern can be reused for `/schedule-video-chat` v2 work — port `getFAQsFromBlogPosts` similarly if needed.
- Editor UX unchanged: editor still ticks `addFaqMetadata` and Tophat auto-gen fires for parent contexts; server-side code emits for slugged contexts.

---

### cl-003: slugged-child-static-script-gap
**Date:** 2026-04-30 | **Status:** active (workaround via dp-004 + dp-005)
**Source:** `dotcompb-7929-json-ld-non-pdp.md` (F23, v7 finding)

**Constraint:** For CMS content with `takesUrlParameters: true`, `additionalScripts[]` entries with `forceInterpolation: false` (static, non-Pug) are **silently dropped** from raw HTML on slugged children, while `forceInterpolation: true` entries (Pug-driven) emit correctly.

**Verified scope:**
- `/colorbar/locations/` (content_id 2350, `takesUrlParameters: true`) has 2 entries: [0] HairSalon Pug `forceInterpolation: true` (emits), [1] FAQPage static auto-gen `forceInterpolation: false` (DROPPED).
- Reproduces consistently across all slugged children (`/towson`, `/hillsboro`, `/pembrokepines`, `/bethesda`, etc. — ~85+ HCB locations).
- Cache-bust query params don't change behavior — not a cache fluke.
- Parent directory `/colorbar/locations` (content_id 2349, NO `takesUrlParameters`) emits its FAQPage correctly.

**Cause candidates (none verified):**
1. Silent Pug compile error in `scriptsUtils.js:134-142` (try/catch swallows `pug.compile()` errors → `_htmlScript = ''`). The static FAQPage text contains escaped HTML in answer fields (`<a href=\"...\">`) which might confuse Pug block-text mode.
2. Cache layer staleness — `getHtmlFromCache` cache key may pre-date the FAQPage being added to content_id 2350.
3. Render-path bug specific to `takesUrlParameters: true` — `addScriptsDefsInto` may run differently for slugged children.

**Workaround (chosen for DOTCOMPB-7929):**
- Don't fix the pipeline (high blast radius — touches shared `htmlRenderer.js` / `scriptsUtils.js`).
- Use dp-005 (server-side port) + dp-004 (idempotent push) to emit FAQPage from the HCB Location detail route handler.
- Pipeline gap remains; future investigation can fix it without breaking the workaround.

**Diagnostic command:**
```sh
DEBUG=cms.scriptsUtils npm run dev-ssr  # surface silent Pug compile errors
curl -sS http://localhost:3000/colorbar/locations/towson | grep -c FAQPage  # baseline
```

**Consequences:**
- Any future Tophat content with `takesUrlParameters: true` + static `additionalScripts` entries hits this gap.
- Editor experience: editor saves FAQPage in CMS, parent page emits, slugged children don't — confusing without this documentation.
- v2 follow-up could investigate root cause; not blocking v1 ship.

---

### rr-005: broken-in-cms-detection-methodology
**Date:** 2026-04-30 | **Status:** active
**Source:** `dotcompb-7929-json-ld-non-pdp.md` (v13 harness `cms-check.mjs`)

**Methodology:** Auto-classify URLs into `BROKEN-IN-CMS` (Tophat configured but pipeline drops it) vs `MISSING` (Tophat not configured, code needed) by comparing CMS state to raw HTML.

**Implementation:** Two-pass audit harness.

**Pass 1 — CMS state cache** (`cms-check.mjs`):
```javascript
// For each URI in urls.txt:
// 1. Try exact db.content.findOne({uri})
// 2. Fallback: db.content.findOne({uri: uri + '/', takesUrlParameters: true})
// 3. Walk up path: /a/b/c → try /a/b/, /a/ with takesUrlParameters: true
// 4. Once content resolved, fetch contentVersion + templateVersion
// 5. Summarize all additionalScripts at content + template levels
//    → { level, type, inHeader, forceInterpolation, generatedAutomatically, schemaTypes, textBytes }
// Output: cms-state-cache.json keyed by URI
```

**Pass 2 — HTML audit** (`check.mjs`):
```javascript
// For each URI:
// 1. Fetch raw HTML, extract <script type="application/ld+json"> blocks
// 2. Parse + collect schema @types
// 3. Compare against expected schema types (from urls.txt)
// 4. Classify:
//    - PASS: all expected types present, no validity issues
//    - MISSING + cmsCache shows expected type in additionalScripts → BROKEN-IN-CMS
//    - MISSING + cmsCache shows nothing relevant → MISSING (code-only fix needed)
//    - INVALID: types present but malformed (trailing commas, multi-object concatenation, etc.)
//    - NOT-A-CANDIDATE: FAQPage expected but no FAQ content in raw HTML (per Schema.org policy)
```

**Reusable for:**
- Any future SEO/structured-data ticket needing to classify URLs by Tophat-configured vs code-only.
- Validating Tophat editor changes (re-run audit after save → status flips MISSING → PASS).
- Detecting pipeline bugs site-wide (BROKEN-IN-CMS appearing on URLs we expect to work).

**Implementation file:** `.tasks/seo-jsonld-check/cms-check.mjs` (DOTCOMPB-7929 PR; reuse from there for future tickets).

**Limitations:**
- Requires `docker exec mr-mongo mongosh cms` to be available (local dev or admin access).
- URI parent-fallback assumes the standard MR convention (parameterized parent has trailing slash + `takesUrlParameters: true`). Won't resolve URIs that map to PDPs (Magento product router) or non-CMS routes.
- Doesn't follow `templateRefs` cross-references — assumes content's own `additionalScripts` is the full set.

