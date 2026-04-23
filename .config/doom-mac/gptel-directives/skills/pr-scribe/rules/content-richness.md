---
title: Content Richness — Universal Content Quality Standards Independent of Brand Layout
impact: HIGH
impactDescription: Content richness (status tags, individual test rows, detailed expected labels) was previously locked inside brand-specific rules. This file extracts them as universal content quality standards any brand can adopt, separating the "how detailed is each entry" axis from the "what shape does the section take" axis.
tags: pr, content, richness, quality, tags, NEW, MOD, DEL, MOV, test-rows, expected, detail-level, universal, cross-brand
---

This rule defines **content enrichment standards** that are independent of brand layout. Brand rules pick a **structure variant** (Pattern A vs B, TEST-SINGLE vs TEST-TWO-TABLE, etc.) from `changes-list.md` and `supporting-sections.md`. This file defines the **content quality** that fills those structures.

The two axes are orthogonal:

| Axis | Defined by | Examples |
|---|---|---|
| **Structure / Layout** | Brand rule + `changes-list.md` + `supporting-sections.md` | Pattern A vs B, `TEST-SINGLE` vs `TEST-TWO-TABLE`, QA heading wording |
| **Content Richness** | This file (`content-richness.md`) | Status tags on entries, individual test rows vs summaries, expected label detail |

A brand rule declares both: which structure variant AND which content richness level. Content richness defaults to **RICH** unless the brand rule explicitly selects MINIMAL.

## Content Richness Levels

### RICH (default)

Every section is filled with maximum useful detail. This is the default for all brands unless overridden.

### MINIMAL

Sections use grouped summaries and shorter entries. Appropriate for trivial PRs or brands that prefer brevity.

## Enrichment 1: Status Tags on Changes Entries

**Applies to:** ANY Changes pattern (A or B).

The `[NEW]` / `[MOD]` / `[DEL]` / `[MOV]` tag vocabulary is **not Pattern-B-exclusive**. It is a content enrichment that works with any Changes structure. Tags give reviewers instant visual scanning of what changed without reading the diff.

### Tags in Pattern A (flat list)

```markdown
**Changes:**

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

- **[NEW]** **`HcbLocationSections.vue`** (`HcbLocationPageV2/HcbLocationSections/`):
  - ALL visible section content extracted from parent
  - Route: `location-details` at `/colorbar/locations/:locationCode`

- **[MOD]** **`HcbLocationPageV2.vue`** (`HcbLocationPageV2/`):
  - Refactored to thin parent: `router-view` + `serverPrefetch` only
  - `galleryImages` computed merges CMS + DB images
```

### Tags in Pattern B (themed subsections)

Tags are mandatory in Pattern B (unchanged from `changes-list.md`).

### Tag vocabulary (same for both patterns)

| Tag | Meaning |
|---|---|
| `[NEW]` | File did not exist before this PR |
| `[MOD]` | Existing file edited |
| `[DEL]` | File removed from the repo |
| `[MOV]` | Renamed or relocated |

### CRITICAL: Tag legend blockquote

When tags are used (RICH richness on either pattern), a legend blockquote **must** appear immediately after `**Changes:**` (Pattern A) or `### Implementation` (Pattern B). This is mandatory -- never use tags without the legend. Reviewers unfamiliar with the vocabulary need the key.

**Pattern A placement:**

```markdown
**Changes:**

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

- **[NEW]** **`MyComponent.vue`** (`path/`):
  - detail
```

**Pattern B placement:** unchanged (under `### Implementation` as already defined in `changes-list.md`).

One legend per PR. Never duplicate it in other subsections.

### When to omit tags

Under MINIMAL richness, tags may be omitted from Pattern A entries (and the legend is omitted with them). Under RICH richness, tags and legend are always present on both patterns.

## Enrichment 2: Test Case Rows (Threshold-Based)

**Applies to:** ANY Testing Coverage variant (TEST-SINGLE or TEST-TWO-TABLE).

### CRITICAL: Test table length threshold

PR bodies are read by reviewers, not machines. A 85-row test table buries the QA section and wastes vertical space. The format **must** adapt based on test count:

| Total tests | Format | Rationale |
|---|---|---|
| 1 to 30 | **Individual rows** -- one row per `test('...')` | Scannable, reviewer sees every case |
| 31+ | **Grouped by file** -- one row per test file with count + top 3-5 coverage areas | Keeps the table under ~15 rows. Full test list lives in the test files themselves. |

This threshold is a **hard rule**, not a suggestion. A 60-row table is never acceptable regardless of richness level.

### Test metadata lines

Always include these two lines above the table, regardless of test count or richness level:

```markdown
**Test runner:** <framework @ version>
**Command:** `<exact command to run tests>`
```

Example: `**Test runner:** Vitest @ 3.2.4` / `**Command:** \`cd website && npm run test:vue\``. These tell reviewers exactly how to reproduce the test run. Never omit them.

### Individual rows (30 or fewer tests)

```markdown
### Unit Testing Coverage

**Test runner:** Vitest @ 3.2.4
**Command:** `cd website && npm run test:vue`

| Component | Test | Status |
|---|---|---|
| `PageV2.test.js` | renders router-view when location is loaded | ✅ |
| `PageV2.test.js` | hides router-view when location is null | ✅ |
| `PhotosPage.test.js` | renders sticky header with location name | ✅ |
| `PhotosPage.test.js` | handleNavigateBack Back calls trackMREventAndRedirect | ✅ |

**28 tests** across 4 test files -- all passing.
```

### Grouped by file (31+ tests)

```markdown
### Unit Testing Coverage

**Test runner:** Vitest @ 3.2.4
**Command:** `cd website && npm run test:vue`

| Component | Tests | Key Coverage | Status |
|---|---|---|---|
| `HcbLocationPageV2.test.js` | 21 | Router-view guard (null/empty/loaded), `galleryImages` merge + filter + ordering toggle, `routeViewProps` shape, `serverPrefetch` | ✅ |
| `HcbLocationSections.test.js` | 12 | Layout structure, CMS section visibility, breadcrumbs lifecycle, location tracking watcher | ✅ |
| `HcbLocationPhotosPage.test.js` | 18 | Header rendering, grid/hero/masonry visibility, computed props, ADA (role/aria-label), tracking (page view, handleNavigateBack) | ✅ |
| `HairColorBarLocationHeroV2.test.js` | 10 | Render guard, desktop grid, hero image selection, matchMedia lifecycle, handleBookServiceClick | ✅ |
| `LocationImageCarousel.test.js` | 24 | Carousel/skeleton rendering, ADA, computed props (slides/counts), image watcher, slide click + view-more delegation | ✅ |

**85 tests** across 5 test files -- all passing.
```

### Rules

- The "Key Coverage" column lists the 3-5 most important areas tested, not every test name. Use short phrases separated by commas. Group related tests into a single phrase (e.g. "galleryImages merge + filter + ordering toggle" covers 6 tests).
- The test count in the Tests column must be exact.
- The summary line below the table is always present.
- Under MINIMAL richness, grouped format is always used regardless of count.

## Enrichment 3: Detailed Expected Labels in QA Steps

**Applies to:** ANY QA variant (QA-INSTRUCTIONS or QA-HOW-TO-TEST).

Each expected outcome must be specific and observable. Never write "it works" or "page loads correctly."

### RICH format

```markdown
1. Open a location page with multiple images, e.g. `/colorbar/locations/nyc-flat`
   - **Expected:** Desktop hero shows 2 images in a 3fr/2fr grid. A "+X photo(s)" tag appears on the secondary image (bottom-right corner, dark purple background).
   - **Expected:** The count reflects total gallery images minus the 2 visible hero images.
```

### MINIMAL format

```markdown
1. Open `/colorbar/locations/nyc-flat`
   - **Expected:** Hero shows images with "+X photos" tag.
```

### Rule

Under RICH richness, each expected label names the exact UI element, position, color, count, or behavior. Under MINIMAL, a single concise sentence suffices.

## Enrichment 4: Technical Details Formatting

**Applies to:** ANY Technical Details variant (TD-FREEFORM or TD-4FIELD).

### RICH format

Each bullet explains the decision, the alternative considered, and the concrete consequence:

```markdown
### Technical Details

- **CSS-only masonry**: Replaced JS `matchMedia` + `columnCount` with CSS `column-count` media queries. Zero hydration mismatch since SSR and client produce identical HTML.
- **Hard redirect for back/close**: `$router.push` to `location-details` doesn't work on direct `/photos` access (CMS page has different context). `trackMREventAndRedirect` ensures full SSR cycle with correct `cmsSettings`.
```

### MINIMAL format

```markdown
### Technical Details

- CSS masonry replaces JS column logic to avoid hydration mismatch.
- Back/close uses hard redirect for correct SSR on direct access.
```

## How Brand Rules Reference This File

Each brand rule declares its content richness level alongside its structure selections:

```markdown
**Content richness:** RICH (default)
```

Or for a minimal brand:

```markdown
**Content richness:** MINIMAL
```

The brand rule's variant selections (Pattern A/B, TEST-SINGLE/TWO-TABLE, etc.) define the structure. This file's enrichment level defines the detail within that structure.

## Correct vs incorrect examples

### Example 1: Tags locked to Pattern B only

**Incorrect** -- refusing tags in Pattern A because "Pattern A doesn't use tags":
```markdown
**Changes:**

- **`HcbLocationSections.vue`** (`HcbLocationPageV2/HcbLocationSections/`, NEW):
  - extracted content from parent
```

**Correct** -- RICH content with tags + legend in Pattern A:
```markdown
**Changes:**

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

- **[NEW]** **`HcbLocationSections.vue`** (`HcbLocationPageV2/HcbLocationSections/`):
  - ALL visible section content extracted from parent
```

### Example 2: Test summary instead of individual rows

**Incorrect** under RICH -- grouping tests into summary cells:
```markdown
| `PageV2.test.js` | 21 | Router-view guard, galleryImages, serverPrefetch |
```

**Correct** under RICH -- one row per test:
```markdown
| `PageV2.test.js` | renders router-view when location has code | ✅ |
| `PageV2.test.js` | hides router-view when location is null | ✅ |
```
