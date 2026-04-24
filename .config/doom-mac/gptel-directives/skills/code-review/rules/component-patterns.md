---
title: Component Patterns — Accordion, Store-Free Sticky, MountedFlag, Carousel, CMS Data, Dead Code, V1/V2 Reuse
impact: HIGH
impactDescription: Component-level patterns that prevent content flashes, store coupling, carousel overflow, dead code accumulation, CMS data crashes, and premature template rendering. Violations produce subtle visual bugs, memory leaks, bundle bloat, and production TypeErrors that are hard to trace to their root cause.
tags: component, pattern, accordion, max-height, TransitionExpand, flash, fixed, sticky, props, emits, store-free, MountedFlag, body-class, carousel, overflow, swiper, flex, viewport, white-space, pre-wrap, pre-line, cms, textarea, dead-code, removal, audit, grep, v1, v2, reuse, evaluate, rebuild, v-if, guard, truthy, empty-object, vuex, initial-state, cms-url, strip, query-params, ImgBox, dataToolSvc, null-guard, destructure
---

Rules for reviewing Vue component implementation patterns in the MR monorepo. These patterns were validated across 10+ tickets and 56+ PR review comments. Each addresses a specific failure mode discovered during the Site Revolution Redesign: content flashes, store coupling preventing reuse, carousel overflow, CMS data crashes, premature rendering, and dead code accumulation.

## Rules

| # | Rule | Severity | Section |
|---|---|---|---|
| 1 | **maxheight-accordion** — CSS max-height transition, NOT TransitionExpand | MEDIUM | [Details](#maxheight-accordion) |
| 2 | **no-store-sticky** — Fixed/sticky overlay UI must be props+emits only | MEDIUM | [Details](#no-store-sticky) |
| 3 | **mountedflag-body** — MountedFlag for body class coordination between siblings | MEDIUM | [Details](#mountedflag-body) |
| 4 | **carousel-overflow** — Viewport-relative max-width on carousel flex children | MEDIUM | [Details](#carousel-overflow) |
| 5 | **prewrap-cms** — white-space: pre-wrap for CMS textarea content | LOW | [Details](#prewrap-cms) |
| 6 | **dead-code-exhaustive** — Feature removal must audit ALL artifact types | HIGH | [Details](#dead-code-exhaustive) |
| 7 | **v1v2-reuse-eval** — Evaluate V1 equivalents before building V2 | MEDIUM | [Details](#v1v2-reuse-eval) |
| 8 | **vif-guard-specific** — v-if on Vuex object state: check meaningful property | HIGH | [Details](#vif-guard-specific) |
| 9 | **cms-url-strip** — Strip CMS media URL query params before ImgBox | MEDIUM | [Details](#cms-url-strip) |
| 10 | **datatool-nullguard** — Null-guard dataToolSvc responses before destructuring | HIGH | [Details](#datatool-nullguard) |

---

## maxheight-accordion

Expand/collapse animations MUST use CSS `max-height` transition. Do NOT use `TransitionExpand` — it causes a visible content flash (uses `position: absolute` + `visibility: hidden` measurement that leaks a frame).

Content stays in the DOM (no `v-if`). Toggle via `:class="{ expanded: isOpen }"` + `aria-hidden`.

```stylus
.answer
  max-height 0
  overflow hidden
  transition max-height 500ms ease-out
  &.expanded
    max-height 100em  // arbitrary high value — CSS needs explicit, not auto
    transition max-height 500ms ease-in
```

**Exception:** SEE MORE/LESS that slices arrays (products, cards) uses `v-if`/slice, NOT this pattern. This rule is specifically for animated reveal of a single content block (FAQ-style accordion).

**Validated:** HairColorBarLocationFAQs (DOTCOMPB-7290). Session decision #23.

---

## no-store-sticky

Fixed-position or sticky overlay UI components (CTAs, toolbars, banners floating above content) must be props+emits only with zero Vuex store dependencies. Parent wires store data to props.

**Exception:** CSS `position: sticky` elements in normal document flow (e.g., ShadeShopPage filter header) may use `ResizeObserver` + DOM measurement for offset calculation. This is different from overlay UI.

**Validated:** FixedCtaBar (DOTCOMPB-7556, 35 tests). Props: `visible`, `ctaText`, `ctaDisabled`, `ctaLoading`, `ariaLabel`. Emits: `cta-click`.

---

## mountedflag-body

Use `MountedFlag(v-if="condition" flag="class-name")` for body class coordination between cross-tree siblings that have no common Vue ancestor. The consuming component reads via CSS selector only (no JS). The flag is declarative and auto-cleans on unmount.

```pug
MountedFlag(v-if="!isDesktop" flag="bt-with-sticky-cta")
```
```stylus
// In SierraWidget.vue — reads body class via CSS
@media mq-tablet-less
  .bt-with-sticky-cta &
    transform translateY(-90px)
```

Flag names use `bt-` prefix and must be globally unique. `v-if` guard for breakpoint-specific flags.

**Exception:** When components share a Vue ancestor, use props/emits instead.

**Validated:** DOTCOMPB-7652 (SierraWidget overlap with FixedCtaBar).

---

## carousel-overflow

Swiper carousels inside flex containers need `max-width: calc(100vw - <horizontal-padding>)` at mobile/tablet. Swiper calculates slide widths that may exceed viewport. Remove constraint at desktop when grid naturally constrains width.

```stylus
.carousel-wrapper
  max-width calc(100vw - 2.5em)  // matches page horizontal padding
  @media mq-desktop-plus
    max-width none
```

**Exception:** If parent has `overflow: hidden`, may be unnecessary. Test first.

**Validated:** HairColorBarLocationServices (DOTCOMPB-7290).

---

## prewrap-cms

CMS textarea content: `white-space: pre-wrap` in Stylus (not utility class — none exists). `pre-wrap` preserves ALL newlines including consecutive `\n\n`. `pre-line` collapses them — destroying intentional paragraph spacing from Tophat editors.

**Exception:** NOT for CMS Partial HTML (has own `<p>`/`<br>`) or headings (should never contain newlines).

**Validated:** Getting Here, Payments, service descriptions, MarketingBanner `.banner-description` (DOTCOMPB-7290). Session decision #27.

---

## dead-code-exhaustive

Feature removal PRs must audit ALL five artifact layers: Template (elements), Script (methods, computed, data, constants), Styles (rule blocks), Tests (describe blocks), and Imports (orphaned `import` statements). Grep to confirm zero remaining references.

**Validated examples:**
- DOTCOMPB-7555: Template MrBtn + method + computed + constant + style block + 2 test blocks = 6 artifacts
- DOTCOMPB-7717: Vuex `import { mapState }` + prop + 4 computeds + template binding + 2 test updates

**Exception:** Parked work on separate branches (e.g., `DOTCOMPB-7555_full_width`) is NOT dead code — do not flag for removal. Session decision #39.

---

## v1v2-reuse-eval

Before building V2 components, evaluate V1 equivalents. Reuse criteria: (1) props-driven, (2) Options API, (3) matches design with minor modifications. When reused, add V2 conventions (self-contained landmarks, `v-if` guards, tracking). When rebuilt, document rationale.

**Validated:** 3 V1 footer components reused as-is (MoreLocations, RegionList, MoreInfo). 7 V2 components rebuilt (Services, Reviews, FAQs, MarketingBanner, etc.). Session decision #6.

---

## vif-guard-specific

`v-if` on Vuex object state must check a meaningful property (`location?.code`), not object truthiness (`location`). Vuex modules initialize state as `{}` (empty object) — truthy in JavaScript. `v-if="location"` passes on `{}`, rendering children with empty data.

**Incorrect:**
```pug
router-view(v-if="location" v-slot="{ Component }")
```

**Correct:**
```pug
router-view(v-if="location?.code" v-slot="{ Component }")
```

For arrays: check `.length` (empty `[]` is truthy). For strings: direct truthiness works (`""` is falsy).

**Extends:** Rule 4 (which catches `null`/`undefined` but not truthy-but-empty `{}`).

**Validated:** DOTCOMPB-7712 — `router-view` guard. Session decision #67.

---

## cms-url-strip

Strip CMS media URL query params (`url.split('?')[0]`) before passing to ImgBox. CMS bakes in crop/size params (`?w=400&h=300&fit=crop`) that conflict with ImgBox's own CDN param generation.

Strip at the data-preparation layer (parent computed), not the rendering component.

```javascript
galleryImages() {
  return this.rawImages
    .filter(item => item?.image?.url)
    .map(item => ({
      ...item,
      image: { ...item.image, url: item.image.url.split('?')[0] },
    }));
}
```

**Exception:** DashHudson images (third-party API) don't need stripping. DB `carouselImages` may not have query params — strip only where params exist.

**Validated:** galleryImages computed (DOTCOMPB-7289, moved to parent in DOTCOMPB-7712). Session decision #61.

---

## datatool-nullguard

`dataToolSvc.getData()` responses: null-guard `res.data` with `|| {}` before destructuring. CMS returns `null` when Tophat Data Object is unpublished, empty, or misconfigured.

Two-layer defense: `|| {}` at destructuring + `?.` at every nested property access.

**Incorrect:**
```javascript
const res = await dataToolSvc.getData({ mixinKey: 'config' });
const { settings } = res.data; // TypeError if res.data is null
```

**Correct:**
```javascript
const res = await dataToolSvc.getData({ mixinKey: 'config' });
const data = res.data || {};
commit('setSettings', data.settings);
```

Also in templates: `item?.link?.text` (not `item.link.text`). 17 spots audited in SiteNavShopContent.

**Exception:** Internal APIs with guaranteed response shapes may skip. ALL `dataToolSvc` responses are mandatory.

**Validated:** siteNav.js Sentry TypeError at L60 (DOTCOMPB-7463). Session decisions #41, #42.
