# Site Revolution Architecture Memory

## 1. Summary

**Domain:** Hair Color Bar location pages — SSR, CMS Partials, booking flow, navigation, experiments
**Project:** Madison Reed Site Revolution Redesign
**Last updated:** 2026-04-23

**Source sessions:**
- `site-revolution-redesign.md` (2026-03-02 → 2026-04-23) — 107 decisions, 15+ patterns extracted

**Referenced by:**
- `dotcompb-7942-google-sso-booking.md` (2026-04-23) — 6 entries referenced (ad-002, ad-003, ad-008, dp-001, dp-003, dp-004, dp-005, dp-006, sf-002, cl-003)

**Knowledge categories:**
- Architecture Decisions: 8 entries
- Design Patterns: 6 entries
- Shared State & Data Flow: 3 entries
- Constraints & Limitations: 5 entries
- Reusable References: 2 entries

---

## 2. Architecture Decisions

### ad-001: css-accordion-over-transition-expand
**Date:** 2026-03-15 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** Needed expand/collapse animation for FAQ sections and review text truncation.
**Decision:** CSS `max-height` transition with `overflow: hidden` — content stays in DOM, toggled via `:class` and `aria-hidden`.
**Alternatives considered:**
- `TransitionExpand` component — rejected: causes content flash (uses `position: absolute` + `visibility: hidden` measurement that leaks a frame)
- JS-measured height + inline style — rejected: unnecessary complexity when CSS handles it
**Consequences:**
- Content stays in DOM (no `v-if`) — use `aria-hidden` to toggle accessibility
- Animation duration is approximate (max-height must overshoot actual height)
- Pattern applies to all accordion/expand-collapse UI in the project

### ad-002: cookie-based-cross-app-state
**Date:** 2026-03-30 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** FeaturedServicesV2 CTA needs to pre-select a service in the booking flow on a different page.
**Decision:** Set `selected_service` cookie + `trackMREventAndRedirect` to services URL.
**Alternatives considered:**
- Vuex `setSelectedService` + `$router.push` — rejected: doesn't survive cross-app page reloads
- URL query parameter — rejected: booking flow doesn't read query params for service selection
**Consequences:**
- Cookie name is a contract between producer and consumer
- Consumer (`ServicesPage.setServiceFromCookie()`) must exist on the destination page
- Pattern applies to any future cross-page state pre-selection

### ad-003: experiment-gated-redirect-not-new-routes
**Date:** 2026-04-10 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** Adding redesigned variant of existing page (Shade Shop replacing ShopProductCategory for color family keys).
**Decision:** `v-if` inside existing component checking experiment flag, renders new component via `defineAsyncComponent`. Existing CMS pipeline, Pug templates, and route definitions untouched.
**Alternatives considered:**
- New Vue Router routes — rejected: would require new CMS pages, new Pug templates, new route registration
- Server-side redirect — rejected: experiment flag only available client-side via `window.experiments`
**Consequences:**
- Experiment B components have no SSR (experiments populated in `mounted()` from `window.experiments`)
- Brief V1-to-V2 flash is expected and acceptable (established pattern across all experiment splitters)
- Data loading must be in `created()` only — `serverPrefetch()` never runs

### ad-004: css-columns-for-masonry
**Date:** 2026-04-03 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** Photos page needed responsive masonry-style grid layout.
**Decision:** CSS-only `column-count: 2/3/4` via media queries. Zero JS.
**Alternatives considered:**
- CSS Grid with `grid-auto-rows: 1px` + ResizeObserver — rejected: caused hydration mismatch between SSR and client column counts
- Flexbox — rejected: flex doesn't support true masonry flow
- JS `matchMedia` + `columnCount` data — rejected: SSR/client divergence
**Consequences:**
- Zero hydration mismatch — SSR and client produce identical HTML
- CSS handles responsive column distribution automatically
- No aspect-ratio control per-item (columns fill top-to-bottom, not row-balanced)

### ad-005: dvh-over-vh-for-mobile
**Date:** 2026-04-08 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** Mobile nav panels using `100vh` included area behind iOS URL bar, Dynamic Island, and home indicator.
**Decision:** `100dvh` (Dynamic Viewport Height) replaces `100vh` for mobile viewports.
**Alternatives considered:**
- `100vh` + `env(safe-area-inset-bottom)` padding — rejected: site viewport meta lacks `viewport-fit=cover`, so `env()` always resolves to `0px` (no-op)
**Consequences:**
- Browser support: iOS Safari 15.4+, Chrome 108+, Firefox 101+
- All `env(safe-area-inset-bottom)` padding hacks can be removed
- Applies to any future full-viewport mobile UI

### ad-006: cms-partial-ssr-replaces-custom-components
**Date:** 2026-03-19 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** Location-specific booking URLs needed in CMS Partial marketing modules.
**Decision:** `CmsPartialSsr` with `clientConfig` prop passes data to dynamically compiled CMS template. Replaces custom globally-registered components with URL workarounds.
**Alternatives considered:**
- `MarketingBanner` global component with `isBookServiceCta` text-sniffing — rejected: fragile, relies on CTA text matching
**Consequences:**
- CMS templates can reference `clientConfig.bookingUrl` directly
- No new globally-registered components needed for per-location URLs
- Pattern applies to any CMS Partial needing runtime-computed data

### ad-007: div-role-link-for-multi-content-cards
**Date:** 2026-04-15 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** Level Access ADA scanner flags native `<a>` containing multi-part visible text (name + description + tags).
**Decision:** `<div role="link" tabindex="0" @keydown.enter>` with NO `aria-label` — inner text becomes accessible name, satisfying WCAG 2.5.3.
**Alternatives considered:**
- Native `<a>` with `aria-label` — rejected: scanner flags mismatch between `aria-label` and concatenated inner text
**Consequences:**
- Space-key activation and right-click "Open in new tab" unavailable
- Level A compliance takes priority over UX convenience
- Same pattern for any card with multi-part visible text

### ad-008: sticky-header-resize-observer-pattern
**Date:** 2026-04-15 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** Components adding sticky elements below site nav need to know the full sticky header height (nav + banners + carousel).
**Decision:** `document.querySelector('.sticky-header-wrap.is-sticky')` + `ResizeObserver` to compute `top` offset dynamically.
**Alternatives considered:**
- `--mr-navigation-height` CSS variable — rejected: only measures nav, not full sticky wrap (misses banners/carousel)
- `--mr-sticky-header-height` — rejected: dead variable, never set by any file in the codebase
- Hardcoded `top: 144px` — rejected: magic number, breaks when banner state changes
**Consequences:**
- Scales to experiment A/B, banner visible/dismissed, carousel present/absent, breakpoint changes
- Requires `$nextTick` + `typeof ResizeObserver` SSR guard
- Decoupled from experiment name — `.is-sticky` selector handles gating

---

## 3. Design Patterns

### dp-001: self-contained-landmarks
**Name:** Self-Contained Landmarks
**When to use:** Any component that needs an ARIA landmark (section, region, navigation).
**Structure:**
- Component owns `role="region"` + `aria-labelledby` + heading `id` in its own template
- Parent wrappers are purely structural (no ARIA attributes)
- `v-if` on component root guards heading and `aria-labelledby` together
- For delegated headings, pass `titleId` prop
**Why:** Cross-component `@ready` coordination caused ADA violations — `aria-labelledby` on `div` without `role` is invalid.
**Validated in:** HeroV2, About, Services, Reviews, FeaturedDeals, ShadeShopPage
**Anti-pattern:** `aria-labelledby` on parent wrapper divs without a role. Never use `@ready` events for heading ID coordination.

### dp-002: cross-store-data-bridging
**Name:** Cross-Store Data Bridging for SSR
**When to use:** When a page's CMS Partial references globally registered components that depend on a different Vuex store module than the page primarily populates.
**Structure:**
- Parent `serverPrefetch`: load primary data → copy to dependent store via mapped mutations
- Example: `await loadLocation(code)` → `setBookingLocation(location)`
**Why:** CMS Partials execute during SSR. Their global components access store state server-side — if the dependent store is empty, SSR crashes with 500.
**Validated in:** HcbLocationPageV2 → FeaturedServicesV2 (via `partial-featured-services-v2`)
**Anti-pattern:** Assuming CMS Partial components are client-only. They render during SSR.

### dp-003: experiment-gated-async-component
**Name:** Experiment-Gated Component via defineAsyncComponent
**When to use:** When adding a redesigned variant of an existing page behind an A/B experiment.
**Structure:**
- `defineAsyncComponent` import for experiment B component
- 3 computed: experiment flag check, data readiness, combined gate
- 1 template `v-if`: experiment B ? new component : existing content
- Data loading in `created()` only (no `serverPrefetch`)
**Why:** Keeps existing CMS pipeline, routes, and Pug templates untouched. Follows established `Splitter.vue` pattern.
**Validated in:** ShopProductCategory → ShadeShopPage (ShadeShopSiteRevolution experiment)
**Anti-pattern:** Creating new Vue Router routes for experiment variants.

### dp-004: self-sufficient-component-spacing
**Name:** Self-Sufficient Component Spacing
**When to use:** Every imported component in a page template.
**Structure:**
- Component handles own spacing via utility classes on root element (e.g., `.py-150m`)
- Parent lists components directly without wrapper divs or spacing classes
- Dividers: `.bottom-divider-light` on component root alongside spacing
- Repeating items: `.pb-150m.mb-150m` with `:last-child` removing border + margin + padding
**Why:** Makes components reusable across pages without parent needing to know about spacing needs.
**Validated in:** About, Services, Reviews, FAQs, HcbLocationPageV2 (all wrapper divs removed)
**Anti-pattern:** Parent wrapper divs with spacing classes around imported components.

### dp-005: live-region-outside-conditional
**Name:** Live Region Placement Outside v-if Gate
**When to use:** Any component with dynamic announcements for screen readers.
**Structure:**
- `.hiddenButPresent(aria-live)` element at root level, OUTSIDE any `v-if` block
- Wrap template content in a structural root div with the live region as sibling
- Use `filtersEverUsed` flag to gate announcements after first user interaction
**Why:** ATs observe mutations to live regions already present in DOM. If the region is created with text already populated (e.g., from URL params), the announcement is silently dropped.
**Validated in:** ShadeShopPage filter announcements
**Anti-pattern:** Placing live region inside `v-if="hasData"` — AT misses the initial announcement.

### dp-006: focus-return-on-collapse
**Name:** Focus Return on Collapse/Destroy
**When to use:** When a collapsing toggle may destroy the currently focused DOM node.
**Structure:**
- `event.currentTarget.focus()` in `$nextTick` after state change
- Template passes `$event`: `@click="toggleExpand(type, $event)"`
- MrBtn forwards native DOM event on emitted `click`
**Why:** When expanded items beyond visible count are removed from DOM, focused element is lost.
**Validated in:** ShadeShopPage product grid expand/collapse
**Anti-pattern:** Not managing focus on collapse — user's keyboard position is lost.

---

## 4. Shared State & Data Flow

### sf-001: navigation-data-pipeline
**Name:** Navigation Data Pipeline
**Source:** Tophat Data Tool — `dataToolSvc.getData({ mixinKey: 'sr-top-nav' })`
**Transforms:** CMS API → `siteNav.js` Vuex action → `res.data || {}` null guard → individual mutations
**Consumers:** SiteNavDesktopV2, SiteNavMobileV2MainNav, SiteNavShopContent, SiteNavMobileWrapper
**Gotchas:**
- `res.data` can be null — always destructure with `|| {}` guard
- All nested CMS object access requires `?.` (17 spots audited in SiteNavShopContent)
- `mixinKey` changed from `'top-nav'` to `'sr-top-nav'` for Site Revolution

### sf-002: experiment-flag-lifecycle
**Name:** Experiment Flag Lifecycle (SSR → Client)
**Source:** `window.experiments` (set by Express middleware via cookie)
**Transforms:** SSR: `this.experiments` = `{}` (empty) → Client `mounted()`: populated from `window.experiments`
**Consumers:** Any component checking `this.experiments?.['ExperimentName'] === 'B'`
**Gotchas:**
- `this.experiments` is available globally via `globalMixins` but is EMPTY during SSR
- Experiment B components only render after client `mounted()` — `serverPrefetch()` never runs on them
- `inSiteRevolutionExperiment` computed exists on `mrVueApp.js` root but is NOT a global mixin — not inherited

### sf-003: sticky-header-height-measurement
**Name:** Sticky Header Height Measurement
**Source:** `.sticky-header-wrap.is-sticky` DOM element (SsrApp.vue + vue-layout.pug)
**Transforms:** `document.querySelector` → `getBoundingClientRect().height` → `ResizeObserver` for updates
**Consumers:** Any component adding its own sticky element below the site nav
**Gotchas:**
- `--mr-navigation-height` only measures `.mr-navigation`, NOT full sticky wrap (misses banners/carousel)
- `--mr-sticky-header-height` is DEAD — referenced but never set by any file
- `.is-sticky` class gates on `BookingFlowSiteRevolution === 'B'` — check selector, not experiment name directly

---

## 5. Constraints & Limitations

### cl-001: cms-parseurl-segment-validation
**Scope:** Vue Router children under CMS-managed routes
**Constraint:** CMS `parseUrl` validates URL segment count against Tophat `urlParameterList.length`. Extra segments fail → 404.
**Cause:** `mr_modules/cms/lib/router.js:650` — server-side URL resolution before Vue Router.
**Workaround:** Define optional parameters in Tophat for each child route segment.
**Impact:** Child routes return 404 on direct access and full-page refresh without Tophat config.

### cl-002: req-url-coupled-cms-and-vue-router
**Scope:** SSR URL handling
**Constraint:** `req.url` is read by both CMS resolution (`htmlRenderer.js:536`) and Vue Router SSR (`server.js:22`). URL rewriting for CMS breaks Vue Router SSR.
**Cause:** Tight coupling in `htmlRenderer.js` → `locals.parsedUrl` → `ssrContext.url` → `router.replace(context.url.path)`.
**Workaround:** None without core CMS infrastructure changes. Use Tophat optional params instead of URL rewriting.
**Impact:** Hydration mismatch if req.url is modified between CMS resolution and Vue Router SSR.

### cl-003: no-ssr-for-experiment-b-components
**Scope:** Any component rendered conditionally based on experiment flags
**Constraint:** `this.experiments` is `{}` during SSR. Experiment B components never render server-side.
**Cause:** `globalMixins.data()` initializes experiments as `{}`, populated in `mounted()` from `window.experiments`.
**Workaround:** Data loading in `created()` (runs both SSR and client). Brief V1→V2 flash is accepted.
**Impact:** No SSR SEO benefit for experiment B content. Brief layout shift on initial load.

### cl-004: viewport-fit-cover-not-set
**Scope:** iOS Safari safe area handling
**Constraint:** `env(safe-area-inset-bottom)` always resolves to `0px` because the site's viewport meta tag lacks `viewport-fit=cover`.
**Cause:** `<meta name="viewport" content="width=device-width, initial-scale=1">` — no `viewport-fit=cover`.
**Workaround:** Use `100dvh` instead of `100vh + env()` padding hacks. `dvh` handles safe areas natively.
**Impact:** All `env(safe-area-inset-bottom)` padding calculations are no-ops — they add 0px.

### cl-005: ssrapp-no-root-router-view
**Scope:** CMS page Vue Router integration
**Constraint:** `SsrApp.vue` has NO root `<router-view>`. CMS HTML is injected via string interpolation.
**Cause:** `entry/server.js` line 28: "SSR doesn't render `<router-view>`". CMS content delivered via dynamic template.
**Workaround:** Place `<router-view>` inside the CMS-rendered component (e.g., `HcbLocationPageV2`). This is the ONLY router-view on CMS pages.
**Impact:** Zero dual-render conflict risk when registering location child routes.

---

## 6. Reusable References

### rr-001: colorbar-cache-media-object
**Name:** ColorBar Cache Media Object Shape
**Source:** `colorbarCache.getLocation()` → `carouselImages[]`
**Shape:**
```json
{
  "_id": "string",
  "url": "string (full CDN URL, may have query params — strip with url.split('?')[0])",
  "width": "number (pixels — enables dynamic aspect ratio)",
  "height": "number (pixels)",
  "alt_text": "string (ImgBox reads automatically — don't pass explicit :alt)",
  "file_name": "string",
  "file_type": "string",
  "aspects": ["array of aspect ratio objects"],
  "versionInfo": ["array"]
}
```
**Consumers:** HairColorBarLocationHeroV2, HcbLocationPhotosPage, LocationImageCarousel

### rr-002: cms-svg-icon-object
**Name:** CMS SVG Icon Object Shape (for ImgBox)
**Source:** Tophat CMS — add-on services, feature icons
**Shape:**
```json
{
  "icon": {
    "color": "string",
    "size": "string",
    "icon": {
      "file_type": "svg+xml",
      "svg_data": { "...SVG attributes and paths..." }
    },
    "fill": "string",
    "originalColor": "string"
  }
}
```
**Consumers:** HairColorBarLocationServices (add-on icons via `ImgBox(:media-obj="addon.icon")`)
**Note:** Use `ImgBox` for CMS SVG icons — NOT `mr-icon`. ImgBox detects `isNewSvg` internally. Use `mr-icon` only for locally bundled SVGs from `src/assets/svg-icons/`.
