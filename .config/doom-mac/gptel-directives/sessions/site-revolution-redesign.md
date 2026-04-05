<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the Site Revolution Redesign session. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read the sections in order on first load — after that, reference them by number as needed. The data is organized into 5 sections:

| Section                        | Purpose                                                                                                         | When to reference                                                                                  |
|--------------------------------|-----------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------|
| **1. Global Guidelines**       | Coding rules, patterns, and conventions that apply to ALL work in this session.                                 | Before writing or reviewing any code. These are mandatory constraints.                             |
| **2. Session Overview**        | High-level context: what this project is, which tickets are in scope, session-wide decisions, and pending work. | When starting a new task — understand scope and status first.                                      |
| **3. Feature Implementations** | Per-ticket detail: what was built, where files live, component trees, key decisions, tests, and PR status.      | When resuming work on a specific ticket, or when a new task relates to an existing implementation. |
| **4. File Index**              | Quick-reference table of all file paths (components, tests, roam nodes, directives).                            | When you need to read, edit, or reference a specific file without searching.                       |
| **5. Last Interaction**        | Short-term memory: what was done last, what's pending, where to resume.                                         | At the very start of a new conversation — this is your entry point for continuing work.            |

**Operational Rule:** Throughout the conversation, the user will give new tasks. **Always look for the last request, identified by a markdown title starting with `###`.** Any `###` title means it is the newest request. Based on that section, load relevant skills and apply the rules from Section 1.

**Key principle:** Data may appear in multiple sections with different framing. Section 1 frames it as a rule to follow. Section 2 frames it as context to understand. Section 3 frames it as an implementation to reference. This is intentional — each section answers a different question about the same knowledge.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** The loaded skills for this project — `mr-dotcom-dev` (Vue/Vuex/Pug/Stylus patterns), `mr-style` (design system classes and variables), `code-review` (quality analysis), and `mr-roam-node` (ticket documentation) — cover general framework conventions but do not capture every project-specific decision, architectural pattern, or guideline that emerged during development. This section stores those as session-scoped globals: patterns validated through code reviews, bug fixes, and architectural discussions that the skills don't enforce yet. Treat them as mandatory constraints alongside the skill rules. Over time, stable patterns here should be pulled into the corresponding skill files and refined there — this section is their staging area.

### 1.1 Framework & API

*   **Vue 3** — Options API only (`export default { ... }`). No Composition API, no `<script setup>`.
*   **Templating:** Pug (`<template lang="pug">`).
*   **Styling:** Scoped Stylus (`<style lang="stylus" scoped>`).
*   **JS Syntax:** Always use curly braces for `if` statements, even single-line returns.

### 1.2 Styling Architecture

*   **Utility-first:** Aggressively apply responsive utility classes. Consolidate common classes on parent elements (DRY).
*   **Scoped Stylus:** Only for structural layouts, pseudo-elements, transitions, complex layouts, `:deep()` overrides, and custom media queries. If a utility class exists for a CSS property, the utility class MUST be used instead of Stylus.
*   **Utility class replacement rule:** All Stylus properties that have utility class equivalents MUST use the utility class in the template instead. This includes but is not limited to: `display flex` → `.flex`, `display inline-block` → `.inline-block`, `overflow hidden` → `.overflow-hidden`, `width 100%` → `.full-width`, `flex-direction column` → `.flex-col`, `height 100%` → `.full-height`, `flex 1` → `.flex-1`, `margin 0 auto` → `.div-center`, `font-weight 600` → `.semi-bold`, `font-weight 700` → `.bold`. When replacing, remove the property from Stylus. If the Stylus selector becomes empty after removal, remove the selector entirely.
*   **Units:** `rem`/`em` for spacing and typography. `px` for small fixed-size elements (borders, box-shadows) where em/rem produces non-intuitive decimals (e.g., `border: 2px solid` not `0.143em`).
*   **Design System Variables:** Never hardcode hex colors — use `brand-color-*`, `cta-color-*`, `text-color-*`, `ui-color-*`. Use breakpoint mixins. Keep hardcoded `px` if no exact breakpoint variable exists.
*   **Responsive padding/margin via utility classes, never Stylus:** When padding or margin needs to change at a breakpoint, use responsive utility class prefixes (e.g., `.px-100m.xl-px-400m`), NOT custom `padding-left`/`padding-right` in `<style>` blocks. Stylus `@media` rules for padding/margin are forbidden — that's what the utility class system is for.
*   **Class ordering:** Alphabetical in Pug, except structural/positioning classes which may precede for clarity.
*   **CSS property ordering:** Alphabetize within `<style>` blocks.
*   **Glassmorphism:** `background-color: rgba(...)` + `backdrop-filter: blur(...)` for modern overlays.
*   **Font sizes in templates only, never Stylus:** All `font-size`, `font-family`, and `color` for text must use utility classes in the Pug template (`.xs-f-small`, `.bold`, `.text-color-2`). Never use `font-size`, `font-family`, or text `color` in `<style>` blocks — those belong to the utility class system.
*   **`.max-at-tweak` is MANDATORY on every font utility class:** Every element that uses a responsive font size class (`.xs-f-*`, `.sm-f-*`, `.md-f-*`, `.lg-f-*`, `.xl-f-*`, `.font-*`) MUST also have `.max-at-tweak`. This caps the flex-sizing so fonts don't grow infinitely at large viewports. No exceptions.
*   **`.upper` is MANDATORY on every `.f-secondary` heading:** Kapra Neue is an uppercase font. Pattern: `h2#my-title.color-mr-purple.f-secondary.sm-f-xxlarge.max-at-tweak.upper`. Validated across About, Services, Reviews, FeaturedDeals, Getting Here, Payments, Location & Hours.
*   **Heading text format — inline, not multi-line `|`:** Use `h2.f-secondary.upper My Title` not `h2.f-secondary.upper` followed by `| My Title` on a new line. Dynamic interpolation works inline: `h2.upper about {{ location.name }}`.

### 1.3 Component Design Patterns

*   **Canonical Field Order:** `name` → `components` → `emits` → `props` → `data` → `computed` → `watch` → lifecycle hooks → `methods`.
*   **Props over `:deep()`:** Use child props API before resorting to `:deep()`.
*   **Module-Level Constants:** Static, non-reactive data (maps, configs, magic numbers) → `const` outside `export default`.
*   **No Magic Numbers:** All numeric literals → named constants (e.g., `MINIMUM_BREADCRUMB_COUNT = 3`).
*   **Constants in templates — camelCase computed wrapper:** When a module-level `UPPER_SNAKE_CASE` constant must be used in a Pug template (e.g., `v-for` count), expose it via a `camelCase` computed property: `const SKELETON_CARD_COUNT = 3;` → `skeletonCardCount() { return SKELETON_CARD_COUNT; }` → template `v-for="i in skeletonCardCount"`. Never use `UPPER_SNAKE_CASE` in templates — it looks like a leaked global. Constants used **only** in script methods/computed should be referenced directly without a wrapper. Validated pattern: `LocationImageCarousel.thumbnailSkeletonCount`, `HairColorBarLocationReviews.maxStars`, `HairColorBarLocationReviews.skeletonCardCount`.
*   **Data Preparation in Parent:** Parent filters/shapes data in computed before passing props. CMS data may have invalid entries — always filter (e.g., require `.image.url`).
*   **Complex Feature Abstraction:** Features with internal state → self-contained child component.
*   **No Inline Event Logic:** `@click` must call a single method. No inline expressions.
*   **DRY Templates:** Repeated boolean expressions → extract to a method.
*   **Pre-compute Reactive Data:** Values computed once per lifecycle → store in `data`.
*   **Optional Chaining (`?.`):** Always. `getObjProperty` is deprecated and forbidden.
*   **Computed alphabetization:** Alphabetize computed properties within `<script>`.

### 1.4 Responsive / Breakpoint Strategy

*   **Centralized breakpoint (`global/isDesktop`):** For show/hide logic at the standard desktop threshold (960px+), use `mapGetters('global', ['isDesktop'])` from the Vuex `global` module. This is provided by `mainAppMixin` with a throttled resize listener. **Do not duplicate with local `matchMedia` when the threshold matches.**
*   **Local `matchMedia`:** Only when you need a breakpoint that differs from the global getter (e.g., `max-width: 559px` for mobile-only image layout switching). Store the `MediaQueryList` in `data` (SSR-safe). Add listener in `mounted`, remove in `beforeUnmount`.
*   **Rule:** Only replace local breakpoint detection when the threshold exactly aligns with the centralized getter. Different breakpoints serve different UI decisions.
*   **`window.resize` is forbidden** for responsive logic.
*   **Utility class breakpoint prefixes and their actual values:** `xs-` = mobile (default), `sm-` = tablet (560px+), `md-` = desktop MD (760px+), `lg-` = desktop (960px+), `xl-` = desktop XL (1200px+). These map to Stylus variables: `bp-tablet-min = 560px`, `bp-desktop-md-min = 760px`, `bp-desktop-min = 960px`. The `lg-` prefix maps to `mq-desktop-plus` (960px+). Choose the prefix closest to the desired breakpoint — since elements hidden below that breakpoint won't show the padding anyway, exact match isn't always required (e.g., `.deals-column` is `display: none` below 1024px, so `lg-` at 960px is fine).

### 1.5 Accessibility (A11y)

*   **Semantic HTML:** `<main>`, `<section>`, `<dl>` over generic `<div>`.
*   **ARIA:** `role="region"`, `aria-label`, `aria-labelledby`, `aria-controls`, `aria-expanded`.
*   **`aria-labelledby` must reference heading IDs, never root element IDs:** `aria-labelledby` resolves to the **text content** of the referenced element. Pointing to a root container div would include ALL nested text (titles, descriptions, buttons, prices) as the accessible name — useless for screen readers. Always point to the actual heading element that gives the section a meaningful label.
*   **Root elements use classes, not IDs:** Since `aria-labelledby` targets headings, root elements use **class selectors** (`.hair-color-bar-location-services`), not IDs. IDs are reserved exclusively for heading elements that serve as `aria-labelledby` targets. This eliminates redundant ID-on-root patterns and keeps a clear separation: classes for styling/DOM queries, IDs for accessibility references.
*   **No redundant class when ID exists:** When a heading element has an ID (for `aria-labelledby`), do NOT add a class with a similar name. The ID is sufficient as a CSS/Stylus selector. Example: use `h2#services-section-title.color-mr-purple` — NOT `h2#services-section-title.services-title.color-mr-purple`. The `.services-title` class is redundant because `#services-section-title` already uniquely identifies the element. This rule applies to all elements with IDs, not just headings.
*   **Self-contained landmarks (replaces `@ready` pattern):** Each component owns its own landmark structure — `role="region"`, `aria-labelledby`, and the heading `id` all live in the **same component template**. Parent wrapper divs are purely structural (layout) and must NOT have ARIA attributes. This eliminates the old `@ready` + parent `aria-labelledby` pattern, which caused ADA violations (`aria-labelledby` on a `<div>` without a `role` is invalid).
    *   **Component pattern:**
        ```pug
        .my-component(v-if="hasData" role="region" aria-labelledby="my-section-title")
          h2#my-section-title.color-mr-purple.f-secondary Title Text
        ```
    *   **Parent pattern:**
        ```pug
        //- NO aria-labelledby, NO @ready — purely structural wrapper
        .section-wrapper
          MyComponent(:data="data")
        ```
    *   **Why this works:** The `v-if` on the component root guards the heading and `aria-labelledby` together — if the component doesn't render, neither exists. No timing issues, no dangling references, no cross-component coordination.
    *   **Delegated heading (e.g., PageIntro):** When the heading is inside a nested child component, add a `titleId` prop to pass the ID through:
        ```pug
        PageIntro(:title="location.name" title-id="hero-section-title")
        //- PageIntro renders: h1(:id="titleId") {{ title }}
        ```
    *   **Anti-pattern — DO NOT:** Put `aria-labelledby` on parent wrapper `<div>`s without a `role`. Screen readers ignore it and ADA tools flag it as a violation. Never use `@ready` events to coordinate heading IDs between parent and child — the old pattern is deprecated.
    *   **Validated across:** HeroV2 (`hero-section-title`), About (`about-section-title`), Services (`services-section-title`), Reviews (`reviews-section-title`), FeaturedDeals (`deals-section-title`). All parent wrappers cleaned of ARIA attributes.
*   **Keyboard nav:** Non-native interactive elements → `tabindex="0"` + `@keydown.enter.prevent` + `@keydown.space.prevent`.
*   **Native buttons:** Never use raw `<button>` elements. Use `MrBtn` for button interactions. For navigation actions that track + redirect, use `<a>` with `:href` + `@click.prevent` — this provides correct semantics (navigation) while allowing cookie/tracking logic before redirect. Global `<a>` styles provide `color: cta-color-1`, `cursor: pointer`, `font-family: f-primary` automatically — no need to duplicate in Stylus.
*   **No fake interactive roles on non-interactive elements:** Do not add `role="button"`, `tabindex`, or click handlers to elements that only perform tracking (no visible user action). If the tracking is tied to a real interactive element nearby (e.g., a "Book Now" link), let that element handle the tracking.
*   **No nested interactives:** Never `<a>` inside `role="button"`. Use `<div aria-hidden="true">` for decorative overlays.
*   **Interactive content inside links:** When a child component renders interactive elements (buttons, inputs — e.g., Google Maps), do NOT wrap in `<a>`. Use `<div>` with `role="link"`, `tabindex="0"`, `@keydown.enter.prevent`, `.clickable`, `aria-label`, and `window.open(url, '_blank')` for navigation. Block child interactivity with `:deep(*) pointer-events none`. Validated: `HairColorBarLocationAbout` map thumbnail (`HcbGmap` inside `.map-thumbnail-wrap`).
*   **`aria-expanded` on toggle buttons:** Buttons controlling expandable content must have `:aria-expanded="!!stateVar"`. The `!!` coerces `undefined` → `false` so the attribute is always rendered. Validated: Reviews expand/collapse toggle, About Show More/Less.
*   **Dynamic `aria-label` via props for repeated elements:** When multiple identical-text links exist (e.g., "Book Now"), add `aria-label` with context: `:aria-label="\`Book ${service.name}\`"`. For container components, pass context via props (e.g., `LocationImageCarousel` `locationName` prop → `:aria-label="\`${locationName} photo gallery\`"`).

### 1.6 Testing (Vitest & Vue Test Utils)

*   **Run:** `cd website && npm run test:vue {component_name}`.
*   **No snapshot tests.** Forbidden.
*   **`shallowMount` by default.** Children are stubbed.
*   **Stubbed components:** Assert `.exists()`, `.props()`, `.emitted()`. Do NOT assert text/internal rendering of stubs.
*   **Async:** `await wrapper.vm.$nextTick()` after state changes.
*   **matchMedia mocking:** `vi.stubGlobal('matchMedia', vi.fn().mockReturnValue(mockMediaQueryObject))` with `addEventListener`/`removeEventListener` spies.
*   **Store getter mocking:** `createMockStore(locationState, isDesktop)` pattern — second param configures `global` module getter.
*   **Mock globals before import:** Module-level code runs at import time.
*   **Emit before redirect:** When testing `trackMREventAndRedirect`, verify `$emit` fires first (redirect may navigate away).

### 1.7 Tracking

*   **`trackMREvent(eventName, properties)`** — Fire-and-forget. Use when the user **stays on the current page**. Examples: toggle buttons, viewport events, `<a>` with native `href`, new-tab opens (`target="_blank"`), page view events in watchers.
*   **`trackMREventAndRedirect(eventName, url, properties)`** — Track then navigate with 300ms delay. Use when the interaction triggers a **hard redirect** (`location.href`). The third `properties` param is optional. Examples: "Book Now" CTAs that redirect to booking flow.
*   **Anti-pattern:** Never `trackMREvent()` + `goToPath()` sequentially — `goToPath` does `location.href` immediately, event may not flush. Always use `trackMREventAndRedirect` for hard redirects.
*   **Page load events:** `watch` with `immediate: true`.
*   No direct `segmentTracking` or `window.analytics` — globally available via mixin.

### 1.8 Image & Loading Patterns

*   **Skeleton via `:deep(.image-box)` background (preferred):** Add `background-color ui-color-4` + `border-radius` + `height 100%` + `width 100%` on `:deep(.image-box)` inside the image container. The gray placeholder shows while the image downloads; the image covers it when loaded. No conditional classes, no JS tracking — purely CSS. Validated: HeroV2 (`.main-display`, `.secondary-display`), Services (`.service-img`).
*   **Skeleton for non-ImgBox containers:** For components that load asynchronously (e.g., Google Maps), add `background-color ui-color-4` directly on the wrapper element. Validated: About (`.map-thumb`).
*   **Complex components (carousel):** Handle skeleton state internally when `images` prop is empty (Reviews pattern with `v-if="isLoading"` + `.reviews-skeleton`).
*   **CMS image URL stripping:** When CMS media objects have URLs with baked-in dimension params (`?w=400&h=300&fit=crop...`), strip query params in the parent's computed to get full-res images: `url.split('?')[0]`. The CDN serves the full image, and ImgBox handles its own crop/format params. Validated: HeroV2 `galleryImages` computed.
*   **ImgBox auto alt text:** `ImgBox` reads `mediaObject.alt_text` automatically (imgBox.vue L80). Do NOT pass explicit `:alt` unless overriding CMS data. Exception: decorative images use `alt=""`.
*   **CMS SVG icons via `ImgBox`:** When the CMS provides SVG data in the `{ icon: { color, size, icon: { file_type: 'svg+xml', svg_data: {...} }, fill, originalColor } }` format, use `ImgBox(:media-obj="item.icon")` — NOT `mr-icon`. `ImgBox` detects `isNewSvg` (checks `mediaObj.icon.file_type === 'svg+xml'` + `svg_data`) and renders via `MrIcon` internally with correct color, size, and fill. Use `mr-icon` only for locally bundled SVGs from `src/assets/svg-icons/`.
*   **Local SVG icons via `mr-icon`:** Use `currentColor` for fill or stroke in the SVG file so CSS controls the color. No fixed `width`/`height` on the `<svg>` — use `viewBox` only and control size via Stylus. Validated: `star-solid.svg` in Reviews.
*   **Anti-pattern — `.has-skeleton` conditional class:** Deprecated. The old pattern toggled a class based on data availability, but it disappeared before the image finished downloading. Use the `:deep(.image-box) background-color` pattern instead.

### 1.9 PR Workflow

*   **Directive:** `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/ticket-pr-template.md`
*   **PR Title format:** `[DOTCOMPB-XXXX]: {Subtitle}`
*   **Labels:** `DOTCOM TEAM`, `Pending Code Review`
*   **Changes section:** Sub-list format (one detail per line, not dense paragraphs).
*   **Unit Testing table:** One row per individual test case (`| Component | Test | Status |`).

### 1.10 Accordion / Expand-Collapse Pattern

*   **CSS `max-height` transition (preferred):** Use `max-height: 0; overflow: hidden; transition: max-height 500ms ease-out` for collapse, `max-height: 100em; transition: max-height 500ms ease-in` for expand. Content stays in the DOM (no `v-if`) — use `:class="{ expanded: isOpen }"` and `aria-hidden` to toggle. No flash, no JS measurement. Validated: FAQs accordion. **Anti-pattern:** Do NOT use `TransitionExpand` component for accordion content — it causes a flash where content appears before the animation starts (uses `position: absolute` + `visibility: hidden` measurement that leaks a frame).
*   **ADA for accordion:** `<button>` with `aria-expanded` + `aria-controls` linking to the answer panel. Answer panel has `role="region"` + `aria-label` + `aria-hidden`. Track clicks with `trackMREvent`.

### 1.11 Self-Sufficient Component Spacing

Every imported component must handle its own spacing internally — the parent page should NOT add wrapper divs or spacing classes around them. This makes components reusable across pages without the parent needing to know about their spacing needs.

*   **Pattern:** Add `py-150m` (or appropriate value) directly on the component's root element in its own template. The component sits directly in the parent's column/container without a wrapper div.
    ```pug
    //- CORRECT — component handles own spacing:
    .main-column
      HairColorBarLocationAbout
      HairColorBarLocationServices(:services="servicesList")
      HairColorBarLocationReviews

    //- WRONG — parent adds wrapper with spacing:
    .main-column
      .about-section.pb-100m
        HairColorBarLocationAbout
      .services-section.py-100m
        HairColorBarLocationServices(:services="servicesList")
    ```
*   **Divider sections:** Components that need a divider add `.bottom-divider-light` on their own root alongside `py-150m`. Example: `.hcb-reviews.bottom-divider-light.py-150m`.
*   **Hardcoded inline sections** (Getting Here, Payments) keep their wrapper divs since they're not imported components — they're inline template content. They follow the same `.bottom-divider-light.py-150m` pattern.
*   **Repeating items** (review cards, skeleton cards, addon items): Use `.bottom-divider-light.pb-150m.mb-150m` with `:last-child` removing border + margin + padding in Stylus. Last item should have `margin-bottom: 0` to avoid extra space before the next section.
*   **V1 reference:** The three V1 footer components (`HairColorBarIndividualMoreLocations`, `HairColorBarIndividualRegionList`, `HairColorBarMoreInfo`) use internal `.inner-wrap` with `padding 1.5em 0` — same principle, different implementation. Our V2 components use `.py-150m` utility class on the root instead.
*   **Validated across:** About (`.hcb-about.pb-150m`), Services (`.hcb-services.py-150m`), Reviews (`.hcb-reviews.bottom-divider-light.py-150m`), FAQs (`.hcb-faqs.py-150m`). All wrapper divs removed from `HcbLocationPageV2`.

### 1.12 V1-to-V2 Component Reuse Strategy

*   **Evaluate before building:** Before creating a new component for V2, audit V1 equivalents (`HcbLocationPage`, `HcbIndividual` component trees) for reuse vs. rebuild.
*   **Reuse criteria:** If the V1 component is props-driven, Options API, and matches the new design with minor modifications → reuse. If it's tightly coupled to V1 layout, uses deprecated patterns, or needs major restructuring → rebuild.
*   **Integration pattern:** V1 components imported into `HcbLocationPageV2` must follow V2 conventions: self-contained landmarks (`role="region"` + `aria-labelledby` on component root), `v-if` guards for empty state, tracking events from the EVENT TRACKING spec.

### 1.13 CMS Partials Architecture

CMS Partials are a core content delivery mechanism in the MR platform. Understanding how they work is critical for any component that renders Tophat-configured content.

**What is a Partial?**
*   A Partial is a reusable HTML template configured in **Tophat** (the internal CMS/admin tool) and identified by a unique `mixin-key` string (e.g., `"partial-featured-services-v2"`).
*   Partials contain raw HTML that may reference **globally registered Vue components** (e.g., `<featured-services-v2>`). These are not imported locally — they are resolved at compile time because they are registered globally in `mrVueApp.js`.
*   Partial content (HTML, featured products, services) is configured entirely in Tophat — it does NOT come from `cmsSettings` props or the codebase. The data is fetched at render time from the CMS API.

**How CMS Partials render — the `CMSPartial` component:**
*   **Component:** `website/src/vuescripts/components/CMSPartial/CMSPartial.vue`
*   Receives a `mixin-key` prop identifying which partial to load.
*   Calls `cms` Vuex store's `loadPartial` action → fetches HTML from `/api/cmsSvc/getPartial`.
*   The returned HTML string is compiled as a Vue template at runtime.
*   Any globally registered Vue components referenced in the HTML are resolved and rendered as live Vue components (not static HTML).

**SSR behavior (critical):**
*   `CMSPartial` has a `serverPrefetch()` that runs during SSR when `isVueSSRApp` is true.
*   During SSR, the partial HTML is fetched, compiled, and rendered server-side — including all globally registered components referenced in it.
*   **This means those global components' lifecycle hooks, computed properties, and store dependencies all execute during SSR.** If a global component inside a partial accesses store state that hasn't been populated yet, the SSR render will crash with a 500 error.
*   **Example (discovered in this session):** `FeaturedServicesV2` is a globally registered component used inside `partial-featured-services-v2`. It accesses `hairColorBarBooking.servicesOfferedDictionary`, which calls `location.servicesOffered.forEach()`. If `hairColorBarBooking.location` is `null` during SSR, this crashes.

**Cross-store data sharing pattern for Partials:**
*   When a partial's global component depends on store data that another store module populates, the parent page must bridge the data explicitly.
*   Pattern: In the parent's `serverPrefetch`, after loading the primary data, copy it to the dependent store before the partial renders.
*   **Concrete example in `HcbLocationPageV2`:**
    ```javascript
    async serverPrefetch() {
      await this.loadLocation(this.routeParams.locationCode);    // populates colorbar.location
      this.setBookingLocation(this.location);                     // copies to hairColorBarBooking.location
    }
    ```
*   This ensures `FeaturedServicesV2` (inside the CMS Partial) has the data it needs when it renders during SSR.

**Key guidelines when using CMS Partials:**
1.  **Investigate dependencies:** Before adding a `CMSPartial`, check what globally registered components the partial HTML references. Read those components to understand their store dependencies.
2.  **Satisfy store dependencies for SSR:** If the page uses SSR, ensure all store state required by the partial's global components is populated in `serverPrefetch` BEFORE the partial's own `serverPrefetch` runs.
3.  **Cross-store bridging:** Use mapped mutations (e.g., `setBookingLocation`) to copy data between store modules when a partial's component depends on a different store than the page primarily uses.
4.  **Partials are NOT in cmsSettings:** Partial content comes from Tophat via the CMS API at render time, not from route props.
5.  **Global component registration matters:** If a partial references a component that isn't globally registered, it will render as an unknown element (no error, just empty). Verify registration in `mrVueApp.js`.

### 1.14 Component Naming & Location Conventions (Session-Specific)

*   **Self-explanatory, general names:** Component names should be general enough to allow reuse across the site, not overly specific to one page. E.g., `FeaturedDeals` not `HairColorBarLocationFeaturedDeals`.
*   **Location by domain, not by page:** Place reusable booking-related components in `HairColorBarBookingV2/components/` (where `PageIntro`, `FixedCtaBar` already live), not in `HcbIndividual/` (which is for page-specific sections).
*   **Exception for page sections:** Components that are truly page-section wrappers (like `HairColorBarLocationAbout`) belong in `HcbIndividual/` because they represent a specific section of the location page.
*   **Folder structure:** `ComponentName/ComponentName.vue` + `ComponentName/index.js` (barrel export). Tests co-located as `ComponentName.test.js`.
*   **Short CSS root classes:** Root element classes use the `hcb-` prefix (abbreviation for Hair Color Bar) instead of the full component name. E.g., `.hcb-page-v2` not `.hcb-location-page-v2-container`, `.hcb-hero-v2` not `.hair-color-bar-location-hero-v2`. Scoped styles make collision impossible.
*   **Short internal classes:** Remove redundant prefixes inferred from component context. Keep names self-explanatory — don't abbreviate to the point of ambiguity (`.service-description` not `.service-desc`).

### 1.15 Layout & CSS Class Naming Conventions

| Layer | Pattern | Purpose | Example |
|---|---|---|---|
| **Root element** | Component name (kebab-case class) | Identifies the component instance | `.hair-color-bar-location-hero-v2`, `.showcase-carousel` |
| **Structural primitive** | `.row` | Max-width constraint, top-level section wrapper | `.row.hero-section`, `.row.location-body` |
| **Layout container** | `{context}-{role}` | Describes what the area IS, not what it does | `.location-body`, `.location-gallery`, `.location-header` |
| **Grid columns** | `{role}-column` | Named by their purpose in the grid | `.main-column`, `.sidebar-column`, `.carousel-column` |
| **Semantic sections** | `{name}-section` | Named by their content, receives `aria-labelledby` | `.hero-section`, `.about-section`, `.services-section` |
| **Card/item children** | `{component}-{element}` | BEM-like, scoped to the card context | `.service-card`, `.service-image`, `.service-content` |

**Rules:** `.row` is a structural primitive, not a semantic element. Layout containers describe what they contain, not their CSS role. Grid column names use `{role}-column`. Semantic sections use `{name}-section`. Card children use `{component}-{element}` BEM-like naming. Display areas follow `.main-display` / `.secondary-display` for media-heavy layouts.

**Reference — `HcbLocationPageV2` layout structure:**
```
.hcb-page-v2
├── SiteMessageBannerCarousel
├── .row.hero-section
│   └── HairColorBarLocationHeroV2      <- self-sufficient, owns landmark
├── .row.location-body.py-125m          <- grid container
│   ├── .main-column
│   │   ├── HairColorBarLocationAbout   <- self-sufficient (.pb-150m)
│   │   ├── CMSPartial (marketingPartialLight)
│   │   ├── HairColorBarLocationServices <- self-sufficient (.py-150m)
│   │   ├── CMSPartial (marketingPartialDark)
│   │   ├── .getting-here-section       <- CMS-driven (v-if, .bottom-divider-light.py-150m)
│   │   ├── .payments-section           <- CMS-driven (v-if, .bottom-divider-light.py-150m)
│   │   ├── HairColorBarLocationReviews <- self-sufficient (.bottom-divider-light.py-150m)
│   │   └── HairColorBarLocationFAQs    <- self-sufficient (.py-150m)
│   └── .sidebar-column
│       └── FeaturedDeals               <- owns role="region" + aria-labelledby
├── HairColorBarIndividualMoreLocations <- V1 reuse
├── HairColorBarIndividualRegionList    <- V1 reuse
└── HairColorBarMoreInfo                <- V1 reuse
```

### 1.16 CMS-Driven Info Sections Pattern

*   **When to use:** For text content sections (e.g., "Getting Here / Parking", "Payments") that are configured per-location in Tophat.
*   **Place directly in the page template** (`HcbLocationPageV2`), not in a child component.
*   **Data source:** `cmsSettings.gettingHere` and `cmsSettings.payments` — plain text strings from Tophat textarea fields. Defaults managed by Tophat, not in code.
*   **`v-if` guard:** Hide section if CMS string is empty — `v-if="gettingHereText"`. No fallback defaults in computed — empty string means "not configured, don't show."
*   **Title style:** `.color-mr-purple.f-secondary.mb-25m.sm-f-xxlarge.max-at-tweak.upper`.
*   **Description style:** `.sm-f-medium.max-at-tweak` with `white-space: pre-wrap` in Stylus to preserve textarea newlines (`\n`). Use `pre-wrap` not `pre-line` — `pre-wrap` preserves consecutive newlines, `pre-line` collapses them.
*   **Divider:** `.bottom-divider-light.py-150m` — self-sufficient spacing pattern.

### 1.17 Andris Review Patterns (Extracted from 56 PR Comments)

> **Source:** 56 review comments from `andris310` across 10 PRs (#19523–#20064). 34 comments (61%) were already covered by existing rules. The 22 below are patterns not covered elsewhere — treat as mandatory constraints alongside Section 1 rules.

#### Frontend Architecture & Templates

*   **andris-guideline-6 — Single H1 per page:** Each page has exactly one `<h1>`. Section components use `<h2>` or lower. Components must never assume they own the page title.
*   **andris-guideline-7 — Omit redundant `div` in Pug:** `.my-class` auto-creates a `<div>`. Never write `div.my-class`. Only include the tag name for non-div elements (`section.my-class`, `span.label`).
*   **andris-guideline-8 — Use existing MrBtn variants:** `MrBtn` default uses `cta-color-1`. Before adding custom button CSS, check if `secondary`, `tertiary`, or `light` variant already works. Don't create per-color CSS classes — use the variant system.
*   **andris-guideline-12 — Extract, don't bloat:** When adding new functionality, create a standalone component instead of cramming mode flags into an existing one. If the feature can stand alone, it must be its own component.
*   **andris-guideline-15 — Keep simple expressions on one line:** Don't break trivial return statements or short values across multiple lines. Multiline only for genuinely complex or long expressions.

#### Code Quality & Style

*   **andris-guideline-2 — No info-only comments:** Don't add comments that merely describe what code does. Code is self-documenting via naming. Keep only: `// TODO:` with ticket refs, `// eslint-disable`, and explanations of genuinely non-obvious logic.
*   **andris-guideline-3 — One blank line between logical blocks:** Exactly one blank line between methods, computed properties, data fields, test cases, template sections, and style rules. No consecutive blanks (remove excess), no zero-separation walls (add one).
*   **andris-guideline-4 — Template methods must be traceable:** Every method called in the Pug template must be defined in `methods`, mapped via `mapActions`/`mapMutations`, or be a known global mixin method. Reviewers must trace every reference within the file.
*   **andris-guideline-5 — No formatting-only changes in unrelated files:** Don't include auto-lint/format corrections on files with no functional changes. Reformatting untouched code adds diff noise and reviewer cognitive load. Format only lines you're modifying.
*   **andris-guideline-20 — Clean programmatic strings:** No whitespace in programmatic string values (error codes, flags). If a string is used once and is self-explanatory, inline it — don't extract to a variable.

#### Component Design

*   **andris-guideline-10 — Vuex helpers at top of sections:** `...mapState()`/`...mapGetters()` go first in `computed`. `...mapActions()`/`...mapMutations()` go first in `methods`. Local definitions follow, alphabetized.
*   **andris-guideline-11 — No hardcoded route paths:** Don't hardcode route strings inside reusable components. Use `this.$route.path`, a prop, or a URL key. Hardcoded routes create hidden coupling.
*   **andris-guideline-14 — Styles inside the SFC:** All Vue component styles live in `<style scoped lang="stylus">`. No external `.styl`/`.css` files. Migrate existing externals into the SFC when touching the component.
*   **andris-guideline-16 — Comment third-party CSS overrides:** When overriding library classes (Swiper, Google Maps, Stripe) in `<style>`, add a comment identifying the library. Helps reviewers distinguish your classes from overrides.

#### Accessibility & UX

*   **andris-guideline-1 — Meaningful alt text:** Never use `alt=""` unless the image is purely decorative. Content images (maps, photos, products) must have descriptive alt text.
*   **andris-guideline-17 — Verify error messages match user intent:** Before implementing error messages, verify the copy aligns with the user's action context. Flag with designer/PM when in doubt.

#### Backend / API Patterns

*   **andris-guideline-18 — No `console.error` on backend:** Use `log.error()` for server logging and return structured error responses.
*   **andris-guideline-19 — Backend returns error codes, not HTML:** Return structured codes (`MISSING_INFO`, `UPDATE_ERROR`) and let the Vue component handle display. No user-facing text in routes/controllers.
*   **andris-guideline-21 — Never string-match response messages:** Don't check `response.message.includes('...')` for control flow. Use `response.success`, `response.errorCode`, or HTTP status codes.
*   **andris-guideline-22 — Use existing validation systems:** When a component has Vuelidate or another validation system, integrate new rules into it. Don't add a parallel error display mechanism.

#### Images & Assets

*   **andris-guideline-9 — No static images in the codebase:** Never commit PNG/JPG/GIF to the repo. Use CMS Media Gallery (Tophat) for all images. Only exception: SVG icons in `src/assets/svg-icons/`.

#### Experiments

*   **andris-guideline-13 — Track experiment exposure explicitly:** When implementing A/B experiments, explicitly call tracking when the customer sees a variant. Branching logic alone is not enough — track in `mounted` or watcher with `immediate: true`.

### 1.18 Code Review Checklist (45 Rules)

> Reference this table when running `/code-review` on any component in this session. Each row maps to the source rule. Load the referenced skill/section before reviewing. Validated on `SiteNavShopContent.vue` and `SiteNavMobileWrapper.vue` (2026-03-17) — 45/45 passed.

| # | Category | Pattern | Source |
|---|---|---|---|
| 1 | **Template** | All templates use Pug (`lang="pug"`), no HTML | `mr-dotcom-dev/rules/pug-templates.md` §1 |
| 2 | **Template** | Vue components in kebab-case in Pug (`mr-btn`, not `MrBtn`) — project uses PascalCase for local refs (skip) | `mr-dotcom-dev/rules/pug-templates.md` §1 |
| 3 | **Template** | Heading inline text (`h2.classes Title`), not multi-line `\|` | Session §1.2, decision 22 |
| 4 | **Template** | `v-if` guards on all sections/images where data may be null | Session §1.3, `mr-dotcom-dev/rules/vue-patterns.md` §9 |
| 5 | **Script** | Options API only — no Composition API, no `<script setup>` | Session §1.1, `mr-dotcom-dev/rules/vue-patterns.md` §2 |
| 6 | **Script** | Canonical field order: `name → components → emits → props → data → computed → watch → lifecycle → methods` | Session §1.3, `mr-dotcom-dev/rules/vue-patterns.md` §2 |
| 7 | **Script** | `emits` declared explicitly when component emits events | `mr-dotcom-dev/rules/vue-patterns.md` §2 |
| 8 | **Script** | Computed properties alphabetized | Session §1.3, `mr-dotcom-dev/rules/vue-patterns.md` §3 |
| 9 | **Script** | Vuex helpers at top of `computed`/`methods` | Session §1.17 (andris-guideline-10) |
| 10 | **Script** | No unused imports, methods, computed, data | `mr-dotcom-dev/rules/vue-patterns.md` §11 |
| 11 | **Script** | Optional chaining (`?.`) always — no `getObjProperty` | Session §1.3 |
| 12 | **Script** | Module-level constants for static data | Session §1.3 |
| 13 | **Script** | No inline event logic — `@click` calls a single method | Session §1.3 |
| 14 | **Styling** | Scoped Stylus (`style scoped lang="stylus"`) | Session §1.2 |
| 15 | **Styling** | Utility-first for padding, margin, color, font-size, text-align, border-radius, font-weight, text-transform | Session §1.2, `mr-style` |
| 16 | **Styling** | Do NOT use utility classes for flex layout — keep in Stylus | Memory: `feedback_no_flex_utility.md` |
| 17 | **Styling** | `.max-at-tweak` mandatory on every responsive font class | Session §1.2 |
| 18 | **Styling** | Design system variables for colors — no hardcoded hex unless no match | Session §1.2 |
| 19 | **Styling** | CSS properties alphabetized within style blocks | Session §1.2 |
| 20 | **Styling** | No padding/margin in Stylus when utility handles it. Stylus only for `@media` overrides | Session §1.2 |
| 21 | **Styling** | Stylus nesting mirrors template hierarchy | `mr-style` |
| 22 | **Naming** | Root class: short component prefix | Session §1.14 |
| 23 | **Naming** | Remove redundant prefixes from parent context | Session §1.14 |
| 24 | **Naming** | Card children: `{parent}-{element}` pattern | Session §1.15 |
| 25 | **Naming** | Grid columns: `{role}-column` | Session §1.15 |
| 26 | **Naming** | No BEM `__` or `--` — hyphens only | `mr-style` |
| 27 | **ADA** | Self-contained landmarks in same component | Session §1.5 |
| 28 | **ADA** | `aria-hidden="true"` on decorative images adjacent to text | Session §1.5 |
| 29 | **ADA** | `role="list"` on `<ul>` with `list-style: none` | ADA review finding |
| 30 | **ADA** | Heading levels: no jumps (`h1` → `h2` → `h3`) | WCAG 1.3.1 |
| 31 | **ADA** | `:focus-visible` outline on custom interactive elements | Session §1.5 |
| 32 | **ADA** | No nested interactive elements | Session §1.5 |
| 33 | **ADA** | No duplicate landmarks when nested | ADA review finding |
| 34 | **Images** | Use `ImgBox` for all images — no raw `<img>` | `mr-dotcom-dev/rules/vue-patterns.md` §11 |
| 35 | **Images** | Skeleton via `:deep(.image-box)` with `background-color ui-color-4` | Session §1.8 |
| 36 | **Images** | `:deep(img)` for aspect-ratio, border-radius, object-fit | Session §1.8 |
| 37 | **Tracking** | `trackMREvent` for fire-and-forget, `trackMREventAndRedirect` for navigation | Session §1.7 |
| 38 | **Tracking** | Every template method must be traceable | Session §1.17 (andris-guideline-4) |
| 39 | **Code Style** | One blank line between logical blocks | Session §1.17 (andris-guideline-3) |
| 40 | **Code Style** | No info-only comments | Session §1.17 (andris-guideline-2) |
| 41 | **Code Style** | Minimal-touch on unrelated code | Session §1.17 (andris-guideline-5) |
| 42 | **MrBtn** | Use variants (`secondary`, `tertiary`, `light`) before custom CSS | Session §1.17 (andris-guideline-8) |
| 43 | **MrBtn** | `:deep(.mrbtn)` for color overrides | Code review pattern |
| 44 | **Components** | Shared components at `SiteNav/` level | Decision 37 |
| 45 | **Components** | Thin wrapper pattern: mobile wrapper + shared content | Architecture decision |

---

## SECTION 2: SESSION OVERVIEW

> This section provides the overall context, purpose, and scope of the Site Revolution Redesign session.

### 2.1 Purpose

This session covers the **Site Revolution Redesign** for the Hair Color Bar (HCB) section of the Madison Reed website. The primary goal is a complete visual and functional overhaul of the HCB location-specific pages, including a new hero section, about section, image gallery, booking flow integration, services display, FAQs, reviews, marketing modules, and additional locations.

### 2.2 Scope

| Ticket          | Type  | Summary                                                      | Status                                                                  |
|-----------------|-------|--------------------------------------------------------------|-------------------------------------------------------------------------|
| `DOTCOMPB-7289` | Story | Specific Location page updates — HCB details                 | In Code Review (hero, about, page scaffolding complete)                 |
| `DOTCOMPB-7290` | Story | Specific Location page updates — Services + additional info  | **ALL ACs COMPLETE.** PR #20190 open. 143 tests passing. Code reviewed. |
| `DOTCOMPB-7556` | Bug   | Add sticky Book Services button to location page             | Complete (FixedCtaBar created, integrated, tested, PR ready)            |
| `DOTCOMPB-7557` | Bug   | ADA: Cannot tab to Book Services on desktop                  | Roam node created, not yet implemented                                  |
| `DOTCOMPB-7463` | Story | Navigation Redesign                                          | PR #20210 open. CI failing (core_integration_tests + core_tests). Reviews dismissed. Bug fixes on `DOTCOMPB-7463-nav-bug-fixes`. |
| `DOTCOMPB-7652` | Bug   | Madi overlapping with sticky CTA                             | Complete (MountedFlag + SierraWidget CSS fix, committed)                |
| `DOTCOMPB-7555` | Bug   | Remove non-functional "Photos" button from location hero     | **Complete** — committed, 13 tests passing, PR ready                   |
| `DOTCOMPB-7749` | Story | Nav title font size increase + CTA implementation            | In progress — font size bump done on `DOTCOMPB-7463-nav-bug-fixes`, CTA implementation pending |
| `DOTCOMPB-7763` | Bug   | Mobile Shop submenu not fully scrollable — iOS Safari overlap | **MERGED** (PR #20317, 2026-03-26). iOS scroll fix + header spacing + double-tracking fix. 52 tests passing. |
| `DOTCOMPB-7742` | Bug   | Featured service CTA on location page doesn't pre-select service in booking flow | **COMPLETE** (2026-03-30). Implemented, code reviewed (0 violations), 19 tests passing, 5263 full suite. Changes UNSTAGED — needs commit + PR. See §3.10. |
| `DOTCOMPB-7712` | Story | New page to display location photos                          | **IN PROGRESS** (2026-04-03). V1+V2 migration done. Photos page: hero pair + CSS masonry + CMS/DB image merge. 81 tests across 5 files. 2 code review rounds (48 findings, 12 implemented). Express 404, hydration fix, skeleton backgrounds, ADA fixes. NEXT: PR prep + commit. See §3.11. |

### 2.3 Key Architectural Decisions (Session-Wide)

1. **(~2026-03-02, DEPRECATED 2026-03-15)** **~~`@ready` event pattern~~ → Self-contained landmarks** — Replaced with self-contained landmarks: each component owns `role="region"` + `aria-labelledby` + heading `id` in its own template. Parent wrappers are purely structural with no ARIA attributes.
2. **(~2026-03-02)** **Parent data preparation** — Parent filters CMS data (e.g., invalid image entries) in computed properties before passing to children.
3. **(~2026-03-02)** **Responsive component swapping** — `v-if="isMobile"` to render entirely different child components for mobile vs. desktop.
4. **(~2026-03-02)** **Shared component enhancement** — Add boolean props (e.g., `staticMode`) to shared components for contextual behavior, rather than CSS overrides.
5. **(~2026-03-05)** **Centralized breakpoint for show/hide** — `global/isDesktop` Vuex getter (960px+) for desktop detection. Local `matchMedia` only for non-standard thresholds.
6. **(2026-03-12)** **V1-to-V2 reuse evaluation** — Before creating new section components for DOTCOMPB-7290, audit existing V1 components for reuse vs. rebuild.
7. **(2026-03-13)** **Cross-store data sharing for CMS Partials** — When a CMS Partial references globally registered components that depend on a different Vuex store module, the parent page must bridge data via mapped mutations in `serverPrefetch`.
8. **(2026-03-13)** **ID on heading element, class on root container** — `aria-labelledby` must point to a **heading** (`h2#my-section-title`) whose text content becomes the accessible name. Root container elements use **class selectors**, not IDs.
9. **(2026-03-13)** **Never use raw `<button>` for navigation** — Use `<a>` with `:href` + `@click.prevent` for actions that track + navigate. Use `MrBtn` for true button interactions (toggles, submissions).
10. **(2026-03-13)** **Utility-first is exhaustive** — Every CSS property that has a utility class equivalent MUST use the utility class in the template. Stylus is only for properties with no utility equivalent.
11. **(2026-03-13)** **CMS-driven add-ons via `ImgBox`, not `mr-icon`** — Add-on service data icons use CMS SVG format, rendered by `ImgBox` which detects `isNewSvg` internally.
12. **(2026-03-13)** **Carousel overflow fix — viewport-relative max-width** — Swiper carousel flex content can exceed viewport. Fix: `max-width: calc(100vw - 2.5em)` on the carousel wrapper. `max-width: none` at 1024px+ when grid constrains width naturally.
13. **(2026-03-13)** **Hardcoded info sections in page template** — Static content like "Getting Here / Parking" and "Payments" lives directly in `HcbLocationPageV2` template, not in child components.
14. **(2026-03-15)** **Constants in templates use camelCase computed wrappers** — Module-level `UPPER_SNAKE_CASE` constants exposed to Pug templates via `camelCase` computed properties.
15. **(2026-03-15)** **Full-stack Birdeye reviews via widget scraping** — `BirdeyeAPI.getReviewsViaWidget()` scrapes the widget's `__NEXT_DATA__` JSON → controller → webservice → Vuex action → custom review cards.
16. **(2026-03-15)** **Backend code review pattern: match existing file patterns** — When adding new functions to existing files, audit adjacent functions for error logging patterns, third-party API logging, and callback/error handling shapes. Apply matching patterns 100% without changing logic.
17. **(2026-03-15)** **`#EFEFF1` is an intentional design-specific hex color** — Used in Reviews `.see-more-btn` and About `.location-about-data :deep(.mrbtn)`. No matching design system variable. Keeping as hardcoded hex is correct.
18. **(2026-03-15)** **Tracking function selection: `trackMREvent` vs `trackMREventAndRedirect`** — `trackMREvent` for fire-and-forget (user stays on page). `trackMREventAndRedirect` for hard redirects (300ms delay to ensure Segment flushes before navigation).
19. **(2026-03-15)** **Map thumbnail: `<div role="link">` over `<a>` for interactive children** — `<a>` wrapping `HcbGmap` caused nested interactives ADA violation.
20. **(2026-03-15)** **`.upper` mandatory on all `.f-secondary` headings** — Kapra Neue is designed for uppercase.
21. **(2026-03-15)** **Heading inline text format** — All Pug headings use inline text (`h2.classes Title`) not multi-line.
22. **(2026-03-15)** **DashHudson NOT integrated into hero gallery** — No per-location gallery API. DO NOT attempt future integration by merging DashHudson images into `galleryImages`. If needed, implement as a separate section below the hero.
23. **(2026-03-15)** **FAQs: CSS `max-height` accordion, not `TransitionExpand`** — `TransitionExpand` causes a content flash. V1 pattern uses pure CSS `max-height` with `overflow: hidden` + transition.
24. **(2026-03-15, UPDATED 2026-03-16)** **Self-sufficient component spacing** — Each component handles its own spacing via utility classes on root. NO wrapper divs on parent.
25. **(2026-03-16)** **V1 footer components reused directly** — `HairColorBarIndividualMoreLocations` + `HairColorBarIndividualRegionList` + `HairColorBarMoreInfo` reused as-is from V1.
26. **(2026-03-16)** **CMS settings refactor** — `cmsSettings.hero.defaultLocationImages` → `cmsSettings.defaultLocationImages` (flattened). Added `cmsSettings.gettingHere` and `cmsSettings.payments`. HeroV2 prop renamed from `heroSettings` (Object) to `heroImages` (Array).
27. **(2026-03-16)** **`white-space: pre-wrap` for CMS textarea content** — Preserves all `\n` newlines from Tophat textareas (including consecutive). Applied to Getting Here/Payments sections, `.service-description`, and MarketingBanner `.banner-description`. Use `pre-wrap` not `pre-line`.
28. **(2026-03-16)** **Marketing Modules via CMS Partials + globally registered `MarketingBanner`** — `MarketingBanner` registered in BOTH `mrVueApp.js` AND `registerGlobalsSsr.js`. `isBookServiceCta` computed auto-generates location-specific booking URL. *(Deprecated 2026-03-19 — see §2.3 decision 40 and §3.8)*
29. **(2026-03-16)** **Global component SSR registration** — When creating globally registered components used inside CMS Partials, register in BOTH `mrVueApp.js` (client) AND `registerGlobalsSsr.js` (SSR via `defineAsyncComponent`).
30. **(2026-03-16)** **Intentional hex colors** — `#9A8CAD` (MarketingBanner light theme outer bg) and `#EFEFF1` (About/Reviews button bg) have no matching design system variables. Keep as hardcoded hex.
31. **(2026-03-16)** **Minimal-touch on existing files (DOTCOMPB-7463)** — When modifying an existing file, ONLY the lines being added or changed follow session guidelines and patterns. Do NOT refactor, improve, restyle, rename, reformat, or "fix" surrounding code.
32. **(2026-03-16)** **Navigation data is Tophat-driven via Data Tool** — All navigation content comes from `dataToolSvc.getData({ mixinKey: 'top-nav' })` stored in `siteNav` Vuex module.
33. **(2026-03-16)** **Design system color mappings discovered** — `#3A2D4A` = `brand-color-1` = `color-mr-purple`. `#343434` = `text-color-1`. `font-size-grande` = 36px → utility class `xs-f-grande`.
34. **(2026-03-17)** **New Tophat data object `sr-top-nav`** — Replaces `top-nav` for Site Revolution nav. New ShopNav structure: `featuredTools[]`, `shopCollections[]`, `shopSolutions[]`, `quickActions[]`, `marketingSlot`. Store `siteNav.js` switched `mixinKey` from `'top-nav'` to `'sr-top-nav'`.
35. **(2026-03-17)** **Nav breakpoint reverted to 960px** — `SiteNavShopContent` CSS handles all viewports internally.
36. **(2026-03-17)** **V3 ShopNav components created alongside V2** — New `SiteNavShopContent/` and `SiteNavMobileWrapper/` folders created. Imports swapped in parent components. Allows easy rollback by reverting imports.
37. **(2026-03-17)** **Section title utility class consolidation** — When an element exceeds 4 utility classes, move font-family/text-transform/color into the scoped class (`.section-title`). Keep only responsive font-size classes as utilities.
38. **(2026-03-17)** **`NavCTAs` dynamic from Tophat** — Replaced hardcoded "Refer & Earn $15" with CMS-driven CTAs. Root-level `NavCTAs[]` array in `sr-top-nav` object.
39. **(2026-03-18)** **DOTCOMPB-7555 scope: Photos button removal only** — Ticket misread initially as "add carousel for all breakpoints." Actual scope: remove only the non-functional `.more-photos` CTA, `handleImageGalleryClick()` (TODO stub since launch), `additionalImagesCount` computed, and `VISIBLE_HERO_IMAGES_COUNT` constant from `HairColorBarLocationHeroV2`. The 2-column static desktop layout (`primaryHeroImage`, `secondaryHeroImage`, `isMobile` matchMedia) remains untouched. Full desktop banner carousel (`LocationImageBannerCarousel`) is built and parked on `DOTCOMPB-7555_full_width` — DO NOT MERGE until business confirms.
40. **(2026-03-19)** **`CmsPartialSsr` `clientConfig` replaces `MarketingBanner` URL workaround** — `CmsPartialSsr` accepts a `config` prop (Object) and passes it as `clientConfig` to the dynamically compiled CMS template component. This allows CMS partial HTML templates to reference `clientConfig.bookingUrl` directly (e.g., `:href="clientConfig.bookingUrl"`). Andris's PR #20229 (DOTCOMPB-7717) leveraged this to fix location-specific booking URLs by switching from `CMSPartial` → `CmsPartialSsr` and passing `{ bookingUrl }` as config. This made the `isBookServiceCta` text-sniffing workaround in `MarketingBanner` fully redundant — see §3.8 for cleanup plan.
41. **(2026-03-24)** **`siteNav.js` null guard pattern** — `res.data || {}` before destructuring CMS API responses prevents `TypeError` when `res.data` is null (Sentry error on `siteNav.js#L60`). Pattern: `const data = res.data || {}; commit('setX', data.X)`.
42. **(2026-03-24)** **`SiteNavShopContent` mandatory optional chaining** — All CMS data item accesses (`.link.text`, `.link.href`) in both template and script must use `?.`. Audited 17 spots, all fixed. Rule: never access nested properties on CMS objects without `?.`.
43. **(2026-03-24)** **`SiteNavDesktopV2` nav title font size scale** — Bumped one step for DOTCOMPB-7749: `xs-f-small` → `xs-f-medium` (14→16px), `md-f-medium` → `md-f-xmedium` (16→18px), `lg-f-xsmall` → `lg-f-small` (12→14px), `xl-f-small` → `xl-f-medium` (14→16px). Applied to all 5 nav title elements.
44. **(2026-03-24)** **Design system hex matches** — `#F7F7F8` = `ui-color-3`. `#EEEEEE` has no exact match (closest: `ui-color-4 = #eaeaea`). Keep as hardcoded hex if exact value required.
45. **(2026-03-27)** **Single scroll container per mobile nav panel** — Each mobile nav panel owns exactly ONE scroll container. No nested scroll containers. MainNav: root element (`flex: 1` + `overflow-y: auto`). SubNav/AboutNav: `.nav-content` child (root is flex column, header pinned, `.nav-content` gets `flex: 1` + `overflow-y: auto`). ShopNav: `SiteNavShopContent` owns scroll (`height: 100%` + `overflow-y: auto` at `mq-desktop-md-less`). `SiteNavMobileWrapper` is purely structural — NO `overflow-y` on the wrapper.
46. **(2026-03-27)** **Safe area padding values by panel** — `SiteNavShopContent`: `calc(8em + env(safe-area-inset-bottom))` (denser content, scroll container inside wrapper). MainNav/SubNav/AboutNav: `calc(4em + env(safe-area-inset-bottom))` (standard clearance for Safari toolbar + home indicator).
47. **(2026-03-27)** **No flex on `SiteNavMobileWrapper`** — Adding `display: flex`, `flex-direction: column`, or `height: 100%` to the wrapper breaks the ShopContent scroll behavior. Wrapper must remain a simple structural container. ShopContent handles its own scroll at mobile via `height: 100%` + `overflow-y: auto` in `@media mq-desktop-md-less`. Wrapper's `.pb-400m` was redundant once ShopContent got proper safe area padding — removed.
48. **(2026-03-27)** **`overflow-y: auto` over `overflow-y: scroll`** — All mobile nav panels use `auto` (scrollbar only when needed) not `scroll` (always shows). SubNav and AboutNav changed from `scroll` to `auto`.
49. **(2026-03-27)** **CSS `padding` shorthand overrides `padding-bottom`** — `padding: 1em` resets all four sides, killing a more specific `padding-bottom` from a parent media query. Always add explicit `padding-bottom` AFTER the shorthand when safe area clearance is needed. Caught on `SiteNavShopContent` where `@media mq-mobile` `padding: 1em` was overriding the safe area `padding-bottom` from `@media mq-desktop-md-less`.
50. **(2026-03-30)** **Cookie-based service pre-selection for `FeaturedServicesV2`**
51. **(2026-03-30)** **Non-CMS page pattern for location photos** — Photos page uses direct `res.render()` → Pug → Vue hydration, bypassing CMS entirely. Pattern: `res.render('hcb-location-photos/hcb-location-photos', { locationCode, locationName, locationImages, content })`. Pug template extends `vue-layout.pug`, passes Express locals as static HTML attributes to Vue component props. Reference routes: `hcb-addon/message.pug`, `dashboard.pug`, `shop-menu.pug`.
52. **(2026-03-30)** **Express route guidelines from 18-route analysis** — Documented in roam node `2026-03-30-150000-dotcompb_7712.org` DEVELOPMENT AC section. Template path convention: `<feature-dir>/<template-name>` → `views/desktop/<feature-dir>/`. Locals: layout flags (`simpleFooter`, `headerClass`) + nested `content` for meta tags + flat data props. Error: `res.code(404)` / `res.code(500)`, always `return`. Terminal routes use `res.render()`, passthrough uses `next()`.
53. **(2026-03-30)** **CMS + cache image merging for photos page** — Express route loads CMS page content via `cms.loaders.getLoader().loadPage()` to get `defaultLocationImages` (hero images) AND `colorbarCache.getLocation()` for `carouselImages` + `headerImage`. Deduped by `_id`. CMS images first (hero primary/secondary), cache images second.
54. **(2026-03-30)** **Recovered `MrBtn.more-photos` from DOTCOMPB-7555** — Commit `2dfd59d4a1f` removed the non-functional Photos button. DOTCOMPB-7712 restores it with real navigation: `handleImageGalleryClick()` now uses `trackMREventAndRedirect` to `/colorbar/locations/{code}/photos`. Same constant (`VISIBLE_HERO_IMAGES_COUNT`), computed (`additionalImagesCount`), template, and styles recovered. Button inside `.secondary-display` (which has `position: relative`).
55. **(2026-03-30)** **MrBtn has `position: relative` internally** — `.mrbtn` class sets `position: relative` (line 218 of MrBtn.vue). When placing MrBtn with `position: absolute`, MrBtn's internal style overrides after hydration. Workaround: keep original `.more-photos` scoped styles; the scoped attribute specificity handles it in practice.
56. **(2026-03-30)** **`carouselImages` from `colorbarCache` are raw Tophat media objects** — Shape: `{ _id, site, file_name, file_type, file_size, url, width, height, alt_text, aspects[], versionInfo[], ... }`. Have top-level `width` and `height` for dynamic aspect ratio computation. `ImgBox` handles URL construction from these objects via `:media-obj`.
57. **(2026-03-30)** **CSS Grid masonry with pixel-perfect aspect ratios** — `grid-auto-rows: 1px` + `row-gap: 0` + `column-gap: 0.5rem`. Each item gets `grid-row: span N` where N = exact pixel height computed from `ResizeObserver`-measured grid width + image `width`/`height`. Separate `--row-span` and `--row-span-desktop` CSS custom properties because column spans differ per breakpoint (desktop capped at 50%).
58. **(2026-03-30)** **`fitRowsToGrid()` row organization** — Organizes images into rows summing to exactly `GRID_COLUMNS` (6). Only expands the LARGEST item in each row (guard: `largest[spanKey] >= MIN_COL_SPAN + 1` = span 3+). Small images (span 2) never get stretched. `grid-auto-flow: dense` fills remaining gaps. — `FeaturedServicesV2.selectService()` must use the `selected_service` cookie + `trackMREventAndRedirect` to services URL (same pattern as `HairColorBarLocationServices`). The Vuex `setSelectedService` + `$router.push` approach doesn't survive cross-app page reloads. Maxi's `ServicesPage.setServiceFromCookie()` (PR #20308) handles consumption. Additionally, `HcbLocationPageV2.serverPrefetch` must call `hairColorBarBooking/getLocation` (booking endpoint) to populate `servicesOffered` — without it, `FeaturedServicesV2` silently renders nothing.
59. **(2026-04-02)** **SsrApp.vue has NO root `<router-view>`** — CMS HTML is injected via string interpolation into a dynamic template (`${self.htmlContent}`). `entry/server.js` line 28 confirms: "SSR doesn't render `<router-view>`". The `router-view` inside `HcbLocationPageV2` is the ONLY one on CMS pages. Zero dual-render conflict risk when registering location routes.
60. **(2026-04-02)** **Dual-route section extraction architecture for DOTCOMPB-7712** — `HcbLocationPageV2` becomes a thin parent (data loading + `router-view` only). ALL visible content extracted into `HcbLocationSections` (new component). Two routes: `/colorbar/locations/:locationCode` → `HcbLocationSections`, `/colorbar/locations/:locationCode/photos` → `HcbLocationPhotosPage`. Vue Router swaps between them — no `v-if`.
61. **(2026-04-02)** **`galleryImages` computed ownership moved to `HcbLocationPageV2` parent** — Filter/URL-strip logic (was in `HairColorBarLocationHeroV2`) moves to the parent so BOTH child routes (sections + photos) receive the same pre-computed array. HeroV2 prop changes from `heroImages` (raw CMS) to `galleryImages` (pre-filtered).
62. **(2026-04-02)** **`HcbLocationSections` uses static import, `HcbLocationPhotosPage` uses dynamic** — Sections is the main page content and MUST render synchronously during SSR. Photos page is lazy-loaded on client navigation only.
63. **(2026-04-03)** **Express 404 validation for `/colorbar/locations/:urlKey/photos`** — Validates location exists via `colorbarCache.getLocation` before CMS catch-all. Invalid → `res.code(404)`. No URL rewrite (caused hydration mismatch). Just validates and calls `next()`.
64. **(2026-04-03)** **CSS-only responsive columns for photos masonry (Option A)** — Replaced JS `matchMedia` + `columnCount` data with CSS `column-count: 2/3/4` via media queries. Zero hydration mismatch. SSR and client produce identical HTML. Images in flat list, CSS handles distribution.
65. **(2026-04-03)** **Photos page hero pair + masonry layout** — First 2 images (`heroImages` computed) render full-width stacked on mobile, 50% side-by-side on desktop. Remaining images (`masonryImages` computed) render in CSS masonry grid below. `HERO_IMAGE_COUNT = 2` constant.
66. **(2026-04-03)** **Back/close on photos page uses `trackMREventAndRedirect` (hard redirect)** — `$router.push` to `location-details` doesn't work on direct `/photos` access (CMS page has different context, sections don't render properly). Hard redirect via `trackMREventAndRedirect` ensures full SSR cycle with correct `cmsSettings`.
67. **(2026-04-03)** **`v-if="location?.code"` guard on `router-view`** — Defense-in-depth on thin parent. Initial Vuex state is `{}` (empty object, truthy), so `v-if="location"` is insufficient. `?.code` catches both `null` and empty `{}`.
68. **(2026-04-03)** **Padding/margin utility classes MUST use breakpoint prefix** — `.xs-pt-50m` not `.pt-50m`. Mobile-first convention. Rule added to code-review skill `rules/mr-review-checklist.md`.

### 2.4 PR Review Resolutions (DOTCOMPB-7289)

8 comments addressed: magic numbers → named constants; repeated boolean logic → `isSelected(slideImage)` method; oddly specific em values → reverted to `px`; inline event logic → dedicated methods; placeholder button → `// TODO:` comment; resize listener → `window.matchMedia` API; `@hook:mounted` → custom `@ready` event (later deprecated); `getObjProperty` → optional chaining (`?.`).

### 2.5 Pending Work

*   **DOTCOMPB-7463-nav-bug-fixes** — Mobile nav scroll normalization + icon spacing + safe area padding changes are **UNSTAGED**. Tests need to run before committing. See §3.5 for full change list.
*   **DOTCOMPB-7749** — CTA implementation for nav pending on `DOTCOMPB-7463-nav-bug-fixes`. Font size bump done. PR prep in `~/.brain.d/roam-nodes/madison_reed/2026-03-16-043543-dotcompb_7463.org`.
*   **DOTCOMPB-7463** — PR #20210 open. CI failing (`core_integration_tests` + `core_tests`). Re-review needed (reviews dismissed). Tophat `featuredTools.title` field, QA verification.
*   **DOTCOMPB-7290** — PR [#20190](https://github.com/MadisonReed/mr/pull/20190) open. Cherry-picked `CmsPartialSsr` fix (commit `329bce55079`). `partial-featured-services-v2` rendering issue root-caused — see §3.10.
*   **DOTCOMPB-7742** — **COMPLETE** (2026-03-30). Changes UNSTAGED on branch `DOTCOMPB-7742`. Needs commit + PR creation. Commit msg and PR body in roam node `2026-03-27-120100-dotcompb_7742.org`.
*   **DOTCOMPB-7555** — Complete on branch, PR description ready in roam node. Needs PR creation.
*   **DOTCOMPB-7555_full_width** — Parked carousel work. Activate only when business confirms desktop banner carousel for location hero.
*   **DOTCOMPB-7557** — ADA: Cannot tab to Book Services on desktop. Roam node exists, not started.
*   **DOTCOMPB-7712** — **IN PROGRESS** (2026-04-03). Implementation complete. 81 tests, 2 code review rounds. All changes UNSTAGED on branch `DOTCOMPB-7712`. NEXT: PR prep + commit. See §3.11.
*   **DOTCOMPB-7717 cleanup** — **COMPLETE** (2026-03-24). MarketingBanner dead workaround removed. Commit + PR body in §3.8.

---

## SECTION 3: FEATURE / TICKET IMPLEMENTATIONS

> Each subsection documents a specific ticket's implementation: what was built, where it lives, what decisions were made, and how to verify it.

---

### 3.1 DOTCOMPB-7289: Specific Location Page Updates (HCB Details)

**Created:** ~2026-03-02 | **Last updated:** 2026-03-13
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-02-131328-dotcompb_7289.org`
**Branch:** `DOTCOMPB-7289_new_feat` → `feat-website-booking-flow-site-revolution_with_performance`
**Status:** In Code Review. Hero, about, and page scaffolding complete. PR reviewed (8 comments resolved).

**Component Tree:**
```
HcbLocationPageV2 (page orchestrator)
├── HairColorBarLocationHeroV2   — hero section with gallery
│   ├── PageIntro                — reusable heading + slot
│   ├── MrBtn                    — desktop "book service" (.lg-only)
│   ├── ImgBox                   — desktop grid images (×2)
│   └── LocationImageCarousel    — mobile-only (≤559px) Swiper gallery
└── HairColorBarLocationAbout    — about/details section
    ├── MrBtn                    — show more/less toggle (aria-expanded)
    └── HcbGmap                  — Google Maps thumbnail (staticMode, div role="link")
```

**Key facts:** `HcbLocationPageV2` uses `serverPrefetch` to load location then bridge to `hairColorBarBooking` store. `HairColorBarLocationHeroV2` uses local `matchMedia('max-width: 559px)')` for mobile detection. `galleryImages` strips CMS URL params via `url.split('?')[0]`. `todayIndex` is a `computed` (not `data`) to avoid SSR timezone hydration mismatch. Map thumbnail uses `<div role="link">` (not `<a>`) to prevent nested interactives ADA violation.

---

### 3.2 DOTCOMPB-7556: Sticky Book Services Button

**Created:** ~2026-03-11 | **Status:** Complete — 35 tests passing, PR ready.
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-11-165514-dotcompb_7556.org`

**`FixedCtaBar`** — `website/src/vuescripts/components/HairColorBarBookingV2/components/FixedCtaBar/` — Fixed bottom CTA bar. Props: `visible`, `ctaText` (required), `trackEventName`, `redirectUrl`, `ctaDisabled`, `ctaLoading`, `ariaLabel`. Emits: `cta-click`. No store coupling.

---

### 3.3 DOTCOMPB-7557: ADA — Cannot Tab to Book Services on Desktop

**Created:** ~2026-03-10 | **Status:** Roam node created. Not yet implemented.
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-10-122138-dotcompb_7557.org`

---

### 3.4 DOTCOMPB-7290: Specific Location Page — Services + Additional Info

**Created:** 2026-03-12 | **Last updated:** 2026-03-24
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-12-114716-dotcompb_7290.org`
**Branch:** `DOTCOMPB-7290` → `feat-website-booking-flow-site-revolution_with_performance`
**PR:** [#20190](https://github.com/MadisonReed/mr/pull/20190) | **Status:** ALL ACs COMPLETE. In Code Review. 143 tests passing.

**(2026-03-24) Cherry-pick:** Commit `329bce55079` from `DOTCOMPB-7717` applied to this branch — switches `FeaturedDeals` from `CMSPartial` → `CmsPartialSsr` with `:config="{ bookingUrl }"`. `FeaturedDeals` now accepts `bookingUrl` prop (String, default `''`). Currently investigating `partial-featured-services-v2` behavior on this branch.

**Scope:** Extends `HcbLocationPageV2` (from 7289) with 7 new page sections: services, add-ons, FAQs, Birdeye reviews, marketing modules (×2 via CMS Partials), additional locations (V1 reuse), and special deals (desktop sidebar).

**Key components:**
*   **`HairColorBarLocationServices`** — Horizontal Swiper carousel of service cards + CMS-driven add-on list. Props: `services[]`, `addons[]`. `handleBookNow` uses `trackMREventAndRedirect`. Book Now links use `<a>` with `:href`. Add-on icons via `ImgBox(:media-obj="addon.icon")`. Carousel overflow fix: `max-width: calc(100vw - 2.5em)` below 1024px.
*   **`HairColorBarLocationReviews`** — Custom review cards replacing Birdeye iframe widget. Full-stack: `BirdeyeAPI.getReviewsViaWidget()` scrapes `__NEXT_DATA__` → controller → webservice (Fisher-Yates shuffle, 3 cards) → Vuex action. Skeleton loading, avatar initials fallback (`@error`), star ratings via `mr-icon(name="star-solid")`, text truncation with expand/collapse, IntersectionObserver viewport tracking.
*   **`HairColorBarLocationFAQs`** — CSS `max-height` accordion (NOT `TransitionExpand`). Props: `faqs[]`, `title`. ADA: `<button>` with `aria-expanded` + `aria-controls`.
*   **`MarketingBanner`** — Globally registered (both `mrVueApp.js` + `registerGlobalsSsr.js`). Two themes: `dark` (`brand-color-1` bg) and `light` (`#9A8CAD` bg). `isBookServiceCta` auto-generates booking URL from `hairColorBarBooking.location.code`.
*   **`FeaturedDeals`** — Thin wrapper: `h2` heading + `CMSPartial` for `partial-featured-services-v2`. Needs cross-store bridge in `serverPrefetch`.

**Backend files modified:** `mr_modules/birdeye/BirdeyeAPI.js` (added `getReviewsViaWidget`), `mr_modules/controllers/lib/birdeye.js` (added `getReviewsByBirdeyeId`), `mr_modules/webservices/lib/colorbar.js` (un-stubbed `getLocationReviews`).

**Event tracking (6 events):** Service card clicked, Book service clicked, FAQs clicked, Reviews widget viewed (IntersectionObserver), Marketing module clicked, Additional Location clicked.

**Tests:** Reviews 40, About 27, FAQs 15, HeroV2 13 (updated from 15→13 for 7555), HcbLocationPageV2 14, Services 14, MarketingBanner 8, PageIntro 7, FeaturedDeals 3 — **143 total, all passing.**

---

### 3.5 DOTCOMPB-7463: Navigation Redesign

**Created:** 2026-03-16 | **Last updated:** 2026-03-27
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-16-043543-dotcompb_7463.org`
**Branch:** `DOTCOMPB-7463` (main PR) + `DOTCOMPB-7463-nav-bug-fixes` (post-PR bug fixes)
**PR:** [#20210](https://github.com/MadisonReed/mr/pull/20210) | **Status:** OPEN. CI failing (`core_integration_tests` + `core_tests`). Reviews dismissed (re-review needed). Tophat `featuredTools.title` field pending. QA not done.

**(2026-03-24) Bug fixes on `DOTCOMPB-7463-nav-bug-fixes`:**
- **Sentry fix in `siteNav.js`** — `res.data || {}` guard before destructuring prevents `TypeError` on null response
- **`SiteNavShopContent` focus-visible** — Added `:focus-visible` outline (`2px solid cta-color-1`) to `.collection-product` and `.solution-product` anchors
- **`SiteNavShopContent` optional chaining** — 17 spots audited and fixed: all `.link.text`/`.link.href` now use `?.`
- **`SiteNavShopContent` MrBtn hover color** — Added `color ui-color-1` to hover/active/focus state in `:deep(.mrbtn)` — text was invisible (cta-color-1 on cta-color-1 background)

**(2026-03-25) Optional chaining for Object/Array props (commit `25df20c`):**
- Added `?.` and `?? []` guards for optional Object/Array props across `SiteNavMobileV2AboutNav`, `SiteNavMobileV2MainNav`, `SiteNavMobileV2SubNav`, `SiteNavMobileWrapper`, `SiteNavShopContent`
- Fixed undefined `v-for` keys across SiteNav components

**(2026-03-26) DOTCOMPB-7763 — PR [#20317](https://github.com/MadisonReed/mr/pull/20317) — MERGED:**
- **iOS scroll fix** — `padding-bottom: calc(4em + env(safe-area-inset-bottom))` on `.shop-nav-content` in `SiteNavShopContent.vue` at `@media mq-desktop-md-less`
- **Header spacing** — Moved vertical padding from `header.shop-nav-header` (`.py-100m` removed) to `h2.nav-title` (`.py-25m` added) in `SiteNavMobileWrapper.vue`
- **Double-tracking fix** — `mix_trackFTVNavViewed` moved into `else` branch in `SiteNavDesktopV2.toggleNav()` and `SiteNavMobileV2MainNav.openNav()` — shop nav only fires `Shop Nav - Opened`
- **Style commit** — Adjusted mobile shop nav header padding (commit `7fb428b`)
- 52 tests passing. Approved by Maxi-Di-Mito. Roam node: `~/.brain.d/roam-nodes/madison_reed/2026-03-26-dotcompb_7763.org`

**(2026-03-27) Mobile nav scroll normalization (UNSTAGED on `DOTCOMPB-7463-nav-bug-fixes`):**

Changes normalize scroll architecture across all 4 mobile nav panels. Each panel now owns exactly one scroll container with proper iOS safe area clearance.

- **`SiteNavMobileV2MainNav.vue`:**
  - Added `flex: 1` + `overflow-y: auto` + `padding-bottom: calc(4em + env(safe-area-inset-bottom))` on root — root IS the scroll container
  - Added `.py-25m` to all 3 `i.icon-caret-right` elements (Shop, navItems loop, About) for icon vertical spacing
  - MainNav does NOT get `.site-nav-mobile-content` from parent (unlike the other 3 panels)

- **`SiteNavMobileV2SubNav.vue`:**
  - Root: replaced `padding-bottom: 4em` with `display: flex` + `flex-direction: column`
  - `.nav-content`: replaced `height: 100%` + `overflow-y: scroll` with `flex: 1` + `overflow-y: auto` + `padding-bottom: calc(4em + env(safe-area-inset-bottom))`

- **`SiteNavMobileV2AboutNav.vue`:**
  - Identical pattern to SubNav (same class name `.site-nav-mobile-v2-sub-nav`)
  - Root: replaced `padding-bottom: 4em` with `display: flex` + `flex-direction: column`
  - `.nav-content`: replaced `height: 100%` + `overflow-y: scroll` with `flex: 1` + `overflow-y: auto` + `padding-bottom: calc(4em + env(safe-area-inset-bottom))`

- **`SiteNavMobileWrapper.vue`:**
  - Removed `.pb-400m` from root template — wrapper is now purely structural
  - No style changes — ShopContent owns its own scroll

- **`SiteNavShopContent.vue`:**
  - `@media mq-desktop-md-less`: removed `padding: 1.5em 1em` shorthand, updated `padding-bottom` from `calc(4em + ...)` to `calc(8em + env(safe-area-inset-bottom))`
  - `@media mq-mobile`: added `padding-bottom: calc(8em + env(safe-area-inset-bottom))` after `padding: 1em` — fixes shorthand override bug

**Mobile nav scroll architecture (final state, 2026-03-27):**

Parent layout (`SiteNavMobileV2.vue`):
```
.site-nav-mobile-v2 → height: 100vh, overflow: hidden, display: flex, flex-direction: column
├── .site-nav-mobile-header (top bar with hamburger, logo, cart)
└── <transition>
    ├── MainNav         ← NO .site-nav-mobile-content class
    ├── ShopNav(Wrapper) ← .site-nav-mobile-content (flex: 1, position: relative, overscroll-behavior: contain)
    ├── SubNav           ← .site-nav-mobile-content
    └── AboutNav         ← .site-nav-mobile-content
```

| Panel | Root styles | Scroll container | Safe area padding |
|---|---|---|---|
| MainNav | `flex: 1`, `overflow-y: auto` | Root itself | `calc(4em + env(...))` on root |
| SubNav | `display: flex`, `flex-direction: column` | `.nav-content` (`flex: 1`, `overflow-y: auto`) | `calc(4em + env(...))` on `.nav-content` |
| AboutNav | Same as SubNav | Same as SubNav | Same as SubNav |
| ShopNav | Wrapper: no scroll styles. ShopContent at mobile: `height: 100%`, `overflow-y: auto` | ShopContent | `calc(8em + env(...))` on ShopContent |

**Component Tree:**
```
SsrApp.vue
└── MrNavigation (MODIFIED — breakpoint classes)
    └── SiteNav (MODIFIED — showMobileNav, mixinKey sr-top-nav)
        ├── SiteNavDesktopV2 (MODIFIED — .f-secondary titles, import swapped)
        │   ├── SiteNavShopContent (NEW — all viewports, CSS breakpoints only)
        │   └── SiteNavDesktopV2ShopNav (ORIGINAL — untouched, rollback available)
        └── SiteNavMobileV2 (MODIFIED — import swapped)
            ├── SiteNavMobileV2MainNav (MODIFIED — redesign)
            ├── SiteNavMobileWrapper (NEW — thin shell)
            └── SiteNavMobileV2ShopNav (ORIGINAL — untouched, rollback available)
```

**Data flow:** `dataToolSvc.getData({ mixinKey: 'sr-top-nav' })` → `siteNav` store (`dynamicShopNav`, `dynamicNavItems`, `dynamicNavLinks`, `dynamicNavCTAs`, `aboutNavExtraConfig`) → `SiteNav.vue` props → Desktop/Mobile components.

**`sr-top-nav` shape:** `shopNav.featuredTools[]`, `shopNav.shopCollections[]` (title + `products[]`), `shopNav.shopSolutions[]` (title + `products[]`), `shopNav.quickActions[]`, `shopNav.marketingSlot`. Root: `NavCTAs[]` (dynamic mobile action buttons).

**`SiteNavShopContent`** — Responsive CSS Grid: `1fr 2fr 1fr` (≥1300) → `1fr 1fr 1fr` (≤1299) → `column-count: 2` with `display: contents` (≤759) → `column-count: 1` (≤559). Collections: circular 90px images. Active bullet: pill with `@keyframes`. 5 Segment events: Shop Nav Opened, Shop by shade clicked, Shop by type clicked, Shop other products clicked, Quiz clicked.

---

### 3.6 DOTCOMPB-7652: Madi Overlapping with Sticky CTA

**Created:** 2026-03-17 | **Status:** Complete — committed, roam node with commit/PR sections.
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-17-065717-dotcompb_7652.org`
**Branch:** `DOTCOMPB-7652`

**Fix:** `MountedFlag(v-if="!isDesktop" flag="bt-with-sticky-cta")` in `HcbLocationPageV2.vue` adds body class on mobile. `SierraWidget.vue` shifts launcher up 90px via `transform: translateY(-90px)` inside `@media mq-tablet-less` when `.bt-with-sticky-cta` is present.

**Also fixed in this session:** `HairColorBarLocationReviews` "See More Reviews" MrBtn aligned with `HairColorBarLocationAbout` pattern — removed direct class, used `:deep(.mrbtn)` from root wrapper, added `aria-label="See more reviews on Birdeye (opens in new tab)"`.

---

### 3.7 DOTCOMPB-7555: Remove Non-functional "Photos" Button from Location Hero

**Created:** 2026-03-18 | **Status:** Complete — committed on branch, 13 tests passing, PR description in roam node.
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-18-121233-dotcompb_7555.org`
**Branch:** `DOTCOMPB-7555`

**Root cause:** The `.more-photos` button on the desktop hero (`MrBtn.more-photos`) called `handleImageGalleryClick()`, which was a TODO stub since page launch — it tracked a Segment event but never opened a gallery. The button was misleading dead UI.

**Changes to `HairColorBarLocationHeroV2.vue`:**
- **Template:** Removed `MrBtn.more-photos` overlay element
- **Script:** Deleted `handleImageGalleryClick()`, `additionalImagesCount` computed, `VISIBLE_HERO_IMAGES_COUNT` constant
- **Styles:** Removed `.more-photos` rule block (background, border, hover states)
- **Tests:** Removed `additionalImagesCount` and `handleImageGalleryClick` describe blocks — **13 tests remain, all passing**

The 2-column static desktop layout (`primaryHeroImage`, `secondaryHeroImage`, `isMobile` matchMedia, `mounted`/`beforeUnmount` lifecycle) remains **unchanged** — only dead Photos button code was removed.

**⚠ Parked Work — DO NOT INCLUDE until business confirms:**

During development, the ticket scope was initially misread as "replace static images with carousel for all breakpoints." A full desktop banner carousel was built before clarification. That work is preserved on branch `DOTCOMPB-7555_full_width` (based on `feat-website-booking-flow-site-revolution_with_performance`) as a NEW component `LocationImageBannerCarousel`:
- Swiper autoplay (5s), animated progress bullet pagination (`--autoplay-delay` CSS variable), `loop: true`, max 5 slides, SSR-safe `isMounted` guard
- 21 unit tests across `HairColorBarLocationHeroV2` (17) + `LocationImageBannerCarousel` (4)
- Full documentation in the roam node

### 3.8 DOTCOMPB-7717: MarketingBanner Cleanup (Follow-up to Andris PR #20229)

**Created:** 2026-03-19 | **Status:** Plan refined, not yet implemented.
**Files:** `MarketingBanner.vue`, `MarketingBanner.test.js` — nothing else touches these.

---

#### Background: What Andris built in DOTCOMPB-7717

Booking CTAs on HCB location pages were navigating to `/colorbar/locations` (the generic list) instead of the specific location's booking flow. Andris's fix in PR [#20229](https://github.com/MadisonReed/mr/pull/20229) solved this by leveraging a capability in `CmsPartialSsr` that was not previously used here.

**The `CmsPartialSsr` `clientConfig` mechanism:**
- `CmsPartialSsr` accepts a `config` prop (Object) and passes it as `clientConfig` to the Vue component it compiles dynamically from the CMS partial HTML string.
- Any key in `config` becomes accessible as `clientConfig.<key>` inside the Tophat-authored HTML template.
- Andris switched `CMSPartial` → `CmsPartialSsr` in `HcbLocationPageV2` and `FeaturedDeals`, and passed `:config="{ bookingUrl }"` where `bookingUrl` is computed from `this.location.code` → `/colorbar/booking/${code}/services`.
- The Tophat CMS partial templates were then updated to use `clientConfig.bookingUrl` directly on their booking links.

**Why `MarketingBanner` had a workaround:**
Before this mechanism was known, `MarketingBanner` (a globally registered component used inside CMS partial HTML) independently reconstructed the booking URL by reading `hairColorBarBooking.location.code` from Vuex. Since it couldn't receive the URL as data, CMS partial authors signaled their intent through the button text — setting `cta-text="book service"`. `MarketingBanner` detected this string via `isBookServiceCta` and overrode `ctaUrl.url` with the Vuex-built URL. Now that `clientConfig.bookingUrl` is available, the CMS templates pass the URL directly via `ctaUrl`, and the button text via `ctaUrl.text`. The workaround is fully dead.

---

#### Exact changes — `MarketingBanner.vue`

| Location | What | Action |
|---|---|---|
| Line 11 | `{{ ctaText }}` | Change to `{{ ctaUrl.text }}` |
| Line 15 | `import { mapState } from 'vuex'` | **Remove entire line** — `mapState` is the only vuex import |
| Lines 27-30 | `ctaText` prop definition | Remove — button text now comes from `ctaUrl.text` |
| Lines 51-71 | Entire `computed` block | **Remove the whole block** — all three properties gone, block becomes empty |

The `computed` block contains: `...mapState('hairColorBarBooking', { bookingLocation: 'location' })`, `bookingUrl()`, `isBookServiceCta()`, `redirectUrl()`. After cleanup `redirectUrl` does not need a computed — it moves inline into `handleCtaClick`:

```js
methods: {
  handleCtaClick() {
    this.trackMREventAndRedirect('HCB Location Page - Marketing module clicked', this.ctaUrl?.url, {
      moduleName: this.title,
      isFrontEndEvent: true,
    });
  },
},
```

This removes the `redirectUrl` computed entirely and avoids leaving an empty `computed: {}` block.

---

#### Exact changes — `MarketingBanner.test.js`

The `createWrapper` default already has `ctaUrl: { url: '/membership', text: 'Membership Page' }` — **no change needed there**.

No Vuex store mock was ever in this test file — `mapState` was silently returning `undefined` in all tests (none exercised the Vuex path). **Nothing to remove from setup.**

| Test | Action |
|---|---|
| `"renders CTA button with text"` (line 47) | Change prop from `{ ctaText: 'Join Now' }` → `{ ctaUrl: { url: '/some-url', text: 'Join Now' } }`. Assertion `toBe('Join Now')` stays the same. |
| `"uses default CTA text when not provided"` (line 52) | Rewrite: rename to `"renders CTA button text from ctaUrl"`. Use plain `createWrapper()`. Assert `wrapper.find('.banner-cta').text()).toBe('Membership Page')` (from the default `ctaUrl` in `createWrapper`). |
| `"calls trackMREventAndRedirect on CTA click"` (line 71) | **No change.** Already asserts against `'/membership'` from `ctaUrl.url`. With `redirectUrl` inlined, the call still passes `ctaUrl?.url` = `'/membership'`. ✓ |
| All other tests | No change. |

---

#### Step-by-step implementation

1. **Branch:** `git checkout -b DOTCOMPB-7717-marketing-banner-cleanup` off `DOTCOMPB-7717`

2. **Edit `MarketingBanner.vue`** per the exact changes table above.

3. **Edit `MarketingBanner.test.js`** per the exact test changes table above.

4. **Run tests:** `cd website && npm run test:vue MarketingBanner` — expect 8 tests, all passing. Do not proceed to step 5 if any test fails.

5. **Spawn a subagent to create the PR.** Once all tests pass, delegate PR creation to a subagent using the `/create-pr` skill. Pass it the PR reference text below as the required context. Instruct the subagent explicitly to:
   - Target base branch `DOTCOMPB-7717`
   - Add label `DOTCOM TEAM`
   - Use the PR reference text below as the content for the description — do not summarize or shorten it; the explanation of Andris's work and the `clientConfig` mechanism must be preserved in full so reviewers understand both PRs without needing to trace the history themselves.

---

#### Commit Message (copy-paste)

```
remove dead booking url workaround from MarketingBanner

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>
```

---

#### PR Title (copy-paste)

```
[DOTCOMPB-7717]: MarketingBanner cleanup — remove dead booking URL workaround
```

---

#### PR Body (copy-paste — follows ticket-pr-template.md directive)

```
https://madison-reed.atlassian.net/browse/DOTCOMPB-7717

## Checklist for PR Author (Check if it applies)
- [x] contains testing instructions
- [ ] requires a lambda deployment to test or release to production
- [ ] requires special deployment requirements/instructions
- [x] has unit tests
- [ ] contains db migrations
- [x] all Github Checks have passed ([Please document flaky tests here](https://madison-reed.atlassian.net/wiki/spaces/ENGINEERIN/pages/815857713/Flakey+Unit+Tests))

## What does this PR do?

This PR addresses [DOTCOMPB-7717](https://madison-reed.atlassian.net/browse/DOTCOMPB-7717). Follow-up cleanup to #20229 that removes a dead client-side workaround from `MarketingBanner` — made fully redundant once the CMS partials were updated to use `clientConfig.bookingUrl` from Andris's fix.

> `MarketingBanner` is a globally registered Vue component rendered inside CMS partial HTML templates. Before #20229, it had no way to receive the location-specific booking URL as data, so it reconstructed it independently from Vuex and detected the button label text "book service" as a signal to override navigation. That workaround is now dead code.

**Changes:**

- **`MarketingBanner.vue`** (`website/src/vuescripts/components/HairColorBarBookingV2/components/MarketingBanner/`):
  - Removed `import { mapState } from 'vuex'` — entire line, nothing else used it
  - Removed `ctaText` prop — button label now comes from `ctaUrl.text`
  - Removed entire `computed` block:
    - `...mapState('hairColorBarBooking', { bookingLocation: 'location' })`
    - `bookingUrl()` — was rebuilding `/colorbar/booking/${code}/services` from Vuex
    - `isBookServiceCta()` — was sniffing `ctaText.toLowerCase().includes('book service')` as URL-override signal
    - `redirectUrl()` — was branching on `isBookServiceCta`; inlined directly into `handleCtaClick`
  - Template: `{{ ctaText }}` → `{{ ctaUrl.text }}`
  - `handleCtaClick` now passes `this.ctaUrl?.url` directly instead of `this.redirectUrl`

- **`MarketingBanner.test.js`**:
  - `"renders CTA button with text"` — drives button text through `ctaUrl.text` instead of removed `ctaText` prop
  - `"uses default CTA text when not provided"` → renamed `"renders CTA button text from ctaUrl"` — asserts text comes from `ctaUrl.text`

### Technical Details

- **Why the workaround existed:** `CmsPartialSsr` already had a `config` prop (passed as `clientConfig` to the Vue component compiled at runtime from the CMS partial HTML string) — the mechanism to inject dynamic data into CMS partials was always available. It was simply not wired up during the original `MarketingBanner` development. To compensate, the component sniffed the button label text: if `cta-text` contained `"book service"`, it ignored the provided URL and rebuilt the booking URL from Vuex state instead.
- **What #20229 did:** Switched `CMSPartial` → `CmsPartialSsr` in `HcbLocationPageV2` and `FeaturedDeals`, passed `:config="{ bookingUrl }"`, and updated the Tophat CMS partial templates to use `clientConfig.bookingUrl` directly. The URL now flows as data from the page through the partial system — no component-level reconstruction needed.
- **Result:** `MarketingBanner` is now purely data-driven. Its only responsibility is rendering a banner card and tracking the CTA click.

### Unit Testing Coverage

| Component | Test | Status |
|---|---|---|
| `MarketingBanner.test.js` | renders the banner with title | ✅ |
| `MarketingBanner.test.js` | renders description when provided | ✅ |
| `MarketingBanner.test.js` | does not render description when empty | ✅ |
| `MarketingBanner.test.js` | renders CTA button with text | ✅ |
| `MarketingBanner.test.js` | renders CTA button text from ctaUrl | ✅ |
| `MarketingBanner.test.js` | applies dark theme class by default | ✅ |
| `MarketingBanner.test.js` | applies light theme class | ✅ |
| `MarketingBanner.test.js` | calls trackMREventAndRedirect on CTA click | ✅ |

**8 tests** across 1 test file — all passing.

## Instructions on how QA can test this PR

1. Open a location page, e.g. `/colorbar/locations/nyc-flat`
2. Locate the light marketing banner (between About and Services sections)
   - **Expected:** banner renders with correct title, description, and button label
   - **Expected:** clicking the CTA navigates to the correct location booking URL (`/colorbar/booking/nyc-flat/services`)
3. Locate the dark marketing banner (between Services and Getting Here sections)
   - **Expected:** same rendering and navigation behaviour as above
4. On desktop (1024px+), locate the FeaturedDeals sidebar ("special deals for this month")
   - **Expected:** deals partial renders correctly and any booking CTA navigates to the correct location

## Documentation
<!-- Add screenshots here -->
```

---

```
Labels: DOTCOM TEAM, Pending Code Review
```

---

### 3.9 DOTCOMPB-7749: Nav Title Font Size + CTA Implementation

**Created:** 2026-03-24
**Roam Node:** Documented in `~/.brain.d/roam-nodes/madison_reed/2026-03-16-043543-dotcompb_7463.org` (§ Bug Fix Branch — Mar 24, 2026)
**Branch:** `DOTCOMPB-7463-nav-bug-fixes`
**Status:** Font size bump done. CTA implementation pending. Do NOT create PR until CTA work is complete.

**Font size change — `SiteNavDesktopV2.vue`:** All 5 nav title elements (Shop button, navItems loop, About button, navLinks `<a>`, Extole `.site-nav-title`) bumped one step:
- `xs-f-small` → `xs-f-medium` (14px → 16px)
- `md-f-medium` → `md-f-xmedium` (16px → 18px)
- `lg-f-xsmall` → `lg-f-small` (12px → 14px)
- `xl-f-small` → `xl-f-medium` (14px → 16px)
2 snapshots updated in `SiteNav.test.js`. 48 tests passing.

**PR prep** — Title: `[DOTCOMPB-7749]: Nav title font size increase and site nav bug fixes`. Full PR body in roam node.

---

### 3.10 DOTCOMPB-7742: Featured Service CTA Pre-Selection on Location Page

**Created:** 2026-03-27 | **Last updated:** 2026-03-30
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-27-120100-dotcompb_7742.org`
**Branch:** `DOTCOMPB-7742` (based on `master`, Maxi's `set-service-from-cookie-in-new-booking-flow` merged)
**PR Dependency:** [#20308](https://github.com/MadisonReed/mr/pull/20308) (Maxi — cookie-based service pre-selection in V2 booking flow)
**Status:** **COMPLETE** (2026-03-30). Implemented, code reviewed, 19 tests passing, full suite clean (5263/5263). Changes UNSTAGED — needs commit + PR.

#### Problem Statement

The `FeaturedServicesV2` component (inside CMS partial `partial-featured-services-v2`, rendered by `FeaturedDeals` in the V2 location page sidebar) has two compounding failures on the HCB location details page:

1. **Rendering failure:** `FeaturedServicesV2` silently renders nothing because `hairColorBarBooking.location.servicesOffered` is not populated. The location page calls `colorbar/loadLocation` → `/api/colorbar/getLocation` (CMS endpoint), which does NOT return `servicesOffered`. The booking endpoint (`hairColorBarBooking/getLocation` → `/api/colorbar/getLocationForBooking`) that returns `servicesOffered` is never called.

2. **State loss on navigation:** `FeaturedServicesV2.selectService()` uses Vuex `setSelectedService` + `$router.push({ name: 'booking-calendar' })`. On the location page this triggers a full page reload (different app entry point than the booking SPA) — Vuex state is wiped and the selected service is lost.

**Two API endpoints — two data shapes:**

| Endpoint | Store Action | Used By | Returns `servicesOffered`? |
|---|---|---|---|
| `/api/colorbar/getLocation` | `colorbar/loadLocation` | `HcbLocationPageV2` (location page) | No |
| `/api/colorbar/getLocationForBooking` | `hairColorBarBooking/getLocation` | `HairColorBarBookingV2` (booking flow) | Yes |

Current `HcbLocationPageV2.serverPrefetch`:
```js
await this.loadLocation(code);          // colorbar store — CMS data only
this.setBookingLocation(this.location); // shallow copy to hairColorBarBooking — no servicesOffered
```

Result: `servicesOfferedDictionary` → `{}` → `servicesPopulated` → `[]` → `FeaturedServicesV2` renders nothing.

**Scope note:** V1 location page (`HcbLocationPage`) does NOT use `partial-featured-services-v2` — only V2 does via `FeaturedDeals`. The partial also renders on the booking flow services page (`HairColorBarBooking/Services/Services.vue`) where it works because the booking endpoint is called.

#### Existing Cookie Pattern (Working Reference)

`HairColorBarLocationServices` (V2 location services section) uses a cookie-based flow that survives page reloads:
1. Click "Book Now" → `this.$cookies.set('selected_service', code)` (with `_consult`/`_only` normalization based on `hadAppointment`)
2. Redirect to `/colorbar/booking/${code}/services` via `trackMREventAndRedirect`
3. `ServicesPage` mounts → `setServiceFromCookie()` reads cookie → pre-selects → navigates to calendar

`FeaturedServicesV2` does NOT use this pattern — uses Vuex + `$router.push` which doesn't survive cross-app page reloads.

#### Maxi's Solution — PR #20308 (`set-service-from-cookie-in-new-booking-flow`)

Maxi rewrote the cookie consumption side in `ServicesPage.vue` (`HairColorBarBookingV2/ServicesPage/`):

| File | Change |
|---|---|
| `ServicesPage.vue` | `setServiceFromCookie()` rewritten: normalizes codes (strips `_consult`, tries `_only`), finds service via `serviceTagMap` tag subset matching, computes enhancement add-ons, sets store state via mutations, navigates to `booking-calendar`. `mounted`: `$watch('categories')` defers call until async data loads. |
| `hairColorBarBooking.js` | New `serviceSetByCookie` state + mutation. `loadAppointmentBookingProgress`: two early-return guards (before + after async call) skip session restore when cookie flow is active. |
| `HairColorBarBookingV2.vue` | Skips `resumeUnsavedBooking()` when `serviceSetByCookie` is true. |
| `ServicesPage.test.js` | +167 lines of cookie pre-selection tests. |

**Result:** Booking flow consumption side is ready. Missing piece: making `FeaturedServicesV2` produce the cookie.

#### Implementation Plan

**Step 1 — Fix rendering: populate `servicesOffered` on V2 location page**

Add `hairColorBarBooking/getLocation` call in `HcbLocationPageV2.serverPrefetch`:

```js
async serverPrefetch() {
  await Promise.all([
    this.loadLocation(this.routeParams.locationCode),
    this.getBookingLocation(this.routeParams.locationCode),
  ]);
}
```

Map the action: `...mapActions('hairColorBarBooking', { getBookingLocation: 'getLocation' })`.

This populates `hairColorBarBooking.location` with the full booking object (including `servicesOffered`), enabling `FeaturedServicesV2` to render. The existing `this.setBookingLocation(this.location)` mutation becomes redundant — `getBookingLocation` commits via `setLocation` directly. Remove it.

Side effects to validate:
- `hairColorBarBooking/getLocation` also commits `addOnTreatments` — harmless on location page
- Adds one extra API call during SSR (parallelized via `Promise.all`, no latency penalty)

**Step 2 — Fix `FeaturedServicesV2` to use cookie-based pre-selection**

Modify `selectService()` to set the `selected_service` cookie and redirect to the services URL:

Current (`FeaturedServicesV2.vue` L143–154):
```js
selectService(service, includesColorWonder) {
  if (service) {
    this.trackMREvent("Services redesign 2024 - featured service selected", { serviceCode: service.code });
    this.setSelectedService({...service, includesColorWonder});
    this.$router.push({ name: 'booking-calendar', params: { code: this.location.code } });
  }
}
```

New:
```js
selectService(service) {
  if (service) {
    this.$cookies.set('selected_service', service.code);
    this.trackMREventAndRedirect(
      'Services redesign 2024 - featured service selected',
      `/colorbar/booking/${this.location.code}/services`,
      { serviceCode: service.code, isFrontEndEvent: true },
    );
  }
}
```

Changes:
- Set `selected_service` cookie with `service.code`
- Navigate to services URL (not calendar) so `ServicesPage.setServiceFromCookie()` runs
- Use `trackMREventAndRedirect` (consistent with `HairColorBarLocationServices` pattern)
- Remove `setSelectedService` Vuex call (doesn't survive reload)
- Remove `includesColorWonder` param (handled by Maxi's tag matching)

**Impact on booking flow services page:** `FeaturedServicesV2` also renders there via `CMSPartial`. After this change, clicking redirects to the same services URL. `ServicesPage` remounts, reads cookie, pre-selects, and navigates to calendar. User experience is identical — the flash is imperceptible.

**Step 3 — Verify old booking flow compatibility**

`HairColorBarBooking/Services/Services.vue` has an older `setServiceFromCookie()` with simpler code matching. Since `FeaturedServicesV2` now sets the cookie, verify the old method handles the service codes correctly, or confirm all traffic routes to the V2 booking flow.

**Step 4 — Tests**

- `FeaturedServicesV2.test.js` — update `selectService` tests: assert cookie set, assert `trackMREventAndRedirect` called with services URL
- `HcbLocationPageV2.test.js` — add test for `getBookingLocation` in serverPrefetch
- Run: `cd website && npm run test:vue FeaturedServicesV2 && npm run test:vue HcbLocationPageV2`

**Step 5 — End-to-end verification**

1. V2 location page → sidebar featured service CTA → booking flow → calendar with service pre-selected
2. Booking flow services page → featured service CTA → calendar with service pre-selected
3. Verify cookie is removed after consumption
4. Verify `serviceSetByCookie` flag prevents session restore from overwriting

---

### 3.11 DOTCOMPB-7712: New Page to Display Location Photos

**Created:** 2026-03-30 | **Last updated:** 2026-04-03
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-30-150000-dotcompb_7712.org`
**Branch:** `DOTCOMPB-7712`
**Status:** IN PROGRESS — V1+V2 migration complete. Photos page with hero pair + CSS masonry + DB image merge. 81 tests passing. 2 full code reviews (round 1: 30 findings/5 fixed, round 2: 18 findings/7 fixed). All changes UNSTAGED. **NEXT: PR prep + commit.**

**Scope:** "+X photos" tag on location hero images (desktop + mobile), new photos gallery page at `/colorbar/locations/{locationCode}/photos`.

**Architecture (Final — V2 Section Extraction + Dual Routes):**
```
HcbLocationPageV2 (thin parent — data loading + router-view)
│  serverPrefetch: loadLocation + setBookingLocation
│  Computed: defaultLocationImages, galleryImages, routeViewProps
│  Guard: v-if="location?.code" on router-view
│
└── router-view(v-slot="{ Component }")
    ├── /colorbar/locations/:locationCode → HcbLocationSections (static import)
    │   ├── SiteMessageBannerCarousel
    │   ├── HairColorBarLocationHeroV2 (:gallery-images="galleryImages")
    │   │   ├── PageIntro, ImgBox ×2 (desktop), a.more-photos → $router.push
    │   │   └── LocationImageCarousel (mobile, +X overlay → $router.push)
    │   ├── .location-body (grid: main-column + sidebar)
    │   │   ├── About, CMSPartials, Services, Getting Here, Payments, Reviews, FAQs
    │   │   └── FeaturedDeals (sidebar)
    │   ├── V1 footer (MoreLocations, RegionList, MoreInfo)
    │   ├── MountedFlag + FixedCtaBar (mobile CTA)
    │   └── Breadcrumbs + location tracking watcher
    │
    └── /colorbar/locations/:locationCode/photos → HcbLocationPhotosPage (dynamic import)
        ├── Sticky header (back arrow-left + title + x-rounded close)
        ├── Hero pair (first 2 images, stacked mobile / 50% desktop)
        └── CSS masonry grid (column-count: 2 mobile / 3 tablet / 4 desktop)
```

**All Migration Steps (COMPLETE):**

| Step | Status |
|---|---|
| V1: Remove Express route, Pug, global registrations | DONE |
| V1: Move HcbLocationPhotosPage, refactor (galleryImages prop, remove sessionStorage) | DONE |
| V1: Register routes, update navigation ($router.push) | DONE |
| V2: Move routes.js to HcbLocationPageV2, add location-details route | DONE |
| V2: Update route imports in mrVueApp.js + ssr/router.js | DONE |
| V2: Create HcbLocationSections (extract ALL content from HcbLocationPageV2) | DONE |
| V2: Refactor HcbLocationPageV2 to thin parent (router-view + galleryImages) | DONE |
| V2: Update HeroV2 (heroImages → galleryImages prop, remove galleryImages computed, remove router-view) | DONE |
| V2: Move HcbLocationPhotosPage from HeroV2 to HcbLocationPageV2 folder | DONE |
| Express: Add 404 validation for /photos URL (no URL rewrite) | DONE |
| Photos page: CSS-only masonry (replaced JS matchMedia — fixes hydration mismatch) | DONE |
| Photos page: Hero pair layout (first 2 images large, rest in masonry) | DONE |
| Photos page: Sticky header refinement (arrow-left, x-rounded, text-left, gap-12) | DONE |
| Back/close: trackMREventAndRedirect (hard redirect, not $router.push) | DONE |
| Utility class prefix fix: 11 classes updated to use xs- prefix across 3 files | DONE |
| Tests: 75 tests across 5 files (PageV2 11, Sections 12, HeroV2 10, PhotosPage 18, Carousel 24) | DONE |

**Code Review (2026-04-03 — 8 parallel subagents, 45-rule checklist):**
- 30 findings total: 1 CRITICAL, 5 HIGH, 12 MEDIUM, 12 LOW
- 5 implemented: nested interactive fix (a→span in carousel), focus-visible on slides, computed alphabetization (HeroV2), hidden h2 removal, utility class prefix fixes
- 25 skipped: pre-existing issues, acceptable page-specific patterns, andris-5 minimal-touch

**Key Decisions:**

| Decision | Date | Rationale |
|---|---|---|
| Section extraction + dual routes | 2026-04-02 | `router-view` at page level. Two routes swap ALL visible content. No `v-if`. |
| CSS-only masonry (Option A) | 2026-04-03 | JS matchMedia caused hydration mismatch (SSR 2 cols, client 4). CSS `column-count` is SSR-safe. |
| Hero pair + masonry layout | 2026-04-03 | Figma spec: first 2 images large (store exterior/interior), rest in grid. |
| `trackMREventAndRedirect` for back/close | 2026-04-03 | `$router.push` to location-details fails on direct /photos access (CMS context differs). Hard redirect ensures full SSR. |
| Express 404 without URL rewrite | 2026-04-03 | URL rewrite caused SSR to render sections route but client expected photos route → hydration mismatch. |
| `v-if="location?.code"` on router-view | 2026-04-03 | Initial Vuex `location: {}` is truthy. `?.code` catches both null and empty object. |
| Padding/margin utilities must use `xs-` prefix | 2026-04-03 | Mobile-first convention. `.xs-pt-50m` not `.pt-50m`. Rule added to code-review skill. |
| Merge CMS + DB images in `galleryImages` | 2026-04-03 | CMS `defaultLocationImages` first (hero images), then `location.carouselImages` (Tophat uploads). Normalized to same shape `{ image: {...} }`. `locationImages` computed wraps DB flat objects. |
| `location.carouselImages` are Tophat uploads, NOT DashHudson | 2026-04-03 | Verified: DB images managed via Tophat admin (`croppedImage` field). DashHudson is a separate client-side widget with zero connection to carouselImages. Session decision #22 still applies. |
| Inline `aspect-ratio` for images with `width`/`height` | 2026-04-03 | `getImageAspectStyle(image)` returns `{ aspectRatio: 'w / h' }` for DB images (have dimensions). CMS images without dimensions use natural aspect ratio via `height: auto`. |
| Remove `role="region"` from HeroV2 root | 2026-04-03 | PageIntro already owns the self-contained landmark (`role="region"` + `aria-label` + `h1#id`). HeroV2 root is purely structural — no ARIA attributes. Eliminates nested duplicate landmarks. |
| Parallel subagent code review flow | 2026-04-03 | 8 subagents per rule category, all in parallel. Each checks 4-12 rules against all files. Interactive one-by-one resolution. Documented in code-review skill SKILL.md. |

**Tests:** 81 passing across 5 test files. All components have coverage.

**Code Review (2 rounds, 2026-04-03):**
- Round 1: 8 subagents, 30 findings (1 CRITICAL, 5 HIGH, 12 MEDIUM, 12 LOW). 5 implemented, 25 skipped.
- Round 2: 8 subagents, 18 findings (0 CRITICAL, 4 HIGH, 7 MEDIUM, 7 LOW). 7 implemented, 10 skipped (1 N/A).
- Total implemented across both rounds: nested interactive fix, focus-visible, computed alphabetization (×2), hidden h2 removal, utility prefix fixes, magic number constant, CSS alphabetization, nested landmark fix, aria-hidden on icons, skeleton backgrounds.

---

## SECTION 4: FILE INDEX

> Quick reference for all files created or modified during this session.

### Components (New)

| File | Ticket |
|---|---|
| `website/src/vuescripts/components/HairColorBarBookingV2/components/FixedCtaBar/FixedCtaBar.vue` | DOTCOMPB-7556 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/FixedCtaBar/FixedCtaBar.test.js` | DOTCOMPB-7556 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/FixedCtaBar/index.js` | DOTCOMPB-7556 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/FeaturedDeals/FeaturedDeals.vue` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/FeaturedDeals/FeaturedDeals.test.js` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/FeaturedDeals/index.js` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationReviews/HairColorBarLocationReviews.vue` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationReviews/HairColorBarLocationReviews.test.js` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationReviews/index.js` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationFAQs/HairColorBarLocationFAQs.vue` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationFAQs/HairColorBarLocationFAQs.test.js` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationFAQs/index.js` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/MarketingBanner/MarketingBanner.vue` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/MarketingBanner/MarketingBanner.test.js` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/MarketingBanner/index.js` | DOTCOMPB-7290 |
| `website/src/assets/svg-icons/star-solid.svg` | DOTCOMPB-7290 |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationPhotosPage/HcbLocationPhotosPage.vue` | DOTCOMPB-7712 (photos gallery: hero pair + CSS masonry, sticky header) |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationPhotosPage/HcbLocationPhotosPage.test.js` | DOTCOMPB-7712 (18 tests) |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationPhotosPage/index.js` | DOTCOMPB-7712 (barrel export) |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/routes.js` | DOTCOMPB-7712 (two routes: location-details static + location-photos dynamic) |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationSections/HcbLocationSections.vue` | DOTCOMPB-7712 (extracted ALL section content from HcbLocationPageV2) |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationSections/HcbLocationSections.test.js` | DOTCOMPB-7712 (12 tests) |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationSections/index.js` | DOTCOMPB-7712 (barrel export) |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationHeroV2/LocationImageCarousel/LocationImageCarousel.test.js` | DOTCOMPB-7712 (24 tests, NEW) |
| `website/src/vuescripts/components/SiteNav/SiteNavShopContent/SiteNavShopContent.vue` | DOTCOMPB-7463 |
| `website/src/vuescripts/components/SiteNav/SiteNavShopContent/SiteNavShopContent.test.js` | DOTCOMPB-7463 |
| `website/src/vuescripts/components/SiteNav/SiteNavShopContent/index.js` | DOTCOMPB-7463 |
| `website/src/vuescripts/components/SiteNav/SiteNavMobileWrapper/SiteNavMobileWrapper.vue` | DOTCOMPB-7463 |
| `website/src/vuescripts/components/SiteNav/SiteNavMobileWrapper/SiteNavMobileWrapper.test.js` | DOTCOMPB-7463 |
| `website/src/vuescripts/components/SiteNav/SiteNavMobileWrapper/index.js` | DOTCOMPB-7463 |

### Backend (Modified)

| File | Ticket |
|---|---|
| `mr_modules/birdeye/BirdeyeAPI.js` | DOTCOMPB-7290 |
| `mr_modules/controllers/lib/birdeye.js` | DOTCOMPB-7290 |
| `mr_modules/webservices/lib/colorbar.js` | DOTCOMPB-7290 |

### Components (Modified)

| File | Ticket |
|---|---|
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationPageV2.vue` | 7289, 7290, 7556, 7652 |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationPageV2.test.js` | 7289, 7290, 7556 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationHeroV2/HairColorBarLocationHeroV2.vue` | 7290 (aria fix), 7555 (photos btn removed), 7712 (photos btn recovered) |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationHeroV2/HairColorBarLocationHeroV2.test.js` | 7290, 7555 (13 tests) |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationHeroV2/LocationImageCarousel/LocationImageCarousel.vue` | 7712 (re-enabled +X photos overlay, locationCode prop) |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationAbout/HairColorBarLocationAbout.vue` | 7290 (aria + utility) |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationAbout/HairColorBarLocationAbout.test.js` | 7290 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationServices/HairColorBarLocationServices.vue` | 7290 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationServices/HairColorBarLocationServices.test.js` | 7290 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationReviews/HairColorBarLocationReviews.vue` | 7290, 7652 |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationReviews/HairColorBarLocationReviews.test.js` | 7290, 7652 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/PageIntro/PageIntro.vue` | 7290 (titleId prop) |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/PageIntro/PageIntro.test.js` | 7290 |
| `website/src/vuescripts/mrVueApp.js` | 7290 (MarketingBanner), 7712 (REMOVE global reg L769, ADD route import + spread) |
| `website/src/vuescripts/ssr/registerGlobalsSsr.js` | 7290 (MarketingBanner), 7712 (REMOVE global reg L80+L246) |
| `website/src/vuescripts/ssr/router.js` | 7712 (ADD route import + spread) |
| `website/src/routing/views.js` | 7712 (REMOVE Express route L1579–1594) |
| `website/src/vuescripts/store/modules/colorbar.js` | 7290 (getLocationReviews try/catch) |
| `website/src/vuescripts/components/SiteNav/SiteNavDesktopV2/SiteNavDesktopV2.vue` | 7463, 7749 (font size bump) |
| `website/src/vuescripts/components/SiteNav/SiteNavMobileV2/SiteNavMobileV2MainNav/SiteNavMobileV2MainNav.vue` | 7463 |
| `website/src/vuescripts/components/SiteNav/SiteNavMobileV2/SiteNavMobileV2.vue` | 7463 |
| `website/src/vuescripts/components/SiteNav/SiteNav.vue` | 7463 |
| `website/src/vuescripts/store/modules/siteNav.js` | 7463 (mixinKey → sr-top-nav, res.data null guard) |
| `website/src/vuescripts/components/SierraWidget/SierraWidget.vue` | 7652 |

### Roam Nodes

| File | Ticket / Purpose |
|---|---|
| `~/.brain.d/roam-nodes/madison_reed/2026-03-02-131328-dotcompb_7289.org` | DOTCOMPB-7289 |
| `~/.brain.d/roam-nodes/madison_reed/2026-03-12-114716-dotcompb_7290.org` | DOTCOMPB-7290 |
| `~/.brain.d/roam-nodes/madison_reed/2026-03-11-165514-dotcompb_7556.org` | DOTCOMPB-7556 |
| `~/.brain.d/roam-nodes/madison_reed/2026-03-10-122138-dotcompb_7557.org` | DOTCOMPB-7557 |
| `~/.brain.d/roam-nodes/madison_reed/2026-03-16-043543-dotcompb_7463.org` | DOTCOMPB-7463 |
| `~/.brain.d/roam-nodes/madison_reed/2026-03-17-065717-dotcompb_7652.org` | DOTCOMPB-7652 |
| `~/.brain.d/roam-nodes/madison_reed/2026-03-18-121233-dotcompb_7555.org` | DOTCOMPB-7555 (photos btn + parked carousel) |
| `~/.brain.d/roam-nodes/madison_reed/2026-03-18-135209-site_revolution_redesign.org` | Site Revolution architecture reference (no JIRA ticket) |
| `~/.brain.d/roam-nodes/madison_reed/2026-03-30-150000-dotcompb_7712.org` | DOTCOMPB-7712 (photos page + gallery) |
| `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` | Sprint Board Index |

### Session & Directives

| File | Purpose |
|---|---|
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/site-revolution-redesign.md` | This session file |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/ticket-pr-template.md` | PR template directive |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.** This section captures the most recent work and immediate next steps.

### What was done last (2026-04-03)

**Session 1 (V2 migration + initial code review):**
*   **V1+V2 migration fully executed** — 8 V1 steps + 7 V2 steps. Express/Pug removed, section extraction into `HcbLocationSections`, thin parent `HcbLocationPageV2` with `router-view`, HeroV2 updated (galleryImages prop), `HcbLocationPhotosPage` moved to PageV2 folder.
*   **Photos page layout built per Figma** — Hero pair (first 2 images, stacked mobile / 50% desktop) + CSS masonry grid (column-count 2/3/4). Sticky header (arrow-left, title left-aligned, x-rounded close).
*   **Hydration mismatch fixed** — JS matchMedia replaced with CSS-only `column-count`. Express URL rewrite removed (caused SSR/client route divergence).
*   **Code review round 1** — 8 parallel subagents, 30 findings, 5 implemented.
*   **75 tests, utility prefix rule enforced** (11 classes updated to `xs-` prefix).

**Session 2 (DB image merge + round 2 code review):**
*   **CMS + DB image merge** — `galleryImages` computed now merges `defaultLocationImages` (CMS, first) + `location.carouselImages` (DB/Tophat uploads, after). New `locationImages` computed normalizes DB flat objects to `{ image: {...} }` shape. Verified: DB images are Tophat manual uploads, NOT DashHudson.
*   **Aspect ratio fallback** — `getImageAspectStyle(image)` returns inline `aspect-ratio` for images with `width`/`height` (DB images). CMS images without dimensions use natural ratio via `height: auto`.
*   **Photos page header refined** — Font sizes (xs-f-small title, xs-f-xsmall count), text-color-3 for count, gap-12 between arrow and text, arrow-left icon, x-rounded close icon.
*   **Code review round 2** — 8 parallel subagents, 18 findings, 7 implemented: computed alphabetization (PageV2), magic number → `MOBILE_MEDIA_QUERY` constant (HeroV2), CSS alphabetization (Sections), nested landmark fix (removed `role="region"` from HeroV2 root — PageIntro owns it), `aria-hidden="true"` on decorative mr-icons (PhotosPage), skeleton `:deep(.image-box)` + `:deep(img)` on carousel main display + thumbnails, hero image skeleton `height 100%`.
*   **81 tests passing** — PageV2 (17), Sections (12), HeroV2 (10), PhotosPage (18), Carousel (24).
*   **Code-review skill updated** — Parallel subagent flow documented in SKILL.md. MR checklist (45 rules + 22 Andris) saved to `rules/mr-review-checklist.md`.

### Pending

*   **DOTCOMPB-7712** — Implementation complete. 81 tests, 2 code review rounds. All changes UNSTAGED on branch `DOTCOMPB-7712`. **NEXT: PR prep + commit.** Use `/create-pr` skill.
*   **DOTCOMPB-7742** — **COMPLETE.** Changes UNSTAGED on branch `DOTCOMPB-7742`. Needs commit + PR. Roam node `2026-03-27-120100-dotcompb_7742.org`.
*   **DOTCOMPB-7768** — Roam node needed. On branch `DOTCOMPB-7768`.
*   **DOTCOMPB-7463-nav-bug-fixes** — 5 files UNSTAGED (mobile nav scroll normalization). Need testing then commit. See §3.5.
*   **DOTCOMPB-7749** — CTA implementation pending on `DOTCOMPB-7463-nav-bug-fixes`. Font size bump done.
*   **DOTCOMPB-7463** — PR #20210 open. CI failing. Re-review needed.
*   **DOTCOMPB-7290** — PR #20190 open. `partial-featured-services-v2` rendering root cause in §3.10.
*   **DOTCOMPB-7555** — Needs PR creation. PR description in roam node.
*   **DOTCOMPB-7557** — ADA: Cannot tab to Book Services on desktop. Not started.

### Where to resume

**IMMEDIATE: Create PR for DOTCOMPB-7712.** On branch `DOTCOMPB-7712`. Implementation complete, 2 code review rounds done, 81 tests passing. Use `/create-pr` skill.

**Current git status (all UNSTAGED on `DOTCOMPB-7712`):**
- Modified: `views.js` (Express 404 route), `mrVueApp.js` (route import), `ssr/router.js` (route import), `HcbLocationPageV2.vue` (thin parent + galleryImages merge), `HcbLocationPageV2.test.js` (17 tests), `HairColorBarLocationHeroV2.vue` (galleryImages prop, no region, MOBILE_MEDIA_QUERY constant), `HairColorBarLocationHeroV2.test.js` (10 tests), `LocationImageCarousel.vue` (ADA fixes, focus-visible, skeleton, no nested interactive)
- New: `HcbLocationPageV2/routes.js`, `HcbLocationSections/` (vue + test + index), `HcbLocationPhotosPage/` (vue + test + index), `LocationImageCarousel.test.js` (24 tests)
- Deleted: `HeroV2/routes.js` (moved), `views/desktop/hcb-location-photos/` (Pug), `HairColorBar/HcbLocationPhotosPage/` (moved), global registrations in `mrVueApp.js` + `registerGlobalsSsr.js`

**Before committing, verify:**
1. Run full test suite: `cd website && npm run test:vue HcbLocationPageV2 HairColorBarLocationHeroV2 LocationImageCarousel` — expect 81 passing
2. Run lint: `eslint` on changed files
3. Test manually:
   - Location page → click "+X photos" → photos page loads (hero pair + masonry)
   - Photos page back/close button → hard redirect to location page
   - Direct `/photos` URL → page loads, Express validates location code
   - Invalid location code on `/photos` → 404
   - Mobile carousel → "+X photos" overlay → photos page
   - Verify DB images (location.carouselImages) appear after CMS images in gallery

If user wants to **commit DOTCOMPB-7742**: Use `/create-pr` skill — commit msg and PR body in roam node `2026-03-27-120100-dotcompb_7742.org`. Branch `DOTCOMPB-7742`.
If user switches to **DOTCOMPB-7768**: Create roam node first using `/mr-roam-node`.
If user wants to **commit nav scroll work**: Run tests on `DOTCOMPB-7463-nav-bug-fixes`, then commit 5 files.
If user asks for **Site Revolution architecture**: Read `~/.brain.d/roam-nodes/madison_reed/2026-03-18-135209-site_revolution_redesign.org`.

<!-- DESCRIPTION AND USER CONTEXT END -->

### HYDRATION FIX APPROACHES (2026-04-03) — Photos Page Column Layout

**Problem:** SSR renders 2 columns (`window` undefined), client renders 4 (desktop matchMedia). Hydration mismatch: images land in different `.masonry-column` divs, different child counts, different `src`/`alt` attributes. Only happens on direct `/photos` URL access (SSR). Normal flow (client navigation from location page) has no issue.

**Option A — CSS-only responsive columns (PREFERRED, test first)**
Remove JS `columnCount`/`matchMedia` entirely. Use CSS `column-count` or CSS Grid with media queries to distribute images into columns. The template renders a flat list of images. CSS handles layout at every breakpoint. SSR and client produce identical HTML — zero hydration mismatch.

Implementation:
- Remove: `columnCount` data, `columns` computed, `getInitialColumnCount`, `beforeMount` matchMedia setup, `updateColumnCount`, `tabletQuery`/`desktopQuery` data, `beforeUnmount` cleanup
- Template: replace `.masonry-column(v-for)` with flat `.photo-item(v-for="item in galleryImages")`
- Styles: add `column-count: 2` (mobile default), `@media tablet: column-count: 3`, `@media desktop: column-count: 4` on `.gallery-grid`
- Add `break-inside: avoid` on `.photo-item` to prevent images from splitting across columns

Pros: Zero hydration mismatch. No JS for layout. Simpler component (remove ~40 lines of matchMedia logic).
Cons: CSS columns distribute top-to-bottom then left-to-right (not left-to-right then top-to-bottom like the JS masonry). Visual difference from current layout — images flow vertically per column instead of horizontally across columns.

**Option B — Accept SSR flash, suppress warning**
Keep current JS masonry. Accept that direct `/photos` access shows 2 columns briefly, then corrects to 4 after `beforeMount`. Vue says "this mismatch is check-only. The DOM will not be rectified in production" — meaning production doesn't re-render, it just keeps the server HTML. The `beforeMount` correction handles the visual update.

Implementation: No code changes. The current `SSR_DEFAULT_COLUMNS = 2` + `beforeMount` `updateColumnCount()` is already correct. Add a comment explaining the expected hydration warning on direct access.

Pros: No layout change. Current masonry behavior preserved exactly.
Cons: Brief 2→4 column flash on direct URL access. Console warnings in dev (not in prod).

**Decision:** Test Option A first. If layout doesn't meet expectations, revert to Option B.

---

### EDGE CASE ANALYSIS (2026-04-03) — DOTCOMPB-7712 V2 Migration

**Scope:** Analysis of all 7 V2 execution steps from the DEFINITIVE ARCHITECTURE PLAN. Each step was compared against the current working tree state (branch `DOTCOMPB-7712`), session guidelines (Section 1.1-1.17), SSR safety rules, and reactive dependency chains. Steps 1-6 appear to be partially executed in the working tree; Step 7 (tests) has not been started yet.

**Current state summary:** `HcbLocationPageV2.vue` has been refactored to thin parent. `HcbLocationSections.vue` has been created. `routes.js` has been moved to `HcbLocationPageV2/`. Route imports added in `mrVueApp.js` and `ssr/router.js`. `HairColorBarLocationHeroV2.vue` has been updated with V1 additions (+N photo(s) overlay, carousel locationCode, router-view) but Step 6 changes (prop rename, computed removal, router-view removal) have NOT been applied. The old `HeroV2/routes.js` has been deleted from disk.

---

| # | Step | Issue | Severity | Detail |
|---|---|---|---|---|
| 1 | Step 1 (routes.js) | Route path collision with Express | MEDIUM | Express `views.js` has `/colorbar/locations/:urlKey` which only matches single path segments, so `/nyc-flat/photos` falls through to the CMS catch-all. The plan documents this as "RESOLVED" but notes: **verify Tophat serves the same `cmsSettings` for the `/photos` URL variant**. If Tophat does not serve `defaultLocationImages` for the `/photos` page, direct-URL access renders an empty gallery. This cannot be verified from code alone -- requires Tophat configuration audit. |
| 2 | Step 2 (mrVueApp.js) | CRITICAL: Old import of deleted file still present | HIGH | `mrVueApp.js` line 134 still has `import hcbLocationHeroRoutes from '@components/HairColorBar/HcbIndividual/HairColorBarLocationHeroV2/routes';`. This file has been deleted from disk. The diff shows the new `hcbLocationPageRoutes` import was ADDED at line 135 but the old import was NOT removed. Line 163 still spreads `...hcbLocationHeroRoutes` into the routes array. **This will cause a build-time import resolution failure.** The Vite bundler (both client and SSR) will crash when it cannot find the module. |
| 3 | Step 2 (ssr/router.js) | CRITICAL: Old import of deleted file still present | HIGH | `ssr/router.js` line 7 still has `import hcbLocationHeroRoutes from '@components/HairColorBar/HcbIndividual/HairColorBarLocationHeroV2/routes';`. Same deleted file. Line 17 still spreads `...hcbLocationHeroRoutes`. **SSR bundle will fail to build.** The diff for `ssr/router.js` only shows additions, confirming the old lines were not removed. |
| 4 | Step 3 (barrel export) | No issues found | LOW | `HcbLocationSections/index.js` correctly imports `./HcbLocationSections.vue` and re-exports. The `routes.js` imports `./HcbLocationSections` which resolves to the barrel. Consistent with codebase patterns. |
| 5 | Step 4 (HcbLocationSections.vue) | CMSPartial serverPrefetch timing with nested router-view | MEDIUM | `CMSPartial` has its own `serverPrefetch()` that fetches HTML referencing globally registered components (e.g., `FeaturedServicesV2`) that depend on `hairColorBarBooking.location`. The parent's `serverPrefetch` populates this store state. In Vue 3 SSR, parent `serverPrefetch` completes before children render, so the store is populated before `CMSPartial` fires. With the new architecture, `HcbLocationPageV2.serverPrefetch` runs first, then `router-view` resolves `HcbLocationSections` (static import, immediate), then Sections renders `CMSPartial`, which runs ITS `serverPrefetch`. **Order preserved -- no regression.** But this is fragile: if `HcbLocationSections` were changed to a dynamic import, the timing could break. Document in PR. |
| 6 | Step 4 (HcbLocationSections.vue) | `location` watcher with `immediate: true` fires duplicate tracking events | LOW | When user navigates photos-to-sections (back), `HcbLocationSections` remounts and the watcher fires again with `immediate: true`, sending a duplicate page-view tracking event. Pre-existing behavior from the original `HcbLocationPageV2` -- not introduced by migration. Worth noting in PR description. |
| 7 | Step 4 (HcbLocationSections.vue) | MountedFlag body class removal on route change | LOW | Navigating from location page to photos unmounts Sections, which unmounts `MountedFlag`, removing `bt-with-sticky-cta` from `document.body`. The plan documents this as CORRECT (no sticky CTA on photos page). When navigating BACK, Sections remounts and `MountedFlag` re-adds the class. Vue unmounts old before mounting new -- brief classless gap during transition. No CSS transition on the Sierra widget offset, so no visual flash. |
| 8 | Step 4 (HcbLocationSections.vue) | Breadcrumbs rely on store data from serverPrefetch only | MEDIUM | `setBreadcrumbFragments()` in `mounted()` reads `this.location` from Vuex. On initial SSR load, `location` is populated by the parent's `serverPrefetch`. On client-side navigation back from photos, `location` is still in the store (same page context). But if the user somehow navigates client-side to a DIFFERENT location code, `loadLocation` only runs in `serverPrefetch` (not `mounted`/`created`), so the store retains the OLD location. Breadcrumbs would show stale data. **Pre-existing issue** -- same pattern existed in the original component. |
| 9 | Step 4 (HcbLocationSections.vue) | Scoped style `.hcb-sections .row` correctly targets own template elements | LOW | `.row` class is on `.row.hero-section` and `.row.location-body` which are direct template elements in Sections. Scoped styles apply correctly via `data-v-*` attribute. If any child component had `.row` on its root, Vue's scoped parent selector would also match (intended scoped behavior). No real risk -- child components use component-specific class names. |
| 10 | Step 4 (HcbLocationSections.vue) | `:deep(.marketing-banner)` scope ID transparency | LOW | The marketing banner class is inside dynamically compiled `CMSPartial` template. `:deep()` pierces the scoped boundary to target any descendant `.marketing-banner`. Scope ID change (from `HcbLocationPageV2`'s filepath hash to `HcbLocationSections`'s filepath hash) is transparent because `:deep()` removes the scope constraint for descendants. No issue. |
| 11 | Step 5 (HcbLocationPageV2.vue) | Template loses `.gap-md.px-125m` from parent root | LOW | Original root: `.hcb-page-v2.div-center.gap-md.px-125m`. New root: `.hcb-page-v2.div-center`. The gap/padding moved to `HcbLocationSections` root (`.hcb-sections.gap-md.px-125m`). The photos page has its own padding (`.px-125m` on `.photos-grid`). The parent now has NO padding/gap, which means the `router-view` children control their own spacing. **Correct per session guideline 1.11** (self-sufficient component spacing). But verify the photos page doesn't need the parent's gap. Current photos page: `.hcb-photos-page.div-center` has `max-width: 1440px` in Stylus -- no gap needed since it's a single full-width element. Safe. |
| 12 | Step 5 (HcbLocationPageV2.vue) | `routeViewProps` passes unused props causing Vue 3 fallthrough attrs warning | LOW | `routeViewProps` includes `locationCode` and `locationName` (consumed by photos page only). `HcbLocationSections` does NOT use them but DOES declare them as props (lines 95-101 in current Sections file). **Correctly mitigated per the plan's inline action.** No console warning. |
| 13 | Step 5 (HcbLocationPageV2.vue) | `location?.code` and `location?.name` empty during SSR failure | MEDIUM | If `loadLocation` in `serverPrefetch` fails (network error, bad location code), `location` remains `null`. `routeViewProps` returns `locationCode: ''` and `locationName: ''`. Photos page header would show " Images" (empty name + "Images"). Sections would render with `location` being `null` -- all `v-if="location"` guards throughout child components would prevent rendering. The hero guard `v-if="location"` (line 3 of HeroV2) would hide the entire hero. **Not a new issue** -- same failure mode existed before. Consider adding `v-if="location"` on `router-view` in thin parent for defense-in-depth. |
| 14 | Step 5 (HcbLocationPageV2.vue) | SSR hydration mismatch for dynamic import on `/photos` direct access | MEDIUM | `HcbLocationPhotosPage` is a dynamic import in `routes.js`. During SSR, dynamic imports resolve asynchronously. Vite's SSR bundler typically pre-bundles dynamic imports for SSR, but if this chunk is not pre-resolved, SSR renders empty for the `/photos` route while the client hydrates and lazy-loads the component -- causing hydration mismatch. **The plan accepts this:** "The primary flow is client-side navigation. Direct access is edge-case." Photos page will flash empty briefly on direct URL access, then hydrate with content. |
| 15 | Step 6 (HeroV2) | CRITICAL: Step 6 NOT executed -- `heroImages` prop NOT renamed to `galleryImages` | HIGH | The plan says: REMOVE `heroImages` prop, ADD `galleryImages` prop, REMOVE `galleryImages` computed. Current HeroV2 on disk still has: (a) `heroImages` prop (lines 63-68), (b) `galleryImages` computed (lines 87-98), (c) `router-view` in template (line 38). The Sections template passes `:gallery-images="galleryImages"` but HeroV2 declares `heroImages`, not `galleryImages`. Vue 3 fallthrough: `gallery-images` becomes an HTML attribute on the root div (ignored functionally). `heroImages` prop defaults to `[]`. `galleryImages` computed filters `[]` and returns `[]`. **Result: hero displays ZERO images.** |
| 16 | Step 6 (HeroV2) | CRITICAL: `router-view` still in HeroV2 template | HIGH | Line 38: `router-view(:gallery-images="galleryImages" :location-code="location.code" :location-name="location.name")`. The plan says REMOVE this. With the current FLAT route config (no `children` array), this nested `router-view` finds no matching child route and renders nothing. It is dead code producing an empty element. Not functionally breaking, but confusing and wastes DOM. Must be removed per plan. |
| 17 | Step 6 (HeroV2) | `photosUrl` hardcodes route path | LOW | `photosUrl` returns `/colorbar/locations/${code}/photos`. Session guideline andris-guideline-11 says "no hardcoded route paths in reusable components." HeroV2 is page-specific (not reusable), so this is acceptable. Could use `this.$router.resolve({ name: 'location-photos', params: {...} }).href` for consistency but not blocking. Same pattern exists in `LocationImageCarousel`. |
| 18 | Step 6 (HeroV2) | HcbLocationPhotosPage back navigation uses path string, not named route | LOW | `handleBackClick`/`handleCloseClick` use `this.$router.push(this.backUrl)` where `backUrl = /colorbar/locations/${locationCode}`. This works because the path matches `location-details`. Plan recommends refactoring to `{ name: 'location-details', params: { locationCode } }`. Non-blocking but should be done per plan inline action. |
| 19 | Step 7 (tests) | CRITICAL: Existing test file will fail | HIGH | `HcbLocationPageV2.test.js` (7628 bytes) was written for the original component rendering all sections. The thin parent has no `components`, no section HTML, no breadcrumbs, no CMS computed. Every assertion on rendered section content, breadcrumb mutations, or CMS computed properties will fail. Must be rewritten for: `routeViewProps` shape, `galleryImages` filter logic, `serverPrefetch`, `router-view` existence. |
| 20 | Step 7 (tests) | HeroV2 test file needs prop rename and describe block removal | MEDIUM | Tests pass `heroImages` as prop. After Step 6, prop name is `galleryImages`. The `galleryImages` describe block (filter/map tests) must be removed since logic moved to parent. `primaryHeroImage`/`secondaryHeroImage` tests must pass pre-filtered data directly. |
| 21 | Step 7 (tests) | HcbLocationSections.test.js must be created | MEDIUM | All section tests (breadcrumbs, CMS computed, layout structure, tracking watcher) must move from `HcbLocationPageV2.test.js` to a new `HcbLocationSections.test.js`. Mount `HcbLocationSections` with `cmsSettings`, `routeParams`, `galleryImages` as props. Mock store: `colorbar` (location), `global` (isDesktop, breadcrumb mutations). |
| 22 | General | Route name `location-details` is new -- no other code references it yet | LOW | The `location-details` named route is new. No existing code navigates TO it by name (only the photos page `handleBackClick` uses a path string). If Step 18 (refactor to named route) is done, `location-details` will be referenced. No breakage risk from the name itself. |
| 23 | General | `HcbLocationPhotosPage` `mounted()` tracking event fires on every mount | LOW | On client-side navigation from location page to photos, `mounted()` fires the tracking event. If user goes back and forward again, it fires again. This is correct per session guideline 1.7 (page load events in lifecycle hooks). No double-fire concern since the component is unmounted on back navigation. |
| 24 | Step 4 (HcbLocationSections.vue) | `SiteMessageBannerCarousel` locally imported AND globally registered | LOW | Both locally imported in Sections (line 58) and globally registered in `registerGlobalsSsr.js` (line 201). Local import takes precedence in Vue 3. No conflict. Standard codebase pattern. |
| 25 | Step 1 (routes.js) | Route order correctness | LOW | `location-details` (`:locationCode`) before `location-photos` (`:locationCode/photos`) in the array. Vue Router param matching does NOT capture slashes, so `nyc-flat/photos` does not match `:locationCode`. The longer path route `/photos` is correctly placed second. Safe. |
| 26 | Step 5 (HcbLocationPageV2.vue) | Computed alphabetization follows session guidelines | LOW | `...mapState` first (andris-guideline-10), then `defaultLocationImages`, `galleryImages`, `routeViewProps`. Alpha order: d < g < r. Correct per SS1.3. |
| 27 | Step 5 (HcbLocationPageV2.vue) | `router-view` v-slot `Component` may be undefined during async route resolution | LOW | During async route resolution (e.g., `HcbLocationPhotosPage` dynamic import), `Component` may be `undefined` briefly. `component(:is="undefined")` renders nothing -- Vue handles gracefully. No error. No `<transition>` or `<keep-alive>` involved. Safe. |

---

**BLOCKING ISSUES -- Must fix before commit:**

1. **Issues #2 and #3 (HIGH):** `mrVueApp.js` line 134 and `ssr/router.js` line 7 still import `hcbLocationHeroRoutes` from `HairColorBarLocationHeroV2/routes` which has been deleted from disk. Both the import line AND the spread (`...hcbLocationHeroRoutes`) in the routes array must be removed. **Build will fail without this fix.**

2. **Issue #15 (HIGH):** HeroV2 Step 6 has not been executed. `heroImages` prop must be renamed to `galleryImages`, the `galleryImages` computed must be removed, and the hero currently receives no image data from the Sections parent (which passes `:gallery-images`). **Hero displays zero images.**

3. **Issue #16 (HIGH):** `router-view` on line 38 of HeroV2 must be removed. It is a nested router-view with no matching child routes (flat route config). Dead code.

4. **Issue #19 (HIGH):** `HcbLocationPageV2.test.js` must be rewritten for the thin parent and `HcbLocationSections.test.js` must be created. Steps 4-7 must be committed atomically per the plan's critical constraint.

**NON-BLOCKING RECOMMENDATIONS:**

- Issue #18: Refactor `HcbLocationPhotosPage` back/close to use named route `location-details`.
- Issue #13: Consider `v-if="location"` on `router-view` in thin parent for defense-in-depth on SSR failure.
- Issue #1: Verify Tophat CMS serves identical `cmsSettings` for `/photos` URL variant (cannot verify from code).
- Issue #5: Document in PR that `CMSPartial` serverPrefetch timing is preserved but fragile if Sections import changes to dynamic.

### NAMING CONVENTION REVIEW (2026-04-03) --- DOTCOMPB-7712 V2 Migration

Reviewed all naming decisions in the Definitive Architecture Plan and the already-implemented code on branch `DOTCOMPB-7712` against session guidelines SS1.3, SS1.14, SS1.15, SS1.17 (andris patterns 2, 7, 12, 15, 20).

**Files analyzed:**
- `HcbLocationPageV2/HcbLocationPageV2.vue` (refactored thin parent)
- `HcbLocationPageV2/HcbLocationSections/HcbLocationSections.vue` (new extracted component)
- `HcbLocationPageV2/routes.js` (new route definitions)
- `HairColorBarLocationHeroV2/HairColorBarLocationHeroV2.vue` (pending Step 6 prop change)
- `HairColorBarLocationHeroV2/HcbLocationPhotosPage/HcbLocationPhotosPage.vue` (refactored)
- `HairColorBarLocationHeroV2/LocationImageCarousel/LocationImageCarousel.vue` (reference)

#### Component Names

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 1 | Component Name | `HcbLocationSections` | Consider `HcbLocationContent` | "Sections" is generic and does not communicate what KIND of sections. Every page has "sections." The component renders the entire visible content of the location detail page: hero, about, services, reviews, FAQs, footer links, mobile CTA. `HcbLocationContent` better conveys "all the visible page content" vs. "some arbitrary sections." Alternative: keep `HcbLocationSections` if the team reads "sections" as "the section-based content below the router-view" -- but `Content` is more self-explanatory per SS1.14 ("self-explanatory, general names"). Counter-argument: `Content` is even MORE generic than `Sections`. The word "sections" at least implies structured, divided content areas. The component IS literally the collection of page sections. VERDICT: **Acceptable as-is.** Both names are defensible. If the team has no preference, `HcbLocationSections` is fine -- it accurately describes a component that renders multiple page sections. | SS1.14 |
| 2 | Component Name | `HcbLocationPhotosPage` | Correct | Follows `Hcb` prefix convention. "LocationPhotos" is the domain. "Page" suffix correctly indicates this is a page-level component (not a reusable section). Consistent with `HcbLocationPageV2` naming pattern. | SS1.14 |
| 3 | Component Name | `HcbLocationPageV2` | Correct | Already validated across session. Thin parent role is an internal refactor -- name stays. | SS1.14 |

#### Folder Structure & Location

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 4 | Folder Location | `HcbLocationPageV2/HcbLocationSections/` | Correct | Nesting under `HcbLocationPageV2/` is correct because `HcbLocationSections` is a child route component exclusively used by `HcbLocationPageV2`'s `router-view`. It is NOT reusable elsewhere. SS1.14 says "location by domain, not by page" but makes an exception: "components that are truly page-section wrappers belong in the page folder." This is exactly that case. | SS1.14 |
| 5 | Folder Location | `HairColorBarLocationHeroV2/HcbLocationPhotosPage/` | FLAG: Inconsistent after V2 refactor | The photos page is now a sibling route to `HcbLocationSections`, rendered by `HcbLocationPageV2`'s `router-view`. Logically, it should live alongside `HcbLocationSections` inside `HcbLocationPageV2/`. Instead, it remains nested inside `HairColorBarLocationHeroV2/` (a V1 artifact from when `router-view` was inside the hero). The `routes.js` import already uses a relative path `'../HcbIndividual/HairColorBarLocationHeroV2/HcbLocationPhotosPage'` which is a code smell -- a sibling route importing from a deeply nested unrelated component folder. **Recommendation:** Move to `HcbLocationPageV2/HcbLocationPhotosPage/` in a follow-up. Not blocking for this PR since the import works, but document the tech debt. | SS1.14 |

#### CSS Root Class Names

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 6 | CSS Root Class | `.hcb-sections` | Correct | Follows `hcb-` prefix convention (SS1.14: "short CSS root classes use `hcb-` prefix"). Consistent with established pattern: `.hcb-page-v2`, `.hcb-hero-v2`, `.hcb-about`, `.hcb-services`, `.hcb-reviews`, `.hcb-faqs`. The name `.hcb-sections` is descriptive enough -- scoped styles prevent collision. Adding `location-` (`.hcb-location-sections`) would break the "short" rule. | SS1.14 |
| 7 | CSS Root Class | `.hcb-photos-page` | Correct | Follows `hcb-` prefix, consistent with `.hcb-page-v2`. Descriptive: identifies the photos page. | SS1.14 |
| 8 | CSS Root Class | `.hcb-page-v2` (parent) | Correct | Already validated. Retained after refactor to thin parent. | SS1.14 |
| 9 | CSS Internal Class | `.image-carousel` (LocationImageCarousel root) | Correct | Not `hcb-` prefixed because it is NOT a page section -- it is a UI component inside the hero. Short, descriptive. Scoped. | SS1.14, SS1.15 |

#### Route Names

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 10 | Route Name | `location-details` | Correct | Consistent with booking flow pattern: `booking-location`, `booking-services`, `booking-calendar`, etc. The `location-` prefix groups all location page routes. `details` accurately describes the main location detail view. Already referenced by `HcbLocationPhotosPage` navigation (`{ name: 'location-details' }`). Cross-reference: `HairColorBarLocationHeroV2.handleImageGalleryClick` uses `{ name: 'location-photos' }` -- both routes use the `location-` prefix consistently. | SS1.14, consistency with booking routes |
| 11 | Route Name | `location-photos` | Correct | Matches the URL path segment (`/photos`). Follows `location-` prefix grouping. Already used by `LocationImageCarousel.handleViewMoreClick` and `HairColorBarLocationHeroV2.handleImageGalleryClick`. Consistent. | SS1.14 |

#### Prop Names

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 12 | Prop Name | `galleryImages` (on `HcbLocationSections`) | Correct | Semantically accurate -- these are the filtered, URL-stripped gallery images passed from the parent. Same prop name used consistently by `HcbLocationPhotosPage`. The rename from `heroImages` (old HeroV2 prop) to `galleryImages` is correct: the images are not "hero images" anymore -- they are shared gallery data consumed by both the hero AND the photos page. | SS1.3 |
| 13 | Prop Name | `heroImages` (current HeroV2 prop, pending Step 6 change) | CHANGE TO `galleryImages` | Step 6 of the plan correctly specifies removing `heroImages` prop and adding `galleryImages` prop on `HairColorBarLocationHeroV2`. This has NOT been executed yet (the HeroV2 file still declares `heroImages`). The `HcbLocationSections` template already passes `:gallery-images="galleryImages"` -- a mismatch. **This must be resolved in Step 6.** The plan is correct; execution is pending. | SS1.3, plan Step 6 |
| 14 | Prop Name | `locationCode` (String) on `HcbLocationSections` | Correct (fallthrough suppression) | Declared to suppress Vue 3 fallthrough attrs warning per the plan's edge case analysis. Not consumed by the component. This is an intentional pattern, not dead code. Acceptable. | Plan edge case mitigation |
| 15 | Prop Name | `locationName` (String) on `HcbLocationSections` | Correct (fallthrough suppression) | Same rationale as #14. | Plan edge case mitigation |
| 16 | Prop Name | `cmsSettings` (Object) on `HcbLocationSections` | Correct | Pass-through from parent. Same name and shape as the original `HcbLocationPageV2` prop. No rename needed. | SS1.3 |
| 17 | Prop Name | `routeParams` (Object) on `HcbLocationSections` | Correct | Pass-through from parent. Same name and shape as original. | SS1.3 |
| 18 | Prop Name | `locationCode` (String, required) on `HcbLocationPhotosPage` | Correct | Required prop from `routeViewProps`. Used for back URL construction and tracking. Matches the route param name (`:locationCode`). | SS1.3 |
| 19 | Prop Name | `locationName` (String) on `HcbLocationPhotosPage` | Correct | Used in header title and tracking. | SS1.3 |

#### Computed Property Names & Ordering

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 20 | Computed Name | `routeViewProps` (in `HcbLocationPageV2`) | Correct | Clearly describes what it returns: the props object for the `router-view` slot's child component. Self-documenting. Not a common computed pattern in the codebase but justified by the `v-slot` architecture. | SS1.3 |
| 21 | Computed Order | `HcbLocationPageV2` computed: `...mapState` -> `defaultLocationImages` -> `galleryImages` -> `routeViewProps` | Correct | Vuex helpers first (SS1.17 andris-guideline-10). Then alphabetical: `d` -> `g` -> `r`. Correct order. | SS1.3, SS1.17-10 |
| 22 | Computed Order | `HcbLocationSections` computed: `...mapState` -> `...mapGetters` -> `addonServices` -> `bookingUrl` -> `faqsList` -> `faqsTitle` -> `gettingHereText` -> `marketingPartialDark` -> `marketingPartialLight` -> `paymentsText` -> `servicesList` -> `siteMessageTopics` | Correct | Vuex helpers first (mapState, then mapGetters). Local computed alphabetized: `a` -> `b` -> `f` -> `f` -> `g` -> `m` -> `m` -> `p` -> `s` -> `s`. Perfect alphabetical order. | SS1.3, SS1.17-10 |
| 23 | Computed Order | `HcbLocationPhotosPage` computed: `backUrl` -> `columns` -> `hasImages` | Correct | Alphabetical: `b` -> `c` -> `h`. | SS1.3 |
| 24 | Computed Name | `galleryImages` (in `HcbLocationPageV2` parent) | Correct | Moved from HeroV2. Same name, now in the parent. Semantically accurate -- filters and URL-strips CMS images into gallery-ready data. | SS1.3 |
| 25 | Computed Name | `defaultLocationImages` (in `HcbLocationPageV2`) | Correct | Extracts `cmsSettings?.defaultLocationImages`. Name matches the CMS field. Self-documenting. | SS1.3 |

#### CSS Internal Class Names

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 26 | CSS Class | `.photos-header` | Correct | `{context}-{role}` pattern per SS1.15. "photos" is the context, "header" is the role. | SS1.15 |
| 27 | CSS Class | `.header-content` | Correct | `{context}-{role}` pattern. Nested inside `.photos-header` -- context is clear from nesting. | SS1.15 |
| 28 | CSS Class | `.header-text` | Correct | | SS1.15 |
| 29 | CSS Class | `.header-title` | Correct | | SS1.15 |
| 30 | CSS Class | `.image-count` | Correct | Descriptive, no redundant prefix. | SS1.14, SS1.15 |
| 31 | CSS Class | `.back-link` | Correct | Descriptive action element. | SS1.15 |
| 32 | CSS Class | `.close-btn` | Correct | | SS1.15 |
| 33 | CSS Class | `.photos-grid` | Correct | `{context}-{role}` pattern. | SS1.15 |
| 34 | CSS Class | `.gallery-grid` | Correct | Internal grid container inside `.photos-grid`. Distinguishes the masonry layout from the outer semantic region. | SS1.15 |
| 35 | CSS Class | `.masonry-column` | Correct | `{role}-column` pattern per SS1.15 grid column convention. | SS1.15 |
| 36 | CSS Class | `.photo-item` | Correct | `{component}-{element}` BEM-like pattern for repeating items. | SS1.15 |
| 37 | CSS Class | `.hero-section` (in HcbLocationSections) | Correct | `{name}-section` semantic section pattern. Already validated in SS1.15 reference. | SS1.15 |
| 38 | CSS Class | `.location-body` (in HcbLocationSections) | Correct | `{context}-{role}` layout container. Already validated. | SS1.15 |
| 39 | CSS Class | `.main-column` / `.sidebar-column` | Correct | `{role}-column` pattern. Already validated. | SS1.15 |
| 40 | CSS Class | `.getting-here-section` / `.payments-section` | Correct | `{name}-section` semantic section pattern. Already validated. | SS1.15 |

#### Constants & Variables

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 41 | Constant | `MINIMUM_BREADCRUMB_COUNT = 3` (in HcbLocationSections) | Correct | Moved from parent. No magic number. UPPER_SNAKE_CASE. Not used in template so no computed wrapper needed. | SS1.3 |
| 42 | Constant | `VISIBLE_HERO_IMAGES_COUNT = 2` (in HeroV2) | Correct | Already existed pre-refactor. Not used in template directly -- only in `additionalImagesCount` computed. | SS1.3 |
| 43 | Constant | `TABLET_BREAKPOINT` / `DESKTOP_BREAKPOINT` (in HcbLocationPhotosPage) | Correct | Module-level constants for matchMedia strings. No magic strings. UPPER_SNAKE_CASE. | SS1.3 |

#### Method Names

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 44 | Method Name | `handleBackClick` (HcbLocationPhotosPage) | Correct | Follows `handle{Action}Click` pattern consistent with codebase (`handleBookServiceClick`, `handleImageGalleryClick`, `handleSlideClick`). | SS1.3, SS1.17-4 |
| 45 | Method Name | `handleCloseClick` (HcbLocationPhotosPage) | Correct | Same pattern. | SS1.3 |
| 46 | Method Name | `updateColumnCount` (HcbLocationPhotosPage) | Correct | Descriptive action name. Not a click handler (called from matchMedia listener), so no `handle` prefix -- correct. | SS1.3 |
| 47 | Method Name | `setBreadcrumbFragments` (HcbLocationSections) | Correct | Moved from parent. Already validated. | SS1.3 |
| 48 | Method Name | `handleImageGalleryClick` (HeroV2) | FLAG: Uses `$router.push` with `trackMREvent`, not `trackMREventAndRedirect` | This is a naming review, not a logic review, but the method name `handleImageGalleryClick` is correct. However, the implementation uses `trackMREvent` + `$router.push` which is acceptable for client-side route changes (no hard redirect). `trackMREventAndRedirect` is for `location.href` hard navigations per SS1.7. The `$router.push` is a soft SPA transition. **Name is correct.** | SS1.7, SS1.3 |

#### Webpack Chunk Name

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 49 | Chunk Name | `"HCBLocationPhotosPage"` | Minor: Consider `"HcbLocationPhotosPage"` | The webpackChunkName uses `HCB` (all caps) while the component name uses `Hcb` (PascalCase). This is cosmetic only -- chunk names don't affect runtime behavior. But for consistency with the component naming convention (`Hcb` prefix), `HcbLocationPhotosPage` would be more consistent. Not blocking. | Consistency |

#### `name` Property in `export default`

| # | Category | Current Name | Suggestion | Rationale | Guideline Ref |
|---|---|---|---|---|---|
| 50 | Component `name` | `'HairColorBarLocationsHeroV2'` (HeroV2, line 53) | FLAG: Typo -- should be `'HairColorBarLocationHeroV2'` | The component file is `HairColorBarLocationHeroV2.vue` (singular "Location") but the `name` property says `HairColorBarLocationsHeroV2` (plural "Locations"). This is a pre-existing bug, not introduced by DOTCOMPB-7712. **Not blocking for this PR** but worth noting. The `name` property is used for devtools and recursive component references. | SS1.14, pre-existing |

#### Summary

**Blocking issues:** 0
**Pending execution (already planned):** 1 item (#13 -- HeroV2 prop rename `heroImages` -> `galleryImages`, Step 6)
**Non-blocking recommendations:** 2 items (#5 -- move `HcbLocationPhotosPage` folder in follow-up; #49 -- chunk name casing)
**Pre-existing bugs discovered:** 1 item (#50 -- `name` property typo in HeroV2)
**All other decisions (44 of 50):** Correct per session guidelines.

<!-- Local Variables: -->
<!-- gptel-model: gemini-pro-paid -->
<!-- gptel--backend-name: "Gemini Local" -->
<!-- gptel--bounds: ((response (40 13827))) -->
