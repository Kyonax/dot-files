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
| `DOTCOMPB-7712` | Story | New page to display location photos                          | **IN PROGRESS** (2026-03-30). Roam node created. Route + Pug + Vue component + hero button + carousel overlay implemented. Gallery grid with dynamic aspect ratios. Tests pending. See §3.11. |

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
*   **DOTCOMPB-7712** — **IN PROGRESS** (2026-03-30). New photos page + "+X photos" button recovery. Route, Pug, Vue component, hero button, carousel overlay implemented. Gallery grid with dynamic sizing. Tests pending, commit + PR pending. See §3.11.
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

**Created:** 2026-03-30 | **Last updated:** 2026-03-30
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-30-150000-dotcompb_7712.org`
**Branch:** `DOTCOMPB-7712`
**Status:** IN PROGRESS. Route + Pug + Vue component + hero button + carousel overlay implemented. Gallery grid with dynamic aspect ratios and sizing. Tests pending. Commit + PR pending.

**Scope:** "+X photos" tag on location hero images (desktop + mobile), new photos gallery page at `/colorbar/locations/{locationCode}/photos`.

**Architecture — Non-CMS Direct Render Page:**
```
Express route (views.js)
  → colorbarCache.getLocation(urlKey)
  → cms.loaders.getLoader().loadPage(locationUri) — gets CMS defaultLocationImages
  → Merges CMS images + carouselImages + headerImage (deduped by _id)
  → res.render('hcb-location-photos/hcb-location-photos', { locationCode, locationName, locationImages })
  → Pug template (extends vue-layout.pug)
  → hcb-location-photos-page(location-code=locationCode location-name=locationName :location-images=locationImages)
  → Vue hydrates client-side, no serverPrefetch
```

**Component Tree:**
```
HcbLocationPhotosPage (photos gallery page)
└── ImgBox (per image, locally imported)

HairColorBarLocationHeroV2 (modified — recovered +X photos button)
├── MrBtn.more-photos (recovered from DOTCOMPB-7555, inside .secondary-display)
└── LocationImageCarousel (modified — re-enabled +X photos overlay)
    └── a.view-more-overlay (on 5th slide when remainingImagesCount > 0)
```

**`HcbLocationPhotosPage.vue` key architecture:**
- **Props:** `locationCode` (String), `locationImages` (Array — JSON from Express), `locationName` (String)
- **No Vuex dependency** — fully props-driven from Express route data
- **Gallery grid:** CSS Grid with `grid-auto-rows: 1px`, `row-gap: 0`, `column-gap: 0.5rem`, `grid-template-columns: repeat(6, 1fr)`, `grid-auto-flow: dense`
- **Dynamic sizing per image:** `getItemStyle(item)` computes `--col-span`, `--col-span-desktop`, `--row-span`, `--row-span-desktop` from image `width`/`height` and `maxImageWidth`
- **Desktop cap:** `DESKTOP_MAX_SPAN = 3` (50% of 6 columns)
- **Min span:** `MIN_COL_SPAN = 2` (no image smaller than ~33%)
- **`fitRowsToGrid()`:** Organizes images into rows summing to 6 columns. Only expands largest item (span >= 3) via `expandLargestInRow()`. Separate passes for mobile and desktop spans.
- **`ResizeObserver`** on grid container measures real pixel width for exact row span computation: `cellHeight = cellWidth * (h/w)`, `rowSpan = round(cellHeight / 1px)`
- **Tracking:** `TrackSegmentPage` fires in `mounted()` — `colorbar locations {code} photos`

**`HairColorBarLocationHeroV2.vue` changes (recovered from `2dfd59d4a1f^`):**
- `VISIBLE_HERO_IMAGES_COUNT = 2` (constant, was deleted in 7555)
- `additionalImagesCount` (computed, was deleted)
- `MrBtn.more-photos` template (inside `.secondary-display`, was sibling before)
- `handleImageGalleryClick()` — now navigates to `/colorbar/locations/{code}/photos` via `trackMREventAndRedirect` (was TODO stub)
- `.more-photos` styles recovered: `position: absolute; bottom: 1em; right: 1em` within `.secondary-display` (has `position: relative`)

**`LocationImageCarousel.vue` changes:**
- Added `locationCode` prop (String)
- Added `photosUrl` computed
- Re-enabled `a.view-more-overlay` on 5th slide (removed TODO comment)
- Added `handleViewMoreClick()` — `trackMREventAndRedirect` to photos URL
- `handleSlideClick()` delegates to `handleViewMoreClick` when `isViewMoreSlide(index)`
- Overlay styles: `position: absolute; inset: 0; background-color: rgba(0,0,0,0.5); color: white`
- Parent passes `:location-code="location.code"` to carousel

**Key Decisions:**

| Decision | Date | Rationale |
|---|---|---|
| Direct Pug render, no CMS | 2026-03-30 | Photos page doesn't need CMS template data — images come from cache + CMS loader in Express. Follows `hcb-addon/message.pug` pattern. |
| CMS + cache image merging | 2026-03-30 | Hero uses `cmsSettings.defaultLocationImages` (CMS), photos page needs those + `carouselImages` (cache). Express loads both via `cms.loaders` + `colorbarCache`. |
| 6-column grid with dynamic spans | 2026-03-30 | 6 columns provides granularity for proportional sizing. `span N = round(sizeRatio * 6)`. Desktop capped at span 3 (50%). |
| `grid-auto-rows: 1px` + `row-gap: 0` | 2026-03-30 | Standard `gap` compounds between sub-rows within a spanning item, distorting aspect ratios. `1px` rows + `0` row-gap = pixel-perfect heights. Visual spacing via `margin-bottom`. |
| `ResizeObserver` for grid measurement | 2026-03-30 | Row spans must be computed from actual pixel width. Static formula can't account for responsive container width changes. |
| Recover button inside `.secondary-display` | 2026-03-30 | Original had button as sibling. Moving inside gives proper `position: absolute` context from `.secondary-display`'s `position: relative`. |
| `expandLargestInRow` with span >= 3 guard | 2026-03-30 | Only items with span 3+ get expanded to fill row gaps. Prevents small images (span 2) from being stretched. |
| Header NOT sticky | 2026-03-30 | Removed `position: sticky` per Kyo's direction. Header scrolls with content. |
| `ImgBox` imported locally | 2026-03-30 | Not globally registered for this component context. Explicit import required. |

**Tests:** Pending. Existing tests unaffected — 13 hero tests + 14 page tests passing.

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
| `website/src/vuescripts/components/HairColorBar/HcbLocationPhotosPage/HcbLocationPhotosPage.vue` | DOTCOMPB-7712 |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPhotosPage/index.js` | DOTCOMPB-7712 |
| `website/src/views/desktop/hcb-location-photos/hcb-location-photos.pug` | DOTCOMPB-7712 |
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
| `website/src/vuescripts/mrVueApp.js` | 7290 (MarketingBanner), 7712 (HcbLocationPhotosPage) |
| `website/src/vuescripts/ssr/registerGlobalsSsr.js` | 7290 (MarketingBanner), 7712 (HcbLocationPhotosPage) |
| `website/src/routing/views.js` | 7712 (photos page Express route) |
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

### What was done last (2026-03-30 / 2026-03-31)

*   **DOTCOMPB-7712 implemented** — Full photos page feature:
    - Created roam node (`2026-03-30-150000-dotcompb_7712.org`) from JIRA ticket, populated DEVELOPMENT AC with plan, updated index
    - Analyzed 18 Express routes to extract non-CMS page creation guidelines (documented in roam node)
    - Express route at `/colorbar/locations/:urlKey/photos` — `res.render()` direct Pug, no CMS. Loads images from both `cms.loaders.getLoader().loadPage()` (hero images) and `colorbarCache` (carousel + header images), deduped by `_id`
    - Pug template: `website/src/views/desktop/hcb-location-photos/hcb-location-photos.pug` extending `vue-layout.pug`
    - Vue component: `HcbLocationPhotosPage` — props-driven (no Vuex), CSS Grid gallery with 6 columns, `grid-auto-rows: 1px` + `row-gap: 0` for pixel-perfect aspect ratios, `ResizeObserver` for grid width measurement, `fitRowsToGrid()` row organization
    - Global registration in `registerGlobalsSsr.js` + `mrVueApp.js`
    - Recovered `MrBtn.more-photos` on desktop hero from DOTCOMPB-7555 removal (commit `2dfd59d4a1f^`) — `handleImageGalleryClick()` now navigates to photos page
    - Re-enabled `+X photos` overlay on mobile `LocationImageCarousel` 5th slide + `locationCode` prop + `handleViewMoreClick()`
*   **Gallery refinement iterations** — Multiple layout approaches tested (CSS columns, flex masonry, CSS Grid). Final: 6-column grid with dynamic `--col-span` / `--row-span` per breakpoint, desktop max 50% (span 3), min span 2, `expandLargestInRow` only for items with span >= 3
*   **Key discoveries:** `carouselImages` are raw Tophat media objects with `width`/`height`/`aspects[]`; `cmsSettings.defaultLocationImages` is separate data source from `carouselImages`; `MrBtn` has internal `position: relative`; `ImgBox` needs local import in non-CMS pages
*   **All existing tests passing** — 13 hero + 14 page tests unaffected

### Pending

*   **DOTCOMPB-7712** — Tests pending (`HcbLocationPhotosPage.test.js`, `LocationImageCarousel.test.js`, hero test updates). Commit + PR pending. Changes UNSTAGED on branch `DOTCOMPB-7712`.
*   **DOTCOMPB-7742** — **COMPLETE.** Changes UNSTAGED on branch `DOTCOMPB-7742`. Needs commit + PR. Roam node `2026-03-27-120100-dotcompb_7742.org`.
*   **DOTCOMPB-7768** — Roam node needed. On branch `DOTCOMPB-7768`.
*   **DOTCOMPB-7463-nav-bug-fixes** — 5 files UNSTAGED (mobile nav scroll normalization). Need testing then commit. See §3.5.
*   **DOTCOMPB-7749** — CTA implementation pending on `DOTCOMPB-7463-nav-bug-fixes`. Font size bump done.
*   **DOTCOMPB-7463** — PR #20210 open. CI failing. Re-review needed.
*   **DOTCOMPB-7290** — PR #20190 open. `partial-featured-services-v2` rendering root cause in §3.10.
*   **DOTCOMPB-7555** — Needs PR creation. PR description in roam node.
*   **DOTCOMPB-7557** — ADA: Cannot tab to Book Services on desktop. Not started.

### Where to resume

**IMMEDIATE: Finish DOTCOMPB-7712.** On branch `DOTCOMPB-7712`. Implementation complete. Remaining:
1. Write tests: `HcbLocationPhotosPage.test.js` (new), `LocationImageCarousel.test.js` (new), update `HairColorBarLocationHeroV2.test.js` (recover `additionalImagesCount` + `handleImageGalleryClick` tests)
2. Commit + PR creation using `/create-pr` skill

If user wants to **commit DOTCOMPB-7742**: Use `/create-pr` skill — commit msg and PR body in roam node `2026-03-27-120100-dotcompb_7742.org`. Branch `DOTCOMPB-7742`.
If user switches to **DOTCOMPB-7768**: Create roam node first using `/mr-roam-node`.
If user wants to **commit nav scroll work**: Run tests on `DOTCOMPB-7463-nav-bug-fixes`, then commit 5 files.
If user asks for **Site Revolution architecture**: Read `~/.brain.d/roam-nodes/madison_reed/2026-03-18-135209-site_revolution_redesign.org`.

<!-- DESCRIPTION AND USER CONTEXT END -->

<!-- Local Variables: -->
<!-- gptel-model: gemini-pro-paid -->
<!-- gptel--backend-name: "Gemini Local" -->
<!-- gptel--bounds: ((response (40 13827))) -->
