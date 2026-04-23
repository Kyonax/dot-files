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

### 1.18 Code Review Checklist (45 Rules + 2 Graduated 2026-04-14)

> Reference this table when running `/code-review` on any component in this session. Each row maps to the source rule. Load the referenced skill/section before reviewing. Validated on `SiteNavShopContent.vue` and `SiteNavMobileWrapper.vue` (2026-03-17) — 45/45 passed. **(2026-04-14)** Added ad-23 and ad-24 — graduated from Andris PR review comments on DOTCOMPB-7466 into `code-review` skill `rules/mr-review-checklist.md`.

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
| `DOTCOMPB-7289` | Story | Specific Location page updates — HCB details                 | **MERGED** (PR #20137). Hero, about, page scaffolding.                  |
| `DOTCOMPB-7290` | Story | Specific Location page updates — Services + additional info  | **MERGED** (PR #20190). 143 tests. Services, FAQs, reviews, marketing modules. |
| `DOTCOMPB-7556` | Bug   | Add sticky Book Services button to location page             | **MERGED** (PR #20166). FixedCtaBar component.                          |
| `DOTCOMPB-7557` | Bug   | ADA: Cannot tab to Book Services on desktop                  | **MERGED** (2026-04-08, JIRA: Finalizada). Roam node exists.           |
| `DOTCOMPB-7463` | Story | Navigation Redesign                                          | **MERGED** (PR #20210). Bug fixes also merged (PR #20309). Font size merged (PR #20294). |
| `DOTCOMPB-7652` | Bug   | Madi overlapping with sticky CTA                             | **MERGED** (PR #20203). MountedFlag + SierraWidget CSS fix.            |
| `DOTCOMPB-7555` | Bug   | Remove non-functional "Photos" button from location hero     | **MERGED** (PR #20218). 13 tests passing.                              |
| `DOTCOMPB-7749` | Story | Nav title font size increase + CTA implementation            | **MERGED** (PR #20294). Font size bump + nav bug fixes.                 |
| `DOTCOMPB-7763` | Bug   | Mobile Shop submenu not fully scrollable — iOS Safari overlap | **MERGED** (PR #20317, 2026-03-26). iOS scroll fix + header spacing + double-tracking fix. |
| `DOTCOMPB-7742` | Bug   | Featured service CTA on location page doesn't pre-select service in booking flow | **IN TEST** (2026-04-08, JIRA: Pruebas). PR #20368. Cookie-based pre-selection + serverPrefetch fix. See §3.10. |
| `DOTCOMPB-7712` | Story | New page to display location photos                          | **IN CODE REVIEW** (2026-04-23). PR #20423. Branch `DOTCOMPB-7712`. 85 tests, 4 code review rounds. Express `:path?` fix pending commit. See §3.11. |
| `DOTCOMPB-7527` | Story | Dash Hudson Module Updates — UGC carousel style overrides   | **MERGED** (PR #20424, 2026-04-06). CSS `:deep()` overrides, configurable SDK props, event tracking fix, ADA. 118 tests. See §3.13. |
| `DOTCOMPB-7768` | Bug   | Mobile nav dropdowns not scrollable with subcopy text        | **MERGED** (2026-04-08, JIRA: Finalizada). PR #20335.                  |
| `DOTCOMPB-7903` | Bug   | Fix Shop All link not tappable on mobile devices             | **IN PROGRESS** (2026-04-08). `100vh` → `100dvh` fix + removed no-op `env(safe-area-inset-bottom)`. PR #20481 OPEN. See §3.14. |
| `DOTCOMPB-7886` | Story | Go to services page when clicking location                   | **IMPLEMENTED** (2026-04-10). Included in DOTCOMPB-7466 branch. LocationCard + LocationsDirectory redirect when `BookingFlowSiteRevolution` experiment B. 2 tests. See §3.15. |
| `DOTCOMPB-7466` | Story | Shade Shop Page Redesign                                     | **PENDING QA REVIEW** (2026-04-15). PR #20512 OPEN. Andris approved 2026-04-14, ADA review addressed 2026-04-15. OR filter + 11 ADA fixes + 3 justified discards + **sticky-header-wrap ResizeObserver fix** + **View Details redirect fix**. 93 tests on PR surface (62 ShadeShopPage + 15 FilterButtons + 16 LocationCard). PR description fully rewritten in roam node via pr-scribe (2026-04-15). See §3.15. |
| `DOTCOMPB-7944` | Bug | Shop All Products breadcrumb links to wrong URL on Shade Shop PLP | **IN PROGRESS** (2026-04-15). Branch `DOTCOMPB-7466` (piggybacked per user, not a new branch). 1-line fix (line 377 `/shop/all-hair-color` to `/shop-all`) + regression-lock test added to `ShadeShopPage.test.js`. Uncommitted in working tree. Commit message drafted. See §3.16. |
| `DOTCOMPB-7958` | Bug | `[DOTCOMPB-7886]` LocationCard "View Details" link href disagrees with click destination in experiment B | **RESOLVED** (2026-04-15) by the View Details redirect fix bundled in PR #20512. See §3.15 follow-on sub-section. |
| `DOTCOMPB-7959` | Bug | `[DOTCOMPB-7886]` `/colorbar/locations` LocationCard has 4 click targets that all land on services — no path to location details in experiment B | **RESOLVED** (2026-04-15) by the View Details redirect fix bundled in PR #20512 — "View Details" now provides the path back to location details under experiment B. See §3.15 follow-on sub-section. |
| `DOTCOMPB-7960` | Bug | `[DOTCOMPB-7886]` Old location details page `/colorbar/locations/{code}` still serves 200 in experiment B | **SEPARATE TICKET** (out of scope for PR #20512). Concerns whether the old details route should return 301/404 when experiment B is active. Not addressed on this branch. |
| `DOTCOMPB-7961` | Bug | `[DOTCOMPB-7886]` SSR hydration mismatch warning on `/colorbar/locations` (experiment A and B) | **SEPARATE TICKET** (out of scope for PR #20512). SSR-specific issue; not addressed on this branch. |
| DashHudson Research | Research | Platform research + per-location gallery integration plan    | **COMPLETE** (2026-04-06). Full platform analysis documented. Roam node: `2026-04-06-dashhudson_research.org`. |

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
69. **(2026-04-05)** **DashHudson per-location gallery integration — REVISES decision #22**
70. **(2026-04-06)** **`:deep()` for third-party SDK DOM overrides** — When the Dash Hudson (or any third-party) SDK injects its own DOM inside a Vue component, use scoped `:deep()` selectors to override styles. Target SDK class names (`.aspect-ratio-box`, `.ls-slider-item`) directly. Pattern validated on `DashHudsonScriptInner.vue` under `&.version-2`.
71. **(2026-04-06)** **Conditional `aria-labelledby` for slotted headings** — When a heading `id` is in a slot (defined by parent), use `:aria-labelledby="showTitle ? 'ugc-section-title' : null"` on the component root. Vue removes the attribute when value is `null`. Prevents dangling reference when heading hasn't rendered yet.
72. **(2026-04-06, CORRECTED 2026-04-10)** **~~`isFrontEndEvent: true` is explicitly passed~~ → DO NOT pass `isFrontEndEvent: true`** — `segmentTracking.js` (`trackSegmentEvent`) auto-adds it for all frontend events. Passing manually is redundant. Coding standard rule in `.claude/rules/coding-standards.md` explicitly forbids it. DOTCOMPB-7466 removed it from all 4 tracking calls.
73. **(2026-04-06)** **Scope `document.querySelector` to component ref** — When polling for SDK-injected DOM via `waitForElement`, use `container.querySelector()` (scoped to `this.$refs`) not `document.querySelector()`. Prevents wrong element match when multiple widget instances exist. — Decision #22 said "No per-location gallery API." Research confirms Dash Social (rebranded from DashHudson Jan 2025) HAS a Gallery API: `GET /brands/{brand_id}/galleries/{gallery_id}/media`. Per-location segmentation is possible via one gallery per location with a `gallery_id` stored in the location data model (new Tophat field `dashHudsonGalleryId`). Two implementation paths: Option A (widget — client-side, `DashHudsonWidget` component, separate section) or Option B (API — server-side fetch, merge into `galleryImages`, appears in hero +X count and photos page masonry). Full research and plan in session appendix "DASH HUDSON / DASH SOCIAL DEEP RESEARCH".
74. **(2026-04-08, SUPERSEDES decisions #46, #47, #49)** **`100dvh` for mobile nav, not `100vh` + `env(safe-area-inset-bottom)`**
75. **(2026-04-10)** **Experiment-gated redirect inside existing component (not new routes)** — When adding a redesigned variant of an existing page (e.g., Shade Shop replacing `ShopProductCategory` for color family keys), DO NOT create new Vue Router routes. Instead, add a `v-if` inside the existing component that checks the experiment flag and renders the new component. This follows the established `Splitter.vue` pattern. The existing CMS pipeline, Pug templates, and route definitions remain untouched. The redirect is a `defineAsyncComponent` import + 3 computed properties + 1 template `v-if`. Validated: `ShopProductCategory` → `ShadeShopPage` for `ShadeShopSiteRevolution` experiment.
76. **(2026-04-10)** **Experiment-gated components have no SSR** — `this.experiments` (from `globalMixins.data()`) is `{}` during SSR and populated in `mounted()` via `window.experiments`. Experiment B components only render after client `mounted()` — `serverPrefetch()` never runs on them. Data loading must be in `created()` only. Brief V1→V2 flash is the established pattern across all experiment splitters.
77. **(2026-04-10)** **`useScroll` composable for scroll-triggered UI (not `position: sticky`, not polling)** — The codebase has `website/src/vuescripts/composables/useScroll.js` — modern, reactive, 200ms throttled with passive listeners, SSR-safe. Use for sticky headers, scroll-triggered animations, etc. The codebase avoids CSS `position: sticky`. `StickyWrap` (polling-based) exists but `useScroll` is recommended for new code.
78. **(2026-04-10, CORRECTED 2026-04-10)** **~~Coverage data is boolean flags~~ → `grayCoverage` is a single string field** — Runtime API validation confirmed `grayCoverage` is a single string: `"100%"`, `"Gray Blending"`, `"Knockout"`, `"No Gray"`, `"Superior"`, or `undefined`. The boolean flags (`grayCoverageGauranteed`, `noGrayCoverage`, `lowGrayProduct`) exist in PAT field configs but are NOT the source for V2 shop API data. Filter logic uses direct string comparison. — `100vh` on iOS includes area behind URL bar, Dynamic Island, and home indicator — content overflows past the visible viewport. `100dvh` (Dynamic Viewport Height) matches the actual visible area. The `env(safe-area-inset-bottom)` padding hacks in SiteNavShopContent and SiteNavMobileWrapper were no-ops because the site's viewport meta tag lacks `viewport-fit=cover` (without it, `env()` always resolves to `0px`). Fix: `SiteNavMobileV2.vue` `height: 100vh` → `height: 100dvh`. Removed all `padding-bottom: calc(Nem + env(safe-area-inset-bottom))` from ShopContent and MobileWrapper. Browser support: iOS Safari 15.4+, Chrome 108+, Firefox 101+. Branch: `DOTCOMPB-7903`.

79. **(2026-04-15, SUPERSEDES #78 partial)** **OR filter logic for Shade Shop** — Donna Yan confirmed multi-filter should be OR (`activeFilters.some()`), not AND. Also confirmed closest-shades design is no longer necessary since OR logic means results should always be available. `filteredSections` uses `some()`; `closestShades` computed removed; `.suggestions` template removed. Empty-state kept only as safety net for the rare zero-match edge case. Validated in DOTCOMPB-7466.
80. **(2026-04-15)** **WCAG 2.5.3 "Label in Name" — `<div role="link">` for multi-content cards, not native `<a>`** — Level Access ADA scanner flags native `<a>` elements containing multi-part visible text (product name + description + coverage tags) with: *"A element contains visible text not found in the accessible name."* The `<a>` accessible-name algorithm does not aggregate concatenated inner text the way the scanner expects. Use `<div role="link" tabindex="0" @keydown.enter>` with NO `aria-label` — inner text becomes the accessible name and satisfies 2.5.3. Trade-off: Space-key activation and right-click "Open in new tab" unavailable, but Level A compliance takes priority. Same rule applies to single-phrase `<a>` elements where the `aria-label` would insert extra words (e.g., quiz link with `aria-label="Take our quick shade quiz"` vs. visible "take our quick quiz" — scanner flags the mismatch). Solution: drop the `aria-label` and rely on sentence context for 2.4.4 Link Purpose in Context.
81. **(2026-04-15)** **Test files: `import { vi } from 'vitest'` explicitly for new code** — PilkoLint (`bin/autoLintPR.js`) resolves ESLint config differently from local CLI and does not pick up the `vuescripts/.eslintrc.js` test-override that declares `vi` as a global. Any new `vi.fn()`, `vi.mock()`, `vi.stubGlobal()` in test files gets flagged as "`vi` is not defined" on the diff. Add `import { vi } from 'vitest'` at the top of new test files to sidestep the false positive. Existing files can keep the implicit global.
82. **(2026-04-15)** **`filtersEverUsed` flag pattern for transient announcements** — When a live-region announcement should only fire AFTER user interaction (not on initial mount with pre-populated state, e.g., URL filter params), add a boolean flag that flips on first user interaction and gates the announcement. Pattern: `data: { filtersEverUsed: false }` + flip in toggle handlers + check in the computed `resultsAnnouncement`. Prevents silent AT updates on URL-param first-load while still announcing "Filters cleared. N shades available." when user clears all.
83. **(2026-04-15)** **Live region placement: outside `v-if` gate** — The `.hiddenButPresent(aria-live)` element MUST be rendered from component mount, not inside a `v-if="hasProducts"` block. ATs observe mutations to live regions that were already present and empty in the DOM — if the region is created with text already populated (e.g., from a `?coverage=` URL param), the announcement is silently dropped. Wrap the component template in a root structural div (e.g., `.shade-shop-page`) and put the live region as a sibling of the v-if/v-else content.
84. **(2026-04-15)** **Focus management on "See Less" collapse** — When a collapsing toggle may destroy the currently focused DOM node (e.g., expanded product cards beyond the visible count), return focus to the toggle button via `event.currentTarget.focus()` in `$nextTick` after the state change. Pattern: `toggleSectionExpand(sectionType, event) { if (wasExpanded && event?.currentTarget) { this.$nextTick(() => event.currentTarget.focus()); } }`. Template passes `$event`: `@click="toggleSectionExpand(section.type, $event)"`. MrBtn forwards the native DOM event on its emitted `click`, so `event.currentTarget` is the button element.
85. **(2026-04-15)** **`aria-controls` + `aria-expanded` pair on expand/collapse buttons** — `aria-expanded` alone is not enough; add `aria-controls` pointing to the region ID so screen-reader users can navigate to what the button controls. Pattern: dynamic `:id="\`product-grid-${section.type}\`"` on the grid + matching `:aria-controls` on the toggle MrBtn.
86. **(2026-04-15, REINFORCED)** **NEVER run git write commands, even when user says "commit this"** — "Gimme a commit" / "commit this" / "make a commit" means WRITE OUT the commit message as text for the user to copy-paste. It does NOT grant permission to execute `git commit`, `git add`, `git stash`, `git reset`, `git push`, etc. Read-only commands (`git status`, `git diff`, `git log`, `git show`, `git blame`) are fine. Only explicit "run git X" / "go ahead and run git X" grants narrow-scope, single-command authorization. Memory updated in `feedback_no_git.md`.
87. **(2026-04-15)** **Hybrid PR body format — MR top-level skeleton + Kyonax internal formatting** — For this session's MR PRs, the user overrides pr-scribe's default brand detection (MR → MR-only) and requests a hybrid. Apply: (a) MR top-level section headings (`## Checklist for PR Author (Check if it applies)` with 6 MR items, `## Instructions on how QA can test this PR`, `## Special Deployment Requirements`, `## Documentation`); (b) Kyonax internal formatting inside those sections — Pattern B themed Changes subsections (`### Implementation` etc.) with the `[NEW] / [MOD] / [DEL] / [MOV]` closed tag vocabulary + mandatory legend blockquote `> **[NEW]** new file · **[MOD]** modified file · ...`, TD-4FIELD Technical Details with `***Chose:*** / ***Over:*** / ***Why:*** / ***Trade-off:***`, TEST-TWO-TABLE with `**Test runner:**` + `**Command:**` metadata + `#### Automated tests` + `#### Quality gates`, QA-HOW-TO-TEST content style with ASCII flow tree at top + `> **Prereqs:**` blockquote per feature group + `***Expected:***` bold-italic at 6-space indent, DEPLOY-SEVERITY numbered list with `(CRITICAL)` / `(REQUIRED)` / `(OPTIONAL)` labels, DOC-MEDIA-VOCAB `### <MEDIA-TYPE> — <target>` closed vocab; (c) MR reference footer with `atlOrigin` preserved on both ticket URLs. Validated on PR #20512.
88. **(2026-04-15)** **Preserve `atlOrigin` query param on every ticket URL** — MR brand rule "Preserve it verbatim when pasted" (brand-madison-reed.md:119). Every Jira URL in the PR body (top-line + inline summary links + reference footer) should include the `atlOrigin=...` param when available. The atlOrigin on the DOTCOMPB-7466/7886 branch is `eyJpIjoiNWRkNTljNzYxNjVmNDY3MDlhMDU5Y2ZhYzA5YTRkZjUiLCJwIjoiZ2l0aHViLWNvbS1KU1cifQ`.
89. **(2026-04-15)** **Changes bullet format — no brackets around ticket ID** — Pattern A (MR) and hybrid-Pattern-B Changes bullets use bare ticket IDs after the em dash: `- **\`ComponentName.vue\`** — DOTCOMPB-XXXX (\`path/\`):`. NOT `— [DOTCOMPB-XXXX]`. The brackets are reserved for reference-link footer resolution in the summary paragraph, not for bullets. Caught during PR body rewrite audit.
90. **(2026-04-15)** **Vuex 4 `commit` spy asserts positional arg shape** — When spying on `store.commit` in Vitest, Vuex 4 invokes the wrapped commit with `(name, payload, options)` where `options` is typically `undefined`. `toHaveBeenCalledWith('name', expected)` fails because the actual call has 3 args. Use `commitSpy.mock.calls.find(call => call[0] === 'mutation/name')` to locate the call, then `toEqual` on `call[1]` for the payload shape. Pattern validated on the `/shop-all` breadcrumb regression-lock test in `ShadeShopPage.test.js`.
91. **(2026-04-15)** **When discovering a follow-on bug from an open PR, stay on the same branch if user requests** — User preference: when a bug is discovered in work shipped by an open PR that is still in review (e.g., PR #20512 awaiting QA on DOTCOMPB-7466, bug DOTCOMPB-7944 filed against it), do NOT auto-create a new branch. Piggyback the fix on the existing branch so the PR carries both the feature and the fix to QA together. Roam node for the bug still gets created with proper parent cross-reference.
92. **(2026-04-15)** **`:style` binding is allowed and idiomatic in this codebase** — 69 usages across `website/src/vuescripts/**/*.vue`. No ESLint rule forbids it (`vue/no-static-inline-styles` is NOT configured). No `.claude/rules` or skill file prohibits it. Use `:style` when the value is a runtime-measured numeric value that cannot be expressed as a utility class or pre-computed via CSS variables — e.g., `:style="stickyHeaderStyle"` returning `{ top: '${offset}px' }` from a `ResizeObserver` measurement. Static values still belong in utility classes or Stylus; `:style` is only for dynamic runtime values. Precedents: `ColorBarLocationSectionV1.vue:43` (`:style="mapColumnTopStyle"`), `PurchasePanelV2Modal.vue:110` (`:style="rightColStyles"`), `HcbGmap.vue:14` (`:style="mapDimensions"`), `MarqueeText.vue:2` (`:style="{'--text-size': textWidth + 'px', ...}"`).
93. **(2026-04-15)** **`.sticky-header-wrap.is-sticky` is the SINGLE site-wide sticky scaffold, gated on `BookingFlowSiteRevolution === 'B'`** — Defined in `SsrApp.vue:90` and `views/desktop/vue-layout.pug:39`. Contains `SiteMessageBanner` (sitewide topics), `SiteMessageBannerCarousel` (content topics), `MrNavigation` — three siblings, in that vertical order. When experiment B is active, `vue-layout.styl:451-454` applies `position: sticky; top: 0; z-index: 1400` to the wrap. When experiment is A (control), the wrap scrolls with the page. Any component adding its own sticky element below the site nav MUST observe this wrap's actual rendered height to avoid banner / carousel overlap. **Checking `.is-sticky` via `document.querySelector('.sticky-header-wrap.is-sticky')` is preferred over checking `this.experiments?.['BookingFlowSiteRevolution'] === 'B'` directly — the selector decouples from the experiment name so future stickiness-gating changes in SsrApp.vue don't require touching consumers.**
94. **(2026-04-15)** **`--mr-navigation-height` is incomplete — only measures `.mr-navigation`, not the full sticky wrap** — Set in `MrNavigation.vue:159-168` via `useResizeObserver(mrNavigationRef, ...)` where `mrNavigationRef` points to the `.mr-navigation` root element. It measures ONLY the nav's border-box, NOT the sitewide banner or carousel above it. Consumers using `top: var(--mr-navigation-height)` for their own sticky elements will overlap the banner + carousel when experiment B makes the wrap sticky with all three children visible. Do NOT rely on this variable alone for sticky offsets when banners may be present.
95. **(2026-04-15)** **`--mr-sticky-header-height` is referenced but NEVER set — dead variable** — `ColorKitHeroV3.vue:97` uses `top: calc(var(--mr-sticky-header-height, 0px) + 16px)` with a hardcoded `top: 144px` override and `// TODO: remove when DOTCOMPB-7713 get merge` comment. Grep confirms no file in the codebase calls `setProperty('--mr-sticky-header-height', ...)`. The variable falls back to `0px`, so ColorKitHeroV3's PDP V3 layout relies on the magic `144px`. If you introduce this variable, audit every other sticky element that reads it.
96. **(2026-04-15)** **Correct pattern for full-stack sticky offset: observe `.sticky-header-wrap.is-sticky` via `ResizeObserver`** — Scales to every scenario (experiment A / B, banner visible / dismissed, carousel present / absent, mobile / desktop breakpoints, banner state changes mid-session). Pattern: in `mounted`, `$nextTick` → `document.querySelector('.sticky-header-wrap.is-sticky')` → if found, read `getBoundingClientRect().height` + attach `ResizeObserver` to update on resizes; if not found, offset stays `0`. Tear down in `beforeUnmount`. SSR-guard with `typeof window === 'undefined' || typeof ResizeObserver === 'undefined'`. Validated on `ShadeShopPage.vue` (follow-on fix for DOTCOMPB-7466).
97. **(2026-04-15)** **`inSiteRevolutionExperiment` computed exists on `mrVueApp.js:393-395` but is NOT a global mixin — not inherited by components** — This computed is defined inside the root app object (alongside `customerInfo`, `urlParams`) but it is NOT in `globalMixins.js`. Individual components must either define their own local computed or check `this.experiments?.['BookingFlowSiteRevolution'] === 'B'` directly. `this.experiments` IS available globally via `globalMixins` (populated in `mounted()` from `window.experiments`). Already-validated local-check pattern: `LocationCard.vue:72-74`, `LocationsDirectory.vue:93-95`, `MrFooter.vue:123`.
98. **(2026-04-15)** **`LocationCardBody` child has its own local `detailsUrl` computed that the parent's experiment-gating does NOT cover** — The child template wires `@click.prevent="$emit('details-click')"` + `:href="detailsUrl"` where the child's `detailsUrl` is hardcoded to `/colorbar/locations/{code}`. Parent `LocationCard.onCardClick` handles the emit and uses the PARENT's experiment-gated `detailsUrl`. This creates a **left-click vs right-click divergence bug**: right-click "Open in new tab" follows the child's `href` (details page) while left-click goes through the parent handler (services page under experiment B). Filed as DOTCOMPB-7958 and DOTCOMPB-7959. **Fix pattern:** add a dedicated parent handler (`onViewDetailsClick`) that always routes to the non-experiment-gated URL, wire the emit to the new handler (not `onCardClick`). Child's `detailsUrl` can stay hardcoded — parent controls the destination via the handler it wires to `@details-click`.
99. **(2026-04-15)** **DOTCOMPB-7886 AC1 scope is the location NAME only** — Ticket AC1 says `WHEN I click on the NAME of a location from /colorbar/locations, THEN I should be taken to the services screen`. Explicitly scoped to the `.location-name` anchor. The AC is silent on "View Details", "card image", and other click targets. Sibling clickable elements (card image, View Details, Book Service CTA) are out of AC1 scope and can route independently — View Details intentionally routes to the details page (pre-change behavior) to preserve a path to `/colorbar/locations/{code}` under experiment B.
100. **(2026-04-15)** **Local dev server is LAN-reachable out of the box — no config change needed** — `website/gulpfile.js:239-246` BrowserSync proxies Express on port 3001 and listens on port 3000 on `0.0.0.0`. Mac LAN IP works: `http://<lan-ip>:3000/shop/brown` from any device on the same Wi-Fi. Caveats: macOS firewall may block; guest Wi-Fi networks with client isolation block device-to-device traffic; experiment cookies (`abt_*`) scoped to `mdsnrd.com` don't transfer — other devices get fresh assignments, which is actually useful for testing experiment A/B states with `?xid=<id>&v=A|B` override. For HTTPS / cross-network / firewall-blocked scenarios, use `cloudflared tunnel --url http://localhost:3000` or `ngrok http 3000`.
101. **(2026-04-15)** **`gh api` write operations require explicit user authorization (parallel to no-git-writes rule)** — `gh pr comment`, `gh api POST`, `gh pr edit`, `gh pr merge`, `gh pr close`, `gh issue comment`, etc., are externally visible and only partially reversible. The same authorization gate as git writes applies: phrases like "gimme a comment" / "draft a reply" mean WRITE OUT the text for the user to copy-paste. Only explicit "post it now" / "use gh now" / "push that comment with gh" authorizes execution — and only for the exact command scoped to that authorization. Read-only `gh` commands (`gh pr view`, `gh pr list`, `gh api` GET requests, `gh pr checks`) are unrestricted. Source: validated 2026-04-15 — user authorized `gh api POST /pulls/20512/comments/{id}/replies` with explicit "use gh now". See §5 last interaction for the specific rebuttal that was posted as `r3090109723` in response to an AI-agent review comment (`r3090085918`).
102. **(2026-04-15)** **Pattern for rebutting AI review agents that lack codebase context** — AI agents reviewing MR PRs do not have access to the session file, loaded skills, or the full Tophat / view-route / globalMixins data-flow context. They flag surface-level issues like "no input validation" as bugs when the input domain is actually constrained by construction (tightly-scoped Vue Router regex, component-exclusive URL writers, experiment-gated rendering). When rebutting: (a) name the routing boundary (`Shop/routes.js` declaration + constraint regex), (b) identify the exclusive writer of the value in question (usually the component's own `updateUrlFilters` / `setBreadcrumbs` / `$router.push`), (c) explain round-trip invariants (shared links come from the same pipeline), (d) distinguish legitimate terminal states from bug paths (empty-state safety nets), (e) offer defense-in-depth hardening as a separate follow-up if the team wants belt-and-suspenders, but hold the line that a "potential bug" label overstates unreachable inputs. Validated pattern on PR #20512 comment `r3090109723`.

103. **(2026-04-22)** **CMS `parseUrl` parameter-count validation is the bottleneck for Vue Router children direct access** — `parseUrl` (`mr_modules/cms/lib/router.js:650`) validates incoming URL segment count against Tophat `urlParameterList.length`. Extra segments (e.g., `/photos` after `/:locationCode`) fail validation → raw URL returned → `loadPage` exact-match fails → 404. Vue Router children are client-side only and have zero effect on server-side URL resolution. Tophat must define optional parameters for each child route segment.
104. **(2026-04-22)** **`req.url` is coupled between CMS resolution and Vue Router SSR** — `htmlRenderer.js:536` reads `req.url` → `locals.parsedUrl` → `ssrContext.url` → `server.js:22` `router.replace(context.url.path)`. URL rewriting for CMS breaks Vue Router SSR (different child rendered on server vs client → hydration mismatch). Cannot decouple without modifying core CMS infrastructure.
105. **(2026-04-22)** **Tophat optional second URL parameter for location page children** — CMS content `_id: 2350` (`/colorbar/locations/`) has `urlParameterList: [{ name: 'locationCode', optional: false }, { name: 'photos', optional: true }]`. Makes `parseUrl` accept `/peachtreecorners/photos` (2 segments ≤ 2 params) → returns base URI → `loadPage` finds CMS content → HcbLocationPageV2 renders → Vue Router SSR navigates to `/photos` child. No hydration mismatch.
106. **(2026-04-22)** **Route cache refresh via Redis broadcast** — `initRoutes()` runs at startup (`router.js:741`) and on Redis `PARAM_ROUTES_INVALID` event (`router.js:84`). Manual trigger: `redis-cli publish broadcast '{"event":"PARAM_ROUTES_INVALID","data":{"env":"DEV"}}'`. Also fires on CMS content save (`content.js:59`, `dataObject.js:43`). Server restart not required if Redis is running.
107. **(2026-04-23)** **Express `:path?` generic optional param for location subpage validation** — Changed `/colorbar/locations/:urlKey` to `/colorbar/locations/:urlKey/:path?` so the existing validation handler (location exists → 404 if not, sets `req.metaData`) also covers child routes like `/photos`. Handler body unchanged — only uses `req.params.urlKey`, `:path` is ignored. Replaces hardcoded `['/colorbar/locations/:urlKey', '/colorbar/locations/:urlKey/photos']` array.

### 2.4 PR Review Resolutions (DOTCOMPB-7289)

8 comments addressed: magic numbers → named constants; repeated boolean logic → `isSelected(slideImage)` method; oddly specific em values → reverted to `px`; inline event logic → dedicated methods; placeholder button → `// TODO:` comment; resize listener → `window.matchMedia` API; `@hook:mounted` → custom `@ready` event (later deprecated); `getObjProperty` → optional chaining (`?.`).

### 2.5 Pending Work

*   **DOTCOMPB-7903** — PR #20481 OPEN (2026-04-08). `100dvh` fix for mobile nav viewport. Branch `DOTCOMPB-7903`. Changes staged, not yet committed.
*   **DOTCOMPB-7712** — PR #20423 OPEN. In Code Review (JIRA). Branch `DOTCOMPB-7712`. 85 tests, 4 code review rounds. Express `:path?` fix pending commit + push.
*   **DOTCOMPB-7742** — PR #20368. In Test (JIRA: Pruebas). Cookie-based service pre-selection.
*   **DOTCOMPB-7886** — Implemented (2026-04-10). Included in DOTCOMPB-7466 PR #20512. LocationCard + LocationsDirectory experiment redirect. 2 tests.
*   **DOTCOMPB-7466** — PR #20512 OPEN (2026-04-15). Approved by Andris 2026-04-14; ADA review addressed in commit `41c4a25`. **PR description fully rewritten in roam node via pr-scribe (Hybrid MR+Kyonax format)** — user can copy-paste from the `* PR BODY` `#+begin_src markdown` block in the roam node. Labels updated to `DOTCOM TEAM`, `Pending QA Review`, `Special Deploy Requirements`. 93 tests on PR surface. **Three follow-on fixes uncommitted in the working tree** — all on the same branch per user instruction: (1) DOTCOMPB-7944 Shop All breadcrumb, (2) sticky-header `ResizeObserver` rewrite, (3) View Details redirect fix (also resolves DOTCOMPB-7958 and DOTCOMPB-7959). See §3.15.
*   **DOTCOMPB-7944** — NEW BUG (2026-04-15). Shop All Products breadcrumb links to wrong URL on Shade Shop PLP. Fix implemented on branch `DOTCOMPB-7466` (piggybacked per user, not a new branch). 1-line component change + 1 regression-lock test. Uncommitted in working tree. Commit message drafted and ready. See §3.16.
*   **Sticky-header-wrap fix on Shade Shop PLP** (no ticket number — follow-on to DOTCOMPB-7466 reported via live QA). Fix uses `document.querySelector('.sticky-header-wrap.is-sticky')` + `ResizeObserver` to compute `top` offset dynamically. Scales to experiment A / B, banner presence, carousel presence, breakpoint changes. Uncommitted. +4 regression-lock tests. See §3.15 follow-on sub-section.
*   **View Details redirect fix** (no dedicated ticket — resolves DOTCOMPB-7958 and DOTCOMPB-7959 by side effect). `LocationCard.vue` adds `locationDetailsUrl` computed + `onViewDetailsClick` handler; template rewires `@details-click` from `onCardClick` to `onViewDetailsClick`. Uncommitted. +2 regression-lock tests. See §3.15 follow-on sub-section.
*   **DOTCOMPB-7717 cleanup** — MarketingBanner dead workaround removal. Plan in §3.8. Not yet implemented.
*   **DOTCOMPB-7555_full_width** — Parked carousel work. Activate only when business confirms desktop banner carousel for location hero.
*   **DashHudson Integration** — **RESEARCH COMPLETE** (2026-04-06). Awaiting PM input on open questions.

---

## SECTION 3: FEATURE / TICKET IMPLEMENTATIONS

> Each subsection documents a specific ticket's implementation: what was built, where it lives, what decisions were made, and how to verify it.

---

### 3.1 DOTCOMPB-7289: Specific Location Page Updates (HCB Details)

**Created:** ~2026-03-02 | **Last updated:** 2026-04-06
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-02-131328-dotcompb_7289.org`
**Branch:** `DOTCOMPB-7289_new_feat` → `feat-website-booking-flow-site-revolution_with_performance`
**Status:** **MERGED** (PR #20137). Hero, about, and page scaffolding complete.

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

**Created:** ~2026-03-11 | **Status:** **MERGED** (PR #20166). 35 tests passing.
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-11-165514-dotcompb_7556.org`

**`FixedCtaBar`** — `website/src/vuescripts/components/HairColorBarBookingV2/components/FixedCtaBar/` — Fixed bottom CTA bar. Props: `visible`, `ctaText` (required), `trackEventName`, `redirectUrl`, `ctaDisabled`, `ctaLoading`, `ariaLabel`. Emits: `cta-click`. No store coupling.

---

### 3.3 DOTCOMPB-7557: ADA — Cannot Tab to Book Services on Desktop

**Created:** ~2026-03-10 | **Status:** Roam node created. Not yet implemented.
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-10-122138-dotcompb_7557.org`

---

### 3.4 DOTCOMPB-7290: Specific Location Page — Services + Additional Info

**Created:** 2026-03-12 | **Last updated:** 2026-04-06
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-12-114716-dotcompb_7290.org`
**Branch:** `DOTCOMPB-7290` → `feat-website-booking-flow-site-revolution_with_performance`
**PR:** [#20190](https://github.com/MadisonReed/mr/pull/20190) | **Status:** **MERGED**. 143 tests passing.

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

**Created:** 2026-03-16 | **Last updated:** 2026-04-06
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-16-043543-dotcompb_7463.org`
**Branch:** `DOTCOMPB-7463` (main PR) + `DOTCOMPB-7463-nav-bug-fixes` (post-PR bug fixes)
**PR:** [#20210](https://github.com/MadisonReed/mr/pull/20210) | **Status:** **MERGED**. Bug fixes also merged (PR #20309). Font size merged (PR #20294).

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

**Created:** 2026-03-17 | **Status:** **MERGED** (PR #20203).
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-03-17-065717-dotcompb_7652.org`
**Branch:** `DOTCOMPB-7652`

**Fix:** `MountedFlag(v-if="!isDesktop" flag="bt-with-sticky-cta")` in `HcbLocationPageV2.vue` adds body class on mobile. `SierraWidget.vue` shifts launcher up 90px via `transform: translateY(-90px)` inside `@media mq-tablet-less` when `.bt-with-sticky-cta` is present.

**Also fixed in this session:** `HairColorBarLocationReviews` "See More Reviews" MrBtn aligned with `HairColorBarLocationAbout` pattern — removed direct class, used `:deep(.mrbtn)` from root wrapper, added `aria-label="See more reviews on Birdeye (opens in new tab)"`.

---

### 3.7 DOTCOMPB-7555: Remove Non-functional "Photos" Button from Location Hero

**Created:** 2026-03-18 | **Status:** **MERGED** (PR #20218). 13 tests passing.
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
**Status:** **MERGED** (PR #20294). Font size bump + nav bug fixes.

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
**Status:** **PR #20368 OPEN** (2026-03-30). Implemented, code reviewed, 19 tests passing, full suite clean (5263/5263).

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
**Status:** **PR #20423 OPEN** (2026-04-06). Committed (`6d5791497e6`), pushed, PR created. 81 tests passing. 3 code reviews completed.

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
| Consolidate handleBackClick/handleCloseClick | 2026-04-21 | Andris review: DRY. Consolidated into `handleNavigateBack(action)` with string interpolation for event name. |
| ~~Express `/photos` route consolidation (Option C)~~ | 2026-04-22 | SUPERSEDED by component-less parent route below. |
| Component-less parent route + Express consolidation | 2026-04-22 | Andris review: nested children structure. Direct nesting (HcbLocationPageV2 as route component) causes double-render (CMS + router both instantiate it). Solution: component-less parent (no `component` property, just `path` + `children`). Vue Router hoists children to depth 0 — same behavior as flat routes but grouped. Express `/photos` consolidated into existing handler at L1602. Dashboard's EditAutoDelivery is the only codebase precedent for nested `children:` — works because Dashboard (CMS) and EditAutoDelivery (route parent) are separate components. |
| `import { vi } from 'vitest'` in all test files | 2026-04-21 | ESLint 9 flat config migration (from master) dropped Vitest globals. Explicit import resolves `vi is not defined` lint errors. No runtime impact. |
| Tophat optional second URL parameter | 2026-04-22 | CMS content `_id: 2350` already had `photos` optional param in `urlParameterList`. Makes `parseUrl` accept `/code/photos` URLs → same CMS page. |
| Express `:path?` generic optional param | 2026-04-23 | Replaces hardcoded `[':urlKey', ':urlKey/photos']` array. Single route `/colorbar/locations/:urlKey/:path?` covers any child. Handler body unchanged. |

**Tests:** 85 passing across 5 test files. All components have coverage.

**Code Review Round 3 (2026-04-21, Andris review #2):**

Andris requested 2 changes. Both addressed:
1. `HcbLocationPhotosPage.vue:112` — Consolidate `handleBackClick`/`handleCloseClick` into `handleNavigateBack(action)`. DONE.
2. `views.js:1581` — Remove Express `/photos` route, use nested Vue Router children instead. See implementation plan below.

**IMPLEMENTATION PLAN: Component-Less Parent Route + Express Consolidation**

*Source: Andris PR comment thread on `views.js:1581` (2026-04-09 through 2026-04-22). Andris proposed nested Vue Router children. Direct nesting with HcbLocationPageV2 as route component causes a double-render (CMS renders it, then router-view renders it again inside itself). Solution: component-less parent route groups children without entering the route tree. Express `/photos` route consolidated into existing handler.*

*Supersedes: Option C (flat routes + Express consolidation only). This plan addresses both Andris concerns: nested children structure AND Express route removal.*

*** Background: Why Direct Nesting Fails (Double-Render Problem)

Andris suggested `component: HcbLocationPageV2` as the route parent with `children:`. This creates a double-render:

1. CMS renders HcbLocationPageV2 (OUTER instance, receives `cmsSettings` prop from CMS)
2. Outer instance's `<router-view>` at depth 0 renders `matched[0]` = HcbLocationPageV2 (INNER instance)
3. Inner instance has NO `cmsSettings` (CMS passes props to outer only, not router-rendered inner)
4. Inner's `routeViewProps` passes empty CMS data to children -- broken page

The Dashboard's `EditAutoDelivery` avoids this because Dashboard (CMS wrapper) and EditAutoDelivery (route parent) are SEPARATE components. HcbLocationPageV2 is both -- hence the double-render.

*** Solution: Component-Less Parent Route

Vue Router 4 supports routes with `children` but NO `component`. When the parent has no component:
- No entry in the `matched` array for the parent (no depth-0 component)
- Children resolve at depth 0 directly (same as flat routes)
- HcbLocationPageV2 (CMS-rendered) stays outside the route tree -- no double render
- Its `<router-view>` at depth 0 renders children normally
- Path inheritance gives the grouped structure Andris wants

*** Step 1: Update `routes.js`

**File:** `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/routes.js`

BEFORE (flat siblings, absolute paths):
```javascript
export default [
  {
    name: 'location-details',
    path: '/colorbar/locations/:locationCode',
    component: HcbLocationSections,
  },
  {
    name: 'location-photos',
    path: '/colorbar/locations/:locationCode/photos',
    component: HcbLocationPhotosPage,
  },
];
```

AFTER (component-less parent, relative paths):
```javascript
export default [
  {
    path: '/colorbar/locations/:locationCode',
    children: [
      {
        path: '',
        name: 'location-details',
        component: HcbLocationSections,
      },
      {
        path: 'photos',
        name: 'location-photos',
        component: HcbLocationPhotosPage,
      },
    ],
  },
];
```

What changes:
- Parent route wraps both children, has `path` but NO `component` and NO `name`
- `location-details` path: `/colorbar/locations/:locationCode` (absolute) to `''` (relative)
- `location-photos` path: `/colorbar/locations/:locationCode/photos` (absolute) to `'photos'` (relative)
- Route names (`location-details`, `location-photos`) stay identical

What does NOT change:
- Imports (same HcbLocationSections static, HcbLocationPhotosPage dynamic)
- Route names (all `$router.push({ name: 'location-photos' })` calls still work)
- `params: { locationCode }` in navigation (inherited from parent path pattern)

*** Step 2: Delete Commented-Out Express Handler

**File:** `website/src/routing/views.js`

DELETE lines 1593-1601 entirely (the commented-out separate handler).

*** Step 3: Add `/photos` to Existing Express Validation Handler

**File:** `website/src/routing/views.js` line 1603

BEFORE:
```javascript
app.get('/colorbar/locations/:urlKey', (req, res, next) => {
```

AFTER:
```javascript
app.get(['/colorbar/locations/:urlKey', '/colorbar/locations/:urlKey/photos'], (req, res, next) => {
```

Handler body UNCHANGED. Both paths extract `:urlKey` the same way, run `colorbarCache.getLocation`, return 404 if invalid, call `next()` if valid.

*** Step 4: Run Tests (Expect Zero Failures)

```bash
cd website && npm run test:vue HcbLocationPageV2.test HcbLocationSections.test HcbLocationPhotosPage.test HairColorBarLocationHeroV2.test LocationImageCarousel.test
```

Expected: 85/85 passing. No test changes needed because:
- Tests mock `router-view` as a stub (route structure irrelevant)
- Tests mock `$router.push` (route names unchanged)
- Tests mount with direct props (not through router)

*** Step 5: Manual Verification

1. `npm run dev` -- SPA navigation: click "+X photos" from location page, verify URL updates
2. Direct URL `/colorbar/locations/nyc-flat/photos` -- page loads with images
3. Direct URL `/colorbar/locations/invalid-code/photos` -- returns 404
4. Back/close buttons on photos page -- redirect to location page
5. Mobile carousel "+X photos" overlay -- navigates to photos page

*** Step 6: Update PR Body in Roam Node

1. Changes section: update `routes.js` entry ("component-less parent with nested children")
2. Changes section: update `views.js` entry ("consolidated `/photos` into existing handler")
3. Technical Details: replace "Express 404 without URL rewrite" with "Express validation consolidated + component-less parent route"

*** Step 7: Reply to Andris + Push

Post reply to PR thread, then push the commit.

*** Verified: What Does NOT Change

| File | Why unchanged |
|---|---|
| `HcbLocationPageV2.vue` | CMS-rendered wrapper, not in route tree. Template, props, serverPrefetch, computeds identical. |
| `HcbLocationSections.vue` | Receives props from parent's `routeViewProps`. Same depth, same props. |
| `HcbLocationPhotosPage.vue` | `handleNavigateBack` uses hardcoded URL, not named route. Unchanged. |
| `HairColorBarLocationHeroV2.vue` | `$router.push({ name: 'location-photos' })` -- name unchanged, params inherited. |
| `LocationImageCarousel.vue` | Same `$router.push` pattern. |
| `mrVueApp.js` | `...hcbLocationPageRoutes` spread still valid -- one object with children. |
| `ssr/router.js` | Same spread pattern. |
| `registerGlobalsSsr.js` | HcbLocationPageV2 global registration unchanged. |
| All 5 `.test.js` files | Mock `router-view` as stub, mock `$router.push` by name. Zero test changes. |

*** Risk Mitigations

| Risk | Why safe |
|---|---|
| Named route resolution with nameless parent | Vue Router resolves by name, not hierarchy. `location-photos` resolves regardless of parent name. |
| SSR route matching | SSR doesn't render `<router-view>` (per server.js L32-33 comment). Route matching works the same. |
| Spread operator with nested structure | `[{ path, children }]` spreads into routes array identically to Dashboard's EditAutoDelivery pattern. |
| Express `:urlKey` for both paths | Single-segment match. `/nyc-flat` and `/nyc-flat/photos` both extract `urlKey` = `nyc-flat`. |
| `$router.push` params | `locationCode` param inherited from parent path `/:locationCode`. Navigation unchanged. |

*** Total Diff

| File | Change |
|---|---|
| `routes.js` | Restructure: flat siblings to component-less parent with children |
| `views.js` | Delete commented handler (9 lines) + add `/photos` to path array (1 line) |
| **Total** | **2 files modified, 0 created, 0 deleted, 0 test changes**

**Code Review Round 4 (2026-04-22, Andris review #3):**

Andris re-reviewed. 1 new `CHANGES_REQUESTED` comment on `views.js:1593`: "We should not need these routes anymore, especially the photos one." — Wanted Express `/photos` route removed entirely.

**Deep CMS investigation (2026-04-22):** Traced full request flow for direct `/photos` access without Express route:
1. Express `:urlKey` / `:hcbKey` only match single path segments — `/peachtreecorners/photos` skips all Express handlers
2. CMS catch-all `pageRouter` → `parseUrl(req)` → parameter-count validation against Tophat `urlParameterList`
3. `req.url` is coupled between CMS (`htmlRenderer.js:536`) and Vue Router SSR (`server.js:22`) — URL rewriting causes hydration mismatch
4. Discovery: Tophat CMS content `_id: 2350` already has optional second parameter `photos` in `urlParameterList` — added weeks ago
5. With Tophat param: `parseUrl` accepts 2 segments → CMS resolves → Vue Router SSR navigates to `/photos` child → no hydration mismatch
6. BUT: invalid location codes on `/photos` return 200 (CMS serves page, `serverPrefetch` fails silently, `v-if="location?.code"` hides content — blank shell, not 404)
7. Fix: change Express route from hardcoded array to `/colorbar/locations/:urlKey/:path?` — generic optional param covers any child route. Handler body unchanged.

**PR comment posted** (2026-04-22): Reply `r3126988258` to `r3125738124` explaining findings — Vue Router children alone don't handle direct access, Express validation doesn't cover subpages, `:path?` fix.

**Route cache refresh mechanism documented:** `initRoutes()` at startup + Redis `PARAM_ROUTES_INVALID` broadcast. Manual: `redis-cli publish broadcast '{"event":"PARAM_ROUTES_INVALID","data":{"env":"DEV"}}'`

**Code Review (2 rounds, 2026-04-03):**
- Round 1: 8 subagents, 30 findings (1 CRITICAL, 5 HIGH, 12 MEDIUM, 12 LOW). 5 implemented, 25 skipped.
- Round 2: 8 subagents, 18 findings (0 CRITICAL, 4 HIGH, 7 MEDIUM, 7 LOW). 7 implemented, 10 skipped (1 N/A).
- Total implemented across both rounds: nested interactive fix, focus-visible, computed alphabetization (×2), hidden h2 removal, utility prefix fixes, magic number constant, CSS alphabetization, nested landmark fix, aria-hidden on icons, skeleton backgrounds.

### 3.13 DOTCOMPB-7527: Dash Hudson UGC Carousel Style Overrides

**Created:** 2026-04-06 | **Last updated:** 2026-04-06
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-04-06-160000-dotcompb_7527.org`
**Branch:** `DOTCOMPB-7527`
**Status:** **MERGED** (PR #20424, 2026-04-06). 118 tests passing.

**Scope:** Restyle the Dash Hudson UGC module on the RCC PDP to match the 2026 Redesign Figma. CSS overrides, configurable SDK props, event tracking fix, ADA, CTA removal.

**Component chain:** `ColorKitPdpV2/V3` → `DashHudsonScript` (pass-through) → `DashHudsonScriptInner` (SDK loader + styles).

**Changes:**
*   **`DashHudsonScriptInner.vue`** — 7 new configurable SDK props (`carouselDots`, `gapSize`, `infinite`, `mobileGapSize`, `mobileRowSize`, `rowSize`, `scrollDisabled`) with backward-compatible defaults. CSS `:deep()` overrides under `&.version-2`: `aspect-ratio: 4/5`, `border-radius: 12px`, `object-fit: cover`, `overflow: hidden`. Removed "Explore Similar Shades" CTA + `tryItOn` + `openPlaygroundModal` + `mapActions`. Event tracking fixed: `mix_trackEvent` → `trackMREvent('PDP - UGC module clicked', { isFrontEndEvent: true })`. ADA: `role="region"` + conditional `aria-labelledby`. Scoped `document.querySelector` to `this.$refs.dhUgc`. Code cleanup (single quotes, object class syntax, simplified `showTitle`).
*   **`DashHudsonScript.vue`** — Pass-through for all 7 new props.
*   **`ColorKitPdpV2.vue`** + **`ColorKitPdpV3.vue`** — Carousel config: `mobile-row-size="1.5"`, `:infinite="true"`, `:carousel-dots="true"`, `:scroll-disabled="true"`. Title: `h2#ugc-section-title.f-domaine-display-condensed.xs-f-xlarge.md-f-grande.max-at-tweak.brand-color-1.upper`.
*   **`PdpBottom.test.js.snap`** — Snapshot updated with new prop defaults.

**Key Decisions:**

| Decision | Date | Rationale |
|---|---|---|
| CSS `:deep()` for SDK overrides, not `data-media-format` | 2026-04-06 | SDK attribute only supports `"original"`, not custom ratios. `:deep()` gives full control over aspect ratio, radius, and overflow. |
| Remove CTA for version 2 | 2026-04-06 | Figma has no "Explore Similar Shades" button. QuizResults page has its own button in `NotExactMatchSection`. |
| `trackMREvent` not `mix_trackEvent` | 2026-04-06 | `trackMREvent` is global mixin standard. `mix_trackEvent` was from deprecated `trackEventMixin`. Event name matches JIRA spec. |
| Tablet 3.5 items deferred | 2026-04-06 | DH SDK has no `data-tablet-row-size`. Only mobile/desktop split. Tablet override needs `:deep()` CSS media query — requires deployed env to verify SDK breakpoint. |
| DH SDK empty on localhost | 2026-04-06 | SDK only serves content to whitelisted domains. Style overrides must be verified on deployed QA environment. |

**Tests:** 118 tests across 4 suites (ColorKitPdpV3 76, LightWorksPdp 37, NotExactMatchSection 4, PdpBottom 1). All passing.

---

### 3.14 DOTCOMPB-7903: Fix Shop All Link Not Tappable on Mobile Devices

**Created:** 2026-04-08 | **Last updated:** 2026-04-08
**Branch:** `DOTCOMPB-7903`
**PR:** #20481 | **Status:** OPEN. Changes staged, PR description updated.

**Root cause:** `SiteNavMobileV2.vue` used `height: 100vh`, which on iOS includes the area behind the URL bar, Dynamic Island, and home indicator. The flex container was taller than the visible viewport, pushing bottom nav items (Shop All) behind the home indicator into an untappable zone. Additionally, `SiteNavShopContent` and `SiteNavMobileWrapper` had `padding-bottom: calc(Nem + env(safe-area-inset-bottom))` — these were no-ops because the site's viewport meta tag lacks `viewport-fit=cover`, so `env(safe-area-inset-bottom)` always resolved to `0px`.

**Fix (3 files):**
- **`SiteNavMobileV2.vue`** — `height: 100vh` → `height: 100dvh` (the actual fix)
- **`SiteNavShopContent.vue`** — Removed `padding-bottom: calc(4em + env(safe-area-inset-bottom))` from `@media mq-desktop-md-less` and `@media mq-mobile`
- **`SiteNavMobileWrapper.vue`** — Removed `padding-bottom: calc(4em + env(safe-area-inset-bottom))` from `.mobile-wrapper`

**Key Decision:**

| Decision | Date | Rationale |
|---|---|---|
| `100dvh` over `viewport-fit=cover` + `env()` | 2026-04-08 | Adding `viewport-fit=cover` to the viewport meta is a site-wide change affecting all pages. `100dvh` solves the container sizing problem without global side effects. `dvh` support: iOS Safari 15.4+, Chrome 108+, Firefox 101+. |

---

### 3.15 DOTCOMPB-7466: Shade Shop Page Redesign

**Created:** 2026-04-08 | **Last updated:** 2026-04-15
**Branch:** `DOTCOMPB-7466`
**Status:** PR #20512 OPEN. Andris approved 2026-04-14; ADA review batch addressed 2026-04-15 in commit `41c4a25` (11 fixes, 3 justified discards). **PR description rewritten in roam node via pr-scribe (2026-04-15, Hybrid MR+Kyonax format)** — ready for user to copy-paste from roam node `* PR BODY` block. Three follow-on fixes uncommitted in the working tree on the same branch: **(1) DOTCOMPB-7944** Shop All breadcrumb URL, **(2) sticky-header-wrap ResizeObserver rewrite** (no ticket), **(3) View Details redirect fix** (no dedicated ticket — resolves DOTCOMPB-7958 and DOTCOMPB-7959). 93 tests on PR surface (62 ShadeShopPage + 15 FilterButtons + 16 LocationCard). See §3.16 for DOTCOMPB-7944 and the two follow-on sub-sections below for the other fixes.
**Roam node:** `~/.brain.d/roam-nodes/madison_reed/2026-04-08-120100-dotcompb_7466.org`

**Architecture:**
- *Route:* New route `/shop/:colorFamily(brown|blonde|red|black)` in `Shop/routes.js` before the generic `:keyA` catch-all. No changes to `ShopProductCategory.vue`. CMS pipeline untouched.
- *Data:* V2 shop API (`shopCategoriesForKeysV2`). `grayCoverage` is a single string field (values: `"100%"`, `"Gray Blending"`, `"Knockout"`, `"No Gray"`, `"Superior"`). ALL 7 product types displayed as sections (not filtered).
- *CMS Data Tool:* Section headers (title, description, image) from `sr_shop_categories_config` (Data Object ID: 49). Fetched via `dataToolSvc.getData()` in `Promise.all` with V2 shop API. Falls back to API `product_type_display_name` when missing.
- *Product type sections:* 7 sections: color_kit, colorwonder, root_perfection, gray_escape, root_touch_up, gloss, hair_masks. Each with header (title + description + image), product grid, SEE MORE/LESS toggle (responsive: 10 mobile, 14 desktop).
- *Filter logic:* AND (`every`) — single filter exact match, 2+ filters → zero results → empty state + closest shades (OR via `some`, sorted by rarity). Filters apply within each section; empty sections hidden.
- *Sticky header:* CSS `position: sticky` with `top: var(--mr-navigation-height)` (BookingFlow pattern, not `useScroll`).
- *Breadcrumbs:* Existing `Breadcrumbs` component via Vuex `global/setBreadcrumbs`, v3 variant auto-selects when `RCC Site Revolution` experiment is B.

**Component tree:**
```
ShadeShopPage/
├── ShadeShopPage.vue       — Page: quiz banner, H1, sections (header + grid + toggle), empty state, skeleton
├── ShadeShopPage.test.js   — 52 tests
├── index.js
└── components/
    ├── FilterButtons.vue    — Reusable toggle filter buttons
    └── FilterButtons.test.js — 15 tests
```

**Key decisions:**
| Decision | Date | Rationale |
|---|---|---|
| Route-level swap, not experiment gate in component | 2026-04-10 | Avoids SSR flash — Vue Router resolves directly |
| `grayCoverage` is a string, not boolean flags | 2026-04-10 | Runtime API validation confirmed |
| AND → OR filter logic (`every` → `some`) | 2026-04-15 | Donna confirmed multi-filter should show products matching ANY selected tag |
| Closest Shades removed (`closestShades` computed + `.suggestions` template) | 2026-04-15 | Donna: "closest shade design may not be necessary since there should always be results available with OR logic" |
| Empty-state kept as safety net only (filter-match zero case) | 2026-04-15 | Rare but possible edge case (e.g., `['Superior']` matches no products). Template shows "Oops! No matches" for filter-zero; loading skeleton is `v-else` fallback |
| Product cards stay `<div role="link">`, NOT native `<a>` | 2026-04-15 | Level Access ADA scanner flags WCAG 2.5.3 "Label in Name" on native `<a>`. Dropped `aria-label` so inner text (name + description + coverage tags) is the accessible name |
| Quiz link: removed experimental `aria-label` | 2026-04-15 | Same WCAG 2.5.3 scan conflict. Inner text "take our quick quiz." is the accessible name. Sentence context satisfies 2.4.4 Link Purpose in Context (Level A) |
| Live region `.hiddenButPresent` lifted above `v-if` | 2026-04-15 | Stable DOM presence so AT observes mutations on first load. Wrapped template in `.shade-shop-page` root |
| `filtersEverUsed` flag pattern | 2026-04-15 | Silent on first mount; announces "Filters cleared. N shades available." after user clears all filters |
| `.card-description` contrast `text-color-3` → `text-color-2` | 2026-04-15 | 4.57:1 → 9.73:1. Moved from Stylus to `.text-color-2` utility class per sg-6 |
| `toggleSectionExpand($event)` returns focus to MrBtn on collapse | 2026-04-15 | Prevents focus drop to `<body>` when expanded cards unmount |
| `aria-controls` on See More + `:id` on `.product-grid` | 2026-04-15 | Completes `aria-expanded` pairing |
| H1 "SHADES" → "Shades"; H2 "OOPS! NO MATCHES" → "Oops! No matches" | 2026-04-15 | `.upper` CSS handles visual uppercasing; template uses natural case for SR cadence |
| Loading skeleton `role="status"` | 2026-04-15 | `aria-label` on plain `<div>` without role is not reliably announced; `role="status"` gives ARIA the accessible-name context |
| `import { vi } from 'vitest'` in test files | 2026-04-15 | PilkoLint flags implicit `vi` global on new diff lines; explicit import sidesteps false positive |
| CSS `position: sticky`, not `useScroll` composable | 2026-04-10 | Matches BookingFlowSiteRevolution pattern |
| Card markup inline, not separate component | 2026-04-10 | Not complex enough for extraction |
| `COVERAGE_LABELS` derived from `COVERAGE_OPTIONS` | 2026-04-10 | Single source of truth |
| No meta overrides in component | 2026-04-10 | Handled by Tophat/CMS |
| Breadcrumb v3 requires `RCC Site Revolution` experiment B | 2026-04-10 | Deployment dependency documented in roam node |
| All 7 product types as sections | 2026-04-10 | Business requirement — match previous design's product type grouping |
| CMS Data Tool `sr_shop_categories_config` for section headers | 2026-04-10 | Titles, descriptions, images managed in Tophat — not hardcoded |
| SEE MORE/LESS per section (10 mobile, 14 desktop) | 2026-04-10 | Responsive visible count prevents overwhelming grids |
| Section header `v-if` guard for all three fields | 2026-04-10 | Hides header entirely when title + description + image all missing |
| `flushPromises` in tests (not `$nextTick`) | 2026-04-10 | `Promise.all` in `created()` needs extra microtask ticks to resolve |

**Discarded ADA findings (replied with justification on PR #20512):**
| Finding | Reason |
|---|---|
| Filter-toggle drops focus to `<body>` (Andris L413) | Edge case after AND→OR filter decision. Sections rarely disappear from DOM under OR. Filter-button-focus-stays-put matches Amazon/Sephora/Ulta shop UX |
| Native `<a>` for product cards (Andris L33, WCAG 4.1.2) | WCAG 2.5.3 Level A scan violation overrides WCAG 4.1.2 recommendation. Kept `<div role="link">` |
| Quiz link `aria-label` (Andris L5, WCAG 2.4.9 AAA) | WCAG 2.5.3 Level A scan violation overrides 2.4.9 AAA recommendation. Removed aria-label |
| FilterButtons `aria-label` attr vs `:ariaLabel` prop (Andris L13) | False positive — Vue 3 normalizes kebab-case to declared camelCase prop |
| Route `props` configuration (Andris routes.js L22) | `/shop/brown` verified working end-to-end via existing CMS/SSR pipeline (same mechanism `ShopProductByCategoryV2` uses) |
| Sentry loading-skeleton indefinite (bot L80) | Not reachable in production — `/shop/brown\|blonde\|red\|black` always return products per Tophat configuration |

**CMS Data Tool dependency (CRITICAL):**
- `mixinKey: "sr_shop_categories_config"` (Data Object ID: 49)
- Must be created/published in each Tophat environment before QA
- 7 entries keyed by `productType` with `title`, `description`, `image` fields
- Images must be transparent PNGs (no background, no baked-in shadow — CSS adds `drop-shadow`)
- Full verification checklist in roam node under DEPLOYMENT NOTEs

**Files created:**
- `website/src/vuescripts/components/Shop/ShadeShopPage/ShadeShopPage.vue`
- `website/src/vuescripts/components/Shop/ShadeShopPage/ShadeShopPage.test.js`
- `website/src/vuescripts/components/Shop/ShadeShopPage/index.js`
- `website/src/vuescripts/components/Shop/ShadeShopPage/components/FilterButtons.vue`
- `website/src/vuescripts/components/Shop/ShadeShopPage/components/FilterButtons.test.js`
- `website/src/assets/svg-icons/close-thin.svg`

**Files modified:**
- `website/src/vuescripts/components/Shop/routes.js` — added 1 route
- `website/src/vuescripts/components/ColorBarLocationSectionV1/LocationCard.vue` — DOTCOMPB-7886 experiment redirect
- `website/src/vuescripts/components/ColorBarLocationSectionV1/LocationCard.test.js` — 2 new experiment tests
- `website/src/vuescripts/components/ColorBarMapSection/LocationsDirectory.vue` — DOTCOMPB-7886 experiment redirect

**DOTCOMPB-7886 (included in same branch):**
- `LocationCard.vue`: `detailsUrl` returns `/colorbar/booking/{code}/services` when `BookingFlowSiteRevolution` experiment B, else `/colorbar/locations/{code}`
- `LocationsDirectory.vue`: `getLocationUrl()` method + `isBookingFlowExperiment` computed for same conditional redirect

**Commits (on origin):**
1. `f42de1b` — `feat: Shade Shop Page Redesign + Location click redirect to services`
2. `5b4a28c` — `feat: Product type sections with CMS Data Tool integration`
3. `3ce08fb` — `fix: Address PR review — extract dedup method, cache imagery lookup, fix naming`
4. `497d872` — `fix: Superior Gray Coverage Length Solution`
5. `41c4a25` — `fix: Address Andris ADA review — focus management, live region, contrast, scan-safe ARIA` (2026-04-15, latest)

**PR:** #20512 — `[DOTCOMPB-7466]: Shade Shop Page Redesign + Location Click Redirect`. Labels: `DOTCOM TEAM`, `Pending QA Review`, `Special Deploy Requirements`. All CI checks green (CircleCI, Codecov, security); PilkoLint currently failing on `vi is not defined` — fix staged locally (explicit `import { vi } from 'vitest'`) pending user commit + push.

**Andris review responses (2026-04-14):**
- L305 dedup pattern (ad-23): Fixed — extracted `addUniqueProducts()` method
- L342 imagery lookup (ad-24): Fixed — cached `product?.imagery` into `const imagery`

**Andris ADA review batch (2026-04-15, 14 inline + 4 review summaries — all replied):**

*Implemented (11):*
- L38 focus-visible on cards → `.focusable` utility class
- L17 live region inside `v-if` → lifted above `v-if` via root `.shade-shop-page` wrapper
- L36 `aria-label` suppresses inner content → dropped `aria-label`; inner text is accessible name
- L222 clear-last-filter silence → `filtersEverUsed` flag + "Filters cleared. N shades available." announcement
- L564 `text-color-3` contrast → `text-color-2` (9.73:1) via `.text-color-2` utility class
- L432 SEE LESS focus loss → `toggleSectionExpand($event)` returns focus to MrBtn on collapse
- L52 missing `aria-controls` → `:id` on `.product-grid` + `:aria-controls` on See More
- L8 H1 "SHADES" hardcoded → "Shades" (natural case + `.upper` CSS)
- L60 H2 "OOPS! NO MATCHES" hardcoded → "Oops! No matches"
- L84 loading skeleton `aria-label` without role → added `role="status"`
- LocationsDirectory L127 `isFrontEndEvent: true` removal → confirmed

*Discarded (3, each with concise justification reply):*
- L33/L66 native `<a>` for cards (WCAG 4.1.2) — WCAG 2.5.3 scan violation overrides; kept `<div role="link">` with inner text as accessible name
- L13 FilterButtons `aria-label` prop — Vue 3 normalizes kebab-case to declared prop (not fallthrough)
- L413 filter-toggle focus loss — edge case after Donna's OR-logic decision; filter-button-focus-stays-put matches Amazon/Sephora/Ulta shop UX
- Quiz link `aria-label` (L5) — same WCAG 2.5.3 conflict as cards; removed aria-label, sentence context satisfies 2.4.4

**Bot review responses:**
- Sentry "loading skeleton indefinite" (L80): Not reachable — Tophat always configures products for brown/blonde/red/black color families. Reply posted.
- PilkoLint `vi is not defined` (9 flags across 2 test files): Fix staged locally; pending user commit (import vi from vitest).

**Re-validation:** Every change re-validated with Level Access browser extension — clean.

**Tests (87 total on PR surface):** 58 ShadeShopPage (+9 new — OR logic, native-anchor verification discard, grid id, filtersEverUsed, section toggle focus, Shop All breadcrumb `/shop-all` regression-lock) + 15 FilterButtons + 14 LocationCard (includes experiment redirect + breadcrumb assertions). Full website suite unchanged at 5373 passing, no regressions.

**PR body rewrite (2026-04-15, session-reset artifact):**
- Source: rewritten via pr-scribe in the `* PR BODY` `#+begin_src markdown` block of `~/.brain.d/roam-nodes/madison_reed/2026-04-08-120100-dotcompb_7466.org`
- Format: Hybrid MR top-level skeleton + Kyonax internal formatting (see §2.3 decision #87)
- Checklist: 6 verbatim MR items; all 5 boxes checked except `requires a lambda deployment` and `contains db migrations`
- Changes: Pattern B with `### Implementation` subsection only (no `### Release` / `### CI & Tooling` / `### Dependencies` / `### Docs` since this PR doesn't touch those), mandatory tag legend blockquote, themed group labels (Shade Shop Page, Routing, Location Card, Location Directory, Assets), `[NEW]` / `[MOD]` tags (7 NEW + 3 MOD)
- Technical Details: TD-4FIELD with 9 decisions × 4 sub-fields (Chose / Over / Why / Trade-off)
- Tracking Events: separate subsection listing 5 Segment events
- Testing Coverage: TEST-TWO-TABLE with `**Test runner:**` / `**Command:**` metadata + `#### Automated tests` (3-row table) + `#### Quality gates` (6-row table)
- QA heading: MR wording (`## Instructions on how QA can test this PR`) + Kyonax step format (ASCII flow tree, `> **Prereqs:**` per feature group, `***Expected:***` bold-italic at 6-space indent, 2 feature groups = 20 steps total)
- Special Deployment: DEPLOY-SEVERITY numbered list — 2 CRITICAL (CMS Data Tool object + image format), 2 REQUIRED (RCC Site Revolution + BookingFlowSiteRevolution experiments), 1 OPTIONAL (Tophat route collision check)
- Documentation: DOC-MEDIA-VOCAB with 4 media headings (VIDEO demo + DESKTOP/MOBILE/SCREENSHOT placeholders)
- Reference footer: `[DOTCOMPB-7466]` + `[DOTCOMPB-7886]` both with `atlOrigin` preserved
- Global-rule sweep: 0 emojis outside `✅` status cells, 0 arrow glyphs (including `=>`), 0 gitignored file refs, all absolute URLs, checkboxes only in top Checklist

#### Follow-on fix A: Sticky-header-wrap ResizeObserver rewrite (2026-04-15, uncommitted)

**Reported:** User QA on `dotcom-feat.mdsnrd.com/shop/brown` showing sticky header misaligned. Compared against `dotcom.mdsnrd.com/shop/brown` (working) and `localhost:3000/shop/brown`. Root cause traced to `--mr-navigation-height` only measuring `.mr-navigation`, not the full `.sticky-header-wrap` (which contains `SiteMessageBanner` + `SiteMessageBannerCarousel` + `MrNavigation` when experiment B is active). See §2.3 decisions #93–#96.

**Broken scenarios before the fix:**
- Experiment A (control): nav is NOT sticky → `.sticky-header-wrap.is-sticky` absent → but `--mr-navigation-height` still set by `ResizeObserver` → sticky header offset by a phantom nav height
- Experiment B + visible banner: `.sticky-header-wrap.is-sticky` active → sticky header offset by ONLY nav height → overlaps banner + carousel
- Mobile vs desktop: banner skeleton heights differ (`5.5em` / `4em` / `3.6em` per breakpoint) → single CSS var never matches

**Fix:**
- `ShadeShopPage.vue` template: `.sticky-header(:style="stickyHeaderStyle")` replaces the static `top var(--mr-navigation-height, 0px)` in scoped Stylus.
- `data()`: new `stickyHeaderOffset: 0`, `stickyHeaderObserver: null`.
- `computed`: new `stickyHeaderStyle()` returning `{ top: `${this.stickyHeaderOffset}px` }`.
- `mounted()` adds `$nextTick(() => this.setupStickyHeaderOffset())`; `beforeUnmount()` calls `teardownStickyHeaderOffset()`.
- `setupStickyHeaderOffset()`: SSR-guarded (`typeof window === 'undefined' || typeof ResizeObserver === 'undefined'` short-circuits). Queries `document.querySelector('.sticky-header-wrap.is-sticky')`. If absent (experiment A or no sticky scaffold), offset stays `0`. If found, reads `getBoundingClientRect().height` and attaches a `ResizeObserver` to keep the offset live (covers banner dismiss, carousel slide, viewport resize).
- `teardownStickyHeaderOffset()`: disconnects the observer, nulls the reference.
- Scoped Stylus: removed stale `top var(--mr-navigation-height, 0px)` line from `.sticky-header`.

**Key pattern decisions:**

| Decision | Date | Rationale |
|---|---|---|
| `document.querySelector('.sticky-header-wrap.is-sticky')` (not experiment name) | 2026-04-15 | Decouples from experiment name; if SsrApp's gating logic changes, consumer still works |
| `ResizeObserver` on the wrap (not polling / `useScroll`) | 2026-04-15 | Border-box changes are caught live; banner dismiss / carousel slide / breakpoint change all trigger correct re-measure |
| `:style` binding (not CSS variable on `<html>`) | 2026-04-15 | Scoped to this component; avoids cross-cutting change to `MrNavigation.vue`; verified `:style` is allowed and idiomatic (69 precedents, no ESLint rule) — §2.3 #92 |
| SSR-guarded setup in `mounted` via `$nextTick` | 2026-04-15 | Ensures `globalMixins.mounted` has already populated `this.experiments`; double-guard via `typeof ResizeObserver === 'undefined'` covers older environments |

**Tests added (+4 in new `describe('sticky header offset')` block in `ShadeShopPage.test.js`):**
- defaults `stickyHeaderOffset` to `0` and `stickyHeaderStyle.top` to `'0px'`
- keeps offset at `0` when `.sticky-header-wrap.is-sticky` is absent (experiment A scenario)
- reads wrap height and observes when `.sticky-header-wrap.is-sticky` is present (experiment B + banners — mock `getBoundingClientRect()` returns `172`, assert `stickyHeaderOffset === 172`, assert `observe` called with the wrap element)
- disconnects the observer on `beforeUnmount`

**Tests: 62 `ShadeShopPage.test.js` passing locally (was 58 → +4 new).**

#### Follow-on fix B: View Details redirect fix (2026-04-15, uncommitted)

**Reported:** User QA flagged two inconsistencies on `/colorbar/locations`:
1. Right-click → "Open in new tab" on "View Details" opens `/colorbar/locations/{code}` (details)
2. Left-click on "View Details" lands on `/colorbar/booking/{code}/services` (services)

**Root cause:** `LocationCardBody.vue` child component has its own local `detailsUrl` computed hardcoded to `/colorbar/locations/{code}`, but wires `@click.prevent="$emit('details-click')"` which bubbles up to `LocationCard.onCardClick` — and THAT handler uses the parent's experiment-gated `detailsUrl`. So `href` and click destination diverge under experiment B. Already filed as **DOTCOMPB-7958** and **DOTCOMPB-7959** (linked to 7886 via "created by" on the JIRA ticket).

**Fix (scoped to `LocationCard.vue` — no change needed to `LocationCardBody.vue`):**
- `computed`: added `locationDetailsUrl()` returning `/colorbar/locations/${this.location.code}` (NEVER experiment-gated). `detailsUrl` refactored to reuse this new computed in the non-B branch (functionally identical, cleaner).
- `methods`: added `onViewDetailsClick()` firing `trackMREventAndRedirect('Locations page – Location card clicked', this.locationDetailsUrl, this.locationProperties)` — same tracking event as card click but hardcoded destination.
- Template: `LocationCardBody(... @details-click="onViewDetailsClick")` replaces the prior `@details-click="onCardClick"`. Card image + name anchors still use the experiment-gated `detailsUrl` → DOTCOMPB-7886 AC1 still satisfied (§2.3 #99).

**Interaction matrix (now consistent across all input modes):**

| Interaction on "View Details" | Destination |
|---|---|
| Left-click (with tracking) | `/colorbar/locations/{code}` |
| Keyboard Enter | `/colorbar/locations/{code}` |
| Right-click → Open in new tab | `/colorbar/locations/{code}` |
| Middle-click / Cmd-click | `/colorbar/locations/{code}` |
| Copy link address | `/colorbar/locations/{code}` |

**Tests added (+2 in `LocationCard.test.js`):**
- `computes correct locationDetailsUrl (never experiment-gated)` — asserts `locationDetailsUrl === '/colorbar/locations/woodlands'` even with `experiments: { 'BookingFlowSiteRevolution': 'B' }`
- `onViewDetailsClick always redirects to location details page (not booking), even when experiment is B` — asserts `trackMREventAndRedirect` called with `/colorbar/locations/woodlands` under experiment B

**Tests: 16 `LocationCard.test.js` passing locally (was 14 → +2 new).**

**Defects resolved by this fix:** DOTCOMPB-7958 (href vs click divergence), DOTCOMPB-7959 (no path to location details under experiment B). DOTCOMPB-7960 and DOTCOMPB-7961 remain open — separate concerns (SSR-side route handling + SSR hydration mismatch).

**AC1 of DOTCOMPB-7886 verification:** Location NAME click still routes through the experiment-gated `detailsUrl` → services page (B) or details page (A). AC1 is scoped to NAME only (§2.3 #99); View Details is out of AC scope. AC1 remains satisfied.

#### PR-surface tests summary (post follow-ons, pre-commit)

- `ShadeShopPage.test.js`: 62 passing (was 57 before session, now +5: 1 `/shop-all` breadcrumb regression-lock from DOTCOMPB-7944, +4 sticky-header-offset regression-locks)
- `FilterButtons.test.js`: 15 passing (unchanged)
- `LocationCard.test.js`: 16 passing (was 14, +2 View Details regression-locks)
- **Total PR surface: 93 passing**, 0 regressions. Full website Vitest suite unchanged at baseline.

#### Commit message plan (drafted text only — NOT executed, per `feedback_no_git.md`)

Two separable commits OR one bundled commit — user preference. Drafts available in the `ShadeShopPage.test.js` / `LocationCard.test.js` / `ShadeShopPage.vue` / `LocationCard.vue` working-tree edits.

---

### 3.16 DOTCOMPB-7944: Shop All Products Breadcrumb Wrong URL (Bug)

**Created:** 2026-04-15 | **Last updated:** 2026-04-15
**Branch:** `DOTCOMPB-7466` (piggybacked — NOT a new branch, per user instruction)
**Status:** **IN PROGRESS** (JIRA: En curso). Fix implemented locally, uncommitted in working tree. Commit message drafted and ready. Will ship with PR #20512.
**Roam node:** `~/.brain.d/roam-nodes/madison_reed/2026-04-15-220000-dotcompb_7944.org` (UUID `1d126143-ebe7-4cb3-bd00-1736b196eaf3`)
**Parent ticket:** [DOTCOMPB-7466](https://madison-reed.atlassian.net/browse/DOTCOMPB-7466) — Shade Shop Page Redesign (introduced the hardcoded breadcrumb)

**Bug summary:**
The Shop All Products breadcrumb on every Shade Shop PLP (`/shop/brown`, `/shop/blonde`, `/shop/red`, `/shop/black`) points to `/shop/all-hair-color` (the filtered V2 hair-color category view served by `ShopProductByCategoryV2`) instead of the intended site-wide Shop All landing page at `/shop-all`. Carley Keran confirmed the target destination is `/shop-all`.

**Root cause:**
Hardcoded URL in `website/src/vuescripts/components/Shop/ShadeShopPage/ShadeShopPage.vue:377` inside `setBreadcrumbFragments()`:

```javascript
// BEFORE
{ title: 'Shop All Products', url: '/shop/all-hair-color' },  // WRONG

// AFTER
{ title: 'Shop All Products', url: '/shop-all' },
```

Single point of failure — grep across `website/src` confirmed this was the **only** place in the codebase pairing the text "Shop All Products" with `/shop/all-hair-color`. Every other Shop All link (SiteNav, PDP breadcrumb, Cart empty state, Dashboard cards) correctly uses `/shop-all`. The Express route `/shop-all` is already registered in `website/src/routing/views.js:850`.

**Reference patterns audited (all use `/shop-all`):**

| File | Line | Pattern |
|---|---|---|
| `SiteNav/constants.js` | 333 | `{ title: 'Shop All Products', url: '/shop-all' }` |
| `PdpEntry.vue` | 314 | `fragments.push({ title: 'Shop All', url: '/shop-all' });` |
| `PdpEntrySsr.vue` | 302 | `fragments.push({ title: 'Shop All', url: '/shop-all' });` |
| `CartV2.vue` | 7 | `native-href="/shop-all"` |
| `EmptyCart.vue` | 7 | `a(href="/shop-all") See all products` |
| `LocationsEmptyState.vue` | 31 | `trackMREventAndRedirect('Locations page - Shop Now clicked', '/shop-all')` |
| `views.js` | 850 | `app.get('/shop-all', (req, res) => {...})` (Express route) |

**Implementation options considered:**

| Option | Scope | Decision |
|---|---|---|
| 1. One-line URL change + test assertion | 1 file, ~20 lines | **Chose this** — minimal, aligned with existing codebase pattern |
| 2. Extract `SHOP_ALL_URL` module-level const | Same file | Rejected — used only once, creates inconsistency with other call sites |
| 3. Centralize nav URLs in `@constants/navigation.js` | 10+ files | Rejected — cross-cutting refactor not appropriate for single-URL bug fix; candidate for separate tech-debt ticket |

**Files changed (2):**

- **`website/src/vuescripts/components/Shop/ShadeShopPage/ShadeShopPage.vue`** — Line 377: `/shop/all-hair-color` replaced with `/shop-all`
- **`website/src/vuescripts/components/Shop/ShadeShopPage/ShadeShopPage.test.js`** — Added 1 new test in the `describe('breadcrumbs')` block asserting the middle fragment's URL is `/shop-all` (regression lock). Uses `vi.spyOn(store, 'commit')` + `commitSpy.mock.calls.find(...)` pattern because Vuex 4 invokes the bound commit with 3 positional args (name, payload, options) — see §2.3 decision #90.

**Test added:**
```javascript
it('sets the Shop All Products breadcrumb url to /shop-all', async () => {
  const store = createMockStore();
  const commitSpy = vi.spyOn(store, 'commit');
  createWrapper({ keys: ['brown'] }, store);
  await flushPromises();
  const setBreadcrumbsCall = commitSpy.mock.calls.find(call => call[0] === 'global/setBreadcrumbs');
  expect(setBreadcrumbsCall).toBeDefined();
  expect(setBreadcrumbsCall[1]).toEqual(
    expect.arrayContaining([
      expect.objectContaining({ title: 'Shop All Products', url: '/shop-all' }),
    ]),
  );
});
```

**Tests result:** 73 passing on local run (58 ShadeShopPage including new breadcrumb test + 15 FilterButtons). LocationCard untouched (still 14 tests — not re-run this session but unaffected). Total PR surface: 87 tests passing.

**Key decisions:**

| Decision | Date | Rationale |
|---|---|---|
| Fix on `DOTCOMPB-7466` branch, not a new branch | 2026-04-15 | User instruction — bug is a direct regression from the open PR; ship both together to QA |
| Option 1 (one-line URL change) | 2026-04-15 | Aligned with every other call site; no cross-file refactor needed |
| Add regression-lock test | 2026-04-15 | Existing `describe('breadcrumbs')` block only asserted `setBreadcrumbs` was called, not the URL — future change could silently reintroduce the bug |
| `commitSpy.mock.calls.find(...)` over `toHaveBeenCalledWith(...)` | 2026-04-15 | Vuex 4 invokes `commit` with `(name, payload, options)` where `options` is typically undefined. `toHaveBeenCalledWith('name', expected)` fails on positional-count mismatch. Find + `toEqual` on `call[1]` is cleaner. See §2.3 decision #90 |

**Commit message (drafted, awaiting user to run `git commit`):**
```
fix: Shop All Products breadcrumb links to /shop-all on Shade Shop PLP

DOTCOMPB-7944: The Shop All Products breadcrumb on every Shade Shop PLP
(/shop/brown, /shop/blonde, /shop/red, /shop/black) pointed to the
filtered V2 hair-color category view (/shop/all-hair-color) instead of
the site-wide Shop All landing page (/shop-all). Confirmed destination
is /shop-all per Carley Keran — consistent with SiteNav, PDP, Cart, and
Dashboard which all use /shop-all.

Root cause: hardcoded URL on line 377 of ShadeShopPage.setBreadcrumbFragments.
Single point of failure — grep across website/src confirmed no other call
site pairs "Shop All Products" with /shop/all-hair-color.

Fix: 1-line URL change in ShadeShopPage.vue + a regression-lock test in
ShadeShopPage.test.js that asserts the middle breadcrumb fragment's url
is /shop-all. Tests: 73 passing, 0 regressions.

Modified-by: Cristian D. Moreno <kyonax.corp@gmail.com>
```

**Index entries:**
- BACKLOG: `<<bug-7944>>` anchor with org-roam link to node UUID `1d126143-...`
- Sprint Board `IN PROGRESS`: nested under `<<ticket-7466>>` (same lane, parent-child)
- BACKLOG statistics cookie: `[69%] [23/33]` bumped to `[68%] [23/34]`

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
| `website/src/routing/views.js` | 7712 (Express `:urlKey/:path?` generic optional param, L1593) |
| `website/src/vuescripts/store/modules/colorbar.js` | 7290 (getLocationReviews try/catch) |
| `website/src/vuescripts/components/SiteNav/SiteNavDesktopV2/SiteNavDesktopV2.vue` | 7463, 7749 (font size bump) |
| `website/src/vuescripts/components/SiteNav/SiteNavMobileV2/SiteNavMobileV2MainNav/SiteNavMobileV2MainNav.vue` | 7463 |
| `website/src/vuescripts/components/SiteNav/SiteNavMobileV2/SiteNavMobileV2.vue` | 7463 |
| `website/src/vuescripts/components/SiteNav/SiteNav.vue` | 7463 |
| `website/src/vuescripts/store/modules/siteNav.js` | 7463 (mixinKey → sr-top-nav, res.data null guard) |
| `website/src/vuescripts/components/SierraWidget/SierraWidget.vue` | 7652 |
| `website/src/vuescripts/components/PDP/DashHudsonScriptInner.vue` | 7527 (7 new props, `:deep()` overrides, event fix, ADA, CTA removal) |
| `website/src/vuescripts/components/PDP/DashHudsonScript.vue` | 7527 (pass-through for 7 new props) |
| `website/src/vuescripts/components/PDP/ColorKitPdpV2/ColorKitPdpV2.vue` | 7527 (carousel config + title) |
| `website/src/vuescripts/components/PDP/ColorKitPdpV3/ColorKitPdpV3.vue` | 7527 (carousel config + title) |
| `website/src/vuescripts/components/PDP/PdpBottom/__snapshots__/PdpBottom.test.js.snap` | 7527 (snapshot updated) |

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
| `~/.brain.d/roam-nodes/madison_reed/2026-04-06-dashhudson_research.org` | DashHudson / Dash Social platform research (8-part doc, CDN inventory, widget internals, API reference, implementation plans) |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-06-160000-dotcompb_7527.org` | DOTCOMPB-7527 (Dash Hudson UGC carousel overrides) |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-08-120000-dotcompb_7886.org` | DOTCOMPB-7886 |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-08-120100-dotcompb_7466.org` | DOTCOMPB-7466 (31 ACs, 10 mockups, data pipeline investigation, CMS architecture, experiment design, 8-phase plan, rewritten `* PR BODY` block 2026-04-15 via pr-scribe Hybrid format) |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-15-220000-dotcompb_7944.org` | DOTCOMPB-7944 (BUG: Shop All Products breadcrumb wrong URL — root cause + 3 implementation options + recommendation + test plan + parent cross-ref to ticket-7466) |
| `~/.brain.d/roam-nodes/madison_reed/assets/mobile_shop:brown*.jpg` (5 files) | DOTCOMPB-7466 Figma mockups (mobile) |
| `~/.brain.d/roam-nodes/madison_reed/assets/desktop_shop*.png` (5 files) | DOTCOMPB-7466 Figma mockups (desktop) |
| `website/src/vuescripts/components/Shop/ShadeShopPage/ShadeShopPage.vue` | DOTCOMPB-7466 (page) + DOTCOMPB-7944 (`/shop-all` fix) + sticky-header-wrap ResizeObserver rewrite (2026-04-15 follow-on: `:style="stickyHeaderStyle"` binding, `setupStickyHeaderOffset` / `teardownStickyHeaderOffset` methods, removed stale `top var(--mr-navigation-height)` Stylus) |
| `website/src/vuescripts/components/Shop/ShadeShopPage/ShadeShopPage.test.js` | DOTCOMPB-7466 (57 tests) + DOTCOMPB-7944 (+1 breadcrumb regression-lock) + sticky-header follow-on (+4 regression-locks) — 62 total |
| `website/src/vuescripts/components/Shop/ShadeShopPage/index.js` | DOTCOMPB-7466 (barrel export) |
| `website/src/vuescripts/components/Shop/ShadeShopPage/components/FilterButtons.vue` | DOTCOMPB-7466 |
| `website/src/vuescripts/components/Shop/ShadeShopPage/components/FilterButtons.test.js` | DOTCOMPB-7466 (15 tests) |
| `website/src/assets/svg-icons/close-thin.svg` | DOTCOMPB-7466 (dismiss icon for filter buttons) |
| `website/src/vuescripts/components/Shop/routes.js` | DOTCOMPB-7466 (added shade shop route) |
| `website/src/vuescripts/components/ColorBarLocationSectionV1/LocationCard.vue` | DOTCOMPB-7886 (experiment redirect) + View Details follow-on fix (2026-04-15: `locationDetailsUrl` computed, `onViewDetailsClick` handler, template rewires `@details-click`) |
| `website/src/vuescripts/components/ColorBarLocationSectionV1/LocationCard.test.js` | DOTCOMPB-7886 (14 tests) + View Details follow-on (+2 regression-locks) — 16 total |
| `website/src/vuescripts/components/ColorBarLocationSectionV1/LocationCardBody.vue` | Pre-existing. Unchanged by session work — its local `detailsUrl` computed intentionally stays hardcoded to `/colorbar/locations/{code}` and is the source of truth for the "View Details" `href` |
| `website/src/vuescripts/components/ColorBarMapSection/LocationsDirectory.vue` | DOTCOMPB-7886 (experiment redirect + `isFrontEndEvent` removal) |
| `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` | Sprint Board Index — `<<bug-7944>>` added 2026-04-15 (nested under `<<ticket-7466>>` in IN PROGRESS, BACKLOG cookie `[68%] [23/34]`) |

### Session & Directives

| File | Purpose |
|---|---|
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/site-revolution-redesign.md` | This session file |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/ticket-pr-template.md` | PR template directive |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.** This section captures the most recent work and immediate next steps.

### What was done last (2026-04-23) — DOTCOMPB-7712 Andris review round 4: CMS investigation + Express `:path?` fix

*   **Andris code review round 4 (PR #20423, 2026-04-22)** — 1 new `CHANGES_REQUESTED`: "We should not need these routes anymore, especially the photos one." Wanted Express `/photos` route removed entirely.
*   **Deep CMS `parseUrl` investigation** — Traced full request flow: Express → CMS `parseUrl` → `loadPage`. Found `req.url` is coupled between CMS resolution (`htmlRenderer.js:536`) and Vue Router SSR (`server.js:22`) — URL rewriting causes hydration mismatch. Vue Router children are client-side only, don't affect server-side URL resolution.
*   **Tophat optional parameter discovery** — Queried MongoDB directly: CMS content `_id: 2350` (`/colorbar/locations/`) already has `urlParameterList` with `locationCode` (required) + `photos` (optional). `parseUrl` accepts 2-segment URLs → CMS resolves to same page → Vue Router SSR navigates to `/photos` child. Verified: `curl` returns 200 with `hcb-photos-page`, `gallery-grid`, `photo-item` rendered server-side.
*   **404 validation gap found** — Invalid location codes on `/photos` returned 200 instead of 404 (CMS served page, `serverPrefetch` failed silently). Fix: `/colorbar/locations/:urlKey/:path?` generic optional param on existing Express validation handler.
*   **PR comment posted** — Reply `r3126988258` to andris310's review comment explaining findings + fix.
*   **Commit message drafted** — `fix: replace hardcoded /photos Express route with generic optional :path param`
*   **85/85 tests passing** across 5 files. No test changes needed.
*   **`views.js` change needs to be applied** — Change line 1593 from `app.get('/colorbar/locations/:urlKey', ...)` to `app.get('/colorbar/locations/:urlKey/:path?', ...)`.

### Pending

*   **DOTCOMPB-7712 — apply `:path?` change + commit + push** — Edit `views.js:1593` to add `/:path?`, then commit with message: `fix: replace hardcoded /photos Express route with generic optional :path param`. Then push. Then await andris310 re-review.
*   **Uncommitted local edits on branch `DOTCOMPB-7466`** — three logical changes still ready to commit:
    1. DOTCOMPB-7944: `ShadeShopPage.vue` breadcrumb URL fix + regression-lock test.
    2. Sticky-header-wrap fix: `ShadeShopPage.vue` ResizeObserver rewrite; +4 regression-locks.
    3. View Details fix: `LocationCard.vue` redirect handler; +2 regression-locks.
*   **DOTCOMPB-7466 — paste refined PR description into GitHub PR #20512** — Source: roam node `* PR BODY`. Only on explicit approval.
*   **DOTCOMPB-7960 and DOTCOMPB-7961** — Separate open tickets. Not addressed.
*   **DOTCOMPB-7903** — Changes staged, not committed. PR #20481.
*   **DOTCOMPB-7742** — In Test (JIRA: Pruebas). PR #20368.
*   **DOTCOMPB-7717 cleanup** — MarketingBanner dead workaround. Plan in §3.8.
*   **DashHudson Integration** — Research complete. Awaiting PM input.

### Where to resume

If user wants to **apply + commit + push DOTCOMPB-7712**: Edit `views.js:1593` — change `'/colorbar/locations/:urlKey'` to `'/colorbar/locations/:urlKey/:path?'`. Commit: `fix: replace hardcoded /photos Express route with generic optional :path param`. Push. NEVER run git commands unless user explicitly says so.
If user wants to **commit DOTCOMPB-7466 fixes**: See §3.15 and §3.16. Three commits or one bundled.
If user wants to **update PR #20512 body**: Copy from roam node `** PR BODY`. Only on explicit approval.
If user asks for a **new task**: check §2.5 Pending Work.

<!-- DESCRIPTION AND USER CONTEXT END -->

<!-- ==========================================================================
     EXTERNAL APPENDIX TOMBSTONES
     The following sections were absorbed into the context block on 2026-04-23.
     Recovery pointers below point to the authoritative sources.

     - HYDRATION FIX APPROACHES (2026-04-03): Decision captured in §2.3 #64 (CSS-only masonry).
     - EDGE CASE ANALYSIS (2026-04-03, 27 items): All blocking issues resolved. Migration complete per §3.11.
     - NAMING CONVENTION REVIEW (2026-04-03, 50 items): All correct. 0 blocking items.
     - DASH HUDSON DEEP RESEARCH (2026-04-05): Full research in roam node
       ~/.brain.d/roam-nodes/madison_reed/2026-04-06-dashhudson_research.org
       Key facts: brand_id=18947, Gallery API exists (GET /brands/{id}/galleries/{gid}/media),
       DashHudsonWidget already supports per-gallery via galleryId prop,
       Options A0-A3 (widget, no backend) and B (API, full backend).
       Recommendation: Start with A0, upgrade to A2/A3 for per-location, then B if needed.
       8 open questions for PM. Status: RESEARCH COMPLETE, awaiting PM input.
     ========================================================================== -->



<!-- INIT OF THE USER PROMPT END -->

<!-- Local Variables: -->
<!-- gptel-model: gemini-pro-paid -->
<!-- gptel--backend-name: "Gemini Local" -->
<!-- gptel--bounds: ((response (40 13827))) -->
<!-- End: -->
