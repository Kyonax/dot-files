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
| `DOTCOMPB-7712` | Story | New page to display location photos                          | **IN CODE REVIEW** (2026-04-08). PR #20423. Branch `DOTCOMPB-7712`. 81 tests, 3 code review rounds. See §3.11. |
| `DOTCOMPB-7527` | Story | Dash Hudson Module Updates — UGC carousel style overrides   | **MERGED** (PR #20424, 2026-04-06). CSS `:deep()` overrides, configurable SDK props, event tracking fix, ADA. 118 tests. See §3.13. |
| `DOTCOMPB-7768` | Bug   | Mobile nav dropdowns not scrollable with subcopy text        | **MERGED** (2026-04-08, JIRA: Finalizada). PR #20335.                  |
| `DOTCOMPB-7903` | Bug   | Fix Shop All link not tappable on mobile devices             | **IN PROGRESS** (2026-04-08). `100vh` → `100dvh` fix + removed no-op `env(safe-area-inset-bottom)`. PR #20481 OPEN. See §3.14. |
| `DOTCOMPB-7886` | Story | Go to services page when clicking location                   | **IMPLEMENTED** (2026-04-10). Included in DOTCOMPB-7466 branch. LocationCard + LocationsDirectory redirect when `BookingFlowSiteRevolution` experiment B. 2 tests. See §3.15. |
| `DOTCOMPB-7466` | Story | Shade Shop Page Redesign                                     | **IN CODE REVIEW** (2026-04-14). PR #20512 OPEN. Andris reviewing. 67 tests. OR filter logic pending (Donna confirmed, Closest Shades blocked). See §3.15. |
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

### 2.4 PR Review Resolutions (DOTCOMPB-7289)

8 comments addressed: magic numbers → named constants; repeated boolean logic → `isSelected(slideImage)` method; oddly specific em values → reverted to `px`; inline event logic → dedicated methods; placeholder button → `// TODO:` comment; resize listener → `window.matchMedia` API; `@hook:mounted` → custom `@ready` event (later deprecated); `getObjProperty` → optional chaining (`?.`).

### 2.5 Pending Work

*   **DOTCOMPB-7903** — PR #20481 OPEN (2026-04-08). `100dvh` fix for mobile nav viewport. Branch `DOTCOMPB-7903`. Changes staged, not yet committed.
*   **DOTCOMPB-7712** — PR #20423 OPEN. In Code Review (JIRA). Branch `DOTCOMPB-7712`. 81 tests, 3 code review rounds.
*   **DOTCOMPB-7742** — PR #20368. In Test (JIRA: Pruebas). Cookie-based service pre-selection.
*   **DOTCOMPB-7886** — Implemented (2026-04-10). Included in DOTCOMPB-7466 PR #20512. LocationCard + LocationsDirectory experiment redirect. 2 tests.
*   **DOTCOMPB-7466** — PR #20512 OPEN (2026-04-14). Andris reviewing. 2 comments addressed (ad-23 dedup method, ad-24 cache imagery). OR filter logic: code change ready, Closest Shades blocked on Donna. Commit 3 staged (Andris fixes + BEM rename + emits order). See §3.15.
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

**Tests:** 81 passing across 5 test files. All components have coverage.

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

**Created:** 2026-04-08 | **Last updated:** 2026-04-10
**Branch:** `DOTCOMPB-7466`
**Status:** PR #20512 OPEN. Under code review by Andris. 67 tests passing. OR filter logic pending (Donna confirmed, Closest Shades blocked).
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

**Key decisions (2026-04-10):**
| Decision | Date | Rationale |
|---|---|---|
| Route-level swap, not experiment gate in component | 2026-04-10 | Avoids SSR flash — Vue Router resolves directly |
| `grayCoverage` is a string, not boolean flags | 2026-04-10 | Runtime API validation confirmed |
| ~~AND~~ → OR filter logic | 2026-04-14 | Donna confirmed: multi-filter should show products matching ANY selected tag. AND always produced zero results (single string field). Closest Shades behavior pending Donna's clarification. |
| CSS `position: sticky`, not `useScroll` composable | 2026-04-10 | Matches BookingFlowSiteRevolution pattern |
| Card markup inline, not separate component | 2026-04-10 | Not complex enough for extraction |
| `COVERAGE_LABELS` derived from `COVERAGE_OPTIONS` | 2026-04-10 | Single source of truth |
| No meta overrides in component | 2026-04-10 | Handled by Tophat/CMS |
| Breadcrumb v3 requires `RCC Site Revolution` experiment B | 2026-04-10 | Deployment dependency documented in roam node |
| All 7 product types as sections, not just color_kit + colorwonder | 2026-04-10 | Business requirement — match previous design's product type grouping |
| CMS Data Tool `sr_shop_categories_config` for section headers | 2026-04-10 | Titles, descriptions, images managed in Tophat — not hardcoded |
| SEE MORE/LESS per section (10 mobile, 14 desktop) | 2026-04-10 | Responsive visible count prevents overwhelming grids |
| Section header `v-if` guard for all three fields | 2026-04-10 | Hides header entirely when title + description + image all missing |
| `flushPromises` in tests (not `$nextTick`) | 2026-04-10 | `Promise.all` in `created()` needs extra microtask ticks to resolve |

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

**Commits:**
1. `f42de1b` — `feat: Shade Shop Page Redesign + Location click redirect to services` (pushed)
2. Pushed — `feat: Product type sections with CMS Data Tool integration`
3. Staged — `fix: Address PR review — extract dedup method, cache imagery lookup, fix naming` (Andris fixes + BEM rename + emits order)

**PR:** #20512 — `[DOTCOMPB-7466]: Shade Shop Page Redesign + Location Click Redirect`. Labels: `DOTCOM TEAM`, `Pending Code Review`.

**Andris review comments (2026-04-14, PR #20512):**
- *L305 — Repetitive dedup pattern:* Fixed — extracted `addUniqueProducts(source, target, seen)` method (ad-23)
- *L342 — Repeated imagery lookup:* Fixed — cached `product?.imagery` into `const imagery` (ad-24)

**AI review bot responses (3 total, all dismissed):**
- *Missing `props` on route (sentry[bot]):* Not valid — `keys` prop populated by CMS/SSR pipeline, not Vue Router `props: true`
- *AND filter logic (sentry[bot]):* Directionally correct — **(2026-04-14) Donna confirmed** filters should use OR. Change pending.
- *SSR hydration mismatch on experiment (sentry[bot]):* Not valid — `this.experiments` populating in `mounted()` is the established pattern across all experiment splitters in the codebase

**Full code review (2026-04-14, 8 parallel subagents, 84 rules):**
- 2 implemented: Rule 6 (emits before props in FilterButtons), Rule 26 (BEM `--` → single hyphen `.shade-shop-loading`)
- 3 skipped: Rule 31 (focus-visible on cards), ad-15 (trivial multi-line returns), sg-8 (px→rem clean conversions)
- False positives filtered: Rule 2 (PascalCase accepted), Rule 16 (user overrode for this component), ad-16 (first-party `:deep()`), ad-23/ad-12 (card extraction — deliberate decision), sg-9 (space key on role="link"), sg-12 (role="link" deliberate), ad-1 (decorative alt deliberate)

**Filter logic change (2026-04-14):**
- Donna Yan confirmed filters should be OR (show products matching ANY selected tag)
- `filteredSections`: `activeFilters.every()` → `activeFilters.some()` — NOT YET APPLIED (blocked on Closest Shades clarification)
- Closest Shades section behavior: **BLOCKED — awaiting Donna's response** on when/what to display now that OR logic means multi-filter produces results
- Kyo asked Donna for clarification on 2026-04-14; response pending

**Tests:** 67 total (52 ShadeShopPage + 15 FilterButtons + 2 LocationCard experiment tests).

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
| `~/.brain.d/roam-nodes/madison_reed/2026-04-08-120100-dotcompb_7466.org` | DOTCOMPB-7466 (31 ACs, 10 mockups, data pipeline investigation, CMS architecture, experiment design, 8-phase plan) |
| `~/.brain.d/roam-nodes/madison_reed/assets/mobile_shop:brown*.jpg` (5 files) | DOTCOMPB-7466 Figma mockups (mobile) |
| `~/.brain.d/roam-nodes/madison_reed/assets/desktop_shop*.png` (5 files) | DOTCOMPB-7466 Figma mockups (desktop) |
| `website/src/vuescripts/components/Shop/ShadeShopPage/*` (3 files + components/) | DOTCOMPB-7466 (page, index, tests) |
| `website/src/vuescripts/components/Shop/ShadeShopPage/components/*` (2 files) | DOTCOMPB-7466 (FilterButtons + tests) |
| `website/src/assets/svg-icons/close-thin.svg` | DOTCOMPB-7466 (dismiss icon for filter buttons) |
| `website/src/vuescripts/components/Shop/routes.js` | DOTCOMPB-7466 (added shade shop route) |
| `website/src/vuescripts/components/ColorBarLocationSectionV1/LocationCard.vue` | DOTCOMPB-7886 (experiment redirect) |
| `website/src/vuescripts/components/ColorBarLocationSectionV1/LocationCard.test.js` | DOTCOMPB-7886 (2 experiment tests) |
| `website/src/vuescripts/components/ColorBarMapSection/LocationsDirectory.vue` | DOTCOMPB-7886 (experiment redirect) |
| `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` | Sprint Board Index |

### Session & Directives

| File | Purpose |
|---|---|
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/site-revolution-redesign.md` | This session file |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/ticket-pr-template.md` | PR template directive |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.** This section captures the most recent work and immediate next steps.

### What was done last (2026-04-14)

*   **Andris PR review addressed** — 2 comments on PR #20512 fixed: (1) `addUniqueProducts()` extracted from duplicated dedup blocks (ad-23), (2) `product?.imagery` cached into `const imagery` (ad-24).
*   **Full code review (8 subagents, 84 rules)** — 2 implemented (emits order in FilterButtons, BEM `--` → single hyphen), 3 skipped (focus-visible, trivial returns, px→rem). Multiple false positives filtered.
*   **3 AI bot comments dismissed** — Missing route props (CMS pipeline), AND filter logic (Donna confirmed OR), SSR hydration on experiments (established pattern).
*   **Code-review skill updated** — ad-23 (extract repeated logic) and ad-24 (cache repeated lookups) added to `rules/mr-review-checklist.md` and `SKILL.md`. 84 rules total now.
*   **Commit 3 staged** — Andris fixes + BEM rename + emits order. Ready to commit.
*   **OR filter logic still blocked** — Donna confirmed OR, but Closest Shades clarification pending.

### Pending

*   **DOTCOMPB-7466 — Commit 3** — Staged (Andris fixes). Needs commit + push to PR #20512.
*   **DOTCOMPB-7466 — OR filter logic** — Code change ready (`every` → `some`). Closest Shades behavior blocked on Donna's response.
*   **DOTCOMPB-7903** — Changes staged, not committed. PR #20481 description updated. Branch `DOTCOMPB-7903`.
*   **DOTCOMPB-7712** — PR #20423 OPEN. In Code Review. Branch `DOTCOMPB-7712`. 81 tests.
*   **DOTCOMPB-7742** — In Test (JIRA: Pruebas). PR #20368.
*   **DOTCOMPB-7717 cleanup** — Not yet implemented. Plan in §3.8.
*   **DashHudson Integration** — Research complete. Awaiting PM input.

### Where to resume

If user wants to **commit Andris fixes**: Commit 3 staged. Message: `fix: Address PR review — extract dedup method, cache imagery lookup, fix naming`. Push to PR #20512.
If user wants to **implement OR logic**: Change L200 `activeFilters.every()` → `activeFilters.some()`. Tests will break until Closest Shades decision resolves.
If Donna responds **"remove Closest Shades"**: Remove `closestShades` computed, `.empty-state` + `.suggestions` template, 5 tests. Update announcement.
If Donna responds **"keep for edge cases"**: Rewrite `closestShades` for unselected coverage types.
If user asks for a **new task**: Check Section 2.5 (Pending Work).

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

### DASH HUDSON / DASH SOCIAL DEEP RESEARCH (2026-04-05) — Location-Specific Gallery Integration

#### 1. What Is Dash Hudson (Now Dash Social)?

Dash Social (rebranded from Dash Hudson in **January 2025**) is a social media management and social commerce platform. Key capabilities:
- Social media analytics/reporting across Instagram, TikTok, Facebook, Pinterest, YouTube, LinkedIn, Threads
- Content scheduling and publishing
- **Shoppable galleries** — embeddable UGC/social photo galleries on websites
- **UGC discovery** — aggregates user-generated content, rights management
- **Content library** with AI-powered engagement predictions, visual search, boards for segmentation
- **LikeShop** — link-in-bio tool

**Rebrand details:**
- `dashhudson.com` → 301 redirects to `dashsocial.com`
- CDN URLs mixed: `cdn.dashsocial.com` (board-embed) + `cdn.dashhudson.com` (product-carousel, board-carousel — still working)
- API base URL unchanged: `library-backend.dashhudson.com`
- Auth endpoint: `auth.dashhudson.com`
- Developer docs: `developer.dashsocial.com`

#### 2. Current Integration in Madison Reed Codebase

**Type:** 100% client-side widget injection. **Zero backend/API integration.**

**Two components, two patterns:**

| Component | File | Used On | How It Works |
|---|---|---|---|
| `DashHudsonScriptInner` | `PDP/DashHudsonScriptInner.vue` | All PDPs + V1 HCB location pages | Injects `product-carousel-embed.js` with hardcoded `brand_id=18947`. Shows **brand-wide** UGC — same content everywhere. |
| `DashHudsonWidget` | `DashHudsonWidget/DashHudsonWidget.vue` | Globally registered (`dash-hudson-widget`) — used in CMS partials | Flexible: supports `board-embed`, `product-carousel-embed`, `board-carousel-embed`. Takes `galleryId` prop — **already supports per-gallery embedding**. |

**Madison Reed's brand_id:** `18947` (hardcoded in `DashHudsonScriptInner.vue` line 76)

**V1 HCB usage (both use `DashHudsonScript`, which wraps `DashHudsonScriptInner`):**
- `HcbIndividual.vue` line 18: `DashHudsonScript#hair-color-bar-photos` — brand-wide carousel, title "Real clients, really gorgeous results"
- `HcbLocationPage.vue` line 15: `DashHudsonScript` — same brand-wide carousel, same title

**V2 HCB:** `HcbLocationPageV2` does **NOT** use DashHudson. Uses `galleryImages` from CMS + DB carousel images instead. Session decision #22 (2026-03-15) said "DashHudson NOT integrated into hero gallery — No per-location gallery API."

**Legacy reference — Pixlee:** The predecessor UGC platform. `location.cmsData.pixleeAlbumId` exists in the data model (per-product, per-location album IDs configured in Tophat). `mr_modules/controllers/lib/pixlee.js` has a Dec 2024 comment: "switching over to Dash Hudson." `pixleeUpload` lambda function also flagged for removal. **Pixlee had per-location album IDs — DashHudson currently does not replicate this pattern.**

**No DashHudson fields in location data model:** Searched `mr_modules/dataAccess`, `colorbarCache.js`, `appointments/lib/classes/Location.js` — zero references to `dashHudson`, `dash_hudson`, or `galleryId`. The location object has no DashHudson configuration.

#### 3. Dash Social API — Full Capabilities

**Authentication:** Bearer token in `Authorization` header. Generated in Dash Social admin → Settings → API & Integrations → Developers. Requires brand admin role. **Must never be exposed client-side.**

**Base URLs:**

| Service | URL |
|---|---|
| Auth/Self | `https://auth.dashhudson.com` |
| Library API | `https://library-backend.dashhudson.com` |
| Developer Docs | `https://developer.dashsocial.com` |
| Widget CDN | `https://cdn.dashsocial.com` / `https://cdn.dashhudson.com` |

**Gallery API (most relevant):**

| Method | Endpoint | Description |
|---|---|---|
| `GET` | `/brands/{brand_id}/galleries` | List ALL galleries for the brand |
| `GET` | `/brands/{brand_id}/galleries/{gallery_id}` | Get single gallery |
| `GET` | `/brands/{brand_id}/galleries/{gallery_id}/media` | **Get media items in a gallery** |
| `POST` | `/brands/{brand_id}/galleries` | Create gallery |
| `POST` | `/brands/{brand_id}/galleries/{gallery_id}/media` | Add media to gallery |

**Media object structure (from Gallery Media API):**
```
{
  id: integer,
  brand_media_id: integer,
  source: "INSTAGRAM" | "TIKTOK" | "UPLOAD" | ...,
  type: "IMAGE" | "VIDEO" | "CAROUSEL" | "REEL",
  source_type: "UGC" | "OWNED" | "OTHER",
  image: {
    sizes: {
      original: { url, width, height, size },
      original_converted: { url, width, height, size },
      small: { url, width, height, size },
      small_square: { url, width, height, size },
      medium_square: { url, width, height, size }
    }
  },
  content_tags: [string],
  products: [{ coordinates, source_ids, pricing }],
  caption_sentiment: { positive, negative, neutral },
  created_at, updated_at
}
```

**Gallery object:** `{ id, brand_id, name, label, description, gallery_type, gallery_size, live, tags: [{name, color}], latest_media, created_at, updated_at }`

**Other relevant endpoints:**
- `PUT /brands/{brand_id}/media/v2` — List/filter media objects (bulk)
- `GET /brands/{brand_id}/campaigns/{campaign_id}/ugc` — Campaign UGC
- `GET /organizations/{org_id}/content-tags` — List content tags

**Rate limits:** Referenced in docs but specific numbers not publicly documented.
**Pagination:** `paging` object with `count`, `next`, `previous`.

#### 4. Location-Based Content — The Key Question

**There is NO native "location" concept in Dash Social galleries.** But the architecture supports location-based segmentation through:

**Option A — One Gallery Per Location (Widget approach, client-side):**
- Create a gallery in Dash Social admin for each HCB location (e.g., "HCB - West Village", "HCB - Beverly Hills")
- Each gallery gets a numeric `gallery_id`
- Store the `gallery_id` in the location's data (Tophat CMS or DB field)
- Use existing `DashHudsonWidget` component: `<dash-hudson-widget script-type="board-embed" :gallery-id="location.dashHudsonGalleryId">`
- **Pros:** No API key exposure, no backend changes, existing component already works
- **Cons:** Renders a DashHudson-controlled widget (not custom Vue), requires Tophat field, requires manual gallery curation in Dash Social admin

**Option B — Gallery API (Server-side, custom rendering):**
- Call `GET /brands/18947/galleries/{gallery_id}/media` server-side with Bearer token
- Fetch images for the location's gallery, cache in Redis
- Merge into `galleryImages` alongside CMS + DB images
- Render in existing custom masonry grid (HcbLocationPhotosPage) and hero carousel
- **Pros:** Full control, renders in our own components, can cache, no third-party DOM injection
- **Cons:** Requires API key management (secureConfig), new backend endpoint, rate limit management

**Option C — Content Tags (Advanced, single gallery):**
- Tag all media in one brand gallery with location codes as content tags
- Query `PUT /brands/18947/media/v2` with tag filters
- **Pros:** Single gallery management, flexible filtering
- **Cons:** Most complex, relies on consistent tagging, requires server-side API

#### 5. Widget Types Available

| Type | Script | Config ID | Description |
|---|---|---|---|
| Board (grid) | `board-embed.js` | `data-gallery-id` | Static grid gallery |
| Board Carousel | `board-carousel-embed.js` | `data-gallery-id` | Carousel gallery |
| Product Carousel | `product-carousel-embed.js` | `data-id` + `data-id-type` | Brand-wide product carousel (current MR usage) |
| LikeShop | `embed.js` | `data-id` + `data-id-type` | Link-in-bio gallery |

**Key insight:** The current `DashHudsonScriptInner` uses **product-carousel-embed** with `brand_id` — this is always brand-wide. To get per-location content, you need **board-embed** or **board-carousel-embed** with a location-specific `gallery_id`. The `DashHudsonWidget` component already supports this.

#### 6. Implementation Plan — DashHudson Per-Location Photos (API — Option B)

**SUPERSEDES Session Decision #22** — That decision was based on "No per-location gallery API." Research confirms galleries CAN be location-specific via `gallery_id`. Revising.

**Architecture model:** Follows the verified Birdeye reviews full-stack pattern (17 extracted patterns, P1-P17). See Birdeye trace below for the source of truth.

##### 6.1 Current State: Zero Auth, Client-Side Only

The existing DashHudson integration uses **zero authentication**. There is no `config.dashHudson` in secureConfig, no API keys, no Bearer tokens, no server-side calls. The entire integration is a `<script>` tag injection:

```javascript
// DashHudsonScriptInner.vue (line 73-77) — what runs today on V1 HCB + all PDPs
tag.src = "https://cdn.dashhudson.com/web/js/product-carousel-embed.js";
tag.setAttribute("data-id", "18947");        // brand ID, public in HTML source
tag.setAttribute("data-id-type", "brand_id"); // tells widget to load brand-wide content
```

DashHudson's CDN JavaScript loads, reads `data-*` attributes, renders a widget in its own DOM. No API key needed for widget embedding — the brand ID `18947` is public.

**`DashHudsonWidget.vue`** (the globally registered component) already supports per-gallery rendering via `galleryId` prop — also client-side, also no auth:
```javascript
script.setAttribute("data-gallery-id", this.galleryId); // prop-driven, per gallery
```

**All 21 DashHudson files in codebase (by role):**

| Role | Files |
|---|---|
| **Core components** | `DashHudsonWidget/DashHudsonWidget.vue`, `DashHudsonWidget/index.js`, `PDP/DashHudsonScriptInner.vue`, `PDP/DashHudsonScript.vue` |
| **Global registration** | `mrVueApp.js` (L87 import, L747 `app.component('dash-hudson-widget')`), `ssr/registerGlobalsSsr.js` (L115 async import, L271 register) |
| **V1 HCB pages** | `HcbIndividual/HcbIndividual.vue` (L18, L48), `HcbLocationPage/HcbLocationPage.vue` (L15, L36) |
| **PDPs (8 files)** | `DefaultPdp.vue`, `DefaultPdpV2.vue`, `ColorKitPdp.vue`, `ColorKitPdpV2.vue`, `LightWorksPdp.vue`, `MisterPdp.vue`, `PerfectPairPdp.vue`, `PdpBottom.vue` |
| **Quiz** | `HairColorQuizResultsPage.vue`, `NotExactMatchSection.vue`, `NotExactMatchSection.test.js` |
| **Snapshots** | `PdpBottom.test.js.snap`, `LightWorksPdp.test.js.snap` |

**V2 HCB (`HcbLocationPageV2`):** Does NOT use DashHudson at all. Uses `galleryImages` from CMS + DB.

**No backend files.** No `mr_modules/dashHudson/`. No `config.dashHudson`. No webservice endpoints. No controllers. No services.

##### 6.2 Why API (Option B) Needs secureConfig

The widget approach (current) loads DashHudson's JavaScript which renders its own DOM — we have zero control over the images, their order, or how they display. To merge DashHudson images into our own `galleryImages` array (appearing in the masonry grid, hero +X count, photos page), we need the **raw image URLs and dimensions**. That data only comes from the Dash Social REST API:

```
GET https://library-backend.dashhudson.com/brands/18947/galleries/{galleryId}/media
Authorization: Bearer {token}
```

This Bearer token is generated in Dash Social admin (Settings → API & Integrations → Developers), shown once, and must be stored securely. In MR's infrastructure, that means AWS SSM Parameter Store, loaded at startup by `mr_modules/secureConfig.js` and accessible as `require('config').dashHudson`.

**This is the same pattern as every other third-party API in the codebase:**

| Integration | Config Key | Auth Method |
|---|---|---|
| Birdeye | `config.birdeye` → `{ baseURL, apiKey }` | `?api_key=X` in URL |
| Iterable | `config.iterable` | API key |
| Facebook | `config.facebook` | OAuth |
| SmartyStreets | `config.smartystreets` | Auth ID + token |
| Stripe | `config.stripe` | Secret key |
| **DashHudson (proposed)** | **`config.dashHudson`** → **`{ baseURL, apiKey, brandId }`** | **`Authorization: Bearer X`** |

##### 6.3 Birdeye Full-Stack Architecture (Verified Source of Truth)

**Layer-by-layer trace of the working Birdeye reviews integration — this is the pattern we replicate:**

```
Layer 1: Vue Component
  HairColorBarLocationReviews.vue
  └── mounted() → this.getLocationReviews(this.location.code)
  └── mapState('colorbar', ['location', 'locationReviews'])
  └── Tri-state loading: null=loading | {}=no data | populated=ready
  └── Guard: v-if="location.birdeyeId"

Layer 2: Vuex Store
  colorbar.js
  └── state: { locationReviews: null }
  └── action: getLocationReviews({ commit }, code)
      commit('setLocationReviews', null)  ← loading state
      try { data = await service({ code }); commit(data) }
      catch { commit({ avgRating: 0, reviewCount: 0, reviews: [] }) }  ← graceful fallback
  └── mutation: setLocationReviews(state, reviews) { state.locationReviews = reviews }

Layer 3: Frontend Service
  vueColorbarSvc.js
  └── getLocationReviews(params) → mrApi.get('/api/colorbar/getLocationReviews', { params })
  └── params: { code: 'location-code-string' }

Layer 4: Webservice (Thin pass-through)
  webservices/lib/colorbar.js
  └── function getLocationReviews(params, req, callback)
  └── locationCtl.getLocationReviews(params.code, callback)  ← NO business logic
  └── .allowAgentImpostor = true
  └── .params = { code: { type: 'string' } }

Layer 5: Location Controller (Orchestrator)
  appointments/lib/location.js
  └── getLocationReviews(code, callback)
      1. getLocationByCode(code) → location object (from colorbarCache)
      2. Guard: if (!location || !location.birdeyeId) → return empty fallback
      3. birdeyeCtl.getReviewsByBirdeyeId(location.birdeyeId, callback)
      4. On error → return empty fallback (NOT propagate error)
      5. Assemble response:
         - avgRating, reviewCount from location.reviewData (MongoDB cached summary)
         - reviews from live Birdeye API call (fresh data)

Layer 6: Integration Controller (Data transformation)
  controllers/lib/birdeye.js
  └── getReviewsByBirdeyeId(birdeyeId, callback)
      1. new BirdeyeAPI(config.birdeye, birdeyeId)
      2. birdeyeApi.getReviews({ statuses: ['published'] })
      3. Normalize response array (handles multiple response shapes)
      4. Filter qualified reviews (has author, non-empty comment, has photo)
      5. Fisher-Yates shuffle (variety on page reload)
      6. Slice to DISPLAY_REVIEW_COUNT (3)
      7. Map to frontend schema: { rating, content, author, profilePhotoUrl, date, sourceName }
      8. Normalize photo URLs (relative → absolute with CDN prefix)

Layer 7: API Client (HTTP transport)
  birdeye/BirdeyeAPI.js
  └── class BirdeyeAPI
      Private fields: #baseURL, #apiKey, #businessId
      Constructor(config, businessId) ← config from secureConfig, businessId per-location
      _makeApiRequest(path, method, params):
        - url.resolve(#baseURL, path) for full URL
        - ThirdPartyAPILog for request/response logging
        - Retry loop: 8 attempts prod, 3 test (async.whilst)
        - Success: 200-299 → resolve data
        - Failure: log.error + retry, after max attempts → reject
        - GET params in querystring, POST params in JSON body
        - Accept: application/json header
      getReviews(filters, startIndex, count):
        - POST /resources/v1/review/businessId/{businessId}?api_key={apiKey}&sindex=0&count=20
        - body: { statuses: ['published'] }
```

##### 6.4 Extracted Backend Patterns (P1-P17)

| # | Pattern | Birdeye Reference | DashHudson Application |
|---|---|---|---|
| **P1** | **Module structure: dedicated directory** | `mr_modules/birdeye/` with `package.json`, `index.js`, `BirdeyeAPI.js` | `mr_modules/dashHudson/` with `package.json`, `index.js`, `DashHudsonAPI.js` |
| **P2** | **API client as ES class with private fields** | `class BirdeyeAPI { #baseURL; #businessId; #apiKey; }` | `class DashHudsonAPI { #baseURL; #apiKey; #brandId; }` |
| **P3** | **Config from secureConfig** | `config.birdeye` → `{ baseURL, apiKey }` | `config.dashHudson` → `{ baseURL, apiKey, brandId }` |
| **P4** | **ThirdPartyAPILog** | `ThirdPartyAPILog.CONFIG.BIRDEYE` | Register `DASH_HUDSON` in `ThirdPartyAPILog.js` CONFIG |
| **P5** | **Retry with async.whilst** | 8 prod / 3 test attempts | Same retry logic |
| **P6** | **Auth per-request** | `?api_key=X` in URL | `Authorization: Bearer X` in headers |
| **P7** | **Webservice = thin pass-through** | `locationCtl.getLocationReviews(params.code, callback)` | `locationCtl.getLocationDashHudsonMedia(params.code, callback)` |
| **P8** | **Location controller orchestrates** | lookup → guard birdeyeId → call controller → assemble | lookup → guard dashHudsonGalleryId → call controller → return |
| **P9** | **Guard ID before API call** | `if (!location.birdeyeId) return fallback` | `if (!location.dashHudsonGalleryId) return { media: [] }` |
| **P10** | **3-layer graceful fallback** | API → controller → Vuex all catch errors | Same: DH fails → controller returns [] → Vuex commits [] → CMS+DB images still show |
| **P11** | **Controller transforms response** | birdeye.js: filter, shuffle, slice, map | dashHudson.js: filter IMAGE type, limit, map to MR media shape |
| **P12** | **Normalize foreign data to MR schema** | `review.reviewer.nickName` → `author` | `media.image.sizes.original.url` → `url`. Map to `{ _id, url, width, height, alt_text, file_type }` |
| **P13** | **Fetch in mounted(), not SSR** | Reviews load client-side | DH images load client-side. CMS+DB render SSR. |
| **P14** | **Per-location ID on location object** | `location.birdeyeId` (MongoDB, Tophat) | `location.dashHudsonGalleryId` (new Tophat field) |
| **P15** | **Log module** | `new Log('BirdeyeController')` | `new Log('DashHudsonController')` |
| **P16** | **Lazy-load require()** | `controllers.birdeye` | `controllers.dashHudson` |
| **P17** | **async function + callback bridge** | `async function getReviewsByBirdeyeId(id, callback)` with try/catch | `async function getMediaByGalleryId(id, callback)` with try/catch |

##### 6.5 Birdeye vs DashHudson API Comparison

| Aspect | Birdeye (Current) | Dash Social (Proposed) | Compatible? |
|---|---|---|---|
| **Auth method** | `?api_key=X` in URL querystring | `Authorization: Bearer X` in header | Yes — add to `headers` in `_makeApiRequest` options |
| **Request library** | `request` (Node.js) | Same `request` library | Exact match — supports Bearer natively |
| **API base URL** | `https://api.birdeye.com` | `https://library-backend.dashhudson.com` | Same pattern — `config.dashHudson.baseURL` |
| **Per-location ID** | `birdeyeId` (numeric, on location doc) | `galleryId` (numeric, new field) | Same — need Tophat field |
| **Brand-level ID** | N/A (each location = own businessId) | `brandId=18947` (shared, all locations) | Different — DH uses brandId+galleryId, Birdeye uses only businessId |
| **Endpoint structure** | `/resources/v1/review/businessId/{id}` | `/brands/{brandId}/galleries/{galleryId}/media` | Same RESTful pattern |
| **HTTP method** | POST (body: filter object) | GET (query params) | Different — minor: `qs` vs `json` in options |
| **Response shape** | `{ reviews: [...] }` or raw array | `{ data: [...], paging: { count, next, previous } }` | Different — extract `.data`, handle pagination |
| **Media URLs** | Relative or absolute (CDN prefix needed) | Always absolute | Simpler — no normalization |
| **Image sizes** | Single thumbnail URL | Multiple: `original`, `small`, `medium_square`, `small_square` | Better — use `original` for masonry, `medium_square` for thumbs |
| **Rate limits** | Not documented | Not publicly documented | Same — retry handles both |
| **Caching** | No cache (fresh each load) | Should add Redis (TTL: 1hr) | Enhancement — images change less than reviews |
| **Data volume** | 20 fetched, 3 displayed | Unknown per gallery (5-500). Cap at `count=50` | Must handle pagination or set limit |

##### 6.6 Prerequisites (Non-code, BLOCKING)

| # | Task | Owner | Why Blocking | How to Unblock |
|---|---|---|---|---|
| 0.1 | **Confirm Dash Social plan includes REST API access** | PM/Marketing | API requires paid plan tier. Without it, Option B is impossible. | Check plan at dashsocial.com or contact Dash Social account rep. |
| 0.2 | **Generate API Bearer token** | Marketing/Admin | No token = no auth = no API calls. Token shown once at creation. | Dash Social admin → Settings → API & Integrations → Developers → Create token. |
| 0.3 | **Add `config.dashHudson` to AWS SSM secureConfig** | DevOps | `require('config').dashHudson` must resolve at runtime. Without it, `new DashHudsonAPI(config.dashHudson, brandId)` throws `undefined`. | Add 3 SSM params: `dashHudson.baseURL` = `https://library-backend.dashhudson.com`, `dashHudson.apiKey` = Bearer token from 0.2, `dashHudson.brandId` = `18947`. |
| 0.4 | **Create location-specific galleries in Dash Social** | Marketing | No galleries = no gallery IDs = no data to fetch. Each HCB location needs its own gallery with curated photos. | In Dash Social admin: Content Library → Galleries → Create. Name: "HCB - {Location Name}". Add 10-30 photos per gallery. Record the numeric `gallery_id` from the URL. |
| 0.5 | **Add `dashHudsonGalleryId` field to location data model** | Tophat team | `location.dashHudsonGalleryId` does not exist in the location document. Without it, Phase 1 code has nothing to read. | Add field in Tophat admin config for HCB locations. String type, nullable. Similar to existing `location.cmsData.pixleeAlbumId` (Pixlee predecessor). Verify field propagates through `colorbarCache.getLocation()` → Vuex `colorbar.location`. |
| 0.6 | **Populate gallery IDs for pilot locations** | Marketing/Ops | Even with the field, empty values mean the guard (`!location.dashHudsonGalleryId`) returns `{ media: [] }`. | Enter numeric gallery IDs from 0.4 into Tophat for each pilot location. |
| 0.7 | **Decide image count limit** | PM/Engineering | Birdeye shows 3 reviews. Gallery photos could be 5-500. Need max for photos page. | Recommend: 30-50 images max. Set in `MAX_GALLERY_IMAGES` constant (controller). |
| 0.8 | **Content rights clearance** | Marketing/Legal | UGC photos need rights before public display. Dash Social has built-in rights management. | Confirm all gallery images are rights-cleared in Dash Social before going live. |

##### 6.7 Implementation Phases (All Code — Option B API)

**Phase 1: API Client Module** — `mr_modules/dashHudson/`

New files (following `mr_modules/birdeye/` structure exactly — P1):

```
mr_modules/dashHudson/
├── package.json          ← { "name": "dashHudson", "version": "0.0.1", "main": "./index.js" }
├── index.js              ← module.exports = { DashHudsonAPI: require('./DashHudsonAPI') }
└── DashHudsonAPI.js      ← class DashHudsonAPI (see skeleton below)
```

`DashHudsonAPI.js` — full implementation skeleton (P2, P4, P5, P6):
```javascript
const request = require('request');
const url = require('url');
const async = require('async');
require('MRUtils').promise;
const Log = require('Log');
const log = new Log('DashHudsonAPI');
const MRError = require('error').madisonReed;
const ThirdPartyAPILog = require('ThirdPartyAPILog');
const thirdPartyAPILog = new ThirdPartyAPILog(ThirdPartyAPILog.CONFIG.DASH_HUDSON);

/** @enum {string} */
const HttpMethod = { GET: 'GET', POST: 'POST' };
Object.freeze(HttpMethod);

class DashHudsonAPI {
  /** @private @type {string} */ #baseURL;
  /** @private @type {string} */ #apiKey;
  /** @private @type {number} */ #brandId;

  /**
   * @param {{ baseURL: string, apiKey: string }} config from secureConfig
   * @param {number} brandId Madison Reed brand ID (18947)
   */
  constructor(config, brandId) {
    this.#baseURL = config.baseURL;
    this.#apiKey = config.apiKey;
    this.#brandId = brandId;
  }

  /**
   * Make authenticated request to Dash Social API with retry logic.
   * Mirrors BirdeyeAPI._makeApiRequest (P5) with Bearer auth (P6).
   *
   * @param {string} path API path
   * @param {HttpMethod} method HTTP method
   * @param {object} params query params (GET) or body (POST)
   * @returns {Promise<object>}
   */
  _makeApiRequest(path, method, params) {
    const requestMethod = (method === HttpMethod.GET ? request.get : request.post);

    const options = {
      url: url.resolve(this.#baseURL, path),
      method,
      [method === HttpMethod.GET ? 'qs' : 'json']: { ...params },
      headers: {
        Accept: 'application/json',
        Authorization: `Bearer ${this.#apiKey}`,  // ← P6: Bearer instead of URL param
      },
    };
    options.json = options.json || true;

    const requestLog = thirdPartyAPILog.info({
      timePerformed: new Date(),
      endpoint: options.url,
      requestMethod: options.method,
      requestParams: options.json || options.qs,
    });

    return new Promise((resolve, reject) => {
      let attempt = 0;
      let makeRequest = true;
      const maxRequestAttempts = process.env.NODE_ENV === 'test' ? 3 : 8;

      async.whilst(() => attempt < maxRequestAttempts && makeRequest, cb => {
        attempt++;
        requestMethod(options, (error, response, data) => {
          if (error) {
            requestLog.patch({
              additionalInfo: { clientRequestError: error.toString() },
            });
            return cb(new MRError(500, error)());
          }

          requestLog.patch({
            timeCompleted: new Date(),
            requestHeaders: response.req._headers,
            responseHeaders: response.headers,
            responseBody: typeof response.body === 'string' ? response.body : JSON.stringify(response.body),
          });

          if (response.statusCode >= 200 && response.statusCode < 300) {
            makeRequest = false;
            return cb(null, data);
          }

          const errorMessage = response.body?.message || response.body?.error;
          const err = new MRError(response.statusCode, `DashHudson request failed with ${response.statusCode}, ${errorMessage || 'UNKNOWN ERROR'}`)();
          log.error(err);
          cb(err);
        });
      }, (err, data) => {
        if (err) {
          return reject(err);
        }
        resolve(data);
      });
    });
  }

  /**
   * Get media items from a specific gallery.
   * Endpoint: GET /brands/{brandId}/galleries/{galleryId}/media
   * Docs: https://developer.dashsocial.com/reference
   *
   * @param {number|string} galleryId gallery ID from Dash Social
   * @param {number} [count=50] max items to return
   * @returns {Promise<{ data: Array, paging: { count: number, next: string|null, previous: string|null } }>}
   */
  getGalleryMedia(galleryId, count = 50) {
    return this._makeApiRequest(
      `/brands/${this.#brandId}/galleries/${galleryId}/media`,
      HttpMethod.GET,
      { count }
    );
  }

  /**
   * List all galleries for the brand.
   * Useful for discovery/admin — find which gallery IDs exist.
   *
   * @returns {Promise<{ data: Array }>}
   */
  listGalleries() {
    return this._makeApiRequest(
      `/brands/${this.#brandId}/galleries`,
      HttpMethod.GET,
      {}
    );
  }
}

module.exports = DashHudsonAPI;
```

**Phase 2: ThirdPartyAPILog Registration**

File: `mr_modules/ThirdPartyAPILog/ThirdPartyAPILog.js`

Add to `LOG_CONFIG` object (after `BIRDEYE` entry, line ~53):
```javascript
DASH_HUDSON: {
  name: 'dashHudson',
},
```

**Phase 3: Integration Controller** — `mr_modules/controllers/lib/dashHudson.js`

New file (P11, P12, P15, P17):
```javascript
const config = require('config');
const Log = require('Log');
const log = new Log('DashHudsonController');

const { DashHudsonAPI } = require('../../dashHudson');

const MAX_GALLERY_IMAGES = 30;

exports.getMediaByGalleryId = getMediaByGalleryId;

/**
 * Fetch gallery media from Dash Social API, filter to images only,
 * normalize to MR media object shape for ImgBox compatibility.
 *
 * @param {string|number} galleryId Dash Social gallery ID
 * @param {Function} callback (err, { media: Array })
 */
async function getMediaByGalleryId(galleryId, callback) {
  const dashHudsonApi = new DashHudsonAPI(config.dashHudson, config.dashHudson.brandId);
  try {
    const data = await dashHudsonApi.getGalleryMedia(galleryId, MAX_GALLERY_IMAGES);

    // P11: Normalize response shape (DH returns { data: [...] } or array)
    const mediaList = Array.isArray(data) ? data : (data?.data || []);

    // P12: Filter + transform to MR media object shape
    const images = mediaList
      .filter((item) => item.type === 'IMAGE' && item.image?.sizes?.original?.url)
      .slice(0, MAX_GALLERY_IMAGES)
      .map((item) => ({
        _id: `dh_${item.id}`,
        url: item.image.sizes.original.url,
        width: item.image.sizes.original.width || 0,
        height: item.image.sizes.original.height || 0,
        alt_text: '',
        file_type: 'image/jpeg',
        source: 'dashhudson',
        thumbnailUrl: item.image.sizes.medium_square?.url || item.image.sizes.original.url,
      }));

    callback(null, { media: images });
  } catch (err) {
    log.error(err);
    callback(err);
  }
}
```

**Phase 4: Location Controller** — `mr_modules/appointments/lib/location.js`

Add function + export (P8, P9, P10). Mirrors `getLocationReviews` exactly:
```javascript
// Export (add to exports block ~line 102):
exports.getLocationDashHudsonMedia = getLocationDashHudsonMedia;

// Function (add after getLocationReviews, ~line 1201):
/**
 * Get Dash Hudson gallery media for a location.
 * Looks up location by code, checks for dashHudsonGalleryId,
 * calls DashHudson controller. Graceful fallback on any error.
 *
 * @param {string} code location code
 * @param {Function} callback (err, { media: Array })
 */
function getLocationDashHudsonMedia(code, callback) {
  const dashHudsonCtl = controllers.dashHudson;

  getLocationByCode(code, (err, location) => {
    if (err) {
      return callback(err);
    }
    if (!location || !location.dashHudsonGalleryId) {
      return callback(null, { media: [] });
    }

    dashHudsonCtl.getMediaByGalleryId(location.dashHudsonGalleryId, (err, mediaData) => {
      if (err) {
        return callback(null, { media: [] });
      }
      callback(null, { media: mediaData?.media || [] });
    });
  });
}
```

**Phase 5: Webservice Endpoint** — `mr_modules/webservices/lib/colorbar.js`

Add export + function (P7). Thin pass-through, mirrors `getLocationReviews`:
```javascript
// Export (add to exports block ~line 47):
exports.getLocationDashHudsonMedia = getLocationDashHudsonMedia;

// Function (add after getLocationReviews, ~line 256):
function getLocationDashHudsonMedia(params, req, callback) {
  locationCtl.getLocationDashHudsonMedia(params.code, callback);
}
getLocationDashHudsonMedia.allowAgentImpostor = true;
getLocationDashHudsonMedia.params = {
  code: { type: 'string' },
};
```
→ Auto-exposed at: `GET /api/colorbar/getLocationDashHudsonMedia?code=X`

**Phase 6: Frontend Service** — `website/src/vuescripts/services/vueColorbarSvc.js`

Add export + function. Mirrors `getLocationReviews`:
```javascript
// Add to default export object:
getLocationDashHudsonMedia,

// Add function:
export function getLocationDashHudsonMedia(params) {
  return mrApi.get('/api/colorbar/getLocationDashHudsonMedia', { params });
}
```

**Phase 7: Vuex Store** — `website/src/vuescripts/store/modules/colorbar.js`

Add state + action + mutation. Mirrors `locationReviews` pattern:
```javascript
// Import (add to import from vueColorbarSvc):
import { getLocationDashHudsonMedia } from '@services/vueColorbarSvc';

// State (add after locationReviews: null):
dashHudsonMedia: [],

// Action (add after getLocationReviews):
async getDashHudsonMedia({ commit }, code) {
  try {
    const { data } = await getLocationDashHudsonMedia({ code });
    commit('setDashHudsonMedia', data?.media || []);
  } catch {
    commit('setDashHudsonMedia', []);
  }
},

// Mutation (add after setLocationReviews):
setDashHudsonMedia(state, media) {
  state.dashHudsonMedia = media;
},
```

**Phase 8: Component Integration** — `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationPageV2.vue`

Fetch in `mounted()` (P13), merge into `galleryImages`:
```javascript
// mapState — add dashHudsonMedia:
...mapState('colorbar', ['location', 'dashHudsonMedia']),

// mapActions — add getDashHudsonMedia:
...mapActions('colorbar', ['loadLocation', 'getDashHudsonMedia']),

// mounted() — fetch DashHudson media client-side only:
mounted() {
  if (this.location?.dashHudsonGalleryId) {
    this.getDashHudsonMedia(this.location.code);
  }
},

// galleryImages computed — add DashHudson as third source:
galleryImages() {
  // CMS images first (hero primary/secondary — always render in SSR)
  const cmsImages = (this.defaultLocationImages || [])
    .filter(item => item?.image?.url)
    .map(item => ({ ...item, image: { ...item.image, url: item.image.url.split('?')[0] } }));
  // DB images second (Tophat uploads — render in SSR)
  const dbImages = this.locationImages;
  // DashHudson images third (social/UGC — appends client-side after mounted fetch)
  const dhImages = (this.dashHudsonMedia || [])
    .map(item => ({ image: item }));
  return [...cmsImages, ...dbImages, ...dhImages];
},
```

**SSR behavior:** CMS + DB images render during SSR (available from `serverPrefetch`). DashHudson images are `[]` during SSR (action fires in `mounted()`). After mount, `dashHudsonMedia` populates reactively → `galleryImages` recomputes → hero "+X photos" count updates → photos page masonry rerenders with additional images. Zero hydration mismatch because DH images append, never replace.

**Phase 9: Testing**

| Layer | Test File | What to Test |
|---|---|---|
| API Client | `mr_modules/dashHudson/DashHudsonAPI.test.js` | Constructor stores config. `getGalleryMedia` calls `_makeApiRequest` with correct path/method. Retry on failure. Bearer header present. |
| Controller | `mr_modules/controllers/lib/dashHudson.test.js` | Filters non-IMAGE types. Normalizes to MR shape. Respects MAX_GALLERY_IMAGES. Handles empty/null response. Error → callback(err). |
| Webservice | (Covered by integration tests) | Thin pass-through — no unit test needed per codebase pattern. |
| Vuex Store | `colorbar.js` existing test file | `getDashHudsonMedia` action commits data on success, `[]` on error. |
| Component | `HcbLocationPageV2.test.js` | `galleryImages` computed includes DH images after `dashHudsonMedia` state. `mounted()` calls action when `dashHudsonGalleryId` exists. Does NOT call when missing. |

**Phase 10: Tracking Events**

| Event | When | Properties |
|---|---|---|
| `HCB Location Photos - DashHudson Gallery Loaded` | After `getDashHudsonMedia` resolves with images | `{ locationCode, imageCount, isFrontEndEvent: true }` |
| `HCB Location Photos - DashHudson Image Clicked` | When user clicks a DH-sourced image on photos page | `{ locationCode, imageSource: 'dashhudson', isFrontEndEvent: true }` |

##### 6.8 Complete File Manifest (Option B API)

**New files (7):**

| File | Purpose | Pattern Source |
|---|---|---|
| `mr_modules/dashHudson/package.json` | Module metadata | `mr_modules/birdeye/package.json` |
| `mr_modules/dashHudson/index.js` | Barrel export `{ DashHudsonAPI }` | `mr_modules/birdeye/index.js` |
| `mr_modules/dashHudson/DashHudsonAPI.js` | API client class (auth, retry, logging) | `mr_modules/birdeye/BirdeyeAPI.js` |
| `mr_modules/controllers/lib/dashHudson.js` | Integration controller (filter, normalize) | `mr_modules/controllers/lib/birdeye.js` |
| `mr_modules/dashHudson/DashHudsonAPI.test.js` | API client unit tests | Birdeye test patterns |
| `mr_modules/controllers/lib/dashHudson.test.js` | Controller unit tests | Birdeye test patterns |
| *(No new frontend service file)* | Reuse `vueColorbarSvc.js` — add function | Same file as `getLocationReviews` |

**Modified files (6):**

| File | Change | Lines Affected |
|---|---|---|
| `mr_modules/ThirdPartyAPILog/ThirdPartyAPILog.js` | Add `DASH_HUDSON` to `LOG_CONFIG` | ~2 lines (after BIRDEYE entry) |
| `mr_modules/appointments/lib/location.js` | Add `getLocationDashHudsonMedia` export + function | ~20 lines (after `getLocationReviews`) |
| `mr_modules/webservices/lib/colorbar.js` | Add `getLocationDashHudsonMedia` export + function | ~8 lines (after `getLocationReviews`) |
| `website/src/vuescripts/services/vueColorbarSvc.js` | Add `getLocationDashHudsonMedia` export + function | ~5 lines |
| `website/src/vuescripts/store/modules/colorbar.js` | Add state + action + mutation for `dashHudsonMedia` | ~15 lines |
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationPageV2.vue` | Add mapState/mapActions, mounted() fetch, galleryImages merge | ~15 lines |

**Infrastructure (non-code):**

| Item | Change | Owner |
|---|---|---|
| AWS SSM Parameter Store | Add `dashHudson.baseURL`, `dashHudson.apiKey`, `dashHudson.brandId` | DevOps |
| Tophat location config | Add `dashHudsonGalleryId` field (String, nullable) | Tophat team |
| Dash Social admin | Create per-location galleries, curate photos | Marketing |

##### 6.9 CORRECTION (2026-04-06): `DashHudsonWidget` is a CMS Partial Component — No Tophat Field Needed

**Finding:** `dash-hudson-widget` is globally registered (`mrVueApp.js` L747, `registerGlobalsSsr.js` L271) but **never referenced in any Vue template in the codebase**. It exists solely for use inside **CMS partial HTML authored in Tophat** — the same pattern as `marketing-banner` and `featured-services-v2`.

**How it works (existing CMS partial pattern):**
1. Tophat admin authors HTML in a partial textarea (identified by `mixin-key`)
2. That HTML references globally registered Vue components with props inline: `<dash-hudson-widget script-type="board-embed" gallery-id="12345">`
3. `CMSPartial.vue` fetches the HTML via `/api/cmsSvc/getPartial`, compiles it as a Vue template at runtime
4. Globally registered components resolve and render as live Vue components

**Evidence:**
- `marketing-banner` — globally registered, never imported locally in any `.vue` file. Props (`title`, `theme`, `ctaUrl`) come from Tophat partial HTML.
- `featured-services-v2` — same pattern. Used inside `partial-featured-services-v2` Tophat partial.
- `dash-hudson-widget` — same pattern. `galleryId` prop would come from Tophat partial HTML, not a database field.

**What this means for Option A:**
- **No `dashHudsonGalleryId` database field needed** — the gallery ID lives in the CMS partial HTML, not in the location data model
- **No Tophat schema changes** — just create a new CMS partial in Tophat with the widget HTML
- Tophat admin creates partial with mixin-key like `"partial-dashhudson-gallery"`, HTML body contains `<dash-hudson-widget script-type="board-embed" gallery-id="XXXXX">`
- Page references it via `CMSPartial(mixin-key="partial-dashhudson-gallery")`

**Per-location gallery IDs via CMS Partials — two sub-options:**

**A1 — Shared partial (brand-wide, same gallery for all locations):**
- One partial in Tophat: `mixin-key="partial-dashhudson-gallery"` with a single `gallery-id`
- Every location shows the same DashHudson gallery
- Simplest — same content as V1 `DashHudsonScript` but using the board-embed grid instead of product-carousel

**A2 — Per-location partial (different gallery per location):**
- Add a new `cmsSettings` field in Tophat (like `marketingPartialLight`/`marketingPartialDark`) — e.g., `cmsSettings.dashHudsonPartial`
- Each location's Tophat page config points to a different partial mixin-key
- Each partial has a different `gallery-id` in its HTML
- Component reads: `CMSPartial(v-if="dashHudsonPartial" :mixin-key="dashHudsonPartial")`
- **This is exactly how marketing modules already work per-location** — no new infrastructure pattern

**A3 — `CmsPartialSsr` with `clientConfig` (per-location, one partial):**
- One shared partial in Tophat, HTML uses: `<dash-hudson-widget script-type="board-embed" :gallery-id="clientConfig.galleryId">`
- Page passes `clientConfig` with the location's gallery ID: `CmsPartialSsr(:mixin-key="..." :config="{ galleryId: location.dashHudsonGalleryId }")`
- **This requires `dashHudsonGalleryId` on the location object** (Tophat field) — but only one partial to manage
- Same pattern as Andris's `clientConfig.bookingUrl` fix (session decision #40)

**A0 — Simplest possible (zero config, matches V1 exactly):**
- Use `DashHudsonScript` directly (not `DashHudsonWidget`) — brand-wide product carousel
- No CMS partial, no gallery ID, no Tophat changes — just import and render
- Every location shows the same brand-wide DashHudson content
- This is literally what V1 does on `HcbIndividual.vue` and `HcbLocationPage.vue`

##### 6.10 Option A — Widget Plan (No API, No Backend, No Auth)

**When to use:** If API access is unavailable, if the team wants a fast MVP, or if DashHudson-controlled rendering is acceptable.

**Key difference from Option B:** DashHudson's JavaScript renders its own DOM inside the page. We do NOT get raw image URLs. Images do NOT merge into `galleryImages`. The widget is a **separate section** on the page, not part of the photo gallery or hero.

**What we get:**
- Per-location social/UGC photo grid on location pages
- Zero backend code
- Zero auth / API key / secureConfig
- Existing `DashHudsonWidget` component already works — just pass `galleryId`

**What we DON'T get:**
- Images in the photos page masonry grid
- Images in the hero "+X photos" count
- Control over image order, sizing, aspect ratios
- Ability to deduplicate against CMS/DB images
- SSR rendering (widget is always client-side)

###### 6.10.1 Sub-Options Summary

| Sub-Option | Gallery Scope | Tophat Changes | Code Changes | Prerequisites |
|---|---|---|---|---|
| **A0** | Brand-wide (same for all locations) | None | 1 file (~10 lines) | None |
| **A1** | Brand-wide (same gallery, grid layout) | Create 1 CMS partial | 1 file (~15 lines) | 1 Tophat partial |
| **A2** | Per-location (different gallery per location) | Create N CMS partials + 1 `cmsSettings` field | 1 file (~20 lines) | N partials + Tophat page config |
| **A3** | Per-location (one partial + `clientConfig`) | Create 1 CMS partial + 1 location field | 1 file (~20 lines) | 1 partial + Tophat location field |

**Recommendation:** Start with **A0** (zero config, 10 lines) to validate the section placement and UX. Then upgrade to **A2** or **A3** for per-location galleries once Marketing creates location-specific galleries in Dash Social.

###### 6.10.2 Prerequisites by Sub-Option

**A0 (brand-wide, matches V1):**
- None. Zero Tophat changes. Zero Dash Social changes.

**A1 (brand-wide, grid layout via CMS Partial):**
- Create 1 CMS partial in Tophat with `mixin-key="partial-dashhudson-gallery"`. HTML: `<dash-hudson-widget script-type="board-embed" gallery-id="XXXXX">`. Get any gallery ID from Dash Social admin.

**A2 (per-location via N CMS partials):**
- Create a gallery per location in Dash Social admin → get `gallery_id` per location.
- Create a CMS partial per location in Tophat, each with its gallery ID.
- Add a `cmsSettings` field (like `marketingPartialLight`) that points to the partial mixin-key per location page.

**A3 (per-location via `clientConfig`):**
- Create a gallery per location in Dash Social admin → get `gallery_id` per location.
- Create 1 shared CMS partial in Tophat. HTML uses `:gallery-id="clientConfig.galleryId"`.
- Add `dashHudsonGalleryId` field to location data model in Tophat.
- Populate gallery IDs per location.

**NOT needed for any Option A sub-option:**
- ~~Dash Social API plan tier~~ — widget is free
- ~~Bearer token~~ — no auth
- ~~secureConfig / AWS SSM~~ — no server-side calls
- ~~Redis caching~~ — nothing to cache
- ~~New backend files~~ — zero `mr_modules/` changes

###### 6.10.3 Architecture — Where the Widget Goes

**V1 reference** — both V1 pages place `DashHudsonScript` (brand-wide carousel) in the main content flow:
```pug
//- HcbLocationPage.vue (V1) — between Reviews and FAQs:
.row.xs-mb-200m
  DashHudsonScript
    template(#title="")
      h3.bold.upper.xs-f-small.md-f-small.lg-f-large.max-at-tweak.text-center Real clients, really gorgeous results
```

**V2 placement** — add to `HcbLocationSections.vue`, after Reviews and before FAQs (same position as V1). Uses the globally registered `dash-hudson-widget` (which is `DashHudsonWidget.vue`) — NOT `DashHudsonScript/DashHudsonScriptInner` (that's the brand-wide carousel).

```
V2 layout with Option A:

.main-column
  ├── HairColorBarLocationAbout
  ├── CMSPartial (marketingPartialLight)
  ├── HairColorBarLocationServices
  ├── CMSPartial (marketingPartialDark)
  ├── .getting-here-section
  ├── .payments-section
  ├── HairColorBarLocationReviews
  ├── .dashhudson-section ← NEW (Option A widget, between Reviews and FAQs)
  └── HairColorBarLocationFAQs
```

**Why between Reviews and FAQs:**
- Matches V1 position (DashHudsonScript sits between Reviews and FAQs)
- Social photos are "social proof" — logically groups with reviews
- FAQs + footer components (MoreLocations, RegionList, MoreInfo) are page-end content

###### 6.10.4 Implementation — All Sub-Options

---

**SUB-OPTION A0: Brand-Wide (Matches V1 Exactly)**

Zero Tophat changes. Import `DashHudsonScript` (the same component V1 uses). Shows brand-wide product carousel with `brand_id=18947`.

Template addition in `HcbLocationSections.vue` (between Reviews and FAQs):
```pug
      HairColorBarLocationReviews

      .dashhudson-section.bottom-divider-light.xs-py-150m(role="region" aria-labelledby="dashhudson-section-title")
        h2#dashhudson-section-title.color-mr-purple.f-secondary.xs-mb-25m.sm-f-xxlarge.max-at-tweak.upper Real clients, really gorgeous results
        DashHudsonScript

      HairColorBarLocationFAQs(:faqs="faqsList" :title="faqsTitle")
```

Script changes:
```javascript
// Import (add to imports):
import DashHudsonScript from '@components/PDP/DashHudsonScript.vue';

// Components (add to components):
DashHudsonScript,
```

**Files modified: 1** (`HcbLocationSections.vue` — ~10 lines: template + import + component registration)
**Tophat changes: 0**
**Per-location content: NO** — same brand-wide carousel everywhere, identical to V1

---

**SUB-OPTION A1: Brand-Wide Grid via CMS Partial**

One CMS partial in Tophat. Shows a specific gallery in grid layout (not product carousel).

Tophat partial (created in Tophat admin, `mixin-key="partial-dashhudson-gallery"`):
```html
<dash-hudson-widget script-type="board-embed" gallery-id="XXXXX" row-size="4" gap-size="4" mobile-row-size="2" mobile-gap-size="2" call-to-action="book_now"></dash-hudson-widget>
```

Template addition in `HcbLocationSections.vue`:
```pug
      HairColorBarLocationReviews

      .dashhudson-section.bottom-divider-light.xs-py-150m(role="region" aria-labelledby="dashhudson-section-title")
        h2#dashhudson-section-title.color-mr-purple.f-secondary.xs-mb-25m.sm-f-xxlarge.max-at-tweak.upper Real clients, really gorgeous results
        CMSPartial(mixin-key="partial-dashhudson-gallery")

      HairColorBarLocationFAQs(:faqs="faqsList" :title="faqsTitle")
```

No new imports — `CMSPartial` is already imported in Sections. `dash-hudson-widget` is already globally registered.

**Files modified: 1** (`HcbLocationSections.vue` — ~8 lines: template only)
**Tophat changes: 1** (create partial)
**Per-location content: NO** — same gallery for all locations

---

**SUB-OPTION A2: Per-Location via CMS Partials (Same Pattern as Marketing Modules)**

One `cmsSettings` field + N CMS partials. Each location points to its own partial with its own `gallery-id`.

Tophat setup:
- Add `cmsSettings.dashHudsonPartial` field (same type as `marketingPartialLight` — object with `cms_partial` string)
- Create a partial per location: `partial-dashhudson-nyc-flat`, `partial-dashhudson-beverly-hills`, etc.
- Each partial's HTML: `<dash-hudson-widget script-type="board-embed" gallery-id="{location-specific-id}" ...>`
- Each location's Tophat page config sets `dashHudsonPartial.cms_partial` to its partial mixin-key

Template addition in `HcbLocationSections.vue`:
```pug
      HairColorBarLocationReviews

      .dashhudson-section.bottom-divider-light.xs-py-150m(v-if="dashHudsonPartial" role="region" aria-labelledby="dashhudson-section-title")
        h2#dashhudson-section-title.color-mr-purple.f-secondary.xs-mb-25m.sm-f-xxlarge.max-at-tweak.upper Real clients, really gorgeous results
        CMSPartial(:mixin-key="dashHudsonPartial")

      HairColorBarLocationFAQs(:faqs="faqsList" :title="faqsTitle")
```

Computed addition:
```javascript
dashHudsonPartial() {
  return this.cmsSettings?.dashHudsonPartial?.cms_partial || '';
},
```

**Files modified: 1** (`HcbLocationSections.vue` — ~12 lines: template + computed)
**Tophat changes: N+1** (1 field + N partials, one per location)
**Per-location content: YES** — each location has its own gallery

---

**SUB-OPTION A3: Per-Location via `CmsPartialSsr` + `clientConfig`**

One CMS partial + one location field. Gallery ID passed via `clientConfig` (same pattern as Andris's booking URL fix, session decision #40).

Tophat setup:
- Create 1 shared partial: `partial-dashhudson-location-gallery`. HTML: `<dash-hudson-widget script-type="board-embed" :gallery-id="clientConfig.galleryId" row-size="4" gap-size="4" mobile-row-size="2" mobile-gap-size="2" call-to-action="book_now">`
- Add `dashHudsonGalleryId` field to location data model (String, nullable)
- Populate gallery IDs per location

Template addition in `HcbLocationSections.vue`:
```pug
      HairColorBarLocationReviews

      .dashhudson-section.bottom-divider-light.xs-py-150m(v-if="location.dashHudsonGalleryId" role="region" aria-labelledby="dashhudson-section-title")
        h2#dashhudson-section-title.color-mr-purple.f-secondary.xs-mb-25m.sm-f-xxlarge.max-at-tweak.upper Real clients, really gorgeous results
        CmsPartialSsr(mixin-key="partial-dashhudson-location-gallery" :config="{ galleryId: location.dashHudsonGalleryId }")

      HairColorBarLocationFAQs(:faqs="faqsList" :title="faqsTitle")
```

No new imports — `CmsPartialSsr` is globally registered (auto-stubbed in tests).

**Files modified: 1** (`HcbLocationSections.vue` — ~10 lines: template only)
**Tophat changes: 2** (1 partial + 1 location field with per-location values)
**Per-location content: YES** — each location has its own gallery, but only 1 partial to manage

---

**Design decisions shared across all sub-options:**
- **Placement:** Between Reviews and FAQs — matches V1 position, groups with social proof
- **Self-contained landmark** (SS1.5): `role="region"` + `aria-labelledby="dashhudson-section-title"` + `h2#dashhudson-section-title`
- **Section title style:** `.color-mr-purple.f-secondary.sm-f-xxlarge.max-at-tweak.upper` — matches Getting Here, Payments headings (SS1.16)
- **Title text:** "Real clients, really gorgeous results" — matches V1 exactly (`HcbIndividual.vue` L20, `HcbLocationPage.vue` L17)
- **`.bottom-divider-light.xs-py-150m`** — self-sufficient spacing (SS1.11)
- **`v-if` guard:** Section hidden when no gallery data configured. Zero impact on unconfigured locations.

###### 6.10.5 Complete File Manifest (Option A — all sub-options)

**Modified files: 1**

| File | Change | Lines Affected |
|---|---|---|
| `HcbLocationSections.vue` | Template: add `.dashhudson-section` with `dash-hudson-widget`. Methods: add `handleDashHudsonMounted`. Style: add overflow guard. | ~20 lines |

**New files: 0**

**Backend files: 0**

**Infrastructure (non-code): 2**

| Item | Change | Owner |
|---|---|---|
| Tophat location config | Add `dashHudsonGalleryId` field (String, nullable) | Tophat team |
| Dash Social admin | Create per-location galleries, curate photos | Marketing |

###### 6.10.6 What Does NOT Change (Option A — any sub-option)

- `galleryImages` computed — untouched. DashHudson images are NOT in the gallery array.
- `HcbLocationPhotosPage` — untouched. Photos page masonry shows CMS + DB images only.
- `HairColorBarLocationHeroV2` — untouched. Hero "+X photos" count reflects CMS + DB only.
- `colorbar.js` Vuex store — no new state, actions, or mutations.
- `vueColorbarSvc.js` — no new service functions.
- `mr_modules/` — zero backend changes.
- `mrVueApp.js` / `registerGlobalsSsr.js` — `dash-hudson-widget` is already globally registered.

###### 6.10.7 Testing (Option A)

| Test | File | What to Assert |
|---|---|---|
| Section renders when `dashHudsonGalleryId` exists | `HcbLocationSections.test.js` | Mount with `location: { dashHudsonGalleryId: '12345' }` → `.dashhudson-section` exists. `dash-hudson-widget` stub exists with correct props (`script-type`, `gallery-id`). |
| Section hidden when `dashHudsonGalleryId` is falsy | `HcbLocationSections.test.js` | Mount with `location: { }` → `.dashhudson-section` does not exist. |
| Section hidden when `dashHudsonGalleryId` is empty string | `HcbLocationSections.test.js` | Mount with `location: { dashHudsonGalleryId: '' }` → `.dashhudson-section` does not exist. |
| Heading text matches V1 | `HcbLocationSections.test.js` | Assert `h2#dashhudson-section-title` text is "Real clients, really gorgeous results". |
| Tracking fires on widget mount | `HcbLocationSections.test.js` | Trigger `dashhudsonMounted` emit on stub → assert `trackMREvent` called with `'HCB Location Page - DashHudson Gallery Viewed'`. |
| ADA: landmark structure | `HcbLocationSections.test.js` | `.dashhudson-section` has `role="region"` + `aria-labelledby="dashhudson-section-title"`. `h2` has matching `id`. |

All tests use `shallowMount` — `dash-hudson-widget` is a stub (globally registered component is auto-stubbed). Assert `.exists()` and `.props()` on the stub, never its internal rendering (SS1.6 testing rules).

###### 6.10.8 Option A → Option B Upgrade Path

If Option A validates the concept and the team later wants images merged into `galleryImages`:

1. Remove `.dashhudson-section` from `HcbLocationSections.vue` template
2. Remove `handleDashHudsonMounted` method and scoped style
3. Implement Option B phases 1-8 (backend API → Vuex → `galleryImages` merge)
4. `dashHudsonGalleryId` field in Tophat stays — both options use it
5. Gallery curation in Dash Social stays — both options use same galleries

The Tophat field and Dash Social galleries are shared infrastructure. Switching from A→B is purely a code swap — no re-curation, no re-configuration.

###### 6.10.9 Decision: Option A vs Option B

| Criteria | Option A (Widget) | Option B (API) |
|---|---|---|
| **Time to implement** | 1-2 days (1 file changed) | 5-7 days (7 new + 6 modified files) |
| **Backend changes** | 0 | 7 new files + 3 modified |
| **Prerequisites** | 2 (Tophat field + galleries) | 6 (+ secureConfig + API token + plan confirmation) |
| **Auth/secrets needed** | None | Bearer token in AWS SSM |
| **Rendering control** | DashHudson controls DOM | Full custom (ImgBox, masonry) |
| **Merges into galleryImages** | No (separate section) | Yes |
| **In hero +X count** | No | Yes |
| **In photos page masonry** | No | Yes |
| **SSR** | No (client-side widget) | Yes (mounted() fetch, reactive merge) |
| **Upgrade difficulty** | Easy → Option B | N/A |

**Recommendation:** Start with Option A. Validate with 2-3 pilot locations. If stakeholders want tighter integration (masonry, hero count), upgrade to Option B. The Tophat field and Dash Social galleries carry over.

##### 6.11 Open Questions for PM/Stakeholders

1. **Does Madison Reed have API access on their Dash Social plan?** If not, Option B is impossible.
2. **Who curates location-specific galleries?** Marketing? Store managers? This determines operational feasibility.
3. **How many locations need galleries initially?** Pilot (2-3) or full rollout (~60+ HCBs)?
4. **Image count limit per gallery?** Recommend 30-50 max for performance.
5. **Is there an existing Dash Social gallery structure?** Check if MR already has location-specific boards/galleries created.
6. **Content rights:** Are UGC photos rights-cleared in Dash Social before they appear in galleries?
7. **Who has Dash Social admin access?** Need brand admin role to generate API token.
8. **Redis caching desired?** Birdeye doesn't cache reviews. DashHudson images change less frequently — 1hr TTL recommended but not required for initial launch.

<!-- Local Variables: -->
<!-- gptel-model: gemini-pro-paid -->
<!-- gptel--backend-name: "Gemini Local" -->
<!-- gptel--bounds: ((response (40 13827))) -->
