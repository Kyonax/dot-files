<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the DOTCOMPB-8120 (Marketing LP — Hero Section) session. It is loaded at the start of every conversation to give the AI full context without re-discovering anything.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, patterns, conventions for ALL work in this session. | Before any code task. |
| **2. Session Overview** | Scope, decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-feature detail: files, trees, decisions, tests. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference file path table. | When reading, editing, or locating files. |
| **5. Last Interaction** | Short-term memory: last work, pending, resume points. | At conversation start. |
| **6. Activity Log** | Datetime-stamped, append-only audit trail of every meaningful event. | When you need exact "what was done when". |

**Operational Rule:** Always look for the last request identified by `###` title. Load relevant skills and apply Section 1 rules.

**Architectural baseline:** This session **inherits** from `site-revolution-redesign.md`. That session is the pattern catalog for the entire HCB / Site Revolution program — Section 1 below pulls only the patterns relevant to the Marketing LP Hero. For anything not covered here, fall back to `site-revolution-redesign.md` (Sections 1.1 — 1.18) and `site-revolution-architecture.md`.

**Cross-session references** use `[session: site-revolution-redesign > section-N.M]` syntax — see `~/.claude/skills/session-memory/rules/reference-syntax.md`.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `mr-dotcom-dev` (Vue/Vuex/Pug/Stylus), `mr-style` (design system classes), `code-review` (quality analysis), `mr-roam-node` (ticket documentation), `pr-scribe` (PR body authoring).
>
> Patterns marked **(inherited)** are abstracted from `site-revolution-redesign.md` — the cited section is authoritative; this entry is a precis tuned for the Marketing LP hero.

### 1.1 Framework & API (inherited)

*   **Vue 3 — Options API only** (`export default { ... }`). No Composition API, no `<script setup>` for V2 page-level components. (See `[session: site-revolution-redesign > §1.1]`.)
*   **Templating:** Pug (`<template lang="pug">`).
*   **Styling:** Scoped Stylus (`<style lang="stylus" scoped>`).
*   **JS Syntax:** Always use curly braces for `if` statements, even single-line returns.
*   **Composables over mixins** when stateful logic must be shared. Never create new mixins.

### 1.2 Heading & Title Patterns (inherited — critical for hero)

The Marketing LP is a NEW page, so it owns its own `h1`. Apply these rules verbatim:

*   **Single H1 per page** — the hero owns it; all other sections (`HairColorBarLocationAbout`, `Reviews`, etc., if any are imported) use `h2`. Never let a section component "steal" the page title. (See `[session: site-revolution-redesign > §1.17 andris-guideline-6]`.)
*   **`.upper` is MANDATORY on every `.f-secondary` heading** — Kapra Neue is uppercase by design. Pattern:
    ```pug
    h1#hero-section-title.color-mr-purple.f-secondary.sm-f-xxlarge.max-at-tweak.upper Find a Hair Color Bar Near You
    ```
*   **`.max-at-tweak` is MANDATORY** on every responsive font class (`.xs-f-*`, `.sm-f-*`, `.md-f-*`, `.lg-f-*`, `.xl-f-*`, `.font-*`). It caps flex-sizing so fonts don't grow infinitely at large viewports.
*   **Heading inline text format** — write `h1.classes Title` not `h1.classes` followed by `| Title` on a new line. Dynamic interpolation works inline: `h1.upper Hello, {{ user.name }}`.
*   **Section title utility class consolidation (decision #37):** When an element exceeds **4 utility classes**, move font-family/text-transform/color into the scoped class (`.section-title`). Keep only responsive font-size classes as utilities. (For the Marketing LP hero, the H1 will likely have 5+ utilities — plan to consolidate from the start.)
*   **Delegated heading via `titleId` prop** — when the heading lives inside a child like `PageIntro`, pass an ID through:
    ```pug
    PageIntro(:title="cmsTitle" title-id="hero-section-title")
    //- PageIntro renders: h1(:id="titleId") {{ title }}
    ```

### 1.3 Utility-First Styling (inherited)

*   **Utility classes first**, Stylus only for what utilities can't express. Aggressively consolidate common classes on parent elements (DRY). (See `[session: site-revolution-redesign > §1.2]`.)
*   **Utility class replacement rule** — every CSS property with a utility equivalent MUST be expressed as a utility in the template. Examples: `display flex` → `.flex`, `width 100%` → `.full-width`, `margin 0 auto` → `.div-center`, `font-weight 600` → `.semi-bold`.
*   **Font sizes / families / colors live ONLY in the template** as utility classes — never in `<style>` blocks. Use `.xs-f-small`, `.bold`, `.text-color-2`, `.color-mr-purple`.
*   **Responsive padding/margin via utility prefixes** — `.px-100m.xl-px-400m`, never `@media` rules in Stylus for spacing.
*   **Padding/margin utility classes MUST use breakpoint prefix** — `.xs-pt-50m` not `.pt-50m`. Mobile-first convention. (Decision #68.)
*   **Class ordering:** alphabetical in Pug, except structural/positioning classes which may precede for clarity.
*   **CSS property ordering:** alphabetize within `<style>` blocks.
*   **`px` only for small fixed elements** (borders, box-shadows). Default to `rem`/`em`.
*   **Design system variables only** — `brand-color-*`, `cta-color-*`, `text-color-*`, `ui-color-*`. Never hardcode hex unless no exact variable exists (intentional examples: `#EFEFF1`, `#9A8CAD` — see decisions #17, #30).

### 1.4 Breakpoint Strategy (inherited)

*   **Centralized breakpoint via `global/isDesktop`** — for show/hide logic at 960px+, use `mapGetters('global', ['isDesktop'])` from the Vuex `global` module (provided by `mainAppMixin`, throttled). (See `[session: site-revolution-redesign > §1.4]`.)
*   **Local `matchMedia`** only when the breakpoint differs from the global getter (e.g., the mobile sticky CTA may need a non-960px breakpoint). Store the `MediaQueryList` in `data` (SSR-safe), add listener in `mounted`, remove in `beforeUnmount`.
*   **`window.resize` is forbidden** for responsive logic.
*   **Utility class breakpoints:** `xs-` mobile (default), `sm-` 560px+, `md-` 760px+, `lg-` 960px+, `xl-` 1200px+.
*   **Mobile sticky CTA uses `100dvh`** (Dynamic Viewport Height), not `100vh`. iOS Safari with the URL bar visible miscomputes `100vh`. Avoid `env(safe-area-inset-bottom)` padding hacks unless `viewport-fit=cover` is in the meta tag (it is not — they evaluate to `0px`). (Decision #74 / #78.)

### 1.5 Accessibility (inherited — critical for hero)

*   **Self-contained landmarks** — the hero owns `role="region"` (or omit if hero is wrapped in `<main>`), `aria-labelledby="hero-section-title"`, and the `h1#hero-section-title` in its own template. Parent wrappers are purely structural with NO ARIA attributes. (Decisions #1, #8; `[session: site-revolution-redesign > §1.5]`.)
*   **`aria-labelledby` references heading IDs, never root element IDs** — root containers use `class` selectors, not IDs. IDs are reserved for headings.
*   **No redundant class when ID exists** — `h1#hero-section-title.color-mr-purple` not `h1#hero-section-title.hero-title.color-mr-purple`.
*   **Native semantics** — never use raw `<button>` for navigation. Use `MrBtn` for true button interactions; `<a>` with `:href` + `@click.prevent` for tracked navigation. (Decisions #9, #19.)
*   **No nested interactives** — never `<a>` inside `role="button"`. The location search input must NOT be wrapped in a link.
*   **`role="link"` for multi-content interactive containers** — if the nearby-location card has multiple text elements, use `<div role="link" tabindex="0" @keydown.enter.prevent>` not native `<a>` (WCAG 2.5.3 Label in Name). (Decision #80.)
*   **`aria-expanded` on toggle buttons** uses `:aria-expanded="!!stateVar"` so the attribute always renders.
*   **Dynamic `aria-label` for repeated CTAs** — the offer CTA, primary CTA, and sticky CTA each need distinct accessible names.
*   **Live region for nearby-location appearance** — when the nearby-location section appears after geolocation resolves, use the `filtersEverUsed` flag pattern (decision #82) so it doesn't fire on initial mount with cached state.
*   **Live region placement: outside `v-if`** — render `.hiddenButPresent(aria-live)` from mount, not inside the conditional that wraps the rest of the hero. (Decision #83.)
*   **Skip-link target** — the hero is the page's primary content. If `<main>` already exists at the page level, the hero need not duplicate the landmark.

### 1.6 Form / Input Patterns (new for this ticket)

The Marketing LP hero has a **location search input** — patterns for this specifically are NOT in the parent session. Apply:

*   **Input wrapped in a `<form>`** so Enter submits, even when the user has not clicked the search button.
*   **Visible label OR `aria-label`** on the input — placeholder is not a label.
*   **`autocomplete="postal-code"`** when the input accepts a ZIP, `autocomplete="street-address"` when accepting a full address. Determine which the hero supports from Figma + product before coding.
*   **`inputmode="numeric"`** for ZIP-only inputs, mobile UX win.
*   **Pre-population on `/colorbar/locations`** — the destination page must read place data from URL query params. **Hero now sends a rich payload when the user picks a Google prediction:** `?search=<formatted_address>&lat=<>&lng=<>&placeId=<>`. Free-typed input falls back to `?search=<text>` only. **CRITICAL — receive-side reader does NOT exist yet** (`ColorBarLocationSectionV1.vue` mount-time read is the paired change owed). Until that lands, the map on `/colorbar/locations` will NOT show the picked colorbar — destination page falls through to its own IP/customer/geo resolution.
*   **Search input is forward-only — must NOT mutate sibling state.** `place_changed` captures place data into LOCAL component data only (`selectedPlace`); never dispatch `getClosestLocationsByLatLong` or any other Vuex action that would update `closestLocations` for sibling sections. Sibling sections derive their location from the wrapper's IP/initializeBopis chain, not from the hero search.

### 1.7 CMS Configuration (inherited)

*   **CMS-driven content via `cmsSettings` props** — page title, hero image, offer copy, promo ID, primary CTA text/destination all come from CMS. Apply `v-if` guards everywhere CMS data may be empty.
*   **CMS image stripping** — when CMS media URLs have baked-in dimension params (`?w=400&h=300&fit=crop`), strip with `url.split('?')[0]` in the parent's computed before passing to `ImgBox`. (See `[session: site-revolution-redesign > §1.8]`.)
*   **`ImgBox` for all images** — never raw `<img>`. `ImgBox` reads `mediaObject.alt_text` automatically. For decorative images, pass `alt=""`.
*   **Skeleton via `:deep(.image-box)` background** — `background-color ui-color-4` + `border-radius` + `height 100%` + `width 100%` on `:deep(.image-box)`. The gray placeholder shows while the image downloads; the image covers it when loaded.
*   **Offer callout = CMS partial OR inline component** — investigate which pattern the design implies. If offer rendering is dynamic per location, this is a partial; if it's static per LP variant, inline is fine.
*   **Toast on offer-applied** — verify whether the existing toast system (search `mr_modules/cms/lib/` and `vuescripts/store/modules/global.js`) is reusable. Do NOT build a new toast component without evidence.
*   **`white-space: pre-wrap`** preserves `\n` in CMS textarea content (offer description, etc.).

### 1.8 Tracking (inherited)

*   **`trackMREvent(eventName, properties)`** — fire-and-forget. Use when the user **stays on the current page** (e.g., experiment-viewed event on hero mount, sticky CTA click that opens a modal).
*   **`trackMREventAndRedirect(eventName, url, properties)`** — track then navigate with 300ms delay. Use for **hard redirects** (`location.href`). Examples: primary CTA → booking flow, offer CTA → locations page, nearby-location click → location detail.
*   **Anti-pattern:** never `trackMREvent()` + `goToPath()` sequentially — `goToPath` does `location.href` immediately, the event may not flush. (Decision #18.)
*   **Page load events:** `watch` with `immediate: true` — perfect for the experiment-viewed event on test bucket.
*   **Do NOT pass `isFrontEndEvent: true`** — `segmentTracking.js` adds it automatically. (Decision #72; see also `.claude/rules/coding-standards.md`.)
*   **Track experiment exposure explicitly** — branching logic alone is not enough. Fire `trackMREvent` in `mounted` (or watcher with `immediate: true`) when the test bucket renders. (`andris-guideline-13`.)
*   **Required events** (from JIRA AC — see roam node):
    | Event Name | Trigger | Required Properties |
    |---|---|---|
    | `MREvent (Marketing LP – Offer clicked)` | Offer callout CTA click | `promoCode`, `promoName`, `eventName` |
    | `MREvent (Marketing LP – Primary CTA clicked)` | Primary CTA in hero | `ctaText`, `ctaDestination`, `eventName` |
    | `MREvent (Marketing LP – Sticky CTA clicked)` | Sticky mobile CTA | `eventName` |
    | `MREvent (Marketing LP – Nearby location clicked)` | Click on nearby-location card | `locationCode`, `locationName`, `eventName` |

### 1.9 Experiments (inherited)

*   **Experiment-gated rendering inside the existing component** — DO NOT create a new Vue Router route for the test bucket. Add a `v-if` inside the existing component (or a `Splitter`-style parent) that checks the experiment flag and renders the new component. (Decision #75.)
*   **Experiment-gated components have no SSR** — `this.experiments` is `{}` during SSR and only populated in `mounted` via `window.experiments`. The new test-bucket hero only renders after client `mounted()` — `serverPrefetch` never runs on it. Data loading must be in `created` only. A brief V1→V2 flash is the established pattern across all experiment splitters. (Decision #76.)
*   **Experiment-viewed event fires from the test variant** in `mounted` (or watcher with `immediate: true`) — see §1.8.
*   **Cookie-sticky for 30 days** — see `docs/features/experiments.md` and `.claude/rules/experiments.md` for the SSR-time allocation logic. The hero only consumes the bucket; allocation happens upstream.

### 1.10 Component Placement & Naming (inherited)

*   **Self-explanatory, general names** — `MarketingLpHero` not `MarketingLandingPageLocationSpecificHeroSection`. Names should allow reuse across the site. (`[session: site-revolution-redesign > §1.14]`.)
*   **Location by domain, not by page:**
    *   Reusable booking-related components → `HairColorBarBookingV2/components/` (where `PageIntro`, `FixedCtaBar` already live).
    *   Page-section wrappers (truly page-specific) → page-specific folder (e.g., `MarketingLpLocationSpecific/components/`).
    *   The Marketing LP hero itself is a page section → likely `MarketingLpLocationSpecific/components/Hero/Hero.vue` (or whatever the parent page folder is).
*   **Folder structure:** `ComponentName/ComponentName.vue` + `ComponentName/index.js` (barrel export). Tests co-located as `ComponentName.test.js`.
*   **Short root CSS class** — kebab-case based on component name. The HCB family uses `.hcb-` prefix; this Marketing LP family will likely use `.mlp-` (decide on first PR and stick to it).
*   **Sticky CTA reuse** — `FixedCtaBar` already exists. Audit it before building anything new for AC9.
*   **Self-sufficient component spacing** — the hero owns its own `py-150m` (or appropriate utility). The parent page does NOT add wrapper divs with spacing classes. (Decision #24.)

### 1.11 Testing (inherited)

*   **Run:** `cd website && npm run test:vue {component_name}` (e.g., `npm run test:vue MarketingLpHero`).
*   **No snapshot tests.** Forbidden.
*   **`shallowMount` by default.** Children are stubbed.
*   **matchMedia mocking:** `vi.stubGlobal('matchMedia', vi.fn().mockReturnValue(mockMediaQueryObject))` with `addEventListener`/`removeEventListener` spies.
*   **Store mocking:** `createMockStore(state, isDesktop)` pattern.
*   **Mock globals BEFORE import** — module-level code runs at import time.
*   **Emit before redirect** — when testing `trackMREventAndRedirect`, verify `$emit` (or the call) fires first; the redirect may navigate away.
*   **Test files: `import { vi } from 'vitest'` explicitly** for new code. PilkoLint resolves ESLint config differently than local CLI and flags missing `vi`. (Decision #81.)
*   **Run all Vue tests before PR:** `cd website && npm run test:vue`.

### 1.12 PR Workflow

*   **Skill:** `/create-pr` (auto-fetches JIRA, drafts summary, applies labels).
*   **Directive:** `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/ticket-pr-template.md`
*   **PR Title format:** `[DOTCOMPB-8120]: Hero Section — Marketing Landing Page (Site Revolution HCB LP)`
*   **Labels:** `DOTCOM TEAM`, `Pending Code Review`.
*   **Changes section:** sub-list format (one detail per line).
*   **Unit Testing table:** one row per individual test case.
*   **Branch name:** `DOTCOMPB-8120` (matches ticket key).

### 1.13 Code Review Checklist

> The 45-rule checklist in `[session: site-revolution-redesign > §1.18]` applies here as-is. Run `/code-review` against every component on this branch before opening the PR. New rules to watch on this ticket specifically:
> - **Heading hierarchy:** the page must have exactly one `h1` (the hero title). Verify in `/code-review` and via VoiceOver rotor.
> - **Live region placement:** the nearby-location announcement must use the `filtersEverUsed`-style flag pattern.
> - **Sticky CTA accessibility:** ensure the sticky CTA does not duplicate a focus target already in the in-flow CTA — use `aria-hidden="true"` on whichever is offscreen at the current viewport, OR conditionally render with `v-if="isMobile"`.

### 1.14 Utility-class rules (validated 2026-05-07 — memory takes precedence over skill on flex)

* **Background-color class pattern is `{color}-bg`, NOT `bg-{color}`.** `.brand-color-1-bg` (correct) vs `.bg-brand-1` (does not exist). Same rule for every numbered color family — `.cta-color-1-bg`, `.ui-color-3-bg`, `.feedback-color-2-bg`, etc. (See `/mr-dotcom-dev > rules/typography-utilities.md §6`.)
* **Flex layout stays in scoped Stylus** — per the `feedback_no_flex_utility.md` user memory (validated 2026-05-07): `display: flex`, `flex-direction`, `flex-grow`/`flex: N N 0`, `align-items`, `justify-content` all go in `<style scoped lang="stylus">`, NOT in `.flex`/`.flex-col`/`.align-center`/`.space-center`/`.flex-1` utility classes. The `/mr-dotcom-dev > rules/flexbox-layout.md` rule that says "all flexbox layouts must use utility classes" is **superseded by user preference** for this codebase. (Reason: Kyo treats flex as structural layout, paired with the other layout properties in Stylus.)
* **Non-flex utilities still apply.** `width: 100%` → `.full-width`; `overflow: hidden !important` → `.no-scroll`; padding/margin → em-based `.{prefix}-{p*,m*}-{val}m`; color/font/text-align via utility classes; breakpoint show/hide via `.xs-hide` / `.lg-only` / etc. Only the flex family is excluded.
* **Spacing utilities are em-based by convention.** `.{prefix}-{px,py,m*,p*}-{val}m` where `val/100 = em` (so `.px-100m` = `1em`, `.py-150m` = `1.5em`). No magic-number Stylus padding/margin.
* **Breakpoint mixin syntax is `@media mq-desktop-plus`.** Not `+mq-desktop()`. Available mixins: `mq-mobile`, `mq-mobile-plus`, `mq-tablet`, `mq-tablet-plus`, `mq-desktop-md`, `mq-desktop-md-plus`, `mq-desktop`, `mq-desktop-plus`, `mq-max`. (See `/mr-dotcom-dev > rules/typography-utilities.md §11`.)

### 1.15 Tophat field-naming convention (this template family)

* **Top-level fields on `location-specific-colorbar-v2`** (template _id 1650): camelCase. Existing siblings: `title`, `subtitle`, `colorbarTitle`. New top-level fields follow the same camelCase pattern (e.g., `heroSection`).
* **Top-level field name = matching Vue component name (camelCase first letter).** A Vue `HeroSection.vue` is configured under a Tophat field named `heroSection`. This makes it obvious which component owns which slice of `cmsSettings`.
* **Nested fields under `object` types** also camelCase (`heroSection.title`, `heroSection.image`).
* **Other templates may use PascalCase** (e.g., template 1625 `home-tabs-vs` uses `HomeVsTabs`, `ShowcaseCarousel`). Don't propagate that to this template — match the existing siblings.

### 1.16 Utility-class realities (corrects skill docs in places)

The actual utility-class inventory shipped in `website/src/styles/` is **narrower** than what `/mr-dotcom-dev` rules claim. Validated 2026-05-07. Memorise these or you'll burn iterations.

* **Background-color pattern is `{color}-bg`, NOT `bg-{color}`.** `.brand-color-1-bg`, `.ui-color-1-bg`, `.cta-color-1-bg`, `.feedback-color-2-bg`. **`.color-white-bg` does NOT exist** — the legacy `.color-white` is text-only. Use `.ui-color-1-bg` for white backgrounds.
* **No all-sides padding/margin shorthand.** `.p-{N}m` and `.m-{N}m` do **NOT** exist. Use `.px-{N}m` + `.py-{N}m` together (or directional `.pt-/pb-/pl-/pr-/mt-/mb-/ml-/mr-`). Em-based scale only: `0,10,15,25,30,50,75,100,125,150,175,200,225,250,275,300,350,400,450,500,600,700`.
* **`.gap-{N}` is auto-generated 1-30 in px** (`website/src/styles/layouts/flex.styl`). So `.gap-6`, `.gap-12`, `.gap-26` exist. Above 30 → use the named scale (`.gap-xs/sm/md/lg`).
* **Responsive `max-width-rel-{N}` does NOT ship** despite being in skill docs. Only unprefixed `.max-width-rel-{10..90}` exists. For breakpoint-scoped max-width, fall back to scoped Stylus: `@media mq-desktop-plus { max-width: 70% }`. (Skill docs `/mr-dotcom-dev > rules/spacing-utilities.md` claim `.lg-max-width-rel-70` exists — it does not. Pending skill-doc fix.)
* **Domaine Display Condensed = `.f-domaine-display-condensed`** (utility class). Sizes: `.{prefix}-f-xgrande` = 40px, `.{prefix}-f-poster` = 72px. `.max-at-tweak` mandatory on every responsive font class.
* **Primary Orchid is `.cta-color-1` (#911885)**, NOT `.brand-color-4` (which is the lighter "Bright orchid" #b666a9). The typography-utilities reference labels `.cta-color-1` as "Primary CTA" but the hex IS the primary orchid the design system calls out.

### 1.17 Vue scoped-CSS gotchas with child component classes

When you add a class to a child component element from a parent template (`MrBtn.my-class`), the rendered DOM has the class **on the same element as the child's root** (e.g., `<a class="mrbtn my-class">`). This produces two non-obvious bugs:

* **`.my-class :deep(.mrbtn) { … }` matches NOTHING** because `.mrbtn` is the same element as `.my-class`, not a descendant. Fix: put the override under a wrapper class that is a *true* parent (e.g., `.hero-nearest-location-container :deep(.mrbtn) { … }`). This mirrors the canonical Reviews-component pattern (`.hcb-reviews :deep(.mrbtn)`).
* **MrBtn's default `&:hover, &:active, &:focus` applies `setcolor(color-white)`** (sets `color: white` on those states). When you override `background-color` on hover, you MUST also explicitly re-set `color: cta-color-1` (or whatever you want) on `&:hover, &:active, &:focus` — otherwise text becomes white on the new bg and contrast breaks. The canonical Reviews override has the same gap; replicating it verbatim inherits the bug.

### 1.18 Layout stability — image cover-fit without flicker

A Vue page with a CMS image inside a flex container will load with **no fixed dimensions** by default, then jump to the natural intrinsic size when the image file arrives. Source of the jump: ImgBox's inline `aspect-ratio: <m>` style (from the chosen crop) plus `width: 100%` from imgBox.vue's own scoped Stylus → height is computed as `width × (1/aspect-ratio)` = potentially huge before load, then re-computed on load.

**Anti-flicker pattern (validated for this hero):**
1. Wrap image in `.media-column { position: relative }`.
2. Inside, `:deep(.image-box) { position: absolute; top:0; right:0; bottom:0; left:0 }` — detaches the image from the parent's height calculation.
3. On mobile (where stacked layout means the image row has no flex sibling driving its height), give the column a definite shape via `aspect-ratio: 95/69` (or your design ratio). Layout decision, not image-aspect modification.
4. On desktop, set `aspect-ratio: auto` and rely on flex `align-items: stretch` against the content sibling.
5. `min-width: 0` on flex children — without it, intrinsic content (image source pixels) blows the column out of its 50% target.

**Flex shorthand pitfall:** `flex: 1 1 0` (with `flex-basis: 0`) only works on desktop where the parent has flex distributing space. On a mobile column-reverse stack with absolutely-positioned children, `flex-basis: 0` collapses the row to 0px. Use `flex: 1 1 0` only inside the desktop media query; let mobile fall back to default `flex: 0 1 auto`.

### 1.19 Google Maps lazy-load for places autocomplete

`gmap-vue/v3` is configured with `dynamicLoad: true` in `mrVueApp.js:719`. The Maps API doesn't load on app boot — only when something **requests it through the plugin's tracking** via `this.$gmapApiPromiseLazy()`.

**Calling `googleMapsApiInitializer({...})` alone does NOT trigger the load** through the plugin's tracking — it configures the loader but `getGoogleMapsAPI()` (which `LocationSearchInput.initServices()` polls inside) keeps returning `null`. The V1 page works because its `LocationMapView` calls `$gmapApiPromiseLazy()`. Pages with a places autocomplete but **no map** must call it themselves.

**Canonical pattern (from `GuestCheckoutAddressInput.vue:404`):**
```js
async created() {
  if (import.meta.env.SSR) {
    return;
  }
  const config = this.$root.MRConfig || (typeof window !== 'undefined' && window?.MR?.config) || {};
  googleMapsApiInitializer({
    key: config.googleAutocomplete,
    libraries: 'places',
    v: 'weekly',
  }, false);                          // false = configure only, don't auto-load
  await this.$gmapApiPromiseLazy();   // trigger plugin-tracked lazy load
}
```

Test stubbing: `vi.mock('@gmap-vue/v3', () => ({ utilities: { googleMapsApiInitializer: vi.fn(), getGoogleMapsAPI: () => null } }))` + `mocks: { $gmapApiPromiseLazy: () => Promise.resolve({}) }` so the test `created` doesn't throw.

### 1.20 SVG icon authoring rules

Icons live in `website/src/assets/svg-icons/` and are auto-discovered by `getIconModules()` (`website/src/vuescripts/utilities/path.js:23`) via `import.meta.glob('@icons/*.svg')`. No manifest registration needed — drop in a `.svg` file and Vite picks it up.

Authoring constraints:
* **No fixed `width="..." height="..."` on `<svg>`** — keep `viewBox` only. Size comes from the consumer (e.g., `MrIcon`'s `height` prop).
* **`stroke="currentColor"` and/or `fill="currentColor"`** on the paths — never hardcode hex. The CSS `color` property of the parent then drives the icon color (e.g., `:deep(.search-icon) { color: brand-color-1 }`).
* **Reference: `map-marker-v2.svg`** added 2026-05-07 for the hero's location pin — sourced from Figma, transformed to currentColor stroke + viewBox-only. `map-marker-inverted.svg` is the closest pre-existing precedent (uses `fill="currentColor"` for filled icons). File optimized to 511 bytes (-41% from initial 864).

### 1.21 SVG load latency — dev-only artifact, not a bug

`MrIcon` icon load is **slow in `npm run dev` / `dev-ssr`**, fast in production builds. Three combined causes:

1. **`getIconModules()` returns 166 separate lazy chunks** — `import.meta.glob('@icons/*.svg')` creates one Vite chunk per icon. Each consumer pays one network round-trip.
2. **`vite-plugin-svgicon` has no transform cache** — `@yzfe/svgicon-gen` + SVGO runs *on every dev request* (`vite-plugin-svgicon/dist/index.mjs:60-71`). Cold transform: 50–200ms per icon.
3. **`MrIcon` defers loading to `mounted()`** (`MrIcon.vue:77-79`). Icon doesn't even *start* loading until after component mount → reads as "late painting" rather than "slow loading".

Cold-load cost in dev: 70–260ms per icon. Production: 5–20ms (chunks pre-baked at build time, SVGO runs once, served gzipped via HTTP/2). **Optimizing the source SVG bytes is wasted effort** — transform output dominates, not file size. If dev annoyance ever needs a real fix, the only lever is `getIconModules()` switching critical icons to `eager: true` (app-wide infra change).

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Implement the **Hero Section** of the new Marketing Landing Page (`/colorbar/location-specific`) under the parent epic *Site Revolution - HCB Landing Page* (DOTCOMPB-8119). The hero is CMS-configurable (title, image, optional offer callout), includes a location search input that pre-populates the locations page, conditionally surfaces a nearby-location section when geolocation places the user within 50 miles of a salon, and exposes a sticky mobile CTA. The page is gated behind an A/B experiment — control sees the existing in-production experience, test sees the new hero.

### 2.2 Scope

| Ticket | Type | Summary | Status |
|---|---|---|---|
| `DOTCOMPB-8119` | Epic | Site Revolution - HCB Landing Page | Open (parent). **Foundation in progress** — see PR #20750 below. |
| **PR [#20750](https://github.com/MadisonReed/mr/pull/20750)** | Foundation PR (epic-level) | "Site Revolution Marketing LP — V2 Foundation" — sets up the `feat-location-s` feature branch with the parent wrapper, experiment splitter, Vuex wiring, global registration, and Tophat migration tooling that **every child ticket on this branch will build on top of**. Variant A = today's CMS layout via `<slot/>` (byte-for-byte parity). Variant B = an empty V2 branch — child tickets fill it. | OPEN against `master`, github-actions APPROVED. |
| `DOTCOMPB-8120` | Story (3 SP) | Hero Section | **NOT STARTED.** This ticket's actual work (V2 hero block, location search, primary CTA, offer callout, nearby-location section, sticky mobile CTA, unit tests) lands on a future child branch off `feat-location-s` — separate from PR #20750. |
| Future children (TBD) | Story | Additional V2 sections | NOT STARTED. |

### 2.3 Open Questions — Resolved (audit pass 2026-05-07)

All seven of the original open questions were answered by inspecting the live CMS state (via `cms-migrate.mjs inspect` against `docker exec mr-mongo`) and grepping the codebase. Status of each:

1.  **Experiment name** — RESOLVED. `LocationSpecificSiteRevolution` (experiment doc `_id=504`, experimentId `177809681959624`). Status `Running`. Variants: `default` (variationId `…240`, weight 0) and `b` (variationId `…241`, weight 10000).
2.  **Search input contract (AC11)** — RESOLVED-WITH-BLOCKER. The actual current locations page is **`ColorBarLocationSectionV1.vue`** (NOT `ColorBarMapSection.vue` as DEV-AC10 lists — outdated). It uses the SAME `LocationSearchInput` we use in the hero, with `searchQuery` as its v-model data and `currentPlace` as the resolved Google Place. **Hero now sends a rich payload** on SEARCH click: `?search=<formatted_address>&lat=<>&lng=<>&placeId=<>` when a prediction was picked, or `?search=<text>` for free-typed input. **There is no `$route.query` reader today** in `ColorBarLocationSectionV1.vue` — paired change still owed. **BLOCKING: the map on `/colorbar/locations` does NOT show the picked colorbar** until the receive-side mounts read the URL params, prefill `searchQuery`, and hydrate `currentPlace = { position: { lat, lng }, formatted_address: search }` directly (skipping its own geocode round-trip).
3.  **Geolocation source (AC4/AC5)** — RESOLVED. Use `colorbar/initializeBopis` (already wired in our `mounted`). It performs the customer-address → `navigator.geolocation` → IP fallback chain. Reference precedent: `ColorBarLocationSectionV1.vue:281` (race-tolerant `locationSource: 'ip'|'browser'|'customer'`). **No new utility needed.**
4.  **50-mile distance (AC4 / DEV-AC6)** — RESOLVED. The `colorbar/getClosestLocationsByIp` action returns locations sorted by distance with a `distance` field. The hero shows the nearby section when `closestLocations[0]?.distance <= 50`. **No client-side Haversine needed.**
5.  **CMS offer schema (AC6/AC7/AC8)** — PARTIALLY RESOLVED. The CMS sub-template (template _id 1650) currently only carries `title`, `subtitle`, `colorbarTitle`. The proposed `heroImage`, `primaryCta.{text,destination}`, `offer.{copy,promoCode,promoName,ctaText,ctaDestination}` schema fields do **NOT** exist yet. Phase 2 must coordinate with Carley to add them to the sub-template settings before the hero can render CMS-driven offer content. Promo-application infra exists (`applyPromo.vue`, `BookingPromo.vue`, `ApplyPromoV2.vue`, `hairColorBarBooking.js:setPromoErrors`) — but in the booking flow, not on `/colorbar/locations`. AC8 redirect-with-applied-promo requires a paired `?promo=CODE` reader on the locations page.
6.  **Toast component (AC6)** — RESOLVED. **No toast primitive exists.** No `AppToast.vue`, no `showToast` action, no toast Vuex module. CartAndCheckout flows use the modal system instead. **Decision: reuse the modal system** — dispatch `modal/showModal` with a lightweight bottom-modal theme for the "promo applied" confirmation rather than introducing a new toast component.
7.  **Sticky CTA (AC9)** — RESOLVED. **`FixedCtaBar` is reusable as-is.** Path `website/src/vuescripts/components/HairColorBarBookingV2/components/FixedCtaBar/FixedCtaBar.vue`. Props: `ctaText` (required), `redirectUrl`, `trackEventName`, `visible`, `ariaLabel`, `ctaDisabled`, `ctaLoading`. Emits `cta-click`. Uses `trackMREventAndRedirect` internally. Mobile-fixed via `position: fixed; bottom: 0` + slide-up `Transition`.

### 2.4 Key Decisions (Session-Wide)

1.  **(2026-05-06)** **Feature branch** `feat-location-s`. All DOTCOMPB-8119 epic tickets, including DOTCOMPB-8120, branch off this feature branch.
2.  **(2026-05-06)** **Existing `/colorbar/location-specific` was pure CMS** — no Vue page component existed. Globally-registered components inside Tophat partials.
3.  **(2026-05-06)** **New parent wrapper:** `LocationSpecificColorbarV2` at `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/`. Mirrors `HcbLocationPageV2` props (`cmsSettings`, `routeParams`) and Vuex wiring (`colorbar` store).
4.  **(2026-05-06)** **Experiment name:** `LocationSpecificSiteRevolution`. Variants A (control), B (V2). Tracking via `mix_trackExperimentViewed`. Convention follows `BookingFlowSiteRevolution` and `ShadeShopSiteRevolution`.
5.  **(2026-05-06)** **Splitter pattern:** internal `v-if` inside `LocationSpecificColorbarV2`. NO new Vue Router route. Mirrors `Shop/Splitter.vue` and the Booking calendar's `ExperimentSplitter.vue`.
6.  **(2026-05-06)** **SSR rule:** `this.experiments` is `{}` during SSR (populated client-side from `window.experiments`). The B variant only paints after `mounted()`. Brief V1→V2 flash is the established pattern. Data loading in `created` (with `import.meta.env.SSR` guard) and `serverPrefetch` only.
7.  **(2026-05-06)** **Location data flow:** `colorbar` Vuex module exposes `closestLocations`/`mapLocations` via `mapState`; `getClosestLocationsByIp` runs in `created` (SSR-guarded), `getActiveLocationsListForMapView` in `serverPrefetch` (try/catch resilient), `initializeBopis` in `mounted` (upgrade past IP via address → geolocation → IP fallback chain).
8.  **(2026-05-06)** **CMS migration shipped via `cms-migrate.mjs`** (local Docker only — production replication via Tophat UI). `content_id 3117` advanced from v54 (5 variations on experiment 475 "New Messaging Test July 2025") to **v55** (2 variations on experiment 504 `LocationSpecificSiteRevolution`). Old experiment 475 set to `Paused`; new experiment 504 `Running` with B at weight 10000. New sub-template `location-specific-colorbar-v2` (`_id=1650`) created with jade `location-specific-colorbar-v2(:cms-settings='!{JSON.stringify(settings)}')`. Backups under `.tasks/DOTCOMPB-8120/backups/<stamp>/snapshot.json`. Reproducible via `node cms-migrate.mjs <inspect|backup|migrate --confirm|restore <stamp>>`.
9.  **(2026-05-06)** **Foundation PR #20750 opened** — "Site Revolution Marketing LP — V2 Foundation" against `master` from `feat-location-s`. github-actions APPROVED. Codecov 2.08% patch coverage (expected — wrapper tests deferred to a child PR). **Critical framing:** this is the *epic-level* foundation that sets up the feature branch with the parent wrapper, splitter, Vuex wiring, and CMS migration. **It does NOT implement DOTCOMPB-8120** — the hero, offer callout, nearby-location section, sticky CTA, and the wrapper unit tests all land on separate child branches off `feat-location-s` and merge back into it.
10. **(2026-05-07 — audit pass)** **Toast = modal reuse.** No `AppToast` primitive in the codebase. Phase 2 promo-applied confirmation will dispatch `modal/showModal` with a bottom-modal theme rather than building a new toast component.
11. **(2026-05-07 — audit pass)** **`FixedCtaBar` reuse confirmed** for AC9. Drop-in component; no new sticky-CTA component needed.
12. **(2026-05-07 — audit pass)** **AC11 search prefill + AC8 promo apply require paired changes on `/colorbar/locations`** — out of scope for the hero PR. `ColorBarMapSection.vue` needs a `?search=` reader and a `?promo=` reader/stash. Schedule as separate sub-tickets or a coordinating commit.
13. **(2026-05-07 — audit pass)** **Actual CMS schema differs from §3.1 proposal.** Today's sub-template settings expose only `title`, `subtitle`, `colorbarTitle`. The proposed `heroImage`, `primaryCta.*`, `offer.*` fields require Tophat schema additions before the hero component can consume them.
14. **(2026-05-07)** **`tophat-tools` skill created** — standalone CMS-operations skill at `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/skills/tophat-tools/` (symlinked into `~/.claude/skills/`). Replaces every ad-hoc `docker exec mr-mongo mongosh cms --eval` query in this session with structured, JSON-emitting Node scripts. See §3.3 for full inventory.
15. **(2026-05-07)** **HeroSection plan locked** to a narrow Phase 1 scope per user direction: layout (2-col desktop / 2-row mobile, image-first DOM), Domaine Display Condensed H1, Tophat-driven `cmsSettings.heroSection.{title, image}`. Search input, offer callout, nearby section, sticky CTA, promo apply, and tracking events all parked until explicitly resumed. Full spec in §3.2.
16. **(2026-05-07)** **Tophat field name = `heroSection`** (NOT `hero`). Matches the corresponding Vue component name `HeroSection.vue` with leading-lowercase. Nested fields `heroSection.title` (text, default "Salon results without salon cost & time") and `heroSection.image` (`staticCroppedImage` — chosen because the layout does its own `object-fit: cover`; no responsive `srcset` needed).
17. **(2026-05-07)** **Test image asset for variant B = media `_id=7272`** "Madison Reed Hair Color Bar Interior" (DZ6_4801.jpg, 7072×4705, 3:2 landscape). Generic interior shot — no specific city — works for the national LP. Full payload in §3.2 / §4.
18. **(2026-05-07)** **Memory wins on flex utilities.** Conflict surfaced during reset between `feedback_no_flex_utility.md` (50-day-old user memory: "keep flex layout in Stylus, do not use `.flex` / `.flex-col`") and `/mr-dotcom-dev > rules/flexbox-layout.md` ("all flexbox layouts must use utility classes"). User chose memory wins for this codebase. §1.14 updated to flag this as a skill-vs-memory override; §3.2 Pug + Stylus updated to put `display: flex`, `flex-direction`, `flex: 1 1 0`, `align-items`, `justify-content` in scoped Stylus and only keep non-flex utilities (`.full-width`, `.no-scroll`, `.brand-color-1-bg`, spacing/typography) in the template.
19. **(2026-05-07)** **Schema lock-in (4 fields on heroSection).** Tophat template _id 1650 now exposes: `heroSection.title` (text, required), `heroSection.image` (staticCroppedImage, required, customCrops), `heroSection.searchHelperText` (text), `heroSection.nearestLocationLabel` (text). All four populated on contentVersion 19460 (variant B) for local validation. Carley replicates these schema additions in Tophat staging/prod when she does the production rollout.
20. **(2026-05-07)** **HeroSection v1 BUILT** with three vertical sections inside the dark purple column: H1 → search container (white card with helper text + LocationSearchInput + SEARCH button) → nearest-location card (white card with NEAREST LOCATION TO YOU label + name + address + BOOK A SERVICE button + image right). Right column is the hero image (full-bleed cover). Mobile: column-reverse stack (image first, content below). Desktop ≥960px: 50/50 row with content left. 21 → 36 tests across the wrapper + HeroSection.
21. **(2026-05-07)** **LocationSearchInput extended non-invasively.** Added two optional props `iconName` (default `'search-2'`) and `placeholder` (default `''`) + `resolvedPlaceholder` computed to `ColorBarLocationSectionV1/LocationSearchInput.vue`. Defaults preserve V1 page behavior verbatim — all 17 existing V1 tests still pass. HeroSection passes `iconName="map-marker-v2"` + `placeholder="Enter ZIP or City, State"` to override.
22. **(2026-05-07)** **New SVG icon `map-marker-v2.svg`** added to `website/src/assets/svg-icons/`. Sourced from Figma, modified to use `stroke="currentColor"` + viewBox-only. Auto-registered via `import.meta.glob('@icons/*.svg')`. Color set via CSS `color: brand-color-1` (HeroSection's `:deep(.search-icon)` override).
23. **(2026-05-07)** **Search input wiring fix — Google Maps lazy-load.** Initial `googleMapsApiInitializer` call alone wasn't kicking off Maps API loading because `gmap-vue` plugin uses `dynamicLoad: true`. Pattern from `GuestCheckoutAddressInput.vue:404` adopted: `googleMapsApiInitializer({...}, false)` + `await this.$gmapApiPromiseLazy()` in async `created()`. See §1.19.
24. **(2026-05-07)** **Wrapper max-width restored at design-system value.** `LocationSpecificColorbarV2.vue` scoped Stylus: `.location-specific-colorbar-v2 { margin: 0 auto; max-width: bp-desktop-large }` — `bp-desktop-large = 1440px` per `madison-reed/variables.styl:215`, the canonical desktop-large breakpoint variable used as the max-width cap. Hero is centered at viewports >1440px instead of left-aligned.
25. **(2026-05-07)** **No removal/TODO comments in code — user preference.** When parking work for later (e.g., the dev-fallback nearest-location), use searchable named constants (e.g., `USE_DEV_FALLBACK_LOCATION`) and document the cleanup task in the session file's pending work, not in code comments. The constants themselves are the discovery mechanism. (See §2.5 Phase 1 active list "REMOVE before PR — dev fallback".)
26. **(2026-05-07)** **Search input is forward-only — no sibling-state mutation.** `onPlaceChanged` previously dispatched `colorbar/getClosestLocationsByLatLong` (the wrong logic — search input was driving the page's nearest-location for siblings). Removed entirely. New behaviour: `onPlaceChanged` captures `{ placeId, latitude, longitude, address }` into the local `selectedPlace` data prop only. No Vuex writes. The page-wide closest location flows from the wrapper's IP/initializeBopis chain, not the search input.
27. **(2026-05-07)** **Search submit forwards rich URL params.** `onSearchSubmit` builds a `URLSearchParams` query: when `selectedPlace` exists (user picked a Google prediction), sends `?search=<address>&lat=<>&lng=<>&placeId=<>`. When only typed text exists, sends `?search=<text>` and the destination geocodes itself. Watcher on `searchQuery` clears `selectedPlace` if the user types past the picked text — staleness guard. Empty/whitespace input → no-op.
28. **(2026-05-07)** **50-mile gate live in `shouldShowNearestLocation`** — closes parked open decision #3 from §3.2. The hero card renders only when `closestLocations[0].distance <= 50` (boundary inclusive). Missing distance, distance > 50, missing label, or missing location → card hidden. Closes AC4/AC5 on the hero side.
29. **(2026-05-07)** **Parent wrapper dispatches `colorbar/loadLocation` for the closest within 50 mi.** New `watch: closestLocations` (immediate) on `LocationSpecificColorbarV2.vue` — when `closestLocations[0].distance <= 50` and a `code` exists, dispatches `loadLocation(code)` to populate `state.location`. This is what makes sibling sections (services, prices, region getters in `colorbar` store) reflect the user's nearest salon as THE page location. Mirrors `PdpEntry.vue:234` pattern. No closest, > 50 mi, missing distance, or missing code → wrapper dispatches nothing, leaves `state.location` empty.
30. **(2026-05-07)** **`NEARBY_RADIUS_MILES = 50` constant duplicated** in `HeroSection.vue` and `LocationSpecificColorbarV2.vue`. Known polish item — both places use the same gate so any future change must be made in two spots. Not extracted to a shared constants file (the DRY violation is small and pulling out a single number into a shared module adds more complexity than it saves).
31. **(2026-05-07)** **SVG load latency is dev-only, root cause identified.** `vite-plugin-svgicon` has no transform cache (re-runs `@yzfe/svgicon-gen` + SVGO every dev request); `getIconModules()` produces 166 separate chunks; `MrIcon` defers loading to `mounted()`. Cold-load 70–260ms per icon in dev, 5–20ms in prod. **Spent file-size optimization (864→511 bytes) didn't fix it because transform output dominates.** Production users will never see this. See §1.21.
32. **(2026-05-07)** **Spinner experimentation tried + reverted twice.** First attempt: bound `:loading="searchLoading"` on `LocationSearchInput` (built-in icon-swap to spinner-v2 + input disabled). Reverted because user wanted full-overlay UX. Second attempt: `MrSpinnerVeil(v-if="searchLoading")` overlay over `.search-card` with `position:relative`+`overflow:hidden`. Reverted entirely because the user redirected: search input must be forward-only (decision #26), no sibling-state mutation, no spinner UX needed. Final state: no spinner anywhere on the search input.

### 2.5 Pending Work

**Foundation (PR #20750) — DONE / not blocking child tickets:**

*   [x] Feature branch `feat-location-s` created
*   [x] Parent wrapper `LocationSpecificColorbarV2.vue` scaffolded + globally registered (client + SSR)
*   [x] Splitter logic + experiment-viewed tracking wired
*   [x] Vuex `colorbar` data flow wired (serverPrefetch / created / mounted)
*   [x] CMS migration script + local Docker migration applied (content_id 3117 v54 → v55, experiment 475 → 504)
*   [x] Carley confirmed Tophat-side ownership for production replication
*   [x] PR #20750 opened against `master`

**Production rollout (out of repo — Carley):**

*   [ ] Carley replicates the local CMS migration in Tophat staging then production (sub-template, experiment, content #3117 rebinding — three concrete steps in the PR's *Special Deployment Requirements*)
*   [ ] Wrapper unit tests (`LocationSpecificColorbarV2.test.js`) — deferred from the foundation PR; lands on a follow-up child branch (any of the DOTCOMPB-8119 children can carry it)
*   [ ] Tophat schema additions for the V2 hero — `cmsSettings.heroImage`, `cmsSettings.primaryCta.{text,destination}`, `cmsSettings.offer.{copy,promoCode,promoName,ctaText,ctaDestination}`. Coordinate with Carley before the hero ticket starts implementation.

**DOTCOMPB-8120 (Hero Section) — NOT STARTED. Lands on a child branch off `feat-location-s`:**

**Phase 1 — IMPLEMENTATION COMPLETE (locally validated, on `DOTCOMPB-8120` branch — uncommitted):**

*   [x] Tophat schema applied to template 1650 (4 fields: title, image, searchHelperText, nearestLocationLabel). Backups under `website/cms-backups/templateVersion/1650/<stamp>-v1.json`.
*   [x] Seeded all 4 fields on contentVersion 19460 (variant B).
*   [x] Branch `DOTCOMPB-8120` already exists, off `feat-location-s` at commit `edda6f76318`.
*   [x] Wrapper unit tests written (`LocationSpecificColorbarV2.test.js` — 17 tests, includes loadLocation watcher coverage and the deferred foundation owe).
*   [x] HeroSection component shipped: `HeroSection.vue` + `HeroSection.test.js` (30 tests) + `index.js`.
*   [x] HeroSection mounted in the wrapper's V2 `v-if` branch.
*   [x] Wrapper styling restored: `max-width: bp-desktop-large; margin: 0 auto`.
*   [x] LocationSearchInput extended with `iconName` + `placeholder` + `describedBy` props (V1 unaffected, all V1 tests pass).
*   [x] New `map-marker-v2.svg` icon added (511 bytes optimized).
*   [x] Search container styled + Google Maps lazy-load fix applied.
*   [x] Nearest-location card styled with 50-mile gate (decision #28).
*   [x] **Search input refactored to forward-only** (decision #26) — no sibling-state mutation. Local `selectedPlace` data captures place_changed payload.
*   [x] **Rich URL contract for search submit** (decision #27) — `?search=<address>&lat=<>&lng=<>&placeId=<>` when prediction picked, else `?search=<text>`.
*   [x] **Wrapper `loadLocation` watcher** (decision #29) — dispatches `colorbar/loadLocation(code)` when `closestLocations[0].distance <= 50` so siblings get `state.location` populated.
*   [x] Tests: 47/47 passing (30 hero + 17 wrapper). V1 LocationSearchInput tests still pass. ESLint clean.

**🚨 BLOCKING — paired change owed before PR demo will work end-to-end:**

*   [ ] **`/colorbar/locations` receive-side reader** — `ColorBarLocationSectionV1.vue:mounted` must read `$route.query.{search, lat, lng, placeId}`. Behaviour: prefill `searchQuery` data prop with `search`; if `lat` + `lng` present, hydrate `currentPlace = { position: { lat, lng }, formatted_address: search }` directly (skip the geocode round-trip); if only `search` present, geocode it. Without this paired change, **the map on `/colorbar/locations` does NOT show the picked colorbar** — destination falls through to its own IP/customer/geo resolution. Either bundled into this child PR (recommended — prevents demo confusion) or scheduled as a same-day follow-up sub-ticket.

**TO CLEANUP before PR:**

*   [ ] **`NEARBY_RADIUS_MILES = 50` duplicated** in `HeroSection.vue` and `LocationSpecificColorbarV2.vue` (decision #30). Acceptable to keep duplicated, but if extracted, target a shared `LocationSpecificColorbarV2/constants.js` so both files import the same source of truth.
*   [ ] **Skill-doc bug fix** (cross-cutting, separate ticket): `/mr-dotcom-dev > rules/spacing-utilities.md` claims `.{xs,sm,md,lg,xl}-max-width-{N}` and `-rel-{N}` exist; the actual `globals/utilities.styl` only ships unprefixed. Fix the doc.
*   [ ] **Confirm** with PM whether the BOOK A SERVICE button event name is correct (`MREvent (Marketing LP – Nearby location clicked)`). AC table fits — likely a no-op confirmation.

**Phase 2+ — parked (resume only on explicit user instruction):**

*   [ ] **Hero primary CTA** — separate from the BOOK A SERVICE button on the nearest-location card. Defer until user defines.
*   [ ] **Offer callout** — CMS-gated `v-if`. Requires adding `heroSection.offer.{copy,promoCode,promoName,ctaText,ctaDestination}` to Tophat schema. Click → bottom-modal "promo applied" + redirect.
*   [ ] **Paired `/colorbar/locations` `?promo=CODE` reader** for AC8.
*   [ ] **Sticky mobile CTA** — reuse `FixedCtaBar`. Hero already mounted but no sticky CTA wired.
*   [ ] **Remaining MREvent tracking calls** — Primary CTA, Sticky CTA, Offer click. Nearby Location is already wired.
*   [ ] **ADA pass** — `/code-review` ADA category + VoiceOver/NVDA manual.
*   [ ] **Open child PR** with `/create-pr` against `feat-location-s` (after the BLOCKING receive-side reader resolution).

**Other epic children (TBD) — additional V2 sections** (replace the V1 12-block layout the wrapper currently absorbs).

---

## SECTION 3: FEATURE / TICKET IMPLEMENTATIONS

### 3.1 Epic-level Foundation — `feat-location-s` (PR #20750)

**Created:** 2026-05-06 | **Last updated:** 2026-05-07
**Status:** **SHIPPED** to `feat-location-s` (commit `edda6f76318`). PR #20750 OPEN against `master`. github-actions APPROVED. Awaiting Carley's Tophat replication on staging/prod for the wrapper to receive customer traffic.
**Scope:** Sets up the feature branch with the parent wrapper, experiment splitter, Vuex wiring, global registration, and the local CMS migration tooling. **This is NOT DOTCOMPB-8120's implementation** — it is the precondition every child ticket on this branch (including 8120) builds on top of.

**Files shipped (4 files, +71/-0):**

| File | Change |
|---|---|
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/LocationSpecificColorbarV2.vue` | NEW — 67 lines (wrapper + Splitter + Vuex wiring + scoped Stylus) |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/index.js` | NEW — barrel export |
| `website/src/vuescripts/mrVueApp.js` | +1 line — global client registration via `defineAsyncComponent` |
| `website/src/vuescripts/ssr/registerGlobalsSsr.js` | +2 lines — global SSR registration (decl + `app.component` call) |

**Verified live (2026-05-07 via `cms-migrate.mjs inspect` + `curl http://localhost:3000/colorbar/location-specific`):**

| Layer | State |
|---|---|
| `content._id=3117` | `published_version=55`, `staged_version=55`, `edit_version=55` ✓ |
| `contentVersion` v55/A | experimentId `177809681959624`, variationId `…240`, weight `0` |
| `contentVersion` v55/B | experimentId `…624`, variationId `…241`, weight `10000` |
| `experiment._id=475` "New Messaging Test July 2025" | `status=Paused` ✓ |
| `experiment._id=504` "LocationSpecificSiteRevolution" | `status=Running`, variants `default` (0) + `b` (10000) ✓ |
| `template._id=1650` `location-specific-colorbar-v2` | jade: `location-specific-colorbar-v2(:cms-settings='!{JSON.stringify(settings)}')` ✓ |
| Variant B componentList | site-message-carousel → location-specific-colorbar-v2 → letter-from-amy → party-confetti → hcb-landing-sticky ✓ |
| Live SSR HTML | emits `<location-specific-colorbar-v2 :cms-settings="{title:'Salon results without salon cost & time', subtitle:'Let our pros do it for you. Get roots, all over color, highlights, and more.', colorbarTitle:'Locations near you'}">` ✓ |
| Rendered DOM | `<div class="location-specific-colorbar-v2"><!--[--><!--]--></div>` (empty fragment — V2 branch has no children yet, expected for the foundation) ✓ |

### 3.2 DOTCOMPB-8120 — Marketing LP Hero Section

**Created:** 2026-05-06 | **Last updated:** 2026-05-07 (refactor pass — search-precision + loadLocation watcher)
**Status:** **PHASE 1 BUILT (uncommitted on `DOTCOMPB-8120` branch).** Layout, H1, image, search container (forward-only with rich URL contract), 50-mile-gated nearest-location card all implemented and locally validated. Parent wrapper now dispatches `loadLocation` for the closest within 50 mi so siblings see `state.location`. **47/47 tests pass** (30 hero + 17 wrapper); ESLint clean; SSR HTTP 200. **BLOCKING for end-to-end demo:** receive-side reader on `ColorBarLocationSectionV1.vue` not yet written — without it, the map on `/colorbar/locations` won't show the picked colorbar.
**Branch:** `DOTCOMPB-8120` (already exists, off `feat-location-s` at commit `edda6f76318`).
**Roam node:** `~/.brain.d/roam-nodes/madison_reed/2026-05-06-141812-dotcompb_8120.org` (UUID `0904f0de-07dd-40ce-9ee2-e34fc4c30aa8`)
**Figma:** [Marketing Landing Pages — Hero (node 8005-3)](https://www.figma.com/design/vVTk5xHFRsiFYGUOVoHxYQ/Marketing-Landing-Pages?node-id=8005-3&t=5XgygUpORzgHqH0J-4)
**JIRA:** [DOTCOMPB-8120](https://madison-reed.atlassian.net/browse/DOTCOMPB-8120) (parent: [DOTCOMPB-8119](https://madison-reed.atlassian.net/browse/DOTCOMPB-8119))

#### Foundation contract (already shipped in PR #20750 — see §3.1)

> The wrapper exists and is wired. DOTCOMPB-8120 inherits this contract — it does NOT recreate it. The hero, offer callout, location search input, nearby-location section, and sticky CTA all land *inside* the V2 `v-if` branch of the existing wrapper.

**Files to create on `feat-location-s`:**

| File | Purpose |
|---|---|
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/LocationSpecificColorbarV2.vue` | Parent wrapper + Splitter + Vuex data loading |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/index.js` | Barrel export (`export { default } from './LocationSpecificColorbarV2.vue';`) |

**Component contract (mirrors `HcbLocationPageV2` exactly):**

| Prop | Type | Default | Source |
|---|---|---|---|
| `cmsSettings` | Object | `{}` | CMS template data — Tophat will populate this from the new mixin key once the V2 schema lands |
| `routeParams` | Object | `{}` | Express route params (none for this page today, but kept for symmetry with HcbLocationPageV2) |

**Vuex wiring at the parent (so all children consume via `mapState` without prop drilling):**

| Module | State / Action | Why |
|---|---|---|
| `mapState('colorbar', ['closestLocations', 'mapLocations'])` | nearby + map data | Children (hero nearby section, future map embed) read directly |
| `mapState('global', ['isDesktop'])` (via getter) | breakpoint | Sticky-CTA visibility |
| `mapActions('colorbar', ['getClosestLocationsByIp'])` | IP-resolved nearest salon | Server-safe (uses request IP) — runs in `serverPrefetch` |
| `mapActions('colorbar', ['getActiveLocationsListForMapView'])` | full locations list | For any embedded map widget |
| `mapActions('colorbar', ['initializeBopis'])` | client-side upgrade chain | Tries customer address → geolocation → IP. Run in `mounted` to upgrade past the IP-only SSR result. |

**Splitter logic:**

```js
const EXPERIMENT_NAME = 'LocationSpecificSiteRevolution';

computed: {
  inLocationSpecificExperiment() {
    return this.experiments?.[EXPERIMENT_NAME] === 'B';
  },
},

mounted() {
  this.mix_trackExperimentViewed(EXPERIMENT_NAME);
},
```

Template (Pug):
```pug
.location-specific-colorbar-v2
  template(v-if="inLocationSpecificExperiment")
    //- V2 layout — Site Revolution redesign
    //- TODO(DOTCOMPB-8120): MarketingLpHero
    //- TODO(future tickets): other sections
  template(v-else)
    //- V1 fallback — existing CMS-driven layout (globally registered components)
    //- The Tophat partial that currently composes /colorbar/location-specific stays
    //- the source of truth for the V1 visual experience. This Splitter only owns the
    //- experiment gate; V1 content is rendered by the surrounding CMS Pug template
    //- around this wrapper, OR via <slot/> if Tophat passes the V1 markup as default slot.
    slot
```

**SSR / experiment timing constraints (per decision #76 of parent session):**

*   `this.experiments` is `{}` during SSR — the experiment flag is only available after `mounted()`.
*   `serverPrefetch` runs server-side and CAN call `getClosestLocationsByIp` (no DOM) and `getActiveLocationsListForMapView` (no DOM).
*   The Splitter's `v-if` will evaluate `false` on the server → V1 (slot or empty) renders. After `mounted()`, the client re-evaluates and paints V2 if the user is in bucket B. Brief V1→V2 flash is the established pattern.
*   Data loading goes in `created` and `serverPrefetch`, never in `mounted`-only branches that gate on the experiment.

**Tracking:**

*   `mounted()` fires `mix_trackExperimentViewed('LocationSpecificSiteRevolution')` regardless of variant — required by `andris-guideline-13` (track exposure explicitly).
*   The four `MREvent (Marketing LP – ...)` events listed in §1.8 wire into the children rendered inside the V2 branch (hero CTA, offer callout, etc.) — out of scope for the scaffold PR.

#### Phase 1 BUILT — current as-shipped state on `DOTCOMPB-8120` branch

> Layout + H1 + image + search container + nearest-location card all implemented and locally validated. 36/36 tests, SSR HTTP 200, ESLint clean. Dev-fallback active (see §2.5 — REMOVE before PR).

**Component tree:**

```
LocationSpecificColorbarV2  (existing wrapper, scoped Stylus: max-width: bp-desktop-large; margin: 0 auto)
└── V2 v-if branch
    └── HeroSection                       NEW — root of all hero work
        ├── (mobile column-reverse / desktop row, 50/50 split)
        ├── .hero-section-content (brand-color-1-bg, px-100m + lg-px-500m, padding-top/bottom 52px, gap-26)
        │   ├── h1#hero-section-title (color-white .f-domaine-display-condensed .upper .max-at-tweak .xs-f-xgrande .sm-f-xgrande .md-f-xgrande .lg-f-poster .xl-f-poster .text-left .full-width)
        │   │                          (+ scoped Stylus: @media mq-desktop-plus { max-width: 70% } — hand-rolled because lg-max-width-rel-70 doesn't ship)
        │   ├── .hero-search-container  (ui-color-1-bg, gap-6, px-100m, py-100m, full-width, white card with shadow + 10px radius)
        │   │   ├── p.hero-search-helper-text  ({{ heroSettings.searchHelperText }})
        │   │   └── .hero-search-input-row  (gap-12, full-width, horizontal)
        │   │       ├── LocationSearchInput  (icon-name="map-marker-v2", placeholder="Enter ZIP or City, State")
        │   │       └── MrBtn(round @click="onSearchSubmit")  → SEARCH
        │   └── .hero-nearest-location-container  (ui-color-1-bg, full-width, white card; row layout always; v-if="shouldShowNearestLocation")
        │       ├── .hero-nearest-location-content  (3fr, gap-md, px-100m, py-100m, flex column)
        │       │   ├── p.hero-nearest-location-label  (.cta-color-1 — Primary Orchid)  → {{ nearestLocationLabel }}
        │       │   ├── .hero-nearest-location-info
        │       │   │   ├── h2.hero-nearest-location-name  → {{ nearestLocation.name }}
        │       │   │   └── .hero-nearest-location-address  → address1 / city, state, zip (stylus zeroes p margins for flush stack)
        │       │   └── MrBtn.hero-nearest-location-cta  (round, tag="a", href, aria-label, @click="handleBookAServiceClick")  → BOOK A SERVICE
        │       └── .hero-nearest-location-media  (2fr, position relative)
        │           └── :deep(.image-box) absolute-positioned to fill (anti-flicker pattern §1.18)
        └── .hero-section-media  (right column on desktop, top row on mobile)
            ├── (mobile aspect-ratio 95/69, desktop aspect-ratio auto + flex 1 1 0)
            └── ImgBox  (cms-settings.heroSection.image — media #7272 in dev)
```

**Component contract (`HeroSection.vue`):**

| Prop | Type | Default |
|---|---|---|
| `cmsSettings` | `Object` | `() => ({})` |

**Data:**
* `searchQuery` — v-model bound to `LocationSearchInput`. Free text input.
* `selectedPlace` — `null | { placeId, latitude, longitude, address }`. Captured from `place_changed` events. **Local only — never written to Vuex.**

**Computed:**
* `heroSettings` — `cmsSettings?.heroSection || {}`
* `heroImage` — `cmsSettings.heroSection.image` with `?...` query stripped from URL
* `closestLocations` — from `mapState('colorbar', ['closestLocations'])`
* `nearestLocation` — `closestLocations?.[0] || null`
* `nearestLocationImage` — `nearestLocation.headerImage` with URL stripped, null if absent
* `shouldShowNearestLocation` — `Boolean(heroSettings.nearestLocationLabel) && Boolean(nearestLocation) && typeof distance === 'number' && distance <= NEARBY_RADIUS_MILES` (50 — decision #28). Closes AC4/AC5 on the hero side.
* `bookingUrl` — `/colorbar/booking/${nearestLocation.code}/services` (fallback `/colorbar/locations`)

**Watchers:**
* `searchQuery(val)` — clears `selectedPlace` if `val !== selectedPlace.address`. Staleness guard so picking a prediction then typing past it sends text only, not stale place data.

**Lifecycle:**
* `async created()` — SSR-guarded; calls `googleMapsApiInitializer({...}, false)` then `await this.$gmapApiPromiseLazy()` to trigger plugin-tracked Maps load (§1.19). This is what makes the LocationSearchInput's autocomplete actually work.

**Methods:**
* `onPlaceChanged(place)` — captures `place.place_id`, `place.geometry.location.lat()`, `lng()`, `place.formatted_address` into `selectedPlace`. **No Vuex writes** (decision #26 — search input is forward-only). Bails if geometry coordinates missing.
* `onSearchSubmit()` — builds `URLSearchParams` from current state. If `selectedPlace` exists, sends rich payload (`search`, `lat`, `lng`, `placeId`); else just `search=<text>`. Empty/whitespace input → no-op. Calls `this.goToPath('/colorbar/locations?...')` for full SSR navigation.
* `handleBookAServiceClick()` — fires `trackMREventAndRedirect('MREvent (Marketing LP – Nearby location clicked)', bookingUrl, { locationCode, locationName })`

**Style overrides applied:**
* `.hero-section-content` vertical padding = 52px (= 2 × `gap-26`); horizontal padding `px-100m` mobile / `lg-px-500m` desktop (5em ≈ 80px)
* `.hero-search-input :deep(.search-icon)` color `brand-color-1` (overrides V1's text-color-3)
* `.hero-nearest-location-container :deep(.mrbtn)` — Reviews-pattern button styling (background `#EFEFF1`, border `#EFEFF1`, color `cta-color-1`, plus hover/active/focus all set `color: cta-color-1` to override MrBtn's default `setcolor(color-white)`)

**Schema on Tophat template _id 1650 (`location-specific-colorbar-v2`):**

| Field | Type | Default | Notes |
|---|---|---|---|
| `heroSection.title` | `text` (required) | "Salon results without salon cost & time" | H1 display |
| `heroSection.image` | `staticCroppedImage` (required, customCrops) | — | Hero image (right column); `staticCroppedImage` because layout does its own `object-fit: cover` — no responsive `srcset` |
| `heroSection.searchHelperText` | `text` | "Find a Madison Reed Hair Color Bar near you." | Renders above the search input |
| `heroSection.nearestLocationLabel` | `text` | "NEAREST LOCATION TO YOU" | Renders above the salon name in the nearest-location card |

Schema applied via `set-template-fields.mjs --mode merge --confirm`. Backups under `website/cms-backups/templateVersion/1650/`.

**Variant B contentVersion (19460) seeded values:**
* `heroSection.title` = "Salon results without salon cost & time"
* `heroSection.image` = media #7272 (Madison Reed Hair Color Bar Interior, DZ6_4801.jpg, 7072×4705)
* `heroSection.searchHelperText` = "Find a Madison Reed Hair Color Bar near you."
* `heroSection.nearestLocationLabel` = "NEAREST LOCATION TO YOU"

**Wrapper-level addition for sibling consumption (`LocationSpecificColorbarV2.vue`):**

Module-level constant: `const NEARBY_RADIUS_MILES = 50;` (duplicated in HeroSection.vue — decision #30).

```js
watch: {
  closestLocations: {
    immediate: true,
    handler(locations) {
      const closest = locations?.[0];
      if (!closest || !closest.code) {
        return;
      }
      if (typeof closest.distance !== 'number' || closest.distance > NEARBY_RADIUS_MILES) {
        return;
      }
      this.loadLocation(closest.code).catch(() => {});
    },
  },
},
methods: {
  ...mapActions('colorbar', [
    'getActiveLocationsListForMapView',
    'getClosestLocationsByIp',
    'initializeBopis',
    'loadLocation',
  ]),
}
```

When the wrapper's IP/initializeBopis chain populates `closestLocations` and the closest is within 50 mi, `colorbar/loadLocation(code)` runs → fetches the full `Location` document → commits `setLocation(data)` → **`state.location` populates for the page**, driving sibling-consumed getters: `region`, `availableAtColorbar`, `selectedLocationUrmPrice`, `servicePackage`, etc. If the closest is > 50 mi or missing, `state.location` stays empty (intentional — no stale "the location for this page" state when there's no nearby salon). Mirrors `PdpEntry.vue:234` precedent.

**Tests (47/47 passing):**

| File | Tests |
|---|---|
| `LocationSpecificColorbarV2.test.js` | 17 — Splitter computed, V2/V1 rendering, mounted/created/serverPrefetch dispatches, prop defaults, **`loadLocation` watcher (6 cases)**: dispatches at distance ≤ 50, dispatches at boundary 50, no-op at 50.01, no-op when `closestLocations` empty, no-op when distance missing, no-op when code missing |
| `HeroSection.test.js` | 30 — H1 + classes + ARIA (4), image handling (4), heroSettings computed (2), search container (8 — including text-only URL, rich URL on prediction pick, fallback when geometry missing, stale-place clearing, empty/whitespace no-op), nearest-location card (12 — including 50-mi boundary, hidden at 50.01, hidden when distance missing) |
| `ColorBarLocationSectionV1/LocationSearchInput.test.js` | 17 — original V1 page tests, all still pass after the non-invasive prop additions |

Test setup notes:
* `vi.mock('@gmap-vue/v3', ...)` at the top of HeroSection.test.js so the mocks for `googleMapsApiInitializer` + `getGoogleMapsAPI` don't actually load Maps
* `mocks: { $gmapApiPromiseLazy: () => Promise.resolve({}) }` so the async `created` resolves immediately
* `createNearestStore({ closestLocations })` provides `mapState('colorbar', ['closestLocations'])` data per test

**Open decisions parked:**
1. Casing for the H1 — currently storing the user-facing string in CMS verbatim ("Salon results without salon cost & time") + `.upper` renders it. If editors prefer typing in caps, drop `.upper`.
2. Min-heights `14em / 32em` were placeholders that we removed in favor of content-driven heights (column-reverse + aspect-ratio 95/69 mobile + flex stretch desktop). Pin precise heights from Figma if needed.
3. ~~50-mile gate~~ **RESOLVED 2026-05-07** — applied via `shouldShowNearestLocation` (HeroSection) + `loadLocation` watcher (wrapper). Decisions #28, #29.

---

### 3.3 `tophat-tools` Skill — Created and Refined 2026-05-07

**Created:** 2026-05-07 | **Last updated:** 2026-05-07
**Path:** `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/skills/tophat-tools/` (symlinked to `~/.claude/skills/tophat-tools`)
**Status:** v1.0.0 in production. All 17 scripts smoke-tested live against `content_id 3117`.

#### Why it exists

Every CMS investigation in this conversation followed the same painful path: write a one-off `docker exec mr-mongo mongosh cms --eval` query, parse free-form mongosh output, follow up with a related query, repeat 4-6 times. None of the queries were reusable. The skill packages every recurring CMS operation behind a stable CLI with structured JSON output.

#### Inventory

**Shared library (1):**
* `scripts/lib/mongo.mjs` — `mongoEval`, `mongoJson` (EJSON-wrapped), `findContentById`, `findContentByUri` (with parent walk-up + `takesUrlParameters`), `findTemplate*`, `findExperiment*`, `parseArgs`, `applyMongoFlags`, `printJson`, `die`, `requirePositional`.

**Read-only scripts (13):**
* `inspect-content.mjs` — content doc + variation breakdown + per-variation mixin keys
* `inspect-content-by-uri.mjs` — URI → content_id with parent walk-up
* `inspect-template.mjs` — template + active templateVersion (jade body)
* `inspect-experiment.mjs` — experiment doc + every contentVersion bound to it
* `get-component-list.mjs` — flat ordered list of mixin_keys + settings per variation
* `get-template-jade.mjs` — raw pug body (no JSON wrapper) for piping
* `get-template-fields.mjs` — `templateVersion.config[]` schema in default / `--flat` / `--json` modes
* `find-template-usage.mjs` — content bindings (which content embeds this mixin_key)
* `find-template-template-usage.mjs` — template-to-template references (config-field option / jade embed); self-references filtered
* `get-cms-additional-scripts.mjs` — JSON-LD `additionalScripts[]` from contentVersion + production_content + stage_content (drift check)
* `inspect-jsonld.mjs` — fetches a URL and parses every `<script type="application/ld+json">` block; supports `--variation X` via `?v=&xid=` overrides
* `find-cms-component-code.mjs` — kebab `mixin_key` → PascalCase Vue file + global registration line
* `find-route.mjs` — URI → Express route handler; warns when served via the CMS catch-all

**Mutation scripts — dry-run by default, `--confirm` to apply, every write backed up to `./cms-backups/<scope>/...` (4):**
* `set-experiment-status.mjs` — `Running` / `Paused` / `Stopped`
* `set-variant-weight.mjs` — writes both copies of weight (experiment.variations + every matching contentVersion)
* `add-jsonld-script.mjs` — append to `additionalScripts[]`; per-variation
* `set-template-fields.mjs` — replace OR merge mode for `templateVersion.config[]`; validates every field has `name` + `type`; normalises shape

**Migration / backup (3):**
* `migrate-content-experiment.mjs` — generalised version of the canonical `.tasks/DOTCOMPB-8120/cms-migrate.mjs`; takes a JSON config; idempotent re-runs (drops + re-creates target version)
* `backup-content.mjs` — full snapshot (content + every contentVersion + production_content + stage_content + relevant experiments + counters)
* `restore-content.mjs` — replay a snapshot; dry-run by default

**Rules (8):**
* `cms-data-model.md` — collections, runtime-vs-doc-id trap, denormalisation drift, dev-server cache caveats
* `inspection-scripts.md` — read-only flow recipes
* `experiment-management.md` — two-place weight rule, NAME vs KEY, locked variations
* `json-ld-management.md` — R1/R2/R3 storage paths, per-variation reality, raw-HTML verification rule
* `content-migration.md` — config schema for `migrate-content-experiment.mjs`; idempotency contract
* `code-locator-scripts.md` — kebab↔PascalCase contract, mixin_key→Vue file, URI→Express
* `safety-and-conventions.md` — five mandatory mutation rules (dry-run, backup, idempotency, "DB writes ≠ shipping", explicit defaults)
* `template-field-schema.md` — all 23 field types in `templateVersion.config[]` with usage counts, per-type `options` keys, modern vs legacy `selectOptions` shapes, `fieldConfig` nesting

**Manifest + architecture (2):**
* `SKILL.md` — keyword-rich Skill Analyzer manifest
* `AGENTS.md` — Two-Agent Model rationale, script-vs-rule split, extension points

#### Reusable knowledge captured

Future sessions investigating ANY MR CMS-driven URL should reach for this skill first:

* "What renders on this URL?" → `inspect-content-by-uri.mjs <uri>` → `get-component-list.mjs <id> --variation B` → `find-cms-component-code.mjs <mixin_key>`
* "Did my JSON-LD change take effect?" → `get-cms-additional-scripts.mjs <id>` (Mongo) vs `inspect-jsonld.mjs <url> --variation X` (raw HTML) — drift = dev-server cache or per-variation save mismatch
* "Where will my template change ripple?" → `find-template-usage.mjs <mixin_key>` (content) + `find-template-template-usage.mjs <mixin_key>` (template-to-template)
* "What fields can the editor configure on this template?" → `get-template-fields.mjs <template>` → edit JSON → `set-template-fields.mjs <template> --src <file> --confirm`
* "Pause this experiment / tune weights" → `set-experiment-status.mjs` + `set-variant-weight.mjs`
* "Migrate a content_id between experiments" → `migrate-content-experiment.mjs --from-config <file>` (generalised from `.tasks/DOTCOMPB-8120/cms-migrate.mjs`)

#### Survey findings folded in

Surveying every `templateVersion.config[]` in the live CMS (during `template-field-schema.md` authoring) revealed:
* 23 distinct field types in production. Counts: `text` 17296, `component` 3247, `staticCroppedImage` 2133, `croppedImage` 1996, `boolean` 1953, `image` 1788, `link` 1527, `object` 1352, `staticImage` 1326, `html` 589, `select` 337, `number` 320, `textarea` 309, `sectionHeader` 106, `product` 69, `dateTime` 19, `productPrice` 18, `partial` 17, `icon` 17, `featuredReview` 12, `productType` 5, `promotion` 5, `specificReview` 2.
* Reference templates with rich schemas: `_id=17` (`hero-static`, simple flat schema, 9 fields) and `_id=1625` (`home-tabs-vs`, deeply nested object hierarchy with helpText on every field — gold standard for help-text density).

#### CMS Schema — Actual vs Proposed (confirmed 2026-05-07)

| Field | Type | Status | Notes |
|---|---|---|---|
| `cmsSettings.title` | string | **EXISTS** | Today: "Salon results without salon cost & time". Already populated on variant B. |
| `cmsSettings.subtitle` | string | **EXISTS** | Today: "Let our pros do it for you. Get roots, all over color, highlights, and more." |
| `cmsSettings.colorbarTitle` | string | **EXISTS** | Today: "Locations near you". |
| `cmsSettings.heroImage` | media object | **PROPOSED — not in CMS** | Needs Tophat schema addition. URL-stripped + `ImgBox` skeleton. |
| `cmsSettings.primaryCta.text` | string | **PROPOSED — not in CMS** | "Book Now" default |
| `cmsSettings.primaryCta.destination` | string | **PROPOSED — not in CMS** | Booking flow URL (or fall back to `/colorbar/booking/${closestLocations[0]?.code}/services`) |
| `cmsSettings.offer` | object \| null | **PROPOSED — not in CMS** | When null, hide callout |
| `cmsSettings.offer.copy` | string | **PROPOSED — not in CMS** | Visible promo copy |
| `cmsSettings.offer.promoCode` | string | **PROPOSED — not in CMS** | Tracking + apply |
| `cmsSettings.offer.promoName` | string | **PROPOSED — not in CMS** | Tracking |
| `cmsSettings.offer.ctaText` | string | **PROPOSED — not in CMS** | Offer button label |
| `cmsSettings.offer.ctaDestination` | string | **PROPOSED — not in CMS** | Where to redirect (locations page with promo applied) |

#### Test Plan (wrapper scaffold — owed; not shipped in PR #20750)

| File | Tests |
|---|---|
| `LocationSpecificColorbarV2.test.js` | renders V2 branch when `experiments.LocationSpecificSiteRevolution === 'B'`; renders V1 (slot) fallback when `=== 'A'`; renders V1 fallback when experiment unset / SSR (`{}`); fires `mix_trackExperimentViewed('LocationSpecificSiteRevolution')` on mount; calls `getClosestLocationsByIp` in `created` with SSR guard; calls `getActiveLocationsListForMapView` in `serverPrefetch` (try/catch resilient); calls `initializeBopis` in `mounted`; passes `cmsSettings` and `routeParams` props correctly. **Note:** the shipped scaffold uses `created` (with `import.meta.env.SSR` guard) for `getClosestLocationsByIp`, NOT `serverPrefetch` — matches `ClosestHairColorBar.vue:20-21` precedent and `ssr-safety.md` rule #1. Update test expectations accordingly. |

---

## SECTION 4: FILE INDEX

### Existing (read-only — patterns to mirror)

| File | Why it matters |
|---|---|
| `website/src/vuescripts/components/HairColorBar/HcbLocationPageV2/HcbLocationPageV2.vue` | **Canonical parent-wrapper pattern** — `cmsSettings` + `routeParams` props, `mapState('colorbar', ['location'])`, `serverPrefetch` data load, `setBookingLocation` cross-store mutation. The `LocationSpecificColorbarV2` shell mirrors this 1:1. |
| `website/src/vuescripts/components/Shop/Splitter.vue` | **Canonical experiment splitter** — `const experimentName`, `experiments[experimentName] === 'B'` computed, `mix_trackExperimentViewed` in `mounted`. |
| `website/src/vuescripts/components/HairColorBarBooking/HCBCalendarV2/ExperimentSplitter/ExperimentSplitter.vue` | Second example of the splitter — different experiment, same shape. |
| `website/src/vuescripts/components/ClosestHairColorBar/ClosestHairColorBar.vue` | Existing IP-based "nearest salon" component — uses `vueColorbarSvc.getClosestLocationsByIp()` directly. The new wrapper centralizes this in Vuex instead. |
| `website/src/vuescripts/components/LocationSpecific/ExpectationModule/` | Sibling component on the LP today — globally registered. |
| `website/src/vuescripts/components/ColorBarMapSection/LocationsDirectory.vue` | Locations-list pattern — `serverPrefetch` data load + children consume via component data. |
| `website/src/vuescripts/store/modules/colorbar.js` | Source of truth for location data: `getClosestLocationsByIp`, `getClosestLocationByGeolocation`, `initializeBopis`, `getActiveLocationsListForMapView`. State: `closestLocations`, `mapLocations`, `location`. |
| `website/src/vuescripts/services/vueColorbarSvc.js` | Underlying API service. |
| `website/src/vuescripts/mrVueApp.js:411` | `inSiteRevolutionExperiment` global computed — `BookingFlowSiteRevolution` precedent for our new `LocationSpecificSiteRevolution`. |
| `website/src/routing/views/bookingSiteRevolutionMiddleWare.js` | Pattern for forcing experiment variant server-side via cookies. **NOT needed for the scaffold** — only relevant if the LP needs server-side rejection of certain customer types (TBD with PM). |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/FixedCtaBar/FixedCtaBar.vue` | Sticky mobile CTA — reuse for AC9. |
| `website/src/vuescripts/ssr/registerGlobalsSsr.js:51,123,224,280` | Global component registration — once the V2 component graduates to a global, register here too. |
| `website/src/vuescripts/mixins/menuMixin.js:13` | `mix_trackExperimentViewed` mixin. |

### Created and shipped in PR #20750 (foundation)

| File | Association |
|---|---|
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/LocationSpecificColorbarV2.vue` | foundation (PR #20750) |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/index.js` | foundation (PR #20750) |
| `.tasks/DOTCOMPB-8120/cms-migrate.mjs` | foundation (gitignored — local CMS migration tooling) |
| `.tasks/DOTCOMPB-8120/backups/<stamp>/snapshot.json` | foundation (gitignored — pre-migration backups) |
| `.tasks/DOTCOMPB-8120/pr-body.md` | foundation (gitignored — PR body source) |

### Modified in PR #20750 (foundation)

| File | Association |
|---|---|
| `website/src/vuescripts/mrVueApp.js` | foundation — global client registration (+1 line) |
| `website/src/vuescripts/ssr/registerGlobalsSsr.js` | foundation — global SSR registration (+2 lines) |

### Created on DOTCOMPB-8120 branch (uncommitted, 2026-05-07)

| File | Association |
|---|---|
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/LocationSpecificColorbarV2.test.js` | DOTCOMPB-8120 — 11 wrapper tests (clears the foundation owe) |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/HeroSection/HeroSection.vue` | DOTCOMPB-8120 — full hero (H1 + image + search container + nearest-location card) |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/HeroSection/HeroSection.test.js` | DOTCOMPB-8120 — 25 tests |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/HeroSection/index.js` | DOTCOMPB-8120 — barrel |
| `website/src/assets/svg-icons/map-marker-v2.svg` | DOTCOMPB-8120 — new location-pin icon for the search input |

### Modified on DOTCOMPB-8120 branch (uncommitted, 2026-05-07)

| File | Association |
|---|---|
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/LocationSpecificColorbarV2.vue` | DOTCOMPB-8120 — mount HeroSection in V2 `v-if`; restore wrapper Stylus (`max-width: bp-desktop-large; margin: 0 auto`) |
| `website/src/vuescripts/components/ColorBarLocationSectionV1/LocationSearchInput.vue` | DOTCOMPB-8120 — added optional `iconName` + `placeholder` props (defaults preserve V1 behavior; all 17 V1 tests pass) |

### Phase 2+ — files yet to create

| File | Association |
|---|---|
| `.../LocationSpecificColorbarV2/components/OfferCallout/*` (if extracted) | DOTCOMPB-8120 offer (parked) |
| `ColorBarMapSection.vue:mounted` (paired change) | AC11 search-prefill (parked) |
| `/colorbar/locations` route handler (paired change) | AC8 promo apply (parked) |

### Documentation (external)

| File | Purpose |
|---|---|
| `~/.brain.d/roam-nodes/madison_reed/2026-05-06-141812-dotcompb_8120.org` | Roam node — JIRA AC, event tracking, tasks |
| `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` | Index — entry added under IN PROGRESS + BACKLOG |

### `tophat-tools` skill (created 2026-05-07)

| Path | Purpose |
|---|---|
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/skills/tophat-tools/` | Skill source — symlinked into `~/.claude/skills/tophat-tools` |
| `…/tophat-tools/SKILL.md` | Skill Analyzer manifest |
| `…/tophat-tools/AGENTS.md` | Two-Agent Model rationale + extension points |
| `…/tophat-tools/rules/` | 8 atomic rule files (cms-data-model, inspection-scripts, experiment-management, json-ld-management, content-migration, code-locator-scripts, safety-and-conventions, template-field-schema) |
| `…/tophat-tools/scripts/` | 17 Node scripts (13 read-only + 4 mutation + 3 migration/backup) |
| `…/tophat-tools/scripts/lib/mongo.mjs` | Shared mongosh wrapper (mongoEval, mongoJson, finders, parseArgs) |

### Test image asset (Phase 1 seed)

| Asset | Notes |
|---|---|
| `db.media _id=7272` "Madison Reed Hair Color Bar Interior" | DZ6_4801.jpg, 7072×4705, 3:2, alt-text "Madison Reed Hair Color Bar Interior". URL `https://d3ewrnwdcmri66.cloudfront.net/content/images/2020/6/kd6g8x8g-dz6-4801/dz6-4801.jpeg`. Generic interior — no specific city. |
| `db.media _id=7583` (runner-up) | Woodlands Hair Color Bar — 7952×5304, highest-res. Use if #7272 looks weak in QA. |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last (2026-05-07 PM — search-precision refactor + loadLocation watcher)

*   **Search input refactored to forward-only** (decision #26). Removed the `getClosestLocationsByLatLong` action I had added to the colorbar store — fully reverted. Removed `mapActions('colorbar', ['getClosestLocationsByLatLong'])` and the old `onPlaceChanged` body that mutated `closestLocations`. The hero search no longer affects sibling state in any way.
*   **Rich URL contract for search submit** (decision #27). New local data: `selectedPlace: null | { placeId, latitude, longitude, address }`. New `onPlaceChanged(place)` captures the Google Place result into `selectedPlace`. New `onSearchSubmit()` builds `URLSearchParams`: when `selectedPlace` exists, sends `?search=<address>&lat=<>&lng=<>&placeId=<>`; else `?search=<text>`. Watcher on `searchQuery` clears `selectedPlace` if the user types past the picked text.
*   **50-mile gate added to `shouldShowNearestLocation`** (decision #28) — closes the previously parked open decision #3. Boundary inclusive (≤ 50). Closes AC4/AC5 on the hero side.
*   **Parent wrapper `loadLocation` watcher** (decision #29). New module-level `NEARBY_RADIUS_MILES = 50` in `LocationSpecificColorbarV2.vue`. New `watch: closestLocations` (immediate) — when `closestLocations[0].distance <= 50` and a `code` exists, dispatches `colorbar/loadLocation(code)`. Added `loadLocation` to the wrapper's `mapActions`. This is what makes sibling sections (services/prices/region getters in `colorbar` store) reflect the user's nearest salon as THE page location. Mirrors `PdpEntry.vue:234` pattern.
*   **Spinner experimentation tried + reverted twice** (decision #32). First: `:loading="searchLoading"` prop on `LocationSearchInput` — input disables, icon swaps to `spinner-v2.spin`, placeholder changes to "Detecting Location...". Reverted because user wanted full overlay UX. Second: `MrSpinnerVeil(v-if="searchLoading")` overlay over `.search-card` with `position:relative`+`overflow:hidden`. Reverted entirely because the user redirected away from the spinner approach: the search input must be navigation-only with NO sibling-state dispatch (so there's nothing to "wait" on locally — just navigate).
*   **SVG dev-load latency audit** (decision #31, §1.21). Identified root cause as `vite-plugin-svgicon` having no transform cache + 166 separate chunks via `import.meta.glob` + `MrIcon` deferring to `mounted()`. Cold-load 70-260ms per icon in dev, 5-20ms in prod. SVG file optimized 864→511 bytes — but didn't fix the perceived slowness because transform output dominates. **Production users will never see this.**
*   **Test counts:** wrapper 11→17 (added 6 tests for the loadLocation watcher), hero 25→30 (added rich-URL on prediction pick, fallback when geometry missing, stale-place clearing, plus 50-mi boundary cases on the nearest-location card). 47/47 passing.

### Where to resume

🚨 **The map on `/colorbar/locations` does NOT show the picked colorbar yet.** This is the next thing to fix. The hero now sends a precise URL contract (`?search=<address>&lat=<>&lng=<>&placeId=<>`) but the receive-side reader on `ColorBarLocationSectionV1.vue:mounted` doesn't exist yet. Without it, the destination page falls through to its own IP/customer/geo resolution and the user's pick is silently dropped.

If the user asks to **fix the map showing the picked colorbar** (most likely next step):
1. Edit `website/src/vuescripts/components/ColorBarLocationSectionV1/ColorBarLocationSectionV1.vue:mounted`.
2. Read `$route.query.{search, lat, lng, placeId}`.
3. If `search` is present, set `this.searchQuery = search` (prefills input).
4. If `lat` + `lng` present, hydrate `this.currentPlace = { position: { lat: parseFloat(lat), lng: parseFloat(lng) }, formatted_address: search }` directly — skip the geocode round-trip; map renders immediately. Optionally also set `this.locationSource = 'url'` to ensure the IP/customer/geo fallback chain doesn't override.
5. If only `search` present (free-typed text), call the existing geocode flow with `geocoder.geocode({ address: search })` then route the result through `setCurrentPlace`. (May not need this branch if the AC says "data they entered" and we're OK with the user re-pressing Enter.)
6. Add tests covering the three URL shapes (rich, text-only, none).

If the user asks to **commit and open the PR for DOTCOMPB-8120:**
1. Resolve the BLOCKING receive-side reader first (above) OR scope it to a same-day follow-up sub-ticket and document in the PR body.
2. Optional cleanup: extract `NEARBY_RADIUS_MILES` to a shared constants file.
3. Run `cd website && npm run test:vue LocationSpecificColorbarV2` — confirm 47/47.
4. Run `eslint website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/` — confirm clean.
5. ASK USER before any git command (per `feedback_no_git.md` memory).
6. Stage HeroSection + tests + wrapper + LocationSearchInput edit + svg + (paired locations change if bundled) and commit with JIRA-prefixed message.
7. Open PR via `/create-pr` against `feat-location-s`.

If the user asks to **start any Phase 2+ work** (offer callout, sticky CTA, paired `?promo=CODE` reader, tracking, ADA pass): see §2.5 Phase 2+.

If the user asks to **investigate any CMS-driven URL or template question**: reach for the `tophat-tools` skill (§3.3 / `~/.claude/skills/tophat-tools/`) before writing one-off mongosh queries.

If the user asks for a **new task**: check §2.5 (Pending Work).

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session. Newest row first. See `~/.claude/skills/session-reset/rules/activity-log.md` for the schema.

| Datetime         | Duration | Type             | Reference     | Description |
|------------------|----------|------------------|---------------|-------------|
| 2026-05-07 17:23 | 1h       | session-reset    | this          | Reset captured search-precision refactor + loadLocation watcher. Added §1.21 (SVG dev-load latency mechanics); updated §1.6 with rich-URL contract + forward-only rule; updated §2.3 #2 to RESOLVED-WITH-BLOCKER (map not showing picked colorbar without paired reader); decisions 26-32; rewrote §2.5 with BLOCKING flag for receive-side reader; refreshed §3.2 methods/computed/watchers, added wrapper loadLocation block, tests 36→47; replaced §5 |
| 2026-05-07 17:00 | 0.25h    | refinement       | DOTCOMPB-8120 | Acknowledged NEARBY_RADIUS_MILES=50 duplication in HeroSection.vue + LocationSpecificColorbarV2.vue (decision #30). Polish item, not extracted to shared constants module |
| 2026-05-07 16:45 | 0.5h     | implementation   | DOTCOMPB-8120 | Parent wrapper `loadLocation` watcher — `watch: closestLocations` (immediate) dispatches `colorbar/loadLocation(code)` when distance ≤ 50. Populates state.location for sibling sections (services/prices/region). 6 new wrapper tests (11→17). Mirrors PdpEntry.vue:234 |
| 2026-05-07 16:30 | 0.5h     | implementation   | DOTCOMPB-8120 | Search submit forwards rich URL params: ?search=<address>&lat=<>&lng=<>&placeId=<> when prediction picked, ?search=<text> otherwise. Added selectedPlace local data + capture-only onPlaceChanged + staleness watcher on searchQuery. Hero tests 25→30 |
| 2026-05-07 16:00 | 0.5h     | refinement       | DOTCOMPB-8120 | Reverted the wrong logic that mutated closestLocations from the search input. Reverted the getClosestLocationsByLatLong action I had added to the colorbar store. Search is now navigation-only (decision #26) |
| 2026-05-07 15:45 | 0.25h    | implementation   | DOTCOMPB-8120 | 50-mile gate added to shouldShowNearestLocation (HeroSection.vue). Closes parked open decision #3. Boundary ≤ 50 inclusive. Boundary tests for 50, 50.01, missing distance |
| 2026-05-07 15:15 | 0.5h     | research         | DOTCOMPB-8120 | SVG dev-load latency audit — root cause is vite-plugin-svgicon (no transform cache) + 166 chunks via getIconModules + MrIcon defers to mounted. 70-260ms cold dev, 5-20ms prod. File-size optimization 864→511 bytes didn't help. Documented in §1.21 |
| 2026-05-07 14:45 | 0.5h     | implementation   | DOTCOMPB-8120 | Spinner experimentation tried + reverted twice. First :loading prop on LocationSearchInput; then MrSpinnerVeil overlay. Both reverted when user redirected to forward-only search with no local "waiting" state |
| 2026-05-08 00:30 | 1h       | session-reset    | this          | Reset captured Phase 1 BUILT state. Added §1.16-1.20 (utility-class realities; Vue scoped-CSS gotchas; layout-stability for image cover-fit; Google Maps lazy-load; SVG authoring rules), decisions 19-25, restructured §3.2 to "BUILT" status with full as-shipped tree + tests + schema, refreshed §4 file index, replaced §5 entirely |
| 2026-05-08 00:00 | —        | bug-fix          | DOTCOMPB-8120 | Search input wiring fix — adopted `GuestCheckoutAddressInput.vue:404` pattern: `googleMapsApiInitializer({...}, false)` + `await this.$gmapApiPromiseLazy()`. Plugin's `dynamicLoad: true` requires the plugin-tracked promise to actually load Maps |
| 2026-05-07 23:50 | 0.25h    | bug-fix          | DOTCOMPB-8120 | MrBtn hover override — added explicit `color: cta-color-1` on `&:hover, &:active, &:focus` to override default `setcolor(color-white)` and prevent white text on light gray bg |
| 2026-05-07 23:40 | 0.25h    | bug-fix          | DOTCOMPB-8120 | Vue scoped-CSS bug fix — moved `:deep(.mrbtn)` from `.hero-nearest-location-cta` (same-element, no match) to `.hero-nearest-location-container` (true parent). Mirrors Reviews component's `.hcb-reviews :deep(.mrbtn)` pattern |
| 2026-05-07 23:30 | 0.25h    | refinement       | DOTCOMPB-8120 | H1 width 70% on desktop via scoped Stylus `@media mq-desktop-plus { max-width: 70% }`. `.lg-max-width-rel-70` claimed in skill docs but not in actual `globals/utilities.styl` — flagged as skill-doc bug |
| 2026-05-07 23:00 | 1h       | implementation   | DOTCOMPB-8120 | Nearest-location card built — 3fr/2fr layout, white card with cta-color-1 Primary Orchid label, name + address (margins zeroed), BOOK A SERVICE button with Reviews-pattern styling override + tracking handler |
| 2026-05-07 22:00 | 1h       | implementation   | DOTCOMPB-8120 | Search container built — white card with helper text, LocationSearchInput (icon-name=map-marker-v2, custom placeholder), MrBtn SEARCH. New `map-marker-v2.svg` icon added. LocationSearchInput extended with `iconName`+`placeholder` optional props (V1 backwards-compat) |
| 2026-05-07 21:00 | 1.5h     | implementation   | DOTCOMPB-8120 | Phase 1 layout shipped — HeroSection.vue: H1 + image + 50/50 desktop / column-reverse mobile. Anti-flicker pattern (absolute `.image-box` + container aspect-ratio 95/69 mobile + flex-stretch desktop). Iterative refinements on flex behavior, image overflow, content-driven heights, wrapper max-width (`bp-desktop-large` variable) |
| 2026-05-07 19:30 | 1h       | implementation   | DOTCOMPB-8120 | LocationSpecificColorbarV2.test.js + HeroSection.test.js initial cuts. 21 → 36 tests through the build. Vuex store stub via `createMockStore`; @gmap-vue mock for created hook |
| 2026-05-07 19:00 | 0.5h     | migration        | DOTCOMPB-8120 | CMS schema applied to template 1650: 4 fields under `heroSection` (title, image, searchHelperText, nearestLocationLabel). Variant B contentVersion 19460 seeded with all 4 values. Backups under website/cms-backups/templateVersion/1650/ |
| 2026-05-07 22:30 | 0.5h     | refinement       | DOTCOMPB-8120 | Memory-vs-skill conflict resolved: flex layout stays in scoped Stylus per `feedback_no_flex_utility.md`. Updated §1.14 + §3.2 Pug/Stylus to drop `.flex`/`.flex-col`/`.flex-1`/`.align-center`/`.space-center`; kept non-flex utilities (`.full-width`, `.no-scroll`, `.brand-color-1-bg`, spacing/typography). Added decision #18 |
| 2026-05-07 22:00 | 1h       | session-reset    | this          | Captured tophat-tools skill build + Hero plan lock + image selection; added §1.14/§1.15, locked §3.2 Phase 1 spec, added §3.3 for the new skill, updated §4 with skill paths + test asset, replaced §5 |
| 2026-05-07 21:30 | —        | research         | DOTCOMPB-8120 | Selected hero image asset: media _id=7272 "Madison Reed Hair Color Bar Interior" (7072×4705 3:2) after rejecting product mockups (#16860) and screenshots (#16078) |
| 2026-05-07 21:00 | 1h       | refinement       | DOTCOMPB-8120 | Locked HeroSection plan: 2-col desktop / 2-row mobile (column-reverse), Domaine Display Condensed H1 (xgrande/poster), brand-color-1-bg, heroSection.{title,image} schema. Validated against /mr-dotcom-dev rules — corrected .bg-brand-1 → .brand-color-1-bg + replaced raw flex Stylus with utilities |
| 2026-05-07 19:30 | 1h       | refinement       | n/a           | Refined tophat-tools skill: added find-template-template-usage.mjs, get-template-fields.mjs, set-template-fields.mjs + rules/template-field-schema.md. Surveyed all 23 field types across templateVersion.config[] in production |
| 2026-05-07 18:00 | 2h       | implementation   | n/a           | Built tophat-tools skill v1.0.0: SKILL.md + AGENTS.md + 7 rules + 14 scripts + lib/mongo.mjs (24 files). Symlinked into ~/.claude/skills/. Smoke-tested every script category live against content_id 3117 |
| 2026-05-07 15:30 | 1h       | research         | DOTCOMPB-8120 | Audited live CMS state via `cms-migrate.mjs inspect` and grep'd codebase to resolve 7 open questions (search prefill, geolocation, distance, toast, FixedCtaBar, promo apply, CMS schema) |
| 2026-05-07 15:00 | 0.5h     | qa               | PR #20750     | Verified PR shipped state: 4 files +71/-0, github-actions APPROVED, CMS migration live (content 3117 v55, exp 504 Running B@100%), live page returns HTTP 200 |
| 2026-05-06 21:48 | —        | pr-open          | PR #20750     | "Site Revolution Marketing LP — V2 Foundation" opened against master from feat-location-s |
| 2026-05-06 21:16 | —        | commit           | commit edda6f | feat(DOTCOMPB-8120): scaffold LocationSpecificColorbarV2 + experiment splitter — wrapper + index.js + mrVueApp.js + registerGlobalsSsr.js |
| 2026-05-06 20:00 | 1h       | migration        | DOTCOMPB-8120 | Local CMS migration via `cms-migrate.mjs migrate --confirm` — content 3117 v54→v55, exp 475 Paused, exp 504 (LocationSpecificSiteRevolution) Running B@100%, sub-template 1650 created |
| 2026-05-06 17:00 | 2h       | implementation   | DOTCOMPB-8120 | Wrote `LocationSpecificColorbarV2.vue` foundation (props, Vuex, splitter, lifecycle); registered globally on client + SSR |
| 2026-05-06 15:30 | 0.5h     | planning         | DOTCOMPB-8120 | Slack with Carley confirmed retiring exp 475 in favor of LocationSpecificSiteRevolution; she owns the Tophat-side production replication |
| 2026-05-06 14:18 | 2h       | refinement       | DOTCOMPB-8120 | Created roam node + this session file; abstracted patterns from site-revolution-redesign; pulled JIRA via acli; confirmed feature branch feat-location-s |
| 2026-05-07 17:00 | —        | other            | this          | Activity Log added on this reset; pre-2026-05-06 history not back-filled (none — session was created 2026-05-06) |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
