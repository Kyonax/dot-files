# MR Dotcom Development — Architectural Guide

**Version 3.0.0** — Kyonax, Dotcom Team

## Purpose of This File

This file explains the **philosophy, rationale, and interconnections** behind the individual rule files. It is the "why" document — the rules themselves are the "what." Read this file when you need to understand how rules relate to each other, resolve ambiguity between overlapping concerns, or make architectural decisions that span multiple domains.

The rule files are atomic and self-contained — each one gives you everything you need to execute within its domain. This file gives you the judgment to know _which_ domains matter for a given task and _why_ the rules exist in the first place.

---

## Core Architecture: Utility-First Composition

The single most important principle governing all frontend code at Madison Reed is **composition over custom CSS**. The design system provides a comprehensive set of utility classes for spacing, typography, flexbox, layout, and interactivity. Components are styled by composing these classes directly in Pug templates, not by writing custom Stylus.

**Why this matters:** It creates a single source of truth for visual consistency. When a brand color changes, it changes everywhere. When responsive breakpoints are adjusted, every component using breakpoint-prefixed classes adapts automatically. Custom CSS is reserved strictly for structural concerns that cannot be expressed with utilities: complex positioning, pseudo-elements, transitions, and animations.

This philosophy is enforced across four complementary rule files that together form the complete styling system:

| Rule                            | Domain                   | When It's the Primary Source                                                                  |
|---------------------------------|--------------------------|-----------------------------------------------------------------------------------------------|
| `rules/spacing-utilities.md`    | Margin & Padding         | Any `margin` or `padding` on any element (em and pct systems)                                 |
| `rules/typography-utilities.md` | Text, Color & Variables  | Font families/sizes/weights, all color systems, Stylus variables, breakpoints, letter-spacing |
| `rules/flexbox-layout.md`       | Flex & Aspect Ratio      | Flex containers, alignment, sizing, gap, wrap, aspect ratio boxes                             |
| `rules/utility-classes.md`      | Display, Position & More | Display, visibility, positioning, centering, sizing, overflow, borders, interactivity, a11y   |

These four rules have **zero overlap** — each class exists in exactly one rule file. When a task involves "styling," the agent must determine _which aspect_ of styling is involved and load the correct rule(s). Loading all four is appropriate only for broad styling tasks or full component creation.

Note: `typography-utilities.md` serves double duty — it covers both utility classes for Pug templates AND Stylus variables for `<style>` blocks (colors, fonts, breakpoints, mixins). This is intentional: the same design system tokens are used in both contexts, and splitting them would force loading two files for a single color question.

### Why Custom CSS Is Restricted

Scoped Stylus exists in components, but its role is narrowly defined. It handles only what utility classes cannot express:

- **Structural layouts** that require specific grid definitions or complex calc-based sizing
- **Pseudo-elements** (`:before`, `:after`) which have no utility class equivalent
- **Transitions and animations** beyond the simple `.shake` and `.spin` utilities
- **Complex media queries** that combine multiple conditions or need custom breakpoints

When scoped Stylus is used, it must reference design system variables (`ui-color-5`, `mr-purple`) and breakpoint mixins (`mq-mobile`, `mq-tablet-plus`) — never hardcoded hex values. If no exact breakpoint variable exists for a specific value, keep the hardcoded `px` value rather than using an approximate variable. `variables.styl` and `mixins.styl` are auto-injected by Vite, so no imports are needed.

**Revised unit convention:** Use `rem`/`em` for layout spacing (margins, padding, width) and typography. For small, fixed-size UI elements like `border-width`, `box-shadow` offsets, or icon sizes, use `px` if the `em`/`rem` conversion results in a non-intuitive decimal (more than two decimal places). This revision came from PR feedback — `0.143em` for a `2px` border is unreadable and defeats the purpose of relative units.

### How the Four Styling Rules Interact

A single Pug template will often use classes from all four systems simultaneously. Consider a typical section header:

```pug
.section-header.flex.space-between.align-center.mb-200m.sm-mb-300m
  h2.f-secondary.upper.xs-f-large.md-f-xlarge.color-mr-purple Section Title
  mr-btn.clickable(@click="handleAction") View All
```

This single line pulls from: **flexbox** (`.flex`, `.space-between`, `.align-center`), **spacing** (`.mb-200m`, `.sm-mb-300m`), **typography** (`.f-secondary`, `.upper`, `.xs-f-large`, `.md-f-xlarge`, `.color-mr-purple`), and **utility-classes** (`.clickable`). The rules are additive and non-conflicting — they are designed to compose together.

---

## Component Architecture: Vue 3 Options API

All Vue components follow the **Options API** pattern — Composition API is not used in production code. This is a deliberate architectural choice for consistency across a 289+ component codebase maintained by multiple teams.

**Reference:** `rules/vue-patterns.md` — the largest and most frequently needed rule file.

### Why Options API Over Composition API

The Options API provides a consistent, predictable structure that every developer on the team can read and navigate instantly. The canonical field order (`name → emits → props → data → computed → watch → lifecycle → methods`) is enforced in every component. This consistency eliminates cognitive overhead during code review and makes onboarding faster.

The Composition API offers flexibility that, in a large monorepo with high developer turnover, becomes a liability — there are too many valid ways to organize the same logic. The Options API constrains this freedom in exchange for uniformity, which is the correct tradeoff at this codebase's scale.

### The Component Ecosystem

`vue-patterns.md` is the central rule because it governs not just component structure, but the entire ecosystem of patterns that components interact with. Understanding each subsystem is essential for writing correct, consistent code.

#### Vuex Store Modules

All state management uses namespaced Vuex 4 modules with factory-function state. Components connect via `mapState`, `mapGetters`, `mapActions` spread into their `computed` and `methods` blocks. Cross-module communication uses `dispatch('module/action', payload, { root: true })` from actions, and `rootState.moduleName.property` or `rootGetters['moduleName/getterName']` from getters.

The factory-function pattern (`const state = () => ({...})`) is critical — it prevents state leakage between SSR requests. If state were a plain object, all users would share the same Vuex state on the server.

#### Modal System

Modals are **never** mounted directly with `v-if` or `v-show`. All modals go through a centralized Vuex-based system: `dispatch('modal/showModal', { component, theme, props })`. The `AppModal.vue` component renders the active modal dynamically via `:is="component"`.

This centralization exists because modals have complex shared requirements: transition animations, `<focus-lock>` for accessibility, `body-scroll-lock-upgrade` for scroll management, step-back navigation for multi-step flows, and consistent close behavior (ESC key, background click, close button). Duplicating this logic in every modal would be unmaintainable.

Modal file names must end with "Modal" — this is enforced by convention and serves as a quick identifier in imports and the `showModal` dispatch.

#### API Services

Every API domain has a dedicated service file following the `vue{Domain}Svc.js` naming convention (e.g., `vueCartSvc.js`, `vueCustomerSvc.js`). These wrap `mrApi`, which is itself an Axios wrapper that injects CSRF tokens and timezone headers automatically. Services are also available as `this.$mrApi` in components via a Vue plugin.

The service layer exists to keep API call definitions in one place per domain, preventing scattered `axios.get()` calls throughout components. Components should call services through Vuex actions — the action handles the API call, commits the mutation with the response, and manages loading state.

#### Global Components and Mixins

Approximately 70 components are globally registered in `mrVueApp.js` — including `MrBtn`, `MrIcon`, `ImgBox`, `AppModal`, `TransitionExpand`, `FocusLock`, and `SwiperCarousel`. These can be used in any template without import statements.

Seven legacy mixins are applied globally, providing methods like `trackMREvent()`, `scrollTo()`, `goToPath()`, `openLogInModal()`, and `addToCartAndTrackEvent()` on every component instance. **New shared logic must use composables, not mixins** — the mixin system is legacy and will not be expanded.

#### Design Patterns Enforced by vue-patterns.md

Several patterns are enforced to prevent common mistakes:

**Props over `:deep()` (nuanced):** The `:deep()` selector should be used sparingly. Before using it, always consider if the child's appearance can be controlled via its props API (e.g., adding a `secondary` or `staticMode` boolean prop). Use `:deep()` only for simple, targeted overrides where modifying a shared child component is not feasible or is overly complex. Never fork a component to avoid `:deep()` — that is worse than a targeted override.

**Module-level constants:** Static, non-reactive data (mapping objects, configuration arrays, Swiper options) must be defined as `const` outside the `export default` block. Placing them inside `data()` causes unnecessary re-declaration on every render and makes them reactive when they shouldn't be.

**Parent data preparation:** The parent component is responsible for preparing and shaping data before passing it as props. Use computed properties to filter, transform, or sanitize data for children. Children should receive clean, ready-to-render data.

**Responsive component swapping:** When mobile and desktop layouts are radically different (not just responsive adjustments), use a reactive `isMobile` property to conditionally render entirely different child components rather than trying to CSS-adapt a single component.

**Skeleton loading:** Use a component-scoped class (e.g., `.has-skeleton`) on image containers when image data is `null`, and conditionally render `ImgBox` inside with `v-if`. Both primary and secondary image containers must have `v-if` guards for consistency. Note: `.has-skeleton` is NOT a global utility class — define it in each component's `<style>` block. Complex image components should handle their own skeleton state internally.

**No inline event logic:** Template event handlers must call a single component method. Complex inline expressions with computed arguments are forbidden — they cannot be unit-tested without simulating click events. Extract the logic into a dedicated method (e.g., `handleBookServiceClick`).

**No magic numbers:** All numeric literals used for configuration, iteration counts, or conditional logic must be extracted into named `const` variables at module level. This came from PR feedback — `v-for="i in 3"` and `if (count > 3)` are unreadable without context.

**DRY templates:** Repeated boolean expressions in `v-for` loops must be encapsulated in a component method (e.g., `isSelected(item)`) rather than duplicated across `:class` and `:aria-current` bindings.

**Native elements for focus:** Use native `<button>` for clickable actions. `MrBtn` defaults to `<button>` — using `tag="a"` without a valid `href` renders an anchor that is NOT keyboard-focusable, breaking ADA compliance. Only use `tag="a"` when the element is a true navigation link.

**No nested interactive elements:** Never nest interactive elements (e.g., `<a>` inside `role="button"`). This creates dual tab stops and conflicting handlers. For overlay patterns (like "view more" inside a clickable slide), use a decorative `<div aria-hidden="true">` and handle the action via the parent's single click handler with conditional logic.

**Viewport detection with `matchMedia`:** Raw `window.resize` listeners are forbidden for responsive logic. Use `window.matchMedia` — it fires only on threshold crossings and is far more performant. Store the media query instance in component `data` (not module-level) to avoid SSR crashes where `window` is unavailable.

**Parent-child lifecycle communication:** Parents must NOT listen to child lifecycle hooks (e.g., `@hook:mounted`). To signal readiness, children emit a custom event (`this.$emit('ready')`) and parents listen to it (`@ready="..."`). The `@hook:` pattern is fragile and relies on Vue internals.

#### Accessibility and Event Tracking

Accessibility and analytics are not optional add-ons — they are first-class requirements embedded in every component.

**Semantic HTML:** Use `<main>`, `<section>`, `<dl>`, `<nav>` over generic `<div>` wherever the content has semantic meaning. This provides structure for screen readers and improves SEO.

**ARIA attributes:** Interactive elements must include `aria-controls`, `aria-expanded`, `aria-labelledby` as appropriate. Sections should reference their headings via `aria-labelledby`.

**Keyboard navigability:** Every interactive non-native element (clickable divs, thumbnail selectors, custom buttons) must include `tabindex="0"` and respond to `@keydown.enter.prevent` and `@keydown.space.prevent`.

**Event tracking:** All meaningful user interactions must be tracked via `this.trackMREvent(eventName, properties)`. This method is globally available via mixin — it requires no import. For page load events, call `trackMREvent` in a `watch` with `immediate: true` on the relevant data. **Never** call `segmentTracking`, `window.analytics`, or any other tracking method directly.

---

## Template Layer: Pug

**Reference:** `rules/pug-templates.md`

Pug serves two distinct roles at Madison Reed, and understanding the distinction prevents confusion.

### Pug in Vue Components

Every `.vue` file uses `template(lang="pug")`. This is the template syntax for all Vue component rendering. Components are referenced in kebab-case (`mr-btn`, `address-edit-modal`), props are passed as Pug attributes with `:` for dynamic binding, events use `@`, and slots use the `slot` tag.

This is where the utility-first styling philosophy materializes — classes from all four styling rules are composed directly on Pug elements. The `pug-templates.md` rule defines _how_ to write Pug syntax correctly; the styling rules define _which classes_ to use.

### Pug as Page Layouts

Server-rendered pages use Pug template inheritance via `extends` and `block`. A base layout (`vue-layout.pug`) defines the HTML shell with named blocks (`title`, `headerScripts`, `styles`, `content`, `footerScripts`). Page templates extend this base and fill in their blocks, optionally appending to them with `block append`.

These page-level templates also have access to Express `res.locals` and `config` for server-side conditional rendering — things like environment-specific scripts, A/B test flags, or server-injected data.

### When to Load pug-templates.md

Load this rule whenever the task involves writing or modifying Pug syntax — whether in a `.vue` component template or a page layout file. If the task is purely about which classes to apply (not how to write the Pug), the styling rules alone may suffice.

---

## Testing: Vitest + Vue Test Utils

**Reference:** `rules/testing-standards.md`

Testing follows strict conventions designed to produce fast, reliable, and maintainable test suites. These conventions exist because the codebase has 289+ components, and inconsistent test patterns make the suite fragile and hard to maintain.

### Why Snapshots Are Forbidden

Snapshot tests (`toMatchSnapshot()`) are explicitly forbidden in this codebase. The reasoning is practical: in a large, actively developed codebase with utility-class-heavy templates, snapshot tests break on every minor class change, creating a constant stream of "update snapshot" commits that teach developers to approve changes without reviewing them. This defeats the purpose of testing entirely.

Instead, all rendering assertions must be explicit — verify that specific elements exist (`.exists()`), specific props are passed (`.props()`), specific events are emitted (`.emitted()`), or specific text is rendered (`.text()` on real, non-stubbed elements).

### Why shallowMount Is the Default

`shallowMount` stubs all child components, isolating the unit under test. This is the default because:

1. It makes tests faster by not rendering entire component subtrees
2. It prevents tests from breaking due to changes in child components
3. It forces the test author to focus on the component's own logic, not its children's rendering

When a child is stubbed, you cannot assert its text content or internal structure. Instead, assert that the stub exists, that it received the correct props, and that the parent reacts correctly to its emitted events. Only use `mount` when the test specifically needs to verify parent-child integration.

### The $nextTick Requirement

Vue's reactivity system batches DOM updates. After any programmatic state change (setting `wrapper.vm.someData = newValue`, calling a method that modifies data), the DOM is not updated synchronously. You must `await wrapper.vm.$nextTick()` before asserting anything about the rendered output. Omitting this is the most common cause of flaky tests in the codebase.

### Relationship to vue-patterns.md

Understanding the component patterns (Vuex stores, modals, services, global mixins) is essential for writing correct tests. The `createWrapper` factory in every test file must mock the correct Vuex modules, provide global mixin methods (`trackMREvent`, `scrollTo`, `goToPath`) in `global.mocks`, and stub components that the setup file doesn't already handle.

A common testing challenge is mocking `window.matchMedia` for responsive components. Module-level code executes at import time — before `beforeEach` stubs. The solution is to define a mock object with `addEventListener`/`removeEventListener` spies, stub the global via `vi.stubGlobal`, and assert directly against the mock object's methods. The full pattern is documented in `testing-standards.md` Section 6.

When an agent needs to write or fix tests, it should load **both** `testing-standards.md` (for test conventions and patterns) and `vue-patterns.md` (for the component patterns being tested). Without `vue-patterns.md`, the agent won't know which store modules to mock or which global methods to provide.

---

## Server Infrastructure

### SSR Architecture

**Reference:** `rules/ssr-architecture.md`

The website uses Vue 3 Server-Side Rendering for SEO and initial load performance. Understanding the SSR pipeline is essential when debugging hydration mismatches, adding new pages, or working with Vuex state that must survive the server-to-client handoff.

#### Why SSR Matters for Development

SSR introduces constraints that affect everyday component development:

- **No browser APIs on the server:** `window`, `document`, `localStorage`, and `navigator` are unavailable during `renderToString`. Code that accesses these must be guarded behind lifecycle hooks that only run on the client (`mounted`, `onMounted`) or behind `typeof window !== 'undefined'` checks.
- **Vuex state must be serializable:** The server serializes the entire Vuex state to `window.__INITIAL_STATE__` and the client restores it. Functions, circular references, and non-serializable values will break hydration.
- **Factory-function state is mandatory:** Vuex modules must use `const state = () => ({...})` (factory function), not a plain object. Without this, all SSR requests share the same state object, causing cross-request data leakage.

#### The Dual-Mode Renderer

In development, Vite runs as Express middleware with HMR — changes to Vue files hot-reload instantly without a full page refresh. In production, pre-built server and client bundles are used, with a client manifest driving preload link generation for fonts, CSS, and JS chunks.

This distinction matters for debugging: dev-mode errors may look different from production errors because the code paths diverge at `vueSsr.js`.

### Express Routing

**Reference:** `rules/express-routing.md`

All HTTP traffic flows through a three-file pipeline:

1. **`before.js`** — Pre-processing middleware that runs before any route handler. It extracts and processes query parameters: promo codes (`?offerCode=`), referral campaigns (`?campaignid=`), advisor IDs, and Extole sharing parameters. Uses `async.autoInject()` for ordered async processing.

2. **`endpoints.js`** — REST API routes. The primary pattern is `/api/:module/:method`, which dynamically dispatches to `webservices[module][method]`. Modular endpoint files in `website/src/routing/endpoints/` handle specific domains (cart, user, color-advisor, contact, support, reward). Response format is `{ success: 0/1, err: "message", data: ... }`.

3. **`views.js`** — Page routes. Handles both legacy Pug renders (`res.render('template', data)`) and Vue SSR renders (via the CMS module and `vueSsr.js`).

#### Relationship to SSR

When adding a new page, you need both `express-routing.md` (to define the route in `views.js`) and `ssr-architecture.md` (to understand how the page gets rendered and hydrated). Adding just an API endpoint requires only `express-routing.md`.

#### Module Resolution

Controllers, models, and shared utilities in `mr_modules/` are accessed via `NODE_PATH` — `require('controllers')`, `require('config')`, `require('dataAccess')` work without relative paths. This is a legacy pattern that affects all server-side code.

---

## Dynamic Yield Integration

**Reference:** `rules/dynamic-yield.md`

Dynamic Yield (Experience OS) is the primary engine for personalization and A/B testing. It operates on a fundamentally different lifecycle from the application code — developers build reusable **Templates**, and business teams launch **Campaigns** using those templates without requiring code deployments.

### Why DY Is a Separate Rule

DY integration has its own API (Experience API, `/v2/serve/user/choose`), its own event system (`dyType` events for purchase, add-to-cart, identification), its own user identity model (DYID for device, CUID for cross-device), and its own set of technical constraints (10,000 character line limits, forbidden `DY.`/`DYO.` prefixes, mandatory minification).

It doesn't directly interact with the Vue component system — it's a content delivery mechanism whose responses are consumed by the Vue layer. The DY rule is self-contained and only needs to be loaded when the task explicitly involves DY campaigns, templates, or the Experience API.

### Template Development Caution

Editing a DY Template propagates to **every linked Variation** across all campaigns. This is the highest-risk operation in the DY ecosystem. The rule file covers the safety protocols: version control via Activity Logs, unlinking vs. deleting variations, variable sync behavior, and the requirement to always use templates over "No Template" variations.

---

## Rule Dependencies and Loading Strategy

### The Dependency Graph

```
vue-patterns.md (core — most frequently needed)
├── pug-templates.md (if writing/editing templates)
├── testing-standards.md (if writing/fixing tests)
├── Styling rules (independent siblings, load by keyword match):
│   ├── spacing-utilities.md (margin, padding)
│   ├── typography-utilities.md (font, color, text)
│   ├── flexbox-layout.md (flex, layout, display, visibility)
│   └── utility-classes.md (position, border, cursor, animation, a11y)
├── ssr-architecture.md (if SSR/hydration involved)
├── express-routing.md (if adding routes/endpoints)
└── dynamic-yield.md (if DY campaigns/templates involved)
```

### Loading Principles

1. **`vue-patterns.md` is the hub.** Most tasks that touch frontend code will need it. When in doubt, load it.

2. **Styling rules are additive and non-conflicting.** They cover four orthogonal domains with zero overlap. A single template line often uses classes from 3-4 of them simultaneously. When the styling task is specific ("fix the margin"), load only the relevant rule. When it's broad ("fix the styling"), load all four.

3. **Testing always pairs with vue-patterns.** You cannot write correct test mocks without understanding the component patterns (stores, modals, services, global mixins) that the component depends on.

4. **Infrastructure rules are standalone.** `ssr-architecture.md`, `express-routing.md`, and `dynamic-yield.md` are only needed when the task explicitly enters their domain. They do not overlap with each other.

5. **When in doubt, load more.** The cost of loading an unnecessary rule file (~100-200 lines of well-structured reference) is far lower than the cost of generating incorrect code because the right reference was missing.
