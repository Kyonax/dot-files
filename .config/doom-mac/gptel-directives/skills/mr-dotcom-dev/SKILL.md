---
name: mr-dotcom-dev
description: "Madison Reed Dotcom team frontend and fullstack development standards for the website/ directory. Vue 3 Options API components (props, emits, computed, methods, watch, lifecycle), Pug templates (lang=pug, extends, block, kebab-case tags, slots, v-if, v-for), Vuex 4 namespaced store modules (mapState, mapGetters, mapActions, mutations, cross-module dispatch), modal system (showModal/hideModal dispatch, AppModal, persistent, theme), API services (vue*Svc.js, mrApi, axios wrapper), scoped Stylus styles (design system variables, breakpoint mixins, rem/em units), utility-first CSS classes (spacing .mt-100m .px-50m, typography .f-secondary .xs-f-large .color-mr-purple, flexbox .flex .flex-col .space-between .gap-md, layout .full-width .xs-hide, positioning .hv-center .clickable .border-radius-6, accessibility .ada-tooltip .hiddenButPresent), SSR architecture (Vite, vueSsr.js, entry-server, entry-client, hydration, __INITIAL_STATE__), Express routing (before.js, endpoints.js, views.js, /api/:module/:method), Dynamic Yield personalization (Experience API /v2/serve/user/choose, campaigns, templates, A/B testing, DYID/CUID), testing (Vitest, Vue Test Utils, shallowMount, createWrapper, mocking, no snapshots, matchMedia mock, stubGlobal), global components (MrBtn, MrIcon, ImgBox, AppModal), global mixins (trackMREvent, trackMREventAndRedirect, scrollTo, goToPath), directives (v-ripple, v-click-outside, v-mask), composables over mixins, path aliases (@components, @services, @store, @utilities), ADA accessibility (native button focus, no nested interactives, aria-hidden, keyboard navigability), performance (window.matchMedia over resize, module-level constants, no magic numbers, no inline event logic), CMS data validation (parent filtering, v-if guards, skeleton loading). Trigger when: creating/editing/reviewing Vue components, writing Pug templates, styling with utility classes, working with Vuex stores, writing or fixing tests, adding Express routes or API endpoints, debugging SSR/hydration, refactoring frontend code, reviewing Dotcom PRs, working in website/src/, or any file under website/src/vuescripts/."
metadata:
  author: Kyonax
  version: "3.0.0"
  team: Dotcom
---

# MR Dotcom Development Skill

Standards for Madison Reed's consumer website (`website/`). For architectural rationale and rule interconnections, see `AGENTS.md`.

## Rule Index

Match the task against **Signal Keywords**. If any keyword appears, load that rule.

| Rule | Signal Keywords |
|---|---|
| `rules/vue-patterns.md` | component, prop, emit, computed, method, watch, mounted, data(), Options API, Vuex, store, mapState, mapGetters, mapActions, mutation, dispatch, modal, showModal, hideModal, service, vue*Svc, mrApi, ImgBox, MrBtn, skeleton, trackMREvent, trackMREventAndRedirect, dayjs, composable, mixin, ARIA, keyboard, semantic HTML, matchMedia, resize, isDesktop, global/isDesktop, inline event, magic number, nested interactive, native button, focus, :deep(), @hook, @ready, aria-labelledby, v-if guard, CMS validation, placeholder, TODO |
| `rules/pug-templates.md` | pug, template(lang="pug"), extends, block, include, layout, views/, kebab-case tag, slot, v-if, v-for, :is= |
| `rules/testing-standards.md` | test, .test.js, vitest, jest, shallowMount, mount, createWrapper, createMockStore, vi.fn, vi.mock, expect, assertion, mock, stub, $nextTick, matchMedia mock, stubGlobal, mockReturnValue, emit before redirect |
| `rules/spacing-utilities.md` | margin, padding, spacing, .mt-, .mb-, .mx-, .my-, .pt-, .pb-, .px-, .py-, -100m, -50m, -pct |
| `rules/typography-utilities.md` | font, color, .f-primary, .f-secondary, .f-domaine, .bold, .upper, .color-mr-, brand-color, text-color, cta-color, ui-color, feedback-color, mister-color, cw-color, letter-spacing, line-height, heading style, Stylus variable, breakpoint, mixin, mq-mobile, mq-tablet, mq-desktop |
| `rules/flexbox-layout.md` | flex, .flex, .flex-col, .flex-box, .space-between, .align-center, .flex-1, .no-shrink, .gap-, aspect ratio, a-box |
| `rules/utility-classes.md` | display, visibility, .hide, .xs-hide, .lg-only, position, float, .v-center, .h-center, .hv-center, width, height, max-width, .full-width, .full-height, overflow, rotate, .clickable, cursor, border-radius, divider, shadow, .shake, .spin, text-align, .text-center, .ada-tooltip, .hiddenButPresent, .rich-text-format, .no-scroll, sizing |
| `rules/ssr-architecture.md` | SSR, hydration, Vite, vueSsr, entry-server, entry-client, __INITIAL_STATE__, renderToString, madison.js, HMR |
| `rules/express-routing.md` | route, endpoint, /api/, Express, before.js, endpoints.js, views.js, middleware, REST, controller, NODE_PATH |
| `rules/dynamic-yield.md` | Dynamic Yield, DY, personalization, A/B test, campaign, variation, Experience API, /choose, DYID, CUID |

## Disambiguation

1. **"Create/edit a component"** → `vue-patterns.md` + `pug-templates.md`. Add styling rules only if visual changes mentioned.
2. **"Fix styling"** without specifics → Load all four styling rules.
3. **"Fix styling"** with specifics → Match keyword: margin/padding = spacing, font/color/variable/breakpoint = typography, flex/gap/aspect-ratio = flexbox, display/visibility/position/border/sizing/overflow = utility-classes.
4. **"Write tests"** → `testing-standards.md` + `vue-patterns.md` (need to understand patterns being tested).
5. **"Add a page/route"** → `express-routing.md` + `ssr-architecture.md` + `pug-templates.md`.
6. **"Debug SSR/hydration"** → `ssr-architecture.md`. Add `vue-patterns.md` if Vuex state involved.
7. **"Review PR"** → `vue-patterns.md` as baseline + rules matching changed file types.
8. **Styling rules are additive** — loading one does NOT replace another.

## Task Recipes

| Task                                  | Rules                                                                                          |
|---------------------------------------|------------------------------------------------------------------------------------------------|
| New Vue component (full)              | `vue-patterns`, `pug-templates`, `spacing-utilities`, `typography-utilities`, `flexbox-layout` |
| New component (logic-only)            | `vue-patterns`                                                                                 |
| Add modal / Vuex module / API service | `vue-patterns`                                                                                 |
| Fix spacing or alignment              | `spacing-utilities`, `flexbox-layout`                                                          |
| Fix text or color                     | `typography-utilities`                                                                         |
| Add Express endpoint                  | `express-routing`                                                                              |
| Add new page (full stack)             | `express-routing`, `ssr-architecture`, `pug-templates`, `vue-patterns`                         |
| Write component tests                 | `testing-standards`, `vue-patterns`                                                            |
| DY campaign integration               | `dynamic-yield`                                                                                |
| Debug SSR hydration                   | `ssr-architecture`, `vue-patterns`                                                             |
| Accessibility audit                   | `vue-patterns`, `utility-classes`                                                              |
| Code review                           | `vue-patterns` + rules matching changed files                                                  |
