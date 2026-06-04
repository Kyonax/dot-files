<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the DOTCOMPB-8121 (Marketing LP — Services Section) session. It is loaded at the start of every conversation to give the AI full context without re-discovering anything.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, patterns, conventions for ALL work in this session. | Before any code task. |
| **2. Session Overview** | Scope, decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-feature detail: files, trees, decisions, tests. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference file path table. | When reading, editing, or locating files. |
| **5. Last Interaction** | Short-term memory: last work, pending, resume points. | At conversation start. |
| **6. Activity Log** | Datetime-stamped, append-only audit trail of every meaningful event. | When you need exact "what was done when". |

**Operational Rule:** Always look for the last request identified by `###` title. Load relevant skills and apply Section 1 rules.

**Architectural baseline:** This session **inherits** from two parents:

1.  `site-revolution-redesign.md` — pattern catalog for the entire HCB / Site Revolution program (Sections 1.1 – 1.18).
2.  `dotcompb-8120-marketing-lp-hero.md` — sibling session for the Hero Section block of the SAME Marketing LP wrapper. Sections 1.1 – 1.44 there describe **the exact playbook this ticket uses** for block authorship inside `LocationSpecificColorbarV2`. The Services Section ships as the second block inside the V2 `v-if` branch of that same wrapper. **Do not re-derive any of those patterns — load them by reference.**

**Cross-session references** use `[session: filename > section-N.M]` syntax — see `~/.claude/skills/session-memory/rules/reference-syntax.md`.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `mr-dotcom-dev` (Vue/Vuex/Pug/Stylus), `mr-style` (design system classes), `code-review` (quality analysis), `mr-roam-node` (ticket documentation), `pr-scribe` (PR body authoring), `tophat-tools` (CMS operations).
>
> Section 1 is **inherited by reference** from the 8120 session so the playbook stays in one place. Each row below names the inherited subsection, summarises why it applies to Services, and flags any delta. When working on Services, *read the inherited subsection first, then the delta*.

### 1.0 Inheritance map — DOTCOMPB-8120 § → DOTCOMPB-8121

| Inherited section | Title | Why it applies to Services | Delta for this ticket |
|---|---|---|---|
| `[session: dotcompb-8120-marketing-lp-hero > §1.1]` | Framework & API | Vue 3 Options API + Pug + scoped Stylus across the same wrapper | None |
| `[session: dotcompb-8120-marketing-lp-hero > §1.2]` | Heading & Title Patterns | Section title is an `h2` (hero owns the page `h1`); `.f-secondary.upper.max-at-tweak` mandatory; delegated title-id pattern reusable for service-card sub-headings if any are needed | **Hero owns h1. Services Section's title must be `h2`. No second `h1` on the page** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.3]` | Utility-First Styling | Same rules: utilities first, alphabetical class order, design-system vars only, `px` only for borders/shadows | None |
| `[session: dotcompb-8120-marketing-lp-hero > §1.4]` | Breakpoint Strategy | `global/isDesktop` for show/hide; no `window.resize`; `100dvh` for any sticky elements | None |
| `[session: dotcompb-8120-marketing-lp-hero > §1.5]` | Accessibility | Self-contained landmarks; `aria-labelledby` references heading IDs only; native semantics; `role="link"` for multi-content cards; `aria-expanded` always renders | **Service cards are multi-content interactive containers (title + price + CTA). Use `role="link"` + `tabindex="0"` + `@keydown.enter.prevent` — NOT raw `<a>` wrapping the whole card** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.6]` | Form / Input Patterns | n/a — no form input in Services | Skip |
| `[session: dotcompb-8120-marketing-lp-hero > §1.7]` | CMS Configuration | `cmsSettings` prop; `v-if` guards; `ImgBox` for all images; skeleton via `:deep(.image-box)` background; `white-space: pre-wrap` for textareas | **Section consumes `cmsSettings.servicesSection.*` — pricing-context fields (member/non-member) + variable-promo-toggle field + per-service promo applicability flag(s)** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.8]` | Tracking | `trackMREvent` for stay-on-page; `trackMREventAndRedirect` for hard redirects with 300ms delay; never pass `isFrontEndEvent: true`; track exposure explicitly | **New required events (see §2.4 — JIRA AC):** `MREvent (Marketing LP – Membership Learn More clicked)` (stay — opens modal), `MREvent (Marketing LP – Service CTA clicked)` (hard redirect to locations services step) with `{serviceCode, serviceName, memberPrice, nonMemberPrice}` |
| `[session: dotcompb-8120-marketing-lp-hero > §1.9]` | Experiments | Experiment-gated mounting inside wrapper; SSR-empty `this.experiments`; cookie-sticky 30 days | **Gate is upstream at Tophat `componentList` (per `[…8120 > §1.23]`). Services Section does NOT carry its own experiment v-if — wrapper mounts it directly. Re-confirm before adding any in-component gate** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.10]` | Component Placement & Naming | Self-explanatory generic names; folder structure `ComponentName/ComponentName.vue` + `index.js`; co-located `.test.js`; self-sufficient spacing | **Component lives at `LocationSpecific/LocationSpecificColorbarV2/components/ServicesSection/`. Service-card extraction trigger per `[…8120 > §1.22]` — if a card has ≥3 non-trivial concerns and a named future consumer (it does — 4 cards on this section), extract it as `ServiceCard.vue`** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.11]` | Testing | `cd website && npm run test:vue {component_name}`; no snapshots; `shallowMount` default; mocks before import; `vi` explicit import | None |
| `[session: dotcompb-8120-marketing-lp-hero > §1.12]` | PR Workflow | `/create-pr`; PR title `[DOTCOMPB-8121]: Services Section — Marketing Landing Page (Site Revolution HCB LP)`; labels `DOTCOM TEAM`, `Pending Code Review`; branch `DOTCOMPB-8121` | **Branch name = `DOTCOMPB-8121`. PR opens against `feat-location-s`** (same feature branch as hero — siblings of the same epic) |
| `[session: dotcompb-8120-marketing-lp-hero > §1.13]` | Code Review Checklist | `/code-review` before PR; verify single h1 on page (hero owns it) | **Watch:** (a) service-card focus indicators ≥3:1; (b) Pro+ modal opens without trapping focus before paint; (c) membership callout `aria-label` distinguishes it from service-card CTAs |
| `[session: dotcompb-8120-marketing-lp-hero > §1.14]` | Utility-class rules — memory takes precedence | `{color}-bg` not `bg-{color}`; **flex stays in scoped Stylus**; em-based spacing; breakpoint mixin `@media mq-desktop-plus` | None |
| `[session: dotcompb-8120-marketing-lp-hero > §1.15]` | Tophat field-naming convention | Top-level fields on template 1650 are camelCase; field name = camelCase Vue component name | **New top-level field `servicesSection` on template 1650. Nested fields: `servicesSection.title`, `servicesSection.subtitle`, `servicesSection.membership.{copy, ctaText}`, `servicesSection.services[]` (array), `servicesSection.variablePromo.{enabled, applicableServiceCodes[], copy}`** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.16]` | Utility-class realities | Narrower than skill docs claim; no `.p-{N}m`/`.m-{N}m` shorthand; no responsive `.max-width-rel-*`; Domaine Display Condensed for the section title | None |
| `[session: dotcompb-8120-marketing-lp-hero > §1.17]` | Vue scoped-CSS gotchas with child component classes | `.parent :deep(.child)` requires a true ancestor wrapper; MrBtn hover sets `color: color-white` — re-set on `&:hover, &:active, &:focus` when overriding bg | **Service-card CTA buttons are MrBtn — same hover-override discipline applies** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.18]` | Layout stability — image cover-fit | Anti-flicker pattern (relative parent + absolute `:deep(.image-box)` + aspect-ratio fallback + flex `min-width: 0`) | **If service cards have images per Figma, apply this pattern verbatim** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.19]` | Google Maps lazy-load | n/a — Services has no Maps integration | Skip |
| `[session: dotcompb-8120-marketing-lp-hero > §1.20]` | SVG icon authoring | `viewBox` only, `currentColor` stroke/fill, auto-discovered via `import.meta.glob` | **Any new icons (e.g. membership badge, service icons) follow this rule** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.21]` | SVG load latency | Dev-only artifact — don't optimize source bytes | None |
| `[session: dotcompb-8120-marketing-lp-hero > §1.22]` | Component abstraction trigger | ≥3 concerns + named future consumer + ≤7 props + heading-level prop + `resolvedX` computed for ARIA + constants-vs-prop-defaults discipline | **Extract `ServiceCard.vue` — 4 identical instances on this section, multiple concerns (price pair, ARIA, CTA, tracking, image), clear contract. Possibly also extract `MembershipCallout.vue` if it has reuse potential** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.23]` | External experiment gating via Tophat componentList | Variant B's componentList already includes the LSv2 wrapper; no internal v-if needed | None — Services mounts unconditionally inside the V2 wrapper |
| `[session: dotcompb-8120-marketing-lp-hero > §1.24]` | CMS Partials | 3 moving parts: template, content, mount; global Vue tag registration; 7-step scaffolding via `tophat-tools` scripts | **Decision needed: is the variable-pricing promo a CMS partial (reusable per campaign) or inline `v-if` against `servicesSection.variablePromo.enabled`? Default to inline unless marketing wants per-campaign authored copy. Park the partial-vs-inline question for §2.3 below.** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.25]` | Pug boolean-attribute gotcha for Vue bindings | `String(settings.bool)` coercion required when forwarding boolean CMS fields to Vue bindings via Pug | **Applies if `servicesSection.variablePromo.enabled` is forwarded through a partial. Inline use is unaffected** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.26]` | `.bold` utility is family-swap | `.bold` = Averta-Bold family; pair with `.f-primary` for clarity | None |
| `[session: dotcompb-8120-marketing-lp-hero > §1.27]` | Stash-then-flush pattern for cross-page promo apply | Click stashes via Vuex action + URL param fallback; destination reader writes to same stash; downstream consumer prepends on every API call; clear-on-success-only | **The Service CTA hand-off to the locations page IS cross-page service pre-selection (AC1 last bullet) — mirror this pattern using `selected_service` cookie + `setServiceFromCookie()` precedent (see decision-5 in `[…site-revolution-redesign > ad-005]`)** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.29]` | `notifySuccess` / `notifyError` toast | Root-level Vuex actions for transient confirmations; never build a confirmation modal for single-button OK | **AC4 vocabulary says "modal" — the Pro+ modal is genuinely a modal (form-like content, multi-step). Don't substitute a toast. Reverse direction from the hero ticket** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.30]` | ADA `:focus-visible` outline pattern | Two-color ring (outline + box-shadow) for elements on colored backgrounds; `outline-offset: 2px`; never `:focus`-only | **Apply verbatim to service-card focus state — they're on the section background** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.31]` | Tophat field type `partial` | Object shape `{cms_partial: "..."}` not string; partial picker UX in Tophat | Only if §1.24 decision lands on "partial" |
| `[session: dotcompb-8120-marketing-lp-hero > §1.32]` | CMS partial content shape — no `content.templateKey` | Forbidden field on partial content docs; causes dev-ssr crash via `loaders.js:51` legacy branch | Only if creating a partial for variable promo |
| `[session: dotcompb-8120-marketing-lp-hero > §1.33]` | `vue-component-list-ssr` baked-snapshot caching | Parent contentVersion's `componentList[N].settings` is the rendered shape — seed on both `published_version` AND `edit_version` | **CRITICAL when adding `servicesSection` to the parent template: the new field must be seeded into cv 19460 (published v55) AND cv 19464 (edit v56) on `content._id 3117`, AT BOTH the `templateData.servicesSection.*` path AND the `componentList[lsv2].settings.servicesSection.*` snapshot** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.34]` | Module-scoped counter for stale-response guards | `let actionRequestId = 0` at module scope; atomic `++` in JS; never reset in stale-bail paths | n/a — Services pricing reads from already-resolved state, no async race expected. Skip unless pricing fetch is added |
| `[session: dotcompb-8120-marketing-lp-hero > §1.35]` | No comments in `.vue` or `.test.js` (strict) | Zero comments — even pre-existing — in any `.vue` / `.test.js` file in the diff. Backend `.js` allows hyper-concise WHY-only | None |
| `[session: dotcompb-8120-marketing-lp-hero > §1.36]` | Promo code is functional, never UI display | The promo *code* is never shown; the user-facing copy is `promoName` (`"20% off your first service"`); URL hand-off carries both | **Applies if the variable promo's display copy is a percentage-off — show `variablePromo.copy` (CMS), not a raw discount code** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.37]` | Toast copy is CMS-customizable | `renderPromoToast` + `?promoToast=` URL hand-off; placeholders `{promoName}` / `{promoCode}`; default + fallback fallback chain | n/a unless Services adds its own promo toast — currently no toast in AC |
| `[session: dotcompb-8120-marketing-lp-hero > §1.38]` | Tophat partial canonical document shape — 11 gotchas | Reference table; auto-injected by patched `create-partial-{template,content}.mjs`; `link`-type stores `{url, text}` object; `xsClass: "col-xs-12"` mandatory | Only if §1.24 lands on "partial" |
| `[session: dotcompb-8120-marketing-lp-hero > §1.39]` | Partial slug split — generic template, specific content | Template `partial-foo` reusable; content `partial-foo-campaign` specific | Only if §1.24 lands on "partial" |
| `[session: dotcompb-8120-marketing-lp-hero > §1.40]` | Never duplicate CMS-authored defaults as hardcoded prop defaults | Empty prop defaults when CMS owns the value; document explicitly when dual-mount | **`ServiceCard.vue` props default to `''` / `false` / `null` / `0` — the parent `ServicesSection` reads CMS and shapes the prop payload** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.41]` | Empty prop defaults can break composed strings | Ternary the prefix in/out; regression test asserts `!startsWith(separator)` | **Apply when composing `aria-label` strings for service cards (e.g. CTA + serviceName + price)** |
| `[session: dotcompb-8120-marketing-lp-hero > §1.42]` | SSR IP-loopback hazard | Never call IP-dependent endpoints in `serverPrefetch`; client `mounted` only | n/a — Services consumes already-resolved `state.location` / `closestLocations` |
| `[session: dotcompb-8120-marketing-lp-hero > §1.43]` | Stale-response guard ownership semantics | Newest call owns the in-flight flag; stale paths never reset it | n/a unless async pricing fetch is added |
| `[session: dotcompb-8120-marketing-lp-hero > §1.44]` | PR review comment etiquette | Reply in-thread with `-F in_reply_to=<id>`; 2-4 sentence concise replies; lead with verdict | None |

### 1.45 Pricing source resolution — new pattern for this ticket

Services must show member and non-member pricing per service. Two upstream signals are available from the wrapper's already-wired Vuex (no new actions needed):

1.  **`state.colorbar.closestLocations[0]`** — when populated AND `distance <= 50`, the hero has surfaced a nearby salon (per `[…8120 > §3.2]`, the wrapper's IP/`initializeBopis` chain).
2.  **`state.colorbar.location`** — populated by `colorbar/loadLocation(code)` when the wrapper dispatches it. *(NOTE: the hero refactor removed the wrapper's `loadLocation` watcher per `[…8120 > §3.2]` post-refactor; re-add it gated on Services' need before relying on it. Confirm in planning pass.)*

**Decision logic** (closes AC2 / AC3):
* If `state.location` is populated → use `state.location.servicePrices.{memberPrice, nonMemberPrice}` per service (verify exact field path during planning — likely `state.location.services[].price` keyed by `serviceCode`).
* Else if `closestLocations[0]` is within 50 mi → use `closestLocations[0].servicePrices.*` (verify field availability — if missing, fall back to base).
* Else → use `cmsSettings.servicesSection.services[].basePrice.{member, nonMember}` from Tophat.

**Anti-pattern:** do NOT fetch a separate pricing endpoint per service when rendering. The wrapper has the data; Services reads `mapState`. Async fetch only if planning reveals the price fields are not on the existing `location` shape.

### 1.46 Service card → locations-page services-step hand-off

JIRA AC1 final bullet: "CTA on each → drives to the locations page and preserves the selected service data on the services step". This is cross-page service pre-selection — the exact pattern in `[…site-revolution-redesign > ad-005] cookie-based-service-preselection`.

**Apply verbatim:**
* Click handler: set `selected_service` cookie (`{serviceCode, serviceName}` or just `serviceCode` per existing precedent) + `trackMREventAndRedirect('MREvent (Marketing LP – Service CTA clicked)', '/colorbar/locations#services', {...})`.
* Consumer: the locations page's services-step component already reads `selected_service` cookie via `setServiceFromCookie()` — **verify the consumer exists before shipping** (it's the documented contract per the parent session's decision).
* If consumer is missing or has drifted, the hand-off is broken — escalate before shipping.
* `trackMREventAndRedirect` includes `{serviceCode, serviceName, memberPrice, nonMemberPrice}` per the JIRA event-tracking table.

### 1.47 Membership "Learn More" → Pro+ modal hand-off

AC4 says "the Pro+ modal from version B of the booking flow should open". The Pro+ modal already exists in booking flow v2 — reuse, do not rebuild.

**Action:** dispatch `modal/showModal` with the existing Pro+ modal component reference (search `website/src/vuescripts/components/HairColorBarBookingV2/` for the membership modal component name during planning pass). The standard modal payload shape is documented in `.claude/CLAUDE.md`:

```js
this.$store.dispatch('modal/showModal', {
  component: ProPlusModal, // resolved during planning
  theme: 'default',
  props: { /* whatever the modal needs */ },
  ariaLabel: 'Pro+ membership details',
  returnFocusElement: this.$refs.membershipLearnMoreBtn,
});
```

**ADA contract:**
* `aria-label` on the trigger button distinct from any service-card CTA aria-label.
* `returnFocusElement` set so focus returns to the Learn More button on close.
* Track via `trackMREvent('MREvent (Marketing LP – Membership Learn More clicked)')` — *stay-on-page* event, no redirect (per `[…8120 > §1.8]`).

### 1.48 Embedded 8120 Playbook — AC-relevant excerpts (self-contained)

> The §1.0 inheritance map references 8120's §1.1–1.44 by *title*. This sub-section embeds the actual *content* the Services AC needs, reorganized by AC so the Worker doesn't have to cross-reference. When a pattern below cites a numbered 8120 section, that's the full canonical source — use this as the working reference and only re-open the 8120 session for edge cases not covered here.

#### 1.48.1 Wrapper contract — the host of Services

The `LocationSpecificColorbarV2` wrapper already exists (PR #20750 foundation + PR #20771 hero) and is the parent of every Site Revolution LP section. Services mounts as the *second block* inside the V2 `v-if` branch, below `HeroSection`.

**Vuex wiring at the wrapper** (Services reads via `mapState` — no prop-drilling):

| Module | State / Action | What it gives Services |
|---|---|---|
| `mapState('colorbar', ['closestLocations', 'mapLocations'])` | Nearby salons (sorted by distance, `.distance` field) | Tier-2 pricing source (per `state.colorbar.closestLocations[0]` within 50 mi) |
| `mapState('colorbar', ['location'])` | Resolved "the location for this page" | Tier-1 pricing source — *only populated* if some consumer dispatches `colorbar/loadLocation(code)` |
| `mapState('global', ['isDesktop'])` | Breakpoint flag (mainAppMixin, throttled) | Show/hide logic at 960px+ |
| `mapState('global', ['MRConfig'])` | Runtime config | Use this, not `this.$root.MRConfig` (per 8120 decision #38) |

**`loadLocation` watcher — REMOVED in PR #20771 commit `7077fcd` as "speculative coupling"** ([…8120 > §3.2, decision #33]). Services may need to re-add it; that's open question §2.3 #3. If re-adding:

```js
watch: {
  closestLocations: {
    handler(locs) {
      const closest = locs?.[0];
      if (closest?.code && typeof closest.distance === 'number' && closest.distance <= NEARBY_RADIUS_MILES) {
        this.loadLocation(closest.code).catch(() => {});
      }
    },
    immediate: true,
  },
},
methods: { ...mapActions('colorbar', ['loadLocation']) },
```

**SSR / experiment timing — the wrapper has no SSR for in-experiment paths:**
* `this.experiments` is `{}` during SSR; populated post-`mounted()`.
* `getClosestLocationsByIp` runs in `serverPrefetch` (no DOM); `initializeBopis` (customer → geolocation → IP fallback) runs in client `mounted` to upgrade past the IP-only SSR result.
* Per Tophat `componentList` gating ([…8120 > §1.23]), the wrapper mounts whatever the variation's componentList includes — no in-component experiment v-if needed.

#### 1.48.2 AC1 — Build the section layout (Pug + scoped Stylus + utilities)

**Framework discipline ([…8120 > §1.1]):**
* Vue 3 **Options API only** (`export default { ... }`). No Composition API, no `<script setup>`.
* `<template lang="pug">`, `<style lang="stylus" scoped>`.
* JS: always brackets for `if` even single-line returns.
* Composables over mixins. Never create new mixins.

**Heading rules ([…8120 > §1.2]) — Services title is `h2` (hero owns `h1`):**
* `.upper` MANDATORY on every `.f-secondary` heading (Kapra Neue is uppercase by design).
* `.max-at-tweak` MANDATORY on every responsive font class (`.xs-f-*`, `.sm-f-*`, `.md-f-*`, `.lg-f-*`, `.xl-f-*`, `.font-*`) — caps flex-sizing so fonts don't grow infinitely at huge viewports.
* Inline text format: `h2.classes Title` not `h2.classes\n| Title`. Dynamic interp inline: `h2.upper Hello, {{ user.name }}`.
* Consolidate when **utility count > 4** — move font-family / text-transform / color into the scoped `.section-title` class; keep only responsive font-size classes as utilities.
* Delegated heading via `titleId` prop only when the H lives in a child:
  ```pug
  ServicesSectionHeader(:title="cmsTitle" title-id="services-section-title")
  //- inside: h2(:id="titleId") {{ title }}
  ```

**Utility-first styling ([…8120 > §1.3, §1.14, §1.16]):**
* Utility classes first, Stylus only for what utilities can't express. Aggressively consolidate common classes on parents (DRY).
* Font sizes / families / colors live **ONLY in template** as utility classes — never in `<style>` blocks (`.xs-f-small`, `.bold`, `.text-color-2`, `.color-mr-purple`).
* Responsive padding/margin via utility prefixes — `.px-100m.xl-px-400m`. Never `@media` rules in Stylus for spacing.
* **Padding/margin utility classes MUST use breakpoint prefix** — `.xs-pt-50m` not `.pt-50m` (mobile-first, decision #68).
* Em-based: `val/100 = em` so `.px-100m` = `1em`, `.py-150m` = `1.5em`.
* Scale: `0,10,15,25,30,50,75,100,125,150,175,200,225,250,275,300,350,400,450,500,600,700`.
* **No `.p-{N}m` / `.m-{N}m` shorthand** — use directional only.
* `.gap-{N}` auto-generated 1-30 in px. Above 30 use named scale (`.gap-xs/sm/md/lg`).
* **Background-color pattern is `{color}-bg`, NOT `bg-{color}`** — `.brand-color-1-bg`, `.ui-color-1-bg`, `.cta-color-1-bg`. `.color-white-bg` does NOT exist — use `.ui-color-1-bg` for white bg.
* Class ordering: alphabetical in Pug, except structural/positioning classes which may precede.
* `px` only for borders / box-shadows. Default `rem`/`em`.
* Design system variables only — `brand-color-*`, `cta-color-*`, `text-color-*`, `ui-color-*`. Never hardcode hex unless no exact var exists.
* **Primary Orchid is `.cta-color-1` (#911885)**, NOT `.brand-color-4` (which is the lighter "Bright orchid" #b666a9).

**Flex stays in scoped Stylus (`feedback_no_flex_utility.md` user memory — overrides `/mr-dotcom-dev` skill docs):**
* `display: flex`, `flex-direction`, `flex-grow`, `flex: N N 0`, `align-items`, `justify-content` all go in `<style scoped lang="stylus">`, NOT in `.flex` / `.flex-col` / `.align-center` / `.flex-1` utility classes.
* Reason: Kyo treats flex as structural layout, paired with other layout props in Stylus.
* `width: 100%` → `.full-width` still applies. `overflow: hidden !important` → `.no-scroll`. Color / font / text-align via utility classes. Breakpoint show/hide via `.xs-hide` / `.lg-only` etc.

**Breakpoint strategy ([…8120 > §1.4]):**
* Centralized via `global/isDesktop` (mapGetters('global', ['isDesktop'])) for 960px+ show/hide. Throttled, SSR-safe.
* Local `matchMedia` only when the breakpoint differs from the global getter. Store the `MediaQueryList` in `data` (SSR-safe), add listener in `mounted`, remove in `beforeUnmount`.
* **`window.resize` is forbidden** for responsive logic.
* Utility class breakpoints: `xs-` mobile (default), `sm-` 560px+, `md-` 760px+, `lg-` 960px+, `xl-` 1200px+.
* Mobile sticky CTA uses `100dvh` (Dynamic Viewport Height), not `100vh`.
* Stylus mixin syntax: **`@media mq-desktop-plus`** (NOT `+mq-desktop()`). Available: `mq-mobile`, `mq-mobile-plus`, `mq-tablet`, `mq-tablet-plus`, `mq-desktop-md`, `mq-desktop-md-plus`, `mq-desktop`, `mq-desktop-plus`, `mq-max`.

**Component placement & naming ([…8120 > §1.10]):**
* Self-explanatory generic names — `ServicesSection`, not `MarketingLpLocationSpecificServicesSection`.
* Folder structure: `ComponentName/ComponentName.vue` + `ComponentName/index.js` (barrel export). Tests co-located as `ComponentName.test.js`.
* Short root CSS class — kebab-case (e.g. `.mlp-services-section`).
* Self-sufficient component spacing — the section owns its own `py-150m`. Parent does NOT add wrapper divs with spacing classes (decision #24).

**Component abstraction trigger ([…8120 > §1.22]) — extract `ServiceCard.vue` because:**
* Multiple non-trivial concerns (price pair, ARIA composition, CTA tracking, optional image, promo gating).
* Named future consumer (4 instances on this section; possibly a service-selection grid in the booking flow itself).
* Small contract (≤ 7 props feasible).
* Heading level configurable via `<component :is="headingLevel">`; validate against `VALID_HEADING_LEVELS = ['h1','h2','h3','h4','h5','h6']`.
* `resolvedX` computed pattern for derived ARIA / labels (WCAG 2.5.3 — Label in Name).
* Constants live in *prop default* (not as module-level constants) when only ONE prop reads the value. Module constants reserved for values read in BOTH template/script AND internal logic (e.g., `NEARBY_RADIUS_MILES = 50`).

**Image layout stability ([…8120 > §1.18]) — apply if service cards have images per Figma:**
1. Wrap image in `.media-column { position: relative }`.
2. `:deep(.image-box) { position: absolute; top: 0; right: 0; bottom: 0; left: 0 }` — detaches image from parent height calc.
3. Mobile (stacked layout): definite shape via `aspect-ratio: <design-ratio>`.
4. Desktop: `aspect-ratio: auto`, rely on flex `align-items: stretch`.
5. `min-width: 0` on flex children — without it, intrinsic image pixels blow the column out of its target.
6. `flex: 1 1 0` only inside the desktop media query. Mobile column-reverse stack: let it fall back to default `flex: 0 1 auto`.

**SVG icon authoring ([…8120 > §1.20]):**
* Icons in `website/src/assets/svg-icons/`. Auto-discovered via `import.meta.glob('@icons/*.svg')` — drop in a file, Vite picks it up.
* `viewBox` only — no fixed `width="..." height="..."`. Size comes from the consumer (e.g., `MrIcon`'s `height` prop).
* `stroke="currentColor"` / `fill="currentColor"` on paths — never hardcode hex.

**Vue scoped-CSS gotchas with child components ([…8120 > §1.17]):**
* When you add a class to a child's root from a parent template (`MrBtn.my-class`), the class is on the SAME element as the child's root. So `.my-class :deep(.mrbtn) { … }` matches **nothing**. Fix: put overrides under a *true ancestor* wrapper (e.g., `.mlp-services-section :deep(.mrbtn) { ... }`).
* MrBtn's default `&:hover, &:active, &:focus` applies `setcolor(color-white)`. When overriding `background-color` on hover, you MUST also explicitly re-set `color: <your-color>` on those states — otherwise text becomes white on the new bg and contrast breaks.

**Accessibility ([…8120 > §1.5]):**
* Self-contained landmarks — section owns `role="region"` (or omit if wrapped in `<main>`), `aria-labelledby="services-section-title"`, and the `h2#services-section-title` in its own template.
* **`aria-labelledby` references heading IDs, NEVER root element IDs** (per `feedback_aria_labelledby_pattern.md`).
* No redundant class when ID exists — `h2#services-section-title.f-secondary` not `h2#services-section-title.section-title.f-secondary`.
* Native semantics — never raw `<button>` for navigation. `MrBtn` for true button interactions; `<a>` with `:href` + `@click.prevent` for tracked navigation.
* No nested interactives — never `<a>` inside `role="button"`. Membership Learn More CTA is a button, not a link.
* **`role="link"` for multi-content interactive containers** — service cards (title + price pair + CTA) use `<div role="link" tabindex="0" @keydown.enter.prevent>` NOT raw `<a>` wrapping the whole card (WCAG 2.5.3 — Label in Name; decision #80).
* Dynamic `aria-label` for repeated CTAs — each service card's CTA needs a distinct accessible name (compose from `ctaText + serviceName + price summary`).

**ADA `:focus-visible` pattern ([…8120 > §1.30]) — applied verbatim from mr-minion review on PR #20771:**

```stylus
&:hover,
&:active
  background-color <hovered-bg>
  color <text-on-hover>

&:focus-visible
  background-color <hovered-bg>
  color <text-on-hover>
  outline 2px solid <contrasting-color>
  outline-offset 2px
  box-shadow 0 0 0 4px <surrounding-bg>
```

* `outline-offset: 2px` — keeps outline visible past borders / rounded corners.
* Two-layer ring (outline + box-shadow) — needed when on a colored bg where a single outline color would clash with either element's bg OR surrounding bg.
* **`:focus-visible` (not `:focus`)** — mouse clicks set `:focus` and linger; keyboard Tab sets both. Branching on the latter keeps the keyboard indicator distinct.

#### 1.48.3 AC2 / AC3 — Pricing source resolution

**Decision logic (closes AC2 + AC3):**

| Tier | Source | Condition |
|---|---|---|
| 1 | `state.location.servicePrices.*` per service | `state.location` populated (requires wrapper or Services to dispatch `colorbar/loadLocation(code)`) |
| 2 | `state.closestLocations[0].servicePrices.*` per service | `closestLocations[0]?.distance <= NEARBY_RADIUS_MILES (50)` (boundary inclusive) AND price fields exist on that shape |
| 3 | `cmsSettings.servicesSection.services[].basePrice.{member, nonMember}` | Fallback — base prices from Tophat |

**`NEARBY_RADIUS_MILES = 50`** — module-scoped constant, same value the hero uses for `shouldShowNearestLocation`. Boundary inclusive.

**Anti-pattern:** do NOT fetch a separate pricing endpoint per service when rendering. The wrapper has the data; Services reads `mapState`. Async fetch only if planning reveals the price fields are not on the existing `location` shape.

**SSR IP-loopback hazard ([…8120 > §1.42]) — only if Services adds async pricing fetch:**
* `mrApi.js:90-112` rewrites SSR requests to `${API_HOST}${url}` but does NOT forward `x-forwarded-for` / `req.ip`. The backend sees the SSR server's IP, not the user's.
* Endpoints reading connecting IP (anything calling `getRemoteIp(req)`) must NOT be called in `serverPrefetch` or `created` on SSR. Call from `mounted()` instead.
* Safe in `serverPrefetch`: IP-independent endpoints (global lists, content lookups, locale-by-URL).
* Services consumes already-resolved Vuex state → no fetch needed → no exposure.

#### 1.48.4 AC4 — Membership "Learn More" → Pro+ modal

**AC vocabulary cue ([…8120 > §1.29]):** AC says "modal" → build a modal. AC says "toast" → use `notifySuccess`/`notifyError` Vuex root actions. **AC4 explicitly says "Pro+ modal" → MODAL.** Don't substitute a toast.

**Reuse existing Pro+ modal — search `website/src/vuescripts/components/HairColorBarBookingV2/` for the component during planning pass.** AC4 says "the Pro+ modal from version B of the booking flow" — reuse, do not rebuild.

**Modal dispatch (per CLAUDE.md key patterns):**

```js
this.$store.dispatch('modal/showModal', {
  component: ProPlusModal,
  theme: 'default',
  props: { /* whatever the modal needs */ },
  ariaLabel: 'Pro+ membership details',
  returnFocusElement: this.$refs.membershipLearnMoreBtn,
  persistent: false,
  disableClose: false,
  onClose: () => {},
});
```

**ADA contract:**
* `aria-label` on the trigger button distinct from any service-card CTA aria-label.
* `returnFocusElement` set so keyboard focus returns to Learn More button on close.
* `aria-haspopup="dialog"` on the trigger.

**Tracking:** `trackMREvent('MREvent (Marketing LP – Membership Learn More clicked)')` — *stay-on-page* event, no redirect (per §1.8).

#### 1.48.5 AC5 / AC6 — Variable-pricing promo gating

**Per-service flag pattern (inline default — see §2.3 #1 for partial-vs-inline decision):**

```pug
.promo-badge.f-primary.bold.upper(v-if="showPromo") {{ promoCopy }}
```

```js
computed: {
  showPromo() {
    const promo = this.cmsSettings?.servicesSection?.variablePromo;
    return Boolean(
      promo?.enabled
      && Array.isArray(promo?.applicableServiceCodes)
      && promo.applicableServiceCodes.includes(this.serviceCode),
    );
  },
  promoCopy() {
    return this.cmsSettings?.servicesSection?.variablePromo?.copy || '';
  },
},
```

**Promo code is functional, never UI display ([…8120 > §1.36]):**
* The promo *code* (`WELCOME20`, `FIRST10`) is functional — never shown in UI.
* User-facing copy comes from CMS-authored `variablePromo.copy` (e.g., `"PROMO PRICE"`).
* Anti-pattern: showing `"Promo WELCOME20 saved..."` — leaks the functional token.

**`.bold` is a font-family swap, not a weight modifier ([…8120 > §1.26]):**
* `.bold` = `font-family: f-primary-bold` (Averta-Bold). Not just `font-weight: bold`.
* `.f-secondary.bold` reads as "Kapra family, then override to Averta-Bold". Use `.f-primary.bold` for clarity.
* For the badge text, use `.f-primary.bold.upper` on the parent block; children inherit family/transform.

**If §2.3 #1 lands on "partial":**
* Reference §1.31 (partial field type — value is `{cms_partial: "..."}` object, not string).
* Reference §1.32 (no `content.templateKey` on partials — crashes dev-ssr).
* Reference §1.38 (11-gotcha canonical shape — auto-injected by `tophat-tools` patched scripts).
* Reference §1.39 (partial slug split: generic template, specific content).
* **Pug boolean-attribute gotcha ([…8120 > §1.25]):** if forwarding `enabled` boolean through a partial:
  ```jade
  //- WRONG — Pug renders `true` as attr="attr"
  :enabled=settings.enabled
  //- RIGHT — explicit String coercion
  :enabled=String(settings.enabled)
  ```
* Partial slugs: template `partial-variable-pricing-promo` (generic, reusable); content `partial-services-variable-pricing-<campaign>` (specific).

#### 1.48.6 AC1 final bullet — Service CTA → locations page services-step hand-off

**Cross-page service pre-selection — mirror `[…site-revolution-redesign > ad-005] cookie-based-service-preselection`.** Pattern is the cookie-based cousin of 8120's stash-then-flush promo pattern (§1.27).

**Click handler:**

```js
methods: {
  onServiceCtaClick() {
    document.cookie = `selected_service=${this.serviceCode}; path=/; max-age=3600`;
    this.trackMREventAndRedirect(
      'MREvent (Marketing LP – Service CTA clicked)',
      '/colorbar/locations#services',
      {
        serviceCode: this.serviceCode,
        serviceName: this.serviceName,
        memberPrice: this.memberPrice,
        nonMemberPrice: this.nonMemberPrice,
      },
    );
  },
},
```

**Consumer verification (BLOCKING):** the locations page's services-step component reads `selected_service` cookie via `setServiceFromCookie()` (the documented contract per `[…site-revolution-redesign > ad-005]`). **Verify the consumer exists before shipping** — grep `setServiceFromCookie` in `website/src/vuescripts/`. If missing or drifted, escalate (mirrors 8120's `applySearchQueryHandoff` paired-reader requirement).

**Tracking anti-pattern ([…8120 > §1.8 / decision #18]):** never `trackMREvent() + goToPath()` sequentially — `goToPath` does `location.href` immediately, the event may not flush. Always use `trackMREventAndRedirect` (300ms delay).

**Empty prop defaults can break composed strings ([…8120 > §1.41]) — applies to `ServiceCard.vue`'s composed `aria-label`:**

```js
resolvedCtaAriaLabel() {
  const detail = `${this.serviceName}, ${this.memberPrice} member or ${this.nonMemberPrice} non-member`;
  return this.ctaText ? `${this.ctaText}, ${detail}` : detail;
},
```

**Regression test mandatory:** `expect(resolved.startsWith(',')).toBe(false)` — screen readers announce a leading comma literally, malforming the accessible name (WCAG 4.1.2).

**Never duplicate CMS defaults as hardcoded prop defaults ([…8120 > §1.40]):**
* `ServiceCard.vue` props default to `''` / `false` / `null` / `0`. Tophat owns the user-facing defaults.
* Empty defaults mean composed strings *can* receive empty values — that's exactly why §1.41 guards exist.

#### 1.48.7 AC7 — CMS field changes propagate after publish

**CRITICAL — `vue-component-list-ssr` baked-snapshot caching ([…8120 > §1.33]):**

The parent contentVersion (`content._id 3117`, "Salon-Quality Hair Color Landing Page") carries `templateData.componentList` — an **array of frozen snapshots** of each inner component's `settings`. SSR walks that array; **it does NOT live-read** the inner contentVersion.

* Touching `templateData.servicesSection.*` alone leaves the SSR snapshot stale.
* **Every write must hit BOTH paths**:
  1. `templateData.servicesSection.<field>`
  2. `templateData.componentList.$[c].settings.servicesSection.<field>` with arrayFilters `[{ "c.mixin_key": "location-specific-colorbar-v2" }]`
* **Two versions must stay in sync** — content 3117 has `published_version: 55` (cv 19460, SSR-rendered) AND `edit_version: 56` (cv 19464, Tophat preview/edit-rendered). Seeding only one leaves one rendering path broken.

**Mongosh template (apply per write):**

```js
db.contentVersion.updateOne(
  { _id: <19460 or 19464> },
  { $set: {
      "templateData.servicesSection.<field>": <value>,
      "templateData.componentList.$[c].settings.servicesSection.<field>": <value>
  }},
  { arrayFilters: [{ "c.mixin_key": "location-specific-colorbar-v2" }] }
);
```

**Tophat field-naming ([…8120 > §1.15]):**
* Top-level fields on template 1650 are camelCase. Existing siblings: `title`, `subtitle`, `colorbarTitle`.
* **Top-level field name = matching Vue component name (camelCase first letter).** `ServicesSection.vue` → CMS field `servicesSection`.
* Nested fields under `object` types also camelCase: `servicesSection.title`, `servicesSection.subtitle`, `servicesSection.membership.copy`, `servicesSection.services[]`, `servicesSection.variablePromo.enabled`.
* Other templates may use PascalCase (e.g., template 1625) — don't propagate; match the existing template 1650 siblings.

**Every config entry needs `options.xsClass: "col-xs-12"` ([…8120 > §1.38] row 11)** — or the Tophat content-edit form renders the field at zero width. Especially visible on `link`-type fields.

**Reversal path** — restore-content.mjs from `./cms-backups/3117/<stamp>/snapshot.json` + re-run hero spec JSONs under `.tasks/DOTCOMPB-8120/` recovers the 8120 CMS state when switching branches.

#### 1.48.8 Tracking events (all of AC)

**`trackMREvent(eventName, properties)` — fire-and-forget. Stay-on-page only.**
**`trackMREventAndRedirect(eventName, url, properties)` — track + 300ms delay + redirect.** Always for hard redirects (`location.href`).
**Anti-pattern:** `trackMREvent() + goToPath()` sequentially — race condition, event may not flush.
**Do NOT pass `isFrontEndEvent: true`** — `segmentTracking.js` auto-adds it (per `.claude/rules/coding-standards.md` + decision #72).
**Track experiment exposure explicitly** — `watch` with `immediate: true` or `mounted` fire (Andris guideline 13). The wrapper handles experiment exposure; Services does not duplicate.

**Required events (from JIRA AC):**

| Event Name | Trigger | Properties |
|---|---|---|
| `MREvent (Marketing LP – Membership Learn More clicked)` | Membership callout "Learn More" CTA click | `eventName` |
| `MREvent (Marketing LP – Service CTA clicked)` | Service card CTA click | `serviceCode, serviceName, memberPrice, nonMemberPrice, eventName` |

#### 1.48.9 Testing ([…8120 > §1.11, §1.35])

**Run:** `cd website && npm run test:vue {component_name}` (e.g., `npm run test:vue ServicesSection`).
**Run all before PR:** `cd website && npm run test:vue`.
* **No snapshot tests** — forbidden.
* **`shallowMount` by default** — children stubbed.
* **matchMedia mocking:** `vi.stubGlobal('matchMedia', vi.fn().mockReturnValue(mockMediaQueryObject))` with `addEventListener` / `removeEventListener` spies.
* **Store mocking:** `createMockStore(state, isDesktop)` pattern.
* **Mock globals BEFORE import** — module-level code runs at import time.
* **Emit before redirect** — when testing `trackMREventAndRedirect`, verify the call fires first; the redirect may navigate away.
* **`import { vi } from 'vitest'` explicitly** — PilkoLint flags missing `vi` even when local ESLint doesn't.

**Comment discipline ([…8120 > §1.35]):**
* **`.vue` files: ZERO comments** — Pug `//-`, JS `//`, Stylus `//`, JSDoc `/** ... */` all removed. Even pre-existing comments stripped.
* **`.test.js` files: ZERO comments** — test names already convey intent.
* Backend `.js` (Vuex, utilities, controllers): hyper-concise one-line WHY only.
* `mr_modules/` shared code: standard project JSDoc still applies.

**Required test coverage for Services:**

| File | Target tests |
|---|---|
| `ServicesSection.test.js` | ~15 — title/subtitle render with CMS + v-if guards; service grid renders 4 cards; pricing source switching (3-tier fallback); variable-promo gating (3 cases); membership Learn More dispatches `modal/showModal` + fires `trackMREvent`; tracking event name + payload |
| `ServiceCard.test.js` | ~12 — heading level configurable, ARIA composed correctly with/without ctaText (regression test on leading-comma), member/non-member price both render, image alt fallback, promo badge conditional, CTA sets cookie + fires `trackMREventAndRedirect`, role="link"+tabindex+@keydown.enter activation, focus-visible style |
| `LocationSpecificColorbarV2.test.js` (delta) | +2-3 — wrapper mounts ServicesSection unconditionally; if §2.3 #3 re-adds `loadLocation` watcher, restore the 6-case watcher test suite from PR #20771 commit `501cbee` (since deleted in `7077fcd`) |

#### 1.48.10 PR + Code Review ([…8120 > §1.12, §1.13, §1.44])

**PR Workflow:**
* Skill: `/create-pr` (auto-fetches JIRA, drafts summary, applies labels).
* **PR Title format:** `[DOTCOMPB-8121]: Services Section — Marketing Landing Page (Site Revolution HCB LP)`
* **Labels:** `DOTCOM TEAM`, `Pending Code Review`.
* **Branch:** `DOTCOMPB-8121` off `feat-location-s` (NOT off master).
* **PR base:** `feat-location-s` (same feature branch as hero).
* **Changes section format:** sub-list, one detail per line.
* **Unit Testing table:** one row per individual test case.

**Code Review:**
* Run `/code-review` against every new component before opening PR.
* Verify single `h1` on page (hero owns it; Services uses `h2`).
* Watch: service-card focus indicators ≥3:1 contrast (`:focus-visible` two-color ring per §1.48.2).
* Watch: Pro+ modal opens without trapping focus before paint.
* Watch: membership callout `aria-label` distinguishes it from service-card CTA aria-labels.

**PR review comment etiquette ([…8120 > §1.44]):**
* **NEVER** standalone review comments — always reply in-thread:
  ```bash
  gh api repos/<org>/<repo>/pulls/<pr>/comments \
    -X POST \
    -f body="..." \
    -F in_reply_to=<original_comment_id> \
    -f commit_id="$(git rev-parse HEAD)" \
    -f path="<path>"
  ```
* 2–4 sentences max. Lead with verdict (`Confirmed valid + fixed.` / `Not applicable — would be a regression.`).
* Cite specific file:line. Skip preamble and stack traces.
* If reply gets too long → `gh api .../pulls/comments/<my_id> -X PATCH -f body="..."` to edit.

#### 1.48.11 CMS reshape — re-cap of §3.3 deployment phases (working ref)

| Phase | Purpose | Tool |
|---|---|---|
| **A** | Backup content 3117, template 1650, partial-marketing-lp-hero-20off to `.tasks/DOTCOMPB-8121/backups/<stamp>/` | `inspect-content.mjs`, `inspect-template.mjs`, `inspect-partial.mjs`, `backup-content.mjs` |
| **B** | Strip `heroSection.*` from template 1650 schema | `get-template-fields.mjs --json` → hand-edit → `set-template-fields.mjs --mode replace --confirm` |
| **C** | Strip `heroSection.*` from cv 19460 + 19464 at BOTH paths (`templateData.heroSection` AND `componentList[lsv2].settings.heroSection`) | Direct mongosh `$unset` with `arrayFilters` |
| **D** | Delete partial template 1652 + content 3403 + cv 19462 (PromoBadge — hero-only) | `db.template.deleteOne` / `db.content.deleteOne` / `db.contentVersion.deleteOne` |
| **E** | Add `servicesSection.*` schema to template 1650 (16 fields) | `set-template-fields.mjs --mode merge --confirm` |
| **F** | Seed `servicesSection.*` sample data on cv 19460 + 19464 at BOTH paths | mongosh `$set` with `arrayFilters` on `componentList[lsv2]` |
| **G** | Verify: `inspect-content 3117`, curl SSR HTML, browser smoke with `experiments_504=b` | Read-only |
| **H** | Document production replication for Carley in PR body (Tophat UI steps) | Manual |

**Tophat object inventory pinned to this ticket** (local Mongo, as of 2026-05-12):

| Object | Identity | Action |
|---|---|---|
| `content._id 3117` | "Salon-Quality Hair Color Landing Page" → `/colorbar/location-specific` | Keep |
| `contentVersion._id 19460` | v55 published, variant **B** | Strip hero, add `servicesSection.*` (BOTH paths) |
| `contentVersion._id 19464` | v56 edit, variant **B** | Same — both versions MUST stay in sync |
| `experiment._id 504` | `LocationSpecificSiteRevolution` (Running, B@10000) | Keep |
| `template._id 1650` | `location-specific-colorbar-v2` | Strip `heroSection.*` schema; add `servicesSection.*` schema |
| `template._id 1652` | `partial-promo-badge` | **Delete** (hero-only) |
| `content._id 3403` | `partial-marketing-lp-hero-20off` | **Delete** (hero-only) |
| `contentVersion._id 19462` | content 3403 active version | **Delete** alongside parent |

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Implement the **Services Section** of the new Marketing Landing Page (`/colorbar/location-specific`) under the parent epic *Site Revolution - HCB Landing Page* (DOTCOMPB-8119). The section is CMS-configurable (title, subtitle, membership callout, 4 service cards), shows member + non-member pricing per service, swaps pricing to the nearest salon when one is identified upstream by the Hero Section, hands off the selected service to the booking flow via the existing cookie precedent, opens the Pro+ modal from booking flow v2 on the membership "Learn More" CTA, and gates per-service variable-pricing promo text behind a CMS toggle plus per-service applicability list.

### 2.2 Scope

| Ticket | Type | Summary | Status |
|---|---|---|---|
| `DOTCOMPB-8119` | Epic | Site Revolution - HCB Landing Page | Open (parent). Foundation already shipped via PR #20750. |
| `DOTCOMPB-8120` | Story (3 SP) | Hero Section | PR #20771 OPEN against `feat-location-s` — final QA in progress (see `[session: dotcompb-8120-marketing-lp-hero > §5]`). Services depends on the hero's upstream `closestLocations` / `state.location` resolution. |
| `DOTCOMPB-8121` | Story (3 SP) | **Services Section (this ticket)** | **IN PROGRESS — node created 2026-05-12; planning pass next.** Branch: `DOTCOMPB-8121` off `feat-location-s`. PR will open against `feat-location-s` once Phase 1 is built. |
| Future children | Story | Additional V2 sections (membership block, FAQ, etc.) | NOT STARTED. |

### 2.3 Open Questions — RESOLVED 2026-05-18

All 8 open questions were resolved in this session during build + investigation.

1. ✅ **Variable promo** → **inline `v-if`**. `variablePromo.enabled` toggle + `applicableServiceCodes` list; no partial needed. `promoCopy` per-card (splits at `:` — bold prefix, regular suffix). Matches `BookableCategories.promo-banner` visual pattern.
2. ✅ **Pricing field paths** → `getLocation` API enriches `servicesOffered[]` with `{ code, price, proPlusMemberPrice, proMemberPrice }` per entry. Matched by string `code`. Non-member = `servicesOffered[code].price`; member = `servicesOffered[code].proPlusMemberPrice`. SSR shows CMS fallback; client hydration swaps to live values.
3. ✅ **`loadLocation` watcher** → **re-added** to `LocationSpecificColorbarV2.vue`. Fires when `closestLocations[0].distance ≤ 50` → populates `state.colorbar.location` with enriched servicesOffered data.
4. ✅ **`setServiceFromCookie` consumer** → **confirmed** at `ServicesPage.vue:239-257`. Reads `$cookies.get('selected_service')`, matches category, calls `navigateToAddons`. Cookie is belt-and-braces; session-based `appointmentProgress` is the primary mechanism.
5. ✅ **Pro+ modal** → **`LimitlessProPlusV2Modal`** at `HairColorBarBookingV2/LimitlessProPlusV2Modal/LimitlessProPlusV2Modal.vue`. Dispatched via `modal/showModal` with `component: 'LimitlessProPlusV2Modal', theme: 'default'`.
6. ✅ **Tophat schema** → applied 2026-05-18 to template 1650 (merge mode). Fields: `servicesSection.{title, subtitle, membership.{copy, savingsCopy, ctaText}, services[].{serviceCode, serviceName, description, image, basePrice.{member, nonMember}, promoCopy}, variablePromo.{enabled, applicableServiceCodes[], copy}}`.
7. ✅ **Service card images** → **CMS-authored** via `services[].image` (staticCroppedImage). Same authoring pattern as `bookingFlowConfig.service_categories[].image`. Production images are Carley's Tophat upload responsibility. Dev: seeded with 4 Hillsboro CDN URLs from cv 19405.
8. ✅ **Membership callout layout** → **dark-purple pill banner** above the carousel. Standalone `MembershipCallout.vue` component. Mobile/tablet: 2-column (icon left, text right, left-aligned). Desktop: icon + text inline-centered.

### 2.4 Key Decisions (Session-Wide)

1. **(2026-05-18)** Variable-pricing promo is **inline** (not a partial). CMS `variablePromo.enabled` toggle + per-service `applicableServiceCodes` + per-card `promoCopy`.
2. **(2026-05-18)** Wrapper `loadLocation` watcher **re-added** (`NEARBY_RADIUS_MILES = 50`). Hero PR removed it as "speculative coupling" (decision #33); 8121 re-adds it because Services genuinely needs `state.location.servicesOffered` for live pricing.
3. **(2026-05-18)** Pricing source: **`getLocation` API is authoritative** — `servicesOffered[].price` (non-member) + `.proPlusMemberPrice` (member, server-computed in `mr_modules/appointments/lib/location.js:1055-1056`). Matched by `serviceCode` string (not numeric `_id`). CMS `basePrice` is fallback only when: SSR pre-resolve, salon out-of-range, or salon doesn't carry the service.
4. **(2026-05-18)** Default services: **Hillsboro HCB v2 set** — `cbs_roots_only`, `cbs_all_over`, `cbs_full`, `cbs_roots_gloss` (in Figma order: Roots → All Over → Glossing → Highlights). Images from cv 19405's `componentList[0].settings.servicesList` CDN URLs.
5. **(2026-05-19)** Service CTA finalized: **cookie-only** (`this.$cookies.set('selected_service', payload.serviceCode)`). **No `_consult` transform** — raw service code (`cbs_roots_only`) used for both cookie and session. **`handoffPath`** = `this.location?.code ? /colorbar/booking/${code}/services : /colorbar/locations` — goes direct to booking flow when nearest location already resolved by wrapper. Session payload has no `forceShowAddons` flag. Auto-navigation to addons is handled entirely by `ServicesPage.setServiceFromCookie()` via SSR pre-load (see decision #10).
6. **(2026-05-19) SUPERSEDED** — `forceSkipAddons`/`forceShowAddons` approach **reverted**. `HairColorBarBookingV2.vue` and `hairColorBarBooking.js` are back to their original committed state. See decision #10.
7. **(2026-05-18)** `FORCE_NEAREST_LOCATION_FOR_TESTING` **removed** from `HeroSection.vue`. Closes 8120 §2.5 "REMOVE before PR" row.
8. **(2026-05-18)** Promo banner: **custom `.service-promo-banner`** matching `BookableCategories` pattern (bold prefix from split at first `:`, regular suffix). `VariablePricingMsgPill` dropped.
9. **(2026-05-18)** Membership callout: banner **95% width on mobile/tablet**, `fit-content` on desktop+. Icon `align-items: flex-start` (mobile/tablet) → `center` (desktop). Text left-aligned mobile/tablet → center desktop.
10. **(2026-05-19)** Auto-navigation to addons works via **SSR `serverPrefetch` → `loadInitialData` → `getBookingFlowConfig`** — `bookingFlowConfig` is in `__INITIAL_STATE__` at hydration time. `ServicesPage.mounted()` → `setServiceFromCookie()` finds the `selected_service` cookie + populated `categories` (from `bookingFlowConfig.service_categories`) → `navigateToAddons(category)` → routes to `booking-addons`. No changes to `HairColorBarBookingV2.vue` or `hairColorBarBooking.js` needed. Confirmed by Playwright test `addons-jump.spec.ts` (local, 2026-05-19).
11. **(2026-05-19)** `_consult` suffix **removed** from `ServicesSection.onServiceCtaClick`. The V2 `ServicesPage.setServiceFromCookie` matches against `bookingFlowConfig.service_categories[n].service.code` which uses base codes (`cbs_roots_only`). Consult routing for first-time customers is the booking flow's internal concern — not the pre-selection cookie's.
12. **(2026-05-19)** `NearestLocationCard` extended with **secondary CTA** ("FIND ANOTHER HAIR COLOR BAR" → `/colorbar/locations`). `HeroSection` updated — `showSearchCard` computed hides the search card when nearest-location card is showing (mutual exclusion). These are 8120/8121 shared changes shipped on the `DOTCOMPB-8120` working tree.

13. **(2026-05-20) Maxi's cookie-architecture principle confirmed for service pre-selection.** `/colorbar/locations` → `/colorbar/booking/{code}/services` is NOT an SPA transition — it is a full-page reload that wipes all Vuex state (same root cause as the promo persistence bug from 8120 decision #112 / §1.50). Our `selected_service` cookie IS the load-bearing persistence mechanism. No query params are used. The `saveAppointmentProgressToSession` call was a residual from the reverted `forceShowAddons` approach (decision #6) and actively fought against `setServiceFromCookie` by potentially triggering `resumeUnsavedBooking` to push the user back to services after `setServiceFromCookie` had already forwarded them to addons.

14. **(2026-05-20) `saveAppointmentProgressToSession` removed from `onServiceCtaClick`.** `vueColorbarSvc` import removed. `onServiceCtaClick` is now synchronous. Cookie (`selected_service`) is the sole persistence mechanism between the Marketing LP and the booking flow services step. Confirmed safe by the existing Playwright test `addons-jump.spec.ts`.

15. **(2026-05-20) 3-tier non-member pricing architecture implemented.** `resolvedNonMemberPrice` now resolves in three tiers: (1) live `location.servicesOffered[code].price` (nearest salon ≤50 mi, populated by wrapper's `loadLocation` watcher); (2) `bookableServicesByCode[code].price` from the global `bookableServices` store (the base price set in Tophat at `#/colorbar/service/edit/:id`, fetched once via `GET /api/colorbar/getServicesList`); (3) CMS `basePrice.nonMember` (manual Tophat fallback). Member price stays **2-tier**: (1) live `proPlusMemberPrice` → (2) CMS `basePrice.member`. No global member price exists on `bookableService` — it is computed per-location in `mr_modules/appointments/lib/location.js`.

16. **(2026-05-20) `bookableServices` added to `colorbar` Vuex module — reachable by any component.** State: `bookableServices: []`. Getter: `bookableServicesByCode` (O(1) dictionary keyed by `code`). Mutation: `setBookableServices`. Action: `loadBookableServices` — one-shot (guarded by `length > 0`), errors swallowed (consumers fall back to CMS `basePrice`). `ServicesSection.mounted()` triggers the load. 10 tests added to `colorbar.test.js` (7 new + prior 3).

17. **(2026-05-20) `productPrice` CMS field type ruled out for HCB service pricing.** Deep investigation: `productPrice` field stores `{ productId, subPrice }` referencing `productcatalog.products` — retail e-commerce items (hair color kits). Rendered by the legacy `mr-price` Angular directive via `productCatalogSvc.getProductPrices`. The `bookableService.productId` values for our 4 services map to product names like "Root Touch Up Set: Light Brown" with retail kit prices — unrelated to salon booking fees. No Vue equivalent of `mr-price` exists. No Tophat field type for selecting a bookable service with auto-populated booking price exists (exhaustive survey of all 23 field types + every HCB template).

18. **(2026-05-20) `ServiceCard.vue` empty-price guard added.** `v-if="memberPrice || nonMemberPrice"` on `.service-price-row` — hides the entire price row when both values are empty strings. Prevents malformed output (`'' member / '' non-members`) when the CMS `basePrice` is not filled in and no live location or global base price is available.

19. **(2026-05-20) Code review (accessibility-lead) — 7 findings, all fixed.** Blockers: Swiper `A11y`+`Keyboard` modules not registered (slides invisible to screen readers, no keyboard nav); `MembershipCallout` CTA was `<a href="#">` — replaced with `<button type="button" aria-haspopup="dialog">`. Majors: `MrIcon` `aria-hidden="true"` added; `resolvedCtaAriaLabel` now includes promo copy when `showPromo=true`; `NearestLocationCard` primary CTA focus ring was white-on-white (1:1) — corrected to `outline: cta-color-1` / `box-shadow: color-white` (7.86:1); `aria-label="Services"` fallback added when `heroTitle` is empty. Minors: `titleId` dead prop removed; `CTA_LABEL` computed renamed to `ctaLabel` (camelCase); `emits: []` added; secondary CTA aria-label fallback suffix corrected to `"near you"`.

20. **(2026-05-20) `MembershipCallout` refactored to single `html` CMS field.** Replaced 3 separate props (`copy`, `savingsCopy`, `ctaText`) with one `content` prop rendered via `v-html`. Link destination URL is now CMS-authored inside the HTML rather than hardcoded to open `LimitlessProPlusV2Modal`. No modal dispatch, no `cta-click` emit. Removed `MEMBERSHIP_CTA_EVENT`, `PRO_PLUS_MODAL_COMPONENT`, `onMembershipCtaClick` from `ServicesSection`. CMS template 1653 `membership.fieldConfig` reduced from 3 fields to 1 `html` field. Both cv 19460/19464 `membership` data migrated from `{copy, savingsCopy, ctaText}` to `{content: '<strong>...</strong> ... <a href="...">...</a>'}`.

21. **(2026-05-20) `variablePromo.copy` removed.** Field removed from template 1653 schema, unset from cv 19460/19464 data, and `|| this.variablePromo.copy` fallback removed from `ServicesSection.promoCopyForService`. Each service card's `promoCopy` is now the only source of banner text — no section-level default.

22. **(2026-05-20) Standalone architecture migration — `ServicesSection` and `MembershipCallout` moved out of `LocationSpecificColorbarV2/components/`.** New paths: `LocationSpecific/ServicesSection/ServicesSection.vue` and `LocationSpecific/ServicesSection/components/MembershipCallout/MembershipCallout.vue`. `LocationSpecificColorbarV2` no longer mounts `ServicesSection` — the wrapper now only handles `HeroSection`. `ServicesSection` is self-sufficient: owns `initializeClosestLocation` dispatch + `loadLocation` watcher + `loadBookableServices` serverPrefetch + mounted guard. Global registration updated in both `mrVueApp.js` and `registerGlobalsSsr.js` to point to new paths.

23. **(2026-05-20) CMS template 1653 (`services-section`) created as standalone Tophat component.** Type: `component`, mixin_key: `services-section`, jade: `services-section(:cms-settings=JSON.stringify(settings))`. Field schema reorganised: `sectionHeader` dividers ("Promotional Banner" / "Service Cards") added with `options.heading` for rendered headings; `variablePromo` moved BEFORE `services[]` so Carley configures the promo toggle first; all `helpText` made concise (plain language, cross-referencing related fields). Added to componentList in cv 19460 + 19464 at position 2 (after `location-specific-colorbar-v2`, before `letter-from-amy`). `servicesSection.*` data migrated out of lsv2 settings into standalone component entry.

24. **(2026-05-20) Clean branch `DOTCOMPB-8121-clean` cut from `feat-location-s`.** Exactly 13 files vs `feat-location-s`: 9 new component/test/barrel files + `mrVueApp.js` + `registerGlobalsSsr.js` + `colorbar.js` + `colorbar.test.js`. No 8120 files. Pre-commit hook issue resolved: `mrVueApp.js` eslint-disable comment had two Vue rule names (`vue/component-definition-name-casing`, `vue/order-in-components`) that don't exist in `eslint-plugin-vue` v10 (root node_modules) — removed those two directives, leaving only `jsdoc/require-jsdoc`. PR #20888 opened against `feat-location-s`.

25. **(2026-05-20) Merge conflict resolution — `feat-location-s` had added `initializeClosestLocation` and `LocationsSection` independently.** Three commits landed on `feat-location-s` after our branch point: `844ebc93254` (added `initializeClosestLocation`), `7554ae2d43f` (enriched it with `getLocationForBooking` after closest-location resolution), and `b7a9cff8f91` (PR #20879 — added `LocationsSection` component + global registration). Conflicts in: `mrVueApp.js` (keep both `LocationsSection` + `ServicesSection`), `registerGlobalsSsr.js` (same), `colorbar.js` (trivial trailing newline). feat-location-s's `initializeClosestLocation` is now the canonical implementation — it runs the full address → geolocation → IP fallback chain then calls `getLocationForBooking` on the closest. Our simpler alias (just called `initializeBopis`) has been superseded. `bookableServices` additions are untouched. Merge commit `8e0a922ab62`. 563/563 suite green.

### 2.5 Pending Work

**Setup:**
*   [x] Roam node created (UUID `ab99a73b-cb11-4dc9-8b7b-f25b148dce0a`). Index updated.
*   [x] Session file created. §1.48 embedded 8120 playbook.
*   [x] Planning pass — all 8 open questions resolved (§2.3).

**CMS reshape (all done locally):**
*   [x] Phase E — `servicesSection.*` schema applied to template 1650 (merge mode, 2026-05-18).
*   [x] Phase F — cv 19460 + 19464 seeded at both paths with 4 Hillsboro services + prices + images.
*   [x] Phase G — Verified: curl confirmed all 4 services + prices + banners in SSR HTML.
*   [ ] Phases A-D (hero artifact cleanup: strip heroSection, delete promo-badge partial) — deferred; hero still active on PR #20771.
*   [ ] **Production replication for Carley** — document in PR body as Special Deployment Requirements.

**Phase 1 + 2 — Vue build (all done 2026-05-18, on DOTCOMPB-8120 working tree):**
*   [x] `ServicesSection.vue` + `index.js` — full implementation with Swiper, live pricing, session-based CTA, modal.
*   [x] `ServiceCard.vue` + `index.js` — custom promo banner, `btn-block` CTA, live price formatting.
*   [x] `MembershipCallout.vue` + `index.js` — pill banner, 2-col mobile / inline desktop.
*   [x] Wrapper `LocationSpecificColorbarV2.vue` updated — `ServicesSection` mounted + `loadLocation` watcher re-added.
*   [x] `HeroSection.vue` — `FORCE_NEAREST_LOCATION_FOR_TESTING` removed + `showSearchCard` mutual-exclusion computed added.
*   [x] `NearestLocationCard.vue` — secondary CTA ("FIND ANOTHER HAIR COLOR BAR") added.
*   [x] `hairColorBarBooking.js` — **reverted to original** (2026-05-19). No booking-flow state changes ship with 8121.
*   [x] `HairColorBarBookingV2.vue` — **reverted to original** (2026-05-19). Auto-navigation handled by SSR + `setServiceFromCookie`.
*   [x] `ServicesSection.vue` — CTA finalized: raw service code (no `_consult`), direct handoff via `this.location?.code`, no `forceShowAddons` in session.
*   [x] `.tasks/DOTCOMPB-8121/services-schema.json` — schema spec (trimmed to business-configurable only).

**Phase 2b — Pricing + cookie architecture (done 2026-05-20):**
*   [x] `ServicesSection.vue` — `saveAppointmentProgressToSession` removed; `onServiceCtaClick` made synchronous; 3-tier non-member pricing wired (`lookupLiveService` → `bookableServicesByCode` → CMS `basePrice`); `loadBookableServices` dispatched in `mounted()`; `vueColorbarSvc` import removed. (decision #13, #14, #15)
*   [x] `ServiceCard.vue` — empty-price guard `v-if="memberPrice || nonMemberPrice"` added to `.service-price-row`. (decision #18)
*   [x] `colorbar.js` — `bookableServices` state + `bookableServicesByCode` getter + `setBookableServices` mutation + `loadBookableServices` action added; `getServicesList` imported. (decision #16)
*   [x] `colorbar.test.js` — 7 new tests: `bookableServicesByCode` getter (3 cases) + `loadBookableServices` action (4 cases). All 10 tests green (includes prior 3). State isolation via `freshStore()` factory.

**Phase 2b–2d — Architecture + CMS + Review (all done 2026-05-20):**
*   [x] Code review (accessibility-lead) — 7 findings, all fixed (decision #19). 563/563 suite green.
*   [x] `MembershipCallout` → single `html` field; no modal dispatch (decision #20).
*   [x] `variablePromo.copy` removed (decision #21).
*   [x] `ServicesSection` + `MembershipCallout` moved to `LocationSpecific/ServicesSection/` (decision #22).
*   [x] CMS template 1653 (`services-section`) created standalone; componentList updated in cv 19460+19464 (decision #23).
*   [x] Roam node `* COMMIT MSG` and `* PULL REQUEST` updated via `/pr-scribe`.
*   [x] `DOTCOMPB-8121-clean` branch created; commit `ff3907b7ca9`; PR #20888 opened (decision #24).
*   [x] Merge conflicts resolved vs `feat-location-s` (decisions #24–#25); merge commit `8e0a922ab62`; 563/563 green.

**Pending (PR #20888 is OPEN):**
*   [ ] **CI** — CircleCI jobs running (`website_tests`, `core_tests`, etc.). Monitor for failures.
*   [ ] **PR review** — address comments in-thread; labels `DOTCOM TEAM` + `Pending Code Review` applied.
*   [ ] **Production CMS for Carley** — documented in PR #20888 Special Deployment Requirements. She creates `services-section` template + seeds componentList via Tophat UI after merge.
*   [ ] **CMS Phases A-D** (strip `heroSection` from lsv2, delete promo-badge partial) — DEFERRED until hero PR #20771 merges.

**Phase 3+ — parked:**
*   [ ] E2E tests (Playwright) against `feat-location-s` deploy.

---

## SECTION 3: FEATURE / TICKET IMPLEMENTATIONS

### 3.1 Inherited foundation — `LocationSpecificColorbarV2` wrapper

The wrapper, splitter, Vuex wiring, CMS migration, and global registration are SHIPPED. See `[session: dotcompb-8120-marketing-lp-hero > §3.1]` for the full state. **Services does not re-derive any of this — it mounts as the second block inside the V2 `v-if` branch.**

### 3.2 DOTCOMPB-8121 — Marketing LP Services Section

**Created:** 2026-05-12 | **Last updated:** 2026-05-18 | **Status:** Phase 1+2 BUILT on `DOTCOMPB-8120` working tree. Tests + PR pending. Stash-and-move to `DOTCOMPB-8121` on user's call.
**Branch (destination):** `DOTCOMPB-8121` (to be cut from `feat-location-s` at stash time).
**Roam node:** `~/.brain.d/roam-nodes/madison_reed/2026-05-12-112750-dotcompb_8121.org` (UUID `ab99a73b-cb11-4dc9-8b7b-f25b148dce0a`)
**Figma:** [Marketing Landing Pages — Services frame (node 8005-3)](https://www.figma.com/design/vVTk5xHFRsiFYGUOVoHxYQ/Marketing-Landing-Pages?node-id=8005-3&t=5XgygUpORzgHqH0J-4)
**JIRA:** [DOTCOMPB-8121](https://madison-reed.atlassian.net/browse/DOTCOMPB-8121) (parent: DOTCOMPB-8119, sibling: DOTCOMPB-8120)

#### Actual component tree (SHIPPED 2026-05-18)

```
LocationSpecificColorbarV2 (wrapper — loadLocation watcher re-added)
└── ServicesSection (xs-mt-350m / xs-pb-150m / position:relative z-index:0)
    ├── .services-header (title h2 + subtitle p)
    │   ├── h2#services-section-title (.color-mr-purple.f-domaine-display-condensed.upper.max-at-tweak
    │   │                               .xs-f-grande.md-f-xgrande.lg-f-xxxxgrande.xl-f-poster)
    │   └── p.services-subtitle (.text-color-3.text-center.max-at-tweak.xs-f-small.lg-f-xmedium)
    ├── MembershipCallout.services-membership (width:95% mobile/tablet → fit-content desktop)
    │   └── .callout-row (icon col + text col, align-items flex-start mobile → center desktop)
    └── .services-carousel → Swiper (auto / spaceBetween 12 / grabCursor / a11y)
        └── SwiperSlide (78% mobile → 46% tablet → 32% desktop-md → 25% desktop+)
            └── ServiceCard (emits cta-click → onServiceCtaClick)
                ├── .service-image-wrap (aspect-ratio 1/1 / border-radius 6px 6px 0 0 / overflow hidden)
                │   ├── ImgBox (media-obj, alt=serviceName, object-position center top)
                │   └── .service-promo-banner (position absolute / brand-color-1 bg)
                │       └── p.promo-text.xs-f-xsmall.max-at-tweak (span.bold prefix + suffix split at ":")
                └── .service-content (ui-color-3 bg / border-radius 0 0 6px 6px)
                    ├── h3.service-title (.f-secondary.brand-color-1.upper.max-at-tweak.xs-f-xlarge)
                    ├── p.service-price-row
                    │   ├── span.service-price-primary.bold (cta-color-1 / 21px / f-primary-bold)
                    │   └── span.service-price-secondary (f-primary / 14px / #746874)
                    │       └── span.service-price-secondary-value.bold (f-primary-bold / #746874)
                    ├── p.service-description (.text-color-1.max-at-tweak.xs-f-small.lg-f-medium)
                    └── MrBtn.service-cta.btn-block.round (cta-color-1 via MrBtn defaults)
```

#### Live pricing flow (2026-05-18)

| Tier | Source | Field |
|---|---|---|
| 1 — Live member | `/api/colorbar/getLocation` → `servicesOffered[code].proPlusMemberPrice` | Server-computed, per-location |
| 1 — Live non-member | `/api/colorbar/getLocation` → `servicesOffered[code].price` | Server-computed, per-location |
| 2 — CMS fallback | `cmsSettings.servicesSection.services[].basePrice.{member, nonMember}` | Used when location not loaded / service not carried |

Formatted via `this.$currencyShort(value, true)` (global `filtersPlugin`).

#### Service CTA booking flow (2026-05-19 — finalized)

Card click → `onServiceCtaClick`:
1. `code = payload.serviceCode` — **no `_consult` transform**, raw base code (e.g. `cbs_roots_only`)
2. `this.$cookies.set('selected_service', code)`
3. `await vueColorbarSvc.saveAppointmentProgressToSession({ appointmentProgress: { selectedServiceCode: code, selectedAddOnTreatments: [], ... } })` — no `forceShowAddons`
4. `handoffPath = this.location?.code ? /colorbar/booking/${this.location.code}/services : /colorbar/locations`
5. `trackMREventAndRedirect('MREvent (Marketing LP – Service CTA clicked)', handoffPath, ...)`

Auto-navigation at destination (`/colorbar/booking/{code}/services`):
- SSR `serverPrefetch` → `loadInitialData` → `getBookingFlowConfig('booking_flow_config_site_revolution')` → `bookingFlowConfig` in `__INITIAL_STATE__`
- Client hydration → `ServicesPage.mounted()` → `setServiceFromCookie()` — cookie found + `categories` populated → `navigateToAddons(category, false)` → `setSelectedCategory` + `setSelectedService` (action, refreshes promos) + `router.push('booking-addons?member=0')`
- `HairColorBarBookingV2.vue` + `hairColorBarBooking.js` unchanged from original (no booking-flow state additions)
- Confirmed by Playwright test `automation/playwright/specs/local/addons-jump.spec.ts` (2026-05-19)

#### Default 4 services (Hillsboro/HCB-v2 sourced, seeded in cv 19460 + 19464)

| serviceCode | serviceName | serviceId (bookableService) | basePrice |
|---|---|---|---|
| `cbs_roots_only` | Roots Coverage | 27 | $0 member / $90 non-member |
| `cbs_all_over` | All Over Color | 33 | $29.60 / $127 |
| `cbs_full` | Glossing + Blowout | 86 | $72 / $90 |
| `cbs_roots_gloss` | Highlights | 31 | $88-$128 / $110-$160 |

variablePromo: enabled=true, applicableServiceCodes=["cbs_roots_only","cbs_all_over"]

#### Tests target (still pending)

| File | Target tests |
|---|---|
| `ServicesSection.test.js` | ~15 — CMS read, 4 cards, live pricing vs fallback, promo gating, modal dispatch, session+cookie set, tracking payload |
| `ServiceCard.test.js` | ~12 — heading level, ARIA, price format, promo banner split, btn-block full-width, cta-click emit |
| `MembershipCallout.test.js` | ~6 — copy/savings render, cta-click emit, aria-label |
| `LocationSpecificColorbarV2.test.js` (delta) | +3 — ServicesSection mounts; loadLocation watcher fires at ≤50mi; does not fire at >50mi |

### 3.3 CMS Configuration Plan — local-DB reshape for this ticket

> Full step-by-step lives in the roam node under `** DEPLOYMENT NOTEs → *** CMS Plan`. This section is the index + state-pinning summary so future conversations don't re-derive identity from scratch.

**Goal:** strip hero CMS additions from the local Mongo and configure only what Services needs, so dev-ssr on `DOTCOMPB-8121` runs with `cmsSettings.servicesSection.*` flowing in and nothing leftover from the hero work polluting the wrapper's props.

#### Tophat object inventory (local Mongo as of 2026-05-12)

| Object | Identity | Origin | Action for 8121 |
|---|---|---|---|
| `content._id 3117` | "Salon-Quality Hair Color Landing Page" → `/colorbar/location-specific` | Foundation (PR #20750) | Keep — the page itself |
| `contentVersion._id 19460` | v55 published, variant **B** | Foundation | Strip `heroSection.*` at both paths; add `servicesSection.*` |
| `contentVersion._id 19464` | v56 edit, variant **B** | Foundation | Same — both versions MUST stay in sync per `[…8120 > §1.33]` |
| `experiment._id 504` | `LocationSpecificSiteRevolution` (Running, B@10000) | Foundation | Keep |
| `template._id 1650` | `location-specific-colorbar-v2` (V2 sub-template) | Foundation | Strip `heroSection.*` schema; add `servicesSection.*` schema |
| `template._id 1652` | `partial-promo-badge` (Promo Badge — generic template) | 8120 | **Delete** — hero-only, not consumed by Services |
| `content._id 3403` | `partial-marketing-lp-hero-20off` (Hero 20% off campaign) | 8120 | **Delete** — hero-only |
| `contentVersion._id 19462` | content 3403 active version | 8120 | **Delete** alongside parent |

#### Plan phases (detail in roam node)

| Phase | Purpose | Script / Tool |
|---|---|---|
| **A** | Backup current state to `.tasks/DOTCOMPB-8121/backups/<stamp>/` | `inspect-content.mjs 3117`, `inspect-template.mjs 1650`, `inspect-partial.mjs partial-marketing-lp-hero-20off`, `backup-content.mjs 3117` |
| **B** | Strip `heroSection.*` from template 1650 schema | `get-template-fields.mjs 1650 --json` → hand-edit → `set-template-fields.mjs 1650 --src <no-hero>.json --mode replace --confirm` |
| **C** | Strip `heroSection.*` from cv 19460 + 19464 (BOTH paths — `templateData.heroSection` AND `componentList[lsv2].settings.heroSection`) | Direct mongosh `$unset` with `arrayFilters` (no typed mutator ships in `tophat-tools` for this) |
| **D** | Delete partial template 1652 + content 3403 + cv 19462 | Inspect first, then `db.template.deleteOne` / `db.content.deleteOne` / `db.contentVersion.deleteOne` |
| **E** | Add `servicesSection.*` schema to template 1650 (16 fields per the table in the roam node) | `set-template-fields.mjs 1650 --src services-schema.json --mode merge --confirm` |
| **F** | Seed `servicesSection.*` sample data on cv 19460 + 19464 (BOTH paths) | mongosh `$set` with `arrayFilters` on `componentList[lsv2]` |
| **G** | Verify: `inspect-content.mjs 3117`, curl SSR HTML, browser smoke with `experiments_504=b` | Read-only |
| **H** | Document production replication for Carley in the eventual PR body | manual Tophat UI steps |

#### Critical gotchas pinned (don't re-derive)

1.  **Dual-path seeding/stripping** (`[…8120 > §1.33]`) — SSR reads from `templateData.componentList[N].settings.servicesSection`, NOT from `templateData.servicesSection`. Touching only the top-level path leaves the rendered snapshot stale. Every write to cv 19460 or 19464 must hit BOTH paths via `arrayFilters: [{ "c.mixin_key": "location-specific-colorbar-v2" }]`.
2.  **Both versions of the parent content** (cv 19460 published v55 AND cv 19464 edit v56) MUST stay in sync — Tophat preview / edit mode renders the edit version; SSR renders the published version. Seeding only one leaves one rendering path broken.
3.  **Every config field needs `options.xsClass: "col-xs-12"`** (`[…8120 > §1.38]` row 11) or the Tophat content-edit form renders the field at zero width. Especially visible on `link` fields.
4.  **No `content.templateKey`** on partial content docs (`[…8120 > §1.32]`) — forces the buggy `loaders.js:51` branch and crashes dev-ssr with `Cannot read properties of undefined (reading '0')` at `loaders.js:479`. Applies only if a new partial is added for Services (e.g., variable-pricing promo as partial). Default for Services is inline — partial only on explicit decision per §2.3 #1.
5.  **`tophat-tools` mutation scripts dry-run by default** (`safety-and-conventions.md`). The `--confirm` flag is the only way they write. Backups land under `./cms-backups/` automatically.
6.  **Reversal path documented** — `restore-content.mjs ./cms-backups/3117/<stamp>/snapshot.json --confirm` + re-running the hero spec JSONs under `.tasks/DOTCOMPB-8120/` recovers the 8120 state when switching back to that branch.

#### Cross-references

* Roam node `** DEPLOYMENT NOTEs → *** CMS Plan` — full executable plan with copy-paste-ready mongosh + script invocations.
* `~/.claude/skills/tophat-tools/SKILL.md` — script catalog and authoring discipline.
* `~/.claude/skills/tophat-tools/rules/safety-and-conventions.md` — five mandatory rules for every mutation.
* `~/.claude/skills/tophat-tools/rules/template-field-schema.md` — 23 field types + per-type `options` keys + `xsClass` requirement.
* `[session: dotcompb-8120-marketing-lp-hero > §1.32]` (no `content.templateKey` on partials), `§1.33` (dual-path seeding), `§1.38` (11-gotcha canonical shape), `§1.39` (template/content slug split), `§3.3` (tophat-tools skill inventory).

---

## SECTION 4: FILE INDEX

> ⚠️ **PARALLEL-BRANCH WORKFLOW** — branch in working tree is `DOTCOMPB-8120`, but the rows below tagged `Destination: DOTCOMPB-8121` belong on that ticket's branch. Stash-and-move is on user's explicit call only (per `feedback_document_every_file_created.md` HIGH PRIORITY rule). Add/update a row here IMMEDIATELY after each Write — do not batch.

| Path | Purpose | Status | Destination |
|---|---|---|---|
| `~/.brain.d/roam-nodes/madison_reed/2026-05-12-112750-dotcompb_8121.org` | Ticket roam node | CREATED 2026-05-12 | — |
| `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` | Index — BACKLOG `<<ticket-8121>>` + Sprint Board IN PROGRESS | UPDATED 2026-05-12 | — |
| `…/sessions/dotcompb-8121-marketing-lp-services.md` | This session file | UPDATED 2026-05-20 (pricing architecture, cookie analysis, decisions 13-18, §2.4/§2.5/§4/§5/§6 updated) | — |
| `website/src/vuescripts/components/ServiceCard/ServiceCard.vue` | Generic reusable service card (image, title h3, member/non-member price line, description, single BOOK SERVICE MrBtn). Custom promo banner (split at `:`, brand-color-1 bg) when `showPromo`. Props: serviceCode, serviceName, memberPrice, nonMemberPrice, description, image, showPromo, promoCopy, headingLevel ('h3' default, validates h1-h6), titleId. Emits `cta-click` (payload: serviceCode, serviceName, memberPrice, nonMemberPrice). Composed `resolvedCtaAriaLabel` (no leading-comma per §1.41). **Empty-price guard:** `.service-price-row` hidden via `v-if="memberPrice \|\| nonMemberPrice"` — prevents broken UI when CMS `basePrice` not filled in and no live/global price available (decision #18). Visual style mirrors `BookableCategories` card. | UPDATED 2026-05-20 | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/ServiceCard/index.js` | Barrel export | CREATED 2026-05-18 | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/ServiceCard/ServiceCard.test.js` | Card tests | NOT CREATED | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/MembershipCallout/MembershipCallout.vue` | Dark-purple `brand-color-1-bg` pill banner with ✨ sparkle icon + bold copy + savings text + inline CTA link. Props: copy, savingsCopy, ctaText, ctaAriaLabel, ariaLabel. Emits `cta-click`. Uses `MrIcon name="sparkle"`. Two-color focus-visible ring (white outline + brand-color-1 box-shadow per §1.30). | CREATED 2026-05-18 | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/MembershipCallout/index.js` | Barrel export | CREATED 2026-05-18 | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/MembershipCallout/MembershipCallout.test.js` | MembershipCallout tests | NOT CREATED | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/ServicesSection/ServicesSection.vue` | Section orchestrator. Reads `cmsSettings.servicesSection.*`. Renders `h2#services-section-title` + subtitle + `MembershipCallout` + Swiper carousel of `ServiceCard`. **3-tier non-member pricing** (decision #15): Tier 1 `activeLocationServicesByCode[code].price` (live, from wrapper's `loadLocation`); Tier 2 `bookableServicesByCode[code].price` (global base from `colorbar` store, loaded once in `mounted()`); Tier 3 CMS `basePrice.nonMember`. **2-tier member pricing**: Tier 1 `proPlusMemberPrice` (live); Tier 2 CMS `basePrice.member`. **`onServiceCtaClick` is now synchronous** — `saveAppointmentProgressToSession` removed (decision #14); cookie-only (`$cookies.set('selected_service', code)`); `handoffPath = this.location?.code ? /colorbar/booking/${code}/services : /colorbar/locations`; `trackMREventAndRedirect`. Membership CTA: `trackMREvent` + `modal/showModal` with `LimitlessProPlusV2Modal`. | UPDATED 2026-05-20 | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/ServicesSection/index.js` | Barrel export | CREATED 2026-05-18 | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/ServicesSection/ServicesSection.test.js` | Section tests (pricing tiers, promo gating, modal dispatch, service CTA cookie+event, swiper renders 4 slides) | NOT CREATED | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/LocationSpecificColorbarV2.vue` | Wrapper mounts `ServicesSection` below `HeroSection`. Re-added `loadLocation` watcher on `closestLocations` (immediate: true) — dispatches `colorbar/loadLocation(closestLocations[0].code)` when distance ≤ 50 mi, so `state.colorbar.location` populates with the full Location doc (including `servicesOffered[]`) for downstream sections. `NEARBY_RADIUS_MILES = 50` constant added at module scope. | MODIFIED 2026-05-18 | **DOTCOMPB-8121** (8120 PR #20771 had this watcher REMOVED in commit `7077fcd` as "speculative coupling" per 8120 decision #33; 8121 re-adds it because Services genuinely needs `state.location` populated for live pricing) |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/components/HeroSection/HeroSection.vue` | Removed `FORCE_NEAREST_LOCATION_FOR_TESTING`. Added `showSearchCard` computed — hides search card when nearest-location card is showing (mutual exclusion). Secondary CTA `secondary-cta-text="FIND ANOTHER HAIR COLOR BAR"` passed to `NearestLocationCard`. | MODIFIED 2026-05-19 | **DOTCOMPB-8121** |
| `website/src/vuescripts/components/NearestLocationCard/NearestLocationCard.vue` | Added secondary CTA: props `secondaryCtaText`, `secondaryCtaUrl` (default `/colorbar/locations`), `secondaryCtaAriaLabel`, `secondaryTrackingEvent`. New `.cta-group` flex container wraps primary + secondary `MrBtn`. Primary styled `cta-color-1`, secondary styled `ui-color-4` background. Mobile: `flex-direction column-reverse`. | MODIFIED 2026-05-19 | **DOTCOMPB-8121** |
| `website/src/vuescripts/store/modules/hairColorBarBooking.js` | **REVERTED to original** (2026-05-19). No changes ship with 8121 — auto-navigation handled by SSR + `setServiceFromCookie`. | NO CHANGE | n/a |
| `website/src/vuescripts/components/HairColorBarBookingV2/HairColorBarBookingV2.vue` | **REVERTED to original** (2026-05-19). No changes ship with 8121. | NO CHANGE | n/a |
| `automation/playwright/specs/local/addons-jump.spec.ts` | Playwright test confirming: (1) `selected_service` cookie → auto-navigates to `/addons`; (2) no cookie → stays on `/services`. Runs against real dev server, no API mocks. `activateV2` injects `window.experiments.BookingFlowSiteRevolution = 'B'` via `addInitScript`. | CREATED 2026-05-19 | — (local test, not shipped) |
| `website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/LocationSpecificColorbarV2.test.js` | Wrapper test (delta: assert ServicesSection mounts; restore `loadLocation` watcher tests if §2.3 #3 lands on "re-add") | NOT MODIFIED YET | **DOTCOMPB-8121** |
| `website/cms-backups/templateVersion/1650/<stamp>-services-schema.json` | Tophat schema backup before adding `servicesSection.*` | NOT CREATED | **DOTCOMPB-8121** |
| `.tasks/DOTCOMPB-8121/services-schema.json` | Tophat schema spec for `servicesSection.*` — 1 top-level object + nested groups (title, subtitle, membership{copy, savingsCopy, ctaText}, services[]{serviceCode, serviceName, description, image, basePrice{member, nonMember}, promoCopy}, variablePromo{enabled, applicableServiceCodes[], copy}). Mirrors hero `heroSection.*` shape verbatim (`xsClass: "col-xs-12"` everywhere per §1.38, defaults match Figma copy, helpText covers every field). Trimmed: removed `membership.ctaAriaLabel`, `membership.ariaLabel`, `services[].imageAlt`, `services[].ctaText` — all derivable / hardcoded (a11y plumbing + universal UX language, not business-customizable). Applied 2026-05-18 via `set-template-fields.mjs 1650 --src ... --mode merge --confirm`. | CREATED 2026-05-18 | **DOTCOMPB-8121** (mongo write — separate from git tree, but file lives in repo) |
| `cms-backups/templateVersion/1650/2026-05-18T19-57-56-817Z-v1.json` | Pre-apply backup of templateVersion 5707 (auto-created by `set-template-fields.mjs --confirm`). Restore command: `node ~/.claude/skills/tophat-tools/scripts/set-template-fields.mjs 1650 --src cms-backups/templateVersion/1650/2026-05-18T19-57-56-817Z-v1.json --mode replace --confirm`. | CREATED 2026-05-18 | — (local CMS rollback artifact) |
| `website/src/vuescripts/store/modules/colorbar.js` | Added `bookableServices: []` state, `bookableServicesByCode` getter (O(1) dict keyed by `code`), `setBookableServices` mutation, `loadBookableServices` async action (one-shot guard + error swallow), `getServicesList` import. Any component can `mapState('colorbar', ['bookableServices'])` or `mapGetters('colorbar', ['bookableServicesByCode'])` to access global base prices from `appointments.bookableService` without a location. (decision #16) | MODIFIED 2026-05-20 | **DOTCOMPB-8121** |
| `website/src/vuescripts/store/modules/colorbar.test.js` | Added 7 tests: `bookableServicesByCode` getter — empty object on empty state, keys by code, skips entries without code; `loadBookableServices` action — fetches+commits, skips when already populated, swallows API errors, stores empty array on non-array response. State isolation via `freshStore()` factory (prevents test-to-test state leakage). All 10 tests green. | MODIFIED 2026-05-20 | **DOTCOMPB-8121** |

### Reused components (read-only — do NOT modify on this ticket)

| Path | What we reuse | Why |
|---|---|---|
| `website/src/vuescripts/components/HairColorBarBooking/VariablePricingMsgPill/VariablePricingMsgPill.vue` | Top-of-card promo banner via `:redesign="true"` mode (brand-color-1 bg + white text + 8px 16px padding + center). Service shape: `{ variablePricePromoMsg, variablePricePromoApplyTo }`. | Pre-existing skill component matching Figma per-card banner. `ServiceCard.vue` passes a synthetic `service` object built from the `promoCopy` prop. |
| `website/src/vuescripts/components/HairColorBarBookingV2/LimitlessProPlusV2Modal/LimitlessProPlusV2Modal.vue` | Pro+ membership modal opened by `MembershipCallout.cta-click → ServicesSection.onMembershipCtaClick → modal/showModal` | AC4 — "the Pro+ modal from version B of the booking flow" |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationServices/HairColorBarLocationServices.vue` | Reference for Swiper carousel mechanics + cookie-based hand-off pattern. NOT imported — pattern was adapted. | Same family / same UX language. |
| `website/src/vuescripts/components/HairColorBarBookingV2/ServicesPage/BookableCategories.vue` | Reference for service-card visual style (border-radius 6px, white bg, padding rhythm, `.f-secondary.brand-color-1` title). NOT imported — pattern was adapted. | Same family / matches Figma. |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.** Last reset: 2026-05-28.

### What was done last (2026-05-28 — EnhancementSection Book a Service fixed + AI issue rebutted)

**AI review comment on PR #20981:**
- AI flagged `selected_service` cookie removal as a regression. Replied in-thread: intentional fix — cookie was bypassing Services page entirely via `navigateToAddons`, not preserving a selection. `setServiceFromCookie` no-ops on null-check = correct behavior. Reply: https://github.com/MadisonReed/mr/pull/20981#discussion_r3320398822

**EnhancementSection.vue — "Book a Service" was a no-op:**
- Root cause: `onBookService` emitted `'book-service'` but no parent component exists to receive it — `EnhancementSection` is globally registered and rendered standalone from Tophat `componentList`, so the emit vanished. Button did nothing.
- Fix: same pattern as `ServicesSection.onServiceCtaClick` — replaced emit with internal `trackMREventAndRedirect(ENHANCEMENT_CTA_EVENT, handoffPath, {...})`. Handoff path: `/colorbar/booking/{code}/services` if `location.code` set, `/colorbar/locations` fallback.
- Also applies Carley's direction: lands on Services page, no preselection cookie.
- Added `BOOKING_HANDOFF_PATH` + `ENHANCEMENT_CTA_EVENT` module-level constants; `emits: ['book-service']` → `emits: []`
- `EnhancementSection.test.js`: replaced 1 emit test → 4 CTA tests (direct booking URL, fallback URL, payload prices, no emit). 28/28 passing.

**ServicesSection cookie removal (2026-05-28 earlier):** `selected_service` cookie write removed from `onServiceCtaClick` per Carley's direction. ServicesSection.test.js updated. 5910 tests.

**2026-05-20 staged (PR review fixes, still pending commit):**
- `ServicesSection.vue` — `role="region"` removed
- `ServiceCard.vue` — price labels + `#746874` → `text-color-3`
- `colorbar.js` — duplicate `initializeClosestLocation` + `setClosestLocationInitialized` removed
- `colorbar.test.js` — 2 stale tests removed

### Current state

- **Branch:** `DOTCOMPB-8121-clean`
- **PR #20888:** OPEN — needs single commit bundling all 7 modified files + updated PR body
- **Modified files (all pending commit):**

| File | Change | Date |
|---|---|---|
| `ServicesSection.vue` | `role` removed + cookie write removed | staged 2026-05-20 + 2026-05-28 |
| `ServicesSection.test.js` | Cookie test → never-set assertion | 2026-05-28 |
| `ServiceCard.vue` | Price labels + hex token | staged 2026-05-20 |
| `colorbar.js` | Duplicate action + mutation removed | staged 2026-05-20 |
| `colorbar.test.js` | 2 stale tests removed | staged 2026-05-20 |
| `EnhancementSection.vue` | emit → `trackMREventAndRedirect`; `emits:[]` | 2026-05-28 |
| `EnhancementSection.test.js` | Emit test → 4 CTA tests | 2026-05-28 |

### Pending

*   [ ] **Commit + push** — stage all 7 files, single commit, push to `DOTCOMPB-8121-clean`
*   [ ] **Update PR #20888 body** — add EnhancementSection fix + ServicesSection cookie removal to Changes; update Testing Coverage + QA steps
*   [ ] **PR review** — address any remaining reviewer comments in-thread per §1.44
*   [ ] **CI** — monitor CircleCI `website_tests` after push
*   [ ] **Production CMS for Carley** — documented in PR Special Deployment Requirements
*   [ ] **CMS Phases A-D** — deferred until hero PR #20771 merges
*   [ ] **Force Addon flow broken when nearest location exists** — investigate `initializeClosestLocation` / `loadLocation` interaction with Force Addon dispatch chain
*   [ ] **Carousel extra left-scroll space** — audit Swiper `slidesOffsetBefore`, `centeredSlides`, initial `translate`

### Where to resume

* **"commit"** → stage all 7 files above; commit message should cover 3 things: PR review ADA/Pilko fixes, ServicesSection no-preselect, EnhancementSection Book a Service
* **"PR"** → `gh pr edit 20888 --body-file <path>`; add both `[MOD] ServicesSection.vue` + `[MOD] EnhancementSection.vue` to Changes; reference Carley's Slack direction
* **"Force Addon"** → `initializeClosestLocation` → `loadLocation` → Force Addon dispatch chain when nearest location ≤50 mi
* **"Carousel"** → audit `swiperOptions` in `ServicesSection.vue`; check `slidesOffsetBefore`, `centeredSlides`, initial translate

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session. Newest row first. See `~/.claude/skills/session-reset/rules/activity-log.md` for the schema.

| Datetime         | Duration | Type           | Reference     | Description |
|------------------|----------|----------------|---------------|-------------|
| 2026-05-28 15:49 | 0.25h    | session-reset  | this          | Reset: EnhancementSection Book a Service fixed, AI issue rebutted. §5 updated — commit 7 files + update PR #20888 body next. |
| 2026-05-28 15:30 | 0.5h     | bug-fix        | DOTCOMPB-8121 | Fixed EnhancementSection.onBookService: emit('book-service') → trackMREventAndRedirect to /services page. emits:[] + constants added. 4 CTA tests replace 1 emit test. 28/28 passing. |
| 2026-05-28 15:10 | 0.15h    | pr-feedback    | PR #20981     | Replied to AI issue (r3320398822) flagging ServicesSection cookie removal as regression. Explained intentional fix — cookie bypassed Services page via navigateToAddons, not preserving a selection. |
| 2026-05-28 15:01 | 0.25h    | session-reset  | this          | Reset: service preselection removed, §5 replaced with current state + pending. Next: commit all staged+today changes + update PR #20888 body. |
| 2026-05-28 14:30 | 1h       | bug-fix        | DOTCOMPB-8121 | Removed selected_service cookie from ServicesSection.onServiceCtaClick (SERVICE_COOKIE_NAME constant + if(code) block + unused const code). Full 5-setter chain audited — only Marketing LP setter removed; 2 receivers + 4 other setters untouched. ServicesSection.test.js: deleted cookie-set test, repurposed to assert never-set with valid payload. 5910 tests passing. |
| 2026-05-28 13:30 | 0.5h     | research       | DOTCOMPB-8121 | Full preselection chain audit per Carley Slack direction. Traced: ServicesSection → selected_service cookie → ServicesPage.setServiceFromCookie → navigateToAddons → booking-addons skip. Plan written to .tasks/DOTCOMPB-8121/no-preselect-plan.md. |
| 2026-05-20       | —        | session-reset  | this          | Session reset — PR review complete. Pilko duplicates removed; ADA fixes (role, price labels, hex token); colorbar tests cleaned up; 5 in-thread replies + general comment posted on GH. Staged 4 files awaiting commit. Standalone Tophat architecture pattern documented. §5/§6 updated. |
| 2026-05-20       | 0.5h     | pr-review      | PR #20888     | Resolved all PR comments: Pilko duplicate action + mutation removed from colorbar.js; ADA role/price/hex fixed in ServicesSection + ServiceCard; colorbar.test.js stale initializeClosestLocation tests removed; in-thread replies posted for all 5 inline comments + general comment for Sentry LOW + race condition. CMS membership.content helpText updated with WCAG 2.4.4 link-text authoring guidance. |
| 2026-05-20       | 0.3h     | merge-conflict | DOTCOMPB-8121 | Merge origin/feat-location-s into DOTCOMPB-8121-clean. Three new commits on base: canonical initializeClosestLocation (commits 844ebc93254 + 7554ae2d43f) + LocationsSection (PR #20879 / b7a9cff8f91). Conflicts in mrVueApp.js + registerGlobalsSsr.js (keep both LocationsSection + ServicesSection) + colorbar.js (trivial trailing newline). colorbar.js now uses feat-location-s's initializeClosestLocation — full address→geo→IP fallback + getLocationForBooking after closest resolved. bookableServices additions untouched. Merge commit 8e0a922ab62. 563/563 green. |
| 2026-05-20       | 0.5h     | pr+branch      | PR #20888     | DOTCOMPB-8121-clean branch created from feat-location-s. 13-file clean diff: 9 new (ServicesSection, MembershipCallout, ServiceCard components + tests + indexes) + 4 modified (mrVueApp.js, registerGlobalsSsr.js, colorbar.js, colorbar.test.js). Resolved eslint-disable hook issue (mrVueApp.js had vue-rule disables invalid for eslint-plugin-vue v10 in root). PR #20888 opened. |
| 2026-05-20       | 0.5h     | cms-reorg      | template 1653 | CMS template 1653 (services-section) reorganised: sectionHeader "Promotional Banner" + "Service Cards" with options.heading rendered headings; variablePromo moved before services[]; variablePromo.copy removed; all helpTexts made concise; membership field → single html type; cv 19460+19464 membership data migrated from {copy,savingsCopy,ctaText} to {content: html}. |
| 2026-05-20       | 1.0h     | architecture   | DOTCOMPB-8121 | Full standalone migration: ServicesSection + MembershipCallout moved to LocationSpecific/ServicesSection/; services-section CMS template 1653 created and registered globally; locationSpecificColorbarV2 no longer mounts ServicesSection; componentList cv 19460+19464 updated to add services-section at position 2. MembershipCallout: 3 props replaced with single html field; modal dispatch + cta-click removed. variablePromo.copy removed everywhere. |
| 2026-05-20       | 1.5h     | code-review    | DOTCOMPB-8121 | Accessibility-lead ran full WCAG 2.2 AA audit. 7 findings fixed: Swiper A11y+Keyboard modules; MembershipCallout <a>→<button aria-haspopup="dialog">; MrIcon aria-hidden; resolvedCtaAriaLabel includes promo; NearestLocationCard primary CTA focus ring (1:1 white→cta-color-1 7.86:1); Services section aria-label fallback; dead titleId prop + CTA_LABEL→ctaLabel + emits:[] + secondary CTA aria suffix. 563/563 green. |
| 2026-05-20       | —        | session-reset  | this          | Session reset — Phase 2b + CMS schema + SSR perf complete. Cookie architecture validated (Maxi principle confirmed). 3-tier pricing implemented. `bookableServices` store wired. `saveAppointmentProgressToSession` removed. Empty-price guard added. 10 colorbar store tests (563/563 full suite green). Decisions 13-18 added. §2.4/§2.5/§4/§5/§6 updated. Next: unit tests for 4 components → /code-review → /create-pr. |
| 2026-05-20       | 1.5h     | investigation  | DOTCOMPB-8121 | **`productPrice` CMS field type deep investigation.** Traced: stores `{ productId, subPrice }` referencing `productcatalog.products` (e-commerce retail items). Rendered by legacy `mr-price` AngularJS directive via `productCatalogSvc.getProductPrices`. `bookableService.productId` for our 4 services maps to retail kits ("Root Touch Up Set: Light Brown" etc.) — NOT salon booking fees. Exhaustive survey: 23 Tophat field types + every HCB template — no service-picker field type exists anywhere. All HCB service-carousel templates use manual text `code` fields; no template has price fields (rely entirely on runtime `location.servicesOffered`). |
| 2026-05-20       | 0.3h     | performance    | DOTCOMPB-8121 | **SSR perf: `loadBookableServices` parallelised in wrapper `serverPrefetch`.** Moved dispatch from `ServicesSection.serverPrefetch` (sequential, after wrapper) into `LocationSpecificColorbarV2.serverPrefetch` alongside `getActiveLocationsListForMapView` in `Promise.allSettled`. Total SSR time now `max(getActiveLocationsListForMapView, loadBookableServices)` instead of sequential sum. `bookableServices` baked into `__INITIAL_STATE__` → client `mounted()` guard skips fetch → no price flash. CMS `basePrice.nonMember` removed from template 1650 schema (Tophat no longer shows the field; `basePrice` object now contains only `member`). `$unset` `basePrice.nonMember` from cv 19460 + 19464 at both paths. Backup `cms-backups/templateVersion/1650/2026-05-20T17-20-13-795Z-v1.json`. |
| 2026-05-20       | 0.5h     | implementation | colorbar.js   | **`bookableServices` store + 3-tier pricing.** Added state `bookableServices: []`, getter `bookableServicesByCode` (O(1) by code), mutation `setBookableServices`, action `loadBookableServices` (one-shot guard, error-swallowed) to `colorbar` Vuex module. Imported `getServicesList`. `ServicesSection.mounted()` triggers load. `resolvedNonMemberPrice` updated: Tier 1 live `location.servicesOffered[code].price` → Tier 2 `bookableServicesByCode[code].price` (global base, $70/$107/$107/$165 for the 4 services) → Tier 3 CMS `basePrice.nonMember`. Member stays 2-tier (no global member price on `bookableService`). `vueColorbarSvc` import + `saveAppointmentProgressToSession` try/catch removed. `onServiceCtaClick` now synchronous. Empty-price guard `v-if="memberPrice \|\| nonMemberPrice"` on `ServiceCard.service-price-row`. 7 new tests + state-isolation `freshStore()` helper. 563/563 suite green. Decisions #13–#18. |
| 2026-05-20       | 1.0h     | investigation  | DOTCOMPB-8121 | **Cookie architecture + base-price research.** Confirmed Maxi's decision #112 principle (8120) applies here: `/colorbar/locations` non-SPA, Vuex wiped, cookie-only is correct. `saveAppointmentProgressToSession` identified as residual from reverted `forceShowAddons` — fighting `setServiceFromCookie` in async `resumeUnsavedBooking`. `bookableService.price` (global base, set in Tophat at `#/colorbar/service/edit/:id`) confirmed accessible via `GET /api/colorbar/getServicesList` (no location needed). All 4 marketing LP services `bookableByClient: true`, prices: cbs_roots_only $70, cbs_all_over $107, cbs_roots_gloss $107, cbs_full $165. No global `proPlusMemberPrice` on `bookableService`. |
| 2026-05-19 14:00 | —        | session-reset  | this          | Session reset — finalized service CTA flow, reverted booking-flow files, confirmed auto-navigation via SSR+setServiceFromCookie, added decisions 10-12. Ready to stash. |
| 2026-05-19 12:00 | 1.5h     | bug-fix        | DOTCOMPB-8121 | Reverted `forceShowAddons` approach from HairColorBarBookingV2.vue + hairColorBarBooking.js. Auto-navigation to addons confirmed working via SSR serverPrefetch + setServiceFromCookie without booking-flow changes. |
| 2026-05-19 11:00 | 0.5h     | testing        | DOTCOMPB-8121 | Playwright test `addons-jump.spec.ts` — confirms cookie → /addons auto-navigation; stays on /services when no cookie. Step-by-step screenshots captured. |
| 2026-05-19 09:30 | 0.5h     | implementation | DOTCOMPB-8121 | ServicesSection CTA finalized: removed `_consult` transform, direct handoff via `this.location.code`, no `forceShowAddons` in session. NearestLocationCard secondary CTA + HeroSection `showSearchCard` mutual-exclusion added. |
| 2026-05-18 21:35 | —        | session-reset  | this          | Session reset — compacted Phase 1+2 build (ServicesSection, ServiceCard, MembershipCallout, pricing, booking CTA flow, force-skip-addons). Sections 2.3/2.4/2.5/3.2/5 updated. 9 key decisions recorded. Tests + PR still pending. |
| 2026-05-18       | 0.4h     | force-skip-addons | DOTCOMPB-8121 | **Force-skip-addons → direct-to-calendar even for services that carry addons**. The Marketing LP card click signals user intent to commit to that service "as-is" — bypass both services step AND addons step, land on calendar. Implementation across 3 files: **(1) `hairColorBarBooking.js`** — added `forceSkipAddons: false` to initial state (line 102); added `setForceSkipAddons(state, val)` mutation (line 222-224); extended `loadAppointmentBookingProgress` to destructure `forceSkipAddons` from `appointmentProgress` and commit it (line 1807, 1810). **(2) `HairColorBarBookingV2.vue`** — extended computed `mapState` from `['location']` to `['location', 'selectedService', 'selectedDate', 'selectedTime', 'hold', 'forceSkipAddons']` (fixed a latent issue: `resumeUnsavedBooking` referenced `this.selectedService/Date/Time/hold` at lines 185-189 but never mapped them — was previously a no-op for non-location fields). Extended the calendar branch to also route to `booking-calendar` when `this.forceSkipAddons` is true regardless of `addonServices.length`: `} else if (this.selectedService && (this.forceSkipAddons || !this.selectedService?.addonServices?.length)) { name = 'booking-calendar'; }`. **(3) `ServicesSection.vue`** — added `forceSkipAddons: true` to the `appointmentProgress` payload sent to `saveAppointmentProgressToSession`. **Net flow**: card click → `await vueColorbarSvc.saveAppointmentProgressToSession({ appointmentProgress: { selectedServiceCode, forceSkipAddons: true, ... } })` + cookie set → redirect `/colorbar/locations` → user picks salon → `/colorbar/booking/{code}/services` → `HairColorBarBookingV2.mounted` chain: `getLocation` → `loadBookingProgress` (commits `setSelectedService` + `setForceSkipAddons(true)`) → `loadInitialData` → `resumeUnsavedBooking` reads `forceSkipAddons=true` → **`$router.push({ name: 'booking-calendar', params: { code } })` regardless of addons**. User lands on calendar. |
| 2026-05-18       | 0.6h     | session-progress | DOTCOMPB-8121 | **Replaced cookie-only hand-off with session-based `appointmentProgress` so the booking flow's `resumeUnsavedBooking` auto-routes to calendar (or services if addons exist)**. Investigation traced the full chain that drives the "ton of processes" the user mentioned: `mr_modules/webservices/lib/colorbar.js:1413-1418` stores `appointmentProgress` on `req.session.appointmentProgress`; `vueColorbarSvc.saveAppointmentProgressToSession` (`services/vueColorbarSvc.js:217-218`) is the client-side entry; the booking-flow root `HairColorBarBookingV2.vue:138-149` chains `getLocation(code) → loadBookingProgress() → loadInitialData() → resumeUnsavedBooking()` on mount; `loadAppointmentBookingProgress` (`store/modules/hairColorBarBooking.js:1772-1824`) reads the session, resolves `selectedServiceCode` against `getters.servicesDictionary[code]` (with fallback to `location.servicesOffered.find(s => s.code === code)`), and commits `setSelectedService(service)`; `resumeUnsavedBooking` (`HairColorBarBookingV2.vue:176-203`) then `$router.push`-es to `booking-info` if `service + date + time + hold` all present, **`booking-calendar` if `selectedService` + no addons**, or `booking-services` if `selectedService` + has addons. **Implementation in `ServicesSection.onServiceCtaClick`**: (a) keep `_consult` suffix transform for first-time customers (`code.replace('_only', '') + '_consult'` when `!hadAppointment`) — verified `cbs_roots_consult` (74), `cbs_all_over_consult` (94), `cbs_roots_gloss_consult` (76) all exist in `appointments.bookableService`; (b) keep `this.$cookies.set('selected_service', code)` as fallback (V2's `ServicesPage.setServiceFromCookie` also reads it, so it works as belt-and-braces); (c) **new**: `await vueColorbarSvc.saveAppointmentProgressToSession({ appointmentProgress: { selectedServiceCode: code, selectedAddonCode: null, selectedAddOnTreatments: [], selectedTime: null, selectedDate: null, hold: null } })` — minimal payload clears prior progress and sets just the service; (d) `trackMREventAndRedirect(SERVICE_CTA_EVENT, '/colorbar/locations', payload)`. Made `onServiceCtaClick` async so the session write is awaited before redirect. Error-silenced catch (`void _err`) — cookie still drives the flow even if session save fails. **Net behavior**: card click → session+cookie set → redirect to locations → user picks salon → `/colorbar/booking/{code}/services` → resumeUnsavedBooking sees pre-selected service → routes to `booking-calendar` directly when the service has no addons, OR to `booking-services` with the user's selection pre-highlighted when addons exist. The 4 marketing-LP services all carry addons (3 each), so production behavior is "land on services page with selection highlighted, then advance to addons and calendar". Future enhancement: pass `isFeaturedService: true` equivalent through session to bypass addons unconditionally, mirroring `ServicesPage.navigateToAddons:329` branch. |
| 2026-05-18       | 0.3h     | service-cta-flow | DOTCOMPB-8121 | **Service-card CTA hand-off aligned with canonical `HairColorBarLocationServices.handleBookNow` pattern**. Investigation: found `ServicesPage.vue:239-257` in the booking-flow v2 — `setServiceFromCookie()` reads `$cookies.get('selected_service')`, matches it against `categories[].service.code` / `categories[].standInFor[].code`, and if a match is found, removes the cookie and calls `navigateToAddons(matchedCategory, false)` — auto-advancing past the services step UI. Net flow with cookie set: card click → cookie + redirect to `/colorbar/locations` → user picks a salon → location card navigates to `/colorbar/booking/{code}/services` → ServicesPage mounts → `setServiceFromCookie` fires → user lands on **addons (or calendar if service has no addons)** without seeing the services-step UI. The cookie is the documented hand-off contract; the booking flow's services page is the receiving side. **Refactor of `ServicesSection.onServiceCtaClick`**: (a) switched from `document.cookie` to `this.$cookies.set(...)` (VueCookies plugin registered globally in `mrVueApp.js:709`, used across all comparable handlers — matches `HairColorBarLocationServices.vue:118` + `ServicesList.vue:75`); (b) added `_consult` suffix transform for first-time customers verbatim from `HairColorBarLocationServices.vue:113-116` — when `cdata.hasBookedAppointment` is falsy, strip `_only` from the code and append `_consult` (e.g. `cbs_roots_only` → `cbs_roots_consult`) so the booking flow routes new customers through a consultation; (c) dropped the speculative `#services` URL hash — `/colorbar/locations` is the canonical destination, no anchor needed; (d) added `mapState('customer', ['cdata'])` + `hadAppointment` computed mirroring the precedent. Existing tracking event payload (`MREvent (Marketing LP – Service CTA clicked)`) unchanged. |
| 2026-05-18       | 0.25h    | promo-banner-fit | DOTCOMPB-8121 | Two refinements. **(1) Banner text fits on one line — general fix**: changed promo banner font from `.xs-f-small` (14px) → `.xs-f-xsmall` (12px), tightened padding `8px 16px` → `8px 10px`, set `letter-spacing -0.01em`, kept `line-height 1.3`, set `white-space: normal` so any pathologically long text safely wraps to 2 lines instead of overflowing. Curl confirms both banners render in full: `"New Client Perk: $70 Roots (Mon–Wed)"` and `"New Clients: $102 All-Over Color (Mon–Wed)"` — no truncation, no ellipsis. **(2) Final end-to-end pricing validation across CMS + bookableService + live API**: confirmed all 4 `serviceCode` values match across the three data sources (CMS cv 19460/19464 seed → `appointments.bookableService.code` → `Location.servicesOffered[].code` from `/api/colorbar/getLocation`). Live API at ny-huntington returns: `cbs_roots_only` $0/$85, `cbs_all_over` $29.60/$122, `cbs_roots_gloss` $32/$125; `cbs_full` NOT carried at this location → falls back to CMS basePrice $72/$90 — the fallback contract working as designed. SSR HTML shows the CMS basePrice values (since SSR runs before client `loadLocation` resolves); client hydration swaps to live values once `state.location` populates via the wrapper's `loadLocation` watcher (which fires when `closestLocations[0].distance ≤ 50` from `initializeBopis`). |
| 2026-05-18       | 0.75h    | live-pricing    | DOTCOMPB-8121 | **Live per-location pricing wired end-to-end + image-radius cleanup**. Investigation: traced the booking flow's price source. Found `mr_modules/appointments/lib/location.js:1055-1056` enriches `Location.servicesOffered[idx]` with `proPlusMemberPrice` + `proMemberPrice` on every `getLocation` API call (computed live from `service.price - proPlusServicePrice.discountPrice` per location). Live curl `/api/colorbar/getLocation?code=ny-huntington` confirmed shape — each `servicesOffered[i]` has `{ _id, code, name, image, description, price, proPlusMemberPrice, proMemberPrice, tags, ... }`. **`code` is the canonical lookup key** (cleaner than the numeric `_id` we had). The HCB booking flow uses these fields via `services` / `serviceTagMap` / `serviceMembershipPrices` getters in `hairColorBarBooking.js`; `addUnlimitedMembershipDiscounts` (`HairColorBarBooking/utils.js`) just selects which of the pre-computed member prices to surface as `discountPrice` based on customer tier — it does NOT compute anything itself. **Refactor**: `ServicesSection.activeLocationServicesByCode` computed builds `{ [code]: entry }` from `state.location.servicesOffered`. `resolvedMemberPrice` returns `$currencyShort(live.proPlusMemberPrice, true)` when present, else CMS `basePrice.member`. `resolvedNonMemberPrice` returns `$currencyShort(live.price, true)`, else CMS `basePrice.nonMember`. `$currencyShort` (`filters.js:12`) gives `$0` / `$29.60` / `$110-$160` formatting via global filtersPlugin. **Schema cleanup**: removed `services[].serviceId` (numeric) — no longer needed since matching is by string `code`. `serviceCode` helpText updated to clarify it's both the booking token AND the live-pricing lookup key. `basePrice` made non-required + helpText reframed as "FALLBACK ONLY" for when live data unavailable. Schema reapplied — had to recover from accidental `--mode replace` that wiped heroSection (restored via auto-backup `cms-backups/templateVersion/1650/2026-05-18T21-28-33-476Z-v1.json`, then re-merged the new servicesSection in merge mode). **Verification**: live API at ny-huntington returns `cbs_roots_only` ($85/$0), `cbs_all_over` ($122/$29.60), `cbs_roots_gloss` ($125/$32); `cbs_full` NOT carried at that location → falls back to CMS basePrice ($90/$72) — exactly the fallback contract working. **Image radius cleanup**: removed defensive `border-radius: 0` on `:deep(.image-box)` + `:deep(img)` per user — unnecessary noise; the image's visual bottom comes from the photo itself, not CSS rounding. Card structure now: image-wrap `border-radius: 6px 6px 0 0`, content `border-radius: 0 0 6px 6px`, card root no radius, image and image-box inherit no rounding. |
| 2026-05-18       | 0.4h     | banner-bk-align | DOTCOMPB-8121 | Four refinements aligning the Marketing LP service cards with the established `BookableCategories` brand pattern. **(1) Figma order**: cv 19460 + 19464 reseeded with the explicit Figma order `cbs_roots_only → cbs_all_over → cbs_full → cbs_roots_gloss` (Roots Coverage → All Over Color → Glossing + Blowout → Highlights), overriding the Hillsboro source's order. Implementation: read `templateData.componentList[lsv2].settings.servicesSection.services`, build a map by `serviceCode`, emit reordered array, write back at both paths. **(2) Images stay Tophat-sourced**: each card's `image` field is the FULL embedded media object copied from cv 19405's HCB v2 servicesList (CDN URLs `rectangle-1.png` → `rectangle-1-4.png`, mediaIds 16944-16947, alt text). No code-side hardcoding; CMS author can swap any image via Tophat. **(3) Promo banner now matches `BookableCategories` brand pattern**: dropped `VariablePricingMsgPill` import + usage. Built a custom `.service-promo-banner` element with the BookableCategories shape: `brand-color-1` bg + `padding 8px 16px` + `text-align center` + `color color-white` + `letter-spacing semi-regular-letter-spacing` + `xs-f-small.max-at-tweak`. Template: `span.promo-prefix.bold {{ prefix }}` followed by `{{ suffix }}` — `promoBannerParts` computed splits `promoCopy` at the FIRST colon (prefix includes the colon). Supports the two Figma variants ("New Client Perk: …" / "New Clients: …") plus arbitrary prefix-less copy (entire text renders regular weight if no colon). Banner stays overlay-positioned (`position absolute; top/left/right: 0; z-index: 1`) per prior "image height invariant under banner" requirement — image's pixel height unchanged whether or not banner is present. **(4) Image-bottom border-radius fix**: removed `border-radius: 6px` from `.service-card` root. Card now has no radius. Explicit `border-radius: 6px 6px 0 0` on `.service-image-wrap` (rounds top corners only) + already-present `border-radius: 0 0 6px 6px` on `.service-content`. Added defensive `border-radius: 0` on `:deep(.image-box)` and `:deep(img)` to neutralize any inherited rounding from ImgBox / browser defaults. Image's bottom edges now strictly square; transitions flat into the gray content area. SSR curl confirms render order `[Roots Coverage, All Over Color, Glossing + Blowout, Highlights]`, 2 promo banners with `span.promo-prefix.bold` + suffix text run, no `VariablePricingMsgPill` references, Hillsboro CDN URLs present. |
| 2026-05-18       | 0.75h    | hillsboro-defaults | DOTCOMPB-8121 | Adopted HCB Location V2 (Hillsboro) services as default fallback set + codebase-wide pricing investigation. **(1) Found canonical service set** — `content._id 2350` ("Color Bar Locations PDPs", uri `/colorbar/locations/` with `takesUrlParameters: true`), variation B → cv 19405 v42, mounts component `hcb-location-page-v2` (template 1639). Live data lives in `templateData.componentList[0].settings.servicesList` (NOT top-level `templateData.servicesList` — empty per the same `vue-component-list-ssr` baked-snapshot rule we use, 8120 §1.33). Confirmed 4 services with exact `{name, code, description, image}` shape: `cbs_roots_only` (Roots Coverage, media 16944), `cbs_roots_gloss` (Highlights, media 16945), `cbs_all_over` (All Over Color, media 16946), `cbs_full` (Glossing + Blowout, media 16947). **(2) Resolved service code → numeric ID via `appointments.bookableService`**: cbs_roots_only=27, cbs_roots_gloss=31, cbs_all_over=33, cbs_full=86. This is the cross-collection join key — same `_id` appears in `Location.servicesOffered._id` per location, with location-specific `price` values. **(3) Reseeded cv 19460 + 19464 at both paths** with the 4 Hillsboro-matching services (same names, descriptions, full image objects with the actual production CDN URLs from cv 19405, codes, plus our Marketing LP-specific basePrice/promoCopy/serviceId additions). Variable promo `applicableServiceCodes` resynced to `["cbs_roots_only", "cbs_all_over"]`. **(4) Updated schema helpText to document the pricing tier contract**: `serviceCode` cites `appointments.bookableService.code`; `serviceId` cites `appointments.bookableService._id` (with the 4 example mappings); `basePrice` helpText spells out the tier order — non-member tries live `Location.servicesOffered.price` first then falls back to `basePrice.nonMember`; member always uses `basePrice.member` because the appointments data model carries no member prices on Location. **Codebase pricing investigation findings (recorded for future enhancement)**: `Location.servicesOffered[i]` has only `{ _id, price, bookableOnline, treatmentsIncluded[], hidden }` — no member prices. Member prices come from `bookingFlowConfig` loaded via `dataToolSvc.getData({ mixinKey })` per location; the booking store's `serviceMembershipPrices` getter (`hairColorBarBooking.js:1090`) reads `service.proPlusMemberPrice` / `service.proMemberPrice` / `service.discountPrice` / `service.forMembersPrice` — fields populated by `BookingFlowServicesBuilder` (mr_modules) combining base service info + location overrides + customer membership tier. Loading the full `bookingFlowConfig` for the closest location on the Marketing LP would unlock live member prices but is heavy for a marketing page render; deferred as a future enhancement. Schema reapplied via `set-template-fields.mjs --mode merge --confirm`; backup auto-saved. Curl-verified all 4 services + their Hillsboro CDN URLs + the codes + numeric serviceIds + basePrice all present in SSR HTML. |
| 2026-05-18       | 0.3h     | image-source   | DOTCOMPB-8121 | Card description + service image refinements. **(1) Card description smaller on mobile/tablet**: `.service-description` font-size utility changed from `.xs-f-medium` (16px everywhere) → `.xs-f-small.lg-f-medium` (14px mobile/tablet → 16px desktop+). Matches the Figma desktop spec while keeping mobile/tablet copy proportional. **(2) Adaptive image rendering — `object-position: center top`**: added explicit `object-position: center top` to `:deep(img)` so any source aspect (portrait, landscape, square) gets cropped with the face/focal subject preserved instead of being chopped at the center. Combined with `aspect-ratio: 1/1` + `object-fit: cover`, the renderer now handles any future image aspect gracefully. **(3) Service images come from CMS — confirmed validated**: grepped the booking flow to verify the precedent — `BookableCategories.vue:15-21` renders `:media-id="category.image.mediaId"` where `category.image` comes from `bookingFlowConfig.service_categories[i].image` (CMS-authored, populated by Tophat). The `appointments.service` collection (empty locally) and `Location.servicesOffered[i]` (only `{_id, price, bookableOnline, treatmentsIncluded[], hidden}`) carry no images at the data layer. Our `cmsSettings.servicesSection.services[].image` field is the **same authoring pattern** the booking flow uses — production images are Carley's Tophat upload responsibility. **(4) Reseeded local placeholder images** with appropriate lifestyle/result portraits: `cbs_roots_only → _id 148` (7NCR after2 — red root result), `cbs_color_all → _id 152` (8NVA after1 — blonde face), `cbs_glossing_blowout → _id 156` (9NGV after — blonde shine), `cbs_highlights_partial → _id 160` (9NA after — blonde highlights). All four are CMS media at 180x240 portrait — adequate for local dev validation, real production photos via Tophat upload. SSR curl confirms all 4 distinct filenames and the new description responsive classes render. |
| 2026-05-18       | 0.3h     | mobile-polish  | DOTCOMPB-8121 | Three mobile/tablet refinements. **(1) Subtitle smaller + explicit non-bold on mobile + tablet**: changed `.services-subtitle` font-size utility from `.xs-f-xmedium` (18px everywhere) → `.xs-f-small.lg-f-xmedium` (14px mobile/tablet → 18px desktop+, matching Figma 18px desktop spec). Added explicit `font-family: f-primary` + `font-weight: 400` to scoped CSS so the description text is guaranteed regular weight (defensive against any cascade-inherited bold). **(2) Banner icon top-aligned on mobile + tablet**: removed `.align-center` utility from `.callout-row` template — scoped now has `align-items: flex-start` default + `align-items: center` at `@media mq-desktop-plus`. Icon's `align-items: flex-start` keeps it aligned with the first line of text instead of centered against the wrapped multi-line block. **(3) 4 distinct service images** (no more all-hero placeholder): inspected `cms.media` for service-themed entries; mapped each CMS card to a unique real local media doc — `cbs_roots_only → _id 40` (madison-reed-root-touch-up-compact.jpg), `cbs_color_all → _id 537` (color_chart.jpg), `cbs_glossing_blowout → _id 217` (7_color_gloss.jpg), `cbs_highlights_partial → _id 12` (madison-reed-gloss.png). Reseeded cv 19460 + 19464 at both paths via mongosh `$set` with `arrayFilters` on `componentList[lsv2]`. Each cv now embeds the full media object (not just _id) since contentVersion stores baked snapshots. SSR curl confirms all 4 distinct filenames render in the page HTML and the subtitle utility classes are `xs-f-small lg-f-xmedium`. Real production images stay Carley's job via Tophat upload. |
| 2026-05-18       | 0.15h    | banner-bp      | DOTCOMPB-8121 | Banner mobile + tablet breakpoint fix. **(1) Banner width 95% on mobile + tablet**: `.services-membership` default is now `width: 95%; margin: 24px auto`; `width: fit-content; max-width: 100%` only kicks in at `@media mq-desktop-plus` (≥960px). **(2) Banner text left-aligned on mobile + tablet**: dropped `.text-center` utility class from `.callout-text-col` template — now `text-align: left` by default, `text-align: center` at `@media mq-desktop-plus`. **(3) Moved layout breakpoint flip from `mq-tablet-plus` to `mq-desktop-plus`** for `.callout-row { justify-content }` and `.callout-text-col { flex }` — tablet (560-959px) now stays in the mobile-style 2-column layout (icon col + text col with `flex: 1 1 auto`, left-aligned text). Only at desktop+ does it flatten to inline-centered. |
| 2026-05-18       | 0.6h     | real-location  | DOTCOMPB-8121 | Real-location pricing + bold-primary fix. **(1) Removed `FORCE_NEAREST_LOCATION_FOR_TESTING`** from `HeroSection.vue` (constant + `nearestLocation` computed branch + the `async mounted()` that injected the seeded record). The IP-based `initializeBopis` chain in the wrapper now populates `closestLocations` naturally. Closes 8120 §2.5's "REMOVE before PR" row that was carrying this. **(2) Re-added `loadLocation` watcher to wrapper** (closes 8121 §2.3 open question #3): `LocationSpecificColorbarV2.vue` watches `closestLocations` (immediate: true); when `closestLocations[0].code` exists AND `distance <= NEARBY_RADIUS_MILES (50)`, dispatches `colorbar/loadLocation(closestLocations[0].code)` → `state.colorbar.location` populates with the full `Location` doc including `servicesOffered[]`. **(3) Pricing tier resolution updated to real shape**: read `Location.servicesOffered` shape — array of `{ _id (number), price (number), bookableOnline, treatmentsIncluded[], hidden }`. No `serviceCode` string on these entries; the cross-location-consistent join key is the numeric `_id` (27, 31, 32, 34 etc. — same service ID across all locations, only `price` varies). Added `services[].serviceId` (type number) to CMS schema; reseeded cv 19460 + 19464 at both paths with the IDs (cbs_roots_only=27, cbs_color_all=31, cbs_glossing_blowout=32, cbs_highlights_partial=34). **Non-member price**: lookup `state.location.servicesOffered.find(s => s._id === card.serviceId).price` → format as `$${price}`; fall back to CMS `basePrice.nonMember`. **Member price**: stays from CMS `basePrice.member` (no straightforward backend mapping for member-discounted service prices without loading the full hairColorBarBooking config — documented as future enhancement). **(4) Bold ACTUALLY on `.service-price-primary`**: applied the same belt-and-braces fix used for `.service-price-secondary-value` — explicit scoped `font-family: f-primary-bold` (not `f-primary`). The `.bold` utility class in `fonts.styl:51-54` is a font-family swap to `f-primary-bold` (Averta-Bold); my scoped CSS was setting `font-family: f-primary` at higher specificity, defeating it. SSR curl confirms no `FORCE_NEAREST` references in rendered output, both `service-price-primary` and `service-price-secondary-value bold` classes present. |
| 2026-05-18       | 0.25h    | figma-refine-5 | DOTCOMPB-8121 | Fifth refinement pass — 3 fixes. **(1) Proper full-width button override**: read `MrBtn.vue:222` — root `.mrbtn` has `max-width: max-content` (caps button at content width). MrBtn ships a documented variant `.btn-block` (`MrBtn.vue:283-288`) that resets via `max-width: revert; width: 100%; .btn-content { display: block }`. Switched template `MrBtn.service-cta.full-width...` → `MrBtn.service-cta.btn-block...`. Simplified scoped — removed redundant `:deep(.mrbtn)` overrides for bg/border/color (MrBtn already defaults to cta-color-1 + white text + white-on-hover). Only kept the `:focus-visible` 2px solid cta-color-1 outline at outline-offset 2px (matches hero pattern). **(2) Highlight price ACTUALLY bold (Averta-Bold)**: confirmed `.bold` class in `fonts.styl:51-54` defines `font-family: f-primary-bold; font-weight: normal; font-style: normal`. Added explicit scoped `.service-price-secondary-value { font-family: f-primary-bold }` as a belt-and-braces guarantee — even if cascade order or future scoped CSS interferes with the `.bold` utility, this scoped rule wins for the highlight price span. Template still keeps `.bold` for idiomatic MR style. **(3) Swiper SSR-to-hydrated repaint flash on gaps**: Swiper applies `spaceBetween` via inline styles at runtime, so SSR HTML has gap=0 → hydrated has gap=12 → visual repaint. Solved the same way `HairColorBarLocationServices.vue:149` does: added static `:deep(.swiper-slide) { margin-right: 12px }` matching the runtime `spaceBetween: 12` value, plus `:deep(.swiper-slide:last-child) { margin-right: 0 }`. SSR and hydrated states now render identical gaps. |
| 2026-05-18       | 0.2h     | figma-refine-4 | DOTCOMPB-8121 | Fourth refinement pass — 3 fixes. **(1) Carousel centered on desktop**: added `justify-content: center` on `:deep(.swiper-wrapper)` at `@media mq-desktop-plus`. Root cause: slides cap at `max-width 320px` × 4 = 1280px + 36px gap = 1316px, but `.services-section-inner` on a 1440px viewport stretches to 1440px → 124px empty space on right. Centering the wrapper redistributes that space symmetrically. Swiper's translate-based scroll still works because justify-content only applies at rest. **(2) `.bold` utility now actually works on highlight price**: removed the scoped `.service-price-secondary-value` rule that was duplicating `font-family: f-primary` from the parent. Per 8120 §1.26, `.bold` is a **font-family swap** to `f-primary-bold` (Averta-Bold) — NOT a font-weight modifier. Scoped CSS (higher specificity than global utility classes) was overriding the family swap and keeping the text in regular Averta. Removed the duplicate; child span now inherits color/size/letter-spacing/line-height from `.service-price-secondary` and `.bold` utility class wins for font-family. Class `.bold` confirmed present on the rendered span via SSR curl. **(3) More space between description and button**: changed `.service-description` margin from `0` to `0 0 1rem` — adds 16px below the description. Combined with parent's `gap: 0.75rem` (12px), total separation from description text to button is ~28px (and still works with `flex-grow: 1` on the description filling vertical space). |
| 2026-05-18       | 0.25h    | figma-refine-3 | DOTCOMPB-8121 | Third refinement pass — 4 fixes. **(1) Highlighted non-member price bold**: split `.service-price-secondary` into 3 inline runs — leading " member /", `.service-price-secondary-value.bold` wrapping `nonMemberPrice`, trailing " non-members". Bold variant explicitly re-declares font color/family/size/letter-spacing so it stays in the gray secondary palette. **(2) Description matches Figma `#343434` + 16px + 150% + -0.16px / -0.01em**: dropped scoped `color #746874`; template now uses `.text-color-1` (exact match for `#343434`) + `.xs-f-medium` (16px = font-size-medium). Default weight 400, scoped line-height-medium + letter-spacing -0.01em preserved. **(3) Banner hugs content + 24px h-padding**: `.services-membership` is now `width: fit-content; max-width: 100%; margin: 24px auto` — shrinks to longest text run, centered horizontally, banner's own `.xs-px-150m` provides the 24px L/R padding. Dropped speculative `align-self: center` (parent isn't flex so it was a no-op). **(4) Button truly full-width**: added `.full-width` utility class to MrBtn template + reinforced scoped `:deep(.mrbtn) { display: block; width: 100% }` (display:block needed because MrBtn root is inline-block by default — width:100% on inline-block doesn't expand without it). Curl-verified `service-cta full-width`, `service-price-secondary-value`, `text-color-1 max-at-tweak xs-f-medium`, and `services-membership` all present in SSR HTML. |
| 2026-05-18       | 0.5h     | figma-refine-2 | DOTCOMPB-8121 | Second refinement pass — 6 fixes. **(1) Horizontal centering bug fix**: removed invalid `.div-center` utility (not in mr-style — was a no-op); `.services-section-inner` now centers via scoped `margin 0 auto` + `max-width bp-desktop-large` + `width 100%`. **(2) Title matches hero pattern**: dropped scoped `.services-title` font-family/transform/weight/letter-spacing rules; now uses utility classes mirroring hero's h1 — `.color-mr-purple.f-domaine-display-condensed.upper.max-at-tweak.text-center.xs-f-grande.md-f-xgrande.lg-f-xxxxgrande.xl-f-poster` (size scale kept from services Figma 36→72px). **(3) Image no bottom radius**: dropped `.border-radius-6` utility class from `.service-image-wrap`; the card's own `overflow hidden + border-radius 6px` clips top corners naturally and the content block below has its own `border-radius 0 0 6px 6px` — so image's bottom corners are square. **(4) Book Service button = hero's NearestLocationCard CTA**: added `round` prop to MrBtn; scoped `:deep(.mrbtn)` rules mirror `NearestLocationCard.vue:190-206` `location-cta--primary` exactly (cta-color-1 bg + 2px solid cta-color-1 border + white text + `darken(cta-color-1, 10%)` hover/active/focus-visible + 2px solid cta-color-1 outline on focus-visible at `outline-offset 2px`); `width 100%` added for full-card-width. **(5) Banner: NOT two-column on desktop**: `.callout-text-col` is `flex 1 1 auto` at mobile (gives a true 2-col with text claiming remaining width) and switches to `flex 0 0 auto` at `mq-tablet-plus` (text shrinks to content → icon+text group inline-centered via parent's `justify-content: center`). Parent `.callout-row` mirrors with `justify-content: flex-start` (mobile) → `center` (≥tablet). **(6) "Line of separation" fix**: defensive `border 0` on `.services-section`, `.services-header`, `.services-subtitle` so no inherited / default border can render between the description and the rest. **(7) Card content background**: `.service-content` now `background-color ui-color-3` (#F7F7F8 — exact Figma match) + `border-radius 0 0 6px 6px`; card root background made transparent so only the content area has the gray fill; image area keeps its `ui-color-4` skeleton until the asset loads. Curl-verified all 14 class markers render in SSR HTML. |
| 2026-05-18       | 1.0h     | figma-refine   | DOTCOMPB-8121 | Refined `ServicesSection.vue`, `ServiceCard.vue`, `MembershipCallout.vue` to match Figma desktop (1440px) + mobile (440px) specs. **Title**: scoped `.services-title` consolidating `.f-domaine-display-condensed` + `color brand-color-1` + `text-transform uppercase` + `font-weight 500` (mobile) → `400` (desktop+) + `letter-spacing 0.02em`, plus utilities `.text-center.max-at-tweak.xs-f-grande.md-f-xgrande.lg-f-xxxxgrande.xl-f-poster`. **Subtitle**: scoped `.services-subtitle` with `color #746874` + Averta + `line-height-xsmall` + `letter-spacing 0.02em`, plus `.xs-f-xmedium.text-center.max-at-tweak`. **Banner**: scoped two-column layout (icon col `.no-shrink` + text col `.flex-1`); padding `.xs-py-75m.xs-px-150m` (12px 24px); `.brand-color-1-bg.border-radius-6`; `font-size font-size-medium` (16px) + `line-height-small` (1.3em) + `letter-spacing -0.01em`; lead `.bold`, savings `.normal`, link `.underline.color-white`; `.space-center` keeps icon+text grouped centered per Figma. **Card price**: two spans — `.service-price-primary` (`cta-color-1` + `21px` + bold + `line-height-xsmall` + `letter-spacing -0.01em`) and `.service-price-secondary` (`#746874` + `font-size-small` + `line-height-medium` + `letter-spacing -0.01em`). **Book service button**: `.service-cta.full-width` + `:deep(.mrbtn) { width: 100% }` for full-width inside card. **Equal-height cards (generalized)**: `height: 100%` on `.service-card` + `flex 1 1 auto` on `.service-content` + `flex-grow 1` on `.service-description` + `margin-top auto` on `.service-cta`; combined with already-present `:deep(.swiper-wrapper) { align-items: stretch }` → all cards stretch to tallest sibling regardless of count. **Image height invariant under banner**: per-card promo banner restructured as overlay (`.service-promo-overlay` with `position absolute; top: 0; left: 0; right: 0; z-index: 1`) over `.service-image-wrap` (`position relative; aspect-ratio 1/1`) — mirrors `BookableCategories` `.variable-pricing-overlay` precedent. **56px gap between sections**: `.xs-mt-350m` on section root (350m = 3.5em = 56px). **Sticky-header overlap fix**: `position relative; z-index 0` on `.services-section` isolates stacking context below the page-level `hcb-landing-sticky` header. Curl-verified all 14 new class names render in SSR HTML on `/colorbar/location-specific` with `experiments_504=b`. |
| 2026-05-18       | 0.4h     | cms-seed       | cv 19460 + 19464 | **Phase F dual-path seed** complete per §1.33. Direct mongosh `$set` on cv 19460 (published v55) AND cv 19464 (edit v56), hitting BOTH `templateData.servicesSection` (top-level) AND `templateData.componentList.$[c].settings.servicesSection` (SSR-rendered snapshot) with `arrayFilters: [{ "c.mixin_key": "location-specific-colorbar-v2" }]`. Seeded sample: title + subtitle + membership callout + 4 service cards (Roots/All Over/Glossing/Highlights) with prices matching Figma + 2 cards flagged for variable promo (`cbs_roots_only`, `cbs_color_all`) + per-card promo overrides ("New Client Perk: $70 Roots (Mon–Wed)" / "New Clients: $102 All-Over Color (Mon–Wed)"). All 4 service cards use media `_id=7272` (hero placeholder); Carley swaps real images in Tophat for prod. Verified: `curl -s --cookie "experiments_504=b" http://localhost:3000/colorbar/location-specific` returns HTTP 200 with `services-section`, `services-swiper`, `service-slide`, `membership-callout`, `variable-pricing-pill`, all 4 service titles, all 4 prices, "Book service" CTA, both per-card promo banners present in SSR HTML. **Section renders end-to-end.** |
| 2026-05-18       | 0.5h     | cms-apply      | template 1650 | `servicesSection.*` schema applied to local Mongo cms.templateVersion._id=5707 via `set-template-fields.mjs 1650 --src .tasks/DOTCOMPB-8121/services-schema.json --mode merge --confirm`. Backup at `cms-backups/templateVersion/1650/2026-05-18T19-57-56-817Z-v1.json`. **Trimmed schema** before applying — dropped `membership.ctaAriaLabel`, `membership.ariaLabel`, `services[].imageAlt`, `services[].ctaText`: not business-customizable (a11y plumbing + universal UX language). `MembershipCallout.vue` now derives `aria-label` from `ctaText` and hardcodes `REGION_ARIA_LABEL = 'Membership benefits'`; `ServiceCard.vue` hardcodes `CTA_LABEL = 'Book service'` and reads `alt_text` directly from the media object (falls back to `serviceName`). 20 fields under `servicesSection.*` verified via `get-template-fields.mjs --flat`. `heroSection.*` untouched (merge mode). Phase F (cv 19460+19464 dual-path seeding per §1.33) NOT yet run — schema apply alone does not render content. |
| 2026-05-18       | 1.0h     | phase-1-build  | DOTCOMPB-8121 | First Vue scaffold for Services Section authored in parallel-branch mode (working tree on `DOTCOMPB-8120`, destination `DOTCOMPB-8121`). Created `ServiceCard.vue` + `index.js` at `@components/ServiceCard/`; created `MembershipCallout.vue` + `index.js` at `@components/LocationSpecific/LocationSpecificColorbarV2/components/MembershipCallout/`; created `ServicesSection.vue` + `index.js` at `@components/LocationSpecific/LocationSpecificColorbarV2/components/ServicesSection/`; modified wrapper `LocationSpecificColorbarV2.vue` to mount `ServicesSection` below `HeroSection`. Reuses `VariablePricingMsgPill` (`:redesign="true"`) for per-card promo banner, `LimitlessProPlusV2Modal` for AC4 Pro+ modal, Swiper carousel mechanics adapted from `HairColorBarLocationServices`, card visual style adapted from `BookableCategories`. 3-tier pricing resolution implemented per §1.48.3. Cookie-based service hand-off + `trackMREventAndRedirect` per §1.48.6. Membership CTA dispatches `modal/showModal` with `component: 'LimitlessProPlusV2Modal'` per §1.48.4. §4 File Index updated with destination-branch column per HIGH PRIORITY rule. Tests + `LocationSpecificColorbarV2.test.js` delta still owed. |
| 2026-05-18       | 0.5h     | playbook-embed | this          | Added §1.48 "Embedded 8120 Playbook — AC-relevant excerpts (self-contained)" — 11 sub-sections (1.48.1 wrapper contract → 1.48.11 CMS reshape recap) bringing the actual *content* of 8120's §1.1–1.44 into this session, reorganized by Services AC. §1.0 inheritance map remains as the cross-reference index; §1.48 is now the primary working playbook so the Worker doesn't have to cross-load the 8120 session for routine pattern lookups. Updated §5 Last Interaction to reflect parallel-branch workflow + HIGH PRIORITY file-documentation rule. |
| 2026-05-18       | 0.1h     | memory-rule    | feedback_document_every_file_created.md | New HIGH PRIORITY user-memory rule: every file created MUST be documented immediately in session File Index or roam node (parallel-session stash-and-move workflow). Stashing is on user's explicit call only — never automatic. Indexed at top of MEMORY.md. |
| 2026-05-18       | 0.15h    | pr-check       | PR #20771     | Verified Hero PR state: OPEN against `feat-location-s`, mergeable CLEAN, review decision APPROVED (github-actions), all CI green (24 CircleCI workflows + auto-lint + security + codecov), 1 outstanding mr-minion ADA blocker on `LocationSearchInput` (combobox pattern; hero-only, does not gate 8121). HEAD commit `7047351` (merge from `feat-location-s`). |
| 2026-05-12 11:27 | 0.5h     | session-create | this          | Session file created for DOTCOMPB-8121. Section 1 inherited by reference from `dotcompb-8120-marketing-lp-hero.md` (44 subsections mapped via §1.0 inheritance table with per-row applicability + delta notes). Sections 1.45 (pricing source resolution), 1.46 (service-card hand-off via `selected_service` cookie), 1.47 (Pro+ modal hand-off) added as Services-specific overlays. §2 scope + open questions + pending work seeded. §3 component tree + contract + test targets drafted. |
| 2026-05-12 11:27 | 0.1h     | index-update   | madison_reed  | Index file updated — BACKLOG `<<ticket-8121>>` entry added with UUID + description; Sprint Board IN PROGRESS lane gained one row (count `[0/4] → [0/5]`); BACKLOG cookie recalculated (`[59%] [23/39] → [58%] [23/40]`). No nesting under DOTCOMPB-8120 — no JIRA issue link between siblings; both are direct children of the 8119 epic. |
| 2026-05-12 11:27 | 0.25h    | node-create    | DOTCOMPB-8121 | Roam node generated via `/mr-roam-node` skill at `~/.brain.d/roam-nodes/madison_reed/2026-05-12-112750-dotcompb_8121.org` (UUID `ab99a73b-cb11-4dc9-8b7b-f25b148dce0a`). 7 JIRA AC normalized to GIVEN/WHEN/THEN org-mode entries; EVENT TRACKING sub-section captured as org table with 2 segment events (`Membership Learn More clicked`, `Service CTA clicked`); RELEVANT LINKs include Figma frame, parent epic 8119, sibling hero 8120 (org-roam ID link). TODO TICKET TASKs seeded with 9 items mapped to AC1-AC7. |

<!-- DESCRIPTION AND USER CONTEXT END -->

<!-- INIT OF THE USER PROMPT END -->
