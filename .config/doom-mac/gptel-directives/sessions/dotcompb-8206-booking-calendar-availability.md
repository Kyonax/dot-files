<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the DOTCOMPB-8206 (Booking Flow Optimization — Availability Display) session. **Implementation complete and PR under review.** DOTCOMPB-7943 (arrow nav + fade-in) reconciled into branch by jsalazar-MR. DOTCOMPB-8480 (blank-calendar second-mount bug) fixed, pending commit. PR #20908 open, awaiting human reviewer approval. 142 tests passing (CalendarPage suite).

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, patterns, and conventions for ALL work in this session. | Before any follow-up task. |
| **2. Session Overview** | Scope, AC status, all decisions made. | When picking up the session. |
| **3. Implementations** | Component architecture, key decisions, test coverage. | When reviewing what was built or continuing work. |
| **4. File Index** | All created and modified files. | When reading, editing, or locating files. |
| **5. Last Interaction** | Current state, open items, next steps. | At conversation start — entry point. |
| **6. Activity Log** | Datetime-stamped audit trail. | When you need exact "what was done when". |

**Operational Rule:** Load `dotcom-dev` and `mr-style` before any code task. For further PR work: `gh pr view 20908`.

**Architectural baseline:** Inherits from `site-revolution-redesign.md`. All components under `website/src/vuescripts/components/HairColorBarBookingV2/`.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `dotcom-dev`, `mr-style`, `code-review`, `mr-roam-node`, `pr-scribe`. Section 1 stores session-scoped patterns not yet in those skills.

### 1.1 Framework & API (inherited)

*   **Vue 3 — Options API only** (`export default { ... }`). No Composition API, no `<script setup>`.
*   **Templating:** Pug (`<template lang="pug">`). **Styling:** Scoped Stylus (`<style lang="stylus" scoped>`).
*   **JS Syntax:** Always curly braces for `if` statements, even single-line returns.
*   Semicolons required. Single quotes. No `console.*` — use `Log` module.

### 1.2 Utility-First Styling (inherited)

*   **Utility classes first**, Stylus only for what utilities can't express.
*   **`.max-at-tweak` is MANDATORY** on every responsive font class (`.xs-f-*`, `.sm-f-*`, etc.).
*   **Font sizes on text elements must be utility classes in the template**, never `font-size` in Stylus (rule-pj-mrd-018). Exception: `font-size` on `svg` for icon sizing is acceptable (existing pattern).
*   **Design system variables only** — `brand-color-*`, `cta-color-*`, `text-color-*`, `ui-color-*`. `#EFEFF1` (Secondary Cloud) and `#3A2D4A` (Primary Nightshade) are used in this ticket but no Stylus variables exist — request additions before next ticket using these colors.
*   **Padding/margin utility classes MUST use breakpoint prefix** — `.xs-pt-50m` not `.pt-50m`.
*   **Flex layout stays in Stylus**, not utility classes.

### 1.3 Accessibility (inherited + session additions)

*   **`aria-labelledby` references heading IDs, never root element IDs.**
*   **Visual-only tooltips: `aria-hidden="true"`, NO `role="tooltip"`.** Both attributes are contradictory — `role="tooltip"` registers as AT-accessible, `aria-hidden` hides it. When the tile `aria-label` already conveys context, use only `aria-hidden="true"`.
*   **`aria-label` must encode availability status, not just the date.** Visual states (closed, waitlist) that are conveyed through CSS or JS tooltips must also be present programmatically in `aria-label`. Pattern: `dateCardAriaLabel(date)` / `cellAriaLabel(cell)` methods returning status-qualified strings.
*   **`role="button"` divs must have `tabindex` and `@keydown` handlers.** `tabindex="-1"` for disabled tiles (out of tab sequence), `tabindex="0"` for interactive ones. `@keydown.enter.prevent` and `@keydown.space.prevent` delegate to the same click handler. Existing guards inside the handler no-op on unavailable targets. Swiper's arrow-key navigation module does not conflict with Tab-order `tabindex`.
*   **Use `aria-selected` not `aria-pressed` for date pickers.** `aria-pressed` implies a two-state toggle; `aria-selected` is semantically correct for single-select widgets.
*   **`:focus-visible` required once `tabindex="0"` is added.** Use `outline 2px solid cta-color-1; outline-offset 2px; border-radius 6px`. Use `:focus-visible` (not `:focus`) to keep mouse clicks ring-free.
*   **Disabled tiles need `aria-disabled="true"`** — binding outputs `'true'` or `undefined` (never `'false'`).
*   **Timer icon inside waitlist badge: `aria-hidden="true"`** — decorative; "Waitlist" text is the AT label.
*   **Live region for async date load:** `.sr-only(aria-live="polite" aria-atomic="true")` at the top of `.date-picker`; inner `span(v-if="hasLoadedOnce")` announces "Calendar dates loaded." when dates first arrive. Pattern from `AddonsUpsell.vue` / `SignInOptions.vue`.

### 1.4 Vuex Store Patterns

*   **Single `setAllData` commit** for multiple state fields. (See `rules/vuex-batch-commits.md`.)
*   The booking flow Vuex module is **`hairColorBarBooking`** (namespaced). `fetchAvailableTimes` already filters `available: false` slots — AC3 was pre-satisfied.
*   **`fetchAvailableTimes` returning empty for a waitlist date is intentional.** Empty `availableTimes` with a `selectedDate` triggers `TimePicker`'s `noTimesAvailable` branch → "SELECTED DATE IS FULLY BOOKED" + "JOIN THE WAITLIST" CTA. This is the designed waitlist flow, not a bug.
*   **`datesMap` past-date override must null BOTH availability flags.** Setting only `available: false` while spreading `...date` leaves `waitlistEligible` intact from the API response — a past waitlist-eligible date bypasses every interactive guard (`cellClasses`, `onCellClick`, `tabindex`, `aria-disabled`). Always use: `{ ...date, available: false, waitlistEligible: false }`.

### 1.5 CalendarPage Component Architecture

```
CalendarPage.vue           ← orchestrator (weekly/monthly toggle, date + time selection)
  ├── DatePicker.vue       ← weekly swiper carousel OR monthly grid toggle host
  │     └── MonthlyCalendar.vue  ← 7-column CSS grid, full-month view
  ├── TimePicker.vue       ← 2-column time slots + waitlist/fully-booked states (unchanged)
  └── NearbyLocations.vue  ← alternate location carousel (unchanged)
```

**Parent:** `HairColorBarBookingV2.vue` — thin `router-view` wrapper, no shared state relevant to tile styling.

**Date object shape** (from `hairColorBarBooking` Vuex store):
```javascript
{
  calDate: 20260521,        // YYYYMMDD — primary key
  available: true,          // can be booked directly
  waitlistEligible: false,  // full but waitlist open
  variablePricePromo: false,// show promo banner
  dayOfWeek: 4,             // 0=Sun…6=Sat
  dayNum: 21,               // day of month 1–31
}
```

### 1.6 Day Tile Availability States — Full Figma Spec

**Figma node:** `6967-14470` | **URL:** https://www.figma.com/design/F3Rl6bQqmTgIlNOXbMmG0Y/2026-Redesign?node-id=6967-14470

**State 1 — Closed/Unavailable** (`available: false, waitlistEligible: false`)
- Gray fill `#e8e8e8`, gray border `#c8c8c8`, gray text `#746874`, CSS gradient diagonal strikethrough. NEVER promo banner, NEVER waitlist badge. `aria-label`: `"Wed 3, Closed or unavailable"`.

**State 2 — Available** (`available: true, variablePricePromo: false`)
- White bg, 1px `brand-color-1` border, `brand-color-1` day text. Hover: `cta-color-1` border + `#EFEFF1` circle on number. Select: 2px `cta-color-1` border, `#821577` circle. `aria-label`: `"Select Wed 3"`.

**State 3 — Waitlist** (`available: false, waitlistEligible: true, variablePricePromo: false`)
- White bg, `brand-color-1` border. Waitlist badge at bottom. Hover: tooltip "Waitlist spots available" + `#EFEFF1` circle. Select: same as Available select + badge persists. `aria-label`: `"Select Wed 3, Waitlist spots available"`.

**State 4 — Available with Promo** — `brand-color-1` banner at top in ALL states. Otherwise same as Available.

**State 5 — Waitlist with Promo** — State 3 + State 4 combined automatically via independent `v-if` conditions.

**MonthlyCalendar:** Icon-only (timer, no text). Waitlist cell: `#EFEFF1` bg, `#3A2D4A` border. Tooltip via CSS `::after` + `:focus-visible`. `cellAriaLabel()` mirrors DatePicker label pattern.

### 1.7 CSS Class Mapping — Implemented

**DatePicker (4 tile classes + 1 structural):**
- `.date-card-disabled` — `!available && !waitlistEligible`
- `.date-card-available` — `!isSelected && available`
- `.date-card-waitlist` — `!isSelected && !available && waitlistEligible`
- `.date-card-selected` — `isSelected && (available || waitlistEligible)`
- `.date-card-has-waitlist-badge` — all waitlist tiles regardless of selected; carries `padding-bottom: 25px`

**DatePicker inner elements:** `.date-card-perk`, `.date-card-waitlist-badge`, `.date-tooltip` (JS mouse-following, `position: fixed`)

**MonthlyCalendar (4 cell classes):** `.cell-disabled`, `.cell-available`, `.cell-waitlist`, `.cell-selected`

**MonthlyCalendar inner elements:** `.cell-promo-icon`, `.waitlist-icon` (chip at `bottom: 3px; left: 3px`)

### 1.8 Tooltip Implementation — Two Strategies

*   **DatePicker:** JS mouse-following (`position: fixed`, `clientX/Y + 14px`). Escapes both `overflow: hidden` stacking contexts. Methods: `onDateHoverEnter`, `onDateHoverMove`, `onDateHoverLeave`.
*   **MonthlyCalendar:** CSS `::after` + `content: attr(data-tooltip)`. Guarded by `@media (hover: hover)`. Extended to `:focus-visible` for sighted keyboard users.
*   **Never use `isDesktop` getter for tooltip visibility** — detects screen width, not pointer device. Use `@media (hover: hover)`.

### 1.9 Diagonal Strikethrough — CSS Gradient Pattern

```stylus
background-image linear-gradient(to bottom right, transparent calc(50% - 0.5px), #c8c8c8 calc(50% - 0.5px), #c8c8c8 calc(50% + 0.5px), transparent calc(50% + 0.5px))
```
Goes corner-to-corner at the correct angle for any element aspect ratio. Replaces `rotate(-45deg) ::before` which only worked on square elements.

### 1.10 Naming Conventions (code review findings)

*   **Root class ≤ 12 chars** (scoped styles prevent conflicts).
*   **No redundant prefix on children** scoped inside a parent selector.
*   **WHAT comments banned.** Only WHY comments (non-obvious constraints, cascade interactions, workarounds).

### 1.11 Testing Conventions

*   Tests co-located: `DatePicker.test.js`, `MonthlyCalendar.test.js`, `CalendarPage.test.js`.
*   **SwiperSlide stub pattern** — string stubs swallow slot content. Use:
    ```javascript
    Swiper: { template: '<div><slot /></div>' },
    SwiperSlide: { inheritAttrs: false, template: '<div v-bind="$attrs"><slot /></div>' },
    ```
*   `shallowMount` + `createWrapper` pattern. No snapshots. Test behavior, not structure.
*   **`fetchAvailableTimesSpy`** is now a file-scope spy in `CalendarPage.test.js` (alongside `fetchAvailableDatesSpy`) — extract it from `createMockStore` to verify time-loading assertions.
*   Run: `cd website && npm run test:vue DatePicker MonthlyCalendar CalendarPage`

### 1.12 Auto-Select Invariants — CalendarPage

**Never break these rules when touching `CalendarPage.vue`:**

*   **Auto-select rule is identical across all 3 active paths** (`serverPrefetch`, `mounted().then()`, `mounted() else-if`): `find(d => d.available || d.waitlistEligible)` — waitlist included, never `d.available` alone.
*   **Auto-select never overrides an existing selection.** Always guard with `if (!this.selectedDate)`.
*   **Times always load after auto-select**, and whenever `selectedDate` is set but `availableTimes` is empty. Guard: `else if (!this.availableTimes?.length)`.
*   **Do NOT add auto-select to the `availableDates` watcher** — it fires during week/month navigation and would fight `clearSelectionIfOutOfRange` (user is intentionally browsing a new window).
*   **Do NOT add auto-select to the `serviceIdToBook` watcher** — defensive fallback only; not a real booking-flow scenario.
*   Full entry-point map and state matrix: `.tasks/DOTCOMPB-8480/plan.md`.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Ticket Scope

**DOTCOMPB-8206 — Booking Flow Optimization: Availability Display**
Branch: `DOTCOMPB-8206` | JIRA: https://madison-reed.atlassian.net/browse/DOTCOMPB-8206
PR: https://github.com/MadisonReed/mr/pull/20908 | Figma: https://www.figma.com/design/F3Rl6bQqmTgIlNOXbMmG0Y/2026-Redesign?node-id=6967-14470

| AC | Summary | Status |
|---|---|---|
| AC1 | Unavailable dates grayed out + diagonal strikethrough | ✅ Done |
| AC2 | Waitlist-eligible dates show waitlist badge indicator | ✅ Done |
| AC3 | Held time slots hidden from available display | ✅ Pre-existing (store filter) |
| AC4 | Desktop hover shows availability tooltip | ✅ Done |
| AC5 | Auto-select next available/waitlist date on load | ✅ Done |

**DOTCOMPB-8480 — Blocks 8206** | Bug: Calendar sometimes mounts blank on second SPA visit

| Item | Summary | Status |
|---|---|---|
| Root cause | `setSelectedService` action clears `selectedDate`, not `availableDates`; `else-if` branch skips auto-select | ✅ Fixed |
| Fix | `mounted() else-if` branch now auto-selects + loads times when dates pre-loaded but `selectedDate` null | ✅ Implemented |
| Tests | 3 regression tests added to `CalendarPage.test.js` (33 total) | ✅ Done |
| Commit | Fix not yet committed to branch | ⏳ Pending |

### 2.2 Key Decisions

1. **(2026-05-22) AC3 pre-satisfied.** `fetchAvailableTimes` filters `available: false` at store level.
2. **(2026-05-22) Diagonal strikethrough: CSS gradient, not `::before`.** `rotate(-45deg)` wrong angle for 125×180px cards.
3. **(2026-05-22) DatePicker tooltip: JS mouse-following at root level.** Escapes double `overflow: hidden` stacking contexts.
4. **(2026-05-22) `date-card-has-waitlist-badge` structural class.** `date-card-selected` and `date-card-waitlist` are mutually exclusive — padding must live on a separate class spanning both.
5. **(2026-05-22) MonthlyCalendar waitlist: icon-only.** Figma note: "If copy too small, use icon instead."
6. **(2026-05-22) MonthlyCalendar waitlist cell: `#EFEFF1` bg, `#3A2D4A` border.**
7. **(2026-05-22) `.waitlist-icon` at `bottom: 3px; left: 3px`.** Avoids covering `cell-selected` border/outline.
8. **(2026-05-22) `justify-content: center` on `.date-card-body`.** Non-badge cards center vertically.
9. **(2026-05-22) Promo banner guard on disabled tiles.** `&& (date.available || date.waitlistEligible)` added to `v-if`.
10. **(2026-05-22) `numberClasses` bug fixed.** `number-selected` now uses `available || waitlistEligible`.
11. **(2026-05-22) AC5 includes waitlist-eligible dates.** Both `serverPrefetch` and `mounted` use `d.available || d.waitlistEligible`.
12. **(2026-05-22) `role="tooltip"` removed from `.date-tooltip`.** Contradicted `aria-hidden="true"`.
13. **(2026-05-22) PR review — `tabindex` implemented.** `tabindex="-1"` for disabled tiles, `tabindex="0"` for interactive. `@keydown.enter.prevent` + `@keydown.space.prevent` on both DatePicker SwiperSlides and MonthlyCalendar cells.
14. **(2026-05-22) PR review — `aria-label` encodes availability status.** `dateCardAriaLabel()` / `cellAriaLabel()` methods produce status-qualified labels.
15. **(2026-05-22) PR review — `aria-pressed` → `aria-selected`.** `aria-pressed` implies toggle; `aria-selected` is correct for single-select date pickers.
16. **(2026-05-22) PR review — `:focus-visible` added to both components.** `outline 2px solid cta-color-1` on `.date-card:focus-visible` and `.calendar-cell:focus-visible`.
17. **(2026-05-22) Bug prediction rebutted — AC5 `waitlistEligible` auto-select is intentional.** Empty `availableTimes` for a waitlist date triggers `TimePicker`'s `noTimesAvailable` branch → waitlist CTA. Not a bug.
18. **(2026-05-27) `datesMap` past-date override must null both flags.** Spreading `...date` preserved `waitlistEligible: true` from the API for past dates — bypassing every disabled guard. Fixed: `{ ...date, available: false, waitlistEligible: false }`. Regression test added.
19. **(2026-05-28) DOTCOMPB-7943 reconciled into branch by jsalazar-MR.** Arrow nav (prev/next buttons), `hasLoadedOnce` fade-in, `<button type=button>` for show-calendar, arrow aria-labels, `onArrowClick` tracking — preserved from 7943. Tile states, a11y (aria-selected, tabindex, focus-visible, encoded aria-labels, live-region for SR) applied on top. DatePicker.test.js: 42 → 53 tests (+11 from 7943 features).
20. **(2026-05-28) DOTCOMPB-8480 root cause confirmed.** `setSelectedService` action calls `commit('setSelectedDate', null)` but does NOT clear `availableDates`. `mounted() else-if` branch (taken when dates pre-loaded) was missing auto-select logic. Fix: mirrors `.then()` block — `!selectedDate → auto-select`; `selectedDate && !availableTimes → load times`. CalendarPage.test.js: 30 → 33 tests (+3).

### 2.3 Out of Scope

*   Promo banner copy/content changes.
*   Version A (`CalendarPage` original flow) changes.
*   Design system variable creation for `#EFEFF1` / `#3A2D4A`.

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 DOTCOMPB-8206 — Calendar Tile Availability States

**Created:** 2026-05-22 | **Last updated:** 2026-05-28
**Status:** ✅ Complete — PR #20908 open, all review comments addressed + post-review bug fixed + 7943 reconciled

#### Component Tree

```
DatePicker.vue              ← weekly swiper — all 5 tile states + arrow nav + fade-in (modified)
  └── MonthlyCalendar.vue   ← monthly grid — mirrored tile states (modified)
CalendarPage.vue            ← orchestrator — AC5 auto-select + 8480 else-if fix (modified)
timer.svg                   ← new hourglass SVG asset
DatePicker.test.js          ← new test file (53 tests; was 42 before 7943 reconcile)
MonthlyCalendar.test.js     ← updated (+16 tests, 30 total)
CalendarPage.test.js        ← updated (+7 tests total: +4 for AC5, +3 for 8480 — 33 total)
```

---

#### DatePicker.vue

**Path:** `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/DatePicker.vue`
**Test:** `DatePicker.test.js` — 53 tests
**Role:** Weekly swiper carousel hosting all 5 date tile states + arrow navigation (7943) + fade-in.
**Parent:** `CalendarPage.vue` — `weeklyView`, `canGoPrev`, `headerLabel` props; emits `date-selected`, `toggle-view`, `prev-nav`, `next-nav`.
**Store:** `hairColorBarBooking` — `selectedDate`, `selectedService`, `selectedAddon`, `availableDates`, `monthlyAvailableDates`, `location`, `isSelectedFeatureService`, `isVariablePricingEnabledForCustomer`, `customerHasHCBMemberships`, `cartContainsLimitlessProPlus`.

**Key methods (8206):**
- `dateCardAriaLabel(date)` — status-qualified aria-label
- `dateCardClasses(date)` — 4-class system + `date-card-has-waitlist-badge`
- `onDateHoverEnter(event, date)` / `onDateHoverMove(event)` / `onDateHoverLeave()` — JS tooltip tracking

**Key methods (7943 reconcile):**
- `onArrowClick()` — tracks "Booking flow - calendar arrow clicked" event
- `onSwiperInit(swiper)` — binds arrow refs + calls `scrollSelectedIntoView()`
- `scrollSelectedIntoView()` — slides Swiper to selected date index via `$nextTick`

**Key data:** `tooltipVisible`, `tooltipText`, `tooltipX`, `tooltipY`, `hasLoadedOnce` (false → true when `availableDates` first populates; gates `v-if` fade-in on `.date-selection`)

**ARIA attributes on SwiperSlide:**
- `:tabindex="!date.available && !date.waitlistEligible ? '-1' : '0'"`
- `:aria-label="dateCardAriaLabel(date)"`
- `:aria-selected="date.calDate === selectedCalDate ? 'true' : 'false'"`
- `:aria-disabled="!date.available && !date.waitlistEligible ? 'true' : undefined"`
- `@keydown.enter.prevent` + `@keydown.space.prevent`

**SR live region (7943 + mrminionbot suggestion):**
```pug
.sr-only(aria-live="polite" aria-atomic="true")
  span(v-if="hasLoadedOnce") Calendar dates loaded. Use the arrows to navigate dates.
```

---

#### MonthlyCalendar.vue

**Path:** `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/MonthlyCalendar.vue`
**Test:** `MonthlyCalendar.test.js` — 30 tests (16 new)
**Role:** 7-column CSS grid — full month with 4 cell states + hover/focus tooltip.

**Key methods:** `cellAriaLabel(cell)`, `cellClasses(cell)`, `numberClasses(cell)`, `cellTooltip(cell)`

**`datesMap` computed:** transforms raw API dates; past dates get `{ ...date, available: false, waitlistEligible: false }` (both flags — see Section 1.4).

---

#### CalendarPage.vue

**Path:** `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/CalendarPage.vue`
**Test:** `CalendarPage.test.js` — 33 tests (+7 total: +4 AC5, +3 DOTCOMPB-8480)

**AC5 change:** `serverPrefetch` and `mounted().then()` auto-select uses `d.available || d.waitlistEligible`.

**DOTCOMPB-8480 change** (2026-05-28): `mounted() else-if` branch (taken when `availableDates` already populated):
```javascript
} else if (!this.weeklyWindowStart) {
  this.weeklyWindowStart = dayjs().startOf('day');
  if (!this.selectedDate) {
    const firstAvailable = this.availableDates.find(d => d.available || d.waitlistEligible);
    if (firstAvailable) {
      this.setSelectedDate(firstAvailable);
      this.loadAvailableTimes(firstAvailable);
    }
  } else if (!this.availableTimes?.length) {
    this.loadAvailableTimes(this.selectedDate);
  }
}
```

**`availableDates` watcher** — loads times only (when `selectedDate` set + `availableTimes` empty). Does NOT auto-select — intentional; watcher fires during week/month navigation.

---

#### Test Coverage

| File | Tests | Key Coverage |
|---|---|---|
| `DatePicker.test.js` | 53 | Tile states (4), tooltip, badge, aria, click guard, promo guard; arrow rendering, tracking, aria-labels, Enter/Space, show-calendar, fade-in, toggleCalendarText |
| `MonthlyCalendar.test.js` | 30 | Cell states (4), numberClasses fix, cellTooltip, data-tooltip, waitlist icon, aria-disabled, past-date regression |
| `CalendarPage.test.js` | 33 | AC5 waitlist-first, no-override, all-unavailable no-op; 8480: pre-loaded+null→auto-select, pre-loaded+empty-times→load-times, SSR-complete→no-op |

**Total CalendarPage suite: 142 tests, all passing.**

---

#### Code Review Findings Applied

| Finding | Severity | Action |
|---|---|---|
| `font-size 11px` in Stylus on `.date-tooltip` | CRITICAL | Fixed → `.xs-f-xxsmall.max-at-tweak` in template |
| `role="tooltip"` + `aria-hidden` contradiction | HIGH | Fixed → removed `role="tooltip"` |
| Missing `tabindex` + `@keydown` on `role="button"` tiles | HIGH (PR review) | Fixed |
| `aria-label` not encoding availability status | HIGH (PR review) | Fixed → `dateCardAriaLabel()` / `cellAriaLabel()` |
| `datesMap` only nulled `available`, not `waitlistEligible` | HIGH (post-review) | Fixed + regression test |
| Missing live-region for `hasLoadedOnce` fade-in | MEDIUM (mrminionbot) | Fixed → `.sr-only` with `aria-live` |
| Missing `:focus-visible` rules | MEDIUM (PR review) | Fixed |
| `aria-pressed` vs `aria-selected` semantics | MEDIUM (PR review) | Fixed |
| `.date-card-global-tooltip` > 12 chars | MEDIUM | Fixed → `.date-tooltip` |
| `.cell-waitlist-icon` redundant prefix | MEDIUM | Fixed → `.waitlist-icon` |
| WHAT comments | MEDIUM | Fixed |
| Skipped: `#EFEFF1`/`#3A2D4A` → design system vars | MEDIUM | Deferred |
| Skipped: `px` → `rem` for spacing | MEDIUM | Deferred |

### 3.2 DOTCOMPB-8480 — CalendarPage Blank Calendar on Second Mount

**Created:** 2026-05-28 | **Last updated:** 2026-05-28
**Status:** ✅ Fixed, ⏳ pending commit to `DOTCOMPB-8206` branch

#### Root Cause

`setSelectedService` Vuex action (called when user re-selects service on services page — V=C flow):
```javascript
commit('setSelectedDate', null);  // clears selectedDate
commit('setSelectedTime', null);
// does NOT clear availableDates
```

On second SPA mount of `CalendarPage` (Back → forward navigation):
- `availableDates.length > 0` (persisted from first visit)
- `selectedDate = null` (cleared by service re-selection)
- `mounted()` enters `else if (!weeklyWindowStart)` → only set window start → return
- Calendar mounts with no date selected, no time slots

#### Fix

Added auto-select + time-reload logic to `mounted() else-if` branch (mirrors `.then()` block).
See Section 3.1 for code.

#### Auto-Select Code Path Map

| Path | When | Auto-selects | Loads times |
|---|---|---|---|
| `serverPrefetch` | SSR first load | ✅ | ✅ |
| `mounted().then()` | Mount with empty dates | ✅ | ✅ |
| `mounted() else-if` | Mount with pre-loaded dates | ✅ (new fix) | ✅ (new fix) |
| `availableDates` watcher | Reactive date change | ❌ intentional | ✅ |
| `serviceIdToBook` watcher | Service change on calendar | ❌ acceptable | ❌ |

Full plan + state matrix + QA steps: `.tasks/DOTCOMPB-8480/plan.md`

#### Suggested Commit Message
```
fix(CalendarPage): auto-select date on second mount when dates pre-loaded (DOTCOMPB-8480)

setSelectedService action clears selectedDate without clearing availableDates.
The mounted() else-if branch (taken when dates already loaded) was missing
auto-select logic, leaving the calendar blank on second SPA mount.

Fix: mirror .then() block — !selectedDate → auto-select first available/waitlist;
selectedDate && !availableTimes → reload times. Three regression tests added.
```

---

## SECTION 4: FILE INDEX

### Created (New)

| File | Association |
|---|---|
| `website/src/assets/svg-icons/timer.svg` | DOTCOMPB-8206 |
| `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/DatePicker.test.js` | DOTCOMPB-8206 |

### Modified

| File | Association |
|---|---|
| `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/DatePicker.vue` | DOTCOMPB-8206 + DOTCOMPB-7943 reconcile |
| `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/MonthlyCalendar.vue` | DOTCOMPB-8206 |
| `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/CalendarPage.vue` | DOTCOMPB-8206 + DOTCOMPB-8480 |
| `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/MonthlyCalendar.test.js` | DOTCOMPB-8206 |
| `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/CalendarPage.test.js` | DOTCOMPB-8206 + DOTCOMPB-8480 |
| `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/DatePicker.test.js` | DOTCOMPB-7943 reconcile (+11 tests) |

### Reference Only

| File | Notes |
|---|---|
| `website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/TimePicker.vue` | AC3 pre-satisfied; `noTimesAvailable` branch drives waitlist flow |
| `website/src/vuescripts/store/modules/hairColorBarBooking.js` | `fetchAvailableTimes` filter (unchanged); `setSelectedService` action clears `selectedDate` (8480 root cause) |
| `website/src/vuescripts/components/HairColorBarBookingV2/AddonsPage/AddonsPage.vue` | Uses `setSelectedService` mutation (not action) on CONTINUE — does NOT clear `selectedDate` |
| `.tasks/DOTCOMPB-8480/plan.md` | Full 5-path auto-select map, state matrix, QA steps |
| `~/.brain.d/roam-nodes/madison_reed/2026-05-21-181625-dotcompb_8206.org` | UUID: `d51a9785-b497-49ee-85f8-3c961fe8d9eb` — DONE [5/5] |

---

## SECTION 5: LAST INTERACTION

> **PR #20908 open. DOTCOMPB-8480 fix implemented and tested. Pending commit + push + PR description update.**

### What was done last

*   Loaded session and PR status — found jsalazar-MR had reconciled DOTCOMPB-7943 (arrow nav + fade-in) into the branch across 4 commits; mrminionbot confirmed 0 ADA blockers after reconcile; live-region suggestion addressed in `60a32122c00`.
*   Read DOTCOMPB-8480 ticket; traced root cause to `setSelectedService` action clearing `selectedDate` without clearing `availableDates`, leaving `mounted() else-if` branch with no auto-select path.
*   Implemented fix in `CalendarPage.vue` (`else if` branch now mirrors `.then()` auto-select + time-reload guards).
*   Added 3 regression tests to `CalendarPage.test.js` (33 total); exposed `fetchAvailableTimesSpy` at file scope in `createMockStore`. All 142 tests pass.
*   Created comprehensive plan at `.tasks/DOTCOMPB-8480/plan.md` (5-path map, state matrix, QA steps, invariants).

### Pending / Not yet started

*   [ ] **Commit 8480 fix** — `CalendarPage.vue` + `CalendarPage.test.js` (2 files modified, uncommitted)
*   [ ] **Push to `DOTCOMPB-8206` branch** — update PR #20908
*   [ ] **Update PR description** — add DOTCOMPB-8480 to "What does this PR do?" and Changes block
*   [ ] **Human reviewer approval** — PR is REVIEW_REQUIRED (pilko-linter approved; jsalazar-MR commented but did not formally approve)
*   [ ] **Design system variables** — `#EFEFF1` (Secondary Cloud) and `#3A2D4A` (Primary Nightshade) hardcoded in 7 places; request Stylus variable additions before next ticket using these colors
*   [ ] **AC5 edge case** — all dates in 2-week window fully unavailable → silent no-op; PO confirmation pending on whether to auto-advance to next week

### Where to resume

**If committing the 8480 fix:** `git add website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/CalendarPage.vue website/src/vuescripts/components/HairColorBarBookingV2/CalendarPage/CalendarPage.test.js` → commit with message from Section 3.2 → push → update PR description.

**If PR receives more comments:** `gh pr view 20908 --comments` to fetch new reviews.

**If checking CI status:** `gh pr checks 20908`.

**If a new task:** check Section 2.3 pending or JIRA board for next ticket.

---

## SECTION 6: ACTIVITY LOG

| Datetime         | Duration | Type           | Reference      | Description |
|------------------+----------+----------------+----------------+-------------|
| 2026-05-28 16:14 | —        | session-reset  | this           | Post-8480 reset — 7943 reconciled, 8480 fixed + 3 tests, plan doc, 142 tests passing |
| 2026-05-28 15:30 | 0.5h     | bug-fix        | DOTCOMPB-8480  | CalendarPage else-if branch fix + 3 regression tests (33 total in CalendarPage.test.js) |
| 2026-05-28 15:00 | 0.5h     | research       | DOTCOMPB-8480  | Traced setSelectedService root cause; mapped all 5 auto-select code paths |
| 2026-05-28 14:30 | 0.5h     | pr-feedback    | PR #20908      | Reviewed 7943 reconciliation commits by jsalazar-MR; confirmed mrminionbot 0 blockers |
| 2026-05-27 11:00 | —        | session-reset  | this           | Second reset — no new implementation; pitch writing out of scope for this session |
| 2026-05-27 10:30 | —        | session-reset  | this           | Post-bugfix reset — datesMap waitlistEligible fix, 125 tests, PR #20908 pending merge |
| 2026-05-27 10:15 | 0.5h     | bug-fix        | DOTCOMPB-8206  | Fixed datesMap: waitlistEligible: false for past dates; added regression test (30 tests) |
| 2026-05-22 18:00 | —        | session-reset  | this           | Final reset — PR review comments applied, bug prediction rebutted, 124 tests |
| 2026-05-22 17:30 | 0.5h     | pr-feedback    | PR #20908      | Rebutted AI bug prediction — noTimesAvailable flow is intentional for waitlist auto-select |
| 2026-05-22 17:00 | 1h       | pr-feedback    | PR #20908      | Applied 7 accessibility comments: tabindex, aria-label status, focus-visible, aria-selected |
| 2026-05-22 16:30 | —        | session-reset  | this           | Post-implementation reset — code review applied, PR body authored |
| 2026-05-22 16:15 | —        | bug-fix        | DOTCOMPB-8206  | Removed unused `wrapper` var in CalendarPage.test.js — pre-commit ESLint error |
| 2026-05-22 15:45 | 0.5h     | documentation  | DOTCOMPB-8206  | Rewrote roam node PR DESCRIPTION as pure GitHub markdown |
| 2026-05-22 15:15 | 0.5h     | refinement     | DOTCOMPB-8206  | Code review applied — font utility class, role contradiction, class renames, comments |
| 2026-05-22 13:45 | 1.5h     | implementation | DOTCOMPB-8206  | Visual refinements — padding, badge position, gap responsive, cell height, icon chip |
| 2026-05-22 10:45 | 3h       | implementation | DOTCOMPB-8206  | All 5 tile states in DatePicker + MonthlyCalendar + CalendarPage AC5 + 101 tests |
| 2026-05-22 10:30 | —        | session-reset  | this           | Planning session compacted — 5 Figma specs, 9-phase plan, parent/sibling audit |
| 2026-05-22 09:00 | 1h       | research       | DOTCOMPB-8206  | Parent/sibling audit — AddonCard patterns, isDesktop vs hover media query, a11y gaps |
| 2026-05-22 07:30 | 2.5h     | refinement     | DOTCOMPB-8206  | Component deep-audit + all 5 Figma state specs + 9-phase implementation plan |
| 2026-05-21 18:16 | —        | other          | this           | Activity Log bootstrap — session initialized, roam node created |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
