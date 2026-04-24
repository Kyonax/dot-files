<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the DOTCOMPB-7942 (Google SSO Booking Flow) session. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read the sections in order on first load — after that, reference them by number as needed. The data is organized into 5 sections:

| Section                        | Purpose                                                                                                         | When to reference                                                                                  |
|--------------------------------|-----------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------|
| **1. Global Guidelines**       | Coding rules, patterns, and conventions that apply to ALL work in this session.                                 | Before writing or reviewing any code. These are mandatory constraints.                             |
| **2. Session Overview**        | High-level context: what this project is, which tickets are in scope, session-wide decisions, and pending work. | When starting a new task — understand scope and status first.                                      |
| **3. Feature Implementations** | Per-ticket detail: what was built, where files live, component trees, key decisions, tests, and PR status.      | When resuming work on a specific ticket, or when a new task relates to an existing implementation. |
| **4. File Index**              | Quick-reference table of all file paths (components, tests, roam nodes, directives).                            | When you need to read, edit, or reference a specific file without searching.                       |
| **5. Last Interaction**        | Short-term memory: what was done last, what's pending, where to resume.                                         | At the very start of a new conversation — this is your entry point for continuing work.            |

**Operational Rule:** Throughout the conversation, the user will give new tasks. **Always look for the last request, identified by a markdown title starting with `###`.** Any `###` title means it is the newest request. Based on that section, load relevant skills and apply the rules from Section 1.

**Key principle:** Data may appear in multiple sections with different framing. Section 1 frames it as a rule to follow. Section 2 frames it as context to understand. Section 3 frames it as an implementation to reference. This is intentional — each section answers a different question about the same knowledge.

**Architecture memory:** This session references `site-revolution-architecture.md` for reusable patterns and decisions from the Site Revolution redesign. References use the syntax `[session: site-revolution-architecture > section > entry-id]`. Resolve references on-demand when the current task needs the full pattern details.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** The loaded skills for this project — `mr-dotcom-dev` (Vue/Vuex/Pug/Stylus patterns), `mr-style` (design system classes and variables), `code-review` (quality analysis), and `mr-roam-node` (ticket documentation) — cover general framework conventions. This section stores session-scoped patterns not yet captured in those skills, plus references to validated architecture decisions from the Site Revolution redesign.

### 1.1 Framework & API

*   **Vue 3** — Options API only (`export default { ... }`). No Composition API, no `<script setup>`.
*   **Templating:** Pug (`<template lang="pug">`).
*   **Styling:** Scoped Stylus (`<style lang="stylus" scoped>`).
*   **JS Syntax:** Always use curly braces for `if` statements, even single-line returns.
*   **Optional Chaining (`?.`):** Always. `getObjProperty` is deprecated and forbidden.

### 1.2 Styling Architecture

*   **Utility-first:** Aggressively apply responsive utility classes. Stylus only for structural layouts, pseudo-elements, transitions, complex layouts, `:deep()` overrides, and custom media queries.
*   **`.max-at-tweak` is MANDATORY on every font utility class.**
*   **Design System Variables:** Never hardcode hex colors — use `brand-color-*`, `cta-color-*`, `text-color-*`, `ui-color-*`.
*   **Font sizes in templates only, never Stylus.** All `font-size`, `font-family`, and `color` for text must use utility classes.
*   **Responsive padding/margin via utility classes, never Stylus.** Use breakpoint prefixes (e.g., `.xs-pt-50m.lg-pt-100m`).
*   **CSS property ordering:** Alphabetize within `<style>` blocks.

### 1.3 Component Design Patterns

*   **Canonical Field Order:** `name` → `components` → `emits` → `props` → `data` → `computed` → `watch` → lifecycle hooks → `methods`.
*   **Module-Level Constants:** Static, non-reactive data → `const` outside `export default`. No magic numbers.
*   **Constants in templates — camelCase computed wrapper:** `const UPPER_SNAKE = 3` → `skeletonCount() { return UPPER_SNAKE; }`. Never use `UPPER_SNAKE_CASE` in templates.
*   **No Inline Event Logic:** `@click` must call a single method.
*   **Computed alphabetization:** Alphabetize computed properties within `<script>`.
*   **Vuex helpers at top of sections:** `...mapState()`/`...mapGetters()` first in `computed`. `...mapActions()`/`...mapMutations()` first in `methods`.

### 1.4 Accessibility (A11y)

*   **Self-contained landmarks** — [session: site-revolution-architecture > design-patterns > dp-001]
*   **Semantic HTML:** `<main>`, `<section>`, `<dl>` over generic `<div>`.
*   **ARIA:** `role="region"`, `aria-label`, `aria-labelledby`, `aria-controls`, `aria-expanded`.
*   **`aria-labelledby` must reference heading IDs, never root element IDs.**
*   **Keyboard nav:** Non-native interactive elements → `tabindex="0"` + `@keydown.enter.prevent` + `@keydown.space.prevent`.
*   **Native buttons:** Never use raw `<button>`. Use `MrBtn` for button interactions. For navigation: `<a>` with `:href` + `@click.prevent`.
*   **Live region outside v-if** — [session: site-revolution-architecture > design-patterns > dp-005]
*   **Focus return on collapse** — [session: site-revolution-architecture > design-patterns > dp-006]

### 1.5 Testing (Vitest & Vue Test Utils)

*   **Run:** `cd website && npm run test:vue {component_name}`.
*   **No snapshot tests.** Forbidden.
*   **`shallowMount` by default.** Children are stubbed.
*   **Async:** `await wrapper.vm.$nextTick()` after state changes.
*   **`import { vi } from 'vitest'` explicitly in new test files** — PilkoLint does not pick up the `vuescripts/.eslintrc.js` test-override.
*   **Vuex 4 `commit` spy:** Use `commitSpy.mock.calls.find(call => call[0] === 'mutation/name')` then `toEqual` on `call[1]`.

### 1.6 Tracking

*   **`trackMREvent(eventName, properties)`** — Fire-and-forget. Use when the user stays on the current page.
*   **`trackMREventAndRedirect(eventName, url, properties)`** — Track then navigate with 300ms delay for hard redirects.
*   **Do NOT pass `isFrontEndEvent: true`** — `segmentTracking.js` auto-adds it for all frontend events.
*   **Anti-pattern:** Never `trackMREvent()` + `goToPath()` sequentially — use `trackMREventAndRedirect` for hard redirects.

### 1.7 Andris Review Patterns (Key Subset)

*   **andris-guideline-2 — No info-only comments.** Only `// TODO:`, `// eslint-disable`, and genuinely non-obvious logic.
*   **andris-guideline-3 — One blank line between logical blocks.**
*   **andris-guideline-5 — No formatting-only changes in unrelated files.**
*   **andris-guideline-6 — Single H1 per page.** Section components use `<h2>` or lower.
*   **andris-guideline-7 — Omit redundant `div` in Pug.** `.my-class` auto-creates a `<div>`.
*   **andris-guideline-8 — Use existing MrBtn variants** before adding custom button CSS.
*   **andris-guideline-12 — Extract, don't bloat.** Standalone features → own component.
*   **andris-guideline-14 — Styles inside the SFC.** No external `.styl`/`.css` files.

### 1.8 Architecture Memory References (Site Revolution)

> These are validated architectural patterns from the Site Revolution redesign. Load the full entry from `site-revolution-architecture.md` when the current task needs the detailed pattern.

*   **Cookie-based cross-app state** — [session: site-revolution-architecture > architecture-decisions > ad-002] — Relevant for booking state preservation across SSO redirect.
*   **Experiment-gated async component** — [session: site-revolution-architecture > design-patterns > dp-003] — If Google SSO is behind an experiment flag.
*   **Self-sufficient component spacing** — [session: site-revolution-architecture > design-patterns > dp-004]
*   **Experiment flag lifecycle (SSR → Client)** — [session: site-revolution-architecture > shared-state > sf-002]
*   **No SSR for experiment B components** — [session: site-revolution-architecture > constraints > cl-003]
*   **Sticky header ResizeObserver** — [session: site-revolution-architecture > architecture-decisions > ad-008] — If booking flow has sticky elements below nav.

---

## SECTION 2: SESSION OVERVIEW

> This section provides the overall context, purpose, and scope of the Google SSO Booking Flow session.

### 2.1 Purpose

This session covers the implementation of **Google Sign-On (SSO) into the Madison Reed booking flow** — specifically the "Your Details" step. The feature adds an Express Sign In section with Google (and Facebook) OAuth buttons that pre-fill contact information, handle account creation/linking, and preserve booking state across the OAuth redirect flow.

**Parent Epic:** DOTCOMPB-7229 (Site Revolution - Booking Flow)
**Sprint:** D463 Ghost Busters 04/15 (active, ends 2026-04-29)

### 2.2 Scope

| Ticket          | Type  | Summary                                            | Status                |
|-----------------|-------|----------------------------------------------------|-----------------------|
| `DOTCOMPB-7942` | Story | Add Google sign-on into the new booking flow       | **IN CODE REVIEW**    |
| `DOTCOMPB-7990` | Test  | Google SSO Booking Flow — Functional Test Cases    | **TODO** (linked)     |

### 2.3 Key Architectural Decisions (Session-Wide)

1. **(2026-04-23)** **Session file created with architecture memory references** — This session references `site-revolution-architecture.md` for validated patterns instead of duplicating them inline. Patterns from the Site Revolution redesign (self-contained landmarks, experiment gating, cookie-based state, etc.) are loaded on-demand via reference syntax.

### 2.4 Acceptance Criteria Summary (DOTCOMPB-7942)

**Express Sign In Section — Display:**
*   "Express Sign In" section at top of "Your Details" step, above Contact Info fields
*   Two SSO buttons: Google + Facebook, side by side per Figma
*   Correct provider logo/branding per Google/Meta brand guidelines
*   "OR" divider with horizontal rules between Express Sign In and Contact Info
*   Section hidden for already-authenticated users
*   Existing "Sign In" link (email/password) remains present

**Google OAuth Flow:**
*   Standard Google OAuth 2.0 consent flow
*   Desktop: popup. Mobile: system browser sheet or redirect
*   On success: receive email, first name, last name → pre-fill Contact Info fields
*   On failure/cancellation: return to "Your Details" with no changes, inline error if failure (not cancellation)

**Contact Field Pre-Fill:**
*   First Name, Last Name, Email pre-filled from Google provider data
*   Mobile field must still be completed manually
*   All pre-filled fields editable — no locked/special styling
*   Edited values take precedence over provider values

**Account Handling:**

| User State | Action | Outcome |
|---|---|---|
| New user (no MR account) | Google SSO | Account created from provider data, linked to Google |
| Existing MR, provider not linked | Google SSO (email match) | Provider linked to existing account, MR data takes precedence |
| Existing MR, provider already linked | Google SSO | Authenticated against existing account |
| Any user | Manual entry (no SSO) | Standard flow unchanged |
| Already signed in | Either | SSO section hidden, fields pre-filled from account |

**Booking State Preservation:**
*   Selected service, date/time, location, stylist, add-ons, promo code must survive SSO redirect/popup
*   State persisted in session storage or equivalent before SSO initiates, restored on return

**Communication Preferences:**
*   Checkboxes remain present regardless of SSO or manual entry
*   Existing account: reflect saved preferences. New account: default state. Saved on booking confirmation.

**Security:**
*   No Google password stored. Server-side token handling. No persistent token storage beyond session.

**Error States:**
*   Provider unavailable: inline error, manual form accessible
*   No email returned: error directing to manual entry
*   Network failure: booking state restored, error shown, retry available
*   User-friendly messages only — no technical details exposed

**Event Tracking:**

| Event Name | Trigger | Properties |
|---|---|---|
| `Booking flow – Google Login clicked` | Click Google Login button | `eventName`, ~~`isFrontEndEvent`~~ (auto-added — see §1.6) |

### 2.5 Pending Work

*   [ ] Investigate booking flow codebase — identify "Your Details" step component, existing auth flow, Vuex stores involved
*   [ ] Design component architecture for Express Sign In section
*   [ ] Implement Google OAuth integration (backend + frontend)
*   [ ] Implement account creation/linking logic
*   [ ] Implement booking state preservation across SSO redirect
*   [ ] Implement contact field pre-fill
*   [ ] Implement error states and inline error display
*   [ ] Implement event tracking
*   [ ] Write unit tests
*   [ ] Create PR

---

## SECTION 3: FEATURE / TICKET IMPLEMENTATIONS

> Each subsection documents a specific ticket's implementation: what was built, where it lives, what decisions were made, and how to verify it.

---

### 3.1 DOTCOMPB-7942: Google SSO in Booking Flow

**Created:** 2026-04-23 | **Last updated:** 2026-04-23
**Status:** **IN CODE REVIEW** (JIRA). Implementation investigation pending.
**Branch:** `DOTCOMPB-7942`
**Roam Node:** `~/.brain.d/roam-nodes/madison_reed/2026-04-23-150000-dotcompb_7942.org`

#### Investigation Needed

Before implementation, the following must be explored:

1. **Booking flow "Your Details" step** — Which component renders it? What's the component tree? Where does Contact Info live?
2. **Existing auth/sign-in flow** — How does the current "Sign In" link work in the booking flow? Which controllers/webservices handle authentication?
3. **Google OAuth infrastructure** — Does MR already have Google OAuth configured (Passport strategy, client ID, redirect URIs)? Or is this net-new?
4. **Facebook SSO** — Is Facebook OAuth already implemented elsewhere in the codebase? Can it be reused?
5. **Booking state Vuex modules** — Which store modules hold booking state (service, date, time, location, add-ons, promo)? How is state currently preserved across page navigations?
6. **Session storage pattern** — Are there existing patterns for persisting Vuex state to session storage and restoring it?

#### Key Decisions

*(To be populated as decisions are made during implementation)*

| Decision | Date | Rationale |
|---|---|---|

---

## SECTION 4: FILE INDEX

> Quick reference for all files relevant to this session.

### Components (To Be Identified)

| File | Association |
|---|---|
| *(investigation needed)* | DOTCOMPB-7942 |

### Roam Nodes

| File | Association |
|---|---|
| `~/.brain.d/roam-nodes/madison_reed/2026-04-23-150000-dotcompb_7942.org` | DOTCOMPB-7942 |

### Session Files

| File | Association |
|---|---|
| `sessions/dotcompb-7942-google-sso-booking.md` | This session |
| `sessions/site-revolution-architecture.md` | Architecture memory (referenced) |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last

*   Created this session file (2026-04-23)
*   Parsed JIRA ticket DOTCOMPB-7942 acceptance criteria (39 ACs across 10 scenarios)
*   Identified relevant architecture memory references from Site Revolution
*   Created roam node `2026-04-23-150000-dotcompb_7942.org` with full TICKET CONTEXT (all 39 ACs in GIVEN/WHEN/THEN + event tracking table)
*   Updated index file — added to BACKLOG and IN CODE REVIEW lane

### Pending / Not yet started

*   **Codebase investigation** — Identify booking flow components, auth infrastructure, Vuex stores, existing OAuth strategies
*   **Component architecture design** — Express Sign In section structure
*   **Full implementation** — OAuth flow, account handling, state preservation, pre-fill, errors, tracking, tests

### Where to resume

If the user asks to **start investigating**: Begin with the booking flow "Your Details" step — find the component, read its template and store dependencies. Then check for existing Google/Facebook OAuth Passport strategies in `mr_modules/`.
If the user asks to **design the architecture**: Use the investigation results to propose a component tree and data flow before writing code.
If the user asks for a **new task**: Check Section 2.5 (Pending Work).

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
