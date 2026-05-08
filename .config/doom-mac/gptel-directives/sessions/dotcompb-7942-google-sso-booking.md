<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the DOTCOMPB-7942 (Google SSO Booking Flow) session. It is loaded at the start of every conversation to give the AI full context without re-discovering anything.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, patterns, conventions for ALL work. | Before any code task. |
| **2. Session Overview** | Scope, decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-ticket detail: files, trees, decisions, tests. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference file path table. | When reading, editing, or locating files. |
| **5. Last Interaction** | Short-term memory: last work, pending, resume points. | At conversation start. |

**Operational Rule:** Always look for the last request identified by `###` title. Load relevant skills and apply Section 1 rules.

**Architecture memory:** References `site-revolution-architecture.md` via `[session: site-revolution-architecture > section > entry-id]`.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `mr-dotcom-dev`, `mr-style`, `code-review`, `mr-roam-node`.

### 1.1 Framework & API

*   **Vue 3 Composition API** (`<script setup>`) for V2 booking flow components.
*   **Composables over mixins.** Never create new mixins.
*   **Pug templates.** Kebab-case for all component tags.
*   **Scoped Stylus.** MrBtn and MrIcon are globally registered — no import needed.
*   **Optional chaining (`?.`) always.** `getObjProperty` forbidden.

### 1.2 Utility-First Styling

*   **Utility classes first, Stylus only for what utilities can't express.**
*   **`.max-at-tweak` mandatory** on every responsive font class.
*   **Design system variables only.** `darken()`/`lighten()` when needed.
*   **Alphabetize CSS** within `<style>` blocks.
*   **Stylus only for:** borders with color variables, flex-direction overrides on MrBtn, hover states, `:deep()` overrides, absolute positioning math, letter-spacing/line-height, icon sizing, `:focus-visible` outlines, `pointer-events none`.
*   **Never assume visual properties.** When a screenshot/Figma is provided, implement EXACTLY what's visible.

### 1.3 Component Design Patterns

*   **Module-level constants** for static data.
*   **No inline event logic.** `@click` calls a single method.
*   **Extract repeated logic** into helper functions.
*   **No info-only comments.** JSDoc with `@param`/`@returns` on public composable APIs. **JSDoc block description is MANDATORY** — `@param`/`@returns` tags alone fail PilkoLint. Example:
    ```javascript
    // BAD — fails PilkoLint "Missing JSDoc block description":
    /** @param {object} data - callback data. */
    
    // GOOD — description before tags:
    /**
     * Processes the GSI OAuth callback.
     * @param {object} data - callback data.
     */
    ```
*   **JSDoc type casing:** Use `Function` (capital F), not `function`. PilkoLint enforces this.
*   **Pug attribute order:** Static attributes → dynamic bindings → `aria-*` → event handlers (`@click` last).

### 1.4 Accessibility (A11y)

*   **`aria-labelledby`** references heading IDs, never root IDs.
*   **`:focus-visible`** on all custom buttons with `.btn-reset`.
*   **`role="alert"` persistent in DOM.** No `v-if` — use empty content. Add `aria-atomic="true"`. (See 1.8 CR-LESSON-2 for rationale.)
*   **Loading states:** `role="status"` + `aria-label`.
*   **Live region interaction flag:** `signedInDuringSession` ref pattern — only announce state changes, not initial mount state. (See 1.8 CR-LESSON-9.)
*   **Heading hierarchy:** Page `h1` → parallel sections both `h2`. Never skip levels. Sibling sections at the same depth = same heading level.
*   **`v-if` scoping:** Hide the specific interactive element, not the container. Title stays visible; only the button gets `v-if`.
*   **Focus management on DOM swap:** When `v-if`/`v-else` removes the element the user just interacted with, use `nextTick(() => targetElement?.focus())` to move focus to the next logical target. (See 1.8 CR-LESSON-8.)
*   **Decorative icons inside labeled buttons:** Add `aria-hidden="true"` to `mr-icon` when the parent button already has `aria-label`.

### 1.5 MrBtn + MrIcon Patterns

*   **Icon-only button:** `#prePendContent` slot. `:deep(.btn-content) display none !important`.
*   **Multi-color SVGs:** `mr-icon` with `:original="true"`.
*   **Loading hides icon:** `&.disabled :deep(.prepend-content) opacity 0`. MrBtn adds `disabled` class when `:loading="true"`. **Critical: `:deep()` must be nested under the correct parent class** — wrong nesting = wrong compiled selector.
*   **Icon `pointer-events none`:** Clicks pass through to MrBtn for ripple animation.

### 1.6 Google Sign-In (GSI)

*   **GSI `renderButton()` max 400px.** Use custom MrBtn instead.
*   **`use_fedcm_for_prompt: false`** in `gai.initialize()`. FedCM cooldown fix: `chrome://settings/content/federatedIdentityApi` → remove localhost → hard refresh.
*   **Google "G" SVG:** `google-g.svg` in `svg-icons/`. Colors: red → yellow → green → blue.
*   **Skeleton:** `.ssc-animation` (not `.ssc-square`). `darken(ui-color-4, 5%)`. Height 53px.

### 1.7 Tracking & Testing

*   **`trackMREvent` — fire-and-forget.** Do NOT pass `isFrontEndEvent: true`.
*   **Avoid double tracking:** When `openSignInModal` accepts an `event` param, it internally calls `trackMREvent`. If you also call `trackMREvent` explicitly, the event fires twice. Pick one — prefer the explicit call (visible, under your control) and omit `event` from `openSignInModal`.
*   **`shallowMount` by default.** Mock composables at module level.
*   **Lint before commit:** `bash .tasks/lint-changed.sh` from repo root.

### 1.8 Code Review Lessons (from PR #20652 review)

> These lessons emerged from the PR review process. Each documents a real finding, the decision made, and the rationale — useful for refining the `code-review` skill.

**CR-LESSON-1: Separate error domains in async chains**
- **Pattern:** When an async function has two sequential awaits (e.g., `googleLogin()` then `refreshCustomerCdata()`), wrap the second in its own try/catch so errors from different domains get different messages.
- **Example:** Google auth succeeds but cdata fetch fails → user sees "Something went wrong with Google sign-in" (misleading). Fix: inner try/catch on cdata, proceed with `onSuccess(undefined)`.
- **Rule:** When two awaits serve different user-facing purposes, catch them separately.

**CR-LESSON-2: `role="alert"` persistent vs `v-if` — design decision, not a bug**
- **Finding:** Claude review flagged empty `<p role="alert">` as a bug. MR Minion didn't flag it.
- **Decision:** Persistent is correct per WCAG 4.1.3 / Advanced ADA Rule 1. `v-if` risks NVDA+Firefox missing mount. Empty elements are skipped by screen readers.
- **Rule for code-review skill:** When two ADA approaches conflict, document both trade-offs and pick one explicitly. Don't auto-flag persistent `role="alert"` as a bug.

**CR-LESSON-3: Global singleton flags (`window.googleInitialized`) are not race conditions when consumers don't overlap**
- **Finding:** Sentry flagged this 3 times as HIGH/MEDIUM. Claude flagged it as a concern.
- **Decision:** Not applicable. The flag is intentional, consumers never co-render, `prompt()` uses the callback from whichever `initialize()` ran first.
- **Rule for code-review skill:** Don't flag shared globals as race conditions without verifying that the consumers can actually co-exist on the same page. Check the routing/component tree before flagging.

**CR-LESSON-4: Function props are valid for async callbacks returning data**
- **Finding:** Claude flagged `onSuccess` Function prop as non-idiomatic (should use `emit`).
- **Decision:** `emit` can't return data from async parent handlers. Function props are the correct pattern for callbacks that receive data (e.g., cdata from auth flow). Consistent with `ConsultationInformationModal.onSuccessCallback`.
- **Rule for code-review skill:** Don't flag Function props as non-idiomatic when the callback needs to pass data from child to parent in an async context.

**CR-LESSON-5: Vuex action concurrent guards should return current state, not `undefined`**
- **Finding:** Sentry flagged `refreshCustomerCdata` returning `undefined` during concurrent call.
- **Decision:** Changed `return;` to `return state.cdata;`. All consumers get usable data.
- **Rule:** Any Vuex action with a "skip if already running" guard should return the current state, not bare `return`.

**CR-LESSON-6: JSDoc block description is mandatory in MR (PilkoLint)**
- **Finding:** PilkoLint CHANGES_REQUESTED for JSDoc with tags but no description.
- **Decision:** Added block descriptions. Also: `function` → `Function` (capital F) for types.
- **Rule:** Always write a description line before `@param`/`@returns`. Never use lowercase `function` in JSDoc types.

**CR-LESSON-7: Heading siblings at the same depth must use the same level**
- **Finding:** MR Minion flagged `h2` (Express Sign In) followed by `h3` (Contact Info) as false parent-child.
- **Decision:** Changed Contact Info to `h2`. Both are parallel sections under `h1`.
- **Rule:** When two sections are siblings (not nested), they must use the same heading level, even if visually one is styled smaller.

**CR-LESSON-8: Focus management on `v-if`/`v-else` DOM swap**
- **Finding:** MR Minion flagged focus loss after Google sign-in removes the button from DOM.
- **Decision:** `nextTick(() => document.querySelector('[data-mr-ass="profile-firstname"] input')?.focus())` in the success handler.
- **Rule:** Every `v-if`/`v-else` that removes the element the user just interacted with MUST include focus management. Use `nextTick` to wait for the DOM update, then focus the next logical target.

**CR-LESSON-9: Live regions must use interaction flags to prevent spurious mount announcements**
- **Finding:** MR Minion flagged "You are signed in" announcing on initial mount for already-authenticated users.
- **Decision:** `signedInDuringSession` ref starts `false`, flips `true` only in `handleGoogleSuccess`. Live region reads from the flag, not from auth state.
- **Rule:** Live regions that announce state should track whether the state CHANGED during this session, not just what the current state IS. Use a ref flag set only by the action that triggers the change.

**CR-LESSON-10: Decorative icons inside labeled interactive elements need `aria-hidden`**
- **Finding:** MR Minion flagged Google "G" icon announcing "google-g icon" inside a button with `aria-label`.
- **Decision:** Added `aria-hidden="true"` to `mr-icon`.
- **Rule:** Any `mr-icon` inside a button/link that already has `aria-label` or visible text is decorative and must have `aria-hidden="true"`.

**CR-LESSON-11: Guard Vuex callbacks against overwrite by downstream components**
- **Finding:** Sentry flagged `LoginWithPassword.mounted()` overwriting `onSuccessCallback` with `showAppDownloadModal` for first-time users (no `afterLoginModalViewed` cookie). Pre-existing bug, but impact increased with no-reload approach.
- **Decision:** Added `!this.onSuccessCallback` guard in `LoginWithPassword.mounted()` so it only sets the app download callback when no other callback is already registered.
- **Rule:** When a Vuex store holds a callback set by a parent/caller, downstream components that mount later must not overwrite it unconditionally. Always check if a callback is already set before assigning a default.
**CR-LESSON-12: `USER_ALREADY_EXISTS` must be surfaced to the user**
- **Finding:** QA reported Google SSO shows generic "Something went wrong" when the Google email matches an existing MR account with a password. Backend returns `USER_ALREADY_EXISTS` (403) but frontend didn't check for that code.
- **Decision:** Added `err.response.data.code === 'USER_ALREADY_EXISTS'` check in `useGoogleSignIn.js` error handler. Shows: "An account with this email already exists. Please use the Sign In link below to log in with your email and password."
- **Rule:** Always check for specific backend error codes before falling through to generic error messages. Map each known code to a user-friendly message.

**CR-LESSON-13: Duplicate appointment error ("already booked") must be surfaced**
- **Finding:** When a user books a slot they already have, backend returns plain `Error("Customer X already has booked an appointment...")` with HTTP 500, no error code. Frontend shows generic "Something went wrong."
- **Decision:** Added `errorMessage.includes('already has booked an appointment')` check in `InfoPage.vue bookAppointment()`. Shows: "It looks like you already have an appointment booked at this time. Please choose a different time or manage your existing booking."
- **Rule:** When backend returns plain `Error` (no MRError code), check the message string for known patterns as a fallback.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Google Sign-On (SSO) into the booking flow "Your Details" step. Custom Google button, Express Sign In section, Contact Info header, no-reload auth flow.

**Parent Epic:** DOTCOMPB-7229

### 2.2 Scope

| Ticket | Status |
|---|---|
| `DOTCOMPB-7942` | **PR OPEN — review fixes applied** |
| `DOTCOMPB-7990` | TODO (linked test cases) |

### 2.3 Key Decisions

1-12 from previous resets (Facebook removed, experiment B/C, custom MrBtn, Contact Info v-if scoping, checkbox preservation, FedCM, pointer-events, pr-scribe refinements) — all documented in roam node SCOPE DECISIONS.

13. **(2026-04-25)** PR review: 9 findings implemented, 6 skipped with documented answers. See Section 1.8 for lessons.
14. **(2026-04-25)** PilkoLint: JSDoc block descriptions mandatory (not just tags). Created `.tasks/lint-changed.sh` for local pre-commit lint.
15. **(2026-04-25)** `customer.js` modified: `refreshCustomerCdata` concurrent guard returns `state.cdata` instead of `undefined`.
16. **(2026-04-27)** SENTRY-4: `LoginWithPassword.mounted()` callback overwrite guard added.
17. **(2026-04-27)** SENTRY-5/6: Two new Sentry findings — both skipped as pre-existing/intentional (logout redirect, cdata fetch degradation).
18. **(2026-04-28)** Edge case: Duplicate appointment error surfaced to user (was hidden behind "Something went wrong"). Added message check in `InfoPage.vue`.
19. **(2026-04-28)** `USER_ALREADY_EXISTS` error: Added specific frontend error message in `useGoogleSignIn.js` for Google email matching existing MR account with password.
20. **(2026-04-28)** E2E Playwright test suite expanded: 59 tests, full booking flow edge cases with `select-time-retry` pattern, SPA-safe navigation waits, `scrollIntoViewIfNeeded` for all clicks.
21. **(2026-04-27)** E2E test report document created as roam node with screenshot evidence.
22. **(2026-04-27)** JIRA test case content prepared for DOTCOMPB-7942 test issue.

### 2.4 Pending Work

*   [ ] Commit all fixes + push (7 files: useGoogleSignIn.js, SignInOptions.vue, InfoPage.vue, customer.js, LoginWithPassword.vue + existing)
*   [ ] Wait for PilkoLint CI to pass
*   [ ] Get reviewer approval (andris310 requested)
*   [ ] Manual QA: real Google OAuth, VoiceOver, Figma comparison, Segment tracking
*   [ ] Verify `USER_ALREADY_EXISTS` handling with backend team (keep blocking or allow auto-link?)
*   [ ] Run full E2E edge case suite on QA environment

---

## SECTION 3: FEATURE / TICKET IMPLEMENTATIONS

### 3.1 DOTCOMPB-7942

**Created:** 2026-04-23 | **Last updated:** 2026-04-29
**Status:** **PR #20652 OPEN** — review fixes + QA edge case fixes applied, on QA environment.
**Branch:** `DOTCOMPB-7942`

#### Component Tree

```
InfoPage.vue (<script setup>)
└── MrSidebarLayout
    ├── [default slot] → .info-page-form
    │   ├── SignInOptions (NEW)
    │   │   ├── .sr-only(aria-live) — signedInDuringSession flag
    │   │   ├── section(v-if="!isHardAuthenticated")
    │   │   │   ├── h2 "Express Sign In"
    │   │   │   ├── MrBtn.google-btn (icon-only, #prePendContent)
    │   │   │   │   └── MrIcon(google-g, :original, aria-hidden)
    │   │   │   └── .or-divider "OR"
    │   │   └── section.sign-in-auth(v-else)
    │   ├── .contact-info-header
    │   │   ├── h2 "Contact Info" (always visible)
    │   │   └── button "Sign in" (v-if="!isHardAuthenticated")
    │   ├── .form-fields → nextTick focus target after sign-in
    │   ├── .form-opt-ins (3 checkboxes, preserved via skipLoadCustomer)
    │   └── MrBtn "CONFIRM BOOKING"
    └── [#sidebar] → BookingSummary
```

#### PR Review Resolution Summary

| # | Source | Finding | Decision | Lesson |
|---|---|---|---|---|
| BUG-1 | Claude | refreshCustomerCdata error misclassified | 🔧 Inner try/catch | CR-LESSON-1 |
| BUG-2 | Claude | role="alert" always in DOM | ✅ Skip — intentional ADA | CR-LESSON-2 |
| CONCERN-1 | Claude | window.googleInitialized shared | ✅ Skip — no overlap | CR-LESSON-3 |
| NIT-1 | Claude | onSuccess prop vs emit | ✅ Skip — async data | CR-LESSON-4 |
| NIT-2 | Claude | Empty JSDoc | 🔧 Removed | — |
| NIT-3 | Claude | Double tracking | 🔧 Removed event param | — |
| SENTRY-1 | Sentry | Race condition (same as CONCERN-1) | ✅ Skip | CR-LESSON-3 |
| SENTRY-2 | Sentry | Concurrent refresh returns undefined | 🔧 return state.cdata | CR-LESSON-5 |
| SENTRY-3 | Sentry | Race condition (3rd time) | ✅ Skip | CR-LESSON-3 |
| LINT | GitHub Actions | JSDoc types + missing comments | 🔧 All fixed | CR-LESSON-6 |
| LINT-2 | PilkoLint | Missing block descriptions | 🔧 Added descriptions | CR-LESSON-6 |
| ADA-B1 | MR Minion | Heading h3→h2 | 🔧 Changed | CR-LESSON-7 |
| ADA-B2 | MR Minion | Focus lost on DOM swap | 🔧 nextTick focus | CR-LESSON-8 |
| ADA-S1 | MR Minion | Hardcoded ID | ✅ Skip — single use | — |
| ADA-S2 | MR Minion | Icon exposed to AT | 🔧 aria-hidden | CR-LESSON-10 |
| ADA-S3 | MR Minion | Live region mount | 🔧 Interaction flag | CR-LESSON-9 |
| SENTRY-4 | Sentry | onSuccessCallback overwritten by LoginWithPassword | 🔧 Guard added | CR-LESSON-11 |
| Codecov | Codecov | 88.89% patch | ✅ Skip — strong coverage | — |

#### Test Summary

| File | Tests | Status |
|---|---|---|
| `useGoogleSignIn.test.js` | 18 | ✅ |
| `SignInOptions.test.js` | 14 | ✅ |
| `InfoPage.test.js` | 23 | ✅ |
| Customer store (400 tests) | 400 | ✅ (regression check) |
| **Total verified** | **455** | **All passing** |

Playwright: 52 test blocks. Local lint: `bash .tasks/lint-changed.sh` passes.

---

## SECTION 4: FILE INDEX

### New

| File | Association |
|---|---|
| `website/src/vuescripts/components/HairColorBarBookingV2/components/SignInOptions/SignInOptions.vue` | DOTCOMPB-7942 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/SignInOptions/SignInOptions.test.js` | DOTCOMPB-7942 |
| `website/src/vuescripts/components/HairColorBarBookingV2/components/SignInOptions/index.js` | DOTCOMPB-7942 |
| `website/src/vuescripts/composables/useGoogleSignIn.js` | DOTCOMPB-7942 |
| `website/src/vuescripts/composables/useGoogleSignIn.test.js` | DOTCOMPB-7942 |
| `website/src/assets/svg-icons/google-g.svg` | DOTCOMPB-7942 |

### Modified

| File | Association |
|---|---|
| `website/src/vuescripts/composables/index.js` | Added export |
| `website/src/vuescripts/components/HairColorBarBookingV2/InfoPage/InfoPage.vue` | SignInOptions integration + review fixes |
| `website/src/vuescripts/components/HairColorBarBookingV2/InfoPage/InfoPage.test.js` | Updated tests |
| `website/src/vuescripts/store/modules/customer.js` | Concurrent refresh guard (SENTRY-2) |
| `website/src/vuescripts/components/MrSignInV2/LoginWithPassword/LoginWithPassword.vue` | Guard onSuccessCallback overwrite (SENTRY-4) |

### Tooling (gitignored)

| File | Purpose |
|---|---|
| `.tasks/lint-changed.sh` | Local lint script matching PilkoLint CI |
| `.tasks/qa-automation/playwright.config.ts` | Playwright config (env var overrides, retries, video on) |
| `.tasks/qa-automation/parse-acs.mjs` | AC-to-Playwright parser (select-time-retry, SPA nav, scrollIntoView) |
| `.tasks/qa-automation/DOTCOMPB-7942/specs/` | 59 generated test specs |
| `.tasks/qa-automation/DOTCOMPB-7942/screenshots/` | E2E screenshot evidence |

### Documentation (external)

| File | Purpose |
|---|---|
| `~/.brain.d/roam-nodes/madison_reed/2026-04-27-150000-dotcompb_7942_e2e_test_report.org` | E2E test report with screenshots (PDF-exportable) |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-23-150000-dotcompb_7942.org` | Roam node: ticket, ACs, test blocks, edge cases |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.** DOTCOMPB-7942 itself shipped via PR #20652 (merged Apr 28, 2026). A production hotfix branch (`hotfix-google`, ticket DOTCOMPB-8174) was opened May 1 to fix a regression introduced by the original feature; that work is documented inline below.

### Hotfix follow-up — DOTCOMPB-8174 / `hotfix-google` (2026-05-01 → 2026-05-04)

**Why a hotfix was needed.** The original implementation rendered a custom `MrBtn` and called `gai.prompt()` from a synthetic click handler. On Chrome 125+ desktop, Chrome 128+ Android, Safari (desktop + iOS), and Firefox, FedCM mediation rejects the flow because synthetic clicks have `event.isTrusted: false`. Production users saw `AbortError` / `Not signed in with the identity provider` / `NetworkError` and could not complete sign-in.

**Five approaches attempted before settling.** Plan v1 (FedCM lifecycle wiring), Plan v2 (OAuth Code popup), Plan v3 (OAuth Code redirect), Plan H (click-proxy on hidden Google button), Plan O (invisible overlay). Each implemented + tested before moving on. Failure modes:

| Approach | Blocker |
|---|---|
| FedCM `prompt()` lifecycle | Notification methods deprecated; `prompt()` itself requires the user-trusted activation a custom button can't deliver |
| OAuth Code popup | `window.open()` happens after FedCM async callback — outside user-gesture chain; mobile silently drops the popup |
| OAuth Code redirect | Required GCP `Authorized redirect URIs` + AWS SSM `googleClientSecret` we don't control |
| Click-proxy on hidden button | Synthetic `.click()` carries `isTrusted: false`; FedCM rejects + GSI detects and refuses mediation |
| Invisible overlay | Works mechanically but misrepresents the click target — accessibility ambiguity + dark-pattern smell |

**Final approach.** Drop the custom `MrBtn` for the Google sign-in path. Use `gai.renderButton()` directly into a centered container — same pattern as `SignInMixin` / `LoginSignUp` / `SignIn` / `SignUp`. Google's button is the click target itself, so every click is a real user-trusted activation. Trade-off: the Figma design for this button is not implementable through GSI's API (only theme/size/shape/text/locale presets). Recommend re-negotiating the AC.

**Sentry AI race-condition response.** Reviewer flagged that `window.googleInitialized` is a singleton guard shared between `useGoogleSignIn` and `SignInMixin` — `useGoogleSignIn` set `use_fedcm_for_button: true` while `SignInMixin` did not, creating a mount-order race. Resolved by adding `use_fedcm_for_button: true` to `SignInMixin.js`'s `gai.initialize()` config too. One-line change. PR #20716 thread answered (reply id 3175416600).

**Final unstaged set on `hotfix-google` branch:**
- `M useGoogleSignIn.js` — `use_fedcm_for_prompt: false` → `use_fedcm_for_button: true`; cap `renderButton` width at 400px (`Math.min(offsetWidth, 400)`); drop `useCustomButton` option + `triggerGoogleSignIn` function.
- `M useGoogleSignIn.test.js` — pruned mocks for the simplified composable; reorganized imports above local `ERROR_MESSAGES`; 20 tests passing.
- `M SignInOptions.vue` — `mr-btn` + `mr-icon name="google-g"` slot replaced with `.google-btn-container` (always-rendered `gai.renderButton()` target); responsive `max-width 320px (mobile) / 400px (mq-tablet-plus)`; `text-align: center`.
- `M SignInOptions.test.js` — render assertion updated to `.google-btn-container`; `triggerGoogleSignIn` removed from mock return shape; 14 tests passing.
- `M SignInMixin.js` — added `use_fedcm_for_button: true` for FedCM-mediation consistency.
- `D google-g.svg` — orphan asset (was only used inside removed MrBtn slot).

**Verification:** Lint 0 errors, tests 60/60 passing (`useGoogleSignIn` 20 + `SignInOptions` 14 + `InfoPage` 26 regression).

**Where the hotfix narrative lives.** Roam node `~/.brain.d/roam-nodes/madison_reed/2026-05-01-132350-hotfix_google.org` (UUID `0a4fa07b-15ab-4287-b448-3cb63e9498f9`) with sections WHY CUSTOM BUTTONs CANNOT WORK, APPROACHEs TRIED (5 subsections), FINAL APPROACH, COMMIT MSG, PR DESCRIPTION (full MR brand-formatted PR body). Plan archive: `/Volumes/dev-partition/github-madison-reed/the-code/.tasks/hotfix-google/plan.md`. JIRA: https://madison-reed.atlassian.net/browse/DOTCOMPB-8174 (description rewritten issue-only). PR: https://github.com/MadisonReed/mr/pull/20716.

### Hotfix pending work (DOTCOMPB-8174)

*   [ ] **Human-driven git ops** — stage + commit (commit msg in roam node `* COMMIT MSG`), push, address CI feedback, request review, merge to Release. *AI runs no git commands.*
*   [ ] **Pre-merge verification** — confirm GCP OAuth client *Authorized JavaScript origins* includes `www.madison-reed.com`, `dotcom.mdsnrd.com`, all QA hosts, `localhost:3000`. **No** `Authorized redirect URIs` needed for this approach.
*   [ ] **Manual QA per the QA INSTRUCTIONs** in the roam node — 9 scenarios across desktop Chrome / iPhone Safari / Android Chrome / Firefox / responsive widths / ADA / Segment events.

### Hotfix — Where to resume

**If the user says "ship the hotfix" / "open the PR" / "commit it":** the work is already done. Direct them to the roam node `* COMMIT MSG` + `* PR DESCRIPTION` sections — copy verbatim into `git commit` and the PR body. AI runs no git commands.

**If the user says "respond to reviewer X":** read the latest comments via `gh pr view 20716 --comments` or `gh api /repos/MadisonReed/mr/pulls/20716/comments`. Sentry AI thread is answered.

**If the user says "the hotfix broke something":** verify the unstaged changes are still present (5 modified + 1 deleted). The fix relies on `gai.renderButton()` being the click target. If the custom MrBtn is restored, the FedCM bug returns.

**If the user says "the Figma design needs to land":** the Figma styling is **not implementable** through GSI's API. Re-open the design conversation with Product/Design — pick from `theme` (`outline` / `filled_blue` / `filled_black`), `shape` (`rectangular` / `pill` / `circle` / `square`), `text` presets only. No color, font, padding, or border-color overrides are reachable.

---

### What was done last (original DOTCOMPB-7942 — historical, PR #20652 merged 2026-04-28)

*   **(2026-04-29)** QA testing on QA environment. `USER_ALREADY_EXISTS` error surfaced — added specific frontend message in `useGoogleSignIn.js`. Discussed with Addison + Carley whether backend should allow auto-linking Google to existing password accounts.
*   **(2026-04-28)** Duplicate appointment edge case: added `errorMessage.includes('already has booked')` check in `InfoPage.vue bookAppointment()` to show user-friendly message instead of generic "Something went wrong."
*   **(2026-04-28)** E2E Playwright: 59 tests. Rebuilt parser with SPA-safe navigation (`waitForFunction` instead of `waitForURL`), `scrollIntoViewIfNeeded` on all clicks, `select-time-retry` command (5 attempts, skips fully booked dates, screenshots errors). Location changed to `hillsboro`.
*   **(2026-04-27)** Sentry comments: replied to SENTRY-4 (implemented), SENTRY-5/6 (skipped). E2E test report roam node created. JIRA test case content prepared.

### Original-feature pending (DOTCOMPB-7942 PR #20652 — merged)

*   [ ] Confirm with Carley: keep `USER_ALREADY_EXISTS` blocking or allow Google auto-link to password accounts? *(carries over to DOTCOMPB-8174 — same component still surfaces this message)*

### Original-feature where to resume

If the user asks to **check CI on the hotfix**: `gh run list --repo MadisonReed/mr --branch hotfix-google --limit 5`
If the user asks to **run lint**: `bash .tasks/lint-changed.sh`
If the user asks to **run unit tests**: `cd website && npm run test:vue useGoogleSignIn SignInOptions InfoPage`
If the user asks to **run E2E**: `npx playwright test --config=.tasks/qa-automation/playwright.config.ts DOTCOMPB-7942/ -g "EDGECASE" --project=desktop-chromium --headed --workers=1`
If the user asks about **USER_ALREADY_EXISTS**: Backend returns 403 when Google email matches existing MR account with password. Frontend shows specific message. Backend change to allow auto-link would be in `mr_modules/controllers/lib/customer.js` — `createCustomer` call at line 1908 needs `modifyExistingCustomer: true`. Shared module — needs backend team review.
If the user asks about **duplicate appointment**: Backend `calendar.js:546-567` throws plain `Error` (no code). Frontend checks message string. Proper fix would be `MRError('ALREADY_BOOKED')` in backend — separate ticket.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session.
> See `~/.claude/skills/session-reset/rules/activity-log.md`. Newest row first.

| Datetime         | Duration | Type            | Reference        | Description |
|------------------|----------|-----------------|------------------|-------------|
| 2026-05-04 17:45 | 0.3h     | session-reset   | this             | Bootstrap Section 6 (Activity Log) added per v4.1 rule. Also captured the DOTCOMPB-8174 hotfix branch follow-up that ran 2026-05-01 → 2026-05-04: 5 plan iterations, final approach (drop custom MrBtn for `gai.renderButton()`), Sentry AI race-condition fix on `SignInMixin`, PR #20716 open. |
| 2026-05-04 17:15 | 0.5h     | bug-fix         | DOTCOMPB-8174    | Sentry AI race-condition response: added `use_fedcm_for_button: true` to `SignInMixin.js`'s `gai.initialize()` for FedCM-mediation consistency across all Google sign-in surfaces (booking flow, sign-in modal, sign-up, login/signup combined). Reply at PR #20716#discussion_r3175405177 (reply id 3175416600). |
| 2026-05-04 16:45 | 0.5h     | code-review     | DOTCOMPB-8174    | /code-review pass on the 4 modified + 1 deleted hotfix files: 2 findings (MEDIUM imports placement + LOW stale "overlay" wording in test). Both implemented. Lint 0 errors, tests 60/60 passing. |
| 2026-05-04 16:00 | 1h       | documentation   | DOTCOMPB-8174    | Hotfix roam node refined to final state. Sections: WHY CUSTOM BUTTONs CANNOT WORK, APPROACHEs TRIED (5 subsections), FINAL APPROACH, COMMIT MSG, PR DESCRIPTION. JIRA description rewritten issue-only. |
| 2026-05-04 13:00 | 1h       | implementation  | DOTCOMPB-8174    | FINAL APPROACH lands: drop custom MrBtn → `.google-btn-container` with `gai.renderButton()`. Composable simplified, orphan `google-g.svg` deleted, tests reduced 67 → 60, lint 0 errors. |
| 2026-05-02 (multi-hour) | ~6h | refinement+implementation | DOTCOMPB-8174 | Iteration log: Plan v1 (FedCM lifecycle) → v2 (OAuth Code popup) → v3 (OAuth Code redirect) → Plan H (click-proxy hidden button) → Plan O (invisible overlay) → FINAL (default rendered button). Each tested before pivoting. v3 backend code added then fully reverted on `redirect_uri_mismatch` 400. |
| 2026-05-01 evening | 0.3h | documentation   | DOTCOMPB-8174    | JIRA ticket DOTCOMPB-8174 created (sprint D465 Home Alone 04/29, En curso, child of DOTCOMPB-7942). Hotfix roam node created (UUID `0a4fa07b-15ab-4287-b448-3cb63e9498f9`). |
| 2026-04-28        | —       | pr-merge        | DOTCOMPB-7942    | PR #20652 merged into Production after CI green + reviewer approval. Original Google SSO Booking Flow shipped. *(Hotfix DOTCOMPB-8174 surfaced shortly after.)* |
| 2026-04-29 → 04-27 | (multi-day) | implementation+pr-feedback | DOTCOMPB-7942 | QA on QA env (`USER_ALREADY_EXISTS` message added); duplicate-appointment edge case fix; E2E Playwright suite (59 tests) rebuilt with SPA-safe navigation; Sentry SENTRY-4 implemented + SENTRY-5/6 skipped with reasoning; Codecov skipped; MR Minion ADA blockers + suggestions resolved (heading hierarchy h3→h2, focus return on sign-in, mr-icon aria-hidden, signedInDuringSession interaction flag); JSDoc PilkoLint round 1+2 fixed. (Detailed in PR #20652 review section below.) |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->

---

# PR #20652 — Review Comments (2026-04-25)

## Claude Code Review (@claude review once)

### BUG-1: `refreshCustomerCdata` failure misclassified as Google error
- **File:** `useGoogleSignIn.js:96-143`
- **Severity:** Bug
- **Issue:** If `googleLogin` succeeds but `refreshCustomerCdata` throws, the catch block shows "Something went wrong with Google sign-in" — misleading since Google auth actually succeeded.
- **Fix:** Catch `refreshCustomerCdata` separately or distinguish the error source.
- **Status:** [x] IMPLEMENTED — separated into inner try/catch. Google errors stay in outer catch. Cdata failure proceeds with `onSuccess(undefined)`.

### BUG-2: `role="alert"` always in DOM — empty announcements
- **File:** `SignInOptions.vue:23`
- **Severity:** Bug
- **Issue:** Empty `<p role="alert">` always rendered. Some AT may register it. When error clears, empty string could trigger announcement of nothing.
- **Suggestion:** Use `v-if="signInError"` instead of persistent element.
- **Note:** We intentionally chose persistent `role="alert"` per Advanced ADA Rule 1 ("live region outside v-if"). This is a **design decision conflict** — Claude's suggestion contradicts our ADA audit finding.
- **Status:** [x] SKIPPED — intentional design decision
- **Answer:** This was a deliberate ADA choice validated during our code review round 2. Per WCAG 4.1.3 Status Messages and Advanced ADA Rule 1, `role="alert"` containers should be persistent in the DOM so screen readers reliably announce content changes. Using `v-if` risks some AT combinations (particularly NVDA + Firefox) missing the mount announcement entirely. The empty `<p role="alert">` is a non-issue in practice — screen readers skip empty elements. The persistent approach is the safer pattern for cross-AT compatibility.

### CONCERN-1: `window.googleInitialized` shared with SignInMixin
- **File:** `useGoogleSignIn.js:64-76`
- **Severity:** Concern
- **Issue:** If SignInMixin initializes first, the composable's callback won't be wired. If composable initializes first, mixin's callback won't fire.
- **Status:** [x] SKIPPED — not applicable in current architecture
- **Answer:** Not a risk in the current implementation. The composable uses `useCustomButton: true` and calls `gai.prompt()` — it does not use `renderButton`. The `initialize()` callback is registered by whoever mounts first (InfoPage composable in the booking flow). The SignInMixin (used by the sign-in modal opened via "Sign in" link) sees `window.googleInitialized = true` and only calls `renderButton()` for its own container — it does not re-register the callback. Since `gai.prompt()` always uses the callback from `initialize()`, there is no conflict. BookingV2 InfoPage and the modal never co-render as competing Google auth consumers. If this architecture changes in the future (e.g., multiple Google auth entry points on the same page), a decoupled initialization pattern would be needed — but that's out of scope for this ticket.

### NIT-1: `onSuccess` prop vs emit pattern
- **File:** `SignInOptions.vue:44-48`
- **Issue:** Uses Function prop instead of Vue `emit`. Bypasses DevTools event tracking.
- **Status:** [x] SKIPPED — consistent with codebase pattern
- **Answer:** The Function prop pattern is intentional and consistent with how the booking flow handles async callbacks. `ConsultationInformationModal` uses `onSuccessCallback` Function prop, and the replaced `AuthBanner` used `signIn/setOnSuccessCallback` store commit — both are callback patterns, not emit. The key reason: `onSuccess` receives `cdata` as a return value from the async Google auth flow — Vue `emit` doesn't support returning data from async parent handlers back to the child. The Function prop is the correct pattern for this data flow.

### NIT-2: Empty JSDoc comments
- **File:** `InfoPage.vue:179, 239`
- **Issue:** Empty `/** */` comments. Pre-existing + one new.
- **Status:** [x] IMPLEMENTED — removed both empty JSDoc comments.

### NIT-3: `onSignInClick` tracks event twice
- **File:** `InfoPage.vue:207-225`
- **Issue:** `trackMREvent('Info - Sign in clicked')` called on line 219 AND `openSignInModal` also calls `trackMREvent(event.name)` internally.
- **Fix:** Remove one of the two calls.
- **Status:** [x] IMPLEMENTED — kept explicit `trackMREvent`, removed `event` param from `openSignInModal` to prevent double-fire.

---

## Sentry Bot — Bug Predictions

### SENTRY-1: Race condition on `window.googleInitialized`
- **File:** `useGoogleSignIn.js:75`
- **Severity:** HIGH
- **Issue:** Same as CONCERN-1 above. Shared flag between composable and SignInMixin.
- **Status:** [x] SKIPPED — same reasoning as CONCERN-1
- **Answer:** This is the same concern addressed in CONCERN-1. The shared `window.googleInitialized` flag is intentional and follows the existing pattern established by `SignInMixin`. The composable uses `useCustomButton: true` with `gai.prompt()` — it never competes with `renderButton`. GSI's `gai.initialize()` is designed to be called once globally; the flag prevents redundant initialization. The two consumers (composable on InfoPage, SignInMixin in sign-in modal) never co-render as competing auth entry points. The callback registered by the first `initialize()` call persists and is used by `prompt()` regardless of which component initialized. No race condition exists in practice.

### SENTRY-2: `refreshCustomerCdata` returns undefined during concurrent refresh
- **File:** `useGoogleSignIn.js:122` → fix in `customer.js:561-576`
- **Severity:** MEDIUM
- **Issue:** If `refreshCustomerCdata` is already running (guarded by `state.refreshingCdata`), it returns `undefined`. The `onSuccess` handler receives `undefined` cdata → form not pre-filled.
- **Status:** [x] IMPLEMENTED — changed `return;` to `return state.cdata;` in the concurrent guard. All consumers now get the current cdata instead of undefined.

### SENTRY-3: Race condition (3rd duplicate)
- **File:** `useGoogleSignIn.js:78`
- **Status:** [x] SKIPPED — same as SENTRY-1/CONCERN-1. Replied with reference to previous responses.

---

## GitHub Actions Bot — JSDoc Lint Warnings + PilkoLint

**Round 1 (8 warnings):** 4 type casings `function` → `Function`, 4 missing JSDoc comments.
**Round 2 (2 errors — CHANGES_REQUESTED):** Missing JSDoc block descriptions on `getContainerWidth` and `handleGoogleCallback`. Tags alone not enough — PilkoLint requires a description line.

**Status:** [x] ALL IMPLEMENTED — types fixed, JSDoc added with block descriptions.

---

## Codecov Report

- **Patch coverage:** 88.89% (22 lines missing)
- **Project coverage:** 48.98% (+0.05%)

**Status:** [x] SKIPPED — Coverage is strong. Missing lines are pre-existing error paths and edge cases.

---

## MR Minion Bot — ADA Review

### ADA-BLOCKER-1: Heading hierarchy (WCAG 1.3.1)
- **Status:** [x] IMPLEMENTED — `h3` → `h2` for Contact Info.

### ADA-BLOCKER-2: Focus lost after sign-in (WCAG 2.4.3)
- **Status:** [x] IMPLEMENTED — `nextTick` focus to First Name field.

### ADA-SUGGESTION-1: Hardcoded `id`
- **Status:** [x] SKIPPED — single-use component, no duplicate risk.

### ADA-SUGGESTION-2: Icon exposed to AT
- **Status:** [x] IMPLEMENTED — `aria-hidden="true"` on `mr-icon`.

### ADA-SUGGESTION-3: Live region mount announcement
- **Status:** [x] IMPLEMENTED — `signedInDuringSession` interaction flag.
