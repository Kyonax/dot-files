<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the DOTCOMPB-8211 ([DY Question Email Capture] Popup Dismissed + Popup Capture Submitted events not firing — plus responsive width fix + engagement suppression) session. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read sections 1 → 6 on first load; afterwards, reference by number.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, patterns, conventions for ALL work in this session. | Before any code task. |
| **2. Session Overview** | Scope, decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-feature detail: files, decisions, code blocks. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference file path table. | When reading, editing, or locating files. |
| **5. Last Interaction** | Short-term memory: last work, pending, resume points. | At conversation start. |
| **6. Activity Log** | Datetime-stamped, append-only audit trail. | When you need exact "what was done when". |

**Operational Rule:** Always look for the last request identified by `###` title. Load relevant skills and apply Section 1 rules.

**Key principle:** Data may appear in multiple sections with different framing. This is intentional — each section answers a different question about the same knowledge.

**Architectural baseline:** This session **inherits** from `dy-question-email-capture-epic.md` (umbrella covering the full epic DOTCOMPB-7051: template architecture, event taxonomy, MR-side bridges). The umbrella was itself compacted from `dotcompb-7052-dynamic-yield-email-sms.md` (original refactor session). This session began as a **bug-fix scoped to missing `Popup Capture Submitted` `dpx` event**; the originally-spec'd `Popup Dismissed` event was investigated and confirmed *not implementable from template JS* due to DY platform lifecycle constraints. As of 2026-05-14, scope was expanded to include (a) a responsive-width fix for tablet/mobile and (b) engagement-based popup suppression via `localStorage` (both at Marketing's request).

**Cross-session references** use `[session: dy-question-email-capture-epic > section-N.M]` syntax — see `~/.claude/skills/session-memory/rules/reference-syntax.md`.

**Roam node:** `~/.brain.d/roam-nodes/madison_reed/2026-05-13-111704-dotcompb_8211.org` (ID `6f2c6e99-6cd6-407c-b98a-04d790f5e044`). The roam node is the **canonical paste-ready source** for the DY admin — it contains a SOURCE OF TRUTH section (verbatim live HTML/CSS/JS, never modified) and a PASTE-READY CODE section (final patched version, including the 2026-05-14 width + suppression additions).

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `mr-dotcom-dev` (Vue/Vuex), `mr-roam-node` (ticket documentation), `pr-scribe` (PR body authoring), `tophat-tools` (CMS investigation), `session-memory` (umbrella architectural reference), `session-reset` (this skill).
>
> The work happens **inside the Dynamic Yield admin (`https://adm.dynamicyield.com/`)** — three editor tabs (HTML / CSS / JS). No MR repo changes. Verification is via Chrome DevTools → Network → filter `dpx` and Application → Local Storage → `mr-email-capture-engaged`.

### 1.1 DY Tracking Event Publishing — Pattern (inherited)

DY templates publish custom events via `DY.API('event', { name, properties })`. Each call becomes one `POST` to `https://async-px.dynamicyield.com/dpx`. All existing in-template events use this **exact same shape** — any new event MUST follow it.

| Event Name | Trigger | Fires from | Props | Status |
|---|---|---|---|---|
| `Question Shown` | Modal renders | Init (after `replaceLegalTextWithLinks()`) | `event_category`, `timestamp` | ✅ working |
| `Question Answer Selected` | Quiz button click | Quiz click handler | `event_category`, `selected_answer`, `timestamp` | ✅ working |
| `Email Submitted Successfully` | Email submit success | Email `.then((response) =>)` | `event_category`, `email_submitted`, `quiz_selection` | ✅ working |
| `identify-v1` | Email submit success, if `cuid` | Email `.then((response) =>)` | `cuid` | ✅ working |
| **`Popup Capture Submitted` (email_only)** | GET FREE SHIPPING submit success | Email `.then((response) =>)` | `event_category`, `opt_in_type: 'email_only'`, `reward_granted: 'free_shipping'`, `timestamp` | 🆕 IMPLEMENTED |
| **`Popup Capture Submitted` (email_sms)** | CLAIM MY OFFER submit success | SMS `.then((response) =>)` | `event_category`, `opt_in_type: 'email_sms'`, `reward_granted: 'free_shipping_and_discount'`, `timestamp` | 🆕 IMPLEMENTED |
| `Popup Dismissed` | (any close path) | — | — | ❌ NOT IMPLEMENTABLE — see §1.5 |

### 1.2 Preview URL (Variation 2, 100% traffic in QA A/B)

```
https://www.qa.mdsnrd.com/?dyExperienceId=2667345&dyIsDraft=false&dyIsPreview=true&dyPersistSession=true&dypPreviewKey=659256927&dySmartId=1129717&dyTemp=true&dyVariationId=%5B21887493%5D
```

- Campaign: **Test Email/SMS Capture Modal**, smartId `1129717`, expId `2667345`
- Variation 2: variationId `21887493` (live)
- Variation 1: older email-only template (control)

### 1.3 Source-of-Truth Pattern (CRITICAL workflow rule)

The roam node (`2026-05-13-111704-dotcompb_8211.org`) is structured with **two top-level code-block sections**:

1. **`* SOURCE OF TRUTH — LIVE DY TEMPLATE [#A]`** — verbatim HTML / CSS / JS as currently deployed in DY admin. **NEVER MODIFY.** This is the recovery baseline if any paste breaks production.
2. **`* PASTE-READY CODE — DY ADMIN TABS`** — the patched version for the DY admin (HTML, CSS, JS subsections).

**When editing:** only touch PASTE-READY. SOURCE OF TRUTH stays byte-identical to whatever is deployed. The user's repeated instruction was: *"NEVER NEVER DELETE THOSE"* — these sections (and all NOTEs UPDATE entries) are permanent record.

**Exception logged 2026-05-14:** SOURCE OF TRUTH CSS was updated to reflect what the user had been re-pasting into DY admin during iteration (the `.dy-modal-contents` rule). SOURCE OF TRUTH JS still reflects pre-2026-05-14 baseline (no engagement suppression) — marked as stale in §2.4.

### 1.4 HTML/JS Coupling — `toggleLoader` Contract

The JS `toggleLoader(button, text, loader, show)` helper assumes its `text` and `loader` arguments are real DOM elements (calls `.classList.add('hide')` on each). The submit handlers cache these at script init:

```js
let emailBtnText = emailCtaBtn.querySelector('.btn-text');
let emailBtnLoader = emailCtaBtn.querySelector('.btn-loader');
```

**The HTML buttons MUST contain `<span class="btn-text">${CTA}</span><span class="btn-loader hide"></span>`**, otherwise the queries return `null`, `toggleLoader` throws synchronously on submit, and the entire submit handler aborts — no API call, no transition, no events.

**This was the bug we hit on 2026-05-13:** user pasted source-of-truth HTML without the spans, but the JS expected them. Result: GET FREE SHIPPING did nothing. Fix: HTML must include the spans + CSS must include `.btn-loader` + `@keyframes dy-form-cta-spin`. The roam node PASTE-READY HTML and CSS sections now reflect this fix.

### 1.5 DY Platform Lifecycle Constraint — Why `Popup Dismissed` Is Not Achievable

DY mounts the template instance when the popup opens and unmounts it when the popup closes. Template JS only executes *between* those events. Any listener we attach to detect dismissal (custom close button, document-level capture-phase delegated listener on `.dy-lb-close`, ESC keydown, `beforeunload`) is torn down by DY's unmount before it can complete a synchronous `DY.API('event', { name: 'Popup Dismissed' })` call.

**This is a hard platform limit, not a code-quality issue.** Resolution requires a DY platform-side trigger (campaign-level rule, audience exit rule, or post-action callback) configured in the DY admin — escalated to Marketing / DY rep, not solvable from the template editor.

DY's actual close-button element (confirmed via DOM diagnostic on 2026-05-13): `<div class="dy-lb-close" tabindex="0" role="button" aria-label="Close">`. The class name varies by DY account version and is NOT stable. Even if we could detect it, the lifecycle issue remains.

### 1.6 ADA / Existing Behavior — Do Not Regress

The four currently-working `dpx` events must continue to fire. The popup's visual close behavior (DY's own ✕) must not regress. The four AC checkpoints already implemented under DOTCOMPB-7052 (`max-at-tweak` font utility, single-button gating, modal close on overlay click, accessible focus management) are out of scope but must not be broken.

### 1.7 No Repo Changes Expected

The fix is 100% in the DY admin's three tabs. No MR repo PR. The MR repo's `window.createCustomerFromEmailCapture` and `window.addFirstTimeVisitorPhoneAndOffer` globals (bound in `website/src/vuescripts/mrVueApp.js:687-688` and `SsrApp.vue:239-240`) are consumed by the DY template JS but not modified by this ticket.

### 1.8 DY Overlay DOM Structure (CRITICAL — discovered 2026-05-14)

When DY renders the popup, it produces this exact wrapper hierarchy in the parent document (confirmed via browser inspection):

```
.dy-modal-container.dy-act-overlay         ← fixed, full-viewport, contains backdrop + content
├── .dy-modal-backdrop.lb_overlay
└── .dy-modal-wrapper
    └── .dy-modal-contents                 ← parent of BOTH close-X and our template
        ├── .dy-lb-close                   ← the X button — sibling of our template
        └── .dy_unit.dy_smart_object_2667345
            └── <style>...</style>
            └── .mr-email-sms-modal        ← our template starts here
```

**Critical implication:** the close-X (`.dy-lb-close`) is **NOT a child of our template** — it's a sibling positioned absolutely against `.dy-modal-contents` (the platform-painted parent). Therefore:

- Applying `width` / `padding` to `.mr-email-sms-modal` moves OUR content but **does NOT move the X** — produces a visible gap.
- To shrink the visible modal while keeping the X anchored to the corner, target `.dy-modal-contents` itself. Shrinking the parent moves everything inside, including the X. (See §3.6 for the resulting CSS.)

**CSS targeting reach:** the template's `<style>` block is global CSS — it can target selectors outside the `.dy_unit` wrapper (e.g., `.dy-modal-contents`). No JS reach-up needed.

### 1.9 Engagement-Based Popup Suppression Pattern (NEW 2026-05-14)

DY platform does **not** expose an API for "don't show again after CTA submit." Confirmed with Kaila / DY rep: DY only supports dismiss-based suppression (close-X click) and frequency caps at the campaign level — neither covers the engagement-based case Marketing requested. Pattern is implemented in template JS:

| Element | Role |
|---|---|
| **IIFE wrap** | Wraps the entire template JS so we can `return` early without polluting global scope. DY templates can re-render; the IIFE guarantees no `let`-collision and a clean early-exit. |
| **`ENGAGEMENT_FLAG_KEY = 'mr-email-capture-engaged'`** | Namespace for localStorage. Prefix `mr-` avoids collisions with other DY templates on the same origin. |
| **`ENGAGEMENT_TTL_DAYS = 30`** | Configurable TTL. Default 30 days per industry convention; Marketing to confirm. |
| **`isUserEngaged()`** | Reads the flag, parses JSON, returns `true` if `(Date.now() - data.timestamp) < ttlMs`. Wrapped in `try/catch` for Safari private-browsing graceful degradation (popup shows again if storage write/read fails). |
| **`markUserEngaged(optInType)`** | Writes `{ timestamp: Date.now(), opt_in_type: <'email_only' \| 'email_sms'> }` to localStorage. Called inside email + SMS submit success blocks, right after the corresponding `Popup Capture Submitted` event. |
| **Early-exit at init** | `if (isUserEngaged()) { document.querySelector('.mr-email-sms-modal')?.closest('.dy-modal-container').style.display = 'none'; return; }` — hides the entire overlay (backdrop included) and bails before any listeners or events. `Question Shown` does **not** fire on suppressed renders. |

**Production templates do NOT have this pattern.** Confirmed 2026-05-14 by analyzing both existing prod templates: `Email/SMS Capture Modal` and `Email/SMS Capture Modal Booking`. Neither contains any localStorage write, cookie write, init-time check, or `DY.API` event. Their only flag is in-memory `formSubmitted`, which dies on reload. Whatever "don't re-show" behavior Marketing observes on those templates must be configured at the DY platform level (audience exclusion or campaign frequency cap), not in template code. Worth confirming with the DY rep — this Variation 2 implementation is **net-new** functionality at the template layer, not a replication of an existing pattern.

### 1.10 Viewport-Unit Safety on Mobile (audit 2026-05-14 — NOT yet applied)

The mobile media query (`max-width: 759px`) currently uses `height: 95vh`. This is unsafe on iOS Safari and other mobile browsers when the URL bar is visible — `vh` represents the *largest* possible viewport (chrome hidden), so `95vh` extends beyond the visible viewport and clips the close-X / form bottom. Recommended pattern:

```css
height: 95vh;       /* fallback */
height: 95svh;      /* small viewport height — chrome-visible safe */
```

Safe-area insets (`env(safe-area-inset-*)`) are not yet applied either — notched devices (iPhone X+) can have the modal top obscured behind the notch and the bottom under the home indicator. Recommended additions:

```css
padding-top: max(3em, env(safe-area-inset-top));
padding-bottom: env(safe-area-inset-bottom);
padding-left: max(0px, env(safe-area-inset-left));
padding-right: max(0px, env(safe-area-inset-right));
```

**Status:** audited and documented; user opted not to apply on 2026-05-14. Tracked in §2.4 as pending.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Originally: bug fix for DOTCOMPB-8211 — two `dpx` tracking events spec'd by DOTCOMPB-7051 were not firing on Variation 2 of the Email/SMS Capture Modal (`Popup Dismissed` and `Popup Capture Submitted`). Final tracking-event scope: `Popup Capture Submitted` implemented; `Popup Dismissed` declared unimplementable from template JS.

**Expanded 2026-05-14:** added two more deliverables in the same DY admin paste:
1. Responsive-width fix for tablet (≤958px) and mobile (≤759px) — modal gets a 5% horizontal inset while close-X stays anchored.
2. Engagement-based popup suppression via `localStorage` — popup hides on future visits if user submitted within the TTL (30 days default).

Both expansions land in the same Variation 2 PASTE-READY CSS + JS, alongside the tracking-event additions.

### 2.2 Scope / Cross-Reference

| Key | Type | Status | Role |
|---|---|---|---|
| DOTCOMPB-7051 | Epic | Abierta | Parent epic — DY: Question Email Capture Template |
| DOTCOMPB-7052 | Historia | In Test | Email/SMS Capture Modal refactor — owns `Popup Dismissed` spec |
| DOTCOMPB-7166 | Historia | Pruebas | 1st Screen: Prompt a Question + No Email |
| DOTCOMPB-7167 | Historia | In Test | 2nd Screen: SMS Capture — owns `Popup Capture Submitted` spec |
| **DOTCOMPB-8211** | **Error** | En curso → Closing | **This bug + the two 2026-05-14 add-ons** |

### 2.3 Decisions

1. **(2026-05-13) DEC-001 — Spec'd event names verbatim.** Use `Popup Dismissed`, `Popup Capture Submitted` exactly — Marketing dashboards filter on these strings.
2. **(2026-05-13) DEC-002 — Add events, don't rename.** `Email Submitted Successfully` keeps firing; `Popup Capture Submitted` is added alongside, not as a replacement. Marketing depends on both schemas.
3. **(2026-05-13) DEC-003 — `variant_info` is "if applicable".** Optional template variable `${Variant Info}` (set to `V2` on Variation 2). When unset/blank, the property is omitted from the payload.
4. **(2026-05-13) DEC-004 — Fire `Popup Capture Submitted` on both submits.** Once with `email_only` on email submit success, once with `email_sms` on SMS submit success. Marketing's funnel-analysis can dedupe / use the later event.
5. **(2026-05-13) DEC-005 — `Popup Dismissed` is OUT OF SCOPE.** Strategy A + Strategy C both failed empirically on the live preview — DY's lifecycle prevents any template-JS approach. Escalate to Marketing / DY rep for a platform-side trigger.
6. **(2026-05-13) DEC-006 — Minimal-diff JS.** Final patch adds ONLY: two state flags (`popupCaptureEmailFired`, `popupCaptureSmsFired`), one const (`VARIANT_INFO`), one helper (`firePopupCaptureSubmitted`), and two new inline `DY.API('event', ...)` call sites. Everything else byte-identical to source-of-truth JS.
7. **(2026-05-13) DEC-007 — Source-of-truth pattern.** Roam node has a permanent `* SOURCE OF TRUTH — LIVE DY TEMPLATE [#A]` section that mirrors what's actually deployed in DY admin, NEVER modified. All paste-ready iterations go into the separate `* PASTE-READY CODE — DY ADMIN TABS` section.
8. **(2026-05-13) DEC-008 — HTML buttons MUST have `.btn-text` + `.btn-loader` spans.** Identified after submit-handler broke: JS expects these as elements, not null. Fix encoded into PASTE-READY HTML + CSS.
9. **(2026-05-14) DEC-009 — Responsive width fix targets `.dy-modal-contents`, not `.mr-email-sms-modal`.** Two failed attempts (width 95% on modal, then `padding-left/right: 2.5%` on modal) both produced a visible gap between the close-X and the modal corner. Browser inspection revealed the close-X is a *sibling* of our template — its parent is `.dy-modal-contents`. Final solution: `.dy-modal-contents { max-width: 95vw; margin: 0 auto; }` at both `≤958px` and `≤759px` breakpoints. The platform-painted X moves with the parent. The mobile rule also adds `justify-content: start; padding-top: 3em;` to prevent content collision with the X area.
10. **(2026-05-14) DEC-010 — Engagement suppression lives in template JS via `localStorage`.** DY platform has no built-in API for "don't show after CTA submit" (confirmed by Kaila / DY rep). Both production templates (`Email/SMS Capture Modal`, `Email/SMS Capture Modal Booking`) were analyzed — neither has any template-level suppression code, so whatever existing suppression Marketing observes must be at the DY platform/server level. For Variation 2 we implement at the template layer: IIFE wrap + `isUserEngaged()`/`markUserEngaged()` helpers + early-exit at init. TTL default 30 days (configurable via `ENGAGEMENT_TTL_DAYS`).
11. **(2026-05-14) DEC-011 — `svh` + safe-area-inset recommendations NOT applied this session.** Viewport-unit audit flagged `95vh` on mobile as unsafe (clips behind URL bar on iOS Safari) and noted no `env(safe-area-inset-*)` handling exists. User opted not to apply now — tracked as pending in §2.4 for a follow-up touch.
12. **(2026-05-14) DEC-012 — `textContent` over the HTML-parsing setter for `smsError`.** During engagement-suppression rollout, the SMS error message render was changed from the HTML-parsing setter to `.textContent` — XSS hardening, since the API message is plain text and doesn't need HTML formatting. No functional change. (Security hook in the working file flagged the pre-existing HTML-setter usage on edit; opportunity-cost fix.)

### 2.4 Pending Work

- [ ] **Roam node stale sections** (NEW 2026-05-14) — these still show the OLD flow (without engagement suppression) and need to be brought in line with PASTE-READY:
  - `SOURCE OF TRUTH → JS` (still pre-IIFE / pre-engagement)
  - `STRUCTURE AND FUNCTIONALITY → Flow Diagram` (no "if engaged, hide and bail" branch)
  - `STRUCTURE AND FUNCTIONALITY → State Flags` (doesn't list `ENGAGEMENT_FLAG_KEY` / `ENGAGEMENT_TTL_DAYS`)
  - `QA INSTRUCTIONS → Scenario A` (no step to verify localStorage write + reload-suppression behavior)
- [ ] Apply `svh` fallback + `env(safe-area-inset-*)` to the mobile media query — see §1.10 for the exact pattern. Optional, recommended.
- [ ] Marketing confirmation on engagement TTL (30 days default) — could be 30/60/90/180 days.
- [ ] Marketing confirmation on `opt_in_type: 'email_only' | 'email_sms'` and `reward_granted: 'free_shipping' | 'free_shipping_and_discount'` — match dashboard expectations?
- [ ] Marketing confirmation on `Popup Capture Submitted` firing semantics: fire-twice (once per submit) vs single-event-at-final-state.
- [ ] Add `Variant Info` template variable in DY admin (value `V2` for Variation 2). Optional but recommended.
- [ ] Paste the final HTML / CSS / JS from the roam node PASTE-READY CODE section into the DY admin → Variation 2 → respective tabs. (Note: CSS now includes `.dy-modal-contents` width rules; JS now wrapped in IIFE with engagement helpers.)
- [ ] QA Scenario A on the preview URL: full conversion (email + SMS) → 6 `dpx` POSTs in order. Also verify localStorage flag write + that reload suppresses the popup.
- [ ] Publish Variation 2 once QA passes.
- [ ] Escalate `Popup Dismissed` to Marketing / DY rep for a platform-side trigger.
- [ ] Close DOTCOMPB-8211 with comment summarizing the in-scope fix + lifecycle limit on `Popup Dismissed` + the 2026-05-14 add-ons.

### 2.5 Out of Scope

- `Popup Dismissed` event (lifecycle limit — escalate to DY rep).
- Renaming `Email Submitted Successfully` to match 7167 spec (DEC-002).
- Variation 1 (the email-only control). Backport optional.
- MR repo PR — none expected (DEC / §1.7).
- Retroactively adding engagement suppression to the existing production templates (`Email/SMS Capture Modal` and `Email/SMS Capture Modal Booking`). They have none — see §1.9. Separate ask for Marketing if they want parity.

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 DY Template Variation 2 — Final Patch

**Created:** 2026-05-13 | **Last updated:** 2026-05-14 | **Status:** Patched in roam node; pending paste into DY admin + QA + publish.

**Three-tab change summary (vs source of truth):**

| Tab | Change |
|---|---|
| **HTML** | Both submit buttons gain `<span class="btn-text">${CTA}</span><span class="btn-loader hide"></span>` to satisfy the JS `toggleLoader` contract (§1.4). *No further HTML change on 2026-05-14.* |
| **CSS** | `.form-cta` gains `display: inline-flex; justify-content: center; align-items: center; min-height: 2.5em;`. New rule `.form-cta .btn-loader { ... border-radius: 50%; animation: dy-form-cta-spin 0.8s linear infinite; }` plus `@keyframes dy-form-cta-spin { to { transform: rotate(360deg); } }`. **Added 2026-05-14:** `.dy-modal-contents { max-width: 95vw; margin: 0 auto; }` at both `≤958px` and `≤759px` breakpoints; mobile rule also gets `justify-content: start; padding-top: 3em;` and `width: 100%` (was `99%`). See §3.6. |
| **JS** | Source-of-truth JS + 4 additions: state flags, `VARIANT_INFO`, `firePopupCaptureSubmitted` helper, two new call sites. **Added 2026-05-14:** entire JS wrapped in IIFE, two new helpers (`isUserEngaged`, `markUserEngaged`), early-exit suppression at init, two `markUserEngaged` calls after the existing `Popup Capture Submitted` events. SMS error render swapped to `.textContent` (XSS hardening, DEC-012). See §3.7. |

**Quiz selection logic — UNCHANGED.** `quizButtons.forEach(button => button.addEventListener('click', ...))` with `event.currentTarget.classList.add('selected')` is byte-identical to source of truth.

### 3.2 Strategy Pivot History (Tracking Events)

| Strategy | Approach | Outcome |
|---|---|---|
| **A** (abandoned) | Add custom close button to template HTML + hide DY's via CSS (`.smartblock-close-icon, .dy-pop-close, .dy-close-button, [class*="close-icon"], [class*="overlay-close"]`) | Failed: DY's actual close class is `.dy-lb-close`, none of the 5 hide selectors matched. Custom button HTML did not render. |
| **C** (abandoned) | Document-level capture-phase delegated click listener on `.dy-lb-close` + ESC keydown + `beforeunload`, no HTML/CSS changes | Failed: DY unmounts the template before our listeners fire reliably. Even capture phase loses the race against DY's teardown. |
| **Final** (current) | `Popup Capture Submitted` only — fired from inside submit `.then()` blocks where DY has the template fully mounted. `Popup Dismissed` declared out of scope. | ✅ Implementable. Pending QA. |

### 3.3 Verification Plan (Scenario A — Full Conversion)

| Step | Expected `dpx` POST | Key payload fields |
|---|---|---|
| 1. Modal renders | `Question Shown` | `event_category: 'Email Capture Quiz'`, `timestamp` |
| 2. Tap a quiz button | `Question Answer Selected` | `selected_answer: <text>`, `timestamp` |
| 3. Submit valid fresh email | `Email Submitted Successfully` → `Popup Capture Submitted` → `identify-v1` (if `cuid`) | `opt_in_type: 'email_only'`, `reward_granted: 'free_shipping'`, `variant_info: 'V2'` |
| 4. Submit phone | `Popup Capture Submitted` | `opt_in_type: 'email_sms'`, `reward_granted: 'free_shipping_and_discount'`, `variant_info: 'V2'` |
| 5. **(NEW 2026-05-14)** After submit | DevTools → Application → Local Storage → key `mr-email-capture-engaged` present with timestamp + opt_in_type | — |
| 6. **(NEW 2026-05-14)** Reload preview URL | Popup does NOT render. No `Question Shown` event fires. | — |

**Testing gotcha:** if the test email already exists, `response.data.userAlreadyExists === true` triggers an early `return` from the email `.then()` — **no events fire at all** including `Email Submitted Successfully`. Always test with a fresh `+timestamp@…` alias.

**Reset for re-testing:** `localStorage.removeItem('mr-email-capture-engaged')` in DevTools Console, then reload.

### 3.4 Roam Node Structure (canonical paste-ready source)

The roam node `2026-05-13-111704-dotcompb_8211.org` is organized as:

```
* SOURCE OF TRUTH — LIVE DY TEMPLATE [#A]   ← NEVER MODIFY (mostly — see exception note)
  ** SOURCE OF TRUTH — HTML  (verbatim live, in #+begin_src html)
  ** SOURCE OF TRUTH — CSS   (verbatim live + 2026-05-14 .dy-modal-contents update; reflects deployed state)
  ** SOURCE OF TRUTH — JS    (verbatim live — STILL STALE as of 2026-05-14, no IIFE/engagement)
  ** Existing Events Pattern Reference (table)
* TABLE OF CONTENTs :toc:
* ENVIRONMENT
* STEPs TO REPRODUCE (AC1, AC2)
* EXPECTED RESULT / ACTUAL RESULT / WORKING EVENTs / IMPACT
* ROOT CAUSE ANALYSIS
* FIX PLAN  (final scope — Plan Step 4 NEW 2026-05-14 describes engagement suppression; Open Question #5 added for TTL)
* PASTE-READY CODE — DY ADMIN TABS  ← THIS IS THE DEPLOYMENT TARGET
  ** HTML TAB    (patched — with .btn-text/.btn-loader spans)
  ** CSS TAB     (patched — includes .dy-modal-contents width fix + .btn-loader styling + keyframes)
  ** JAVASCRIPT TAB (patched — IIFE-wrapped, engagement helpers, Popup Capture Submitted, markUserEngaged calls)
* STRUCTURE AND FUNCTIONALITY (Flow Diagram + State Flags + Event Payload Schemas — STALE as of 2026-05-14, no engagement-suppression flow)
* DEPLOYMENT NOTEs (Step-by-step, Rollback)
* QA INSTRUCTIONs (Scenarios A–E, only A in scope — STALE, no localStorage verification steps)
* RELEVANT LINKs
* TODO TICKET TASKs [11/15 done]
* NOTEs (3 historical UPDATE entries)
* COMMENTs
```

**Stale sections requiring follow-up:** see §2.4.

### 3.5 Communication Drafted

**For Bre (DY platform owner):** explanation that `Popup Capture Submitted` is implemented and works on both submits; `Popup Dismissed` is not achievable from template JS because DY owns the popup open/close lifecycle. Recommended escalation path: configure a platform-side trigger in the DY admin.

**For Kaila (Marketing) re: engagement suppression (2026-05-14):**

> Hey Kaila — checked both **Email/SMS Capture Modal** and **Email/SMS Capture Modal Booking** in production. Neither has any JS suppression logic: no localStorage, no cookies, no init-time check. Just an in-memory `formSubmitted` flag that resets on reload.
>
> For the new modal, I implemented it at the template-JS layer. Flow:
> 1. First visit: popup renders normally.
> 2. On successful submit: a flag is written to localStorage with timestamp + opt-in type.
> 3. Future visits (within 30 days): the JS reads the flag at init and hides the overlay before anything renders.
> 4. After 30 days: flag expires, popup is eligible again. TTL is configurable.
>
> If the two existing templates also avoid re-showing after a CTA click, it's not from the template code — has to be configured at the DY platform level (audience/frequency cap) or server-side. Worth a check with the DY rep to confirm where.

### 3.6 Responsive Width Fix (NEW 2026-05-14)

**Goal:** add a small horizontal inset on tablet (≤958px) and mobile (≤759px) so the modal does not hug the viewport edges.

**Iteration log (don't repeat the failures):**

| Attempt | What was tried | Why it failed |
|---|---|---|
| 1 | `.mr-email-sms-modal { width: 95% }` at both breakpoints | Close-X stayed at viewport top-right because it's anchored to `.dy-modal-contents` (parent of our template, not parent of `.mr-email-sms-modal`). Visible gap between modal corner and X. |
| 2 | Revert to `width: 100%` + `padding-left/right: 2.5%` + `box-sizing: border-box` on `.mr-email-sms-modal` | Same gap — padding pushed our content inward, but `.dy-modal-contents` (which holds the X) stayed at full width. |
| **Final** | Revert padding hack. Target `.dy-modal-contents` directly: `max-width: 95vw; margin: 0 auto` at both breakpoints. | ✅ Works — `.dy-modal-contents` shrinks, its child elements (our template AND the close-X) move with it. X stays anchored to the modal's top-right corner. |

**Final CSS (applied at both `≤958px` and `≤759px`):**

```css
.dy-modal-contents {
  max-width: 95vw;
  margin: 0 auto;
}
```

Mobile rule (`≤759px`) was additionally adjusted for content collision with the X area:
- `flex-direction: column` (existing)
- `height: 95vh` (existing — note §1.10 caveat about `svh`)
- `justify-content: start` (NEW)
- `padding-top: 3em` (NEW)
- `width: 100%` (was `99%`)

**Why CSS works without JS:** the `<style>` block injected by the DY template is global CSS — it can target `.dy-modal-contents` even though that element lives outside `.dy_unit`.

### 3.7 Engagement Suppression Implementation (NEW 2026-05-14)

**Goal:** popup does not re-show on subsequent visits if user successfully submitted email or SMS within the TTL window.

**JS shape (PASTE-READY → JAVASCRIPT TAB and `sessions/dynamic.js`):**

```js
(function() {
  var ENGAGEMENT_FLAG_KEY = 'mr-email-capture-engaged';
  var ENGAGEMENT_TTL_DAYS = 30;

  function isUserEngaged() {
    try {
      var raw = localStorage.getItem(ENGAGEMENT_FLAG_KEY);
      if (!raw) { return false; }
      var data = JSON.parse(raw);
      if (!data || !data.timestamp) { return false; }
      var ttlMs = ENGAGEMENT_TTL_DAYS * 24 * 60 * 60 * 1000;
      return (Date.now() - data.timestamp) < ttlMs;
    } catch (e) {
      return false;
    }
  }

  function markUserEngaged(optInType) {
    try {
      localStorage.setItem(ENGAGEMENT_FLAG_KEY, JSON.stringify({
        timestamp: Date.now(),
        opt_in_type: optInType
      }));
    } catch (e) {}
  }

  if (isUserEngaged()) {
    var existingTemplate = document.querySelector('.mr-email-sms-modal');
    var overlay = existingTemplate && existingTemplate.closest('.dy-modal-container');
    if (overlay) {
      overlay.style.display = 'none';
    } else if (existingTemplate) {
      existingTemplate.style.display = 'none';
    }
    return;
  }

  // ... existing template logic ...
  // After email submit success, after Popup Capture Submitted event:
  markUserEngaged('email_only');

  // After SMS submit success, after Popup Capture Submitted event:
  markUserEngaged('email_sms');
})();
```

**Flow:**

| Visit | Behavior |
|---|---|
| First (clean browser) | DY renders template → JS reads flag → empty → continues → `Question Shown` fires → user can interact. |
| Successful submit | `Popup Capture Submitted` event fires → `markUserEngaged()` writes `{ timestamp, opt_in_type }` to localStorage. |
| Future visits within TTL | DY still tries to render → JS reads flag → finds it within TTL → hides `.dy-modal-container` (backdrop + content + close-X) → `return` from IIFE. No events fire. |
| After TTL expires | `isUserEngaged()` returns false → popup is eligible again. |

**Key implementation choices:**

| Choice | Reason |
|---|---|
| `localStorage` over cookies | Simpler, no header overhead, no cross-subdomain concerns. Survives session close. |
| 30-day TTL default | Industry default; configurable. Marketing to confirm. |
| Stored value as JSON `{ timestamp, opt_in_type }` | Lets Marketing inspect the flag in DevTools and lets analytics tell `email_only` from `email_sms` if needed later. |
| Hide entire `.dy-modal-container` | Otherwise the grey backdrop renders alone — poor UX. |
| IIFE wrap | `return` lets us early-exit cleanly. Also prevents `let` collisions on re-render. |
| `try/catch` around localStorage | Safari private browsing throws on write — silent fallback means engaged users in private mode see the popup again. Acceptable graceful degradation. |

**Production-template analysis (2026-05-14):** confirmed both `Email/SMS Capture Modal` and `Email/SMS Capture Modal Booking` in prod have **zero** template-level suppression (no localStorage, no cookies, no init-time check, no `DY.API` events at all). The Booking variant differs only by `email_source: 'COLORBAR-dy-ecm'` and `isBookingOfferCode: true` — same suppression-absent pattern. Therefore the Variation 2 implementation is net-new functionality at the template layer.

---

## SECTION 4: FILE INDEX

| Path | Purpose |
|---|---|
| `~/.brain.d/roam-nodes/madison_reed/2026-05-13-111704-dotcompb_8211.org` | **Canonical bug node** — SOURCE OF TRUTH + PASTE-READY CODE + investigation log. Updated 2026-05-14 with .dy-modal-contents CSS + IIFE engagement-suppression JS + Plan Step 4 + Open Question #5. |
| `~/.brain.d/roam-nodes/madison_reed/2026-05-13-115604-dotcompb_7051.org` | Parent epic node — DY: Question Email Capture Template |
| `~/.brain.d/roam-nodes/madison_reed/2026-02-10-074446-dotcompb_7052.org` | Parent story — Email/SMS Capture Modal refactor |
| `~/.brain.d/roam-nodes/madison_reed/2026-05-13-115604-dotcompb_7166.org` | Sibling story — 1st Screen no email |
| `~/.brain.d/roam-nodes/madison_reed/2026-05-13-115604-dotcompb_7167.org` | Sibling story — 2nd Screen SMS |
| `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` | MR index — 8211 in IN PROGRESS, 7051 in IN TODO, 7166/7167 in IN TEST, full BACKLOG |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dy-question-email-capture-epic.md` | **Umbrella session** — epic architecture, event taxonomy, MR bridges, campaign IDs |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dotcompb-7052-dynamic-yield-email-sms.md` | Original 7052 refactor session — full HTML/CSS/JS evolution, ~3.6k lines |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dy-fix-8211/template.html` | On-disk mirror of PASTE-READY HTML |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dy-fix-8211/template.css` | On-disk mirror of PASTE-READY CSS (may be stale vs roam node — verify before use) |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dy-fix-8211/template.js` | On-disk mirror of PASTE-READY JS (may be stale vs roam node — verify before use) |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dy-fix-8211/DEPLOYMENT.md` | Step-by-step DY admin deployment guide (now stale on Strategy A; final scope canonical in roam node) |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dynamic-yield.html` | HTML reference snapshot (pre-fix) |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dynamic.css` | **Working CSS file** — updated 2026-05-14 with `.dy-modal-contents { max-width: 95vw; margin: 0 auto; }` at both breakpoints + mobile `justify-content: start; padding-top: 3em; width: 100%`. Mirrors PASTE-READY CSS. |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dynamic.js` | **Working JS file** — updated 2026-05-14 with IIFE wrap + `isUserEngaged`/`markUserEngaged` helpers + early-exit at init + `Popup Capture Submitted` events + `markUserEngaged` calls. Mirrors PASTE-READY JS. |
| `website/src/vuescripts/mrVueApp.js:687-688` | MR-side: binds `window.createCustomerFromEmailCapture` + `window.addFirstTimeVisitorPhoneAndOffer` |
| `website/src/vuescripts/ssr/SsrApp.vue:239-240` | MR-side: same bindings under SSR path |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last (2026-05-14)

*   **Responsive-width fix iterated 3 times.** Attempt 1 (`width: 95%` on `.mr-email-sms-modal`) and attempt 2 (`padding-left/right: 2.5%` + `box-sizing: border-box`) both broke the close-X position. Browser DOM inspection revealed the close-X is a **sibling** of our template inside `.dy-modal-contents`, not a child. Final fix targets `.dy-modal-contents { max-width: 95vw; margin: 0 auto }` at both tablet (≤958px) and mobile (≤759px) breakpoints — see §3.6 / DEC-009.
*   **Viewport-unit safety audit.** Identified `95vh` as unsafe on iOS Safari (clips behind URL bar) and no safe-area-inset handling for notched devices. Recommended `svh` fallback + `env(safe-area-inset-*)`. User opted not to apply — tracked in §2.4 / DEC-011.
*   **Engagement-based popup suppression — Marketing-driven feature.** Kaila reported DY does not support "don't show after CTA submit" (only dismiss-based). I checked DY admin → confirmed. Then audited both production templates (`Email/SMS Capture Modal` and `Email/SMS Capture Modal Booking`) → confirmed neither has template-level suppression code (only in-memory `formSubmitted`). Implemented localStorage-based suppression for Variation 2: IIFE wrap, `isUserEngaged`/`markUserEngaged` helpers, early-exit at init that hides `.dy-modal-container`, `markUserEngaged` calls after each `Popup Capture Submitted`. TTL = 30 days (configurable). See §3.7 / DEC-010.
*   **Roam node updates.** Updated `LAST_UPDATE` line. Added the .dy-modal-contents CSS to PASTE-READY → CSS TAB (and to SOURCE OF TRUTH → CSS since user had been re-pasting it during iteration). Replaced PASTE-READY → JAVASCRIPT TAB with the IIFE+engagement version. Added FIX PLAN Plan Step 4 (engagement suppression rationale + helpers + integration points). Added Open Questions #5 (TTL).
*   **Working files updated.** `sessions/dynamic.css` and `sessions/dynamic.js` now mirror PASTE-READY. JS also got the SMS error setter swapped to `.textContent` (DEC-012, XSS hardening — security hook prompted).
*   **Slack drafts.** Drafted concise Slack reply to Kaila explaining the implementation flow + flagging that the existing production templates have no template-level suppression so it must be configured elsewhere at DY platform / server level. (Reply text saved in §3.5.)
*   **Roam node stale sections identified.** SOURCE OF TRUTH → JS, STRUCTURE AND FUNCTIONALITY → Flow Diagram, STRUCTURE AND FUNCTIONALITY → State Flags, and QA INSTRUCTIONS → Scenario A still reflect the OLD flow without engagement suppression. Captured in §2.4 as pending.
*   **Session reset performed — this file refreshed.**

### Pending / Not yet started

*   User pastes patched HTML / CSS / JS from roam node PASTE-READY into DY admin → Variation 2 → respective tabs.
*   Save + reload preview URL.
*   QA Scenario A (now extended with steps 5–6 in §3.3 to verify localStorage flag + reload suppression).
*   Optionally apply `svh` + `env(safe-area-inset-*)` to the mobile media query — see §1.10.
*   Marketing confirmation on engagement TTL (30 days default — could be longer).
*   Marketing confirmation on `opt_in_type` / `reward_granted` values.
*   Add `Variant Info` template variable in DY admin (optional, recommended).
*   Publish Variation 2.
*   Bring the 4 stale roam node sections in line with PASTE-READY (see §2.4).
*   Escalate `Popup Dismissed` platform-side trigger to Marketing / DY rep.
*   Close DOTCOMPB-8211 with summary of in-scope work (tracking events + responsive width + engagement suppression) + lifecycle limit on `Popup Dismissed`.

### Where to resume

If the user reports **the fix still does not work after the new HTML/CSS/JS paste**:
- Verify all 3 tabs were pasted AND saved in DY admin.
- Check DevTools Console for any red error after submit click.
- Verify the test email is fresh (`userAlreadyExists: true` triggers silent early return — see §3.3 gotcha).
- Verify localStorage flag — if the user is testing on a browser that already submitted, the popup correctly will NOT render. Clear with `localStorage.removeItem('mr-email-capture-engaged')` and reload.
- Check Network tab: is `/api/customer/createCustomerFromEmailCapture` 200 OK? If not, MR-side API issue.

If the user asks to **bring the 4 stale roam node sections in line** with PASTE-READY: see §2.4. Mirror the IIFE + engagement helpers into SOURCE OF TRUTH → JS, add the "if engaged, hide and bail" branch to Flow Diagram, add the localStorage flag to State Flags, add steps 5–6 to QA Scenario A.

If the user asks to **apply the `svh` / safe-area-inset audit recommendations**: see §1.10 for the exact CSS pattern. Both the working file `sessions/dynamic.css` and the roam node PASTE-READY → CSS TAB need to be updated identically.

If the user asks to **update the umbrella session** (`dy-question-email-capture-epic.md`) with today's findings: candidates are (a) DOM structure finding (§1.8 — applies to ANY DY template, not just this one — strong candidate for extraction into a `dy-templates-architecture.md` memory file), (b) engagement-suppression pattern (§1.9), (c) viewport-unit safety guidance (§1.10).

If the user asks for a **new task** unrelated to 8211: check Section 2.4. Otherwise, treat as a fresh request and route through Section 1 guidelines.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session. Newest row first.

| Datetime         | Duration | Type            | Reference          | Description |
|------------------+----------+-----------------+--------------------+-------------|
| 2026-05-14 13:00 | —        | session-reset   | this               | Compacted 2026-05-14 work — DY overlay DOM discovery, responsive-width fix via .dy-modal-contents, viewport-unit audit, engagement-suppression implementation via localStorage IIFE, production-template analysis, roam node updates, stale-section inventory. Section 5 fully replaced. |
| 2026-05-14 12:30 | 0.5h     | documentation   | DOTCOMPB-8211      | Drafted concise Slack reply to Kaila: confirmed both existing prod templates have no template-level suppression, explained new implementation flow, recommended Marketing check DY admin for where existing suppression lives. |
| 2026-05-14 12:00 | 0.75h    | research        | DOTCOMPB-8211      | Audited both production DY templates (Email/SMS Capture Modal + Email/SMS Capture Modal Booking) line-by-line. Confirmed zero template-level suppression code in either. Booking variant differs only by email_source + isBookingOfferCode. |
| 2026-05-14 11:30 | 1h       | implementation  | DOTCOMPB-8211      | Engagement-based popup suppression — wrapped entire JS in IIFE, added isUserEngaged/markUserEngaged helpers with 30-day TTL, early-exit at init hides .dy-modal-container, markUserEngaged calls after Popup Capture Submitted on both submits. Applied to sessions/dynamic.js + roam node PASTE-READY → JAVASCRIPT TAB. Updated FIX PLAN Plan Step 4 + Open Questions #5 + LAST_UPDATE. SMS error setter swapped to .textContent (DEC-012). |
| 2026-05-14 10:30 | 0.25h    | documentation   | DOTCOMPB-8211      | Updated roam node SOURCE OF TRUTH → CSS section to reflect what user had been re-pasting during iteration (.dy-modal-contents rule). Updated PASTE-READY → CSS TAB similarly. Updated FIX PLAN Plan Step 2 from "no change" to describe the .dy-modal-contents fix and its rationale. |
| 2026-05-14 10:00 | 0.25h    | research        | DOTCOMPB-8211      | Viewport-unit audit — flagged 95vh on mobile as unsafe (iOS Safari clip), no env(safe-area-inset-*) for notched devices. Documented recommended pattern. User opted not to apply this session (DEC-011). |
| 2026-05-14 09:30 | 1h       | bug-fix         | DOTCOMPB-8211      | Responsive-width fix — iterated 3 attempts (95% on modal, padding on modal, then finally .dy-modal-contents max-width: 95vw). Browser DOM inspection revealed close-X is sibling of template, not child. Final CSS targets .dy-modal-contents at both ≤958px and ≤759px breakpoints. Mobile also gets justify-content: start + padding-top: 3em. DEC-009. |
| 2026-05-13 17:45 | —        | session-reset   | this               | Compacted full day's iteration on DY tracking-event bug — Strategy A/C pivot, final scope, toggleLoader/btn-text root cause, source-of-truth pattern, communication drafts. Section 5 fully replaced. |
| 2026-05-13 16:50 | 0.5h     | bug-fix         | DOTCOMPB-8211      | Root cause for "GET FREE SHIPPING does nothing" found: HTML missing `.btn-text` / `.btn-loader` spans → `toggleLoader` throws → submit handler aborts. Patched PASTE-READY HTML + CSS in roam node. JS unchanged. SOURCE OF TRUTH untouched. |
| 2026-05-13 16:30 | 0.5h     | documentation   | DOTCOMPB-8211      | Drafted polite + concise reply for Bre re: DY lifecycle limit on `Popup Dismissed`. Also drafted condensed eng-team channel update for Carley/Marketing visibility. |
| 2026-05-13 16:00 | 0.5h     | refinement      | DOTCOMPB-8211      | Reset roam node JS to source-of-truth + only 2 inline `DY.API('event', ...)` call sites (no helper functions, no state flags refactor, no quiz delegation). Restored event payload schemas to current scope. Added SOURCE OF TRUTH section at top of roam node (HTML / CSS / JS in `#+begin_src` blocks, marked NEVER MODIFY). |
| 2026-05-13 14:30 | 1.5h     | refinement      | DOTCOMPB-8211      | Strategy C trial: capture-phase document click listener on `.dy-lb-close` + ESC keydown + `beforeunload`. Tested on live preview. Confirmed unimplementable — DY unmount races against our listener every time. Final scope landed: `Popup Capture Submitted` only, `Popup Dismissed` out of scope. Updated roam node FIX PLAN, QA scenarios B–E marked NOT IMPLEMENTED. |
| 2026-05-13 13:20 | 1h       | refinement      | DOTCOMPB-8211      | Strategy A trial: custom close button + hide DY's via 5-selector aggressive CSS. Console diagnostic confirmed DY's actual close class is `.dy-lb-close` (single class, not in our hide list). Custom button never rendered in DOM (`document.getElementById('dy-template-close') === null`). Pivoted to Strategy C. |
| 2026-05-13 12:30 | 0.5h     | documentation   | DOTCOMPB-8211      | Built paste-ready DY assets at `sessions/dy-fix-8211/` (template.html, template.css, template.js, DEPLOYMENT.md). Initially Strategy A. Added all three as `#+begin_src` blocks in roam node PASTE-READY CODE section. |
| 2026-05-13 12:00 | 1h       | implementation  | DOTCOMPB-8211      | Created umbrella session `dy-question-email-capture-epic.md` — full epic architecture, event taxonomy (6 events), MR-side bridges (`createCustomerFromEmailCapture`, `addFirstTimeVisitorPhoneAndOffer`), variation split + campaign IDs, decision log (8 DECs). Inheritance chain established: umbrella ← per-ticket sessions. |
| 2026-05-13 11:56 | 0.5h     | implementation  | DOTCOMPB-7051/7166/7167 | Backfilled 3 missing roam nodes: 7051 (epic), 7166 (sibling story), 7167 (sibling story owns Popup Capture Submitted spec). Cross-linked all 4 family nodes (7051, 7052, 7166, 7167, 8211) bidirectionally via `[[id:UUID]]`. Updated MR index: IN TODO `[0/1]`, IN TEST `[0/7]`, BACKLOG `[23/44]`. |
| 2026-05-13 11:17 | 0.5h     | implementation  | DOTCOMPB-8211      | Created bug roam node (`2026-05-13-111704-dotcompb_8211.org`, UUID `6f2c6e99-…`). Added to MR index IN PROGRESS `[0/6]` lane + BACKLOG. Original session file scaffolded with Section 1-6 structure. |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
