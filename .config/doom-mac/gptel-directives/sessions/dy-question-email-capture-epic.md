<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the entire **DY: Question Email Capture Template** program (JIRA epic DOTCOMPB-7051). It is the architectural baseline that every per-ticket session in this family **inherits from**. Loaded at the start of any conversation that touches the DY Email/SMS Capture Modal, Variation 1, Variation 2, or any of the six `dpx` tracking events.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, patterns, conventions for all DY template work. | Before any DY template task. |
| **2. Epic Overview** | Scope, child tickets, status matrix. | When orienting a new session. |
| **3. Template Architecture** | HTML, CSS, JS contract of the live Variation 2 template. | When editing the DY template. |
| **4. Event Taxonomy** | All six `dpx` events — names, triggers, payloads, current status. | Any tracking work. |
| **5. MR-side Bridges** | Global functions the DY template calls into (`createCustomerFromEmailCapture`, `addFirstTimeVisitorPhoneAndOffer`). | When wiring a new submit handler. |
| **6. Variation Split & Campaign IDs** | smartId, expId, variationId; V1 vs V2 differences. | When loading the preview or DY admin. |
| **7. Child Session Index** | Per-ticket session files that inherit from this one. | When picking up a child ticket. |
| **8. Activity Log** | Datetime-stamped, append-only audit trail. | When you need exact "what was done when". |

**Operational Rule:** Always look for the last request identified by `###` title. Load relevant skills and apply Section 1 rules.

**Architectural baseline:** This is the **root** of the DY: Question Email Capture session family. It pulls in everything previously captured in `dotcompb-7052-dynamic-yield-email-sms.md` (the 7052 refactor session, ~3,600 lines) and reorganizes it as a stable architectural reference rather than a session timeline.

**Cross-session references** use `[session: dy-question-email-capture-epic > section-N.M]` syntax — see `~/.claude/skills/session-memory/rules/reference-syntax.md`.

**Roam nodes:**
- Epic: `~/.brain.d/roam-nodes/madison_reed/2026-05-13-115604-dotcompb_7051.org` (ID `923885f6-1210-4307-bbe9-069e4d700491`)
- 7052: `~/.brain.d/roam-nodes/madison_reed/2026-02-10-074446-dotcompb_7052.org` (ID `ae7c3125-b957-4486-a029-3954e8760f81`)
- 7166: `~/.brain.d/roam-nodes/madison_reed/2026-05-13-115604-dotcompb_7166.org` (ID `cf9946a3-27e1-44e0-bb49-62a3c6c58dc9`)
- 7167: `~/.brain.d/roam-nodes/madison_reed/2026-05-13-115604-dotcompb_7167.org` (ID `4dd472c8-a7a6-4614-b63e-6c7a1d095908`)
- 8211 (bug): `~/.brain.d/roam-nodes/madison_reed/2026-05-13-111704-dotcompb_8211.org` (ID `6f2c6e99-6cd6-407c-b98a-04d790f5e044`)
- XXXX (fast-follow — draft, ticket TBD): `~/.brain.d/roam-nodes/madison_reed/2026-05-21-120000-dotcompb_xxxx-dy-quiz-buttons-dynamic.org` (no UUID yet)

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every DY template task in this family.** Loaded skills: `mr-dotcom-dev` (Vue/Vuex for any MR-side bridge work), `mr-roam-node` (ticket documentation), `tophat-tools` (for the rare case the template needs CMS metadata), `pr-scribe` (PR body authoring if MR-side changes are needed).
>
> Most DY template work happens **inside the Dynamic Yield admin** (`https://adm.dynamicyield.com/`), not in the MR repo. The template editor expects HTML + CSS + JS pasted into three tabs. **MR repo PRs are needed only when a global JS helper or Vuex action must change to support a new template behavior.**

### 1.1 — DY Template Contract

A DY template is composed of three editable sources in the DY admin:

| Source | Purpose | File mirror in this dir |
|---|---|---|
| **HTML** | Layout. Allows `${Variable Name}` placeholders bound to template variables. | `dynamic-yield.html` |
| **CSS** | Styling. MR utility classes (`mr-btn`, `max-at-tweak`, `color-mr-purple`, etc.) are **NOT auto-available** — anything used in the template must be hand-rolled or pasted in. | `dynamic.css` |
| **JS** | Behavior. Runs after the template is injected into the page. Has access to the global `DY.API` and any window-scoped MR globals (`window.createCustomerFromEmailCapture`, `addFirstTimeVisitorPhoneAndOffer`). | `dynamic.js` |

**Template variables** are declared in the DY admin and substituted at render time. The current Variation 2 template uses:

- *Ungrouped:* `Mobile Image`, `Desktop Image`, `Email Capture Offer Code`, `Image Alt Text`
- *Email Capture:* `Email Capture Title`, `Email Capture Text`, `Email Capture CTA`, `Email Capture Bottom Text`, `Email Capture Discount Text`
- *SMS Capture:* `SMS Capture Title`, `SMS Capture Text`, `SMS Capture CTA`, `SMS Capture Bottom Text`, `SMS Capture Offer Code`
- *Quiz:* `Quiz Button 1 Text` … `Quiz Button 5 Text`
- *Legal:* `Terms and Conditions URL`, `Privacy Policy URL`

`.optional[data-text=""]` CSS rule hides any element whose data-text attribute is empty — lets the marketer omit non-essential copy without breaking layout.

### 1.2 — MR Utility Classes Used in the Template

The template body relies on MR design system utility classes. These are *baked into* the template HTML by Cristian during refactor — they are not auto-available in DY:

- **Layout:** `.flex`, `.flex-col`, `.flex-1`, `.full-width`, `.full-height`, `.space-between`, `.gap-sm`, `.overflow-hidden`, `.overflow-auto`
- **Spacing:** `.py-100m`, `.py-150m`, `.py-200m`, `.px-100m`, `.pl-200m`, `.mt-50m`, `.mt-100m`, `.mb-50m`, `.mb-100m`, `.mb-150m`
- **Typography:** `.f-domaine-display-condensed`, `.f-primary`, `.upper`, `.bold`, `.xs-f-*`, `.lg-f-*`, **`.max-at-tweak` (mandatory on every responsive font class)**
- **Color:** `.color-mr-purple`, `.color-mr-black`, `.color-font-error`
- **Visibility:** `.xs-hide`, `.sm-hide`, `.md-block`, `.hide`
- **Borders / radius:** `.border-radius-12`, `.btn-block`, `.mr-btn`
- **Alignment:** `.text-center`, `.text-left`

> **Why this matters:** when editing the template, never assume an MR utility "just works." Verify the class is present in the pasted CSS of the template, or add the rule inline.

### 1.3 — DY Event Publishing Pattern

All custom events use one signature:

```js
DY.API('event', {
  name: 'Event Name',
  properties: { /* event-scoped key/values */ }
});
```

- The event becomes a `POST` to `https://async-px.dynamicyield.com/dpx`.
- Names are spec'd verbatim by Marketing — **never rename a fired event**. Marketing builds dashboards against the exact string.
- Property keys spec'd as `snake_case` (`event_category`, `selected_answer`, `email_submitted`, `quiz_selection`, `opt_in_type`, `reward_granted`, `variant_info`).
- The `identify-v1` event (special, fires after email submit) carries the customer's `cuid` and is required for Marketing to stitch the visitor into a customer record.

### 1.4 — ADA / Accessibility Baseline

AC4 of all three child stories (7052, 7166, 7167) is identical: desktop/tablet/mobile responsive, keyboard-focusable, screen-reader-labeled. The template already satisfies this:

- `.quiz-btn` is a real `<button>` — keyboard and SR accessible by default.
- Form inputs have `type="email"` / `type="tel"` and visible `placeholder` — screen readers still need a label; verify `aria-label` if changing inputs.
- Legal links inside `Bottom Text` are upgraded by `replaceLegalTextWithLinks()` with `aria-label="Read our Terms and Conditions"` / `Privacy Policy`.
- Color contrast: `color-mr-purple` (`#3A2D4A`) on `#DAD8E4` lavender background — passes WCAG AA for text.

### 1.5 — No Repo PR by Default

A new ticket in this family produces an updated DY template (HTML/CSS/JS paste) and **no MR PR** unless either:
1. A new global window function is needed (e.g., a new MR-side opt-in helper).
2. A Vuex action or CMS partial must change.
Treat MR repo changes as the exception, not the default.

---

## SECTION 2: EPIC OVERVIEW

### 2.1 — Scope

Multi-screen Dynamic Yield popup that engages new visitors with a question prompt, captures email, optionally captures SMS, and rewards opt-ins. Six `dpx` tracking events power Marketing's funnel analysis.

### 2.2 — Child Tickets (Status as of 2026-05-13)

| Key | Type | Status | Role | Roam node |
|---|---|---|---|---|
| **DOTCOMPB-7051** | Epic | Abierta | Umbrella | `2026-05-13-115604-dotcompb_7051.org` |
| **DOTCOMPB-7052** | Historia | In Test | 1st Screen: Prompt a Question + Email (Variation 2) | `2026-02-10-074446-dotcompb_7052.org` |
| **DOTCOMPB-7166** | Historia | Pruebas | 1st Screen: Prompt a Question + No Email (Variation 1) | `2026-05-13-115604-dotcompb_7166.org` |
| **DOTCOMPB-7167** | Historia | In Test | 2nd Screen: SMS Capture (owns `Popup Capture Submitted` spec) | `2026-05-13-115604-dotcompb_7167.org` |
| **DOTCOMPB-8211** | Error | En curso | Bug — `Popup Dismissed` + `Popup Capture Submitted` not firing on V2 | `2026-05-13-111704-dotcompb_8211.org` |

### 2.3 — Cross-Story Spec Overlap

| Spec | Owned by | Mirrored in |
|---|---|---|
| AC1 — Configurable content & styling | 7052 | 7166, 7167 (all three are identical except for the screen they configure) |
| AC2 — User interaction (question + advance + dismiss) | 7052, 7166 | 7167 has its own AC2 covering capture logic |
| `Popup Dismissed` event spec | 7052 §2, 7166 §2 | (Same wording in both stories) |
| `Popup Capture Submitted` event spec | 7167 §3 | (Only owner) |
| `Question Shown` + `Question Answer Selected` event specs | 7052 §3, 7166 §3 | (Both stories require the same events on the 1st screen) |
| AC4 — Responsiveness & Accessibility | 7052, 7166, 7167 | (Identical wording in all three) |

### 2.4 — Decisions (Cumulative)

- **DEC-001 — DY admin owns close-X button.** The HTML mirrored in `dynamic-yield.html` has **no close-X element** of its own. The platform's overlay chrome paints the X. Any `Popup Dismissed` event must hook into either (a) the DY platform's overlay-close API, or (b) a click listener on the platform-injected close button via selector (TBD which selector — needs DY-admin inspection).
- **DEC-002 — `Email Submitted Successfully` keeps its name.** Marketing built a dashboard against this exact string. Do *not* rename it to align with the 7167 `Popup Capture Submitted` spec — the two events serve different funnels (email step vs. SMS step) and must remain distinct.
- **DEC-003 — `identify-v1` is mandatory after email submit.** When `response.data.customer.cuid` is present, fire `DY.API('event', { name: 'identify-v1', properties: { cuid } })` so visitors get stitched into the customer record.
- **DEC-004 — `variant_info` is "if applicable".** Omit when no A/B variant is in flight at the event level. Since Variation 2 of the template *is* a variant, include `variant_info: 'V2'` (or the analytic-friendly label Marketing wants — confirm with Marketing before going live).
- **DEC-005 — `formSubmitted` is the single-source-of-truth gate.** The template uses one `formSubmitted` flag shared between the email submit handler, the SMS submit handler, and `toggleQuizButtons`. This prevents double submission across the two-step flow.
- **DEC-006 — 2s setTimeout for the email→SMS transition.** After a successful email submit, the email content fades for 300ms (via `is-fading-out` opacity transition) then hides, with a 2-second hold on the "Thank You" before SMS view appears. This timing was tuned during the 7052 refactor; do not shorten without UX review.
- **DEC-007 — Legal text is upgraded post-render.** `replaceLegalTextWithLinks()` runs once at template init and rewrites the phrase "Terms and Conditions" / "Privacy Policy" inside both bottom-text elements with real anchors. Required because DY template variables are plain text.
- **DEC-008 — Reuse, don't rewrite.** The 7052 refactor goal stated explicitly: "to not modify everything, we can reuse logic already used in here." The form-submit handlers and MR-side bridges (`createCustomerFromEmailCapture`, `addFirstTimeVisitorPhoneAndOffer`) were inherited from the older email-only template.

### 2.5 — Pending Work (Cumulative)

- [X] **8211** — `Popup Dismissed` — **NOT ACHIEVABLE** from template JS. DY owns popup open/close lifecycle; template unmounts before any handler can fire. Escalated out of scope. See 8211 roam node NOTEs → "Final scope: Popup Dismissed is not achievable".
- [ ] **8211** — Paste final JS into DY admin → Variation 2 → JS tab (`Popup Capture Submitted` wired for email + SMS submit). Blocked on Marketing confirmation: `opt_in_type` / `reward_granted` string values + `Variant Info` template variable (value `V2`). Paste-ready JS: `sessions/dy-fix-8211/template.js`.
- [ ] **DOTCOMPB-XXXX (fast-follow — ticket TBD)** — Quiz buttons: add `class="quiz-btn optional"` + `data-text="${Quiz Button N Text}"` to all 5 `<button>` elements in the HTML tab. Leverages existing `.optional[data-text=""]` CSS rule — no JS or CSS changes needed. Roam node draft: `~/.brain.d/roam-nodes/madison_reed/2026-05-21-120000-dotcompb_xxxx-dy-quiz-buttons-dynamic.org`. Mirror updated: `sessions/dynamic-yield.html`. Requested by Kaila Rudolph 2026-05-21.
- [ ] **7166 / 7167** — Move from Pruebas / In Test to Done once 8211 deploys (all working events green).
- [ ] **Long-term** — Decide whether `Email Submitted Successfully` should be renamed once Marketing's dashboard catches up to `Popup Capture Submitted` schema (OUT OF SCOPE per DEC-002).

---

## SECTION 3: TEMPLATE ARCHITECTURE (VARIATION 2 — LIVE)

### 3.1 — HTML Skeleton

The full Variation 2 HTML is in `dynamic-yield.html`. Two-pane layout: left = hero image (md+), right = scrollable content with two stacked sections (`#dy-email-capture-wrap` shown by default, `#dy-sms-capture-wrap` hidden behind `.hide`).

| Element ID | Role |
|---|---|
| `.mr-email-sms-modal` | Outer modal container — flex row, 957×603 min, lavender bg `#DAD8E4` |
| `.left-wrap` | Hero image pane — 70% width desktop, hidden on xs/sm |
| `.right-wrap` | Content pane — 50% width desktop, scrollable |
| `#dy-email-capture-wrap` | Email step container — visible by default |
| `#dy-email-capture-email-title` | H1 — "WHAT LOOK ARE YOU HOPING TO ACHIEVE?" |
| `.quiz-buttons-wrap` → `.quiz-btn × 5` | Five pill buttons (Color Hair, Cover Roots, Add Highlights, Temporarily Conceal Roots, Temporarily Change Hair Color) |
| `#dy-email-capture` | Email form — input + submit |
| `#dy-email-capture-form-wrap` | Email form wrapper — hidden after success |
| `#dy-email-capture-error` | Error message slot — toggled by `.hide` class for 2s |
| `#dy-email-capture-thank-you` | "Your Offer has been applied. Thank you!" — revealed after success |
| `#dy-email-capture-bottom-text` | Legal text — upgraded by `replaceLegalTextWithLinks()` |
| `#dy-sms-capture-wrap` | SMS step container — hidden, revealed after email submit |
| `#dy-sms-capture` | SMS form — phone input + submit |
| `#dy-sms-capture-phone-input` | Phone input — auto-formatted by `formatPhoneNum()` on every keystroke |
| `#dy-sms-capture-error` | SMS error slot — same hide/show pattern |
| `#dy-sms-capture-thank-you` | SMS thank-you message |
| `#dy-sms-capture-bottom-text` | SMS legal text — same upgrade pass |

### 3.2 — CSS Highlights

Full CSS in `dynamic.css`. Notable rules:

- **`.mr-email-sms-modal`** — 957px desktop, 100% under 958px, column flex + 95vh under 760px.
- **`.optional[data-text=""]`** — hides any element whose `data-text` is empty (lets marketer omit copy).
- **`.quiz-btn`** — pill (`border-radius: 100px`), white bg + dark purple border, inverts on hover/`.selected`.
- **`.form-cta`** — pill bg, 80% width, contains inline `.btn-text` + `.btn-loader` (spinner).
- **`.is-fading-out`** — `opacity: 0` for the 300ms email→SMS fade.
- **Mobile (`max-width: 759px`)** — modal becomes column, quiz buttons + form become 100% width, 95vh height with 3em top padding.

### 3.3 — JS Behavior Map

Full JS in `dynamic.js`. Top-down:

1. **DOM caching (lines 1–21)** — cache every interactive ID. Static `formSubmitted` + `selectedQuizOption` state.
2. **`toggleLoader(button, text, loader, show)`** — toggles spinner / button-text visibility and disables the button.
3. **`toggleQuizButtons(disable)`** — gates the quiz buttons during async submit.
4. **`replaceLegalTextWithLinks()`** — rewrites "Terms and Conditions" / "Privacy Policy" with real anchors using `${Terms and Conditions URL}` / `${Privacy Policy URL}` template variables.
5. **`transitionToSmsView()`** — adds `.is-fading-out` then `.hide` to email wrap after 300ms, reveals SMS wrap.
6. **Init** (line 74) — `replaceLegalTextWithLinks()` + first `DY.API('event', { name: 'Question Shown', ... })`.
7. **Quiz click listener (84–100)** — clears `.selected` on all, marks tapped one, captures `selectedQuizOption`, fires `Question Answer Selected`.
8. **Email submit (102–174)** — gates with `formSubmitted`, toggles loader + quiz, calls `window.createCustomerFromEmailCapture()`, on success: handles `userAlreadyExists`, hides top/bottom content, shows thank-you, schedules `transitionToSmsView()` (2s), fires `Email Submitted Successfully`, fires `identify-v1` if `cuid` present.
9. **`formatPhoneNum(input)`** (176–196) — formats raw digits into `(123) 456-7890` (US) or `+CC XXX XXX XX XX` (international).
10. **SMS phone-input listener (198–200)** — reformats on every keystroke.
11. **SMS submit (202–231)** — gates with `formSubmitted`, calls `addFirstTimeVisitorPhoneAndOffer()`, on success hides the form + shows thank-you, on error checks `USER_ALREADY_EXISTS` / `PHONE_ALREADY_SUBSCRIBED`.

### 3.4 — Critical Gaps (Source of the 8211 Bug)

- **No close-X handler exists in `dynamic.js`.** Search results: zero references to "close", "dismiss", or "X". The DY platform paints the close button as part of its overlay chrome — the template body never sees that click. To fire `Popup Dismissed`, we need to either (a) intercept the platform-injected close button via `document.querySelector` on the DY overlay close selector, or (b) use a DY platform API like `DY.API('overlay', 'close', callback)` if one exists. **TBD — needs DY admin inspection during 8211 work.**
- **SMS submit success does NOT fire `Popup Capture Submitted`.** The `.then((response) => { ... })` on line 214 reveals the thank-you but emits zero DY events. Fix: add a `DY.API('event', { name: 'Popup Capture Submitted', properties: { opt_in_type, reward_granted, variant_info } })` call inside the success branch. Determine `opt_in_type` by checking whether the email submit step was reached — `email_sms` if both forms were submitted, `email_only` if the user dismissed before SMS.

---

## SECTION 4: EVENT TAXONOMY

### 4.1 — All Six Events

| # | Event Name | Trigger (current Variation 2) | Required Props | Status on V2 |
|---|---|---|---|---|
| 1 | `Question Shown` | Template init (line 76 of `dynamic.js`) | `event_category: 'Email Capture Quiz'`, `timestamp` (ISO) | ✅ Firing |
| 2 | `Question Answer Selected` | Quiz button click (line 91) | `event_category: 'Email Capture Quiz'`, `selected_answer`, `timestamp` | ✅ Firing |
| 3 | `Email Submitted Successfully` | Email submit success (line 138) | `event_category: 'Email Capture Quiz'`, `email_submitted`, `quiz_selection` | ✅ Firing |
| 4 | `identify-v1` (DY platform event) | After email submit, if `cuid` in response (line 148) | `cuid` | ✅ Firing — DY platform consumes this for identity stitching |
| 5 | **`Popup Dismissed`** | User clicks close-X | `event_category`, `timestamp` (per 7052/7166 §2 spec) | ❌ **MISSING** — no handler in template |
| 6 | **`Popup Capture Submitted`** | User submits email-only or email + SMS (per 7167 §3) | `opt_in_type` (`email_only` vs `email_sms`), `reward_granted`, `variant_info` (if applicable) | ❌ **MISSING** — SMS submit fires no DY event |

### 4.2 — Event Verification Workflow

1. Open the preview URL (Section 6.3).
2. Chrome DevTools → Network → filter input = `dpx`.
3. Each user action should produce exactly one `POST` to `https://async-px.dynamicyield.com/dpx`.
4. Inspect the request payload — DY wraps the `name` and `properties` inside the platform's standard envelope.

### 4.3 — `event_category` Convention

All quiz-flow events use `event_category: 'Email Capture Quiz'`. Keep this string stable — Marketing's segmentation filters on it.

---

## SECTION 5: MR-SIDE BRIDGES (Globals the Template Calls)

The DY template runs in the visitor's browser inside the MR site, so it can call any function attached to `window`. Two MR-side globals are critical:

### 5.1 — `window.createCustomerFromEmailCapture(payload)`

**Signature (observed from `dynamic.js`):**
```js
window.createCustomerFromEmailCapture({
  email: 'user@example.com',
  email_source: 'dy-email-capture-modal',
  offerCode: '${Email Capture Offer Code}',
  isBookingOfferCode: false,
  skipPassword: true
}).then(response => /* ... */);
```

**Resolution payload (observed):**
- `response.data.userAlreadyExists: boolean` — when true, show error and bail.
- `response.data.offerApplied: boolean` — when true, reveal `#dy-email-capture-thank-you`.
- `response.data.customer.cuid: string` — present after first-time customer creation; triggers `identify-v1`.

**Where it lives in MR:** TBD — needs grep in `website/src/`. Likely a Vuex action exposed via `window` for the DY template's benefit, or a `mr_modules/controllers/lib/customer/*.js` controller wrapped through a webservice.

### 5.2 — `addFirstTimeVisitorPhoneAndOffer(payload)`

**Signature:**
```js
addFirstTimeVisitorPhoneAndOffer({
  phone: '+15555551234',
  offerCode: '${SMS Capture Offer Code}'
}).then(response => /* ... */);
```

**Resolution payload:**
- `response.data.offerApplied: boolean` — when true, reveal `#dy-sms-capture-thank-you`.

**Error codes handled:** `USER_ALREADY_EXISTS`, `PHONE_ALREADY_SUBSCRIBED` (response message rendered inline).

**Where it lives in MR:** TBD — same investigation needed. Likely a sibling helper to `createCustomerFromEmailCapture`.

### 5.3 — `DY.API` (Platform Global)

`DY.API('event', { name, properties })` is the only DY platform call used by the template. Other DY API surface area (segmentation, audience targeting, A/B variation selection) is configured in the DY admin, not invoked from the template JS.

---

## SECTION 6: VARIATION SPLIT & CAMPAIGN IDS

### 6.1 — Campaign Metadata

| Field | Value |
|---|---|
| Campaign name | `Test Email/SMS Capture Modal` |
| smartId | `1129717` |
| expId | `2667345` |
| Variation 2 variationId | `21887493` |
| Preview key | `659256927` |
| Current traffic split | Variation 2 = 100% in QA A/B |

### 6.2 — Variation 1 vs Variation 2

| | Variation 1 (control) | Variation 2 (live) |
|---|---|---|
| 1st screen | Older email-only template | Question prompt + email |
| Quiz buttons | None | 5 quiz buttons |
| Email step | Yes (single screen) | Yes (after quiz answer) |
| SMS step | No | Yes (after email) |
| Owned by | (Legacy, pre-epic) | DOTCOMPB-7052 |
| Sibling no-email variant | — | DOTCOMPB-7166 |

### 6.3 — Preview URLs

**Variation 2 (live):**
```
https://www.qa.mdsnrd.com/?dyExperienceId=2667345&dyIsDraft=false&dyIsPreview=true&dyPersistSession=true&dypPreviewKey=659256927&dySmartId=1129717&dyTemp=true&dyVariationId=%5B21887493%5D
```

**DY admin:** `https://adm.dynamicyield.com/` → Campaigns → `Test Email/SMS Capture Modal`.

---

## SECTION 7: CHILD SESSION INDEX

| Session | Inherits from | Ticket | Status |
|---|---|---|---|
| `dotcompb-7052-dynamic-yield-email-sms.md` | — (original DY work) | DOTCOMPB-7052 | In Test — template refactor complete |
| `dotcompb-8211-dy-tracking-events-bug.md` | this file | DOTCOMPB-8211 | En curso — fixing missing events |
| `dy-question-email-capture-epic.md` (this) | — (root) | DOTCOMPB-7051 | Active reference for all DY: Question Email Capture work |

When opening a new session for any DY: Question Email Capture work, the inheritance chain is: **this file → the per-ticket session file**.

---

## SECTION 8: ACTIVITY LOG

| Datetime         | Duration | Type            | Reference        | Description |
|------------------+----------+-----------------+------------------+-------------|
| 2026-05-21 12:00 | —        | session-reset   | this             | Session reset: quiz button fast-follow documented; pending work + roam nodes updated |
| 2026-05-21 12:00 | 1h       | documentation   | DOTCOMPB-XXXX    | Analyzed quiz button dynamic count request (Kaila); identified optional+data-text fix; updated dynamic-yield.html mirror; created roam node draft |
| 2026-05-13 11:56 | —        | other           | this             | Activity Log bootstrapped on 2026-05-21 reset; prior rows back-filled as best-effort below |
| 2026-05-13 11:56 | —        | session-reset   | this             | Umbrella session created; architecture/event taxonomy/bridges/campaign IDs compacted from 7052 session + template snapshots |
| 2026-05-13 11:56 | —        | documentation   | DOTCOMPB-7051    | Roam nodes created for 7051 (UUID 923885f6-…), 7166 (UUID cf9946a3-…), 7167 (UUID 4dd472c8-…); 8211+7052 nodes cross-linked |
| 2026-05-13 11:17 | —        | documentation   | DOTCOMPB-8211    | Roam node created (UUID 6f2c6e99-…); bug session dotcompb-8211-dy-tracking-events-bug.md scaffolded |
| 2026-02-10 07:44 | —        | documentation   | DOTCOMPB-7052    | Roam node created (UUID ae7c3125-…); original DY template refactor session opened |

<!-- DESCRIPTION AND USER CONTEXT END -->
<!-- INIT OF THE USER PROMPT END -->
