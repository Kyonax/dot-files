# DOTCOMPB-8211 — DY Template Fix Deployment Guide

**Target:** Dynamic Yield admin (`https://adm.dynamicyield.com/`)
**Campaign:** `Test Email/SMS Capture Modal` (smartId `1129717`, expId `2667345`)
**Variation:** Variation 2 (variationId `21887493`)

This is a **DY-platform-only change.** No MR repo PR is needed.

---

## 1. Prerequisites — Add Template Variable

In the DY admin template editor, **add one new template variable** under the existing variable groups:

| Variable name | Type | Default value (V2) | Default value (V1) |
|---|---|---|---|
| `Variant Info` | String | `V2` | `V1` |

This populates the `variant_info` property on the four new event payloads. If left blank, the property is omitted (matches the "if applicable" wording in the AC).

---

## 2. Paste-Ready Files

Replace the contents of each DY editor tab with the matching file from this folder:

| DY editor tab | File in this folder |
|---|---|
| **HTML** | `template.html` |
| **CSS** | `template.css` |
| **JavaScript** | `template.js` |

### HTML diff summary
- Adds one new element: `<button id="dy-template-close" class="dy-template-close" ...>` inside `.mr-email-sms-modal`, before `.left-wrap`. Contains an SVG ✕ icon with `aria-label="Close popup"`.
- No other markup changes.

### CSS diff summary
- Adds `position: relative;` to `.mr-email-sms-modal` (so the new close button can absolute-position inside it).
- Adds `.dy-template-close` rules: 32×32 absolute top-right, transparent bg, hover bg `rgba(58, 45, 74, 0.1)`, focus-visible outline for ADA.
- Adds a "hide DY platform close button" rule: targets `.smartblock-close-icon`, `.dy-pop-close`, `.dy-close-button`, plus any class containing `close-icon` or `overlay-close`. Belt-and-suspenders — DY's class name varies by account.
- Adds mobile-breakpoint adjustment for the close button (top 8px / right 8px under 760px).

### JavaScript diff summary
- **New state flags** at top: `userConverted`, `popupDismissedFired`, `popupCaptureEmailFired`, `popupCaptureSmsFired`. Prevent double-fire.
- **New constants:** `VARIANT_INFO` reads the `${Variant Info}` template variable; `EVENT_CATEGORY = 'Email Capture Quiz'` consolidates the string used by Question/Email events.
- **New helper `withVariant(props)`:** merges `variant_info` into a properties object only when `VARIANT_INFO` is non-empty (and not still the unsubstituted `${Variant Info}` placeholder).
- **New helper `firePopupDismissed()`:** fires `Popup Dismissed` with `event_category`, `timestamp`, and `variant_info` (if applicable). Guards against double-firing and skips entirely if `userConverted` is true.
- **New helper `firePopupCaptureSubmitted(optInType, rewardGranted)`:** fires `Popup Capture Submitted` with `opt_in_type`, `reward_granted`, `timestamp`, and `variant_info` (if applicable). Sets `userConverted = true` so a subsequent dismiss does not fire `Popup Dismissed`.
- **Three new listeners for dismissal detection:**
  1. Click on the new `#dy-template-close` button — fires `Popup Dismissed` then hides the modal via `window.DYO.closeAllVariations()` (DY's platform close API) with a CSS-display fallback.
  2. ESC keydown on `document` — fires `Popup Dismissed` (does not visually close — DY's overlay still handles ESC if configured).
  3. `beforeunload` on `window` — fires `Popup Dismissed` when user navigates away without converting.
- **`Question Shown`, `Question Answer Selected`** event payloads wrapped with `withVariant(...)`.
- **Email submit success path** now also calls `firePopupCaptureSubmitted('email_only', 'free_shipping')` after `Email Submitted Successfully`.
- **SMS submit success path** now calls `firePopupCaptureSubmitted('email_sms', 'free_shipping_and_discount')` after the success `.then()`.
- **All existing logic preserved** — same `formSubmitted` gating, same `transitionToSmsView`, same error handling, same `identify-v1` event.

---

## 3. Deployment Order

1. **Open DY admin** → Campaigns → `Test Email/SMS Capture Modal`.
2. **Variation 2** (variationId `21887493`) → Edit template.
3. **Add the `Variant Info` template variable** (Section 1 above). Set value to `V2`.
4. **Paste HTML** from `template.html` into the HTML tab.
5. **Paste CSS** from `template.css` into the CSS tab.
6. **Paste JavaScript** from `template.js` into the JS tab.
7. **Save as draft** — do NOT publish yet.
8. **Verify on preview URL** (see Section 4 below).
9. **Publish** once all six events fire correctly.

**Repeat for Variation 1** (DOTCOMPB-7166) with `Variant Info = V1` if Marketing wants `variant_info` populated on V1 as well — V1 currently does not have `Popup Capture Submitted` spec'd, but does share the `Popup Dismissed` / `Question Shown` / `Question Answer Selected` events.

---

## 4. QA Verification

Open the preview URL in Chrome:

```
https://www.qa.mdsnrd.com/?dyExperienceId=2667345&dyIsDraft=false&dyIsPreview=true&dyPersistSession=true&dypPreviewKey=659256927&dySmartId=1129717&dyTemp=true&dyVariationId=%5B21887493%5D
```

DevTools → Network → filter input = `dpx`.

### Scenario A — Full conversion (email + SMS)

| Step | Expected `dpx` POST | Expected payload (key fields) |
|---|---|---|
| 1. Modal renders | `Question Shown` | `event_category: 'Email Capture Quiz'`, `variant_info: 'V2'` |
| 2. Tap a quiz button (e.g. "Cover Roots") | `Question Answer Selected` | `selected_answer: 'Cover Roots'`, `variant_info: 'V2'` |
| 3. Enter valid email + click `GET FREE SHIPPING` | `Email Submitted Successfully` then `Popup Capture Submitted` then `identify-v1` (if `cuid` returned) | `opt_in_type: 'email_only'`, `reward_granted: 'free_shipping'`, `variant_info: 'V2'` |
| 4. On SMS step, enter phone + click `CLAIM MY OFFER` | `Popup Capture Submitted` | `opt_in_type: 'email_sms'`, `reward_granted: 'free_shipping_and_discount'`, `variant_info: 'V2'` |

### Scenario B — Dismiss without converting

| Step | Expected `dpx` POST |
|---|---|
| 1. Modal renders | `Question Shown` |
| 2. Click the ✕ close button | `Popup Dismissed` (`event_category`, `timestamp`, `variant_info: 'V2'`) |

### Scenario C — Dismiss after email but before SMS

| Step | Expected `dpx` POST |
|---|---|
| 1. Submit email successfully | `Email Submitted Successfully` + `Popup Capture Submitted` (`email_only`) |
| 2. Click the ✕ close button on the SMS screen | **NO** `Popup Dismissed` (because `userConverted` is true) |

### Scenario D — Press ESC at any point

| Step | Expected `dpx` POST |
|---|---|
| ESC pressed before any conversion | `Popup Dismissed` |
| ESC pressed after email submit success | **NO** `Popup Dismissed` (userConverted true) |

### Scenario E — Navigate away without converting

`beforeunload` fires `Popup Dismissed` when the user closes the tab / navigates. Note: `beforeunload` may not always send the request reliably — treat this as best-effort.

---

## 5. Open Questions for Marketing (Confirm Before Publishing)

1. **Property values for `opt_in_type`** — current code uses `email_only` / `email_sms`. Confirm these strings match Marketing's dashboards.
2. **Property values for `reward_granted`** — current code uses `free_shipping` / `free_shipping_and_discount`. Confirm.
3. **Fire-twice vs fire-once semantics for `Popup Capture Submitted`:**
   - Current behavior: email-only success fires it once with `email_only`, then SMS success fires it AGAIN with `email_sms`. Marketing sees TWO events per full-conversion user.
   - Alternative behavior: fire only once at the final commit point (deferred until dismiss/SMS-success). More complex.
   - **Going with fire-twice** unless Marketing requests otherwise.
4. **`Popup Dismissed` suppression after conversion** — current code does NOT fire `Popup Dismissed` if the user already submitted email or SMS. Confirm this matches Marketing's intent (the spec says "without answering" which suggests this is correct).

---

## 6. Rollback Plan

If something breaks after publish:
1. DY admin → Variation 2 → Template editor → Revisions tab → restore the previous revision.
2. Or manually re-paste the original HTML/CSS/JS from `dotcompb-7052-dynamic-yield-email-sms.md` (lines 27–67 HTML, 71–129 CSS, 131–270 JS) or from the snapshots in this sessions folder (`dynamic-yield.html`, `dynamic.css`, `dynamic.js`).

---

## 7. Post-Deploy

1. Comment on DOTCOMPB-8211 with link to this folder + screenshots of the six events firing in the Network panel.
2. Move DOTCOMPB-8211 to **In Test** lane.
3. Update the roam node QA INSTRUCTIONs section with the screenshots.
4. Transition DOTCOMPB-8211 to **Done** once Marketing confirms the events appear in their DY analytics dashboard.
