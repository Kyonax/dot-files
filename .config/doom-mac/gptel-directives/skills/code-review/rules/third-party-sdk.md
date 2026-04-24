---
title: Third-Party SDK Integration — DOM Scoping, Content Detection, Listener Cleanup, ARIA
impact: MEDIUM
impactDescription: Third-party SDKs (Dash Hudson, Birdeye, Google Maps, Stripe) inject their own DOM that Vue cannot manage. Without scoped queries, versioned :deep() overrides, content detection with timeout, and listener cleanup, components break with multiple instances, leak memory on SPA transitions, or leave empty sections visible when the SDK fails.
tags: sdk, third-party, dash-hudson, birdeye, google-maps, stripe, deep, refs, querySelector, waitForElement, timeout, listener, cleanup, beforeUnmount, aria-labelledby, conditional, slotted, version, content-detection, domain-whitelist
---

Rules for reviewing components that host third-party SDK scripts. SDKs inject DOM at runtime that Vue has no compilation-time knowledge of. This creates unique challenges: style overrides must use `:deep()` (no props API), DOM queries must be scoped to `$refs` (multiple instances), content may never render (domain whitelisting, ad blockers), and event listeners must be manually tracked for cleanup. These 5 rules were validated across Dash Hudson (DOTCOMPB-7527), Google Maps (DOTCOMPB-7289), and Birdeye (DOTCOMPB-7290) integrations.

## Rules

| # | Rule | Severity | Section |
|---|---|---|---|
| 1 | **sdk-dom-scope** — `:deep()` for styles, `$refs` for queries, comment identifying SDK | HIGH | [Details](#sdk-dom-scope) |
| 2 | **sdk-content-detect** — Poll for SDK DOM with timeout, hide section on failure | MEDIUM | [Details](#sdk-content-detect) |
| 3 | **sdk-listener-cleanup** — Track and removeEventListener in beforeUnmount | MEDIUM | [Details](#sdk-listener-cleanup) |
| 4 | **sdk-deep-exception** — `:deep()` is the ONE validated exception for SDK DOM | MEDIUM | [Details](#sdk-deep-exception) |
| 5 | **sdk-aria-conditional** — Conditional `aria-labelledby` when heading depends on SDK load | MEDIUM | [Details](#sdk-aria-conditional) |

---

## sdk-dom-scope

When a third-party SDK injects DOM, apply three sub-rules:

1. **`:deep()` for style overrides** — target SDK class names directly. Version-gate under a parent class (e.g., `&.version-2`) so existing consumers are unaffected.
2. **`$refs` for DOM queries** — `this.$refs.container.querySelector('.sdk-class')`, NOT `document.querySelector`. Multiple widget instances on the same page (or across route transitions) produce duplicate DOM.
3. **Comment identifying SDK** — per ad-16, add a comment naming the SDK and version the selectors target.

### Correct vs incorrect

**Incorrect** — global querySelector, no comment, unversioned:
```javascript
mounted() {
  const el = document.querySelector('.dh-ugc-container'); // wrong instance
}
```
```stylus
.aspect-ratio-box
  aspect-ratio 4/5  // leaks to all instances
```

**Correct** — scoped to $refs, versioned, commented:
```javascript
mounted() {
  const container = this.$refs.dhUgc;
  const el = container.querySelector('.dh-ugc-container');
}
```
```stylus
&.version-2
  // Dash Hudson SDK overrides (product-carousel-embed.js)
  :deep(.aspect-ratio-box)
    aspect-ratio 4/5
    border-radius 12px
```

### Edge case: prefer SDK config attributes when available

If the SDK exposes a configuration attribute (e.g., `data-media-format="original"`), prefer it over `:deep()`. Use `:deep()` only when SDK config is insufficient (decision #70: `data-media-format` doesn't support custom aspect ratios).

**Validated:** DashHudsonScriptInner (DOTCOMPB-7527) — `$refs.dhUgc` scoping, `&.version-2` gating.

---

## sdk-content-detect

SDK scripts load asynchronously and may never render content (domain whitelisting, empty gallery, network failure, ad blockers). Use `waitForElement` utility to poll for SDK-specific DOM markers with a timeout. If timeout expires, hide the section.

### Pattern

```javascript
async checkIfContentExists() {
  const container = this.$refs.sdkContainer;
  const element = await waitForElement(
    () => container.querySelector('.sdk-content-marker'),
    200,   // poll interval ms
    5000,  // timeout ms
  );
  if (element) {
    this.noContent = false;
  }
  this.checkFinished = true;
}
```

### Edge cases

- **Localhost**: SDK only renders on whitelisted domains. On localhost, poll ALWAYS times out. `noContent = true` hides the section gracefully. This is expected dev behavior.
- **Component unmounts during poll**: `waitForElement` resolves with null on timeout. The promise callback writes to a destroyed instance — Vue 3 silently ignores this, but it is technically a leak. If cancellation is needed, wrap in a `mounted` guard.
- **`waitForElement` returns null on timeout, not rejection**: callers must check truthiness, not catch errors.

**Validated:** DashHudsonScriptInner `checkIfContentExists()` with `Promise.all` for slider container + clickable elements (DOTCOMPB-7527).

---

## sdk-listener-cleanup

Event listeners attached to SDK-injected DOM must be tracked and removed in `beforeUnmount`. SDK DOM is not managed by Vue's virtual DOM — Vue will not clean up listeners automatically.

### Pattern

```javascript
// Store as non-reactive instance property (not in data())
this._sdkCleanups = [];

// When attaching
sdkElements.forEach((el) => {
  const handler = () => this.trackMREvent('SDK clicked');
  el.addEventListener('click', handler);
  this._sdkCleanups.push({ el, handler });
});

// In beforeUnmount
beforeUnmount() {
  if (this._sdkCleanups) {
    this._sdkCleanups.forEach(({ el, handler }) => {
      el.removeEventListener('click', handler);
    });
    this._sdkCleanups = null;
  }
}
```

### Edge cases

- **Guard `_sdkCleanups` existence**: if `checkIfContentExists()` timed out, the cleanup array was never initialized. The `if (this._sdkCleanups)` guard prevents TypeError.
- **Non-reactive storage**: use `this._` prefix (not `data()`) to avoid Vue reactivity overhead on infrastructure-only references.
- **SDK `<script>` tags**: removal on unmount is NOT necessary — browser GC handles orphaned script elements.

**Validated:** DashHudsonScriptInner `_dhClickCleanups` pattern (DOTCOMPB-7527).

---

## sdk-deep-exception

`:deep()` is normally discouraged (rule sg-2: "prefer props over `:deep()`"). Third-party SDK DOM is the **one validated exception** because SDK-injected DOM has no props API — it is runtime HTML that Vue has no compilation-time knowledge of.

### When `:deep()` IS justified

- Third-party SDK DOM (Dash Hudson, Birdeye widget, Google Maps, Stripe Elements)
- The SDK's own configuration attributes are insufficient for the needed customization
- Overrides are version-gated and commented

### When `:deep()` is NOT justified (use props/slots instead)

- Vue child components with a props API
- CMS-injected HTML (use global styles or `CMSPartial` style hooks)
- Design system components like MrBtn (use variant props first, `:deep()` only for color overrides not covered by variants)

**Validated:** DashHudsonScriptInner `&.version-2 :deep(.aspect-ratio-box)` (DOTCOMPB-7527). SDK `data-media-format` attribute only supported `"original"`, not custom ratios — `:deep()` was the only option.

---

## sdk-aria-conditional

When a section wraps SDK content with an optional slotted heading, `aria-labelledby` must be conditional. Rendering `aria-labelledby="some-id"` when the referenced element doesn't exist (SDK failed, slot empty) is an accessibility violation.

### Pattern

```javascript
computed: {
  showTitle() {
    const titleSlot = this.$slots.title?.();
    return titleSlot?.length > 0 && this.checkFinished && !this.noContent;
  },
},
```
```pug
.section(role="region" :aria-labelledby="showTitle ? 'section-title' : null")
  .title(v-if="showTitle")
    slot(name="title")
```

Vue removes the attribute entirely when value is `null` — no dangling reference.

### Edge cases

- **Three-part gate**: `slot?.length > 0` (slot provided) && `checkFinished` (poll completed) && `!noContent` (SDK rendered). Without `checkFinished`, the heading flashes then disappears during the poll.
- **When heading is always present** (not slotted, not SDK-dependent): static `aria-labelledby="id"` is fine. The conditional pattern is only for SDK-dependent headings.
- **PDP V2 variant**: passes `h3` with no `id` in the title slot — `aria-labelledby` correctly resolves to `null` (nothing to reference without an ID).

**Validated:** DashHudsonScriptInner conditional `aria-labelledby` (DOTCOMPB-7527, decision #71).
