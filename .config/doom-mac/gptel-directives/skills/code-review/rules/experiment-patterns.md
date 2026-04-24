---
title: Experiment Patterns — Href/Click Parity, SSR Hydration Flash, Variant Consistency
impact: HIGH
impactDescription: Experiment-gated components silently break when href and click handler resolve different URLs, when SSR renders control and client patches to variant (flash), or when sibling components check different variant sets for the same experiment. These are invisible in dev and only surface in production under real experiment allocation.
tags: experiment, ab-test, variant, href, click, prevent, ssr, hydration, flash, mounted, window-experiments, control, serverPrefetch, dead-code, variant-consistency, composable, constant, LocationCard, LocationCardBody, LocationsDirectory, ShadeShopPage, BookingFlowSiteRevolution
---

Rules for reviewing experiment-gated code in Vue components. Experiments at MR are allocated server-side (cookie-based), but `this.experiments` is populated client-side from `window.experiments` in `globalMixins.js`'s `mounted()` hook. This creates a fundamental SSR gap: experiment data is always `{}` during server render. The three rules below address the most common bugs that arise from this architecture.

## Rules

| # | Rule | Severity | Section |
|---|---|---|---|
| 1 | **exp-href-click** — Experiment-gated href and @click.prevent must resolve the same URL | HIGH | [Details](#exp-href-click) |
| 2 | **exp-no-ssr** — Experiment-gated v-if renders control on server, patches on client (flash) | HIGH | [Details](#exp-no-ssr) |
| 3 | **exp-variant-consistent** — Same experiment must check same variant set across siblings | HIGH | [Details](#exp-variant-consistent) |

---

## exp-href-click

When a link uses both `:href` and `@click.prevent`, the click handler intercepts left-clicks but `href` governs right-click > "Open in new tab", middle-click, Cmd/Ctrl+click, and AT link announcements. If the experiment gates the click handler to a different URL than href, secondary interactions navigate to the wrong destination.

### The invariant

For any `<a>` with both `:href` and `@click.prevent`:

```
href resolution === click handler destination
```

at every experiment variant, including control (A) and all treatment arms.

### Correct vs incorrect

**Incorrect** — href and click resolve different URLs under experiment:

```javascript
// href always points to location details
detailsUrl() {
  return `/colorbar/locations/${this.location.code}`;
},
// click handler conditionally redirects to booking
onCardClick() {
  const url = this.isBookingFlowExperiment ? this.bookingUrl : this.detailsUrl;
  this.trackMREventAndRedirect('Card clicked', url, this.locationProperties);
},
```

**Correct** — single computed drives both href and click:

```javascript
detailsUrl() {
  if (this.isBookingFlowExperiment) { return this.bookingUrl; }
  return this.locationDetailsUrl;
},
onCardClick() {
  this.trackMREventAndRedirect('Card clicked', this.detailsUrl, this.locationProperties);
},
```

### Edge case: intentional divergence

When AC explicitly requires different destinations for left-click vs right-click, this must be:
1. Documented with a comment: `// AC1: left-click opens booking; right-click/new-tab opens location page`
2. Tested with at least two test cases: one for left-click behavior, one asserting the href value.

Without both the comment and the tests, flag the divergence as a bug.

**Validated:** LocationCard (DOTCOMPB-7958/7959). LocationCardBody had hardcoded href, parent overrode click via `@click.prevent` + `$emit`. Right-click went to details, left-click went to booking.

---

## exp-no-ssr

`this.experiments` is set from `window.experiments` in `mounted()` of `globalMixins.js`. During SSR, `mounted()` does not run, so `this.experiments` is `{}`. Any `v-if` reading `this.experiments` evaluates as falsy on the server and renders control markup.

### The SSR experiment timeline

```
Server:  this.experiments = {} → v-if="isExperimentB" → false → control markup
Client:  mounted() → this.experiments = window.experiments → v-if → true → Vue patches DOM
Result:  user sees flash (control → variant)
```

### When acceptable vs not

| Scenario | Flash? | Acceptable? |
|---|---|---|
| Below-the-fold section | Yes, but user hasn't scrolled | Usually acceptable |
| Hero / above-the-fold | Yes, immediately visible | NOT acceptable — use route-level gating |
| Route-level gating (server picks component) | No | Always preferred for above-the-fold |

### Dead code flag

`serverPrefetch` on a component behind `v-if="isExperimentB"` is dead code — the component is never in the SSR virtual DOM tree. Flag for removal.

### Exception: route-level gating (no flash)

```javascript
// Server resolves experiment from cookie, picks correct component — no client patching
{ path: '/shade-shop', component: () => experimentB ? import('./V2') : import('./V1') }
```

**Validated:** ShadeShopPage (DOTCOMPB-7466) used route-level gating. LocationCard/LocationsDirectory used v-if gating (acceptable — below-the-fold card list).

---

## exp-variant-consistent

When multiple sibling components check the same experiment, they must check the same set of variants. If ComponentA gates on `['B', 'C']` and ComponentB gates on `=== 'B'` only, variant C users see inconsistent behavior.

### The invariant

```
variant set checked by ComponentA === variant set checked by ComponentB
```

### Live bug found

LocationCard checks `['B', 'C'].includes(this.experiments['BookingFlowSiteRevolution'])`. LocationsDirectory checks `=== 'B'` only. MrFooter checks `=== 'B'` only. Variant C users: LocationCard redirects to booking, but LocationsDirectory and MrFooter stay in control mode.

### Fix: shared constant

```javascript
// constants/experiments.js
export const BOOKING_FLOW_TREATMENT_VARIANTS = ['B', 'C'];

// Any component
isBookingFlowExperiment() {
  return BOOKING_FLOW_TREATMENT_VARIANTS.includes(
    this.experiments['BookingFlowSiteRevolution'],
  );
},
```

### Edge cases

1. **Multi-arm with intentionally different behavior per variant** — document with comment: `// Variant B: modal; Variant C: inline (see AC)`
2. **Experiment name typos** — shared constant eliminates this class of bug
3. **Late-added variants** — shared constant makes it a single-line change
