---
id: rule-pj-mrb-009
title: Time-Dependent Values Must Be Computed for SSR
severity: MEDIUM
tags: ssr, timezone, computed, utc, data, hydration
---

Time-dependent display values must use `computed` properties, not `data()` — SSR serializes values at render time in UTC, but the client needs local timezone values, causing hydration mismatch.

### Apply
- Component displays dates, times, countdowns, or "time ago" strings
- Values like "Open Now", "Closes at 8 PM", or formatted dates are set in `data()`
- SSR output shows UTC-based time while client shows local timezone

### Skip
- Time values that are always displayed in a fixed timezone (e.g., "All times in ET" with explicit UTC offset math)
- Components that only use timestamps for logic, not display (e.g., checking expiration in a method)

### Bad
```javascript
// data() runs during SSR and serializes the UTC result
data() {
  return {
    openStatus: this.isStoreOpen() ? 'Open Now' : 'Closed',
    formattedDate: new Date().toLocaleDateString(),
  };
},
```

### Good
```javascript
// computed re-evaluates on the client with the correct timezone
computed: {
  openStatus() {
    return this.isStoreOpen() ? 'Open Now' : 'Closed';
  },
  formattedDate() {
    return new Date().toLocaleDateString();
  },
},
```

### Edge
The SSR server runs with `TZ=UTC` (set in the environment). `data()` values are serialized into the `__INITIAL_STATE__` Vuex payload and hydrated as-is on the client. `computed` properties are re-evaluated during client hydration, picking up the user's local timezone. For values that MUST match between server and client (e.g., cache keys), use UTC explicitly with `toISOString()` or `Intl.DateTimeFormat` with a fixed `timeZone` option.
