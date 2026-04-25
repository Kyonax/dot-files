---
id: rule-pj-mrb-001
title: Audit CMS Partial Store Dependencies
severity: HIGH
tags: cms, partial, serverPrefetch, store, ssr
---

CMS Partial components must populate ALL dependent Vuex stores in `serverPrefetch` before the partial renders on the server.

### Apply
- Component is rendered inside a CMS Partial (loaded via `cms/lib/vueSsr.js`)
- Component uses `mapState`, `mapGetters`, or `this.$store.state` from stores other than its own
- `serverPrefetch` dispatches actions but omits stores that child components depend on

### Skip
- Client-only components (`v-if="isMounted"` guard) that never SSR
- Components rendered inside full CMS pages where the page-level `serverPrefetch` already populates shared stores

### Bad
```javascript
// CMS Partial entry component — only fetches its own store
async serverPrefetch() {
  await this.$store.dispatch('promo/loadPromoData');
  // MISSING: child component reads from 'global' store
},
```

### Good
```javascript
// CMS Partial entry component — fetches ALL dependent stores
async serverPrefetch() {
  await Promise.all([
    this.$store.dispatch('promo/loadPromoData'),
    this.$store.dispatch('global/loadGlobalData'),
  ]);
},
```

### Edge
CMS Partials run in isolation — they do NOT share the store instance of the parent CMS page. A store populated by the full page's `serverPrefetch` is empty inside a Partial. Always trace the full dependency tree of every component the Partial renders and ensure each required store is explicitly populated in the Partial's own `serverPrefetch`.
