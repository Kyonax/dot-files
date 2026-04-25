---
id: rule-pj-mrd-047
title: Avoid Double Tracking
severity: MEDIUM
tags: tracking, double, duplicate, event, openSignInModal
---

Do not call `trackMREvent` before a helper that internally already calls it; this produces duplicate events.

### Apply
- Any call to a helper/utility that includes its own tracking logic (e.g., `openSignInModal`, `addToCart`)

### Skip
- Helpers that are documented as not including internal tracking

### Bad
```javascript
methods: {
  handleSignIn() {
    trackMREvent('Sign In Clicked', { source: 'header' });
    openSignInModal({ source: 'header' }); // openSignInModal already tracks internally
  },
},
```

### Good
```javascript
methods: {
  handleSignIn() {
    openSignInModal({ source: 'header' }); // internal tracking handles the event
  },
},
```

### Edge
If the helper's internal tracking does not capture all the properties you need (e.g., a page-specific source), check if the helper accepts a properties parameter to merge your data into the single event rather than firing a separate one.
