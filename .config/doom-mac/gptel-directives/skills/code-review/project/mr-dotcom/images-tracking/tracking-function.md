---
id: rule-pj-mrd-043
title: Correct Tracking Function Selection
severity: MEDIUM
tags: trackMREvent, trackMREventAndRedirect, navigation, tracking
---

Use `trackMREvent` when the user stays on the page; use `trackMREventAndRedirect` when the action triggers a hard redirect.

### Apply
- Every call to a tracking function tied to a user interaction

### Skip
- Server-side tracking calls that do not involve user navigation

### Bad
```javascript
methods: {
  handleCtaClick() {
    trackMREvent('CTA Clicked', { page: 'home' });
    window.location.href = '/hair-color-bar';
  },
},
```

### Good
```javascript
methods: {
  handleCtaClick() {
    trackMREventAndRedirect('CTA Clicked', { page: 'home' }, '/hair-color-bar');
  },
},
```

### Edge
For Vue Router navigations (pushes, not hard redirects), use `trackMREvent` since the page does not unload and the event has time to flush before the route change.
