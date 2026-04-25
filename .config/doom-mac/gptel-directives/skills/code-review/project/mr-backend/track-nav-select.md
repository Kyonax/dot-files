---
id: rule-pj-mrb-011
title: Use trackMREventAndRedirect for Cross-Context Navigation
severity: HIGH
tags: tracking, navigation, redirect, cross-context, trackMREventAndRedirect
---

Use `trackMREventAndRedirect` for navigation that crosses CMS contexts — calling `trackMREvent` followed by `goToPath` sequentially risks the tracking event being dropped when the page unloads.

### Apply
- Click handler tracks an event AND navigates to a different CMS page (full page load)
- Using `trackMREvent` immediately followed by `window.location.href` or `goToPath`
- Analytics events are intermittently missing for navigation-triggering interactions

### Skip
- Vue Router client-side navigation within the same CMS context (no page unload)
- Tracking events fired without any subsequent navigation
- Server-side tracking (controller-level events) that does not depend on client page lifecycle

### Bad
```javascript
// Sequential — page may unload before the tracking beacon completes
methods: {
  onLocationClick(location) {
    trackMREvent('Location Selected', { locationId: location.id });
    goToPath(`/hair-color-bar/${location.slug}`);
    // Page unload may abort the tracking request
  },
},
```

### Good
```javascript
// trackMREventAndRedirect waits for the beacon to complete before navigating
methods: {
  onLocationClick(location) {
    trackMREventAndRedirect(
      'Location Selected',
      { locationId: location.id },
      `/hair-color-bar/${location.slug}`
    );
  },
},
```

### Edge
`trackMREventAndRedirect` internally uses a callback/timeout pattern to ensure the tracking pixel or XHR completes before initiating navigation. It includes a safety timeout (typically 300-500ms) so navigation is never blocked indefinitely if the tracking service is slow. Do NOT wrap `trackMREvent` in a `setTimeout` as a DIY alternative — use the dedicated utility which handles edge cases like ad blockers and network failures gracefully.
