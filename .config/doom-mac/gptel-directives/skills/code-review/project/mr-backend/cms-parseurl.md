---
id: rule-pj-mrb-002
title: parseUrl Validates Segment Count Against Tophat urlParameterList
severity: HIGH
tags: cms, parseUrl, urlParameterList, tophat, segments
---

`parseUrl` validates that the URL segment count matches the `urlParameterList` configured in Tophat for each CMS page — add optional parameters when Vue Router child routes introduce additional path segments.

### Apply
- Adding a Vue Router child route that appends segments to a CMS page URL (e.g., `/hair-color-bar/:slug/reviews`)
- CMS page `urlParameterList` in Tophat defines the expected URL parameter positions
- Route returns 404 despite the Vue component and route config being correct

### Skip
- Static CMS pages with no URL parameters
- Client-side-only route changes that do not trigger a full page load through Express

### Bad
```javascript
// Tophat urlParameterList: "slug"
// URL: /hair-color-bar/new-york/reviews
// parseUrl sees 2 segments but expects 1 → 404
```

### Good
```javascript
// Tophat urlParameterList: "slug,subpage"
// URL: /hair-color-bar/new-york/reviews
// parseUrl sees 2 segments, expects up to 2 → passes validation

// Vue Router config uses optional param to handle both paths:
{
  path: '/hair-color-bar/:slug/:subpage?',
  component: LocationPage,
  children: [
    { path: '', component: LocationOverview },
    { path: 'reviews', component: LocationReviews },
  ],
}
```

### Edge
The `urlParameterList` is a comma-separated string in Tophat CMS config, not code. When adding child routes, you must update BOTH the Tophat config (adding optional param names) AND the Vue Router definition. A mismatch between the two causes either a 404 from Express or a blank render from Vue Router. Coordinate with the CMS team when modifying `urlParameterList`.
