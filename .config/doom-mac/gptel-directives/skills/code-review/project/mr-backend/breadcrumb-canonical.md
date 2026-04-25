---
id: rule-pj-mrb-012
title: Breadcrumb URLs Must Match Canonical Routes
severity: MEDIUM
tags: breadcrumb, canonical, url, routes, path
---

Breadcrumb navigation URLs must match the canonical paths defined in `routes.js` — mismatches break SEO structured data and cause incorrect navigation.

### Apply
- Adding or modifying breadcrumb components on CMS pages
- Breadcrumb `href` values are hardcoded strings rather than derived from route definitions
- Google Search Console reports breadcrumb structured data errors

### Skip
- Breadcrumbs on non-CMS pages that do not contribute to SEO (e.g., Tophat admin pages)
- External links in breadcrumbs (e.g., linking to a third-party site)

### Bad
```javascript
// Hardcoded breadcrumb URL does not match routes.js canonical path
computed: {
  breadcrumbs() {
    return [
      { text: 'Home', href: '/' },
      { text: 'Hair Color Bar', href: '/haircolorbar' },  // WRONG — route is /hair-color-bar
      { text: this.locationName, href: this.locationUrl },
    ];
  },
},
```

### Good
```javascript
// Breadcrumb URLs match the canonical paths in routes.js
computed: {
  breadcrumbs() {
    return [
      { text: 'Home', href: '/' },
      { text: 'Hair Color Bar', href: '/hair-color-bar' },  // Matches routes.js
      { text: this.locationName, href: `/hair-color-bar/${this.locationSlug}` },
    ];
  },
},
```

### Edge
Breadcrumb URLs feed into JSON-LD `BreadcrumbList` structured data for Google. If a breadcrumb URL does not match the canonical route, Google may flag the structured data as invalid or — worse — index a non-canonical URL variant. Always derive breadcrumb paths from the same source of truth as `routes.js`. When CMS page URLs change in Tophat, verify that breadcrumb components are updated to match the new canonical paths.
