---
id: rule-pj-mrb-005
title: Express Validation Must Cover Vue Router Child Paths
severity: HIGH
tags: express, child-route, optional-param, vue-router
---

Express route validation must accept Vue Router child paths by using an optional `:path?` parameter segment — otherwise the server returns 404 for valid client-side child routes.

### Apply
- Adding Vue Router `children` to a CMS page route
- CMS page has Express-level URL validation (parseUrl, parameterized route matching)
- Child route URLs add path segments beyond what the parent CMS page expects

### Skip
- Hash-based child routing (`#/tab`) that does not change the URL path
- Query-parameter-based tabs/views that keep the same URL path

### Bad
```javascript
// Express route only accepts /hair-color-bar/:slug
// Vue Router child at /hair-color-bar/:slug/reviews → Express 404

// routes.js
{ path: '/hair-color-bar/:slug', component: LocationPage }

// No optional param — /hair-color-bar/nyc/reviews hits Express before Vue Router
```

### Good
```javascript
// Express route accepts optional trailing segment
// routes.js
{
  path: '/hair-color-bar/:slug/:childView?',
  component: LocationPage,
  children: [
    { path: '', component: LocationOverview },
    { path: 'reviews', component: LocationReviews },
  ],
}

// Tophat urlParameterList updated to: "slug,childView"
// Express parseUrl allows 1 or 2 segments → passes validation
```

### Edge
The optional param (`:childView?`) must be present at BOTH the Express/CMS level (Tophat `urlParameterList`) and the Vue Router level. If Express accepts it but Vue Router does not define the child route, the page renders but shows the parent component's default slot — potentially a blank view. Always verify both layers when adding child routes.
