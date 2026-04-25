---
id: rule-pj-mrb-006
title: Component-less Parent Routes Prevent Double Render
severity: HIGH
tags: componentless, parent, vue-router, double-render, cms
---

CMS pages that use Vue Router children need a component-less parent route (no `component` property, only `children`) to prevent the parent component from rendering twice — once as the route match and once as the CMS page wrapper.

### Apply
- CMS page component is also set as the Vue Router parent route `component`
- Page renders duplicated content or layout sections (header, hero) appear twice
- Adding `children` to an existing CMS page route

### Skip
- Routes where the parent component is intentionally a layout shell (e.g., `<router-view />` only) distinct from the CMS page component
- Non-CMS pages that are fully managed by Vue Router without CMS wrapping

### Bad
```javascript
// CMS already renders LocationPage as the page component
// Vue Router ALSO renders LocationPage as the route component → double render
{
  path: '/hair-color-bar/:slug',
  component: LocationPage,  // WRONG — CMS already mounts this
  children: [
    { path: '', component: LocationOverview },
    { path: 'reviews', component: LocationReviews },
  ],
}
```

### Good
```javascript
// Component-less parent — CMS handles the page component,
// Vue Router only manages which child renders in <router-view>
{
  path: '/hair-color-bar/:slug',
  // No component property here
  children: [
    { path: '', component: LocationOverview },
    { path: 'reviews', component: LocationReviews },
  ],
}
```

### Edge
When using a component-less parent, the CMS page component must include a `<router-view />` in its template so the matched child component has a render target. If `<router-view />` is missing, children match the route but render nothing. Test both the default child path (`/hair-color-bar/nyc`) and explicit child paths (`/hair-color-bar/nyc/reviews`) to verify correct rendering.
