---
id: rule-u-ada-001
title: Landmark Self-Contained
severity: HIGH
tags: landmark, role-region, aria-labelledby, section
---

Every landmark region must contain its own heading and reference it via aria-labelledby within the same component.

### Apply
- Any element with `role="region"`, `<section>`, `<nav>`, `<aside>`, or `<main>` that acts as a landmark

### Skip
- Landmarks that are the only one of their type on the page (e.g., a single `<main>`) and are implicitly labeled by the page title

### Bad
```html
<h2 id="services-heading">Our Services</h2>
<section aria-labelledby="services-heading">
  <ul>
    <li>Haircut</li>
    <li>Styling</li>
  </ul>
</section>
```

### Good
```html
<section aria-labelledby="services-heading">
  <h2 id="services-heading">Our Services</h2>
  <ul>
    <li>Haircut</li>
    <li>Styling</li>
  </ul>
</section>
```

### Edge
A component rendered inside another landmark should not double-wrap with its own landmark. If a `<section>` is placed inside an existing `<section role="region">`, the inner component should contribute content — not introduce a redundant landmark that fragments the accessibility tree.
