---
id: rule-u-ada-025
title: div + aria-label Requires Explicit Role
severity: HIGH
tags: wcag-4.1.2, aria-label, div, role, role-group, role-region, role-img
---

A bare `<div>` cannot carry `aria-label` — it has no implicit role for the label to attach to, so the label has no programmatic effect and triggers WCAG 4.1.2 ("Name, Role, Value"). Either add an explicit role (`role="group"`, `role="region"`, `role="img"`, etc.) or drop the aria-label entirely.

### Apply
- `<div :aria-label="...">` or `<div aria-label="...">` without a role attribute
- `<span aria-label="...">` without a role (same issue — spans have no implicit role)
- Container divs that hold a labeled section/group of related controls

### Skip
- `<a aria-label>` (link role is implicit)
- `<button aria-label>` (button role is implicit)
- `<section aria-label>` (region role implicit when named)
- `<nav>`, `<header>`, `<main>`, `<aside>`, `<footer>` (each has an implicit landmark role)
- `<div role="...">` with any role already present
- `aria-labelledby` instead of `aria-label` (different attribute, different rule)

### Bad
```html
<!-- Bare div carrying aria-label — no role to attach the label to -->
<div class="carousel-frame" aria-label="RECKIT — Previews">
  <img src="...">
  <img src="...">
</div>
```

### Good — Option A: add a role
```html
<div class="carousel-frame" role="group" aria-label="RECKIT — Previews">
  <img src="...">
  <img src="...">
</div>
```

### Good — Option B: drop the label, rely on context
```html
<!-- The wrapping dialog already names this content via its own aria-label —
     no need for a redundant inner label. -->
<div class="carousel-frame">
  <img src="...">
  <img src="...">
</div>
```

### Edge
Pick the role that matches semantic intent: `role="group"` for a labeled collection of related controls; `role="region"` for a meaningful named area (creates a landmark — be sparing); `role="img"` if the div renders a composed image (e.g. ASCII art via CSS — the label describes the image as a whole, screen readers announce it as a single image). `role="presentation"` STRIPS the label — don't use it. When the surrounding context (modal title, section heading, parent landmark) already names the content, Option B is preferable to adding a role that creates an unwanted landmark or grouping. Audit by greping for `aria-label` and inspecting the host element on each match.
