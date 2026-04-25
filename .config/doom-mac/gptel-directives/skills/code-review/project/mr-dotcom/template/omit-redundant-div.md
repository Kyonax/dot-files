---
id: rule-pj-mrd-006
title: Omit Redundant div in Pug
severity: MEDIUM
tags: div, pug, redundant, semantic, section
---

Omit the `div` tag name in Pug when a class or ID is present; Pug auto-creates a div. Only write the tag name explicitly for non-div elements.

### Apply
- All Pug templates in `.vue` files

### Skip
- Bare `div` with no class or ID (must be written explicitly as `div`)

### Bad
```pug
div.hero-banner
  div.hero-content
    div#main-cta
      mr-btn Save
```

### Good
```pug
.hero-banner
  .hero-content
    #main-cta
      mr-btn Save
```

### Edge
When a semantic HTML element is needed, always write the tag name explicitly even with a class:
```pug
section.hero-banner
  article.hero-content
    nav.breadcrumbs
```
The rule only targets `div` — all other tag names must remain explicit.
