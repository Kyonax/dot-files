---
id: rule-pj-mrd-017
title: Stylus Nesting Mirrors Pug Hierarchy
severity: MEDIUM
tags: nesting, stylus, hierarchy, template, pug
---

Stylus selector nesting must mirror the Pug template DOM hierarchy; do not flatten or over-nest.

### Apply
- All Stylus blocks that style elements from the component's own Pug template

### Skip
- `:deep()` overrides targeting third-party or child component internals

### Bad
```pug
.hero
  .hero__content
    h2 Title
```
```stylus
//- Flat — loses structural context
.hero__content
  color text-color-dark

//- Over-nested — adds selectors not in template
.hero
  .hero__wrapper
    .hero__content
      color text-color-dark
```

### Good
```pug
.hero
  .hero__content
    h2 Title
```
```stylus
.hero
  .hero__content
    color text-color-dark
```

### Edge
BEM modifier classes (`.hero--dark`) sit at the same nesting level as the block they modify, not nested inside themselves.
