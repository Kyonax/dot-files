---
id: rule-pj-mrd-018
title: Font Properties in Template Only
severity: CRITICAL
tags: font, size, color, template, stylus, xs-f
---

Font sizes, font-family, and text colors must be applied via utility classes in the Pug template, never in Stylus blocks.

### Apply
- All font-size, font-family, font-weight, and color (text) declarations

### Skip
- `:deep()` overrides where utility classes cannot reach a child component's internals

### Bad
```stylus
.heading
  font-size 24px
  font-family 'Kapra Neue', sans-serif
  color brand-color-purple
```

### Good
```pug
h2.xs-f-large.lg-f-display.max-at-tweak.bold.color-mr-purple.f-secondary Heading
```

### Edge
Available font utility classes include `.xs-f-small`, `.xs-f-base`, `.xs-f-large`, `.xs-f-display` with breakpoint variants, `.bold`, `.light`, `.f-secondary` (Kapra Neue), and color classes like `.color-mr-purple`, `.color-mr-dark`. If a needed combination is not available, request a utility addition rather than writing Stylus.
