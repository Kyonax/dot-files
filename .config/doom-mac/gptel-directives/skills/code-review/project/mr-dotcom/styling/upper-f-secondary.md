---
id: rule-pj-mrd-019
title: .upper Mandatory on .f-secondary
severity: MEDIUM
tags: upper, f-secondary, kapra, heading, font
---

`.upper` is mandatory on every element that uses `.f-secondary` because Kapra Neue is an uppercase-only display font.

### Apply
- Every element with the `.f-secondary` utility class

### Skip
- None; there is no valid use of `.f-secondary` without `.upper`

### Bad
```pug
h2.f-secondary.xs-f-large.max-at-tweak Section Title
span.f-secondary Sale
```

### Good
```pug
h2.f-secondary.upper.xs-f-large.max-at-tweak Section Title
span.f-secondary.upper Sale
```

### Edge
Kapra Neue lacks lowercase glyphs. Without `.upper`, the browser renders fallback-font lowercase characters, creating a visual mismatch. This is a design-system invariant, not a stylistic preference.
