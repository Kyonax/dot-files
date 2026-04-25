---
id: rule-pj-mrd-016
title: No Spacing in Stylus
severity: MEDIUM
tags: spacing, padding, margin, utility, stylus
---

Do not write padding or margin in Stylus when an equivalent utility class exists; use the utility class in the template instead.

### Apply
- All margin and padding declarations in Stylus blocks

### Skip
- Negative margins (no utility equivalent)
- Spacing that must be calculated dynamically (`calc()`, variable-based)

### Bad
```stylus
.section
  padding-top 32px
  margin-bottom 16px
```
```pug
.section
  p Content
```

### Good
```pug
.section.xs-pt-200m.xs-mb-100m
  p Content
```

### Edge
When a spacing value does not align with the utility scale (e.g., `padding 7px` for a very specific alignment fix), Stylus is acceptable but must include a comment explaining why the utility class cannot be used.
