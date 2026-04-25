---
id: rule-pj-mrd-020
title: rem/em Units for Spacing
severity: MEDIUM
tags: rem, em, px, border, shadow, units
---

Use rem or em for all spacing values; px is permitted only for borders, box-shadows, and outlines.

### Apply
- All Stylus property values for margin, padding, gap, width, height, top/right/bottom/left

### Skip
- `border`, `border-width`, `box-shadow`, `outline`, and `outline-offset` declarations

### Bad
```stylus
.card
  padding 16px
  margin-bottom 24px
  gap 8px
```

### Good
```stylus
.card
  padding 1rem
  margin-bottom 1.5rem
  gap 0.5rem
  border 1px solid ui-color-border
  box-shadow 0 2px 4px rgba(0, 0, 0, 0.1)
```

### Edge
Media query breakpoints defined in the project's Stylus config may use px. This rule targets component-level styles only. When converting legacy px values, use the base of 16px (1rem = 16px).
