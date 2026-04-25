---
id: rule-pj-mrd-014
title: Design System Variables for Colors
severity: MEDIUM
tags: color, variable, hex, brand-color, ui-color, darken, lighten
---

Use design system Stylus variables for all colors; no hardcoded hex, rgb, or named CSS colors.

### Apply
- All color declarations in `<style scoped lang="stylus">` blocks

### Skip
- Third-party override blocks where the SDK dictates a specific hex value (must be commented)

### Bad
```stylus
.banner
  background-color #7B2D8E
  color white
  border 1px solid rgb(123, 45, 142)
```

### Good
```stylus
.banner
  background-color brand-color-purple
  color text-color-white
  border 1px solid darken(brand-color-purple, 10%)
```

### Edge
Variable families include `brand-color-*`, `cta-color-*`, `text-color-*`, `ui-color-*`, and `bg-color-*`. Use `darken()` and `lighten()` Stylus functions for shading variants. If a needed color does not exist in the system, request its addition rather than hardcoding.
