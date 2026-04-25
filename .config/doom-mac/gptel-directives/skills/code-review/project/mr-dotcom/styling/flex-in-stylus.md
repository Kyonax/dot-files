---
id: rule-pj-mrd-012
title: Flex Layout in Stylus
severity: MEDIUM
tags: flex, stylus, utility, direction, justify
---

Keep flex layout properties (display, flex-direction, justify-content, align-items, flex-wrap, gap) in Stylus blocks, not as utility classes.

### Apply
- All flexbox layout declarations in Vue SFC components

### Skip
- Simple single-axis alignment that has a dedicated utility class and no other flex properties

### Bad
```pug
.container.flex.flex-row.justify-center.align-items-center
  .item Content
```

### Good
```pug
.container
  .item Content
```
```stylus
.container
  display flex
  flex-direction row
  justify-content center
  align-items center
```

### Edge
If the project introduces official flex utility classes in its design system, this rule should be revisited. Currently, flex layout is managed in Stylus to keep template class lists focused on spacing and typography.
