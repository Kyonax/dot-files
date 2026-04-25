---
id: rule-pj-mrd-054
title: Self-Sufficient Component Spacing
severity: MEDIUM
tags: spacing, component, parent, wrapper, self-sufficient
---

Components must handle their own internal spacing; parents must not add wrapper divs solely for spacing.

### Apply
- Every reusable component's margin and padding

### Skip
- Page-level layout containers that coordinate spacing between sibling sections

### Bad
```javascript
// Parent adds wrapper div just for spacing
<template lang="pug">
.page-content
  .card-spacing-wrapper
    ServiceCard(:data="service")
  .card-spacing-wrapper
    ServiceCard(:data="service2")
</template>

<style lang="stylus" scoped>
.card-spacing-wrapper
  margin-bottom 24px
</style>
```

### Good
```javascript
// Component handles its own spacing
<template lang="pug">
.page-content
  ServiceCard(:data="service")
  ServiceCard(:data="service2")
</template>

// Inside ServiceCard.vue:
<style lang="stylus" scoped>
.service-card
  margin-bottom 24px
</style>
```

### Edge
When a component is used in contexts requiring different spacing, accept a spacing prop or use CSS gap on the parent grid/flex container rather than adding wrapper divs.
