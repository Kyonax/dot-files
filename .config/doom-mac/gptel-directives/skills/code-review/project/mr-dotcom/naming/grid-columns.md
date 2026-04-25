---
id: rule-pj-mrd-033
title: Grid Column Role Naming
severity: MEDIUM
tags: grid, column, role, naming
---

Grid columns must be named by their role using the {role}-column pattern.

### Apply
- Any layout using CSS grid or flex columns

### Skip
- Single-column layouts where role naming adds no clarity

### Bad
```javascript
<template lang="pug">
.page-grid
  .left-side
    ProductDetails(:product="product")
  .right-side
    ProductGallery(:images="images")
</template>
```

### Good
```javascript
<template lang="pug">
.page-grid
  .main-column
    ProductDetails(:product="product")
  .sidebar-column
    ProductGallery(:images="images")
</template>
```

### Edge
When a grid has more than two columns, use descriptive role names like `.nav-column`, `.content-column`, `.aside-column` rather than positional names like `.first-column`, `.second-column`.
