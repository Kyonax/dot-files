---
id: rule-fw-vue-001
title: v-if Guard on Nullable Data
severity: HIGH
tags: v-if, guard, null, data, undefined
---

Every template section or element that renders data which may be null or undefined must be wrapped with a `v-if` guard.

### Apply
- Any template block that accesses a prop, computed, or data property that could be null/undefined before data is loaded
- Images, sections, or child components whose source data comes from an async fetch

### Skip
- Data that is initialized with a non-null default and never reassigned to null
- Primitive props with required: true and a guaranteed parent binding

### Bad
```html
<template>
  <div class="hero">
    <img :src="product.image" :alt="product.name" />
    <h2>{{ product.title }}</h2>
  </div>
</template>
```

### Good
```html
<template>
  <div v-if="product" class="hero">
    <img :src="product.image" :alt="product.name" />
    <h2>{{ product.title }}</h2>
  </div>
</template>
```

### Edge
Empty object `{}` is truthy. Guarding with `v-if="product"` will pass even when the object has no meaningful properties. When the shape matters, check a meaningful property instead: `v-if="product?.id"` or `v-if="product?.title"`.
