---
id: rule-fw-vue-008
title: Parent Prepares Data Before Passing Props
severity: MEDIUM
tags: parent, computed, props, preparation, transform
---

The parent should compute and transform data in its own computed properties before passing it as props; the child should not re-derive what the parent could prepare.

### Apply
- When the parent has raw data and the child only needs a subset or transformed version
- When multiple children would need to independently compute the same derived value

### Skip
- When the transformation is inherently the child's responsibility (e.g., formatting for display within a highly reusable component)
- When the child encapsulates domain logic that should not leak into the parent

### Bad
```html
<!-- Parent passes raw data, child re-derives -->
<template>
  <ProductCard :product="rawProduct" />
</template>

<!-- ProductCard.vue -->
<script>
export default {
  props: { product: Object },
  computed: {
    displayPrice() {
      return `$${(this.product.priceInCents / 100).toFixed(2)}`;
    },
    isOnSale() {
      return this.product.salePrice < this.product.originalPrice;
    },
  },
};
</script>
```

### Good
```html
<!-- Parent prepares the data -->
<template>
  <ProductCard
    :display-price="formattedPrice"
    :is-on-sale="isOnSale"
    :name="rawProduct.name"
    :image="rawProduct.image"
  />
</template>

<script>
export default {
  computed: {
    formattedPrice() {
      return `$${(this.rawProduct.priceInCents / 100).toFixed(2)}`;
    },
    isOnSale() {
      return this.rawProduct.salePrice < this.rawProduct.originalPrice;
    },
  },
};
</script>
```

### Edge
Highly reusable, generic components (date pickers, data tables) should own their own formatting logic. This rule targets feature-level parent-child relationships where the parent has the context to prepare data and the child is a presentational leaf.
