---
id: rule-fw-vue-003
title: Canonical Field Order in Options API
severity: MEDIUM
tags: field-order, name, props, data, computed, methods, options-api
---

Follow canonical field order in Options API components: name, components, emits, props, data, computed, watch, lifecycle hooks, methods.

### Apply
- Any Vue component using the Options API
- When adding a new option to an existing Options API component

### Skip
- Components using `<script setup>` / Composition API (no enforced option order)
- Tiny single-purpose components with only one or two options

### Bad
```javascript
export default {
  methods: {
    submit() { /* ... */ },
  },
  name: 'CheckoutForm',
  data() {
    return { loading: false };
  },
  props: {
    cartId: { type: String, required: true },
  },
  computed: {
    isValid() { return this.cartId.length > 0; },
  },
};
```

### Good
```javascript
export default {
  name: 'CheckoutForm',
  props: {
    cartId: { type: String, required: true },
  },
  data() {
    return { loading: false };
  },
  computed: {
    isValid() { return this.cartId.length > 0; },
  },
  methods: {
    submit() { /* ... */ },
  },
};
```

### Edge
Composition API has no enforced option order. When using `<script setup>`, group related logic by feature or concern rather than by type. See `rule-fw-vca-001` for recommended ordering conventions.
