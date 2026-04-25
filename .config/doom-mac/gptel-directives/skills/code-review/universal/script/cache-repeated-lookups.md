---
id: rule-u-sc-006
title: Cache Repeated Lookups
severity: MEDIUM
tags: cache, lookup, property, const
---

When the same deeply nested property is accessed more than once, cache it in a local `const` to improve readability and avoid redundant traversal.

### Apply
- Two or more accesses to the same multi-level property chain within a function

### Skip
- Single-use property access (no benefit to caching)
- Reactive getters (Vue computed properties, Vuex getters) that must be accessed live each time

### Bad
```javascript
function processOrder(state) {
  if (state.checkout.order.items.length === 0) {
    return;
  }

  const total = state.checkout.order.items.reduce((sum, i) => sum + i.price, 0);
  const count = state.checkout.order.items.length;

  log(`Processing ${count} items totalling ${total}`);
}
```

### Good
```javascript
function processOrder(state) {
  const items = state.checkout.order.items;

  if (items.length === 0) {
    return;
  }

  const total = items.reduce((sum, i) => sum + i.price, 0);
  const count = items.length;

  log(`Processing ${count} items totalling ${total}`);
}
```

### Edge
In reactive frameworks, caching a getter (e.g., `const val = this.computedProp`) inside a synchronous block is safe. However, do not cache across async boundaries if the value might change between awaits.
