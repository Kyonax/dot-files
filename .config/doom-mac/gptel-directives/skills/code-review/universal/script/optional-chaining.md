---
id: rule-u-sc-002
title: Optional Chaining
severity: HIGH
tags: optional-chaining, null, undefined
---

Always use optional chaining (`?.`) instead of manual null/undefined guard checks for property access.

### Apply
- Any property access chain where an intermediate value may be null or undefined
- Method calls on potentially absent objects

### Skip
- When you need to explicitly distinguish `null` from `undefined` for branching logic

### Bad
```javascript
const city = user && user.address && user.address.city;

if (order !== null && order !== undefined && order.items) {
  processItems(order.items);
}

const name = callback ? callback() : undefined;
```

### Good
```javascript
const city = user?.address?.city;

if (order?.items) {
  processItems(order.items);
}

const name = callback?.();
```

### Edge
Rare case: when business logic must differentiate between `null` (explicitly set to nothing) and `undefined` (never set), a manual check like `if (value === null)` is appropriate since `?.` collapses both to `undefined`.
