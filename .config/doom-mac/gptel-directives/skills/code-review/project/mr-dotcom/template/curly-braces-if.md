---
id: rule-pj-mrd-004
title: Curly Braces for If Statements
severity: MEDIUM
tags: curly, braces, if, one-line, brackets
---

Always use curly braces for if/else statements, even for single-line bodies; no braceless one-liners.

### Apply
- All JavaScript in `<script>` blocks and all `.js` files in the project

### Skip
- Ternary expressions (these are expressions, not if statements)

### Bad
```js
if (isVisible) return;

if (product.isSoldOut)
  showNotification('Sold out');

if (cart.isEmpty) redirect('/shop');
else loadCart();
```

### Good
```js
if (isVisible) {
  return;
}

if (product.isSoldOut) {
  showNotification('Sold out');
}

if (cart.isEmpty) {
  redirect('/shop');
} else {
  loadCart();
}
```

### Edge
Arrow function bodies that are single expressions do not need braces (e.g., `items.filter(i => i.active)`). This rule targets `if`/`else if`/`else` control-flow blocks only.
