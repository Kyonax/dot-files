---
id: rule-u-cs-004
title: Simple Expressions
severity: LOW
tags: expression, one-line, return
---

Do not split a trivial return or expression across multiple lines when it fits clearly on one.

### Apply
- Single-value returns, simple ternaries, short boolean expressions

### Skip
- Expressions with 3+ chained operations, nested ternaries, or lines exceeding the project's print-width

### Bad
```javascript
function isActive(user) {
  const status = user.active;
  return status;
}

const label = (
  isOpen
    ? 'Open'
    : 'Closed'
);
```

### Good
```javascript
function isActive(user) {
  return user.active;
}

const label = isOpen ? 'Open' : 'Closed';
```

### Edge
If the one-liner obscures intent (e.g., a ternary inside a template literal inside a function argument), prefer the multi-line form for readability.
