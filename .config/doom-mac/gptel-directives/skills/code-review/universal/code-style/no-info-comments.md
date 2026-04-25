---
id: rule-u-cs-002
title: No Informational Comments
severity: LOW
tags: comment, jsdoc, todo
---

Do not write comments that restate what the code already says; keep only TODO, eslint-disable, and comments explaining genuinely non-obvious logic.

### Apply
- Inline comments that mirror the variable name, function name, or obvious operation
- Empty or near-empty JSDoc blocks (`/** */` or `/** @returns */` with no real info)

### Skip
- `// TODO:` or `// FIXME:` annotations
- `// eslint-disable-next-line` pragmas
- Comments explaining a workaround, business rule, or non-obvious algorithm

### Bad
```javascript
// Get the user
const user = getUser(id);

/** */
function reset() {
  this.count = 0;
}

// Set loading to true
this.loading = true;
```

### Good
```javascript
const user = getUser(id);

// TODO: Replace with batch endpoint once API v3 ships
function reset() {
  this.count = 0;
}

// Stripe requires amount in cents, not dollars
const amount = price * 100;
```

### Edge
JSDoc `@param` / `@returns` tags on public API functions are documentation, not informational comments — keep them when the function is exported or part of an interface.
