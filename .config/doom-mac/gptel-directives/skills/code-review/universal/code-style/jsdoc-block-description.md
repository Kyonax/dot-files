---
id: rule-u-cs-006
title: JSDoc Block Description
severity: MEDIUM
tags: jsdoc, description, param, returns, Function
---

Every JSDoc block that contains `@param` or `@returns` tags must include a description line before the first tag; tags alone fail most linters and provide no context.

### Apply
- Any JSDoc block attached to a function, method, or class that uses `@param` or `@returns`

### Skip
- Simple single-line JSDoc descriptions with no tags (e.g., `/** Resets the counter. */`)

### Bad
```javascript
/**
 * @param {string} id
 * @returns {Object}
 */
function getUser(id) {
  return db.find(id);
}
```

### Good
```javascript
/**
 * Retrieves a user record by their unique identifier.
 * @param {string} id - The user's unique identifier.
 * @returns {Object} The user record, or null if not found.
 */
function getUser(id) {
  return db.find(id);
}
```

### Edge
Type casing matters: use `Function` (capital F) not `function` when referencing a callback or function type in JSDoc. Use `Object` (capital O) for generic objects. Lowercase `function` and `object` are valid JavaScript types but are flagged by most JSDoc linters as incorrect in type annotations.
