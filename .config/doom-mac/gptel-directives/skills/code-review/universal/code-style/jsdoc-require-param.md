---
id: rule-u-cs-008
title: JSDoc @param — Declare Every Parameter
severity: HIGH
tags: jsdoc, param, lint, eslint, pikolint, require-param
---

When a function carries any JSDoc block, every formal parameter MUST be declared with its own `@param` tag. Project ESLint configs (`jsdoc/require-param`) — including Madison Reed's Pikolint pre-commit hook — flag each missing declaration as a separate warning. A function with three parameters and no `@param` tags will surface three review comments on the PR.

The complementary rule `@returns` MUST be declared whenever the function returns a non-void value.

### Apply

- Any `export`ed function in `website/src/` (utilities, services, store actions, mixins, plugins).
- Any non-trivial internal function whose JSDoc block already includes a description line (the rule fires once a JSDoc block exists for the function — it does not fire on functions with no JSDoc at all).
- All shared modules under `mr_modules/` consumed by 4+ apps — the project's coding-standards.md explicitly enforces JSDoc here.

### Skip

- Arrow callbacks passed inline to higher-order functions (`array.map(x => x.id)`).
- Functions with no JSDoc block at all — adding partial JSDoc triggers the rule; either go all-in or omit JSDoc entirely.
- `*.test.js` files — JSDoc rules are disabled in tests per project ESLint overrides.
- Trivial 0-arg functions (`function reset() { /* … */ }`).

### Bad

```javascript
/** Idempotent `?promo=<code>` append. */
export function appendPromoToUrl(url, code, name) {
  // ...
}
```

Pikolint emits three warnings on this single function:

- `Missing JSDoc @param "url" declaration.`
- `Missing JSDoc @param "code" declaration.`
- `Missing JSDoc @param "name" declaration.`

### Good

```javascript
/**
 * Idempotent `?promo=<code>` append. URL-encodes; no-ops if `promo=` already set.
 * @param {string} url
 * @param {string} code
 * @param {string} [name]
 * @returns {string}
 */
export function appendPromoToUrl(url, code, name) {
  // ...
}
```

### Concise variant (preferred for utility modules)

When the function's description already says it all, the `@param` block can omit per-parameter prose. Keep tags terse:

```javascript
/**
 * Strips `promo` and `promoName` query params from the URL.
 * @param {string} url
 * @returns {string}
 */
export function stripPromoFromUrl(url) { /* … */ }
```

### Edge

- **Optional parameters:** Wrap the name in brackets: `@param {string} [name]`. Default values use `@param {string} [name='default']`.
- **Destructured parameters:** Use a single `@param` for the whole object plus one per key:
  ```javascript
  /**
   * @param {{code: string, name: string}} payload
   * @param {string} payload.code
   * @param {string} payload.name
   */
  function setPendingPromo({ code, name }) { /* … */ }
  ```
- **Rest parameters:** `@param {...string} args`.
- **Removing JSDoc to silence the rule is acceptable** when the function name + parameter names already carry the meaning. Half-finished JSDoc (description but no `@param`) is worse than no JSDoc.

### Why HIGH severity

These warnings appear as **per-parameter inline comments on the PR**. A single under-documented utility with 3-5 params spams the diff with multiple bot comments, which:

1. Buries human review feedback under lint noise.
2. Requires either fixing or marking-as-resolved on each comment.
3. Blocks the `github-actions` review from APPROVING (Madison Reed's Pikolint runs as a `CHANGES_REQUESTED` reviewer when warnings exist).

Always either fully document or fully omit. The PR feedback loop is too expensive to leave half-JSDoc in.
