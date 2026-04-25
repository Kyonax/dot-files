---
id: rule-u-sdk-001
title: Scope SDK DOM Queries to Refs
severity: HIGH
tags: sdk, deep, refs, dom, scope, querySelector
---

Scope SDK DOM queries to `$refs` or a container ref instead of using `document.querySelector` globally, and include a comment identifying the SDK name and version.

### Apply
- Any code that queries or manipulates DOM elements injected by a third-party SDK

### Skip
- SDK initialization calls that require a global selector as part of their documented API (e.g., `SDK.init({ container: '#global-root' })`)

### Bad
```js
// Somewhere in component methods
const widget = document.querySelector('.sdk-widget-frame');
widget.style.display = 'block';
```

### Good
```js
// ThirdPartyWidget SDK v2.4 — scoped to component ref
const container = this.$refs.sdkContainer;
const widget = container.querySelector('.sdk-widget-frame');
if (widget) {
  widget.style.display = 'block';
}
```

### Edge
When multiple instances of the same SDK widget exist on a single page, global `document.querySelector` will always return the first match, silently targeting the wrong instance. Scoping to a component ref guarantees each instance operates on its own DOM subtree.
