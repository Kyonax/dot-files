---
id: rule-u-sc-008
title: Singleton No Race
severity: MEDIUM
tags: singleton, global, flag, race-condition
---

A global singleton flag is NOT a race condition when its consumers cannot co-exist on the same page; verify the routing and component tree before flagging it as unsafe.

### Apply
- Code reviews where a reviewer sees a module-level boolean or object and suspects a race condition
- Singleton patterns guarding initialization of a resource that is consumed by only one route or component at a time

### Skip
- True shared-state scenarios where multiple consumers can mount simultaneously and compete for the same flag

### Bad
```javascript
// Reviewer comment: "This is a race condition — two components could set initialized = true"
// But only ONE of PageA or PageB can be mounted at a time (route-level exclusivity)

// ❌ False positive flag — this is safe
let initialized = false;

export function init(config) {
  if (initialized) return;
  initialized = true;
  setup(config);
}
```

### Good
```javascript
// Reviewer correctly identifies: both WidgetA and WidgetB can appear on the same page
// and both call init() on mount — this IS a real race

let initialized = false;

export function init(config) {
  if (initialized) return; // Second widget silently skipped — bug
  initialized = true;
  setup(config);
}

// Fix: make init() idempotent or use per-instance state
```

### Edge
Only flag a singleton as a race condition if you can demonstrate that two consumers can mount simultaneously on the same page. Route-exclusive components, modals that replace each other, and tab-based views with destroy-on-switch are all safe singleton consumers.
