---
id: rule-br-kyo-018
title: Debounce Burst-Prone Window Listeners
severity: HIGH
tags: debounce, resize, scroll, setTimeout, 100ms
---

Debounce burst-prone `window.*` listeners (`resize`, `scroll`) by ~100 ms to avoid layout thrashing.

### Apply
- Any `addEventListener('resize', ...)` or `addEventListener('scroll', ...)` on `window` or a scrollable container.

### Skip
- Listeners that already use `IntersectionObserver` or `ResizeObserver` (they handle their own batching).

### Bad
```javascript
// Fires dozens of times per resize drag — layout thrash
window.addEventListener('resize', () => {
  recalculateLayout();
});
```

### Good
```javascript
let resizeTimer = null;

window.addEventListener('resize', () => {
  clearTimeout(resizeTimer);
  resizeTimer = setTimeout(recalculateLayout, 100);
});
```

### Edge
For scroll-driven animations that need smooth visual feedback, use `passive: true` on the listener and `requestAnimationFrame` gating instead of `setTimeout` — but never both simultaneously.
