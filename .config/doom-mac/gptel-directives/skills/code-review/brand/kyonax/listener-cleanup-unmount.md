---
id: rule-br-kyo-019
title: Clear Timers on Unmount
severity: MEDIUM
tags: listener, cleanup, unmount, clearTimeout, timer
---

Clear all timers and remove event listeners on component unmount via `onUnmounted` guard.

### Apply
- Any per-component `setTimeout`, `setInterval`, or `addEventListener` on `window`/`document`.

### Skip
- Singleton composables (rule-br-kyo-008) — they must NOT register onUnmounted cleanup.

### Bad
```javascript
// Timer leaks if component unmounts before it fires
let timer = setTimeout(recalculate, 100);

window.addEventListener('resize', onResize);
```

### Good
```javascript
let timer = null;

function scheduleRecalc() {
  clearTimeout(timer);
  timer = setTimeout(recalculate, 100);
}

window.addEventListener('resize', scheduleRecalc);

onUnmounted(() => {
  clearTimeout(timer);
  window.removeEventListener('resize', scheduleRecalc);
});
```

### Edge
If a component uses both a singleton composable and local timers, only the local timers get `onUnmounted` cleanup. The singleton's connection/state is untouched — this distinction is critical.
