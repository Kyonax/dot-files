---
id: rule-br-kyo-008
title: No onUnmounted Cleanup on Singletons
severity: CRITICAL
tags: singleton, unmount, cleanup, page-lifetime, onUnmounted
---

Never register `onUnmounted` cleanup on singleton composables — they live for the entire page lifetime and must not be torn down when a component unmounts.

### Apply
- Any singleton composable (rule-br-kyo-006) that registers `onUnmounted`, `onBeforeUnmount`, or similar lifecycle teardown.

### Skip
- Per-component composables that are not singletons and genuinely need cleanup.

### Bad
```javascript
let shared_state = null;

export function useObsAudio() {
  if (shared_state) { return shared_state; }

  const ws = new OBSWebSocket();
  ws.connect('ws://localhost:4455');

  // WRONG: first component to unmount kills the shared connection
  onUnmounted(() => {
    ws.disconnect();
    shared_state = null;
  });

  shared_state = { ws };
  return shared_state;
}
```

### Good
```javascript
let shared_state = null;

export function useObsAudio() {
  if (shared_state) { return shared_state; }

  const ws = new OBSWebSocket();
  ws.connect('ws://localhost:4455');

  // No onUnmounted — connection persists for page lifetime
  shared_state = { ws };
  return shared_state;
}
```

### Edge
If a full app teardown is needed (e.g., in tests), expose an explicit `_destroy()` function gated behind `import.meta.env.TEST` — never hook it to component lifecycle.
