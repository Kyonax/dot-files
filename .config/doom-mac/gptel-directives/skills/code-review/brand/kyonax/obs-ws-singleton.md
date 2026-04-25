---
id: rule-br-kyo-006
title: OBS-WS Composable Must Be Module-Level Singleton
severity: CRITICAL
tags: obs, websocket, singleton, module-level, composable
---

Every OBS-WS composable must be a module-level singleton; declare `let shared_state = null` at module scope and return the same instance on every call.

### Apply
- Any `useObs*` or `useStream*` composable that connects to OBS WebSocket or manages shared OBS state.

### Skip
- Composables that are intentionally per-component (e.g., local UI animation state with no shared connection).

### Bad
```javascript
// Creates a new connection per component mount
export function useObsAudio() {
  const ws = new OBSWebSocket();
  const levels = ref([]);
  ws.connect('ws://localhost:4455');
  return { levels };
}
```

### Good
```javascript
let shared_state = null;

export function useObsAudio() {
  if (shared_state) {
    return shared_state;
  }

  const ws = new OBSWebSocket();
  const levels = ref([]);
  ws.connect('ws://localhost:4455');

  shared_state = { levels };
  return shared_state;
}
```

### Edge
If a composable needs to support reconnection, reset the internal websocket reference but keep the same `shared_state` object — consumers must not lose their reference.
