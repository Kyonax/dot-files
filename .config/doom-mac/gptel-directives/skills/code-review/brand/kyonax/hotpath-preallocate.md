---
id: rule-br-kyo-011
title: Preallocate Typed Arrays at Setup
severity: CRITICAL
tags: preallocate, typed-array, float32, mutate, allocation
---

Preallocate typed arrays (`Float32Array`) at setup time and mutate in place every tick; never allocate in a hot loop.

### Apply
- Any per-frame or per-event handler that processes numeric buffers (audio levels, volume meters, animation coordinates).

### Skip
- Cold paths that run once or on user action (e.g., initial config parsing).

### Bad
```javascript
ws.on('InputVolumeMeters', (data) => {
  // Allocates a new array every event (~50Hz) — GC pressure
  const levels = data.inputs.map(i => i.levels[0]);
  updateMeters(levels);
});
```

### Good
```javascript
const NUM_INPUTS = 6;
const levels = new Float32Array(NUM_INPUTS);

ws.on('InputVolumeMeters', (data) => {
  for (let i = 0; i < NUM_INPUTS; i++) {
    levels[i] = data.inputs[i]?.levels[0] ?? -Infinity;
  }
  updateMeters(levels);
});
```

### Edge
If the number of inputs is truly dynamic, preallocate for the maximum expected count and track an `activeCount` index — avoid `new Float32Array()` on every resize event.
