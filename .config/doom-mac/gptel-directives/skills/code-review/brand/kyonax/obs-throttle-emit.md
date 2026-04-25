---
id: rule-br-kyo-010
title: Throttle $emit on Hot Paths to ~10 Hz
severity: HIGH
tags: throttle, emit, 10hz, diagnostic, performance
---

Throttle `$emit` from hot paths to ~10 Hz for diagnostic/readout components; 50 Hz event intake does not mean 50 Hz Vue updates.

### Apply
- Any component that emits events in response to OBS-WS data arriving at >10 Hz (e.g., audio meters, scene stats).

### Skip
- User-initiated events (click, submit) that are inherently low-frequency.

### Bad
```javascript
// Emits at full OBS rate (~50Hz) — 50 Vue update cycles per second
ws.on('InputVolumeMeters', (data) => {
  emit('levels-update', data);
});
```

### Good
```javascript
let lastEmit = 0;
const THROTTLE_MS = 100; // ~10Hz

ws.on('InputVolumeMeters', (data) => {
  processInternally(data);
  const now = performance.now();
  if (now - lastEmit >= THROTTLE_MS) {
    lastEmit = now;
    emit('levels-update', latestProcessed);
  }
});
```

### Edge
For diagnostic panels that are hidden by default, gate the emit behind a visibility check — zero emits when the panel is collapsed.
