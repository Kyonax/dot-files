---
id: rule-br-kyo-009
title: Event-Driven Over requestAnimationFrame
severity: HIGH
tags: event-driven, raf, requestAnimationFrame, clock, 50hz
---

Use event-driven updates instead of `requestAnimationFrame` polling when the data source already emits at a known frequency (~50 Hz for OBS-WS).

### Apply
- Any composable or component that uses `requestAnimationFrame` to read data that arrives via OBS WebSocket events.

### Skip
- Visual animations that need frame-synced rendering (e.g., canvas drawing, interpolated transforms) with no external event source.

### Bad
```javascript
// Two competing clocks: OBS emits at ~50Hz, rAF runs at ~60Hz
function poll() {
  levels.value = lastReceivedLevels;
  requestAnimationFrame(poll);
}
requestAnimationFrame(poll);
```

### Good
```javascript
// One clock: OBS event drives updates directly
ws.on('InputVolumeMeters', (data) => {
  processLevels(data);
});
```

### Edge
If you need to interpolate between OBS events for smoother visual output, use a single rAF loop that reads the latest event data — but the event handler must still be the authority on when new data arrives.
