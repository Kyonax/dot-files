---
id: rule-br-kyo-016
title: Write-Threshold Skip for DOM Updates
severity: HIGH
tags: write-threshold, skip, math-abs, threshold
---

Apply a write-threshold skip: if `|next - last| < 0.01`, skip the DOM write entirely.

### Apply
- Any direct DOM write (style.transform, style.opacity, setAttribute) that fires on every tick/event in a hot path.

### Skip
- Discrete state changes (on/off, visible/hidden) where the value is categorical, not continuous.

### Bad
```javascript
// Writes on every event even if the value barely changed
ws.on('InputVolumeMeters', (data) => {
  const val = data.inputs[0].levels[0];
  barEl.value.style.transform = `scaleY(${val})`;
});
```

### Good
```javascript
let lastWritten = -1;
const THRESHOLD = 0.01;

ws.on('InputVolumeMeters', (data) => {
  const val = data.inputs[0].levels[0];
  if (Math.abs(val - lastWritten) < THRESHOLD) { return; }
  lastWritten = val;
  barEl.value.style.transform = SCALE_STRINGS[Math.round(val * 100)];
});
```

### Edge
For values that naturally plateau (e.g., silence = 0), the threshold prevents zero-jitter writes. Ensure `lastWritten` is initialized to a sentinel value (`-1` or `-Infinity`) so the first write always fires.
