---
id: rule-br-kyo-012
title: Precompute Lookup Tables
severity: HIGH
tags: lookup, table, precompute, jitter, scale-strings, math-random
---

Precompute lookup tables at module init; replace per-frame `Math.random()` with `JITTER_TABLE` and per-frame string concatenation with `SCALE_STRINGS`.

### Apply
- Any hot-path code that calls `Math.random()`, `Math.sin()`, `Math.cos()`, or builds strings (template literals, `.toFixed()`) on every frame/event.

### Skip
- One-time setup computations or user-triggered actions.

### Bad
```javascript
// Per-frame string build + Math.random
function updateMeter(el, value) {
  const jitter = Math.random() * 0.02;
  el.style.transform = `scaleY(${(value + jitter).toFixed(3)})`;
}
```

### Good
```javascript
// Precomputed at module init
const JITTER_TABLE = new Float32Array(256);
for (let i = 0; i < 256; i++) {
  JITTER_TABLE[i] = Math.random() * 0.02;
}

const SCALE_STRINGS = new Array(101);
for (let i = 0; i <= 100; i++) {
  SCALE_STRINGS[i] = `scaleY(${(i / 100).toFixed(2)})`;
}

let jitterIdx = 0;

function updateMeter(el, value) {
  const idx = Math.round(value * 100) | 0;
  jitterIdx = (jitterIdx + 1) & 255;
  el.style.transform = SCALE_STRINGS[Math.min(idx, 100)];
}
```

### Edge
For continuous float values that cannot be bucketed into 101 steps, use the `JITTER_TABLE` approach but write the transform string directly via a reusable char buffer or `el.style.setProperty`.
