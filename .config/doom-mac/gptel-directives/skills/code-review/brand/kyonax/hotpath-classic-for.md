---
id: rule-br-kyo-013
title: Classic for-Loop in Hot Paths
severity: HIGH
tags: for-loop, classic, array-find, destructuring, hot-path
---

Use classic `for(let i=0; i<len; i++)` loops in hot paths; avoid `Array.find`, `Array.map`, `Array.forEach`, `for...of`, and destructuring.

### Apply
- Any code running at >10 Hz (event handlers, animation frames, per-tick processing).

### Skip
- Setup-time code, user-triggered handlers, and cold paths where readability wins.

### Bad
```javascript
// Per-event (~50Hz): iterator protocol + closure overhead
ws.on('InputVolumeMeters', ({ inputs }) => {
  const mic = inputs.find(i => i.name === 'Mic/Aux');
  const { levels: [peak] } = mic;
  updateMeter(peak);
});
```

### Good
```javascript
ws.on('InputVolumeMeters', (data) => {
  const inputs = data.inputs;
  for (let i = 0; i < inputs.length; i++) {
    if (inputs[i].name === 'Mic/Aux') {
      updateMeter(inputs[i].levels[0]);
      break;
    }
  }
});
```

### Edge
If the array is guaranteed to have fewer than 4 elements, a classic loop is still preferred for consistency, but the performance difference is negligible — flag only as informational in review.
