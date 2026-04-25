---
id: rule-br-kyo-014
title: Hardcode Known Targets
severity: MEDIUM
tags: hardcode, target, configurable, mic-aux
---

Hardcode known targets (e.g., `'Mic/Aux'`) when the configurable option is never exercised; avoid indirection overhead in hot paths.

### Apply
- Hot-path code that looks up a target by a configurable variable, but the variable has only one known value in the entire codebase.

### Skip
- Code where the target is genuinely user-configurable or varies across deployments/scenes.

### Bad
```javascript
// config.targetInput is always 'Mic/Aux' — indirection adds property lookup per tick
const TARGET = config.targetInput;

ws.on('InputVolumeMeters', (data) => {
  for (let i = 0; i < data.inputs.length; i++) {
    if (data.inputs[i].name === TARGET) {
      updateMeter(data.inputs[i].levels[0]);
      break;
    }
  }
});
```

### Good
```javascript
ws.on('InputVolumeMeters', (data) => {
  for (let i = 0; i < data.inputs.length; i++) {
    if (data.inputs[i].name === 'Mic/Aux') {
      updateMeter(data.inputs[i].levels[0]);
      break;
    }
  }
});
```

### Edge
If a second target is ever added, refactor to a `Set` lookup at that point. Premature configurability in hot paths costs cycles now for flexibility that may never be needed.
