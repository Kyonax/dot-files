---
id: rule-br-kyo-015
title: Bypass Vue Reactivity in Per-Frame Hot Paths
severity: CRITICAL
tags: reactivity, bypass, template-ref, dom-write, style
---

Bypass Vue reactivity in per-frame hot paths; use template refs and direct DOM writes (`el.style.transform = ...`) instead of reactive bindings.

### Apply
- Any data-driven style/attribute that updates at >10 Hz (meter bars, volume levels, position readouts).

### Skip
- State that changes at low frequency (scene name, connection status, user settings) where reactive bindings are fine.

### Bad
```vue
<template lang="pug">
  //- Reactive binding triggers Vue patch cycle 50x/sec
  .meter-bar(:style="{ transform: `scaleY(${level})` }")
</template>

<script setup>
const level = ref(0);
ws.on('InputVolumeMeters', (data) => {
  level.value = data.inputs[0].levels[0];
});
</script>
```

### Good
```vue
<template lang="pug">
  .meter-bar(ref="barEl")
</template>

<script setup>
const barEl = ref(null);

ws.on('InputVolumeMeters', (data) => {
  if (barEl.value) {
    barEl.value.style.transform = SCALE_STRINGS[Math.round(data.inputs[0].levels[0] * 100)];
  }
});
</script>
```

### Edge
If the component must expose the current value to a parent (e.g., for a diagnostic panel), expose it via a non-reactive property on the composable return object, not a `ref`. Consumers use `watch(tick)` (rule-br-kyo-017) to know when to read it.
