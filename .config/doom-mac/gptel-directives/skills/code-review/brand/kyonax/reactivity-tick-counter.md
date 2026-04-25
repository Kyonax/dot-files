---
id: rule-br-kyo-017
title: Expose tick Counter from Event-Driven Composable
severity: HIGH
tags: tick, counter, ref, watch, non-reactive
---

Expose a `tick: ref(0)` counter from event-driven composables; consumers `watch(tick)` and read non-reactive data synchronously.

### Apply
- Any singleton composable that receives high-frequency data (>10 Hz) and needs to signal consumers without making the data itself reactive.

### Skip
- Composables whose data is already low-frequency and safely reactive.

### Bad
```javascript
// Makes all levels reactive — every consumer triggers a Vue patch on every event
export function useObsAudio() {
  const levels = ref([]);
  ws.on('InputVolumeMeters', (data) => {
    levels.value = data.inputs.map(i => i.levels[0]);
  });
  return { levels };
}
```

### Good
```javascript
export function useObsAudio() {
  const tick = ref(0);
  const levels = new Float32Array(6); // non-reactive

  ws.on('InputVolumeMeters', (data) => {
    for (let i = 0; i < 6; i++) {
      levels[i] = data.inputs[i]?.levels[0] ?? -Infinity;
    }
    tick.value++;
  });

  return { tick, levels };
}

// Consumer:
const { tick, levels } = useObsAudio();
watch(tick, () => {
  // Read non-reactive levels synchronously
  updateLocalUI(levels);
});
```

### Edge
The `tick` counter will eventually overflow `Number.MAX_SAFE_INTEGER` at 50 Hz in ~5.7 million years. This is not a practical concern, but if you prefer, wrap at `0xFFFFFFFF`.
