---
id: rule-br-kyo-004
title: Split Static Decoration from Animated Layer
severity: HIGH
tags: static, animated, layer, split, keyframe, box-shadow
---

Split static decoration from animated effects onto separate DOM elements; static halo on outer span (never in a keyframe), animated glow on inner span.

### Apply
- Any element that has both a static box-shadow/filter AND an animated transform/opacity on the same node.

### Skip
- Elements where the "static" decoration is trivially cheap (e.g., a 1px border with no blur).

### Bad
```vue
<template lang="pug">
  //- Static shadow + animation on the same element = repaint every frame
  .ring(:class="{ active }")
</template>

<style>
.ring {
  box-shadow: 0 0 8px var(--halo);
}
.ring.active {
  animation: pulse 1s infinite;
}
@keyframes pulse {
  0%   { transform: scale(1); box-shadow: 0 0 8px var(--halo); }
  100% { transform: scale(1.1); box-shadow: 0 0 16px var(--halo); }
}
</style>
```

### Good
```vue
<template lang="pug">
  //- Static halo on outer, animation on inner
  span.ring-halo
    span.ring-pulse(:class="{ active }")
</template>

<style>
.ring-halo {
  box-shadow: 0 0 8px var(--halo);
}
.ring-pulse.active {
  animation: pulse 1s infinite;
}
@keyframes pulse {
  0%   { transform: scale(1); opacity: 0.6; }
  100% { transform: scale(1.1); opacity: 1; }
}
</style>
```

### Edge
If the static decoration must visually track the animated element (e.g., halo scales with it), apply the same transform to both via a shared CSS custom property — but keep `box-shadow` off the keyframe.
