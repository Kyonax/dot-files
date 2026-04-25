---
id: rule-br-kyo-002
title: Group Halo on Single Container
severity: HIGH
tags: halo, container, group, filter, hud-frame
---

Apply halo/glow filter on a single container (`HudFrame`) rather than on each leaf element inside it.

### Apply
- Multiple sibling elements each declaring their own filter or box-shadow for the same visual halo effect.

### Skip
- Elements that genuinely need independent, differently-styled halos (e.g., alert vs. info indicators).

### Bad
```vue
<template lang="pug">
  .meter(style="filter: blur(2px)")
  .label(style="filter: blur(2px)")
  .value(style="filter: blur(2px)")
</template>
```

### Good
```vue
<template lang="pug">
  HudFrame.hud-group
    .meter
    .label
    .value
</template>

<style scoped>
.hud-group {
  filter: var(--hud-halo, none);
}
</style>
```

### Edge
If one child needs a stronger glow than the group, layer it with an additional custom property override on that child only — don't duplicate the group filter on every sibling.
