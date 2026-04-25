---
id: rule-pj-mrd-051
title: MrBtn Color Override via Deep Selector
severity: MEDIUM
tags: MrBtn, deep, mrbtn, color, override
---

Color overrides on MrBtn must use `:deep(.mrbtn)`, not a class on the MrBtn component tag itself.

### Apply
- Any MrBtn that needs color customization beyond the built-in variants

### Skip
- MrBtn instances using a built-in variant with no color override needed

### Bad
```javascript
<template lang="pug">
MrBtn.white-btn(:label="cta")
</template>

<style lang="stylus" scoped>
.white-btn
  color white
  background-color ui-color-1
</style>
```

### Good
```javascript
<template lang="pug">
.cta-wrapper
  MrBtn(:label="cta")
</template>

<style lang="stylus" scoped>
.cta-wrapper
  :deep(.mrbtn)
    color white
    background-color ui-color-1
</style>
```

### Edge
If multiple MrBtn instances in the same parent need different overrides, wrap each in a named container and apply `:deep(.mrbtn)` scoped to each wrapper rather than using classes directly on MrBtn.
