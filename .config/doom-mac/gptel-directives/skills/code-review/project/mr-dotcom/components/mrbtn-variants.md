---
id: rule-pj-mrd-050
title: Use MrBtn Variants Before Custom CSS
severity: MEDIUM
tags: MrBtn, variant, secondary, tertiary, light, button
---

Use built-in MrBtn variant props (`secondary`, `tertiary`, `light`) before writing custom button CSS.

### Apply
- Every button rendered in a Vue template

### Skip
- Non-button interactive elements (links styled as buttons still use MrBtn where possible)

### Bad
```javascript
<template lang="pug">
MrBtn.custom-outline-btn(:label="cta")
</template>

<style lang="stylus" scoped>
.custom-outline-btn
  :deep(.mrbtn)
    background-color transparent
    border 1px solid ui-color-1
    color ui-color-1
</style>
```

### Good
```javascript
<template lang="pug">
MrBtn(:label="cta" secondary)
</template>
```

### Edge
When a design requires a button style that genuinely does not match any existing variant (e.g., a colored background not in the palette), custom CSS via `:deep(.mrbtn)` is acceptable. Document why no variant fits.
