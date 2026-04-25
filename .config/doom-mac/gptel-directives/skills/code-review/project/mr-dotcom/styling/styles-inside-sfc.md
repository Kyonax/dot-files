---
id: rule-pj-mrd-021
title: Styles Inside the SFC
severity: MEDIUM
tags: styles, sfc, external, styl, css
---

All component styles must live inside the Vue SFC's `<style>` block; no external `.styl` or `.css` files.

### Apply
- All Vue components under `website/src/vuescripts/`

### Skip
- Global style entry points imported in the app bootstrap (e.g., shared Stylus variable files)

### Bad
```
// HeroSection.vue
<style scoped lang="stylus">
@import './HeroSection.styl'
</style>

// HeroSection.styl (separate file)
.hero
  background brand-color-white
```

### Good
```vue
// HeroSection.vue
<style scoped lang="stylus">
.hero
  background brand-color-white
</style>
```

### Edge
Shared Stylus variable and mixin files (e.g., `variables.styl`, `mixins.styl`) may be imported. The rule prohibits externalizing a component's own styles into a sibling file, not shared design-system abstractions.
