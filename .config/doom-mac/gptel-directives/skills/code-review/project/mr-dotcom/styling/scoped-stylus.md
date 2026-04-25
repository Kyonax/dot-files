---
id: rule-pj-mrd-010
title: Scoped Stylus Required
severity: CRITICAL
tags: scoped, stylus, style, lang
---

All Vue SFC style blocks must use scoped Stylus: `<style scoped lang="stylus">`.

### Apply
- Every `<style>` block in `.vue` files under `website/src/vuescripts/`

### Skip
- Global style entry points (e.g., `website/src/vuescripts/styles/` or app-level imports)

### Bad
```vue
<style lang="stylus">
.hero
  background white
</style>
```

```vue
<style scoped>
.hero {
  background: white;
}
</style>
```

### Good
```vue
<style scoped lang="stylus">
.hero
  background white
</style>
```

### Edge
A component may have both a scoped block and an unscoped block if `:deep()` cannot reach a third-party element. The unscoped block must be minimal and commented with the reason.
