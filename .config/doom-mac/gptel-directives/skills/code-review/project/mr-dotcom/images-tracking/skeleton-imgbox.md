---
id: rule-pj-mrd-041
title: Skeleton Loading via ImgBox Deep Selector
severity: MEDIUM
tags: skeleton, imgbox, deep, background, ui-color-4
---

Skeleton placeholder for ImgBox must be applied via `:deep(.image-box)` with `background-color: ui-color-4`.

### Apply
- Any ImgBox that needs a loading skeleton before the image loads

### Skip
- ImgBox instances where the parent background already provides sufficient contrast

### Bad
```javascript
<style lang="stylus" scoped>
.hero-image
  background-color #e0e0e0
  min-height 200px
</style>
```

### Good
```javascript
<style lang="stylus" scoped>
.hero-image
  :deep(.image-box)
    background-color ui-color-4
</style>
```

### Edge
If a custom skeleton animation (shimmer effect) is needed, still apply it via `:deep(.image-box)` rather than wrapping ImgBox in an extra skeleton div.
