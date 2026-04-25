---
id: rule-pj-mrd-042
title: Deep img Selector for Aspect and Shape
severity: MEDIUM
tags: deep, img, aspect-ratio, border-radius, object-fit
---

Use `:deep(img)` to apply `aspect-ratio`, `border-radius`, and `object-fit` on images rendered by ImgBox.

### Apply
- Any ImgBox where the image needs custom aspect ratio, rounded corners, or cropping behavior

### Skip
- ImgBox instances using default rendering with no shape or aspect customization

### Bad
```javascript
<style lang="stylus" scoped>
.product-image
  aspect-ratio 16 / 9
  border-radius 8px
  overflow hidden
</style>
```

### Good
```javascript
<style lang="stylus" scoped>
.product-image
  :deep(img)
    aspect-ratio 16 / 9
    border-radius 8px
    object-fit cover
</style>
```

### Edge
When both `.image-box` (for background/skeleton) and `img` (for shape) need styling, use two separate `:deep()` selectors rather than nesting them.
