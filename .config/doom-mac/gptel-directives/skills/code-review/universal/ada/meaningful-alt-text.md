---
id: rule-u-ada-012
title: Meaningful Alt Text
severity: MEDIUM
tags: alt, alt-text, image, decorative
---

Never use `alt=""` unless the image is purely decorative; functional images must have descriptive alt text that conveys meaning or purpose.

### Apply
- `<img>` elements that convey information, link destinations, or actions
- Icons used as the sole content of a button or link

### Skip
- Truly decorative images (background flourishes, spacers, redundant illustrations next to text that already describes them)

### Bad
```html
<!-- Product image with empty alt — screen readers skip it entirely -->
<img src="red-lipstick.jpg" alt="" />

<!-- Generic alt that describes nothing -->
<img src="chart-q4.png" alt="image" />
```

### Good
```html
<!-- Descriptive alt conveying what the image shows -->
<img src="red-lipstick.jpg" alt="Classic Red matte lipstick, open tube" />

<!-- Chart image with meaningful summary -->
<img src="chart-q4.png" alt="Q4 revenue chart showing 15% growth" />
```

### Edge
CMS-sourced images should pull their alt text from the CMS `alt_text` field rather than hardcoding it. If the CMS field is empty, flag it as a content issue rather than silently falling back to `alt=""`.
