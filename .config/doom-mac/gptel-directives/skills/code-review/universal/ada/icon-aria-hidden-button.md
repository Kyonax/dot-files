---
id: rule-u-ada-018
title: Icons Inside Labeled Controls Are Decorative
severity: MEDIUM
tags: icon, aria-hidden, button, decorative, svg
---

Icons inside buttons or links that already have visible text or an `aria-label` are decorative and must have `aria-hidden="true"`.

### Apply
- SVG or icon-font elements inside a `<button>` or `<a>` with visible text
- Icon components inside controls with an `aria-label`

### Skip
- Icons that are the sole content of a button (these need their own `aria-label` on the button, not `aria-hidden`)

### Bad
```html
<!-- SVG is exposed to AT — screen reader says "shopping cart icon, Add to cart" -->
<button>
  <svg role="img" aria-label="shopping cart icon">...</svg>
  Add to cart
</button>
```

### Good
```html
<!-- SVG hidden from AT — screen reader says "Add to cart" -->
<button>
  <svg aria-hidden="true" focusable="false">...</svg>
  Add to cart
</button>
```

### Edge
Any SVG or icon component inside a button that already has an `aria-label` or visible text is decorative. The icon duplicates information the label already provides. Also add `focusable="false"` on SVGs to prevent IE/Edge legacy from adding the SVG to the tab order.
