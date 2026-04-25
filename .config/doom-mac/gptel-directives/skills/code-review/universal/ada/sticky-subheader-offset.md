---
id: rule-u-ada-019
title: Sticky Subheader Dynamic Offset
severity: HIGH
tags: sticky, subheader, resize-observer, offset, top
---

Sticky elements positioned below a dynamic-height header must use `ResizeObserver` to compute their `top` offset at runtime.

### Apply
- `position: sticky` elements that sit below a site header whose height changes (e.g., promo banners, responsive nav collapse)
- Sub-navigation bars, filter bars, or sticky CTAs below the primary header

### Skip
- Sticky elements at `top: 0` with no preceding sticky/fixed elements
- Headers with a guaranteed fixed pixel height that never changes

### Bad
```html
<!-- Hardcoded top — breaks when header height changes -->
<nav class="sub-nav" style="position: sticky; top: 64px;">
  ...
</nav>
```

### Good
```html
<nav class="sub-nav" :style="{ position: 'sticky', top: headerHeight + 'px' }">
  ...
</nav>
```

```javascript
data() {
  return {
    headerHeight: 0,
  };
},
mounted() {
  const header = document.querySelector('.site-header');
  if (header) {
    this.resizeObserver = new ResizeObserver((entries) => {
      this.headerHeight = entries[0].contentRect.height;
    });
    this.resizeObserver.observe(header);
  }
},
beforeUnmount() {
  this.resizeObserver?.disconnect();
},
```

### Edge
Use a `:style` binding for the dynamic `top` value rather than a CSS custom property set via JavaScript. CSS custom properties require manual synchronization (`document.documentElement.style.setProperty`) and are harder to scope per-component. The `ResizeObserver` approach reacts to all header size changes including font scaling, banner dismissals, and viewport resizes.
