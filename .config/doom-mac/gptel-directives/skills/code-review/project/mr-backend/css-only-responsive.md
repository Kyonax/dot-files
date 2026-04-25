---
id: rule-pj-mrb-008
title: CSS-Only Responsive Layouts for SSR Compatibility
severity: MEDIUM
tags: css, responsive, matchMedia, hydration, ssr, column-count
---

Use CSS `column-count`, `grid`, or media queries for responsive layout changes instead of JS `matchMedia` — SSR has no window object and will produce hydration mismatches.

### Apply
- Component needs different column counts or layouts at different breakpoints
- Using `window.matchMedia` or `window.innerWidth` to conditionally render different template structures
- SSR output differs from client initial render, causing hydration warnings

### Skip
- Behavior-only JS that does not affect rendered DOM structure (e.g., analytics, scroll listeners added in `onMounted`)
- Components guarded with `v-if="isMounted"` that intentionally skip SSR

### Bad
```javascript
// JS-driven responsive layout — breaks SSR
data() {
  return {
    columns: 3,
  };
},
mounted() {
  const mq = window.matchMedia('(max-width: 768px)');
  this.columns = mq.matches ? 1 : 3;
  mq.addEventListener('change', (e) => {
    this.columns = e.matches ? 1 : 3;
  });
},
```
```pug
//- Template renders different DOM based on JS state
.grid(:style="{ columnCount: columns }")
  slot
```

### Good
```stylus
// CSS-only responsive — works identically in SSR and client
.product-grid
  column-count 3
  column-gap 1.5rem

  @media (max-width: 768px)
    column-count 1
```
```pug
//- Template is static — CSS handles the responsive behavior
.product-grid
  slot
```

### Edge
If the responsive change requires fundamentally different DOM structures (not just layout changes), use `v-if="isMounted"` to defer the client-specific branch and provide a reasonable SSR default. This is acceptable for progressive enhancement but should be the exception, not the rule. Most layout differences (columns, grid areas, show/hide) can be solved with CSS alone.
