---
id: rule-u-ada-020
title: Comment on :deep() Overrides
severity: LOW
tags: deep, override, comment, sdk, design-system
---

`:deep()` overrides targeting third-party or shared design system components must include a comment explaining why the override is needed and identifying the SDK or library version.

### Apply
- `:deep()` selectors targeting components from an external design system or SDK
- Scoped style overrides on third-party widget or library components

### Skip
- `:deep()` used on your own child components within the same project
- Overrides on standard HTML elements

### Bad
```html
<style scoped>
/* No explanation — future devs won't know why or if it's still needed */
:deep(.ds-modal__overlay) {
  z-index: 9999;
}
</style>
```

### Good
```html
<style scoped>
/*
 * Override: design-system v3.2.1 — ds-modal sets z-index: 100 which
 * renders behind our sticky header (z-index: 200). Upstream fix requested
 * in DS-1234. Remove when design-system >= v3.3.0.
 */
:deep(.ds-modal__overlay) {
  z-index: 9999;
}
</style>
```

### Edge
This rule applies only to overrides on third-party or shared design system components where the source is not under your direct control. For your own child components, `:deep()` is a normal scoping mechanism and does not require special documentation. The comment should include: (1) the library name and version, (2) why the override is necessary, and (3) conditions for removal.
