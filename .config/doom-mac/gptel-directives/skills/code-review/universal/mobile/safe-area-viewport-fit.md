---
id: rule-u-mob-002
title: Safe Area Requires viewport-fit=cover
severity: HIGH
tags: safe-area, env, viewport-fit, cover, ios
---

`env(safe-area-inset-*)` requires `viewport-fit=cover` in the viewport meta tag; without it, all `env()` values resolve to `0` and the CSS is dead code.

### Apply
- Any use of `env(safe-area-inset-top)`, `env(safe-area-inset-bottom)`, `env(safe-area-inset-left)`, or `env(safe-area-inset-right)` in stylesheets

### Skip
- Projects that intentionally avoid `viewport-fit=cover` because they do not target notched or Dynamic Island devices

### Bad
```html
<!-- viewport meta missing viewport-fit=cover -->
<meta name="viewport" content="width=device-width, initial-scale=1">
```
```css
.bottom-bar {
  padding-bottom: env(safe-area-inset-bottom);
  /* always 0 — viewport-fit=cover is missing */
}
```

### Good
```html
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
```
```css
.bottom-bar {
  padding-bottom: env(safe-area-inset-bottom);
}
```

### Edge
Adding `viewport-fit=cover` changes the layout model for the entire page — content will extend behind the status bar and home indicator. Test thoroughly on notched devices after enabling it, and ensure all fixed/sticky elements account for the safe area insets.
