---
id: rule-u-mob-001
title: Use 100dvh Not 100vh
severity: HIGH
tags: 100dvh, 100vh, viewport, ios-safari, mobile
---

Use `100dvh` instead of `100vh` for full-viewport mobile layouts because iOS Safari's URL bar, Dynamic Island, and home indicator make `100vh` taller than the visible area.

### Apply
- Any element intended to fill the entire visible viewport height on mobile (hero sections, fullscreen overlays, splash screens)

### Skip
- Desktop-only layouts where the viewport height is stable and no mobile rendering is expected

### Bad
```css
.fullscreen-hero {
  height: 100vh;
}
```

### Good
```css
.fullscreen-hero {
  height: 100vh;    /* fallback for older browsers */
  height: 100dvh;
}
```

### Edge
`100dvh` is not supported in older browsers (Safari < 15.4, Chrome < 108). Always provide a `height: 100vh` fallback immediately before the `100dvh` declaration so the cascade handles both cases gracefully.
