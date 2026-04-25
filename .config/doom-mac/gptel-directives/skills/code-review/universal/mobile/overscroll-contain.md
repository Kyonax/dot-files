---
id: rule-u-mob-005
title: Overscroll Contain on Scroll Overlays
severity: MEDIUM
tags: overscroll, contain, overlay, modal, scroll-chaining
---

Scroll containers inside fullscreen overlays must set `overscroll-behavior: contain` to prevent scroll chaining from propagating to the body behind the overlay.

### Apply
- Modals, drawers, bottom sheets, and any fullscreen overlay that contains a scrollable region

### Skip
- The page body itself or inline scrollable sections that are not overlays (scroll chaining to the body is expected behavior there)

### Bad
```css
.overlay-content {
  overflow-y: auto;
  /* no overscroll-behavior — scrolling past the end chains to the body */
}
```

### Good
```css
.overlay-content {
  overflow-y: auto;
  overscroll-behavior: contain;
}
```

### Edge
`overscroll-behavior: contain` is only needed on scrollable overlays and modals where the body sits behind the overlay. Applying it to the page body itself or to non-overlay scroll containers will suppress the browser's native pull-to-refresh and elastic scroll behaviors, which harms expected UX.
