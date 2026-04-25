---
id: rule-br-kyo-005
title: Contain Layout Paint on HUD Regions
severity: HIGH
tags: contain, layout, paint, hud-group, performance
---

Add `contain: layout paint` on HUD groups, widgets, and frequently-updating regions to limit browser reflow/repaint scope.

### Apply
- Any HUD widget, group container, or region that updates at high frequency (>5 Hz) or contains animated children.

### Skip
- Root-level layout containers that intentionally participate in page flow (e.g., the main app shell).

### Bad
```css
/* No containment — every meter update reflows the entire page */
.hud-panel {
  display: flex;
  gap: 8px;
}
```

### Good
```css
.hud-panel {
  display: flex;
  gap: 8px;
  contain: layout paint;
}
```

### Edge
Avoid `contain: size` on elements whose intrinsic size is used by a parent flex/grid layout — it will collapse to 0. Stick with `layout paint` unless you explicitly set width/height.
