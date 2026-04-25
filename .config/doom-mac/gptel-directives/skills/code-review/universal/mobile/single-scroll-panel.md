---
id: rule-u-mob-003
title: One Scroll Container Per Mobile Panel
severity: MEDIUM
tags: scroll, overflow, panel, mobile, nested
---

Limit each mobile panel to a single scroll container; a wrapper must not set `overflow-y` if a child element already scrolls.

### Apply
- Mobile panels, drawers, bottom sheets, or slide-over views that contain a scrollable content area

### Skip
- Intentional dual-scroll layouts (e.g., a fixed sidebar list + scrollable detail pane designed as a split view)

### Bad
```css
.panel-wrapper {
  overflow-y: auto;     /* wrapper scrolls */
}
.panel-content {
  overflow-y: auto;     /* child also scrolls — nested scroll trap */
}
```

### Good
```css
.panel-wrapper {
  overflow-y: hidden;   /* wrapper does not scroll */
  display: flex;
  flex-direction: column;
}
.panel-content {
  flex: 1;
  overflow-y: auto;     /* only the content area scrolls */
}
```

### Edge
Nested scroll traps are invisible during desktop testing because mouse wheels and trackpads navigate them easily. They only surface on real mobile devices where touch-scroll disambiguation fails, causing the user to get stuck mid-panel. Always verify on an actual phone or a device-accurate emulator with touch input.
