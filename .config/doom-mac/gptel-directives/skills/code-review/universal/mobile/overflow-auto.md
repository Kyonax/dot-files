---
id: rule-u-mob-004
title: Prefer overflow-y auto Over scroll
severity: LOW
tags: overflow-y, auto, scroll, gutter
---

Use `overflow-y: auto` instead of `overflow-y: scroll` to avoid forcing a permanent scrollbar gutter when content fits without scrolling.

### Apply
- Any scrollable container where the content length is dynamic and may or may not exceed the container height

### Skip
- Designs that intentionally show a persistent scrollbar for discoverability (e.g., a settings panel where the user should know more content exists)

### Bad
```css
.content-list {
  overflow-y: scroll;   /* permanent gutter even when 3 items fit */
}
```

### Good
```css
.content-list {
  overflow-y: auto;     /* scrollbar appears only when needed */
}
```

### Edge
Some designs intentionally display a persistent scrollbar to signal that the container is scrollable. This is a rare but valid exception — document the intent with a comment when using `overflow-y: scroll` deliberately.
