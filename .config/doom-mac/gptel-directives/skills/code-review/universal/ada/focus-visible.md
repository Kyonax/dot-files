---
id: rule-u-ada-005
title: Focus Visible
severity: MEDIUM
tags: focus-visible, focus, outline, keyboard
---

Use :focus-visible instead of :focus for keyboard focus outlines to avoid showing outlines on mouse clicks.

### Apply
- All interactive elements that receive custom focus styling (buttons, links, inputs, custom controls)

### Skip
- Elements that rely entirely on the browser's default focus ring without any custom override

### Bad
```html
<button class="action-btn">Submit</button>

<style>
.action-btn:focus {
  outline: 2px solid blue;
}
</style>
```

### Good
```html
<button class="action-btn">Submit</button>

<style>
.action-btn:focus-visible {
  outline: 2px solid blue;
}
</style>
```

### Edge
Custom buttons that apply `outline: none` or `outline: 0` to reset default browser styles MUST add a `:focus-visible` rule to restore a visible keyboard indicator. Removing the outline without replacing it via `:focus-visible` leaves keyboard users with no focus indication at all — a WCAG 2.4.7 failure.
