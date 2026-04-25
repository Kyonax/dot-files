---
id: rule-u-ada-008
title: Keyboard Navigation for Custom Controls
severity: MEDIUM
tags: keyboard, tabindex, keydown, enter, space
---

Non-native interactive elements must have tabindex="0" and keydown handlers for Enter and Space to be keyboard accessible.

### Apply
- Any `<div>`, `<span>`, or other non-interactive element that acts as a button, toggle, or clickable control

### Skip
- Native `<button>`, `<a>`, `<input>`, and `<select>` elements — they handle keyboard interaction natively

### Bad
```html
<div class="toggle" role="button" @click="handleClick">
  Toggle Option
</div>
```

### Good
```html
<div
  class="toggle"
  role="button"
  tabindex="0"
  @click="handleClick"
  @keydown.enter="handleClick"
  @keydown.space.prevent="handleClick"
>
  Toggle Option
</div>
```

### Edge
A native `<button>` element already fires click events on Enter and Space keypresses and is focusable by default — adding tabindex or keydown handlers to it is redundant. This rule only applies when a non-semantic element is used as a control. The `.prevent` modifier on Space is needed to stop the page from scrolling when the key is pressed.
