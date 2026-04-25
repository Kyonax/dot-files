---
id: rule-u-ada-015
title: Focus Return on Collapse
severity: HIGH
tags: focus, return, collapse, v-if, dom-swap, nextTick
---

When `v-if`/`v-else` removes the element the user just interacted with, programmatically move focus to the next logical target via `nextTick`.

### Apply
- Toggle buttons that destroy their own container (e.g., expand/collapse panels)
- `v-if`/`v-else` swaps where the focused element is in the removed branch
- Dismissible banners, inline editors, or any UI where interaction removes the trigger

### Skip
- Hiding with `v-show` (element stays in DOM, focus is preserved)
- Modal close (handled separately by modal focus-trap return logic)

### Bad
```html
<!-- User clicks "Cancel" and the form disappears — focus drops to <body> -->
<div v-if="isEditing">
  <input type="text" />
  <button @click="isEditing = false">Cancel</button>
</div>
```

### Good
```html
<div v-if="isEditing">
  <input type="text" />
  <button @click="cancelEdit">Cancel</button>
</div>
<h2 v-else ref="sectionHeading" tabindex="-1">Profile</h2>
```

```javascript
methods: {
  cancelEdit() {
    this.isEditing = false;
    this.$nextTick(() => {
      this.$refs.sectionHeading?.focus();
    });
  },
},
```

### Edge
The focus target should be the next logical element in the page flow -- the parent heading, the first form field, or the toggle trigger if it still exists. Never send focus to `document.body`; this forces screen reader users to navigate from the top of the page. Use `tabindex="-1"` on non-interactive targets so they can receive programmatic focus without entering the tab order.
