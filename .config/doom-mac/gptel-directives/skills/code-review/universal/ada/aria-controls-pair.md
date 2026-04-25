---
id: rule-u-ada-016
title: aria-controls Must Pair with aria-expanded
severity: MEDIUM
tags: aria-controls, aria-expanded, id, toggle
---

Buttons with `aria-expanded` must also have `aria-controls` pointing to the `id` of the controlled region; both attributes must update together.

### Apply
- Accordion triggers
- Dropdown toggles
- Collapsible panels, expandable sections

### Skip
- Buttons that open modals (modal focus-trap pattern handles its own association)

### Bad
```html
<!-- aria-expanded without aria-controls — AT can't find the panel -->
<button aria-expanded="false">Show details</button>
<div id="details-panel" hidden>Details content</div>
```

### Good
```html
<button
  aria-expanded="false"
  aria-controls="details-panel"
  @click="togglePanel"
>
  Show details
</button>
<div id="details-panel" hidden>Details content</div>
```

```javascript
methods: {
  togglePanel() {
    this.isOpen = !this.isOpen;
    // aria-expanded and the panel's visibility update from the same state
  },
},
```

### Edge
Both `aria-expanded` and `aria-controls` must be driven by the same reactive state. You cannot have `aria-expanded` without `aria-controls` -- screen readers use `aria-controls` to let users jump directly to the controlled region. If the controlled region is conditionally rendered with `v-if`, ensure its `id` is stable and the element exists in the DOM when `aria-expanded="true"`.
