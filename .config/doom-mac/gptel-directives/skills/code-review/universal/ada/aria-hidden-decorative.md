---
id: rule-u-ada-002
title: Aria Hidden on Decorative Elements
severity: MEDIUM
tags: aria-hidden, decorative, img, icon
---

Apply aria-hidden="true" to decorative images and icons that appear adjacent to visible text labels.

### Apply
- Icons or images placed next to a text label that already conveys the same meaning (e.g., a checkmark icon beside the word "Done")

### Skip
- A standalone icon with no adjacent text — it is NOT decorative and needs an accessible label instead

### Bad
```html
<button>
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
    <path d="M3 12l2-2m7 ..." />
  </svg>
  Close
</button>
```

### Good
```html
<button>
  <svg aria-hidden="true" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
    <path d="M3 12l2-2m7 ..." />
  </svg>
  Close
</button>
```

### Edge
An icon inside a labeled button is decorative and must have aria-hidden="true". However, a standalone icon button with no visible text is NOT decorative — it requires an aria-label on the button itself and the icon should still be aria-hidden="true" so the label is not duplicated.
