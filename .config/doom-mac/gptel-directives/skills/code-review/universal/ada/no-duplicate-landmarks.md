---
id: rule-u-ada-007
title: No Duplicate Landmarks Without Labels
severity: MEDIUM
tags: duplicate, landmark, region, nav, aria-label
---

Multiple landmarks of the same type on a page must each have a distinct accessible label.

### Apply
- Any page with more than one `<nav>`, `<aside>`, `<section>` (with role="region"), or other repeated landmark roles

### Skip
- Pages with only one instance of each landmark type — implicit labeling is sufficient

### Bad
```html
<nav>
  <a href="/">Home</a>
  <a href="/about">About</a>
</nav>
<nav>
  <a href="/terms">Terms</a>
  <a href="/privacy">Privacy</a>
</nav>
```

### Good
```html
<nav aria-label="Main navigation">
  <a href="/">Home</a>
  <a href="/about">About</a>
</nav>
<nav aria-label="Footer navigation">
  <a href="/terms">Terms</a>
  <a href="/privacy">Privacy</a>
</nav>
```

### Edge
Two `<nav>` elements need distinct aria-label values so screen reader users can differentiate them in the landmarks list. The same applies to multiple `<aside>` or `<section role="region">` elements. Without distinct labels, all instances appear identically in the accessibility tree, making landmark navigation useless.
