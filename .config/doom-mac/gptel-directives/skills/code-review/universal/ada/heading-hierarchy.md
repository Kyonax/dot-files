---
id: rule-u-ada-004
title: Heading Hierarchy
severity: HIGH
tags: heading, h1, h2, h3, wcag-1.3.1
---

Heading levels must not skip — follow a strict h1 > h2 > h3 descent, and siblings at the same depth must share the same level.

### Apply
- Every page and component that renders heading elements (h1 through h6)

### Skip
- Visually styled text that uses a heading class but is not an actual heading element (e.g., a `<p>` with a bold font)

### Bad
```html
<h1>Welcome</h1>
<h3>Our Services</h3>
<h3>Our Team</h3>
```

### Good
```html
<h1>Welcome</h1>
<h2>Our Services</h2>
<h2>Our Team</h2>
```

### Edge
When a `v-if` or conditional rendering hides the first h2 on a page, the remaining h2 must stay as h2 — do not promote it to h3 just because the earlier sibling is absent. The heading hierarchy is defined by the document outline, not by which elements are currently visible. Conditionally rendered headings should maintain the correct level regardless of visibility state.
