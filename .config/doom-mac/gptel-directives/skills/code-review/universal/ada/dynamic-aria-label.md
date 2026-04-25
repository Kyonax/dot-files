---
id: rule-u-ada-010
title: Dynamic Aria Label for Repeated Links
severity: MEDIUM
tags: aria-label, dynamic, repeated-links
---

Repeated identical links or buttons must each have a unique aria-label that includes distinguishing context.

### Apply
- Any list or loop that renders multiple links or buttons with the same visible text (e.g., "Book", "Learn More", "View Details")

### Skip
- Links or buttons that already have unique visible text that differentiates them from siblings

### Bad
```html
<ul>
  <li>
    <h3>Haircut</h3>
    <p>Classic trim and style</p>
    <a href="/book/haircut">Book</a>
  </li>
  <li>
    <h3>Color</h3>
    <p>Full color treatment</p>
    <a href="/book/color">Book</a>
  </li>
</ul>
```

### Good
```html
<ul>
  <li>
    <h3>Haircut</h3>
    <p>Classic trim and style</p>
    <a href="/book/haircut" :aria-label="`Book ${service.name}`">Book</a>
  </li>
  <li>
    <h3>Color</h3>
    <p>Full color treatment</p>
    <a href="/book/color" :aria-label="`Book ${service.name}`">Book</a>
  </li>
</ul>
```

### Edge
Multiple "Book" buttons for different services each need a unique aria-label so screen reader users navigating by links or buttons can distinguish them. Without unique labels, a screen reader's rotor lists every link as just "Book" with no way to tell which service each one refers to. The label should incorporate the closest identifying context — typically the item name from the same list entry.
