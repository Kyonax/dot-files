---
id: rule-u-ada-006
title: No Nested Interactive Elements
severity: MEDIUM
tags: nested, interactive, button, a, link
---

Never nest interactive elements (buttons, links, inputs) inside other interactive elements.

### Apply
- Any `<a>`, `<button>`, `<input>`, `<select>`, or `<textarea>` that contains or is contained by another interactive element

### Skip
- Non-interactive wrappers (e.g., a `<div>` wrapping a single button) that do not have click handlers or roles

### Bad
```html
<a href="/product/123">
  <div class="card">
    <h3>Product Name</h3>
    <p>Description text</p>
    <button>Add to Cart</button>
  </div>
</a>
```

### Good
```html
<div class="card">
  <h3><a href="/product/123">Product Name</a></h3>
  <p>Description text</p>
  <button>Add to Cart</button>
</div>
```

### Edge
A common pattern is a clickable card wrapped in an `<a>` tag with internal buttons for secondary actions. This creates nested interactive elements — screen readers cannot distinguish between the link and the button targets. Instead, make the card a non-interactive container, use a heading link for the primary action, and keep secondary buttons as siblings.
