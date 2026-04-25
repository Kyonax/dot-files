---
id: rule-u-ada-017
title: Role Link for Clickable Cards
severity: MEDIUM
tags: role-link, card, wcag-2.5.3, tabindex, clickable
---

Multi-content clickable cards should use `div[role="link"]` with `tabindex="0"` and a `keydown.enter` handler instead of wrapping everything in an `<a>` tag.

### Apply
- Cards containing images, headings, descriptions, and/or secondary links that navigate to a single destination on click
- Any clickable container with multiple content blocks where a wrapping `<a>` would create verbose AT announcements

### Skip
- Simple links with a single text node or short label
- Cards where each element has its own distinct action

### Bad
```html
<!-- Wrapping <a> around complex content — AT reads all children as link text -->
<a href="/products/item-42" class="card">
  <img src="item.jpg" alt="Item photo" />
  <h3>Product Name</h3>
  <p>Short description of the product with extra details.</p>
  <span class="price">$29.99</span>
</a>
```

### Good
```html
<div
  role="link"
  tabindex="0"
  @click="navigateTo('/products/item-42')"
  @keydown.enter="navigateTo('/products/item-42')"
  aria-label="Product Name — $29.99"
>
  <img src="item.jpg" alt="" />
  <h3>Product Name</h3>
  <p>Short description of the product with extra details.</p>
  <span class="price">$29.99</span>
</div>
```

### Edge
This pattern avoids WCAG 2.5.3 (Label in Name) scanner flags that fire when a wrapping `<a>` tag's accessible name includes all nested text content. The `aria-label` on the `div[role="link"]` should be a concise summary, not a repetition of all inner text. Ensure the `keydown.enter` handler matches the `click` handler so keyboard users reach the same destination.
