---
id: rule-u-ada-021
title: Stretched-Link Pattern for Structured-Content Cards
severity: HIGH
tags: a-href, card, wcag-2.5.3, role-link, position-absolute, inset, aria-label
---

Card-style links wrapping block-level / grid / flex / heading children must use the stretched-link pattern (empty `<a>` overlay over a non-anchor wrapper) instead of wrapping all content inside `<a>`.

### Apply
- Any `<a>` containing 2+ block children (`<div>`, `<section>`, `<header>`, `<h1>`–`<h6>`, grid/flex container)
- Cards with status row + name + description + version chip + icon (visually 2D layout inside one link)
- Polymorphic `<component :is="has_link ? 'a' : 'div'">` patterns that wrap rich content

### Skip
- Simple text-only links (`<a>RECKIT</a>`, `<a>VIEW REPO</a>`)
- Links with a single line of inline children
- Buttons (use `<button>` with `aria-label` directly — buttons are addressed by different rules)

### Bad
```html
<a href="https://example.com/reckit" aria-label="ON HOLD RECKIT v0.3.0" class="card">
  <div class="card__head">
    <span class="card__status">ON HOLD</span>
  </div>
  <div class="card__name-block">
    <span class="card__name">RECKIT</span>
    <span class="card__version">v0.3.0</span>
  </div>
</a>
```

### Good
```html
<div class="card" style="position: relative">
  <div class="card__head">
    <span class="card__status">ON HOLD</span>
  </div>
  <div class="card__name-block">
    <span class="card__name">RECKIT</span>
    <span class="card__version">v0.3.0</span>
  </div>
  <a class="card__hit" href="https://example.com/reckit" aria-label="ON HOLD RECKIT v0.3.0" />
</div>
```
```css
.card__hit {
  position: absolute;
  inset: 0;
  z-index: 1;
  text-decoration: none;
}
.card__hit:focus-visible {
  outline: 2px solid var(--focus-ring-color);
  outline-offset: 2px;
}
```

### Edge
The visible-text mirror in `aria-label` MUST contain every token a sighted user sees, in the same order. Build as `[status, name, version].filter(Boolean).join(' ')` from the same data source the visible spans render from. Empty link → empty `innerText` → trivially passes WCAG 2.5.3 (`""` is a substring of anything). Focus-visible outline goes on the overlay (not the card) since focus lands on the `<a>`. See companion rule `innertext-newline-mismatch.md` for the root cause this pattern fixes.
