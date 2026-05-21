---
id: rule-u-ada-022
title: innerText vs textContent — Scanners Use innerText
severity: HIGH
tags: wcag-2.5.3, innertext, textcontent, aria-label, display-block, display-flex, display-grid
---

WCAG 2.5.3 scanners compute "visible label" via DOM `innerText`, NOT `textContent`. Block-level / flex-item / grid-item children inject hard `\n` newlines into `innerText` that break the substring check against a space-separated `aria-label`.

### Apply
- Reviewing any flagged WCAG 2.5.3 finding where `textContent` *appears* to match `aria-label`
- Auditing interactive elements (`<a>`, `<button>`, `role="link"`) with structured layout children (grid / flex / multiple block divs)
- Designing the accname for a card-style link

### Skip
- Inline-only content (single line of `<span>` children with `display: inline`)
- Elements with explicit `aria-labelledby` pointing to a single text node

### Bad
```html
<!-- textContent: "ON HOLD RECKIT v0.3.0" (matches aria-label after normalization)
     innerText: "ON HOLD\nRECKIT\nv0.3.0" (grid items inject newlines — FAILS) -->
<a href="..." aria-label="ON HOLD RECKIT v0.3.0" style="display: grid">
  <div class="head"><span>ON HOLD</span></div>
  <div class="name-block">
    <span>RECKIT</span>
    <span>v0.3.0</span>
  </div>
</a>
```

### Good
```html
<!-- Empty link → empty innerText → trivially passes substring check.
     Visible content lives in sibling block elements outside the <a>. -->
<div style="position: relative; display: grid">
  <div class="head"><span>ON HOLD</span></div>
  <div class="name-block">
    <span>RECKIT</span>
    <span>v0.3.0</span>
  </div>
  <a class="hit" href="..." aria-label="ON HOLD RECKIT v0.3.0"
     style="position: absolute; inset: 0" />
</div>
```

### Edge
Grid / flex items are blockified by their parent regardless of their own `display` value — `display: inline-flex` on the child still produces newlines in `innerText`. `display: contents` on intermediate divs doesn't help (grandchildren still blockified by the grid parent). `<br>` adds an explicit newline. There is **no CSS escape hatch** for a grid-layout link to have newline-free `innerText`. The stretched-link pattern (companion rule `stretched-link-structured-cards.md`) is the only reliable fix. When debugging, dump `{innerText, textContent}` via CDP — divergence reveals the layout-derived newlines that surface tools (like axe DevTools, IBM Equal Access) react to. **Separate concern, related trap:** even when the link is fully inline (no newlines in innerText), wrapping a single character in a styled `<span>` with `font-size` bumps as modest as `1.4em` can still trip the scanner via its image-of-text heuristic — that's WCAG 1.4.5 territory leaking through. If a glyph reads small at the surrounding text size, either accept the small size or remove the glyph. Do not wrap it in a sized span inside the interactive element.
