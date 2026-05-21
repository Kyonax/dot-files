---
id: rule-u-ada-023
title: CSS Mask for Decorative Icons Inside Interactive Elements
severity: MEDIUM
tags: wcag-2.5.3, aria-hidden, content-attr, mask-image, icon, a-href, button
---

Decorative icons nested inside `<a>` / `<button>` / `role="link"` must render via CSS `mask-image` (icon-as-image), not via CSS `content: attr(data-text)` (icon-as-text). The text-content pattern trips WCAG 2.5.3 because scanners read CSS pseudo-element text as part of the visible label while `aria-hidden` excludes it from the accessible name.

### Apply
- Decorative glyph spans inside an `<a>` or `<button>` (external-link arrows, chevrons, status icons)
- Any `aria-hidden="true"` element nested in an interactive control where the parent has an `aria-label`
- Migrating existing `[aria-hidden][data-text]::before { content: attr(data-text) }` patterns into interactive contexts

### Skip
- HUD chrome, watermarks, decorative labels OUTSIDE interactive elements (the `content: attr(data-text)` pattern is correct there — it's exempt from WCAG 1.4.3 contrast checks because the text never enters the DOM)
- Icon-only buttons where the icon IS the label (use inline SVG with proper accname or `aria-label` on the button)

### Bad
```html
<!-- CSS pseudo-element renders the glyph as "visible text" the scanner reads
     but aria-hidden excludes it from accname → 2.5.3 mismatch. -->
<a href="..." aria-label="View repo">
  Open
  <span class="icon-glyph" data-text="↗" aria-hidden="true"></span>
</a>
```
```css
[aria-hidden="true"][data-text]::before { content: attr(data-text); }
```

### Good
```html
<a href="..." aria-label="View repo">
  Open
  <span class="icon-mask icon-mask--external" aria-hidden="true"></span>
</a>
```
```css
.icon-mask {
  display: inline-block;
  width: 1em;
  height: 1em;
  background-color: currentColor;
  -webkit-mask: var(--_icon-mask) center / contain no-repeat;
          mask: var(--_icon-mask) center / contain no-repeat;
  vertical-align: -0.15em;
  flex-shrink: 0;
}
.icon-mask--external {
  --_icon-mask: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='black' stroke-width='2'%3E%3Cpath d='M15 3h6v6'/%3E%3Cpath d='M10 14 21 3'/%3E%3Cpath d='M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6'/%3E%3C/svg%3E");
}
```

### Edge
The SVG payload's stroke color is irrelevant — the `mask` uses the alpha channel; the visible color comes from `background-color: currentColor` on the host so the icon inherits the link color. Encode `<` `>` `#` as `%3C` `%3E` `%23` in the data URI. For Nerd Font / icon-font glyphs there's no SVG path available — in that case either (a) replace the font glyph with a Lucide / Heroicons / Feather equivalent, or (b) move the icon outside the interactive element. Don't try to make font-glyph CSS content work inside interactive elements; the 2.5.3 mismatch is structural.
