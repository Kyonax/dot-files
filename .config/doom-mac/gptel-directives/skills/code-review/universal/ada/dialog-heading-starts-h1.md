---
id: rule-u-ada-024
title: Dialog Heading Hierarchy Starts at h1
severity: HIGH
tags: wcag-1.3.1, dialog, aria-modal, h1, h2, h3, heading-hierarchy, modal
---

Inside `<div role="dialog" aria-modal="true">` (or `<dialog>` with `aria-modal="true"`), the first heading must be `<h1>`, and section headings inside start at `<h2>`. The dialog is its own heading context — a fresh outline, not a continuation of the page's outline.

### Apply
- Custom modal components rendering a `<h2>` / `<h3>` as the dialog title
- Modal section subtitles inside dialog bodies
- Any element with `role="dialog"` + `aria-modal="true"` containing headings

### Skip
- Chromeless / titleless dialogs (image viewers, popovers without a header)
- `aria-modal="false"` dialogs (inline disclosures) — those continue the page hierarchy
- Headings that pre-date the dialog rule retrofit, when the rule is being introduced incrementally (note them with a TODO)

### Bad
```html
<!-- Modal title is h2 — scanner flags "inappropriate jump in heading levels
     within the open modal dialog heading hierarchy" -->
<div role="dialog" aria-modal="true" aria-label="Project details">
  <header>
    <h2 class="modal__title">RECKIT</h2>
  </header>
  <h3 class="modal__section-title">Description</h3>
  <p>...</p>
  <h3 class="modal__section-title">Stack</h3>
  <ul>...</ul>
</div>
```

### Good
```html
<div role="dialog" aria-modal="true" aria-label="Project details">
  <header>
    <h1 class="modal__title">RECKIT</h1>
  </header>
  <h2 class="modal__section-title">Description</h2>
  <p>...</p>
  <h2 class="modal__section-title">Stack</h2>
  <ul>...</ul>
</div>
```

### Edge
Multiple `<h1>` elements on a single page IS legal when `aria-modal="true"` isolates the dialog as a separate context — the modal's h1 is only in the DOM while the modal is open, and screen reader heading navigation respects the dialog boundary. CSS rank (font-size, weight, color) is independent of the tag, so promoting `<h2>` → `<h1>` requires zero visual changes if styling is scoped to a `.modal__title` class. The page-level `<h1>` (hero) and the modal-level `<h1>` (title) coexist without breaking the page outline because they're never in the visible tree at the same time.
