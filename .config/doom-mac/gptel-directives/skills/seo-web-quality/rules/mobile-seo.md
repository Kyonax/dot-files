---
title: Ensure Mobile-Friendliness
impact: HIGH
impactDescription: Optimizes the site for mobile devices, a critical factor for Google's mobile-first indexing.
tags: seo, mobile, responsive, viewport, tap targets
---

With Google's mobile-first indexing, the mobile version of your site is the baseline for how Google determines rankings.

## 1. Responsive Viewport

The viewport meta tag instructs the browser on how to control the page's dimensions and scaling. It is essential for responsive design.

```html
<meta name="viewport" content="width=device-width, initial-scale=1">
```

## 2. Legible Font Sizes

Text must be readable on a small screen without requiring users to zoom in. A base font size of `16px` is a common and safe recommendation.

**Bad:** `body { font-size: 12px; }`
**Good:** `body { font-size: 16px; line-height: 1.5; }`

## 3. Accessible Tap Targets

Interactive elements like buttons and links must be large enough to be easily tapped. Google recommends a minimum size of 48x48 CSS pixels.

**Bad:**
```css
.button {
  padding: 5px;
  font-size: 12px;
}
```

**Good:**
```css
.button {
  min-width: 48px;
  min-height: 48px;
  padding: 12px;
  font-size: 16px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}
```
