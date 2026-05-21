---
id: rule-u-seo-011
title: Robots Meta Tag Sets index,follow and max-image-preview
severity: LOW
tags: name="robots", robots, index, follow, max-image-preview, max-snippet
---

Public routes should emit `<meta name="robots" content="index,follow,max-image-preview:large,max-snippet:-1">` (or framework equivalent). The `max-image-preview:large` directive lets Google use the OG image in Discover / Search rich previews; `max-snippet:-1` removes the snippet length cap.

### Apply
- Files producing the `<meta name="robots">` tag
- Head composables setting a `robots` meta entry
- Per-route robots overrides

### Skip
- Routes intentionally `noindex` (admin, draft previews, search result pages) — this rule should NOT flag those
- Cookie-walled or auth-walled routes

### Bad
```js
{ name: 'robots', content: 'index,follow' }
```

### Good
```js
{ name: 'robots', content: 'index,follow,max-image-preview:large,max-snippet:-1' }
```

### Edge
The absence of a `robots` meta defaults to `index,follow` already — so flag absence as LOW, not HIGH. The win from `max-image-preview:large` only matters if the page also declares a hero `og:image` ≥1200×630 — otherwise the directive is a no-op. `max-snippet:-1` is the only way to override Google's default 160-char snippet cap on description-light pages; pair it with a strong meta description (rule-u-seo-002).
