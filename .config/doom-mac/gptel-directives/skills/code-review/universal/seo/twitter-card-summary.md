---
id: rule-u-seo-006
title: Twitter Card Declaration
severity: LOW
tags: twitter:card, twitter:site, twitter:creator, summary_large_image
---

Public routes that declare Open Graph metadata should also declare a Twitter card. Use `summary_large_image` when the og:image is hero-shaped (≥1.91:1 aspect, ≥600×315); use `summary` for square or smaller thumbnails.

### Apply
- Any route emitting `<meta property="og:image">` and intended to be shared on X / Twitter
- Files producing the meta tag array

### Skip
- Internal routes
- Routes with no shareable hero image (the card falls back to `summary` with no image, which is fine)

### Bad
```js
{ property: 'og:image', content: 'https://kyonax.com/og-banner.jpg' },
{ property: 'og:image:width',  content: '1200' },
{ property: 'og:image:height', content: '630' },
```

### Good
```js
{ property: 'og:image',        content: 'https://kyonax.com/og-banner.jpg' },
{ property: 'og:image:width',  content: '1200' },
{ property: 'og:image:height', content: '630' },
{ name: 'twitter:card',        content: 'summary_large_image' },
{ name: 'twitter:site',        content: '@kyonax_on_tech' },
{ name: 'twitter:creator',     content: '@kyonax_on_tech' },
{ name: 'twitter:title',       content: ogTitle },
{ name: 'twitter:description', content: description },
{ name: 'twitter:image',       content: ogImageAbs },
{ name: 'twitter:image:alt',   content: ogImageAlt },
```

### Edge
`twitter:image:alt` is treated by some screen readers when the link is rendered in a tweet — keep it concrete and under 420 characters. `twitter:site` and `twitter:creator` are independent: `:site` is the publisher (org account), `:creator` is the author. They can be the same handle on a personal site.
