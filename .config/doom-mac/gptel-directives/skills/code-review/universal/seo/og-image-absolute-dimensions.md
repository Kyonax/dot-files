---
id: rule-u-seo-005
title: OG Image Absolute URL with Dimensions
severity: MEDIUM
tags: og:image, og:image:width, og:image:height, property="og:, opengraph
---

`og:image` must be an absolute HTTPS URL; `og:image:width` and `og:image:height` must be declared as plain integers. Without dimensions, Facebook, LinkedIn, and Slack defer to lazy-fetching the image and may render a fallback or no preview at all.

### Apply
- Files producing `<meta property="og:image" ...>` or `useHead({ meta: [{ property: 'og:image', ... }] })`
- Locale snippet `meta.og-image-alt` consumers
- Routes with a hero share card

### Skip
- Internal-only routes (admin, auth, dashboards) that should never be shared
- Routes with `og:type` of `video` or `music` (different OG image rules apply)

### Bad
```js
{ property: 'og:image', content: '/og-banner.jpg' }
```

### Good
```js
{ property: 'og:image',        content: 'https://kyonax.com/og-banner.jpg' },
{ property: 'og:image:type',   content: 'image/jpeg' },
{ property: 'og:image:width',  content: '1200' },
{ property: 'og:image:height', content: '630' },
{ property: 'og:image:alt',    content: ogImageAlt },
```

### Edge
The 1200×630 ratio is the LinkedIn/Twitter sweet spot; Facebook accepts 1.91:1 down to 600×315. If the image is dynamic (per-route OG cards via Satori / `@vercel/og`), the dimension props must be set per-render, not hard-coded. Width and height must be strings or numbers, never `null` — head composables will silently drop `null` and the social scraper sees no dimensions.
