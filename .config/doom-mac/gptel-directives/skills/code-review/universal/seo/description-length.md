---
id: rule-u-seo-002
title: Meta Description Length and Framing
severity: HIGH
tags: description, name="description", meta, useHead
---

Meta description must be present, 60–160 characters, written as a self-contained sentence (subject + verb + value), and mirror the title's primary topic. Google truncates at ~155 chars on desktop, ~120 on mobile.

### Apply
- Any file producing `<meta name="description" content="…">`, calling `useHead({ meta: [{ name: 'description', ... }] })`, or assigning a `description` field consumed by a head composable
- Locale snippet trees with a `meta.description` key

### Skip
- AMP-only routes (have their own meta rules)
- Routes intentionally marked `noindex` — description is irrelevant there

### Bad
```js
'description': 'Welcome.'
```

### Good
```js
'description': '8 years building scalable, performant web apps. Currently Frontend Engineer at AgileEngine for Madison Reed. Available for remote work.'
```

### Edge
Descriptions identical across locales lose the per-locale snippet — translate the description, do not auto-fallback. The description is *not* a ranking factor, but it is the snippet CTR driver, so first-clause framing matters more than keyword stuffing.
