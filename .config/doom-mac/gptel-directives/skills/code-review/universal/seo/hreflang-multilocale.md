---
id: rule-u-seo-004
title: Hreflang Alternates for Multi-Locale Sites
severity: HIGH
tags: hreflang, alternate, x-default, link
---

Sites serving more than one locale must emit one `<link rel="alternate" hreflang="…" href="…">` per locale plus exactly one `hreflang="x-default"` pointing to the locale-selector fallback. All URLs absolute HTTPS. Reciprocal: every locale page must list every sibling.

### Apply
- Any site with `LOCALE_URL`, `SUPPORTED_LANGUAGES`, an `i18n/` directory, locale-prefixed routes (`/es`, `/fr`), or `?lang=` query handling
- Files producing the `<link>` tag array consumed by head composables

### Skip
- Single-locale sites
- Locale variants intentionally `noindex`

### Bad
```js
link: [
  { rel: 'alternate', hreflang: 'es', href: '/es' },
]
```

### Good
```js
link: [
  { rel: 'alternate', hreflang: 'en',        href: 'https://kyonax.com/' },
  { rel: 'alternate', hreflang: 'es',        href: 'https://kyonax.com/es' },
  { rel: 'alternate', hreflang: 'x-default', href: 'https://kyonax.com/' },
]
```

### Edge
`hreflang` codes follow BCP 47, not HTML lang — `pt-BR` (region) is valid, `pt_BR` (underscore) is not. If the site has more than ~10 locales, hreflang in the sitemap is cheaper than in the head. Conflicting hreflang ↔ canonical (canonical points to EN, hreflang points to ES on the ES page) makes Google ignore the cluster.
