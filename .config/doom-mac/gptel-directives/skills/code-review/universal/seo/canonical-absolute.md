---
id: rule-u-seo-003
title: Canonical Link Present, Absolute, Per-Locale
severity: HIGH
tags: canonical, rel="canonical", link, hreflang
---

Every public route must emit `<link rel="canonical" href="…">` with an absolute HTTPS URL pointing to the route's preferred form. Multi-locale sites: canonical points to the *current locale's* URL, not always the default.

### Apply
- Any SSR or SSG route emitting `<head>`
- Head composables (`useHead`, `useSeoMeta`, `Helmet`, Next.js `metadata.alternates.canonical`)
- Locale-aware sites with `/en`, `/es`, `?lang=` patterns

### Skip
- Routes explicitly marked `noindex` (canonical irrelevant)
- API endpoints, JSON files, non-HTML responses

### Bad
```js
{ rel: 'canonical', href: '/' }
```

### Good
```js
const canonical = computed(() => LOCALE_URL[locale.value] || LOCALE_URL.en);
useHead({ link: [{ rel: 'canonical', href: canonical }] });
```

### Edge
Canonical must agree with the URL that hreflang alternates point to for that locale — mismatch tells Google to ignore both. If the site canonicalizes trailing-slash form server-side (e.g., `.htaccess` `RewriteRule (.+)/$ /$1`), the canonical in HTML must match the chosen form exactly.
