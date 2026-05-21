---
id: rule-u-seo-001
title: Title Tag Shape and Length
severity: HIGH
tags: title, useHead, meta-title, head, og-title
---

`<title>` (or `useHead({ title })` / framework equivalent) must be 20–60 characters, lead with the page's primary topic, and avoid name padding or punctuation noise. Search results truncate around 600 px / ~60 chars.

### Apply
- Any file rendering `<title>` directly, calling `useHead`, `useSeoMeta`, `defineHead`, Next.js `metadata.title`, or assigning title via i18n key (`'meta.title'`, `'kyo-web.landing.meta.title'`, etc.)
- Locale snippet files declaring a `title` field consumed by head composables

### Skip
- Per-app default fallback titles in framework boilerplate that get overwritten at runtime (verify the override path exists)
- Generated titles built from `${page}` templates where length is enforced at the template layer

### Bad
```js
'title': 'Cristian D. Moreno — Software Engineer (Full-Stack Web Developer)'
```

### Good
```js
'title': 'Senior Full-Stack Software Engineer, Remote from Colombia'
```

### Edge
Titles for paginated or filterable routes should keep the primary topic in the first 50 chars and push pagination tokens (`Page 2`, `Filter: React`) to the tail — search snippets truncate from the right. Title parity across locales is mandatory for hreflang clusters; missing one locale demotes the whole cluster.
