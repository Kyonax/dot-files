---
id: rule-u-seo-007
title: JSON-LD Blocks Parseable and Well-Typed
severity: HIGH
tags: application/ld+json, @type, @graph, schema.org, useStructuredData
---

`<script type="application/ld+json">` blocks must parse as valid JSON, declare `@context` and `@type` on every node, and never use relative URLs in `url` / `image` / `sameAs` fields. Use `@graph` to bundle multiple entities under one `@context`.

### Apply
- Files emitting `application/ld+json` script tags directly
- Composables like `useStructuredData`, `useSchemaOrg`, `useJsonLd`
- Files matching `**/json-ld/**`, `**/seo/structured-data*`
- Build-step JSON-LD injectors (Vite `transformIndexHtml`, Webpack HtmlPlugin)

### Skip
- Routes intentionally `noindex`
- Schema markup intentionally limited to head meta (e.g., `og:type=article` is enough)

### Bad
```html
<script type="application/ld+json">
{
  "@type": "Person",
  "name": "Cristian D. Moreno",
  "url": "/",
  "image": "/og-banner.jpg"
}
</script>
```

### Good
```html
<script type="application/ld+json">
{
  "@context": "https://schema.org",
  "@graph": [
    {
      "@type": "Person",
      "@id": "https://kyonax.com/#person",
      "name": "Cristian D. Moreno",
      "url": "https://kyonax.com/",
      "image": "https://kyonax.com/og-banner.jpg"
    }
  ]
}
</script>
```

### Edge
Required fields per `@type` differ: `Person` needs `name`; `Organization` needs `name` + `url`; `FAQPage.mainEntity` needs `Question` nodes each with `acceptedAnswer.Answer.text`. Validate the shape, not just JSON parseability — a clean JSON parse with missing required fields fails Rich Results. CI should `JSON.parse()` every block and call out missing `@type`. Multiple JSON-LD `<script>` blocks on one page are valid and often preferable (one for `@graph`, one for `FAQPage`).
