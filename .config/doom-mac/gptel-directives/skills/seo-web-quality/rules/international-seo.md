---
title: Configure for International SEO
impact: MEDIUM
impactDescription: Correctly targets content to users in different countries and languages using hreflang tags.
tags: seo, international, i18n, hreflang, language
---

For websites that serve content in multiple languages or to multiple geographic regions, it's crucial to signal this to search engines.

## 1. Hreflang Tags

The `hreflang` attribute tells Google which language you are using on a specific page, so it can serve the correct language or regional URL to users in search results.

Place these tags in the `<head>` section of the page.

**Example: Targeting Languages**
```html
<link rel="alternate" hreflang="en" href="https://example.com/page">
<link rel="alternate" hreflang="es" href="https://example.com/es/page">
<link rel="alternate" hreflang="x-default" href="https://example.com/page">
```
*   `x-default` specifies the fallback page if no other language matches.

**Example: Targeting Language and Region**
```html
<!-- For English speakers in the United States -->
<link rel="alternate" hreflang="en-US" href="https://example.com/us/page">
<!-- For English speakers in Great Britain -->
<link rel="alternate" hreflang="en-GB" href="https://example.com/gb/page">
```

## 2. HTML Language Declaration

While `hreflang` is for search engines, the `lang` attribute on the `<html>` tag signals the page's language to browsers and assistive technologies.

```html
<html lang="en">
<!-- or for Spanish in Mexico -->
<html lang="es-MX">
```
