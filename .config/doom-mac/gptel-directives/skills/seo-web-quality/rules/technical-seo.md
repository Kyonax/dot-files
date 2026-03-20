---
title: Implement Technical SEO Best Practices
impact: HIGH
impactDescription: Ensures search engines can efficiently crawl, index, and understand the site's structure.
tags: seo, technical, crawl, index, robots, sitemap, canonical, url, https
---

## 1. Crawlability (`robots.txt`)

The `robots.txt` file tells crawlers which paths they may or may not access.

**Good Practice:**
```text
# /robots.txt
User-agent: *
Allow: /

# Block admin, API, or private areas
Disallow: /admin/
Disallow: /api/

# Specify the location of the sitemap
Sitemap: https://example.com/sitemap.xml
```

## 2. Indexing Control (`meta name="robots"`)

Use the meta robots tag to control indexing on a per-page basis.

**Examples:**
```html
<!-- Default: allow indexing and following links -->
<meta name="robots" content="index, follow">

<!-- Prevent indexing of this page -->
<meta name="robots" content="noindex, follow">

<!-- Prevent indexing and following links from this page -->
<meta name="robots" content="noindex, nofollow">
```

## 3. Canonical URLs

The `rel="canonical"` tag prevents duplicate content issues by specifying the "preferred" version of a URL.

**Examples:**
```html
<!-- Points to the definitive URL for this content -->
<link rel="canonical" href="https://example.com/the-one-true-page">

<!-- A self-referencing canonical is a common best practice -->
<link rel="canonical" href="https://example.com/current-page">
```

## 4. XML Sitemaps

A sitemap lists the important URLs on your site, helping crawlers discover your content.

**Example Format:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
  <url>
    <loc>https://example.com/</loc>
    <lastmod>2024-01-15</lastmod>
    <changefreq>daily</changefreq>
    <priority>1.0</priority>
  </url>
  <url>
    <loc>https://example.com/products</loc>
    <lastmod>2024-01-14</lastmod>
    <changefreq>weekly</changefreq>
    <priority>0.8</priority>
  </url>
</urlset>
```
**Best Practices:**
- Include only canonical, indexable URLs (200 status code).
- Keep sitemaps under 50MB or 50,000 URLs.
- Submit the sitemap URL in `robots.txt` and Google Search Console.

## 5. URL Structure

Simple, readable URLs are better for users and search engines.

*   **Good:** `https://example.com/blog/how-to-use-widgets`
*   **Poor:** `https://example.com/p?id=12345`

**Guidelines:**
- Use hyphens (`-`) to separate words.
- Use lowercase letters.
- Keep them concise and descriptive.

## 6. HTTPS

Secure Sockets Layer (SSL) is a ranking factor. Ensure the entire site is served over HTTPS to protect user data and build trust. Avoid mixing HTTP and HTTPS content.
