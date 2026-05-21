---
id: rule-u-seo-010
title: robots.txt References Sitemap and Sitemap Exists
severity: MEDIUM
tags: robots.txt, sitemap.xml, Sitemap:, User-agent
---

`public/robots.txt` (or framework equivalent) must reference at least one absolute `Sitemap:` URL, and that sitemap must exist with valid XML containing every public route. Disallow rules should be intentional, never blanket.

### Apply
- `public/robots.txt`, `static/robots.txt`, `app/robots.ts` / `pages/robots.ts` (Next.js), `src/routes/robots.txt/+server.ts` (SvelteKit)
- `public/sitemap.xml`, dynamic sitemap generators (`generate-sitemap.mjs`, `app/sitemap.ts`, `next-sitemap.config.js`)
- Build scripts producing these files

### Skip
- Internal-only sites or development environments (a blanket `Disallow: /` is intentional there)
- Subdomain proxies where robots.txt is owned upstream

### Bad
```
User-agent: *
Allow: /
```

### Good
```
User-agent: *
Allow: /
Disallow: /.git/

Sitemap: https://kyonax.com/sitemap.xml
```

### Edge
A site can have multiple `Sitemap:` lines pointing to a sitemap index plus per-section sitemaps. URLs in robots.txt must be absolute (Google ignores relative). If the site auto-generates the sitemap at build time, the rule still applies — verify the generator runs in CI and the produced XML has `<lastmod>` for every URL. `Disallow: /` accidentally shipped to production is an indexability disaster — treat any blanket disallow in main-branch robots.txt as CRITICAL even though this rule defaults to MEDIUM.
