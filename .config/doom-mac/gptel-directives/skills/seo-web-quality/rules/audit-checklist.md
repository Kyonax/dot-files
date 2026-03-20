---
title: SEO Audit Checklist
impact: MEDIUM
impactDescription: Provides a structured checklist for conducting a comprehensive SEO audit.
tags: seo, audit, checklist, review, process
---

Use this checklist to systematically review a website for SEO compliance and opportunities.

## Critical
- [ ] HTTPS is enabled across the entire site.
- [ ] `robots.txt` is not blocking important content or resources.
- [ ] Key pages do not have a `noindex` meta tag.
- [ ] Every page has a unique, descriptive `<title>` tag.
- [ ] Every page has a single, relevant `<h1>` tag.

## High Priority
- [ ] An XML sitemap exists, is linked in `robots.txt`, and submitted to Google Search Console.
- [ ] Canonical URLs are correctly implemented to prevent duplicate content.
- [ ] The website is mobile-friendly and passes the mobile-friendly test.
- [ ] Core Web Vitals (LCP, INP, CLS) are passing.
- [ ] Every page has a unique meta description.

## Medium Priority
- [ ] Structured data (JSON-LD) is implemented where appropriate (e.g., products, articles).
- [ ] Images have descriptive `alt` text and filenames.
- [ ] Internal linking uses descriptive anchor text.
- [ ] URLs are clean, descriptive, and user-friendly.
- [ ] Breadcrumb navigation is implemented for usability and site structure.

## Recommended Tools

| Tool                            | Primary Use                                     |
|---------------------------------|-------------------------------------------------|
| Google Search Console           | Monitor indexing, performance, and crawl errors |
| Google PageSpeed Insights       | Analyze performance and Core Web Vitals         |
| Google Rich Results Test        | Validate structured data                        |
| Lighthouse (in Chrome DevTools) | Run automated audits for SEO and quality        |
| Screaming Frog SEO Spider       | Crawl a website to find technical issues        |
