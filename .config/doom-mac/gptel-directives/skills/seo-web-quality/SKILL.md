---
name: seo-web-quality
description: >-
  Best practices for Search Engine Optimization (SEO) and web quality. Covers
  technical SEO (robots.txt, sitemaps, canonical, hreflang, HSTS), on-page SEO
  (title shape, meta description length, heading hierarchy, keyword coverage),
  structured data (JSON-LD: FAQPage, BreadcrumbList, Product, HairSalon),
  mobile SEO (Core Web Vitals, viewport, safe-area), international SEO, and
  audit checklists. Trigger: 'improve SEO', 'audit site for search',
  'fix meta tags', 'add structured data', 'check crawlability',
  'optimize for Google ranking', 'check JSON-LD', 'fix canonical'.
user-invocable: true
metadata:
  openclaw:
    emoji: 🔎
    os: [darwin, linux]
    requires:
      bins: [node]
---

# SEO & Web Quality Skill Guide

Provides comprehensive rules and best practices for optimizing web projects for search engines, based on Google Lighthouse guidelines and modern SEO standards.

## When to Apply
Reference these guidelines when:

*   Auditing a website for search engine visibility.
*   Implementing on-page optimizations like titles, meta descriptions, and headings.
*   Configuring technical SEO elements such as `robots.txt`, sitemaps, or canonical tags.
*   Adding structured data (JSON-LD) to enhance search result appearance.
*   Ensuring the site is mobile-friendly for mobile-first indexing.

## Quick Reference

| Rule                | Description                                                                                                              |
|---------------------|--------------------------------------------------------------------------------------------------------------------------|
| `technical-seo`     | Ensures search engine crawlability and indexing via `robots.txt`, sitemaps, canonical URLs, and HTTPS.                   |
| `on-page-seo`       | Optimizes page content with proper title tags, meta descriptions, heading structure, image alt text, and internal links. |
| `structured-data`   | Enhances search results with rich snippets using JSON-LD for articles, products, FAQs, and more.                         |
| `mobile-seo`        | Covers mobile-first best practices including responsive viewports, tap target sizes, and readable fonts.                 |
| `international-seo` | Guides implementation of `hreflang` tags for multi-language and multi-regional websites.                                 |
| `audit-checklist`   | Provides a comprehensive checklist and tool reference for conducting a full SEO audit.                                   |
