# SEO & Web Quality Architectural Guide

## Core Philosophy
Modern Search Engine Optimization (SEO) is a direct reflection of a high-quality user experience. The goal is to build websites that are technically sound, provide clear and valuable content, and are structured in a way that both users and search engine crawlers can easily understand. This skill prioritizes long-term, sustainable practices over short-term tricks.

## The Pillars of On-Site SEO
Our approach is built on three pillars that work together to create a search-friendly website.

### 1. Technical SEO: The Foundation
**If a site cannot be crawled and indexed, its content is invisible.** This is the most critical pillar. It involves configuring the server and website structure to allow search engines to access and interpret content efficiently.
*   **Reference:** For implementation details on `robots.txt`, sitemaps, canonicals, and more, see `rules/technical-seo.md`.

### 2. On-Page SEO: Content & Relevance
This pillar focuses on the content of individual pages. It's how we signal to search engines what a page is about and which keywords it should rank for. This includes everything from the text users see to the metadata crawlers read.
*   **Reference:** For guidelines on titles, meta tags, headings, and image optimization, see `rules/on-page-seo.md`.

### 3. Mobile SEO: The Mobile-First Mandate
Google primarily uses the mobile version of a site for indexing and ranking (mobile-first indexing). A seamless mobile experience is no longer optional; it is a core requirement for visibility.
*   **Reference:** For rules on viewports, tap targets, and responsive design, see `rules/mobile-seo.md`.

## Enhancing Visibility & User Experience
Beyond the core pillars, these practices improve how your site appears in search results and serves specific audiences.

*   **Structured Data:** By providing explicit clues about a page's content via JSON-LD, we can help search engines generate "rich snippets" (e.g., star ratings, prices, FAQ accordions) that increase click-through rates. See `rules/structured-data.md`.
*   **International SEO:** For sites serving multiple regions or languages, it is crucial to signal the correct page version to the user. This prevents duplicate content issues and improves user experience. See `rules/international-seo.md`.

## Process & Auditing
A systematic approach is essential for identifying and prioritizing SEO tasks. A regular audit ensures that the site remains aligned with best practices as search engine algorithms evolve.
*   **Reference:** For a step-by-step process and recommended tools, consult the `rules/audit-checklist.md`.
