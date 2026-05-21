# seo/ — Search Engine Optimization Rules

13 rules. Always loaded. Any web project that renders `<head>` metadata, JSON-LD, or ships an HTML route.

| ID | File | Summary | Severity |
|---|---|---|---|
| rule-u-seo-001 | title-shape-and-length.md | Title 20–60 chars, primary topic first, no padding | HIGH |
| rule-u-seo-002 | description-length.md | Meta description 60–160 chars, action-or-fact framing | HIGH |
| rule-u-seo-003 | canonical-absolute.md | Canonical link present, absolute HTTPS, per-locale | HIGH |
| rule-u-seo-004 | hreflang-multilocale.md | Multi-locale sites declare hreflang per locale + x-default | HIGH |
| rule-u-seo-005 | og-image-absolute-dimensions.md | og:image absolute HTTPS, og:image:width + og:image:height set | MEDIUM |
| rule-u-seo-006 | twitter-card-summary.md | twitter:card present, summary_large_image when og:image is hero-shaped | LOW |
| rule-u-seo-007 | jsonld-presence-shape.md | application/ld+json blocks parseable, @type present, no relative URLs | HIGH |
| rule-u-seo-008 | mailto-plaintext-ssr.md | No plaintext `mailto:user@domain` in SSR HTML — obfuscate or defer to client | MEDIUM |
| rule-u-seo-009 | hsts-header.md | Server config declares Strict-Transport-Security header | MEDIUM |
| rule-u-seo-010 | robots-sitemap-pair.md | robots.txt references absolute Sitemap URL; sitemap.xml exists | MEDIUM |
| rule-u-seo-011 | robots-meta-indexable.md | Robots meta is `index,follow` + `max-image-preview:large` on public routes | LOW |
| rule-u-seo-012 | img-explicit-dimensions.md | `<img>` has width + height attrs OR an aspect-ratio in CSS — prevents CLS | MEDIUM |
| rule-u-seo-013 | keyword-coverage.md | Primary topic keywords appear in ≥2 of {title, description, h1} | LOW |

**Cross-references (do not duplicate):**

- Heading hierarchy is `rule-u-ada-004` (universal/ada/heading-hierarchy.md). SEO worker must NOT re-flag heading skips — only flag missing-h1 if the route has zero h1 in the rendered output.
- Meaningful alt text is `rule-u-ada-012` (universal/ada/meaningful-alt-text.md). SEO worker may reference alt-text shape only when it directly affects `og:image:alt` / image SEO surfaces.

**What this worker reads (per file in diff):**

1. `<head>` block of `index.html` or any SSR template
2. Files matching `**/use-seo-head*`, `**/use-structured-data*`, `**/seo/**`, `**/json-ld/**`
3. `public/.htaccess`, `public/robots.txt`, `public/sitemap.xml`, any `_headers`, `vercel.json`, `netlify.toml`, `nginx.conf`
4. Locale snippet files (`snippets.js`, `messages/*.json`, `i18n/*.js`) — only the `meta` / `title` / `description` / `og` subtrees
5. Built `dist/index.html` (and per-locale variants) when the PR includes a build artifact

**What this worker does NOT touch:**

- Performance metrics (LCP, CLS budgets) — covered by mobile/performance workers
- Heading structure violations beyond presence of an h1
- ARIA / a11y attributes
- Build pipeline / Vite config (only flag if SEO-affecting plugin is removed)

**Severity rubric for this worker:**

| Severity | Trigger |
|---|---|
| CRITICAL | Indexability blocker (`noindex` on public route, missing canonical on multi-locale, JSON-LD that fails to parse). Use sparingly — most SEO defects are HIGH or below. |
| HIGH | Search-result quality blocker (missing title/description, broken hreflang, JSON-LD missing required `@type`). |
| MEDIUM | Crawler-friendliness or privacy gap (plaintext mailto, missing HSTS, image without dimensions, OG image not absolute). |
| LOW | Optimization gap (twitter:card absent, keyword coverage sparse, robots meta missing `max-image-preview`). |

**Worker instructions:**

Review all changed files against these 13 rules. For each finding:

1. Confirm the rule's `### Apply` condition matches.
2. Quote the offending line.
3. Provide a `Good` replacement consistent with the rule's example.
4. Cite the rule ID in the YAML.

Skip rules whose tags don't appear in the diff or the targeted file sections. Report YAML findings; return `NO VIOLATIONS` if clean.

```yaml
- rule: rule-u-seo-008
  file: src/views/components/sections/hero.vue
  line: 184
  severity: MEDIUM
  problem: Plaintext mailto rendered into SSR HTML — harvestable by spam scrapers
  before: 'href="mailto:user@example.com"'
  after: ':href="contact_email_href"  // assembled in onMounted from split parts'
```
