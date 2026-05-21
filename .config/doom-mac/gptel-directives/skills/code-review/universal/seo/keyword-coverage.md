---
id: rule-u-seo-013
title: Primary Keywords Span Title + Description + H1
severity: LOW
tags: title, description, h1, meta, keyword-coverage
---

The page's primary topic keywords (≤5 terms identifying what the page is about) must appear in at least two of three surfaces: `<title>`, `<meta name="description">`, and the visible `<h1>` (or first-rendered heading). Keyword density is no longer a ranking signal, but cross-surface presence helps Google understand the topic and helps users recognize relevance in snippets.

### Apply
- Routes meant to rank for specific terms (homepage, key landing pages, blog posts)
- Files producing title, description, and the first heading in the same template / locale snippet tree

### Skip
- Generic policy pages (privacy, terms, 404)
- Routes intentionally `noindex`
- Sites where the H1 is a brand wordmark and keywords live in subheadings — verify with the user before flagging

### Bad
```js
// Title: "Senior Full-Stack Software Engineer, Remote from Colombia"
// Description: "Quick personal portfolio."
// H1: "CRISTIAN D. MORENO"
// Coverage: 0/3 — none of the keywords (engineer, colombia, remote) appear anywhere
```

### Good
```js
// Title: "Senior Full-Stack Software Engineer, Remote from Colombia"
// Description: "8 years building scalable, performant web apps. ... Available for remote work."
// H1: "CRISTIAN D. MORENO" + hero summary mentioning "remote from Colombia"
// Coverage: 3/3 — engineer/colombia/remote across title + description + hero copy
```

### Edge
This rule is LOW because the underlying audit tool (Lighthouse, Sitechecker, SEMrush) reports it as informational, not blocking. Use the rule when the page is on a target-keyword list (kept in a `seo-targets.json` or roam node). If no target list exists, this rule should be skipped rather than guessed at — extracting "common words" from copy is a worse signal than asking the project owner. Surface the H1 keyword gap as a copy suggestion, not a code-fix.
