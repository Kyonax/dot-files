---
title: ATS Keyword Density Targets and Intentional Omissions
impact: HIGH
impactDescription: Defines what keywords should appear in a CV summary (with target frequency), how long the summary should be, and what high-value keywords are intentionally omitted despite ATS scoring value. Without these targets, summaries either over-stuff (keyword salad) or miss high-value tokens.
tags: cv, summary, ats, keywords, density, frequency, length, omissions, tiers, scanners, greenhouse, workday, lever, parser
---

This rule defines the ATS keyword density targets for a CV summary written using the recipe. It specifies target token coverage by tier, the length window, and which high-value ATS tokens are INTENTIONALLY OMITTED because they would break the recipe's structural rules or push the summary past comfortable reading length.

## Keyword Coverage by Tier

The summary should hit tokens across these tiers. Each row indicates target frequency per single summary.

| Tier | Tokens | Target frequency |
|---|---|---|
| **Title** | `Senior` / `Lead` / `Staff` / `Principal` + `Engineer` / `Developer` | 1× each |
| **Years** | `8+ years`, `10+ years`, etc. | 1× |
| **Domain** | `e-commerce`, `SaaS`, `consumer-facing`, `web applications`, `enterprise`, `B2B`, `B2C` (pick the accurate ones) | 1× each |
| **Stack** | Each named tech (`Vue 3`, `React`, `Next.js`, `TypeScript`, `Node.js`, `Python`, etc.) | 1× each, in [Tech Stack] slot |
| **Discipline** | `frontend architecture`, `performance optimization`, `conversion optimization`, `accessibility`, `SEO`, `AI-assisted development`, `DevOps`, `testing`, `API design`, `microservices` | 1-2× each across the summary |
| **Environment** | `remote`, `Agile`, `Scrum`, `cross-functional` | 1× each |
| **Soft skill** | `ownership`, `mentorship`, `adaptability`, `technical leadership` | 1× each in the tail |
| **Adaptability dimensions** | `stacks`, `products`, `industries`, `services`, `domains`, `team sizes` | 1× each in dimension list |

A well-formed summary using the recipe hits **25–35 distinct ATS-scored tokens in 50-80 words.**

## Length Target

| Length | Status |
|---|---|
| < 40 words | Too short — likely missing a critical slot |
| 40–50 words | Lean — acceptable if all critical slots present |
| **50–80 words** | **Target range — comfortable reading + full ATS coverage** |
| 80–100 words | Acceptable for senior roles with rich background |
| > 100 words | Bloated — tighten by removing weakest [Specialized Domains] entries first |

## High-Value Keywords That Are INTENTIONALLY OMITTED

These keywords have real ATS value but are excluded from the canonical summary because they would break the recipe. They appear in role bullets (Section 3 of a CV) instead, where ATS still hits them.

| Omitted keyword | Why omitted | Where it lives instead |
|---|---|---|
| `testing`, `TDD`, `Playwright`, `Jest`, `Vitest` | Adding `testing` to [Domains] crowds the list; specific tool names break Rule 6 (acronym chains) and add visual noise | Role bullets (per-role tooling) |
| `DevOps`, `CI/CD`, `Docker`, `Kubernetes` | `CI/CD` is a slash chain (banned by Rule 6); `Docker` is too specific for summary level | Role bullets + Skills section |
| `SSR`, `SSG`, `ISR` | Slash-chain banned; flatten to `web performance` if needed | Role bullets |
| `ADA/WCAG` | Slash chain banned; flatten to `accessibility` (Rule 6) | Role bullets if specific to a role; or use `accessibility` alone |
| `Core Web Vitals`, `Lighthouse` | Parenthetical tool mentions disrupt single-breath flow (Rule 1 spirit) | Role bullets where performance baselines were actually used |
| `microservices`, `API design` | Specific architectural choices — covered by experience bullets | Role bullets |
| Specific location (`Colombia`, `Remote — Berlin`) | Location specifier often redundant when `fully remote` is in environment; can read junior | Header contact line (not the summary paragraph) |
| Explicit `Available for X roles` | Reads junior — `fully remote Agile teams` carries the availability signal | Drop entirely, or use in a hero-page CTA outside the summary |

## Keyword Repetition Rules

Bag-of-words scanners (Greenhouse, Workday, Lever) weight tokens with diminishing returns:

| Occurrence count | Score impact |
|---|---|
| 1× | Full token weight |
| 2× | ~1.4× weight (still rewarded) |
| 3× | ~1.6× weight (slight penalty for stuffing starts here) |
| 4×+ | ~1.5× weight + stuffing flag (negative effect on human reviewers) |

**Rule of thumb:** Target 1-2 surface occurrences of any specific token. The recipe naturally hits this — most tokens appear once (in their dedicated slot).

The exception is `performance`, which can legitimately appear twice:
- As adjective in [Core Output] (`high-performance platforms`)
- As noun in [Domains] (`performance optimization`)

Both occurrences are grammatically distinct uses, so they don't trigger stuffing penalties.

## The "Fits the Recipe" Test

Before adding ANY new keyword/phrase to the summary, run this 5-question check:

1. **Can it be expressed without subjects or standalone verbs?** (Rule 2 of the six hard rules)
2. **Is it a buzzword from the banned list?** (Rule 3)
3. **Can it fit into [Specialized Domains] as a noun, or [Soft Skills Tail] as a noun?** (Rules 4+)
4. **Does it create acronym slash chains?** (Rule 6)
5. **Will adding it push the sentence past ~80 words?** (Length)

If any answer fails → reject or refactor before adding.

## Worked Example — Counting Tokens on the Canonical Summary

Using the canonical v12 worked example:

> *Senior Full-Stack and Frontend Engineer* with 8+ years building scalable, high-performance e-commerce platforms, SaaS products, and consumer-facing web applications using *Vue 3*, *React*, *Next.js*, *TypeScript*, and *Node.js*, integrating frontend architecture, performance and conversion optimization, accessibility, SEO, and AI-assisted development in fully remote Agile teams, with end-to-end ownership, mentorship, and fast adaptability across stacks, products, and industries.

**Token inventory (30 distinct ATS-scored tokens in 62 words):**

| Tier | Tokens hit |
|---|---|
| Title | `Senior`, `Full-Stack`, `Frontend`, `Engineer` |
| Years | `8+ years` |
| Core Output | `scalable`, `high-performance`, `e-commerce`, `SaaS`, `consumer-facing`, `web applications` |
| Stack | `Vue 3`, `React`, `Next.js`, `TypeScript`, `Node.js` |
| Discipline | `frontend architecture`, `performance optimization`, `conversion optimization`, `accessibility`, `SEO`, `AI-assisted development` |
| Environment | `remote`, `Agile` |
| Soft skills | `end-to-end ownership`, `mentorship`, `adaptability` |
| Dimensions | `stacks`, `products`, `industries` |

**Density:** 30 distinct tokens / 62 words = ~0.48 ATS-scored tokens per word. This is the target density.

## Correct vs. Incorrect

### Example A — Under-stuffed Summary

**Incorrect:** Missing critical tokens.
> Senior Engineer with 8+ years building web apps with React and Node, working in remote teams, with ownership.

Hits ~12 tokens, missing: domain breadth, multiple stack names, discipline keywords, soft skill triad, dimensions.

**Correct:** Add missing tiers.
> *Senior Full-Stack Engineer* with 8+ years building scalable e-commerce platforms and consumer-facing web applications using *React*, *TypeScript*, and *Node.js*, integrating performance optimization, accessibility, and SEO in fully remote Agile teams, with end-to-end ownership, mentorship, and fast adaptability across stacks, products, and industries.

### Example B — Over-stuffed Summary

**Incorrect:** Adds testing, DevOps, microservices, AWS, CI/CD all at once.
> *Senior Engineer* with 8+ years building platforms using *Vue 3*, *React*, *Next.js*, *TypeScript*, *Node.js*, *Python*, *Go*, *Rust*, integrating performance optimization, conversion optimization, accessibility, SEO, AEO, testing, TDD, DevOps, CI/CD, AWS, GCP, microservices, API design, system design, and AI-assisted development in fully remote Agile teams, with ownership, mentorship, leadership, adaptability, collaboration, and stakeholder management across stacks, products, services, domains, industries, and team sizes.

Hits 50+ tokens but reads as keyword salad. Past 100 words. Recruiter rejects on sight.

**Correct:** Trim to canonical density.
> Use the canonical v12 worked example (62 words, 30 tokens).
