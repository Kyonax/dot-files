---
name: cv-summary-writing
description: >-
  Write, refine, audit, or rewrite professional summary paragraphs for CVs,
  LinkedIn About sections, personal site hero copy, and other professional bios.
  Applies a 6-rule one-sentence recipe (slot template + no-subject voice +
  buzzword ban + participle bridge + stack integration + flattened acronyms),
  the canonical v12 worked example (EN + ES), the soft-skills tail pattern
  (ownership, mentorship, adaptability), twin-concept redundancy avoidance
  (shared head nouns), ATS keyword density targets, the proposed-vs-applied
  protocol for iterative collaboration, and PDF filename conventions for
  recruiter-facing downloads. Captures lessons from a 12-iteration drafting
  journey so future drafts converge in 1-2 cycles instead of 12.
  Trigger: 'write CV summary', 'draft summary paragraph', 'rewrite LinkedIn About',
  'audit CV header', 'fix summary buzzwords', 'refine professional bio',
  'add soft skills to summary', 'check ATS keywords in summary',
  'rename CV PDF', 'CV recipe', 'summary recipe', 'one-sentence summary',
  'no recruiter cliches', 'professional summary'.
user-invocable: true
metadata:
  openclaw:
    emoji: 📝
    os: [darwin, linux]
---

# CV Summary Writing Skill

Provides the canonical 6-rule recipe for writing professional summary paragraphs for CVs, LinkedIn About sections, personal site hero copy, and other professional bios. The recipe was forged through 12 iterations of drafting and reviewer feedback — each iteration revealed a specific failure mode. Future drafts must APPLY the recipe, not re-derive it.

## Core Principle: Apply the Recipe, Don't Re-Derive

The recipe is the convergence point of a long iteration journey. Every rule prevents a known failure mode. Re-deriving the recipe from scratch when drafting a new summary wastes hours and produces drafts that hit the same rejection patterns. The Worker AI should:

1. **Read the rules first** — understand the slot template, the six hard rules, and the anti-patterns BEFORE drafting
2. **Fill the slots** — don't invent new sentence structures
3. **Use the Proposed-vs-Applied protocol** — present drafts; wait for approval; never overcorrect on "almost there"
4. **Reference the canonical worked example** — the v12 EN + ES summary is the validated reference filling

## When to Apply

Reference these guidelines when:

*   Writing a new CV summary paragraph from scratch.
*   Refining an existing CV summary based on reviewer feedback.
*   Auditing a CV summary for buzzwords, banned phrases, or recipe violations.
*   Drafting a LinkedIn About section using the same recipe (single-sentence form adapted to the LinkedIn surface).
*   Writing the hero summary on a personal website or About page.
*   Adding soft skills (ownership, mentorship, adaptability) to an existing summary without breaking the recipe.
*   Merging twin concepts (e.g., performance + conversion optimization) without keyword stuffing.
*   Checking ATS keyword density against the target tiers.
*   Renaming CV PDFs on a candidate's personal site to follow professional conventions.
*   Recovering from a stuck iteration loop where the user is frustrated and rejecting drafts.

## When to Read Which Rules

| If working on... | Read these rules |
|---|---|
| Writing a CV summary from scratch | `rules/recipe-template.md` + `rules/six-hard-rules.md` + `rules/ats-keyword-density.md` |
| Refining an existing draft after reviewer feedback | `rules/six-hard-rules.md` + `rules/anti-patterns.md` + `rules/proposed-vs-applied-protocol.md` |
| Adding soft skills to a summary | `rules/soft-skills-tail.md` + `rules/six-hard-rules.md` |
| Adding multiple related keywords without redundancy | `rules/redundancy-avoidance.md` + `rules/ats-keyword-density.md` |
| Auditing an existing summary for buzzwords | `rules/six-hard-rules.md` + `rules/anti-patterns.md` |
| Adapting the recipe to LinkedIn About / hero copy | `rules/recipe-template.md` + `rules/six-hard-rules.md` |
| Recovering from a stuck iteration cycle | `rules/proposed-vs-applied-protocol.md` + `rules/anti-patterns.md` |
| Naming or renaming CV PDFs on a candidate's website | `rules/pdf-filename-conventions.md` |
| Estimating ATS score impact of a draft | `rules/ats-keyword-density.md` |
| Checking compliance before applying any edit | `rules/six-hard-rules.md` (compliance checklist at the end) |

## Quick Reference

| Rule | Description |
|---|---|
| `recipe-template` | 10-slot single-sentence template (Identity → Core Output → using → Tech Stack → comma → Participle Bridge → Specialized Domains → in → Environment → comma → Soft Skills Tail → period); canonical v12 worked example EN + ES; slot-by-slot fill map; adaptation guidance for LinkedIn About, hero copy, and professional bios. |
| `six-hard-rules` | The six absolute rules every summary must satisfy: one-sentence mandate, no-subject (zero pronouns + no standalone verbs like Leads/Drives/Owns/Architects), buzzword ban (no Proven track record / Core expertise / Specializing in / Experienced in / Performance-first), participle bridge (integrating/combining/architecting/bridging), stack integration via `using`, flattened acronyms (no SSR/SSG, ADA/WCAG, CI/CD slash chains). Includes compliance checklist. |
| `anti-patterns` | Documents the 12-iteration drafting journey with specific failure modes and rejected drafts. Reference table of banned patterns (recruiter clichés, verb-first openers, delivery arcs, generic outcomes, word repetition, em-dashes, semicolons, voice inconsistency). Frustration signal recognition guide (almost there / still awful / I don't know what I want). |
| `soft-skills-tail` | Pattern for adding leadership/ownership/adaptability signals as a prepositional tail modifier (`with [skill1], [skill2], and [skill3 + dimension list]`). Approved tokens (end-to-end ownership, mentorship, technical leadership, fast adaptability) vs banned junior tells (team player, self-starter, passionate about, strong communicator). Adaptability dimension triad (stacks, products, industries). |
| `redundancy-avoidance` | Shared head noun pattern for twin concepts (e.g., `performance and conversion optimization` instead of `performance optimization, conversion optimization`). Reference table of common twin-concept merges in EN and ES. Keyword frequency limits (max 2 surface occurrences per word). ES grammar notes for noun-genitive ordering. |
| `ats-keyword-density` | Keyword coverage targets by tier (Title, Years, Domain, Stack, Discipline, Environment, Soft Skill, Adaptability Dimensions). Length window 50-80 words. Intentionally omitted keywords (testing, DevOps, SSR, CWV, Lighthouse, Colombia) with rationale for each. The "fits the recipe" 5-question test. ATS scanner repetition weighting (Greenhouse / Workday / Lever). |
| `proposed-vs-applied-protocol` | 5-step protocol for collaborative editing (Present → Wait → Execute → No-batch → No-overcorrect). The "almost there" rule (surgical changes only). The "I don't know what I want" working methods (reference / user-writes-rough / strip-down / park). Overcorrection failure mode documentation (v3 → v4 regression). |
| `pdf-filename-conventions` | Naming conventions for recruiter-facing CV PDFs. On-disk format (`[Name]-[Title]-[Locale].pdf`) vs download attribute format (`[Name]-[Title]-CV.pdf`, locale-agnostic). PascalCase hyphenated, "Senior Software Engineer" as most-general title, CV over Resume. Implementation snippets for Vue 3, React/Next.js, and plain HTML. ATS filename parsing notes (Workday). |
