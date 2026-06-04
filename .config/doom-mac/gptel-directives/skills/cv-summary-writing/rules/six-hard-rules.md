---
title: The Six Hard Rules — One-Sentence, No-Subject, Buzzword Ban, Participle Bridge, Stack Integration, Flattened Acronyms
impact: CRITICAL
impactDescription: Six absolute rules that every CV summary draft must satisfy. Violating any one produces a draft that reads as a job-description bullet, a recruiter-cliché summary, or a fragmented multi-sentence paragraph that loses ATS scanning advantages.
tags: cv, summary, rules, six-rules, one-sentence, no-subject, buzzword-ban, participle-bridge, stack-integration, flattened-acronyms, writing-rules, voice, pronouns
---

This rule defines the six absolute rules that every CV/professional summary must satisfy. Each rule emerged from a specific failure mode during 12 iterations of drafting with a reviewer. The rules are non-negotiable — violating any one regresses the draft to a previously-rejected version.

## Rule 1 — The One-Sentence Mandate

**No periods, no paragraph breaks, no bullet points after the initial sentence begins.** The entire summary must be parsable in one breath. Only a single terminal period is allowed.

### Why

A modern ATS scanner reads the entire summary as one token stream. Sentence breaks add no parsing value but introduce rhythm interruptions that humans read as "shifts in voice". Three short sentences read as three separate claims; one chained sentence reads as a unified candidate profile.

### Correct vs. Incorrect

**Incorrect:** Multi-sentence form.
> Senior Engineer with 8+ years. Specialized in React and Node. Strong focus on performance.

**Correct:** Single-sentence form.
> *Senior Engineer* with 8+ years building scalable web applications using *React* and *Node*, integrating performance optimization in fully remote teams.

---

## Rule 2 — The No-Subject Rule (Zero Pronouns)

**Never use "I", "he", "they", or implied subjects.** Do NOT start new clauses with standalone action verbs like `Leads`, `Drives`, `Delivers`, `Owns`, `Architects` — those imply an unstated "He" or "She" subject.

### What IS allowed

All clauses must be one of:

| Clause type | Example |
|---|---|
| Noun phrase | `Senior Engineer with...`, `fully remote Agile teams` |
| Past participle | `Specialized in...`, `Built into every release` |
| Present participle (the bridge) | `integrating`, `building` |
| Prepositional phrase | `with end-to-end ownership`, `across stacks` |

### What is NOT allowed

| Banned opener | Why |
|---|---|
| `I specialize in...` | First-person pronoun |
| `Leads architecture decisions...` | Standalone verb implies unstated subject |
| `Drives performance gains...` | Same |
| `Delivers production-ready releases...` | Same |
| `Owns delivery from design to launch...` | Same |
| `Architects scalable systems...` | Same — note: `architecting` IS allowed as the bridge verb (Rule 4), but `Architects` is not allowed as a clause opener |

### Correct vs. Incorrect

**Incorrect:** Standalone verb opener.
> ...using TypeScript and React, **Leads** architecture decisions...

**Correct:** Participle bridge instead.
> ...using TypeScript and React, **integrating** architecture decisions...

---

## Rule 3 — The Buzzword Ban

**Absolutely banned phrases.** These trigger immediate rejection from human reviewers as recruiter-cliché:

| Banned phrase | Why |
|---|---|
| `Proven track record` / `Historial comprobado` | Generic recruiter cliché |
| `Core expertise` | Filler — list the actual expertise instead |
| `Specializing in` | "Specialized in" (past participle) is acceptable; "Specializing" (present) is generic |
| `Experienced in` / `Especializado en` (as sentence opener) | Filler — replace with `with` + concrete claim |
| `Performance-first across the stack` | Slogan — say what you actually do |
| `Baked into delivery` | Marketing copy — use `integrated`, `built into every release` |
| `Repetitive scaffolding` (as filler) | If using as filler word; concrete reference (e.g., "automates repetitive scaffolding") is OK |
| `Production-ready solutions` / `Production-grade releases` (as filler delivery mechanism) | Generic — drop the delivery-mechanism slot if no concrete metric |
| `Ship production-grade releases` | Filler tail; drop it entirely |
| `Team player` | Junior tell |
| `Self-starter` | Junior tell |
| `Passionate about` | Generic |
| `Strong communicator` | Generic |

### Acceptable alternatives

| If you'd say... | Use instead |
|---|---|
| "Experienced in frontend architecture" | "...integrating frontend architecture, performance optimization..." (move into Specialized Domains slot) |
| "Proven track record of leading..." | "Senior [Role] with 8+ years building..." (Identity & Experience slot carries the seniority signal) |
| "Specializing in scalable systems" | "...building scalable, high-performance [platforms]..." (move into Core Output slot) |

---

## Rule 4 — The Participle Bridge

**Connect the first half (identity + tech stack) to the second half (skills + environment) using a present participle ending in `-ing`.**

### Acceptable bridges

| Bridge | When to use | Spanish equivalent |
|---|---|---|
| `integrating` | Default — works for most domains | `integrando` |
| `combining` | When the second half pairs two distinct disciplines (e.g., frontend + data) | `combinando` |
| `architecting` | When the second half emphasizes system design | `arquitectando` (use sparingly in ES — see anti-patterns) |
| `bridging` | When the second half emphasizes cross-functional or cross-team work | `puenteando` (rare in ES — prefer `integrando`) |

**Note on `architecting`:** It is acceptable AS A BRIDGE VERB in Rule 4. It is NOT acceptable as a standalone clause opener (Rule 2) or as the verb form in role summaries (where "building" or "leading" is preferred). The bridge use is distinct.

### Why the bridge matters

Without it, the second half has to start with either a standalone verb (violates Rule 2) or a new sentence (violates Rule 1). The participle is the ONLY way to chain the two halves cleanly.

### Correct vs. Incorrect

**Incorrect:** Conjunction `and` linking halves.
> ...using Vue and Node, **and** integrating performance...

This reads as a list extension, not a transition between identity and domain practice.

**Correct:** Participle as soft pivot.
> ...using *Vue* and *Node*, integrating performance optimization, accessibility, and...

---

## Rule 5 — Stack Integration

**Tech stack names must be directly attached to platforms via `using` — never listed as a separate thought.**

### Correct pattern

```
...building [platforms/products] using [tech1, tech2, tech3],...
```

The `using` clause modifies the [Core Output] slot. The tech stack appears as a list of tools the candidate uses to build the named output.

### Correct vs. Incorrect

**Incorrect:** Tech stack as separate thought.
> Senior Engineer with 8+ years building e-commerce platforms. **Tech stack:** Vue 3, React, Next.js.

Two sentences (violates Rule 1), and the stack is listed as a label rather than connected to platforms.

**Incorrect:** Tech stack as parenthetical.
> Senior Engineer with 8+ years building e-commerce platforms **(Vue 3, React, Next.js)**, integrating...

Parens break single-breath flow.

**Correct:** Stack glued to platforms via `using`.
> *Senior Engineer* with 8+ years building **e-commerce platforms using *Vue 3*, *React*, and *Next.js***, integrating...

---

## Rule 6 — Flattened Acronyms

**Never chain technical abbreviations with slashes in the summary.** Single standalone acronyms are fine; SLASH CHAINS are banned.

### Banned chains and their replacements

| Banned chain | Replace with |
|---|---|
| `SSR/SSG` | "web performance" or "server-side rendering" |
| `SSR/SSG/ISR` | "web performance" or fold into "performance optimization" |
| `ADA/WCAG` | "accessibility" |
| `SEO/AEO` | "SEO" (drop AEO from summary; covered in role bullets) |
| `CI/CD` | "DevOps" (or drop entirely) |
| `TDD/BDD` | "testing" (or drop entirely) |
| `B2B/B2C` | "consumer-facing" or "enterprise" — pick the more accurate one |
| `iOS/Android` | "mobile" |
| `REST/GraphQL` | "API design" |

### Acceptable standalone acronyms

These appear alone without slashes:

| Acronym | Use as-is |
|---|---|
| `SEO` | ✓ |
| `SaaS` | ✓ |
| `API` | ✓ — but prefer "API design" or "APIs" as the noun |
| `UI`, `UX` | ✓ |
| `CMS` | ✓ |
| `ADA` (alone, not chained) | ✓ — but only if NOT part of `ADA/WCAG` |

### Why

ATS scanners are bag-of-words — they hit `accessibility` just as reliably as `ADA/WCAG`. Slash chains add visual noise that human readers must parse. Flattened outcomes (`accessibility`, `web performance`, `DevOps`) read as natural prose AND still hit the ATS keyword tier.

### Correct vs. Incorrect

**Incorrect:** Acronym chain density.
> ...integrating *SSR/SSG/ISR*, *ADA/WCAG* compliance, and *CI/CD* pipelines...

Three slash chains in one clause. Reads as alphabet soup.

**Correct:** Flattened to outcomes.
> ...integrating web performance, accessibility, and DevOps...

Same ATS coverage, natural prose.

---

## Compliance Checklist (Run Before Applying Any Summary Edit)

- [ ] Rule 1: Single terminal period? No mid-summary periods?
- [ ] Rule 2: Zero pronouns? No standalone verbs at clause starts?
- [ ] Rule 3: No banned phrases from the buzzword list?
- [ ] Rule 4: Present participle (`integrating`, `combining`, `architecting`, `bridging`) connecting halves?
- [ ] Rule 5: Tech stack attached to platforms via `using`?
- [ ] Rule 6: No slash chains? All acronym pairs flattened to outcomes?

If any answer is "no", the draft has regressed. Fix before presenting to the user.
