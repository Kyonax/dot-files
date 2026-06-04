---
title: Anti-Patterns in CV Summary Writing — Lessons from a 12-Iteration Drafting Journey
impact: HIGH
impactDescription: Documents the specific patterns that failed user review across 12 iterations of CV summary drafting. Re-deriving these wastes 3+ hours per session. Use this rule when iterating on a draft that the user has rejected.
tags: cv, summary, anti-patterns, banned-patterns, recruiter-cliche, iteration-journey, learnings, rejected-drafts, voice, tone, registry, frustration-signals
---

This rule documents the anti-patterns surfaced during 12 drafting iterations on a senior engineer's CV summary. Each pattern triggered a user rejection with specific feedback. Future drafts must avoid these patterns BEFORE presenting to the user — otherwise the iteration cycle restarts.

## The 12-Iteration Timeline (Reference)

The following table is the historical record of what failed and why. Use it to recognize patterns when a fresh iteration starts heading in a known-rejected direction.

| Version | Pattern Tried | User Reaction | Lesson |
|---|---|---|---|
| Pre-session baseline | `architecting`, `Proven track record`, `Experienced in frontend architecture, SSR/SSG, CI/CD, AI-assisted development systems` | "Awkward words" | The verb `architecting` reads forced; slash chains read as alphabet soup |
| v0 | `architecting → building`; `Proven track record → Track record` (sentence-level swaps) | Flagged middle sentence ("Experienced in...") | Surgical fixes don't address the structural problem |
| v1 | `Performance-first across the stack` as lead | Rejected | Slogan-style openers are pure buzzword |
| v2 | `Leads architecture decisions from initial design through Lighthouse...` | Rejected | Verb-first openers (`Leads`, `Drives`) read as job description bullets |
| v3 | `Each project moves from architecture decisions through Lighthouse and Core Web Vitals...`; `consumer-facing web applications` introduced | "Almost there" but movement-arc flagged | Manufactured "delivery arc" rhetoric (`moves from X through Y`) sounds artificial |
| v4 | `In practice, that means architecture decisions made upfront...` | Rejected — "too explanatory" | Explanatory bridges (`In practice, that means`) read as presentation slides |
| v5 | `Architecture is decided upfront, performance is established with Lighthouse...` (passive three-clauses) | Rejected — "still awful" | Passive voice clauses with `is decided`/`is established` read flat |
| Recovery | User shared reference: `Senior Full-Stack Engineer with 8+ years of experience building scalable SaaS and e-commerce platforms. I specialize in frontend architecture using TypeScript, React, Vue 3, and Node.js.` | "This is perfect" — gave the voice template | When stuck, ask for a reference example — that unlocks the voice |
| v6 | Matched reference voice; concise 3-sentence form | Accepted as direction | Reference matching beats blind drafting |
| v7 | User-provided recipe template: one-sentence + slot system + 6 rules | "Almost there" | The recipe template is the right structure |
| v8 | Dropped `to ship production-grade releases` | Better | Filler delivery mechanisms hurt; drop the slot if no concrete metric |
| v9 | Added `testing` + `DevOps` to domains list | Rejected | Don't pile keywords; respect the user's vetoed additions |
| v10 | Reverted testing/DevOps; added `performance optimization` + soft skills tail (`ownership`, `adaptability`) | Better; adaptability needed broader dimensions | Soft skills tail is the right place for leadership signals; dimension lists need 3 dimensions |
| v11 | Adaptability expanded to `across stacks, products, and industries` | Good base | Three-dimension lists (stacks + products + industries) cover the breadth |
| v12 | Added `frontend architecture`, `mentorship`; merged `performance and conversion optimization` with shared head noun | **WINNER** — "That is the winner!!!!" | Twin-concept merging, soft skills full triad, and architecture explicitly named — all required |

## Anti-Pattern Reference Table

Each row is a pattern to avoid, with the failure mode and the fix.

| Anti-pattern | Why it fails | What to do instead |
|---|---|---|
| Recruiter clichés (`Performance-first across the stack`, `baked into delivery`, `repetitive scaffolding` as filler) | Reads as marketing copy | Use concrete domain names without sloganeering |
| Verb-first job-description openers (`Leads architecture decisions`, `Owns delivery`, `Drives performance`) | Reads like a JD bullet, not a candidate summary | Use noun-phrase or past-participle openers; bridge via present participle |
| Artificial delivery arcs (`Each project moves from X through Y`) | Manufactured rhetoric — reads as scaffolding | Drop the arc; let the practices stand on their own |
| Generic outcomes (`ship production-grade releases`, `deliver maintainable code`) | Filler; says nothing concrete | Drop the delivery-mechanism slot entirely if no concrete metric available |
| Buzzword chains (`frontend initiatives`, `production-ready solutions in fully remote Agile environments`) | Recruiter clichés stacked | Express the same idea via concrete domains + soft skills |
| Filler list-shapes (`Experienced in X, Y, Z, ...`) | `Experienced in` is the buzzword trigger | Drop the opener; integrate domains via the bridge verb instead |
| Word repetition (`frontend` appearing 3+ times in one paragraph) | Reads as awkward | Mention `frontend` ONCE in title; let domains carry the dimension via `frontend architecture` |
| Generic location framings (`fully remote Agile environments from Colombia`) | Wordy; country specifier often redundant | `fully remote Agile teams` alone carries the signal |
| Em-dash as bridge (`—`) | Visual interrupt; banned in user-facing copy per common style rules | Use commas |
| Semicolons or colons in body summary | Visual interrupt; banned in user-facing copy | Use commas and periods only |
| Mixing first person and impersonal voice (`I specialize` then `Architecture is decided upfront`) | Inconsistent voice register | All-impersonal: noun phrases + participles only |
| Asking "what specifically don't you like" with no concrete options | Iteration without convergence | Use structured multi-select questions; offer 4 working methods (reference / user-writes-rough / strip / park) |
| Twin-concept redundancy (`performance optimization, conversion optimization`) | Reads as keyword-stuffed | Merge head noun: `performance and conversion optimization` (see redundancy-avoidance.md) |
| Pile-on keyword additions (`testing` + `DevOps` + `microservices` + `SSR` all added at once) | Sentence bloats past 80 words; reads as keyword salad | Add max 1 keyword per iteration; respect previous vetoes |
| Wholesale rewrites after `almost there` feedback | Destroys what was working | Surgical changes only — touch ONLY the flagged phrase |

## Frustration Signal Recognition

When iterating, recognize these user signals and respond appropriately rather than presenting another draft:

| User signal | What it means | What to do |
|---|---|---|
| "Almost there" | Direction is right; ONE specific thing is wrong | Surgical refinement on the flagged phrase only |
| "Still awful" / "Worse than the start" | Wholesale rewrite went wrong; revert | Go back to the previous-version baseline and iterate surgically |
| "I don't know what I want" + "this is awful" | Drafting cycle is broken | STOP drafting. Offer 4 working methods (see proposed-vs-applied-protocol.md) |
| "You're almost to nail it but [specific phrase] still feels [X]" | Refine that phrase; everything else is fine | Touch only that phrase; preserve the rest |
| "This is perfect" / "That is the winner" | Lock the draft | Apply immediately; do not iterate further |
| User shares a reference example unprompted | They have a voice in mind they can't articulate | Match the reference voice exactly; that's the unlock |

## Containerization Note

The 12-iteration journey table above is preserved as a HISTORICAL RECORD. Future use of this skill on different candidates' CVs may produce different iteration counts and different specific patterns — but the GENERAL anti-pattern table (above) generalizes. The journey table tells you that 12 iterations DID happen and these specific patterns failed; the anti-pattern table tells you what to avoid REGARDLESS of which candidate's CV you're drafting.
