# Architectural Rationale — CV Summary Writing

This document explains the WHY behind the rules in this skill — why the recipe exists in this specific shape, why each rule prevents a specific failure mode, and how the rules connect into a coherent writing discipline. Read this when you need to understand the philosophy; read individual `rules/*.md` files when you need the implementation detail.

## The Two Audiences

Every professional summary paragraph must serve TWO audiences simultaneously:

| Audience | What they look for | Reading mode |
|---|---|---|
| **ATS scanner** (Greenhouse / Workday / Lever / AI parsers) | Bag-of-words keyword matching against job descriptions | Token frequency + match density |
| **Human recruiter** | Quick first-impression of seniority, fit, and voice | Skim in 5-10 seconds, decide whether to read deeper |

The recipe optimizes for both:
- **Keyword density** satisfies the ATS — 30+ distinct tokens in 62 words
- **Single-sentence form** satisfies the human — reads as a unified candidate profile, parsable in one breath

A summary that wins ATS but fails recruiter review (keyword-stuffed salad) is useless. A summary that wins recruiter review but misses ATS keywords (eloquent but token-sparse) is also useless. The recipe threads both needles.

## Why One Sentence?

The single-sentence mandate (Rule 1) is the most important structural choice. Three reasons:

1. **Unified token stream for ATS** — A bag-of-words scanner doesn't care about sentence boundaries, but multi-sentence summaries introduce "shifts in claims" that human reviewers parse as separate assertions. Three short sentences = three separate claims to evaluate; one chained sentence = one unified profile.

2. **Forces concision** — When the entire summary must fit in one breath (50-80 words), there is no room for filler. Every word must earn its place. Buzzword phrases like "Proven track record of delivering production-ready solutions" can't survive a one-sentence budget — they get cut for higher-density domain keywords.

3. **Demonstrates discipline** — A senior engineer who can communicate their full profile in one well-constructed sentence signals communication skill. A multi-paragraph summary often signals lack of editing.

## Why No Subjects?

Rule 2 (the no-subject rule) forces an impersonal voice via noun phrases, past participles, present participles, and prepositional phrases. Three reasons:

1. **"I" sounds like LinkedIn About, not CV header** — First-person CVs read informal. Senior tier expects impersonal voice.

2. **Standalone verbs (`Leads`, `Drives`, `Owns`) read as job-description bullets** — These verbs imply an unstated subject ("[Cristian] leads architecture decisions"). On a CV, the candidate's name is already at the top — re-stating the implicit subject feels redundant.

3. **Impersonal voice integrates better with the bridge** — A present participle (`integrating`) flows naturally from impersonal preceding clauses. It breaks when preceded by "I" or "Cristian".

## Why a Participle Bridge?

The recipe splits into two halves:
- **Identity half**: who you are + what you build + the tech stack
- **Domain half**: what disciplines you bring + where you work + soft skills

Without a bridge, these halves want to be two sentences. Rule 4 (participle bridge) is the ONLY way to chain them cleanly while satisfying Rule 1 (one sentence) and Rule 2 (no standalone verbs).

The acceptable bridges (`integrating`, `combining`, `architecting`, `bridging`) all share a property: they read as soft transitions, not assertive verbs. Hard verbs at the bridge position (`Drives`, `Owns`) violate Rule 2.

## Why Flattened Acronyms?

Rule 6 (flattened acronyms) bans slash chains like `SSR/SSG`, `ADA/WCAG`, `CI/CD`. The reason is twofold:

1. **ATS doesn't need them** — Bag-of-words scanners hit `accessibility` just as reliably as `ADA/WCAG`. Slash chains add no ATS value over their flattened outcomes.

2. **Slash chains break single-breath reading** — Visual noise interrupts the prose flow. A reader pauses on `SSR/SSG/ISR` to parse three acronyms; they don't pause on `web performance`.

The exception is STANDALONE acronyms (`SEO`, `SaaS`) — these are single tokens, not chains. They flow.

## Why Soft Skills Go in a Tail?

Soft skills (ownership, mentorship, adaptability) are ATS-scored tokens that recruiters specifically scan for in senior tiers. But they can't be added to:
- [Identity & Experience] — title carries the seniority signal, adding "ownership-driven Senior Engineer" reads forced
- [Core Output] — soft skills aren't things you build
- [Tech Stack] — they're not technologies
- [Specialized Domains] — they're not disciplines

The tail prepositional phrase (`with X, Y, and Z`) is the ONLY slot in the recipe that grammatically fits soft-skill nouns. The `with` preposition keeps the no-subject rule intact while letting the candidate close on signal.

## Why Twin-Concept Merging?

When two ATS keywords share a head noun (`performance optimization` + `conversion optimization`), naive listing repeats the suffix and reads as keyword stuffing. The shared head noun pattern (`performance and conversion optimization`) keeps both ATS hits while producing cleaner prose.

The same logic generalizes to `frontend and backend architecture`, `unit and integration testing`, `team and technical leadership` — any twin concept with a grammatically shared head.

## Why the 12-Iteration History Is Preserved

The `anti-patterns.md` rule documents the FULL 12-iteration journey including specific rejected drafts. This is not a vanity log — it's a defensive document. The journey revealed:

- Specific phrases users reject ("performance-first across the stack", "Each project moves from X through Y", "In practice, that means...")
- Specific recovery patterns ("share a reference example unlocks the voice")
- Specific failure cascades (v3 → v4 overcorrection sequence)

A new drafter who skips this history will reproduce the same 12 iterations. A drafter who reads it can converge in 1-2 iterations.

## Why the Proposed-vs-Applied Protocol?

The protocol exists because of one specific failure mode: when the user said "almost there", a wholesale rewrite was attempted (v3 → v4), and the next draft was "worse than before". The recovery cost 3 additional iterations.

The protocol enforces:
- Present drafts in chat, don't apply speculatively
- Wait for unambiguous approval
- On "almost there", make ONLY the surgical fix to the flagged phrase
- Never rewrite parts the user didn't flag

This prevents the overcorrection cascade — the most expensive failure mode in collaborative drafting.

## How the Rules Connect

The skill's eight rules form a layered defense:

```
┌─────────────────────────────────────────────────────────────────┐
│ recipe-template.md      — The structural foundation             │
│                            (defines the 10 slots)                │
├─────────────────────────────────────────────────────────────────┤
│ six-hard-rules.md       — The grammatical rules                  │
│                            (defines HOW each slot is filled)     │
├─────────────────────────────────────────────────────────────────┤
│ anti-patterns.md        — The historical defense                 │
│                            (defines what NOT to do, with proof)  │
├─────────────────────────────────────────────────────────────────┤
│ soft-skills-tail.md     ┐                                       │
│ redundancy-avoidance.md ├─ Slot-specific writing rules         │
│ ats-keyword-density.md  ┘  (deepen specific slots)              │
├─────────────────────────────────────────────────────────────────┤
│ proposed-vs-applied-    — The interaction protocol               │
│      protocol.md           (defines HOW to iterate with reviewer)│
├─────────────────────────────────────────────────────────────────┤
│ pdf-filename-           — The deployment layer                   │
│      conventions.md        (defines naming for downloaded files) │
└─────────────────────────────────────────────────────────────────┘
```

A draft session typically loads:
- Always: `recipe-template.md` + `six-hard-rules.md`
- For fresh drafts: + `ats-keyword-density.md`
- For refinement: + `anti-patterns.md` + `proposed-vs-applied-protocol.md`
- For specific additions: + the slot-specific rule (soft-skills-tail, redundancy-avoidance)

The deployment layer (`pdf-filename-conventions.md`) is independent — it's loaded only when the conversation shifts from "draft the summary" to "deploy the CV".

## When to Override the Recipe

The recipe is canonical, but it is not a sacred text. Override is warranted when:

- The candidate's level/role makes the slot template a poor fit (e.g., a Founder/CEO summary may need a different opener tier)
- The target document type is genuinely different (e.g., a Twitter bio at 160 chars cannot fit the full recipe — strip-down applies)
- The candidate explicitly wants a different voice (after seeing recipe-compliant drafts and rejecting the recipe itself)

In all override cases, document the deviation in the candidate's session file with the reasoning. Future drafts for that candidate should follow the deviation, not snap back to the canonical recipe.
