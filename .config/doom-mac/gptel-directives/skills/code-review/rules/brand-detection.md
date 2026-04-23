---
title: Brand Detection — Auto-Identify Which Brand Rule to Apply Before Review
impact: CRITICAL
impactDescription: Brand-scoped review rules (e.g., OBS FPS budget, per-frame paint discipline) only apply inside their brand's repos. Without an up-front detection step the skill either ignores brand-specific constraints (missing real violations) or misapplies them to unrelated repos (flagging false positives). Detection is always step zero of any review.
tags: code-review, brand, detection, routing, auto-detect, remote, git, owner, organization, kyonax, madison-reed, reckit, obs, browser-source, generic, fallback
---

This rule describes how to identify which brand rule (`brand-<name>.md`) applies to the code under review. Always run brand detection **before** loading any domain or category rules — so the Worker loads the right opinionated review discipline for this repo, not a mismatched one.

Brand rules differ from domain rules:

- **Domain rule** (e.g., `mr-review-checklist.md`) is selected by **tech stack** — Vue+Pug+Stylus, Express+Node, etc. — and covers language/framework conventions.
- **Brand rule** (e.g., `brand-kyonax.md`) is selected by **repo ownership** — and covers the brand's opinionated review discipline (perf budget, architectural constraints, severity calibration) that applies across every tech stack in that brand's codebases.

Both can load for the same review. Brand detection runs first; tech-stack domain detection runs second; both sets of rules are consulted during analysis.

## Detection signals (checked in order)

Check these signals in the order below. The first match wins.

### Signal 1: git remote origin URL

Run or read the origin remote:

```bash
git remote get-url origin
```

Parse the owner / organization segment and match against the brand catalog:

| Remote URL pattern | Brand rule to load | Notes |
|---|---|---|
| Any `Kyonax/*` repo (e.g. `github.com/Kyonax/reckit`, `Kyonax/kyo-*`) | `brand-kyonax.md` | Solo / personal org. RECKIT + OBS overlay toolkits + any future Kyonax codebases. |
| Any `MadisonReed/*` repo | (no brand rule) → continue to tech-stack domain detection | Detection-by-code-path (`website/src/vuescripts/`, `mr_modules/`) and `mr-review-checklist.md` cover MR work. MR has no brand-scoped review rule at this time. |
| Anything else | `brand-generic-fallback` (see below) | Apply only universal categories + any tech-stack domain rule that matches. |

### Signal 2: explicit user override

If the user explicitly names a brand in the prompt (e.g. *"review this as a Kyonax OBS overlay"* or *"apply the RECKIT performance rules"*), that overrides remote detection. Honor it even when the remote points elsewhere. Common overrides:

| User phrase | Load |
|---|---|
| "Kyonax", "RECKIT", "OBS overlay", "OBS browser source", "HUD perf" | `brand-kyonax.md` |
| "generic", "skip brand rules", "universal only" | `brand-generic-fallback` |

### Signal 3: repo-local indicator files

When the remote is ambiguous (detached HEAD, no remote, fork, or multi-remote setup), fall back to repo-local indicators:

| File / pattern | Suggests brand |
|---|---|
| `@<brand>/` folders at project root (e.g., `@kyonax_on_tech/`, `@<any-handle>/`) | Kyonax |
| `brand.js` + `sources.js` with `type: 'hud' \| 'animation' \| 'scene'` | Kyonax |
| `src/shared/composables/use-obs-websocket.js` or any `use-*` file referencing OBS | Kyonax |
| `CHANGELOG.org` at repo root with `#+FILETAGS:` metadata + Tier 1 figlet header | Kyonax |
| `.github/PULL_REQUEST_TEMPLATE.md` contains `contains testing instructions` + `requires a lambda deployment` | MadisonReed (no brand rule — use tech-stack detection) |
| Any path matching `website/src/vuescripts/` or `mr_modules/` | MadisonReed (no brand rule — use tech-stack detection) |

If multiple indicators conflict, prefer the remote URL signal. If multiple match within a single brand catalog (e.g., `@brand/` folders + Tier 1 headers both indicate Kyonax), that's reinforcement — proceed with high confidence.

## Fallback when no brand matches

When none of the above signals match, use **generic fallback**:

- No brand rule loaded. Skip to tech-stack domain detection (existing SKILL.md Stage 1 flow).
- Apply only **universal review categories** (Architecture, Performance, Testability, Accessibility, Code Style).
- Do **not** invent brand-specific constraints. Severity calibration follows the universal table from SKILL.md.
- Announce the fallback to the user before running the review: *"No brand detected — applying universal categories only. Specify a brand if you want its opinionated discipline."*

This fallback is deliberately conservative: it produces correct-but-generic findings for any repo until a `brand-<name>.md` is added.

## Precedence: brand × domain × category

Brand rules and domain rules are **both** consultable — they do not override each other. They address orthogonal axes:

| Axis | Source | Example |
|---|---|---|
| Brand constraints | `brand-<name>.md` | "OBS HUD must hold 60 fps on low-core hardware" |
| Tech-stack conventions | Domain rule (e.g., `mr-review-checklist.md`) | "Pug tag `template(v-slot:...)` must use kebab-case slot names" |
| Universal categories | SKILL.md Stage 2 | "No unnecessary re-computation in watchers" |

A single finding can cite rules from any combination. When listing findings, cite the most specific rule that applies.

## How to adopt a new brand

When a new org/repo pattern appears that warrants its own review discipline:

1. Capture 3-5 representative reviews or known-good/known-bad files from the brand's codebase.
2. Extract the specifics:
   - What performance/architectural constraints are non-negotiable?
   - What are the brand's severity calibrations (is "magic number" HIGH or LOW here)?
   - What tech-stack conventions are brand-specific (vs shared with the tech-stack domain rule)?
   - What category of bugs has historically burned this brand?
3. Write a new `rules/brand-<name>.md` following the shape of `brand-kyonax.md`.
4. Update this file's Signal 1 catalog with the new brand's remote URL pattern + indicator files.
5. Update `SKILL.md` routing table and Quick Reference so the analyzer can route to the new brand rule.

## Output of the detection step

After running brand detection, record the decision in one line at the top of the review's internal working state (not output to the user unless fallback was chosen):

```
<detected-brand: kyonax | generic-fallback | madison-reed-no-brand-rule>
```

Then load the matching `brand-<name>.md` (if any) **alongside** the tech-stack domain rule(s) identified in SKILL.md Stage 1.

## Correct vs incorrect detection

### Example 1: Ignoring remote URL signal on a Kyonax repo

**Incorrect** — running a code review without loading `brand-kyonax.md`:
```
User: "review this cam-log.vue for issues"
Assistant: <inspects the file; reports generic Vue smells; misses that the author wrote `filter: drop-shadow(...)` inside @keyframes — a Kyonax FPS-killer>
```

**Correct** — detect Kyonax first, then review:
```
User: "review this cam-log.vue for issues"
Assistant: <runs `git remote get-url origin`; sees Kyonax/reckit; loads brand-kyonax.md; flags the animated `filter` as CRITICAL per the brand's §C.A3 "animate only transform/opacity" rule>
```

### Example 2: User override honored

**Incorrect** — applying detected brand when the user explicitly overrode:
```
User: "review this as a generic component, ignore the RECKIT rules"
Assistant: <sees Kyonax remote; loads brand-kyonax.md anyway; flags OBS-WS singleton violations irrelevant to the user's intent>
```

**Correct** — honor the user override:
```
User: "review this as a generic component, ignore the RECKIT rules"
Assistant: <skips brand-kyonax.md; uses universal categories + any tech-stack rule; notes "Brand rules skipped per user request">
```

### Example 3: No remote, fall through to local indicators

**Incorrect** — assuming generic fallback because the remote is missing:
```
User: "review this HUD" (repo was just `git init`'d, no remote set yet)
Assistant: <applies generic fallback; misses the `@kyonax_on_tech/` folder at repo root signaling Kyonax>
```

**Correct** — check local indicators when the remote is absent:
```
User: "review this HUD"
Assistant: <no remote; scans for indicator files; finds `@kyonax_on_tech/sources/hud/cam-log.vue`; loads brand-kyonax.md>
```

### Example 4: MadisonReed repo — no brand rule, tech-stack only

**Incorrect** — fabricating a MadisonReed brand rule that doesn't exist:
```
User: "review this Vue component in mr/website"
Assistant: <hallucinates brand-madison-reed.md content; produces rules not grounded in any file>
```

**Correct** — MR has no brand rule in this skill; proceed with tech-stack detection:
```
User: "review this Vue component in mr/website"
Assistant: <detects MadisonReed remote; no brand rule to load; proceeds to SKILL.md Stage 1 tech-stack detection; loads mr-review-checklist.md via the domain signal>
```
