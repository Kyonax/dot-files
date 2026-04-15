---
title: Brand Detection — Auto-Identify Which Brand Rule to Apply from Repo Context
impact: CRITICAL
impactDescription: The skill produces different PR formats for different brands. Without an automatic detection step, the skill may apply the wrong brand's format (wrong checklist, wrong Changes shape, wrong QA heading, wrong vocabulary) — producing PRs that fail the target brand's conventions.
tags: pr, brand, detection, routing, auto-detect, remote, git, owner, organization, madison-reed, kyonax, generic, fallback, ground-truth
---

This rule describes how to identify which brand rule (`brand-<name>.md`) applies to the current repo. Always run brand detection **first** — before loading any format-specific rule — so the skill knows which Changes pattern, which checklist items, which section variants, **and which data sources** to read from.

Each brand rule carries two kinds of spec:

1. **Format overrides** — which Changes pattern, which Technical Details variant, which Testing Coverage layout, etc. (the "what the output looks like" axis).
2. **Data-source conventions** — where the PR content comes from for this brand (roam org-nodes, git diff, user-provided narrative, external tickets, etc.) and the extraction rules that map those sources onto PR slots.

Load the matching brand rule as the Worker's first read after detection. It tells the Worker both *what shape the output takes* and *what to read to fill the shape*.

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
| `github.com/MadisonReed/mr` (or any `MadisonReed/*` repo) | `brand-madison-reed.md` | Single org, single monorepo `mr`. All work lives there. |
| `github.com/Kyonax/reckit` (and any `Kyonax/*` repo) | `brand-kyonax.md` | Personal / solo project org. |
| Anything else | `brand-generic-fallback` (see below) | Use the universal skeleton + no brand-specific overrides |

### Signal 2: explicit user override

If the user explicitly names a brand in the prompt (e.g. *"draft this as a Madison Reed PR"* or *"use the Kyonax conventions"*), that overrides the remote detection. Honor it even when the remote points elsewhere.

### Signal 3: repo-level indicator files

When the remote is ambiguous or points to a fork, fall back to repo-local indicators:

| File / pattern | Suggests brand |
|---|---|
| `CHANGELOG.org` at repo root + `#+FILETAGS:` metadata | Kyonax |
| `.github/PULL_REQUEST_TEMPLATE.md` contains `contains testing instructions` + `requires a lambda deployment` | Madison Reed |
| `.github/PULL_REQUEST_TEMPLATE.md` contains a top `<!--` rule block enumerating numbered enforcement rules | Kyonax |
| Any path matching `website/src/vuescripts/` | Madison Reed |
| Any path matching `src/brands/<handle>/*.vue` | Kyonax |

If multiple indicators conflict, prefer the remote URL signal.

## Fallback when no brand matches

When the repo does not match any known brand catalog, fall back to **generic defaults**:

- Changes format: **Pattern A** from `changes-list.md` (flat `**Changes:**` list with optional ticket references). Safe for any repo.
- Technical Details: **TD-FREEFORM** variant — custom `###` subheadings with prose + bullets.
- Testing Coverage: **TEST-SINGLE** variant — single table if tests exist, short paragraph if not.
- QA heading: `## How to test this PR` with numbered-list steps.
- Expected label: `- **Expected:**` (inline bold) as a nested sub-bullet under each step.
- Special Deployment: **DEPLOY-FREEFORM** variant. Omit when no preconditions.
- Documentation: **DOC-OPEN** variant. Placeholder comment when no assets.
- Reference-link footer: off.
- Top-of-body ticket URL line: off unless the user pastes an issue / ticket URL.
- Checklist items: the smallest universal set — testing instructions, github checks, any repo-specific items the user dictates.

This fallback is deliberately conservative: it uses the least opinionated option in every slot so the output works for any repo until a proper `brand-<name>.md` is written.

## How to adopt a new brand

When a repo / organization's PR conventions differ enough from the existing brand rules that fallback defaults aren't a good fit:

1. Capture 3-5 representative PRs authored by someone who knows the brand's conventions (via `gh pr view <N> --repo <org>/<repo>`).
2. Extract the specifics:
   - Title format (with or without ticket prefix)
   - Exact checklist items (verbatim wording)
   - Top-of-body convention (bare ticket URL? issue reference? nothing?)
   - Design / Reference link format
   - Changes format (flat with tickets vs themed with tags)
   - Technical Details variant
   - Testing Coverage variant
   - QA heading wording + step format + Expected label style
   - Special Deployment variant (free-form vs severity)
   - Documentation variant (open vs MEDIA-TYPE vocab)
   - Reference-link footer (yes/no)
   - Required labels (if any)
3. Write a new `rules/brand-<name>.md` following the shape used by `brand-madison-reed.md` and `brand-kyonax.md`.
4. Update this file's detection table with the new brand's remote URL pattern + indicator files.
5. Update `SKILL.md` routing table and Quick Reference so the analyzer can route to the new brand rule.

## Output of the detection step

After running brand detection, record the decision in one line at the top of whatever draft is being composed (internal working state — not output to the user):

```
<detected-brand: madison-reed | kyonax | generic-fallback>
```

Then load the matching `brand-<name>.md` alongside the section-specific rules (`changes-list.md`, `supporting-sections.md`, `global-writing-rules.md`) that the task requires.

## Correct vs incorrect detection

### Example 1: Ignoring remote URL signal

**Incorrect** — defaulting to one brand regardless of repo:
```
User: "draft a PR body for this repo"
Assistant: <applies Kyonax conventions to a MadisonReed/mr repo>
```

**Correct** — check the remote first:
```
User: "draft a PR body for this repo"
Assistant: <runs `git remote get-url origin`; sees MadisonReed/mr; loads brand-madison-reed.md>
```

### Example 2: User override ignored

**Incorrect** — applying detected brand when the user explicitly named a different one:
```
User: "draft this as a Madison Reed PR — this repo is just a fork I use for testing"
Assistant: <sees the fork's remote is Kyonax/mr-fork; applies Kyonax conventions>
```

**Correct** — honor the user override:
```
User: "draft this as a Madison Reed PR — this repo is just a fork I use for testing"
Assistant: <ignores remote; loads brand-madison-reed.md per user instruction>
```

### Example 3: Unknown repo, fallback misapplied

**Incorrect** — guessing a brand when no signal matches:
```
User: "draft a PR body" (repo is a brand-new personal project with no remote yet)
Assistant: <applies Kyonax conventions because of a vague similarity>
```

**Correct** — apply generic fallback and say what was chosen:
```
User: "draft a PR body" (repo is a brand-new personal project with no remote yet)
Assistant: <uses generic-fallback defaults; notes "Using generic fallback — no brand detected. Specify a brand to apply its conventions.">
```
