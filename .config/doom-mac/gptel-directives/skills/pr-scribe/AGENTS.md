# pr-scribe — Architectural Guide

This document explains the *why* behind the skill's structure. The `rules/*.md` files are the authoritative source of truth for the Worker; this file is optional background reading that helps maintainers understand how the pieces fit together and why brand-awareness is the central design choice.

## The central design choice: brand-awareness

Different GitHub organizations have different PR conventions **and** different data-sourcing habits. A single "universal PR template" is a fiction — real teams at real companies have real preferences on both axes.

### The format axis (what the output looks like)

- **Madison Reed's Dotcom team** uses a specific 6-item checklist, writes a flat `**Changes:**` bullet list with per-component sub-lists, free-form Technical Details subsections (including SDK-limitation callouts), a single-table Unit Testing Coverage, and the heading `## Instructions on how QA can test this PR`.

- **Kyonax's personal projects** use a 13-item checklist with absolute URLs to licensing docs and a CI-label condition, write themed `### Implementation / Release / CI & Tooling / Dependencies / Docs` subsections with a closed `[NEW] / [MOD] / [DEL] / [MOV]` tag vocabulary, four-field decision bullets (Chose / Over / Why / Trade-off), a two-table Testing Coverage (Automated tests + Quality gates), and the heading `## How to test this PR`.

### The data-source axis (where the content comes from)

- **Madison Reed's PRs are authored from a roam org-node** — a `.org` file per ticket with a fixed set of fields (`#+TITLE:`, `#+SOURCE_URL:`, `** STRUCTURE AND FUNCTIONALITY`, `** UNIT TEST COVERAGE`, `** QA INSTRUCTIONs`, `** DEPLOYMENT NOTEs`, etc.). The skill's job is to read that org-node and map its fields onto PR slots using a defined extraction table. The user pastes the org file (or points the skill at it); the skill does NOT fabricate content from git state alone.

- **Kyonax's PRs are authored from the git state of the feature branch plus the user's conversational description**. There is no external ticketing, no org-node, no Figma link (usually). The branch name, commits ahead of base, diff, and the user's narrative in the prompt are the primary sources; `CHANGELOG.org` + `README.org` + `package.json` + `.github/workflows/ci.yml` provide repo cross-references. Local session / clipboard / credential files can inform the Worker but must never be referenced in the output.

### Why both axes matter together

Imposing one brand's format on the other produces PRs that fail local review. Imposing one brand's data-sourcing on the other produces PRs that are either hallucinated (e.g. "Jira URL" on a Kyonax PR) or that miss their actual content source (e.g. drafting an MR PR from git alone when the org-node was the canonical source).

The skill therefore has **four axes of behavior**:

1. **Universal format** — section ordering, writing disciplines (no emojis / arrows / private refs / relative URLs / redundancy), checkbox scope, colon-label typography, self-documenting template invariants.
2. **Brand layout** — exact checklist items, Changes pattern (A vs B), Technical Details shape, Testing variant, QA heading wording + step format, Special Deployment vocabulary, Documentation vocabulary, reference-link footer on/off, required labels. This is the "what shape does the section take" axis.
3. **Content richness** — the detail level that fills each structure: status tags on Changes entries, individual test case rows vs grouped summaries, detailed vs brief expected labels, thorough vs concise Technical Details. This is the "how detailed is each entry" axis. It is **independent of brand layout** — both a flat `**Changes:**` list (Pattern A) and themed subsections (Pattern B) can use `[NEW]/[MOD]/[DEL]` tags. Both `TEST-SINGLE` and `TEST-TWO-TABLE` can list individual test cases. Content richness defaults to RICH for all brands.
4. **Brand data-sourcing conventions** — where PR content comes from for this brand, how to extract / parse it, which fields map to which PR slots, what is NOT available for this brand, and what must never be referenced in output.

The universal axis lives in `global-writing-rules.md`, `pr-body-structure.md`, `changes-list.md` (pattern catalog), and `supporting-sections.md` (variant catalog). The content richness axis lives in `content-richness.md` and is referenced by all brand rules. The brand-specific axes (both layout overrides AND data-sourcing conventions) live together in each `brand-<name>.md` file — a single brand rule answers "what does the PR look like?", "how detailed are the entries?", and "where do I get the content from?".

## How routing works

Every PR task starts with brand detection:

```
User prompt
    │
    ▼
Read brand-detection.md
    │
    ├─ git remote origin URL ──▶ MadisonReed/* ─▶ load brand-madison-reed.md
    │                      │
    │                      └──▶ Kyonax/*    ──▶ load brand-kyonax.md
    │
    ├─ Explicit user override (honored regardless of remote)
    │
    └─ No match ─────────▶ Apply generic-fallback defaults (Pattern A,
                              TD-FREEFORM, TEST-SINGLE, QA-INSTRUCTIONS,
                              DEPLOY-FREEFORM, DOC-OPEN)
    │
    ▼
Load pr-body-structure.md + changes-list.md + supporting-sections.md +
     content-richness.md + global-writing-rules.md alongside the brand rule
    │
    ▼
Draft the PR body
```

The brand rule tells the Worker which Pattern (A or B) and which Variant (per section) to pick from the catalogs. The catalogs in `changes-list.md` and `supporting-sections.md` are the menu; the brand rule is the order.

## Why this split over alternatives

### Why not one monolithic brand rule per brand that inlines everything?

Each brand file would be ~800 lines of format specification. Maintenance would duplicate the section variants across files — when we discover a new variant (e.g. a third Special Deployment format), we'd have to update every brand file that uses it. The catalog-plus-selection split means variants are defined once and referenced by name from each brand rule.

### Why not one universal template with brand toggles (e.g. `{{ checklist_items }}`)?

Template languages don't capture the *semantic* differences between brands cleanly. The checklist isn't a variable — it's a set of conventions with specific wording that reviewers match against. Describing each brand's conventions in plain prose, with its own examples and correct/incorrect pairs, is more faithful to how humans read and apply the rules.

### Why not just tell the Worker "look at recent PRs in this repo"?

Recent PRs may themselves be wrong. An explicit brand rule captures *what the convention SHOULD be*, validated against multiple examples by someone who knows the team's expectations. The brand rule is the source of truth; the examples in recent PRs are evidence of conformance.

## The three scoping axes

Every format decision in the skill lives at exactly one of these scopes:

| Scope | Example decision | Where it lives |
|---|---|---|
| Universal | No emojis anywhere except Testing status cells | `global-writing-rules.md` |
| Universal | `## Checklist` always first, `## Documentation` always last | `pr-body-structure.md` |
| Catalog | Pattern A is flat `**Changes:**`; Pattern B is themed subsections | `changes-list.md` |
| Catalog | `TD-4FIELD` requires Chose / Over / Why / Trade-off sub-fields | `supporting-sections.md` |
| Brand | Madison Reed picks Pattern A; Kyonax picks Pattern B | `brand-madison-reed.md` / `brand-kyonax.md` |
| Brand | Madison Reed uses the 6-item checklist; Kyonax uses the 13-item checklist | Same |

If a decision could be in two scopes, it belongs in the more specific one. The catalog describes **options**; the brand rule describes **choices**.

## The cost of over-loading rules

Each rule file is ~200-400 lines of domain content. The routing table in `SKILL.md` is tuned so that any given task loads only the files it needs:

| Task | Rules loaded | Approximate total |
|---|---|---|
| Full PR body on MR repo | `brand-detection` + `brand-madison-reed` + `pr-body-structure` + `changes-list` + `supporting-sections` + `global-writing-rules` | ~1700 lines |
| Full PR body on Kyonax repo | `brand-detection` + `brand-kyonax` + `pr-body-structure` + `changes-list` + `supporting-sections` + `global-writing-rules` | ~1800 lines |
| Only the Changes block on MR repo | `brand-detection` + `brand-madison-reed` + `changes-list` + `global-writing-rules` | ~900 lines |
| Only Documentation on Kyonax repo | `brand-detection` + `brand-kyonax` + `supporting-sections` + `global-writing-rules` | ~900 lines |
| Validating an existing PR body | `brand-detection` + matching brand rule + `global-writing-rules` + whichever section-specific rule(s) the issue touches | ~700-1200 lines |
| Onboarding a new brand | `brand-detection` + `pr-body-structure` + `changes-list` + `supporting-sections` | ~1200 lines (no brand rules loaded until you write the new one) |

The brand-detection file is small (~150 lines) and loaded every task; this cost is unavoidable because detection happens first. Everything else is selective.

## Why PR-only, not commit + CHANGELOG + PR

This skill was earlier scoped to cover commits and CHANGELOG entries as well. Feedback pulled the scope back to PR bodies only because:

1. **PRs are review artifacts read by other humans**; commits and CHANGELOGs are often read by tooling (release-note generators, bisect, etc.). The audience and constraints differ.
2. **Commit and CHANGELOG conventions vary even more wildly across brands** than PR conventions do — shoving them into the same skill would dilute the PR-body focus or require 4-6 more brand rules per brand.
3. **The PR body is the single highest-leverage artifact** — it's what reviewers spend the most time on, what merges are gated against, and what signals code-review quality.

A separate skill (or skill per artifact) can handle commit messages, release notes, or CHANGELOG entries. This skill stays laser-focused on PR description text.

## Adopting a new brand

Steps for extending the skill to a new GitHub organization:

1. **Gather ground truth.** Use `gh pr view <N> --repo <org>/<repo>` on 3-5 representative merged PRs authored by someone who knows the brand's conventions. Note: title format, exact checklist items, top-of-body conventions (URLs, references), Changes format, Technical Details shape, Testing coverage layout, QA heading wording + step format, Special Deployment specifics, Documentation heading vocabulary, reference-link footer presence, required labels.
2. **Pick variants from the catalogs.** For each slot (Changes pattern, Technical Details variant, Testing variant, QA variant, Deployment variant, Documentation variant), pick one from `changes-list.md` / `supporting-sections.md`. Invent a new variant only when the brand uses something genuinely different from the catalog — and add it to the catalog file.
3. **Write `rules/brand-<name>.md`.** Follow the shape of the existing brand files. Include: title format, checklist (verbatim items), slot-by-slot variant selection, worked examples, correct-vs-incorrect pairs specific to the brand's common mistakes.
4. **Update `brand-detection.md`.** Add the remote URL pattern + indicator files for the new brand.
5. **Update `SKILL.md`.** Add a routing-table row for the new brand and a Quick Reference entry.

## Where this skill does and doesn't help

| Task | Does the skill help? |
|---|---|
| Drafting a PR body for MadisonReed / Kyonax / any repo | Yes — the core use case |
| Maintaining a PR template | Yes |
| Validating existing PR text against a brand's conventions | Yes |
| Adopting a new brand by capturing conventions | Yes — follow the onboarding procedure above |
| Drafting a commit message | No — out of scope |
| Writing a CHANGELOG entry | No — out of scope |
| Running `git commit` / `gh pr create` | No — the skill produces text; the human runs git |
| Deciding whether to open a PR | No — scope / when-to-ship is outside the skill |
| Branch model / protection setup | No — governance lives in the repo's own docs |

The skill is strictly about **structuring PR description text for brand-appropriate review**, not about engineering process. A team that uses this skill might also have a branch-protection policy, a CODEOWNERS file, a release calendar, or a ticketing system — those are orthogonal. The skill assumes those exist and integrates with them (via the brand rule's references), but doesn't manage them.
