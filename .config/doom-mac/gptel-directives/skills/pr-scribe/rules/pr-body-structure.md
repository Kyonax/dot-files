---
title: Pull Request Body Skeleton — Universal Section Ordering with Brand-Configurable Slots
impact: CRITICAL
impactDescription: Defines the universal top-level structure of a PR body and names which parts are brand-configurable versus fixed. Without this rule, PR bodies reorder sections, skip invariant blocks, or misidentify which parts vary between brand conventions.
tags: pr, pull-request, body, structure, skeleton, sections, ordering, template, brand-configurable, slots, checklist, summary, user-story, design-link, pull_request_template, self-documenting
---

This rule defines the universal top-level skeleton every PR body produced by this skill follows, and names which slots are brand-configurable. The specifics of each slot (exact checklist items, Changes format, QA heading wording, Technical Details shape, Documentation vocabulary, etc.) live in the relevant brand rule (`brand-<name>.md`). Load this file alongside the brand rule whenever drafting a full PR body.

## Universal top-level skeleton

Every PR body uses these top-level anchors in this order. The exact headings, wording, and per-section inner format are brand-configurable (see below). What is **universal** is the vertical ordering:

```
[0]  PR Title (always generated, not inside the body — shown separately)
[1]  [optional] Top line (e.g. bare ticket URL)
[2]  ## Checklist
[3]  ## What does this PR do?
       [3a]  Summary paragraph
       [3b]  User-story or context blockquote
       [3c]  Design / Reference link (optional)
       [3d]  Changes block
       [3e]  Technical Details (optional)
       [3f]  Testing / Unit Testing Coverage (optional)
[4]  ## How to test this PR  /  Instructions on how QA can test this PR
[5]  ## Special Deployment Requirements (optional)
[6]  ## Documentation
[7]  [optional] Reference-link footer
```

**Vertical order is fixed across brands.** Reviewers scan top-to-bottom; moving a section changes their mental flow. What varies is *how each slot is filled* — not *where it sits*.

## Per-slot universal rules

### Slot [0] — PR Title (always generated)

Every PR output includes a title above the body. The title is NOT part of the body text — it is a separate field. The skill always generates it and presents it clearly before the body.

**Universal rules:**
- Always output the title on its own line, prefixed with `**PR Title:**` so it's easy to copy.
- The title format is brand-configurable (ticket prefix, scope, casing, max length).
- Max ~70 characters. Use the body for details, not the title.

| Brand | Title pattern | Example |
|---|---|---|
| Madison Reed | `[DOTCOMPB-XXXX]: <Subtitle>` | `[DOTCOMPB-7942]: Add Google Sign-On into the New Booking Flow` |
| Kyonax (feature) | `feat(<scope>): <lowercase summary>` | `feat(widgets): extract audio meter + readout` |
| Kyonax (release) | `[vX.Y]: <Short Release Title>` | `[v0.3]: Vue App + CAM-LOG Overlay` |
| Generic fallback | `<imperative summary>` | `Add user authentication to checkout flow` |

### Slot [1] — Top line (optional)

Some brands open the PR body with a single line *before* the Checklist — typically a bare ticket URL or a prominent issue reference. Whether to include this and what to put in it is brand-configurable.

| Brand convention | Example |
|---|---|
| None | Start directly with `## Checklist` |
| Bare ticket URL | `https://jira.example.com/browse/TICKET-1234` on its own line |
| Issue reference | `Closes #123` or `Fixes #456` |

### Slot [2] — `## Checklist`

Every PR body includes a top Checklist as its first top-level section. The *items* inside the checklist are brand-configurable. Universal rules:

- Every item is a `- [ ]` or `- [x]` checkbox (checkboxes are allowed here — this is the one section where they fit).
- Items cover self-review categories: testing intent, environment impact, security / licensing, code quality, release metadata (if applicable).
- Items are never deleted between PRs of the same brand — the fixed list lets reviewers scan for gaps.
- Item wording is verbatim from the brand rule. Never paraphrase.

### Slot [3] — `## What does this PR do?`

Universal structure of this section:

| Slot | Required? | Content |
|---|---|---|
| [3a] Summary paragraph | Yes | 1-3 sentences, present tense, narrative. References the tracking ticket / issue if one exists. |
| [3b] User-story or context blockquote | Yes | `>` blockquote. Either a user story (`As a X, I want Y, so that Z.`) or the bug summary / impact statement. |
| [3c] Design / Reference link | When applicable | Bold label (e.g. `**Design:** [Figma](url)`) on its own line. Label wording and format are brand-configurable. |
| [3d] Changes block | Yes | Authoritative file inventory. Format is brand-configurable — see `changes-list.md`. |
| [3e] Technical Details | Optional | Decisions / trade-offs / SDK limitations. Format is brand-configurable — see `supporting-sections.md`. |
| [3f] Testing coverage | Usually yes | Test inventory and/or CI-gate table. Format is brand-configurable — see `supporting-sections.md`. |

### Slot [4] — QA / How-to-test section

A top-level section giving reviewers / QA reproducible steps. The heading text is brand-configurable (`## How to test this PR` vs `## Instructions on how QA can test this PR`). Format of the steps is also brand-configurable — see `supporting-sections.md`.

### Slot [5] — `## Special Deployment Requirements` (optional)

Omit entirely when the PR has no preconditions. When present, format is brand-configurable (free-form prose with config blocks vs numbered severity list) — see `supporting-sections.md`.

### Slot [6] — `## Documentation`

Always present. Format is brand-configurable (open-ended vs closed MEDIA-TYPE vocabulary) — see `supporting-sections.md`. Placeholder is acceptable when no media is attached yet.

### Slot [7] — Reference-link footer (optional)

Some brands close the PR body with a reference-link footer — `[TICKET-ID]: URL` entries that the body's inline markdown links resolve to. Whether to include this is brand-configurable.

## What is universal vs brand-configurable

| Element | Universal | Brand-configurable |
|---|---|---|
| Vertical section order | ✓ | |
| Presence of Checklist at the top | ✓ | |
| Exact checklist items | | ✓ (brand rule lists them verbatim) |
| Summary + user-story + Design shape | ✓ | |
| Exact wording / format of Design link | | ✓ |
| Changes block as authoritative inventory | ✓ | |
| Changes format (flat with tickets vs themed with tags) | | ✓ |
| Technical Details / Testing / QA / Deployment / Docs sections presence | ✓ | |
| Technical Details inner format | | ✓ |
| Testing Coverage inner format | | ✓ |
| QA heading wording + step format | | ✓ |
| Deployment severity vocabulary | | ✓ (present or absent) |
| Documentation heading vocabulary | | ✓ |
| Reference-link footer | | ✓ (on or off) |
| Top-of-body ticket URL line | | ✓ (on or off) |
| Cross-cutting writing rules (no emojis, no arrows, etc.) | ✓ | |

## When to omit a slot

| Slot | Omit when |
|---|---|
| [1] Top line | Brand rule doesn't require it |
| [2] Checklist | Never — always include |
| [3a] Summary | Never |
| [3b] User story / context | Never (for bug PRs, use a context blockquote instead of a user story) |
| [3c] Design link | No external design reference exists |
| [3d] Changes block | Never |
| [3e] Technical Details | No non-obvious decisions were made |
| [3f] Testing Coverage | PR is purely docs / CI config AND touches no code paths that tests exercise |
| [4] QA / How-to-test | Never — every PR describes how to verify |
| [5] Special Deployment | No ops preconditions at all (omit the whole heading — never leave "N/A") |
| [6] Documentation | Never — include even with a placeholder |
| [7] Reference-link footer | Brand rule doesn't require it or no references to footnote |

## Maintaining a `.github/PULL_REQUEST_TEMPLATE.md`

When the skill maintains a repo's PR template file, these invariants apply:

### Top-of-file rule block (optional but recommended)

A single HTML comment at the very top enumerating every enforcement rule this brand applies. Invisible in the rendered PR body but present in the Markdown source, so every PR populated from the template carries the rules with it.

```markdown
<!--
  ─────────────────────────────────────────────────────────────
   <Repo Name> Pull Request Template
  ─────────────────────────────────────────────────────────────
   <N> rules this template enforces:
   1. <rule summary>
   2. <rule summary>
   …
  ─────────────────────────────────────────────────────────────
-->
```

### Per-section inline guide comments

Every `##` and `###` section carries an inline `<!-- … -->` comment directly under its heading stating:
1. The section's scope.
2. The required inner format (tables, bullet shape, labels).
3. Any closed vocabulary (e.g. MEDIA-TYPE for Documentation).

These invisible comments survive in the Markdown source of every populated PR description, so reviewers working in the raw editor see the rules in-place.

### Self-documenting discipline

- When a brand rule is added / removed, update the rule-count header at the top of the template file.
- When "simplifying" the template, do NOT strip the inline comment blocks. They are load-bearing — simplification requests apply to the PR *content*, not the guidance scaffolding.
- Link targets from the checklist use absolute URLs pinned to the default branch (per `global-writing-rules.md` rule 4).

## Validation checklist before returning a PR body

- [ ] All universal slots in fixed order (Top line if applicable, Checklist, What does this PR do?, QA/How-to-test, Special Deployment, Documentation, Reference footer if applicable)
- [ ] Checklist items match the brand rule verbatim
- [ ] Summary paragraph names zero files and does not restate the Changes list
- [ ] User-story / context blockquote present
- [ ] Changes block uses the format specified by the brand rule (Pattern A or Pattern B — see `changes-list.md`)
- [ ] Technical Details / Testing / QA / Deployment / Documentation use the variants specified by the brand rule (see `supporting-sections.md`)
- [ ] All 7 cross-cutting rules from `global-writing-rules.md` respected

## Correct vs incorrect examples

### Example 1: Wrong section order

**Incorrect** — Technical Details before the Changes block:
```markdown
## What does this PR do?

Summary paragraph.

> User story blockquote.

### Technical Details
- Decision A

**Changes:**
- **`foo.vue`**: …
```

**Correct** — Changes block first, Technical Details after:
```markdown
## What does this PR do?

Summary paragraph.

> User story blockquote.

**Changes:**
- **`foo.vue`**: …

### Technical Details
- Decision A
```

### Example 2: Missing Checklist

**Incorrect** — PR opens with a summary:
```markdown
## What does this PR do?
Adds a new widget.
```

**Correct** — Checklist is the first top-level section (brand's exact items):
```markdown
## Checklist for PR Author (Check if it applies)
- [x] contains testing instructions
- [ ] requires a lambda deployment …

## What does this PR do?
…
```

### Example 3: Empty Documentation section dropped

**Incorrect** — Documentation omitted entirely because no media attached:
```markdown
## Special Deployment Requirements
…

<!-- body ends here, no Documentation section -->
```

**Correct** — Documentation always present, with a placeholder when no media exists:
```markdown
## Special Deployment Requirements
…

## Documentation
<!-- Add screenshots here -->
```
