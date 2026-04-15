---
title: Supporting Sections — Variant Catalog for Technical Details, Testing, QA, Deployment, Documentation
impact: CRITICAL
impactDescription: These five supporting sections carry the "why" and "how" of every PR. Each has two or more real variants used by different brands. The brand rule (`brand-<name>.md`) picks one variant per section; this file is the catalog the brand rule points at.
tags: pr, technical-details, testing-coverage, qa, how-to-test, deployment, documentation, variants, brand-switching, chose-over-why-trade-off, expected, severity, media-type
---

This rule catalogues the variants of five supporting PR sections. Every brand rule picks one variant per section from this catalog — it never invents a new one. Load this file alongside the brand rule whenever drafting any of these five sections.

## `### Technical Details`

### Variant TD-FREEFORM

Open `###` subsections with free-form prose describing architectural trade-offs, SDK limitations, or non-obvious decisions. Each subsection has a short descriptive heading plus 1-3 bullets summarizing the decision + rationale.

**Template:**

```markdown
### <Custom subheading — a specific decision or limitation>

Short prose or bullet list summarizing the decision and rationale.

- Bullet 1
- Bullet 2
```

**Used by:** brands that treat Technical Details as a discussion space (architectural flags, SDK limitation callouts, future-work notes). Pairs with Pattern A Changes list.

### Variant TD-4FIELD

One bullet per decision, each with exactly four required sub-fields (Chose / Over / Why / Trade-off).

**Template:**

```markdown
### Technical Details

- **<Decision title>**
  - ***Chose:*** <what we decided>
  - ***Over:*** <alternative rejected>
  - ***Why:*** <rationale>
  - ***Trade-off:*** <cost accepted>
```

**Rules:**

- Each top-level bullet = one decision. No prose between bullets.
- All four sub-fields mandatory. Use `—` (em dash) for N/A fields; never drop the sub-bullet.
- Each sub-field is one line. If it needs more than one line, split into two decision bullets.
- Sub-field labels are bold-italic (`***Chose:***` — three asterisks each side).

**Used by:** brands that audit architectural decisions retroactively and want the alternative-considered column visible.

## `### Testing / Unit Testing Coverage`

### Variant TEST-SINGLE

One table listing test cases, no "Quality gates" parallel table.

**Template:**

```markdown
### Unit Testing Coverage

| Component | Test | Status |
|-----------|------|--------|
| `<file>.test.js` | <test description> | ✅ |
| `<file>.test.js` | <another test> | ✅ |

**<N> tests** across <M> test files — all passing.
```

**Rules:**
- Heading: `### Unit Testing Coverage` (exact wording).
- Table: 3 columns — Component, Test, Status.
- One row per individual test case (not per file).
- Status cell uses only `✅` / `❌` / `⚠️` — no other glyphs.
- Summary line in bold below the table.

**Used by:** brands that track every test case individually in the PR body and don't document CI gates per-PR.

### Variant TEST-TWO-TABLE

Two-table format — Automated tests + Quality gates — plus metadata lines.

**Template:**

```markdown
### Testing Coverage

**Test runner:** <framework @ version, or "not yet configured">
**Command:** <`npm run test`, or "—">

#### Automated tests

| Test file | Covers | Tests | Status |
|-----------|--------|-------|--------|
| `<path>.test.js` | <coverage phrase> | <N> | ✅ |

**Total:** <N> tests across <M> files, all passing.

#### Quality gates (run on every PR)

| Gate | Source | Status |
|------|--------|--------|
| Lint | <config> via <command> | ✅ |
| Unit tests | <config> via <command> | ✅ |
| Build | <config> via <command> | ✅ |
| Security scan | <workflow path> | ✅ |
| License headers | <workflow path> | ✅ |
```

**No-tests variant** — replace the Automated tests table with one paragraph:

```markdown
#### Automated tests

No automated tests in this PR. A test suite is tracked as a follow-up in <tracker>. Quality gates below carry coverage responsibility until the suite exists.
```

**Rules:**
- Heading: `### Testing Coverage` (exact wording).
- Both metadata lines always present.
- Quality gates table always populated with every gate that exists in CI — never hide a passing gate.
- Status glyphs: only `✅` / `❌` / `⚠️`.

**Used by:** brands that catalogue CI gates inside the PR body and treat the PR as a coverage audit artifact.

## `## How to test this PR` heading variants

### Variant QA-INSTRUCTIONS

Heading: `## Instructions on how QA can test this PR`.

**Format per step:**

```markdown
1. <action>
   - **Expected:** <observable outcome>
2. <action> — `<command or url>`
   - **Expected:** <outcome>
```

**Rules:**
- Numbered list.
- Expected label: `- **Expected:**` (bold only) as a nested sub-bullet under the step.
- No ASCII flow tree at the top.
- Usually preceded by the route to test.

**Used by:** brands with a dedicated QA team where the heading signals the PR is handed off.

### Variant QA-HOW-TO-TEST

Heading: `## How to test this PR`.

**Format per step:**

```markdown
### <Feature group name>

> **Prereqs:** <one-line list, or "none">

1. <action>
      ***Expected:*** <observable outcome>
2. <action> — `<command or url>`
      ***Expected:*** <outcome>
```

**Rules:**
- Optional ASCII flow tree at the top (PRs with ≥ 3 feature groups) using only `├─ └─ │ ─`.
- Feature groups as `### Feature group` subheadings.
- `> **Prereqs:**` blockquote line under each group.
- Numbered list of steps.
- Expected label: `***Expected:***` (bold-italic, three asterisks each side) on a new line indented **6 spaces**.
- No checkboxes — always numbered lists.

**Used by:** brands that author tests primarily for themselves (solo maintainers or small teams) and want the indented bold-italic Expected label for clarity.

### Rules common to both QA variants

- Every step has an observable, specific outcome — never "it works".
- Commands / URLs in backticks at the end of the action line, separated by em dash.
- Sub-details for a step use nested `-` dashes between the action and the `Expected` line.

## `## Special Deployment Requirements` variants

### Variant DEPLOY-FREEFORM

Free-form prose + inline code blocks. No severity vocabulary. Often contains JSON / YAML config blocks pasted verbatim from a ticket.

**Template:**

```markdown
## Special Deployment Requirements

<Free-form paragraph explaining what deployment requires — service dependencies, data setup, feature flag values, person to coordinate with.>

<Optional code block with exact config to paste into an admin UI or env:>
```
{
  "feature_flag": "example",
  "variant": "B"
}
```

<Optional follow-up paragraph with caveats or ordering constraints.>
```

**Rules:**
- Omit the section entirely when the PR has no preconditions — no empty "N/A" block.
- Code blocks preserved verbatim from the source ticket.
- Call out people by name or role when coordination is required ("Ask <name> to create the object in the target environment before QA begins").

**Used by:** brands where deployment specifics come from Jira / Confluence tickets with exact config snippets to paste.

### Variant DEPLOY-SEVERITY

Numbered list with closed severity vocabulary (`CRITICAL` / `REQUIRED` / `OPTIONAL`).

**Template:**

```markdown
## Special Deployment Requirements

1. **<Topic> (CRITICAL)** — <requirement in one line>
2. **<Topic> (REQUIRED)** — <requirement>
3. **<Topic> (OPTIONAL)** — <requirement>
```

**Severity meaning:**

| Severity | Meaning |
|---|---|
| `CRITICAL` | Deploy fails without this — blocks the release |
| `REQUIRED` | Feature is broken without this, but deploy succeeds |
| `OPTIONAL` | Recommended; nice-to-have |

**Rules:**
- Numbered list only. Never checkboxes.
- Order: CRITICAL first, then REQUIRED, then OPTIONAL.
- Inline commands go in backticks at the end of the requirement line.
- Omit the section entirely when there are no preconditions.

**Used by:** brands that deploy via automation where severity cleanly maps to pipeline gating.

## `## Documentation` variants

### Variant DOC-OPEN

Open-ended `## Documentation` section. Contents are screenshots, videos, diagrams — whatever the author attaches. No fixed heading pattern. Often just `<!-- Add screenshots here -->` as a placeholder.

**Template:**

```markdown
## Documentation
<!-- Add screenshots here -->

### DEMO DESKTOP/TABLET/MOBILE

<attached media>
```

**Used by:** brands that treat Documentation as a free-form asset panel, sometimes with an ad-hoc heading like "DEMO DESKTOP/TABLET/MOBILE".

### Variant DOC-MEDIA-VOCAB

Fixed `### <MEDIA-TYPE> — <Target>` heading pattern per asset.

**Template:**

```markdown
## Documentation

### <MEDIA-TYPE> — <Target>

> *<1-line context blurb>*

<asset or placeholder>

### <MEDIA-TYPE> — <Target>

> *<1-line context blurb>*

<asset or placeholder>
```

**Closed media-type vocabulary:**

| Tag | Use for |
|---|---|
| `DESKTOP` | Screenshot at ≥ 1024px viewport |
| `TABLET` | Screenshot at 560–1024px viewport |
| `MOBILE` | Screenshot at ≤ 560px viewport |
| `VIDEO` | Screen recording |
| `DIAGRAM` | Architecture / flow diagram |
| `SCREENSHOT` | Generic screenshot where viewport is irrelevant |

**Rules:**
- Heading exactly `### <MEDIA-TYPE> — <Target>`. Em dash separator.
- Italic 1-line context under each heading via `> *…*` blockquote wrapper.
- Ordering: group by target, then by media type (DESKTOP → TABLET → MOBILE → VIDEO → DIAGRAM).

**Used by:** brands that want every asset categorised for later retrieval or release-note generation.

## Reference-link footnote footer

Some brands append a reference-link footer to the end of the PR body with `[TICKET-ID]: URL` style entries. This is a brand-level convention, controlled by the brand rule — see the brand file for whether to include it.

Example footer:

```markdown
[TICKET-1234]: https://jira.example.com/browse/TICKET-1234?atlOrigin=…
[TICKET-5678]: https://jira.example.com/browse/TICKET-5678?atlOrigin=…
```

## Variant-selection matrix (brand rules pick one per row)

| Section | Variant options |
|---|---|
| Technical Details | `TD-FREEFORM` or `TD-4FIELD` |
| Testing Coverage | `TEST-SINGLE` or `TEST-TWO-TABLE` |
| QA / How-to-test heading | `QA-INSTRUCTIONS` or `QA-HOW-TO-TEST` |
| Special Deployment | `DEPLOY-FREEFORM` or `DEPLOY-SEVERITY` |
| Documentation | `DOC-OPEN` or `DOC-MEDIA-VOCAB` |
| Reference-link footer | on or off |

## Correct vs incorrect examples

### Example 1: Technical Details variant mismatch

**Incorrect** — brand rule picks `TD-FREEFORM` but draft uses the 4-field shape:

```markdown
### Technical Details

- **Audio pipeline**
  - ***Chose:*** X
  - ***Over:*** Y
  - ***Why:*** Z
  - ***Trade-off:*** W
```

**Correct** — TD-FREEFORM with custom subheading:

```markdown
### SDK Limitations (Flagged for PM)

The following specs are not achievable with the current SDK:
- Centered peek layout on mobile — the SDK does not expose `centerMode` via any attribute
- Progress bar pagination — the SDK only supports default dot pagination
```

### Example 2: QA variant mismatch

**Incorrect** — brand rule picks `QA-INSTRUCTIONS` but draft uses the bold-italic Expected:

```markdown
## Instructions on how QA can test this PR

1. Open the page
      ***Expected:*** the widget loads
```

**Correct** — QA-INSTRUCTIONS uses inline `- **Expected:**`:

```markdown
## Instructions on how QA can test this PR

1. Open the page at `/some/route`
   - **Expected:** the widget loads and renders the hero image
```

### Example 3: Documentation vocabulary drift

**Incorrect** — brand rule picks `DOC-MEDIA-VOCAB` but draft uses an open heading:

```markdown
## Documentation

### Home page desktop view

![screenshot](…)
```

**Correct** — `DOC-MEDIA-VOCAB` with closed tag:

```markdown
## Documentation

### DESKTOP — Home page

> *Home page at 1440px viewport.*

![home-desktop](…)
```
