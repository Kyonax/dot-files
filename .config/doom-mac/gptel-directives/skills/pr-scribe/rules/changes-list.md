---
title: Changes Block — Two Supported Formats (Flat with Ticket Refs, or Themed Subsections with Tag Vocabulary)
impact: CRITICAL
impactDescription: The Changes block is the authoritative inventory of what a PR touches. Two distinct formats are in active use across real brand conventions — the brand rule (`brand-<name>.md`) picks one. Neither format is universally correct; picking the wrong one for the brand produces a PR that fails local conventions.
tags: pr, changes, changelist, patterns, flat, themed, subsections, tickets, tags, NEW, MOD, DEL, MOV, group-labels, implementation, release, ci-tooling, dependencies, docs, brand-switching
---

This rule catalogues the two Changes-block formats this skill supports. Every brand rule selects one. The brand rule (e.g. `brand-<company>.md`) must declare which pattern applies before any Changes block is drafted. If no brand matches, fall back to Pattern A (flat-with-optional-ticket-refs) — it imposes the least structure and is safe for any repo.

## Pattern A — Flat `**Changes:**` list with optional ticket references

The "one bold heading, then nested bullets" format. Used by teams that track work in an external ticketing system and prefer scanning by component rather than by subsystem category.

### Structure

```markdown
**Changes:**

- **`ComponentName.vue`** (`path/to/folder/`):
  - detail
  - detail
  - detail
- **`AnotherComponent.vue`** — <TICKET-ID> (`path/to/folder/`):
  - detail
  - detail
- **`ServiceFile.js`**:
  - detail
```

### Rules

| Rule | Enforcement |
|---|---|
| Heading | `**Changes:**` in bold. Not `### Changes` — plain bold inline. |
| One component per top bullet | The component / file is the anchor; grouping is per-file, not per-subsystem |
| Component name in backticks + bolded | `**`ComponentName.vue`**` — both bold and code-formatted |
| Path in parens follows the name | `(path/to/folder/)` after the backticked name — optional but common |
| Ticket-ID after em dash | When a specific bullet addresses a specific child ticket, append `— <TICKET-ID>` between name and path |
| Nested bullets hold specifics | Each sub-bullet is one concrete detail, one line |
| Status tags (optional, see `content-richness.md`) | Under RICH content richness, prepend `[NEW]` / `[MOD]` / `[DEL]` / `[MOV]` tags to each entry. Under MINIMAL, tags are omitted. See `content-richness.md` for tag vocabulary. |

### When the PR addresses multiple tickets

Add a "Child tickets addressed" line right under the summary paragraph, before the blockquote:

```markdown
Child tickets addressed: [TICKET-A](url) · [TICKET-B](url) · [TICKET-C](url)
```

Separator is ` · ` (middle dot), not comma or slash.

### Custom inline subsections

When the PR has cross-cutting sections that don't fit as a component bullet (e.g. "SDK Limitations (Flagged for PM)", "Technical Details", "Tracking Events"), add them as `###` subheadings *after* the Changes list and *before* the QA section. Their content is free-form prose or sub-lists.

### Correct example

```markdown
## What does this PR do?

This PR addresses [TICKET-1234](https://jira.example.com/browse/TICKET-1234). Fixes the dashboard widget hover overlay extending beyond the image height and refines the SDK configuration.

> On the product page, the widget has a hover overlay issue on desktop. When hovering, the overlay extends beyond the height of the image, creating a visual mismatch.

**Design:** [Figma](https://www.figma.com/design/example)

**Changes:**

- **`DashboardWidget.vue`** (`src/components/Widgets/`):
  - Moved `border-radius` and `overflow: hidden` from `.slider-item` to `.aspect-ratio-box` — constrains the overlay within the image boundary
  - Added `:deep(.aspect-ratio-box figure)` with `height: 100%` — ensures the SDK's `figure` element matches the image height
  - Removed hardcoded `data-autoplay: true` — widget no longer auto-scrolls

- **`ProductGrid.vue`** — TICKET-1234 (`src/components/Grid/`):
  - Changed `mobile-row-size` from `"1.5"` to `"2"` (see SDK limitations below)
  - Bumped section title font from `xs-f-xlarge` to `xs-f-xxlarge`

### SDK Limitations (Flagged for PM)

The following Figma specs are **not achievable** with the current SDK:
- **Centered peek layout on mobile** — SDK does not expose `centerMode` via `data-*` attribute
- **Progress bar pagination** — SDK only supports default dot pagination
```

## Pattern B — Themed `###` subsections with closed `[TAG]` vocabulary

The "five fixed subsections + tagged entries" format. Used by teams that manage release metadata inside the PR body and want reviewers to scan by change type (feature code vs build config vs deps vs docs).

### Structure

```markdown
### Implementation

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

**<Group name>** (`<folder-path>`)
- **[NEW]** `<file>` — description
  - optional sub-bullet
- **[MOD]** `<file>` — description

### Release

**Version:** vX.Y (was vA.B)

- **[MOD]** `package.json` — version bumped from `A.B.0` to `X.Y.0`
- **[MOD]** `README.<ext>` — version markers bumped to `vX.Y`
- **[MOD]** `CHANGELOG.<ext>` — new `[vX.Y]` entry

### CI & Tooling

- **[MOD]** `.github/workflows/ci.yml` — <one-line summary>
  - **<Job name>:** <specific fix>

### Dependencies

- **Runtime added:** <list or —>
- **Dev added:** <list or —>
- **Upgraded:** <list or —>
- **Removed:** <list or —>

### Docs

- **[MOD]** `<file>` — description
```

### The 5 themed subsections (fixed order, omit if empty)

```
### Implementation
### Release
### CI & Tooling
### Dependencies
### Docs
```

### Subsection scope table

| Subsection | Owns |
|---|---|
| `### Implementation` | Everything under `src/` (or the equivalent application source tree) |
| `### Release` | Version bumps — `package.json` `"version"`, README version strings, CHANGELOG release entry. Present only on release PRs. |
| `### CI & Tooling` | `.github/workflows/*`, lint config, build config, `.gitignore`, `.env.example` |
| `### Dependencies` | `package.json` deps + `package-lock.json` |
| `### Docs` | Documentation and governance artifacts (README prose, PR template, CODEOWNERS, etc.) |

### Hard boundary rules for ambiguous files

| File | Subsection |
|---|---|
| `package.json` `"version"` field | `### Release` |
| `package.json` deps changes | `### Dependencies` |
| `package-lock.json` | `### Dependencies` |
| `CHANGELOG.*` | `### Release` (always, even for TODO-only edits) |
| README version strings | `### Release` |
| README prose / structure | `### Docs` |
| Lint / build / CI / gitignore config | `### CI & Tooling` |
| `.github/CODEOWNERS` / `SECURITY.*` / `PULL_REQUEST_TEMPLATE.md` | `### Docs` |
| `LICENSE` / `NOTICE` | `### Docs` |
| Anything under `src/` | `### Implementation` |

### The Changes entry format

```markdown
- **[TAG]** `path/to/file` — one-line description
  - optional nested sub-bullet
```

| Element | Requirement |
|---|---|
| `**[TAG]**` | Tag is bolded and bracketed. Never code-ticked. |
| `` `path/to/file` `` | Path in backticks. Inline code, not a hyperlink. |
| `—` | Em dash (U+2014). Not `-`, not `→`, not `:`. |

### The closed tag vocabulary

| Tag | Meaning |
|---|---|
| `[NEW]` | File did not exist before this PR |
| `[MOD]` | Existing file edited |
| `[DEL]` | File removed from the repo |
| `[MOV]` | Renamed or relocated (show both paths separated by the word `to`: `` `old/path` to `new/path` ``) |

### Mandatory tag legend blockquote

Immediately under `### Implementation`, paste this verbatim:

```markdown
### Implementation

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

<entries follow>
```

Once per PR. Never duplicate in other subsections.

### Ordering within each subsection

1. Tag order: `[NEW]` first, then `[MOD]`, then `[DEL]`, then `[MOV]`.
2. Alphabetical by path within each tag.
3. Nested sub-bullets follow their parent entry immediately.

### Inner format per subsection

- **`### Implementation`** — flat list if ≤ 5 entries; bold inline group labels (`**Widgets** (\`src/shared/widgets/\`)`) when ≥ 3 entries share a subsystem.
- **`### Release`** — bold version header `**Version:** vX.Y (was vA.B)` + exactly 3 Changes entries (package.json / README / CHANGELOG — the canonical release triad).
- **`### CI & Tooling`** — flat list; nested `**<Job name>:**` sub-bullets on workflow files with multiple fixes.
- **`### Dependencies`** — exactly 4 mandatory bullets (`Runtime added`, `Dev added`, `Upgraded`, `Removed`), `—` for empty values, never dropped. Optional fifth `Lockfile` bullet.
- **`### Docs`** — flat list, one-line descriptions.

### Correct example

```markdown
### Implementation

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

**Widgets** (`src/shared/widgets/`)
- **[NEW]** `meter.vue` — self-contained visualizer
- **[NEW]** `readout.vue` — text display with throttle

**Overlays** (`src/brands/example-brand/`)
- **[MOD]** `dashboard-widget.vue` — rewired to consume the new widgets

### Release

**Version:** v0.3 (was v0.1)

- **[MOD]** `package.json` — version bumped from `0.1.0` to `0.3.0`
- **[MOD]** `README.md` — version header bumped to `v0.3`
- **[MOD]** `CHANGELOG.md` — new `[v0.3]` entry

### Dependencies

- **Runtime added:** `vue@^3.5.13`, `vue-router@^4.5.0`
- **Dev added:** `vite@^6.0.7`, `vitest@^4.1.4`
- **Upgraded:** —
- **Removed:** —
- **Lockfile:** `package-lock.json` now tracked

### Docs

- **[MOD]** `.github/PULL_REQUEST_TEMPLATE.md` — structured layout
```

## How the brand rule picks a pattern

Each `brand-<name>.md` rule **declares** which pattern it uses in its opening lines. Examples:

```markdown
<!-- In brand-madison-reed.md -->
**Changes format:** Pattern A (flat `**Changes:**` list with optional ticket references).

<!-- In brand-kyonax.md -->
**Changes format:** Pattern B (themed `###` subsections with [NEW]/[MOD]/[DEL]/[MOV] tags).
```

If no brand matches, fall back to Pattern A — it imposes less structure and is safe for projects that don't manage release metadata inside PR bodies.

## Correct vs incorrect examples

### Example 1: Wrong pattern for the brand

**Incorrect** — using Pattern B themed subsections for a brand that uses Pattern A:

```markdown
<!-- brand-madison-reed.md declares Pattern A, but the draft used B -->
### Implementation
> **[NEW]** …
- **[NEW]** `src/foo.vue` — …
```

**Correct** — Pattern A for that brand:

```markdown
**Changes:**

- **`foo.vue`** — TICKET-1234 (`src/`):
  - <detail>
```

### Example 2: Tags in Pattern A (RICH content richness)

**RICH** — tags + legend provide instant visual scanning:

```markdown
**Changes:**

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

- **[NEW]** **`HcbLocationSections.vue`** (`HcbLocationPageV2/HcbLocationSections/`):
  - ALL visible section content extracted from parent

- **[MOD]** **`HcbLocationPageV2.vue`** (`HcbLocationPageV2/`):
  - Refactored to thin parent
```

**MINIMAL** — tags omitted, detail is briefer:

```markdown
**Changes:**

- **`HcbLocationSections.vue`** (`HcbLocationPageV2/HcbLocationSections/`):
  - Extracted content from parent
```

Both are valid Pattern A. The content richness level (set by brand rule or defaulting to RICH) determines which style to use. See `content-richness.md`.

### Example 3: Pattern B release section with wrong structure

**Incorrect** — missing the canonical release triad:

```markdown
### Release

- **[MOD]** `package.json` — version bump
```

**Correct** — version header + exactly 3 entries:

```markdown
### Release

**Version:** v0.3 (was v0.1)

- **[MOD]** `package.json` — version bumped from `0.1.0` to `0.3.0`
- **[MOD]** `README.md` — version bumped to `v0.3`
- **[MOD]** `CHANGELOG.md` — new `[v0.3]` entry
```
