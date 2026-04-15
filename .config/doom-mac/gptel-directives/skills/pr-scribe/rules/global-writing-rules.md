---
title: Global Writing Rules — Cross-Cutting Discipline for PR Bodies and Templates
impact: CRITICAL
impactDescription: Every piece of generated text (PR body, PR template scaffold) must obey these seven cross-cutting rules regardless of brand. Breaking any of them produces output that either leaks private information, 404s on rendered pages, pollutes the PR page with decoration, or repeats itself across sections.
tags: pr, template, global, cross-cutting, no-emojis, no-arrows, no-private-files, absolute-urls, no-redundancy, bold-italic-labels, checkbox-scope, validation, sweep
---

This rule catalogues the seven cross-cutting writing disciplines that apply to **every** PR body and PR template this skill produces — regardless of which brand rule is active. Breaking any of them is a hard failure, not a style preference. Load this file alongside the brand rule and whichever section-specific rule is being used so the Worker sweeps for these violations before returning output.

## 1. No emojis anywhere

Zero emojis in generated content. Emojis are decoration — reviewers and `git log` readers have not asked for decoration, and adding it inflates diff noise and breaks terminal rendering on narrow viewports.

**The only allowed exceptions** — single-character status glyphs inside specific table cells of the Testing Coverage section:

| Glyph | Meaning | Cell context |
|---|---|---|
| `✅` | pass / green | Status column of Automated tests or Quality gates tables |
| `❌` | fail / red | Same |
| `⚠️` | warn / advisory | Same |

Nowhere else. Not in section headings, not as category markers, not as visual anchors in prose, not in category labels for tiered warnings, not in commit subject lines, not in release titles. If tempted to add a "small" emoji for visual punch, do not.

**Sweep before returning:** grep the output for `[\u{1F300}-\u{1FAFF}]` (the emoji Unicode block) and for individual pictographic characters like `⚖ 👥 📦 🔒 🛠 📝 🎉 🚀 🔧 ✨ 🐛 💡 📝 🎨`. If any appear outside the Testing Coverage status cells, strip them.

## 2. No arrow characters

These glyphs are banned anywhere in the output:

| Glyph | Name |
|---|---|
| `→` | U+2192 rightwards arrow |
| `⇒` | U+21D2 rightwards double arrow |
| `➜` | U+279C heavy round-tipped rightwards arrow |
| `▶` / `◀` / `▲` / `▼` | triangle arrows |
| `=>` | ASCII fat arrow |

Use these instead:

| Intent | Replacement |
|---|---|
| Version bump | `bumped from X to Y` or `X, Y` (before, after) |
| Rename / move | For the `[MOV]` Changes tag, write `` `old/path` `` then the word "to" then `` `new/path` `` on the same line |
| Flow diagram | ASCII box-drawing chars only: `├─ └─ │ ─` |
| Separator after a step | A new line + indented italic label (e.g. `***Expected:***`), not an arrow |
| "A goes into B" in prose | "A into B", "A then B", comma, or em dash |

**Sweep before returning:** grep the output for the five arrow glyphs above + the `=>` literal. If any appear, rewrite the surrounding text. The only arrow-like glyphs permitted are the ASCII box-drawing characters inside an optional How-to-test flow tree.

## 3. No references to private / gitignored files

When the generated artifact is public (PR body, commit message, CHANGELOG entry, README, the `.github/PULL_REQUEST_TEMPLATE.md` that ships with the repo), **never reference files that the repo's `.gitignore` excludes**. Common offenders:

| File category | Examples |
|---|---|
| Personal / session notes | `session.md`, gptel session files, planning journals, scratch `TODO.org` |
| Local clipboard buffers | `COMMIT.org`, `PR.org`, `DRAFT.md` |
| Encrypted credential stores | `.rc.gpg`, `.keys.gpg`, any `*.gpg` used for secrets |
| Local env files | `.env`, `.env.local`, `.env.*.local` |
| Local-only governance notes | `.github/BRANCHES.org`, `.github/DEPLOY.md.local`, branch-setup scratchpads |

When documenting an env-related change, reference the **tracked template** (`.env.example`) rather than the gitignored real file. When a runbook lives in a local clipboard buffer, describe the *mechanism* without naming the file.

**Sweep before returning:** grep the output for every path matched by the repo's `.gitignore`. If any appears as an inline path, link, or cross-reference, rewrite the sentence to describe the concept without naming the private file.

## 4. Absolute URLs only in PR body context

Relative paths **do not resolve** when GitHub renders a PR description. A link like `[X](../LICENSE)` in a PR body renders as a 404 because GitHub resolves it against `https://github.com/<owner>/<repo>/pull/<N>/../LICENSE` — which is not a blob path.

| Context | Link format |
|---|---|
| PR body | `https://github.com/<owner>/<repo>/blob/<branch>/<path>` — absolute, point at the branch where the file exists (usually `master` / `main` after merge) |
| PR template file (`.github/PULL_REQUEST_TEMPLATE.md`) | Same as above — the template's links ship inside every PR body populated from it |
| CHANGELOG.org | Plain `[text](https://…)` absolute URLs or `[[link][text]]` org syntax. Don't link files that are not yet on the branch the CHANGELOG is being rendered from |
| Commit message | Plain absolute URLs or issue references (`#123`). No relative paths |
| README / docs in repo root | Relative paths ARE fine here (GitHub renders them relative to the file's own location) |

For file paths mentioned as identifiers (not clickable links), use inline code (`` ` `` backticks) without a URL — backticked paths render cleanly in all contexts.

**Sweep before returning:** grep the output for `]\(\.\.?/[^)]+\)` or `]\((?!https?://|#)[^)]+\)` to catch relative link targets. Replace with absolute URLs, or drop the link wrapper and keep the path in backticks.

## 5. No redundancy — each fact appears in exactly one section

Every file, decision, or behavior statement lives in its canonical section — never restated in a second section for emphasis. Pick the section that owns each fact and leave the others silent on it.

| Section | Owns |
|---|---|
| Summary paragraph under `## What does this PR do?` | The one-paragraph functional narrative. Names zero files. |
| User story blockquote | The user-goal framing. Names zero files. |
| Changes list subsections | Authoritative file inventory. Each file appears here once. |
| Technical Details | Architectural *rationale*. Names files only where the rationale specifically references them — never as a re-listing. |
| Testing Coverage | Test inventory + gate results. Names files only when they are test files or gate config files. |
| How to test this PR | Reproducible steps. Names files only when a step requires the reader to open one. |
| Special Deployment Requirements | Ops preconditions. Names files only when a precondition is about a specific file. |

**Validation:** after drafting, extract every `` `path/to/file` `` mention from the body and count its occurrences. If a path appears in more than one section as a Changes-list-style entry (as opposed to a prose reference), it is redundancy — leave only the Changes-list occurrence.

## 6. Inline colon labels are bold-italic; group headers are bold-only

Two classes of labels, two different treatments:

| Class | Syntax | Rendering | Use for |
|---|---|---|---|
| **Inline data label** | `***Label:***` (three asterisks each side) | **_Label:_** (bold + italic) | Data fields that carry values on the same line or the line below — `***Expected:***`, `***Chose:***`, `***Over:***`, `***Why:***`, `***Trade-off:***` |
| **Group header label** | `**Label:**` (two asterisks each side) | **Label:** (bold only) | Labels that *introduce* a following block rather than carry a field value — `**Prereqs:**`, `**Version:**`, `**Test runner:**`, `**Command:**`, `**Design / Reference:**` |

The hierarchy is deliberate: a bold-only header reads as louder than a bold-italic sub-field, so the eye flows from group header (louder) into the subordinate data labels (quieter).

**Examples:**

```markdown
> **Prereqs:** dev server running.            ← group header (bold only)

1. Open the landing page.
      ***Expected:*** meta bar shows …        ← inline data label (bold-italic)

**Test runner:** Vitest @ 4.x                  ← group header (bold only)

- **Audio source pipeline**                   ← decision title (bold only)
  - ***Chose:*** External client event stream ← inline data label (bold-italic)
  - ***Over:*** Browser-side polling loop     ← inline data label (bold-italic)
```

## 7. Checkboxes only in the top Checklist

The author self-review block at the very top of a PR body uses `- [ ]` / `- [x]` checkboxes. **Everywhere else, use plain numbered lists.** Do not put checkboxes inside:

- How-to-test step lists
- Special Deployment Requirements
- Documentation subsections
- Changes-list entries
- Any step-by-step instruction block inside the Technical Details

Numbered lists (`1.`, `2.`, `3.` …) are the default for step-like content. Bullets (`-`) are the default for item-like content. Checkboxes communicate "the author is reporting progress on these items" — they belong only where that semantic fits (i.e., the top Checklist).

**Exception:** a dedicated `## Checklist for QA` section (if the template calls for it) may also use checkboxes — but the semantic is still "the reader will tick these off themselves", not "step-by-step reproduction".

## Pre-return sweep checklist

Before returning any generated output, run this mental sweep:

1. [ ] Emoji outside Testing Coverage status cells?
2. [ ] Any `→ ⇒ => ➜ ▶ ▼` anywhere in the body?
3. [ ] Any inline path match the repo's `.gitignore` patterns?
4. [ ] Any relative link target (`]\(../` or `]\(path.md\)`)?
5. [ ] Any file path appearing as a Changes entry in more than one subsection?
6. [ ] Every `***Label:***` an inline data field and every `**Label:**` a group header?
7. [ ] Any checkbox outside the top `## Checklist` block?

If any answer is yes, fix before returning.

## Correct vs incorrect examples

### Example 1: Version bump notation

**Incorrect** — uses arrow:
```markdown
- **[MOD]** `package.json` — version `0.1.0 → 0.3.0`
```

**Correct** — uses "bumped from X to Y":
```markdown
- **[MOD]** `package.json` — version bumped from `0.1.0` to `0.3.0`
```

### Example 2: Private-file reference in PR body

**Incorrect** — references a gitignored runbook:
```markdown
See the release runbook in `COMMIT.org` for the full merge / tag sequence.
```

**Correct** — describes the mechanism without naming the private file:
```markdown
After merge, follow the standard release sequence: tag `vX.Y` on `master`, push the tag, and publish a GitHub Release with the matching CHANGELOG section as notes.
```

### Example 3: Emoji-decorated category header

**Incorrect** — emoji prefix:
```markdown
### 📦 Supply Chain
- `package.json` was modified
```

**Correct** — plain label:
```markdown
### Supply Chain
- `package.json` was modified
```

### Example 4: Relative URL in PR body

**Incorrect** — 404s when rendered:
```markdown
Security rules followed ([SECURITY.org](../SECURITY.org))
```

**Correct** — absolute URL pinned to a branch that contains the file:
```markdown
Security rules followed ([SECURITY.org](https://github.com/<owner>/<repo>/blob/master/.github/SECURITY.org))
```
