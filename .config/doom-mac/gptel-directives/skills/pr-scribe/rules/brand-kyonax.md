---
title: Kyonax Brand — Specific PR Format Overrides
impact: HIGH
impactDescription: Kyonax-owned repos use a structured PR format: bracket-tagged release titles, a 13-item self-review checklist, themed Changes subsections with a closed tag vocabulary, Chose/Over/Why/Trade-off decision blocks, a two-table Testing Coverage pattern, and a closed MEDIA-TYPE vocabulary for Documentation. Applying a different brand's format to a Kyonax PR breaks the layout that feeds this brand's release flow.
tags: brand, kyonax, changes-pattern-b, td-4field, test-two-table, qa-how-to-test, deploy-severity, doc-media-vocab, release, version, NEW, MOD, DEL, MOV, checklist-13
---

This rule defines the specific PR-body overrides for repos under the **Kyonax** GitHub organization. Load this alongside `pr-body-structure.md`, `changes-list.md`, `supporting-sections.md`, and `global-writing-rules.md` whenever drafting a PR for a Kyonax/* repo.

**Changes format:** Pattern B (themed `###` subsections with `[NEW] / [MOD] / [DEL] / [MOV]` tags). See `changes-list.md`.

**Variant selections:**

| Section | Variant |
|---|---|
| Technical Details | `TD-4FIELD` — Chose / Over / Why / Trade-off |
| Testing Coverage | `TEST-TWO-TABLE` — Automated tests + Quality gates |
| QA heading | `QA-HOW-TO-TEST` — `## How to test this PR` |
| Expected label | `***Expected:***` (bold-italic, 6-space indent) |
| Special Deployment | `DEPLOY-SEVERITY` — numbered list with `CRITICAL` / `REQUIRED` / `OPTIONAL` |
| Documentation | `DOC-MEDIA-VOCAB` — `### <MEDIA-TYPE> — <Target>` closed vocab |
| Reference-link footer | **OFF** |
| Top-of-body ticket URL | **OFF** — no external ticketing |

## Data source — the git state of the feature branch

Kyonax PRs are authored from the **git state of the feature branch itself** — there is no external ticketing system, no roam org-node, and (in most cases) no Figma design link. The skill draws PR content from the following sources, in priority order:

### Priority 1 — Git state on the feature branch (always available)

| Signal | Command / location | Produces |
|---|---|---|
| Branch name | `git rev-parse --abbrev-ref HEAD` | Signals the feature name (e.g. `websocket-cam-person`, `feat-preview-modal`). Often informs the PR title when no release-tag prefix applies. |
| Commits ahead of the base branch | `git log --oneline <base>..HEAD` | The commit subjects become the narrative backbone of the summary paragraph and the source of grouping signals for Pattern B's Changes subsections. |
| Diff against the base branch | `git diff --name-status <base>..HEAD` | The authoritative file inventory for the Changes block. Every entry in `### Implementation` / `### Release` / `### CI & Tooling` / `### Dependencies` / `### Docs` comes from this list. |
| Full diff | `git diff <base>..HEAD` | Used to validate claims about specific fields, props, configuration changes, etc. The Worker reads it to confirm the narrative matches the actual code. |

### Priority 2 — User-provided conversational context

The user typically describes the PR in the conversation that precedes generation ("this PR adds X, fixes Y, bumps to v0.3"). This narrative is the **primary source of the summary paragraph and the user-story blockquote**. The skill should incorporate the user's own framing — the tone is self-written because the solo maintainer is also the PR author.

### Priority 3 — Repo state for cross-references

| Source | Used for |
|---|---|
| `CHANGELOG.org` existing release entries | Cross-reference to the `[vX.Y]` entry when this is a release PR; extract the release headline from the `*Summary:*` line. |
| `CHANGELOG.org` TODO block | Identify which TODO items the current PR closes — surface them as user-story hooks in the summary paragraph. |
| `README.org` project identity | For Design / Reference line context when no external design exists (e.g. linking to `.github/assets/logo.txt`). |
| `package.json` version + name | Used in the release PR title (`release: vX.Y`) and the Release subsection version header. |
| `.github/workflows/ci.yml` | Source the Quality gates table rows in Testing Coverage from the actual job names in the workflow. |

### Priority 4 — Local session / clipboard state (internal only, never referenced in output)

Kyonax developers keep private local state files that inform PR drafting but **must never be named or linked from the PR body**:

- Personal session / continuity notes (e.g. roam session files, scratchpads)
- Local clipboard buffers used as draft targets
- Encrypted credential stores (`.rc.gpg`)
- Local env files (`.env`)
- Local-only governance scratchpads (`.github/BRANCHES.org`)

These may be consulted by the Worker to understand context (who I am, what I'm working on, what I decided last session), but the generated PR body contains **zero** paths, links, or references to them. See `global-writing-rules.md` rule 3 for the sweep requirement.

### What is NOT available

| Not available | Implication |
|---|---|
| External ticket URLs (Jira, Linear, etc.) | Slot [1] (top-of-body URL line) is always off. Checklist does not reference tickets. |
| Figma / design links | `**Design / Reference:**` value is either a link to a repo asset (e.g. an ASCII logo file on master) or `N/A`. |
| QA team handoff | The QA section is `## How to test this PR`, framed for the author / reviewer to self-verify — not a dedicated QA team. |
| Labels reminder | Kyonax repos use GitHub's native label picker and the `Pre-Check Failed` CI-driven label. The skill does not print a separate "Labels:" line after the body. |

### Release detection

A Kyonax PR is a **release PR** when **all three** of these files appear in the diff:

1. `package.json` with a `"version"` field change
2. `README.<ext>` with version-string changes (`#+VERSION:` line, ASCII logo footer, shields.io badge)
3. `CHANGELOG.<ext>` with a new `[vX.Y]` entry

Release PRs include the `### Release` subsection with the canonical triad. Non-release PRs omit `### Release` entirely (see `changes-list.md` Pattern B). If a PR touches one or two of the three triad files but not all three, something is off — either flag the inconsistency back to the user or complete the triad before drafting.

### Checklist conditional logic

Check each item in the 13-item checklist based on git state + user context:

| Checklist item | Check when |
|---|---|
| `Contains testing instructions` | Always `[x]` — every PR has a How-to-test section. |
| `Requires environment / credential changes` | Diff includes `.env.example` modifications OR the user mentions new `VITE_*` vars. |
| `Requires special deployment steps` | The PR body will include a `## Special Deployment Requirements` section (non-empty). |
| `Has unit / integration tests` | Diff includes `*.test.{js,ts,vue}` or `*.spec.{js,ts,vue}` files. |
| `Touches licensing, security, or CI` | Diff touches `LICENSE*` / `NOTICE` / `.github/workflows/*` / `SECURITY.*` / `eslint.config.*`. |
| `License headers on new files` | Every NEW source file (`.js` / `.vue` / `.scss` / `.mjs` / `.html` / `.css`) in the diff starts with the MPL header; verify via `head -5 <file> \| grep "Cristian D. Moreno"`. |
| `Attribution preserved on modified files` | No author line was removed from any modified file. |
| `Lint passes (\`npm run lint\`)` | Always `[x]` at PR-open time — the user ran it locally. |
| `Security rules followed` | No `eval`, no `innerHTML`, no hardcoded secrets, no insecure URLs in the diff. |
| `No credentials committed` | No `.env`, `.rc.gpg`, or other secret file in the staged diff; no plaintext secrets. |
| `All GitHub Checks have passed (no \`Pre-Check Failed\` label applied by CI)` | Always `[x]` at PR-open; CI will flip the label if checks fail. |
| `CHANGELOG.org updated (release PRs only)` | `[x]` when this is a release PR (triad satisfied); blank otherwise. |
| `Version bumped (release PRs only)` | `[x]` when this is a release PR; blank otherwise. |

### No fabrication

Every file path, function name, version number, component identifier, and test file name in the generated PR body must trace back to the diff, the user's narrative, or explicit repo state. Do not invent: commit counts, test counts, dependency versions, feature flag names, or release metadata that wasn't present in one of the Priority 1-3 sources.

## Title format

Two canonical patterns depending on PR type:

| PR type | Title pattern | Example |
|---|---|---|
| Feature branch → integration branch | `[vX.Y]: <Short Release Title>` | `[v0.3]: Vue App + CAM-LOG Overlay + Landing Index` |
| Feature branch (non-release) | `feat(<scope>): <lowercase summary>` | `feat(widgets): extract audio meter + readout` |
| Release integration → default branch | `release: vX.Y` | `release: v0.3` |
| Hotfix / follow-up | `fix(<scope>): <lowercase summary>` | `fix(ci): unblock ESLint + Security Scan` |

Version-tag prefix (`[vX.Y]:`) is used when a single PR brings a full version's worth of work into `dev`. Use Conventional Commits prefix (`feat(scope):`, `fix(scope):`) for smaller feature PRs and hotfixes.

## Checklist (Slot [2])

Use the exact 13 items below. Check boxes based on the PR content.

```markdown
## Checklist (check if it applies)

- [ ] Contains testing instructions
- [ ] Requires environment / credential changes
- [ ] Requires special deployment steps
- [ ] Has unit / integration tests
- [ ] Touches licensing, security, or CI
- [ ] License headers on new files ([LICENSING.org](https://github.com/Kyonax/<repo>/blob/master/LICENSING.org))
- [ ] Attribution preserved on modified files
- [ ] Lint passes (`npm run lint`)
- [ ] Security rules followed ([SECURITY.org](https://github.com/Kyonax/<repo>/blob/master/.github/SECURITY.org))
- [ ] No credentials committed
- [ ] All GitHub Checks have passed (no `Pre-Check Failed` label applied by CI)
- [ ] CHANGELOG.org updated *(release PRs only)*
- [ ] Version bumped *(release PRs only)*
```

### Checkbox logic

| Item | Check when |
|---|---|
| `Contains testing instructions` | Always (every PR has a How-to-test section) |
| `Requires environment / credential changes` | PR requires a new env var / secret / credential |
| `Requires special deployment steps` | `## Special Deployment Requirements` section is populated |
| `Has unit / integration tests` | Test files present in the diff |
| `Touches licensing, security, or CI` | PR modifies LICENSE* / NOTICE / `.github/workflows/*` / `SECURITY.*` / `eslint.config.*` |
| `License headers on new files` | Every new `.js` / `.vue` / `.scss` / `.mjs` / `.html` / `.css` has the MPL header |
| `Attribution preserved on modified files` | No existing author lines removed |
| `Lint passes` | Always `[x]` — required before opening |
| `Security rules followed` | No `eval`, `innerHTML`, hardcoded secrets, insecure URLs |
| `No credentials committed` | Always `[x]` — asserted before opening |
| `All GitHub Checks have passed ...` | Always `[x]` at open; re-check before merge |
| `CHANGELOG.org updated (release PRs only)` | Check only when PR bumps the version |
| `Version bumped (release PRs only)` | Check only when PR bumps the version |

### Link format in checklist

Repo-internal references use absolute URLs pinned to the default branch (usually `master`):

```
https://github.com/Kyonax/<repo-name>/blob/master/<path>
```

## Top line (Slot [1])

No top line. The PR body opens directly with the Checklist. Kyonax repos don't have an external ticketing system.

## Summary paragraph (Slot [3a])

A 1-paragraph (1-4 sentences) narrative, present tense, describing what ships. Does not name files or restate the Changes list.

**Standard phrasings:**

| Intent | Phrasing |
|---|---|
| Release | `Cuts **<RepoName> vX.Y** — <headline narrative>. Introduces <major change 1>, <major change 2>, …` |
| Feature | `Adds <feature description> so that <user benefit>.` |
| Fix | `Fixes <bug description> that was <impact>. <One sentence on the resolution approach>.` |
| Release merge PR (dev → master) | `Ships **<RepoName> vX.Y** to \`master\`. Fast-forwards the default branch with the cut already reviewed and merged into \`dev\` via <feature-PR-link>. Contents are identical — <N> feature commits plus one merge commit, no further code changes since that merge.` |

## User story blockquote (Slot [3b])

A `>` blockquote in the canonical `As a <role>, I want <outcome>, so that <benefit>.` shape. One sentence.

```markdown
> As a content creator running OBS, I want a Vite-served app that exposes every RECKIT browser source under a per-brand URL, so that I can add overlays to OBS with a copy-paste URL.
```

For non-feature PRs (fixes, releases, CI work), frame the story around the repo maintainer's goal:

```markdown
> As the repo maintainer, I want an explicit release PR that gates the `dev` to `master` cut on the CHANGELOG + version workflow, so that every release on master has a matching git tag and a GitHub Release entry.
```

## Design / Reference line (Slot [3c])

Bold label `**Design / Reference:**` (with slash, not just `**Design:**` — Kyonax pairs design refs with arbitrary external references like ASCII logos, CHANGELOG sections, architectural docs). Value is one or more absolute URLs.

```markdown
**Design / Reference:** ASCII logo at [`.github/assets/logo.txt`](https://github.com/Kyonax/<repo>/blob/master/.github/assets/logo.txt). Scope tracked in [`CHANGELOG.org`](https://github.com/Kyonax/<repo>/blob/master/CHANGELOG.org).
```

Use `"N/A"` as the value when no external reference exists.

## Changes block (Slot [3d])

Pattern B — five themed `###` subsections with the tag legend blockquote under Implementation. See `changes-list.md` for the full format spec.

```markdown
### Implementation

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

**<Group name>** (`<folder-path>`)
- **[NEW]** `<file>` — description
  - optional sub-bullet

### Release

**Version:** vX.Y (was vA.B)

- **[MOD]** `package.json` — version bumped from `A.B.0` to `X.Y.0`
- **[MOD]** `README.org` — `#+VERSION:`, ASCII logo footer, and shields.io badge all bumped to `vX.Y`
- **[MOD]** `CHANGELOG.org` — new `[vX.Y]` entry

### CI & Tooling

- **[MOD]** `.github/workflows/ci.yml` — <summary>
  - **<Job name>:** <specific fix>

### Dependencies

- **Runtime added:** <list or —>
- **Dev added:** <list or —>
- **Upgraded:** <list or —>
- **Removed:** <list or —>
- **Lockfile:** <status, if changed>

### Docs

- **[MOD]** `<file>` — description
```

## Technical Details (Slot [3e])

Use `TD-4FIELD` variant. One bullet per decision with Chose / Over / Why / Trade-off sub-fields (bold-italic labels).

```markdown
### Technical Details

- **<Decision title>**
  - ***Chose:*** <what we decided>
  - ***Over:*** <alternative rejected>
  - ***Why:*** <rationale>
  - ***Trade-off:*** <cost accepted>

- **<Another decision>**
  - ***Chose:*** …
  - ***Over:*** …
  - ***Why:*** …
  - ***Trade-off:*** …
```

## Testing Coverage (Slot [3f])

Use `TEST-TWO-TABLE` variant.

```markdown
### Testing Coverage

**Test runner:** <framework @ version>
**Command:** `npm run test`

#### Automated tests

| Test file | Covers | Tests | Status |
|-----------|--------|-------|--------|
| `<path>.test.js` | <coverage phrase> | <N> | ✅ |

**Total:** <N> tests across <M> files, all passing.

#### Quality gates (run on every PR)

| Gate | Source | Status |
|------|--------|--------|
| Lint | `eslint.config.mjs` via `npm run lint` | ✅ 0 errors |
| Unit tests | `vite.config.js` via `npm run test` | ✅ |
| Build | `vite.config.js` via `npm run build` | ✅ |
| Security scan | `.github/workflows/ci.yml` | ✅ |
| License headers | `.github/workflows/ci.yml` | ✅ |
| `Pre-Check Failed` label | `pre-check-label` job in `.github/workflows/ci.yml` | ✅ label absent |
```

## QA section (Slot [4])

Use `QA-HOW-TO-TEST` variant.

- Heading: `## How to test this PR` (exact wording).
- Optional ASCII flow tree at the top for PRs with ≥ 3 feature groups — box-drawing chars only (`├─ └─ │ ─`).
- Feature groups as `### <Feature group>` subheadings, each with a `> **Prereqs:**` blockquote.
- Numbered list of steps.
- Expected label: `***Expected:***` (bold-italic) on a new line, indented **6 spaces**.

```markdown
## How to test this PR

```
<PR title>
├─ Setup
├─ <Feature A>
└─ <Feature B>
```

### <Feature group name>

> **Prereqs:** <one-line list, or "none">

1. <action>
      ***Expected:*** <observable outcome>
2. <action> — `<command or url>`
      ***Expected:*** <outcome>
```

## Special Deployment Requirements (Slot [5])

Use `DEPLOY-SEVERITY` variant. Numbered list with closed severity tags, ordered CRITICAL first.

```markdown
## Special Deployment Requirements

1. **<Topic> (CRITICAL)** — <requirement in one line>
2. **<Topic> (REQUIRED)** — <requirement>
3. **<Topic> (OPTIONAL)** — <requirement>
```

Omit the section entirely when there are no preconditions.

## Documentation (Slot [6])

Use `DOC-MEDIA-VOCAB` variant. Fixed `### <MEDIA-TYPE> — <Target>` heading pattern.

```markdown
## Documentation

### DESKTOP — <Target>

> *<1-line context blurb>*

<asset or placeholder>

### MOBILE — <Target>

> *<1-line context blurb>*

<asset or placeholder>

### VIDEO — <Target>

> *<1-line context blurb>*

<url or placeholder>
```

Closed media-type vocabulary (see `supporting-sections.md`): `DESKTOP` / `TABLET` / `MOBILE` / `VIDEO` / `DIAGRAM` / `SCREENSHOT`.

## Reference-link footer (Slot [7])

Off. Kyonax PRs use absolute URLs inline throughout the body — no footnote-style references.

## No label reminder

Kyonax repos use GitHub's native label picker; the skill does not print a separate "Labels:" line after the body. Solo maintainer scope — label hygiene happens in the UI.

## Complete shape reference

```markdown
## Checklist (check if it applies)
- [x] Contains testing instructions
- [ ] Requires environment / credential changes
…
- [x] All GitHub Checks have passed (no `Pre-Check Failed` label applied by CI)
- [x] CHANGELOG.org updated *(release PRs only)*
- [x] Version bumped *(release PRs only)*

## What does this PR do?

<Summary paragraph — 1-3 sentences>

> <User story>

**Design / Reference:** <link or "N/A">

### Implementation

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

**<Group>** (`<folder>`)
- **[NEW]** `<file>` — …

### Release   ← omit if non-release PR

**Version:** vX.Y (was vA.B)

- **[MOD]** `package.json` — …
- **[MOD]** `README.<ext>` — …
- **[MOD]** `CHANGELOG.<ext>` — …

### CI & Tooling   ← omit if empty

- **[MOD]** `.github/workflows/ci.yml` — …

### Dependencies   ← omit if empty

- **Runtime added:** —
- **Dev added:** —
- **Upgraded:** —
- **Removed:** —

### Docs   ← omit if empty

- **[MOD]** `<file>` — …

### Technical Details   ← decisions in 4-field format

- **<Decision>**
  - ***Chose:*** …
  - ***Over:*** …
  - ***Why:*** …
  - ***Trade-off:*** …

### Testing Coverage

**Test runner:** <framework>
**Command:** `npm run test`

#### Automated tests
| Test file | Covers | Tests | Status |
|-----------|--------|-------|--------|
| … | … | N | ✅ |

**Total:** N tests across M files.

#### Quality gates (run on every PR)
| Gate | Source | Status |
|------|--------|--------|
| Lint | … | ✅ |
| … | … | ✅ |

## How to test this PR

### <Feature group>

> **Prereqs:** …

1. <step>
      ***Expected:*** <outcome>

## Special Deployment Requirements   ← omit if none

1. **<Topic> (CRITICAL)** — <requirement>
2. **<Topic> (REQUIRED)** — <requirement>

## Documentation

### DESKTOP — <Target>
> *<context>*
<asset>

### MOBILE — <Target>
> *<context>*
<asset>
```

## Correct vs incorrect examples

### Example 1: Flat `**Changes:**` instead of themed subsections

**Incorrect** — Pattern A on a Kyonax PR:
```markdown
**Changes:**
- **`foo.vue`** (`src/`):
  - detail
```

**Correct** — Pattern B with themed subsections and tag legend:
```markdown
### Implementation

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

- **[NEW]** `src/foo.vue` — detail
```

### Example 2: `**Design:**` instead of `**Design / Reference:**`

**Incorrect** — using the Madison Reed label:
```markdown
**Design:** [Figma](…)
```

**Correct** — Kyonax uses `**Design / Reference:**`:
```markdown
**Design / Reference:** [`.github/assets/logo.txt`](https://github.com/Kyonax/…)
```

### Example 3: QA with inline Expected bullets

**Incorrect** — `- **Expected:**` inline bullets (that's the MR style):
```markdown
1. Open the page
   - **Expected:** <outcome>
```

**Correct** — 6-space-indented bold-italic `***Expected:***` (Kyonax style):
```markdown
1. Open the page
      ***Expected:*** <outcome>
```
