---
title: Madison Reed Brand — Specific PR Format Overrides
impact: HIGH
impactDescription: Madison Reed's Dotcom team has well-established PR conventions — JIRA ticket references, a specific 6-item checklist, flat Changes list with per-component sub-lists, custom Technical Details subsections, single-table Unit Testing Coverage, and "Instructions on how QA can test" heading. Applying any other brand's format to an MR PR will confuse reviewers and break team conventions.
tags: brand, madison-reed, mr, dotcom, jira, DOTCOMPB, changes-pattern-a, td-freeform, test-single, qa-instructions, deploy-freeform, doc-open, labels, figma, reference-footer
---

This rule defines the specific PR-body overrides for repos under the **MadisonReed** GitHub organization. Load this alongside `pr-body-structure.md`, `changes-list.md`, `supporting-sections.md`, and `global-writing-rules.md` whenever drafting a PR for a MadisonReed/* repo.

**Changes format:** Pattern A (flat `**Changes:**` list). See `changes-list.md`.

**Content richness:** RICH (see `content-richness.md`). Status tags (`[NEW]/[MOD]/[DEL]/[MOV]`) on Changes entries, individual test case rows, detailed expected labels.

**Variant selections:**

| Section | Variant |
|---|---|
| Technical Details | `TD-FREEFORM` — custom `###` subheadings with prose + bullets |
| Testing Coverage | `TEST-SINGLE` — single `### Unit Testing Coverage` table (RICH: one row per test case) |
| QA heading | `QA-INSTRUCTIONS` — `## Instructions on how QA can test this PR` |
| Expected label | `- **Expected:**` (inline bold only, nested sub-bullet) |
| Special Deployment | `DEPLOY-FREEFORM` — free-form prose + code blocks |
| Documentation | `DOC-OPEN` — open-ended `## Documentation` |
| Reference-link footer | **ON** |
| Top-of-body ticket URL | **ON** (bare JIRA URL on its own first line) |

## Data source — the roam org-node (required)

Madison Reed PRs are authored from a **roam org-mode node** (one `.org` file per ticket, stored in the author's `org-roam` directory). The org-node is the **primary source of truth** for the PR content. The skill never drafts an MR PR from git state alone — it reads the org-node and maps its fields onto PR slots.

### Extraction table — org fields to PR slots

| Data | Where in the org file | Notes |
|---|---|---|
| Ticket ID | `#+TITLE:` line | Extract `DOTCOMPB-XXXX`. For bugs: strip the `(BUG)` prefix so the ticket key is clean. |
| Jira URL | `#+SOURCE_URL:` line | Full URL. This is the bare-link first line of the PR body AND the target of the inline ticket link in the summary. |
| PR Title | `#+TITLE:` + `#+SUBTITLE:` | Format: `[DOTCOMPB-XXXX]: {Subtitle}`. Subtitle is taken verbatim. |
| Summary paragraph content | First body paragraph between the org header and the first `* SECTION` heading | 1-3 sentences describing what the PR does. Render as plain prose after the Jira-link sentence. |
| User-story / context blockquote | Same first body paragraph (for features) OR a dedicated `** SUMMARY` / `** IMPACT` pair (for bugs) | Rendered as `>` blockquote. Bugs use the structured `**SUMMARY:**` / `**IMPACT:**` shape when present. |
| Figma link | `* RELEVANT LINKs` section | Render as `**Design:** [Figma]({url})` on its own line. Omit entirely if absent. |
| Changes content | `** STRUCTURE AND FUNCTIONALITY` section — specifically the **Process** sub-block | Each top-level entry becomes a component bullet in the flat `**Changes:**` list (Pattern A). Group sub-items under each component as nested bullets. |
| Technical Details content | `** Key Decisions` section (often inside `NOTE` callouts) + any custom sub-sections the org-node uses | Rendered as custom `###` subheadings following `TD-FREEFORM` variant. Common patterns: `### Technical Details`, `### SDK Limitations (Flagged for PM)`, `### Tracking Events`, `### <Tool> Integration`. |
| Unit test inventory | `** UNIT TEST COVERAGE` section | One row per individual test case in the `### Unit Testing Coverage` table. Group rows by component file name. |
| QA instructions | `** QA INSTRUCTIONs` section | Numbered steps with `- **Expected:**` inline bullets. Preserve the author's step hierarchy verbatim. |
| Deployment requirements | `** DEPLOYMENT NOTEs` section | Rendered under `## Special Deployment Requirements`. Preserve any JSON / YAML config code blocks verbatim. |
| Commit message reference | `* COMMIT MSG` section | Used ONLY to inform the summary paragraph's tone — never copied into the PR body as its own section. |

### Code files — optional, for validation only

If the user provides the modified source files alongside the org-node, cross-reference the org-node's claims against the actual code:

1. Verify that mentioned props, methods, component names, and events actually exist in the changed files.
2. Flag any discrepancy between what the org-node describes and what the diff shows.
3. Do **not** invent content that isn't in the org-node — the code files are for validation, not enrichment.

### Org-to-Markdown conversion rules

When rendering the org-node's content as Markdown in the PR body:

| Org syntax | Markdown equivalent |
|---|---|
| `*bold*` | `**bold**` |
| `=verbatim=` | `` `inline code` `` |
| `/italic/` | `*italic*` |
| Org lists (`-`, `+`, `1.`) | Same Markdown list syntax (prefer `-` over `+`) |
| Org tables (`|  \|  \|`) | Pipe-delimited Markdown tables |
| `#+begin_src <lang>` / `#+end_src` | Triple-backtick fenced code block with the same language tag |
| Org internal links `[[target][label]]` | Markdown links `[label](target)` |

### Checklist conditional logic

Check each item in the 6-item checklist based on org-node content:

| Checklist item | Check when |
|---|---|
| `contains testing instructions` | Always `[x]` — every MR org-node includes `** QA INSTRUCTIONs`. |
| `requires a lambda deployment to test or release to production` | Only `[x]` if `** DEPLOYMENT NOTEs` mentions backend / lambda / server changes. |
| `requires special deployment requirements/instructions` | Only `[x]` if `** DEPLOYMENT NOTEs` has CMS config, feature flags, or non-standard deploy steps. |
| `has unit tests` | `[x]` if `** UNIT TEST COVERAGE` exists OR any `.test.{js,ts,vue}` files are in the changeset. |
| `contains db migrations` | Only `[x]` if `** DEPLOYMENT NOTEs` mentions database migrations. |
| `all Github Checks have passed ...` | Always `[x]` — asserted before the PR opens. |

### Bug vs Feature detection

Inspect `#+TITLE:`:
- Contains `(BUG)` → Bug ticket. Summary uses "Fixes …" language. Context blockquote describes the bug (or uses `**SUMMARY:** / **IMPACT:**`).
- No `(BUG)` prefix → Feature / Story. Summary uses "Implements / Adds / Introduces …" language. Context blockquote is the user story.

### Omit-empty principle

When an org-node section is empty, **omit the corresponding PR section entirely** — do not leave empty headings, placeholder text, or `<!-- TODO -->` comments. Missing `** DEPLOYMENT NOTEs` means the PR has no `## Special Deployment Requirements` heading at all.

### No fabrication

Only include information that exists in the org-node or the optional code files. Do not invent test cases, deployment steps, Figma links, or QA instructions. If the org-node is incomplete for a given slot, either omit the slot or flag the gap back to the user — never fill it in from assumptions.

## Title format

```
[DOTCOMPB-XXXX]: <Subtitle>
```

- Bracket the ticket key.
- Em dash OR plain colon after the `]` — the colon-space pattern (`[DOTCOMPB-XXXX]: `) is the canonical form.
- Subtitle is title case, descriptive, ≤ ~70 chars.
- For bug tickets: subtitle describes the bug; do not prefix with "Fix" (the ticket title usually already describes the bug clearly).

**Examples:**
- `[DOTCOMPB-7466]: Shade Shop Page Redesign + Location Click Redirect`
- `[DOTCOMPB-7903]: Fix Shop All link not tappable on mobile devices`
- `[DOTCOMPB-7889]: Dash Hudson Carousel Hover Overlay Is Taller Than the Image on Desktop PDP`

## Top line (Slot [1])

**First line of body is the bare JIRA URL** — not a markdown link, not inside a section heading. Example:

```
https://madison-reed.atlassian.net/browse/DOTCOMPB-7903?atlOrigin=...
```

The `atlOrigin=...` query param often comes from Jira's own "Copy link" action. Preserve it verbatim when pasted. If the user provides a plain URL without the query param, use it as-is.

## Checklist (Slot [2])

Use the exact 6 items below verbatim. Check boxes (`[x]` / `[ ]`) based on the PR content.

```markdown
## Checklist for PR Author (Check if it applies)
- [ ] contains testing instructions
- [ ] requires a lambda deployment to test or release to production
- [ ] requires special deployment requirements/instructions
- [ ] has unit tests
- [ ] contains db migrations
- [ ] all Github Checks have passed ([Please document flaky tests here](https://madison-reed.atlassian.net/wiki/spaces/ENGINEERIN/pages/815857713/Flakey+Unit+Tests))
```

### Checkbox logic

| Item | Check when |
|---|---|
| `contains testing instructions` | Always (every MR PR has a QA section). Default `[x]`. |
| `requires a lambda deployment to test or release to production` | Deployment notes mention backend / lambda / server changes |
| `requires special deployment requirements/instructions` | Deployment notes include CMS config, feature flags, or non-standard deploy steps |
| `has unit tests` | The PR includes `*.test.{js,ts,vue}` files OR the Unit Testing Coverage section exists |
| `contains db migrations` | Deployment notes mention database migrations |
| `all Github Checks have passed ...` | Always `[x]` — asserted before PR opens |

## Summary paragraph (Slot [3a])

Opens with the ticket reference as a Markdown link, followed by a 1-3 sentence narrative.

**Standard phrasings:**

| Intent | Phrasing |
|---|---|
| Feature | `This PR addresses [DOTCOMPB-XXXX](<url>). Implements <short feature description>.` |
| Bug fix | `This PR addresses [DOTCOMPB-XXXX](<url>). Fixes <short bug description>.` |
| Multi-ticket | `This PR targets [DOTCOMPB-XXXX](<url>) and resolves N post-launch bugs/fixes/…` |
| Epic / parent | `This PR addresses [DOTCOMPB-XXXX](<url>). <High-level goal>.` |

### Multi-ticket PRs

When the PR closes multiple child tickets, add a `Child tickets addressed:` line right under the summary paragraph, separator is ` · ` (middle dot):

```markdown
Child tickets addressed: [DOTCOMPB-7756](https://...) · [DOTCOMPB-7759](https://...) · [DOTCOMPB-7760](https://...) · [DOTCOMPB-7761](https://...)
```

## User-story / context blockquote (Slot [3b])

A `>` blockquote with either a user story (feature PRs) or a context / impact statement (bug PRs).

**Feature PR shape:**

```markdown
> As a <role>, I want <outcome>, so that <benefit>.
```

**Bug PR shape (structured):**

```markdown
> **SUMMARY:** <one-line description of the bug>.
>
> **IMPACT:** <severity — e.g. "High - Production issue blocking mobile users">.
```

**Bug PR shape (prose):** A plain one-paragraph description of the bug without the `SUMMARY:` / `IMPACT:` structure is also acceptable when the bug is straightforward.

## Design / Reference line (Slot [3c])

When the ticket has a Figma link, include it on its own line after the blockquote:

```markdown
**Design:** [Figma](https://www.figma.com/design/...)
```

Exact label wording: `**Design:**` (bold only). Omit the line entirely when no Figma / external design exists.

## Changes block (Slot [3d])

Pattern A — flat `**Changes:**` list.

```markdown
**Changes:**

- **`ComponentName.vue`** — DOTCOMPB-XXXX (`path/to/folder/`):
  - detail (one line)
  - detail (one line)
  - detail (one line)

- **`AnotherFile.vue`** (`path/to/folder/`):
  - detail
  - detail
```

### Rules

- Bold `**Changes:**` heading, then flat bullet list — not `### Changes`.
- Each top-level bullet names one component / file.
- Component name in backticks AND bolded: `**\`Name.vue\`**`.
- Optional `— DOTCOMPB-XXXX` after the name when the bullet addresses a specific ticket (especially in multi-ticket PRs).
- Path in parens after the name: `(path/to/folder/)`.
- Nested sub-bullets for specific behaviors — one detail per line.
- Avoid "wall of text" paragraphs inside a bullet. Break dense content into sub-bullets.

## Technical Details / Custom subsections (Slot [3e])

Use `TD-FREEFORM` variant. Create custom `###` subheadings for specific architectural topics, SDK limitations, or cross-cutting decisions.

**Common custom subheading patterns:**

- `### Technical Details` — general architectural notes
- `### SDK Limitations (Flagged for PM)` — what's not achievable with the current toolchain
- `### Tracking Events` — Segment / analytics event inventory
- `### <Tool-name> Integration` — e.g. `### Algolia Search Integration`
- `### Experiment Behavior` — feature-flag / A-B test behavior notes

Content under each subheading is prose + bullet lists. Can include `###` sub-sub-headings when the topic warrants.

**Tracking Events subsection** (common for MR): list every new Segment event as a bullet with its trigger context.

```markdown
### Tracking Events

- `trackMREvent('X – …')` — triggered when the user clicks the Y button on the Z page
- `trackMREventAndRedirect('X – …')` — triggered when the user clicks a product tile
```

## Testing Coverage (Slot [3f])

Use `TEST-SINGLE` variant.

```markdown
### Unit Testing Coverage

| Component | Test | Status |
|-----------|------|--------|
| `ComponentA.vue` | specific test case description | ✅ |
| `ComponentA.vue` | another test case | ✅ |
| `ComponentB.vue` | test case | ✅ |

**<N> tests** across <M> test files — all passing.
```

- Heading exactly `### Unit Testing Coverage` (not `### Testing Coverage`).
- Group rows by component file — repeat the Component cell for each test in that file.
- Status cell uses `✅` only (or `❌` / `⚠️` on a known failure / warning).
- Bold summary line below the table with total test + file counts.

## QA section (Slot [4])

Heading: `## Instructions on how QA can test this PR` (exact wording).

Variant: `QA-INSTRUCTIONS`. Numbered list with `- **Expected:**` inline bold label as a nested sub-bullet under each step.

```markdown
## Instructions on how QA can test this PR

1. Navigate to `/some/route` on the PR deploy environment.
   - **Expected:** <observable outcome>
2. Click the <UI element> and verify the transition.
   - **Expected:** <outcome>
3. Test responsive breakpoints: 375px, 768px, 960px, 1024px, 1299px+.
```

### Multi-feature PRs

When the PR spans multiple independent features, split with `###` subheadings per feature (e.g. `### Shade Shop Page (DOTCOMPB-7466)` and `### Location Click Redirect (DOTCOMPB-7886)`), then a numbered list inside each.

## Special Deployment Requirements (Slot [5])

Use `DEPLOY-FREEFORM` variant. Free-form prose, inline bold callouts for critical items, code blocks for exact config to paste.

**Common patterns:**

- `**<Dependency> dependency (CRITICAL):**` inline bold callout for must-have preconditions.
- Paragraph describing what to create, who to ask, and when.
- `{ "key": "value" }` code blocks for exact JSON / YAML config to paste into Tophat / Launch Darkly / admin UIs.

Omit the section entirely when there are no preconditions — never leave "N/A" or an empty heading.

## Documentation (Slot [6])

Use `DOC-OPEN` variant. Open-ended `## Documentation` section with placeholder:

```markdown
## Documentation
<!-- Add screenshots here -->

### DEMO DESKTOP/TABLET/MOBILE [optional descriptive anchor]

<user-attachments video or screenshots>
```

Ad-hoc headings under `## Documentation` are acceptable (e.g. `### DEMO DESKTOP/TABLET/MOBILE TOPHAT CONFIGURATION`, `### MOBILE SAFARI BEFORE/AFTER`).

## Reference-link footer (Slot [7])

Add a reference-link footer at the very end of the PR body mapping each `[DOTCOMPB-XXXX]` used in the body to its Jira URL:

```markdown
[DOTCOMPB-7466]: https://madison-reed.atlassian.net/browse/DOTCOMPB-7466?atlOrigin=...
[DOTCOMPB-7886]: https://madison-reed.atlassian.net/browse/DOTCOMPB-7886?atlOrigin=...
```

Each ticket ID referenced inline somewhere in the body gets one footer line. Preserves the `atlOrigin` query param when available.

## Required labels (post-body output)

After the PR body, print the required labels as a reminder for the PR author to apply manually:

```
Labels: DOTCOM TEAM, Pending QA Review
```

**Label catalogue:**

| Label | When to apply |
|---|---|
| `DOTCOM TEAM` | Always — identifies the owning team |
| `Pending QA Review` | When the PR is ready for QA (default starting state) |
| `Pending Code Review` | Alternative starting state — code review before QA |
| `Special Deploy Requirements` | When `## Special Deployment Requirements` section is present |
| `WIP` | When the PR is still draft-quality |

## Complete shape reference

```markdown
{JIRA_URL}

## Checklist for PR Author (Check if it applies)
- [x] contains testing instructions
- [ ] requires a lambda deployment to test or release to production
- [{x or blank}] requires special deployment requirements/instructions
- [{x or blank}] has unit tests
- [ ] contains db migrations
- [x] all Github Checks have passed ([Please document flaky tests here](https://madison-reed.atlassian.net/wiki/spaces/ENGINEERIN/pages/815857713/Flakey+Unit+Tests))

## What does this PR do?

This PR addresses [DOTCOMPB-XXXX](<url>). <Summary — 1-3 sentences>.

> <User story or bug context blockquote>

**Design:** [Figma](<url>)    ← optional

**Changes:**

- **`ComponentName.vue`** (`path/`):
  - detail
  - detail

### Technical Details   ← optional, use custom subheadings as needed

<prose + bullets>

### Unit Testing Coverage   ← when tests exist

| Component | Test | Status |
|-----------|------|--------|
| `Name.vue` | test | ✅ |

**N tests** across M test files — all passing.

## Instructions on how QA can test this PR

1. <step>
   - **Expected:** <outcome>

## Special Deployment Requirements   ← omit if none

<prose + optional code blocks>

## Documentation
<!-- Add screenshots here -->

[DOTCOMPB-XXXX]: <url>
```

## Correct vs incorrect examples

### Example 1: Wrong checklist item wording

**Incorrect** — paraphrased items:
```markdown
- [x] includes test instructions
- [ ] needs lambda deployment
```

**Correct** — verbatim MR items:
```markdown
- [x] contains testing instructions
- [ ] requires a lambda deployment to test or release to production
```

### Example 2: Using Kyonax's themed subsections

**Incorrect** — Pattern B `### Implementation` / `### Release` subsections on an MR PR:
```markdown
### Implementation
> **[NEW]** …
- **[NEW]** `src/foo.vue` — …
```

**Correct** — MR uses Pattern A flat `**Changes:**`:
```markdown
**Changes:**

- **`foo.vue`** (`src/`):
  - detail
```

### Example 3: Missing JIRA top line

**Incorrect** — body starts with the Checklist heading:
```markdown
## Checklist for PR Author (Check if it applies)
- [x] contains testing instructions
…
```

**Correct** — bare JIRA URL on the first line:
```
https://madison-reed.atlassian.net/browse/DOTCOMPB-7903

## Checklist for PR Author (Check if it applies)
- [x] contains testing instructions
…
```
