---
title: Universal Conventions — Info Comments, Conciseness, and Organization Floor
impact: CRITICAL
impactDescription: These conventions apply to every PR body this skill produces, on every branch, in every repo, regardless of which brand rule (or no brand rule) is active. They lock in the visual scaffolding (info-comment blockquotes, metadata header lines, column-named tables, legend captions, prereqs blockquotes), the conciseness discipline (one-line entries, qualified status cells, observable Expected outcomes, noun-phrase decision titles), and the organization floor (tag ordering, group labels, fixed subsection sequence). Brand rules may add slots on top of this floor (extra checklist items, brand-specific title formats, different variant selections) but never remove or weaken these patterns. A PR body missing any of these is treated as incomplete, even if the brand rule didn't restate the requirement.
tags: pr, universal, floor, conciseness, organization, info-comments, legend, metadata, column-headers, group-labels, prereqs, expected, observable, status-qualifier, hard-rules, every-branch
---

This rule is the **universal floor** for every PR body the skill produces. Brand rules describe what changes per organization (title format, checklist length, variant selections); this rule describes what stays constant. Load it on every task alongside `global-writing-rules.md` and `pr-body-structure.md` — it never depends on which brand was detected.

The three pillars below cover the same axes the user evaluates a draft on:

| Pillar | Question it answers |
|---|---|
| Info-comment patterns | Does each block carry a caption telling the reader *what they're looking at* before the data starts? |
| Conciseness discipline | Does every cell, bullet, and Expected line read as one tight unit, or does it sprawl into prose? |
| Organization floor | Could a reviewer find a specific file or test by predicting where it lives without scanning every section? |

If a brand rule contradicts this file, this file wins. If a brand rule omits a topic this file covers, this file applies. The only way to opt out is an explicit `**Universal floor override:**` line in the brand rule naming the specific section.

## Pillar 1 — Info-comment patterns (the captions that tell the reader what's coming)

Every structural block opens with a brief, named caption so the reader never has to guess what the data below means. These captions are mandatory; their wording is fixed; they appear at the position prescribed below.

### 1.1 Tag legend blockquote (Changes block)

When status tags appear anywhere in the Changes block (Pattern A under RICH content, Pattern B always), the legend blockquote is mandatory and pasted verbatim immediately after the first `**Changes:**` heading or `### Implementation` heading:

```markdown
> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated
```

Exactly one legend per PR. Never duplicate in other subsections (CI & Tooling, Dependencies, Docs). The middle-dot separator is `·` (U+00B7), not `|`, not `,`, not `/`.

### 1.2 Metadata header lines (Testing Coverage)

Every Testing Coverage section opens with these two bold-label lines, regardless of variant (`TEST-SINGLE` or `TEST-TWO-TABLE`):

```markdown
**Test runner:** <framework @ version>
**Command:** `<exact command to run the suite>`
```

If tests don't exist yet, the values are `not yet configured` and `—`. Never omit the lines themselves.

### 1.3 Prereqs blockquote (every QA test group)

Every `### <Feature group>` inside the How-to-test section opens with a `> **Prereqs:**` blockquote before any numbered step. The value is one line listing what the tester needs (dev server, branch checkout, route, login state, OS settings). When nothing is needed, write `> **Prereqs:** none`.

```markdown
### Build and serve

> **Prereqs:** `npm install` complete.

1. Run `npm run build`
      ***Expected:*** ...
```

### 1.4 ASCII flow tree (QA section overview)

When the How-to-test section has 4 or more `### Feature group` subheadings, open the section with an ASCII flow tree showing every group at a glance. Box-drawing characters only (`├─ └─ │`); no Unicode arrows. The first line is the PR title or scope, indented two spaces inside a fenced block:

```markdown
## How to test this PR

`​`​`
[v2.0]: Vue 3 SSG migration
├─ Setup
├─ Build and serve
├─ Landing flow per locale
├─ JSON-LD and SEO meta
├─ ADA accessibility
├─ YouTube facade
├─ Privacy page routes
└─ Hostinger deploy preview
`​`​`
```

Below 4 groups, the tree is omitted.

### 1.5 Column-named tables (every table in the body)

Every table column has a name in the header row that tells the reader what that column contains. No anonymous columns, no abbreviation-only columns, no symbol-only columns. The canonical column sets are:

| Table | Required column names |
|---|---|
| Changes block (Pattern A flat list) | n/a — table format not used; bullets only |
| Automated tests (RICH, individual rows) | `Test file` · `Covers` · `Tests` · `Status` |
| Automated tests (RICH, grouped by file at 31+ tests) | `Component` · `Tests` · `Key Coverage` · `Status` |
| Automated tests (MINIMAL) | `Component` · `Test` · `Status` |
| Quality gates | `Gate` · `Source` · `Status` |
| Technical Details TD-4FIELD | n/a — bullets only; sub-fields carry the names |
| Documentation DOC-MEDIA-VOCAB | n/a — heading carries the type |

Status columns are exactly one of: `Status`. Never `S`, never blank, never decorated.

### 1.6 Italic context blurb (Documentation media-type blocks)

Every `### <MEDIA-TYPE> — <Target>` block in the Documentation section carries a 1-line italic context blurb wrapped in a blockquote, between the heading and the asset:

```markdown
### DESKTOP — Home page

> *Home page at 1440px viewport with the new hero grid.*

![home-desktop](https://...)
```

When the asset is not yet available, write `[Placeholder — capture after deploy]` in plain text where the asset would go. The italic blurb is still required.

### 1.7 Decision titles (Technical Details)

In `TD-4FIELD`, each decision opens with a bold noun-phrase title on its own bullet line, then the four sub-fields. In `TD-FREEFORM`, each `###` subheading is a noun phrase naming the topic of the decision. Never a sentence, never a question.

```markdown
- **CSS-only masonry**                          ← noun phrase title
  - ***Chose:*** ...
  - ***Over:*** ...
```

## Pillar 2 — Conciseness discipline (every cell, bullet, and line)

The PR body is a scan surface, not a long-read. Every unit of content earns its place in one line. Multi-line prose is restricted to the three sections that explicitly allow it (Summary paragraph, Technical Details, Special Deployment Requirements).

### 2.1 One-line Changes entries

Each `- **[TAG]** \`path\` — description` is exactly one physical line. The description is a sentence fragment, not a complete sentence. It names the *change*, not the file's responsibilities.

```markdown
- **[NEW]** `src/router.js` — vue-router 4 with `/` (EN) and `/es` (ES) routes, no trailing slash
- **[MOD]** `src/main.js` — vite-ssg entry + per-app i18n instance
```

When a single file has 2 or more genuinely separate facts, add nested sub-bullets — each also one line:

```markdown
- **[MOD]** `.github/workflows/ci.yml` — pipeline gates expanded
  - **Lint job:** runs `npm run lint` on PR open
  - **License job:** scans every `.vue` / `.mjs` / `.js`
```

No paragraph wrapping inside a Changes entry. If the description needs paragraph framing, the explanation belongs in Technical Details with the file mentioned there by name.

### 2.2 Concise "Covers" cell in Automated tests

The `Covers` cell is a 3-to-7-word phrase listing the things tested, comma-separated. Use `+` for inseparable pairs. Never a complete sentence.

```markdown
| `tests/unit/data/youtube.test.js` | URL parsing, ID extraction, descriptor builder | 12 | ✅ |
| `tests/unit/seo/sanitize.test.js` | stripHtml + numeric-entity decoder | 8 | ✅ |
```

In the grouped variant the `Key Coverage` cell extends the same discipline to 5-8 phrases:

```markdown
| `LocationImageCarousel.test.js` | 24 | Carousel/skeleton render, ADA, computed props, image watcher, slide click | ✅ |
```

### 2.3 Qualified status glyphs (preferred) vs bare glyphs (acceptable)

The Status cell is one of `✅` / `❌` / `⚠️` and **may** carry a one-clause qualifier when there's a meaningful detail. Qualifiers are short, factual, and value-bearing:

```markdown
| Lint | `eslint.config.mjs` via `npm run lint` | ✅ 0 errors |
| Build | `vite.config.js` via `npm run build` | ✅ both locales |
| `check:seo` | `scripts/seo-audit.mjs` (postbuild) | ✅ 46/0 pass |
```

Bare `✅` is acceptable when the gate has no countable artifact. Decorative qualifiers ("looks great", "all good") are not acceptable.

### 2.4 Observable Expected outcomes

Every `***Expected:***` line names a concrete observable: a UI element, a class name, a URL, a console output, a file path, a count, a network call, a header value. Never `it works` / `page loads correctly` / `displays properly` / `no errors`. The reader must be able to verify the outcome with a clear yes/no.

```markdown
1. Visit `/`
      ***Expected:*** `<html lang="en">`, hero kanji watermark absent, corner labels visible at 1.25rem inset.
2. Run `npm run check:seo`
      ***Expected:*** 46/0 pass across `/`, `/es`, `/privacy`, `/es/privacy`. Title, description, canonical, hreflang, JSON-LD all green.
```

When the observable is a count, name the count. When it's a DOM element, name the element. When it's a network call, name the URL or the request shape.

### 2.5 Concise Source cell (Quality gates)

The `Source` column names *what runs the gate*, not what the gate does (that's the column name). Pattern: `<config-file>` via `<command>`, or just `<workflow-file>` when the gate runs only in CI:

```markdown
| Lint | `eslint.config.mjs` via `npm run lint` | ✅ 0 errors |
| Security scan | `.github/workflows/ci.yml` | ✅ |
```

### 2.6 No marketing voice anywhere

Drop these patterns wherever they appear:

| Banned phrase pattern | Replace with |
|---|---|
| "We decided to..." / "We chose to..." | Bare noun phrase or `***Chose:***` line |
| "This PR is a major step toward..." | Bare functional statement |
| "We're excited to ship..." | Drop the framing; state what ships |
| "Cleanly handles..." / "Gracefully handles..." | Name the case it handles |
| "Robust" / "powerful" / "seamless" / "elegant" | Drop the adjective; state the behavior |
| "Various improvements" / "minor tweaks" | Name the improvements or drop the line |

The PR body is a record of change, not a launch announcement.

## Pillar 3 — Organization floor (predictable placement)

A reviewer should be able to predict which section holds a fact without scanning. The placement rules below apply universally; brand rules may add subsections but never reorder these foundations.

### 3.1 Fixed Pattern B subsection order

When Pattern B is in use, the five themed subsections appear in this exact order, with empty ones omitted entirely:

```
### Implementation
### Release          ← omit if not a release PR
### CI & Tooling     ← omit if empty
### Dependencies     ← omit if empty
### Docs             ← omit if empty
```

This order is mnemonic: source code first, version metadata next, then build pipeline, then external dependencies, then docs.

### 3.2 Tag ordering within every Changes subsection (both patterns)

Within a single Changes subsection (Pattern A's flat list or any of Pattern B's themed subsections), entries appear in this tag order:

1. `[NEW]` first (alphabetical by path within the group)
2. `[MOD]` second (alphabetical by path within the group)
3. `[DEL]` third (alphabetical by path within the group)
4. `[MOV]` fourth (alphabetical by old-path within the group)

This is universal across both Pattern A and Pattern B (changes-list.md previously scoped it to Pattern B only — the rule applies to both).

### 3.3 Bold inline group labels at 3+ entries (both patterns)

When 3 or more entries in any Changes subsection share a folder, group them under a bold inline label naming the subsystem + folder path. Below the threshold, the flat list is fine.

```markdown
### Implementation

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

**Vue 3 + Vite SSG bootstrap** (`src/`)
- **[MOD]** `src/main.js`, `src/App.vue` — vite-ssg entry, hydration boundary
- **[NEW]** `src/router.js` — vue-router 4 with `/` (EN) and `/es` (ES) routes

**UI primitives** (`src/components/ui/`)
- **[NEW]** `modal.vue` — controlled UiModal with body-lock
- **[NEW]** `image-viewer.vue` — shared chromeless lightbox
- **[NEW]** `hud-deco.vue` — corner labels + kanji watermarks
```

The group label is `**<Group name>** (\`<folder-path>\`)` on its own line. No bullet marker. Blank line above. Bullets of that group follow immediately.

Pattern A entries that share a path may also use group labels, with the same `**<Group name>** (\`<folder-path>\`)` syntax above the affected bullets. Below 3 entries, the path stays inline in `(path/to/folder/)` parens after the file name.

### 3.4 Multi-file entry merging (same fact, multiple files)

When the SAME change touches 2 or more files (typo fix across siblings, identical refactor, shared header bump), combine into one entry separated by commas:

```markdown
- **[MOD]** `src/i18n/index.js`, `src/i18n/detect-locale.js` — singleton-leak fix via `createI18nInstance`
```

The em-dash description applies to all files in the comma list. If the files require different descriptions, they're not the same fact — split into separate entries.

### 3.5 QA section group order (execution order)

`### <Feature group>` subheadings inside How-to-test appear in the order the tester would execute them. Canonical sequence for full-stack feature PRs:

1. **Setup** — install, precheck, environment
2. **Build / serve** — compile, start dev or preview server
3. **Feature flow A / B / C** — the actual feature paths under test
4. **Cross-cutting checks** — ADA, performance, regressions
5. **Deploy / release preview** — CI artifact, staging, post-deploy probe

Within each group, the numbered steps appear in chronological order of execution.

### 3.6 Documentation grouping (by target, then by media type)

`### <MEDIA-TYPE> — <Target>` blocks group by **Target** first, then by **Media type** within each target. The media-type order is fixed: `DESKTOP → TABLET → MOBILE → VIDEO → DIAGRAM → SCREENSHOT`.

```markdown
## Documentation

### DESKTOP — Landing per locale
> *EN and ES landings at 1440px.*
[asset]

### MOBILE — Landing per locale
> *EN and ES landings at 375px.*
[asset]

### VIDEO — YouTube facade flow
> *Consent, click-to-play, pause-on-carousel-change.*
[asset]

### DIAGRAM — JSON-LD graph
> *3-node @graph with FAQPage standalone.*
[asset]
```

## How brand rules interact with this floor

Brand rules retain three explicit hooks:

| Brand-rule hook | Effect on this floor |
|---|---|
| **Variant selection** (`TEST-SINGLE` vs `TEST-TWO-TABLE`, Pattern A vs B, etc.) | Picks which structural variant from `changes-list.md` / `supporting-sections.md` applies. The universal floor still applies inside the chosen variant. |
| **Content richness level** (`RICH` vs `MINIMAL`) | Controls *length* of cells and Expected lines. Does not weaken the column-naming, legend, metadata, prereqs, or grouping requirements. MINIMAL still includes legend if tags are used, still includes metadata lines, still includes prereqs blockquotes. |
| **Universal floor override** | A brand rule may opt out of a specific universal floor item via a top-of-rule line: `**Universal floor override:** <section number> — <reason>`. Without that explicit line, the universal floor applies. |

The default for any new brand rule is: variant selections + content richness level + the universal floor. The override is only for genuine, documented brand constraints — never an aesthetic preference.

## Pre-return sweep (universal-floor specific)

Run after the `global-writing-rules.md` sweep:

1. [ ] Tag legend blockquote present once if any `[TAG]` appears in the body
2. [ ] `**Test runner:**` + `**Command:**` metadata lines present above every test table
3. [ ] `> **Prereqs:**` blockquote present under every `### <Feature group>` in How-to-test
4. [ ] ASCII flow tree present when How-to-test has 4 or more groups
5. [ ] Every table has named column headers ending with `Status` (where applicable)
6. [ ] Every `### <MEDIA-TYPE> — <Target>` block has its italic context blurb
7. [ ] Every TD-4FIELD decision starts with a bold noun-phrase title
8. [ ] Every Changes entry is exactly one physical line for its top-level statement
9. [ ] Every `Covers` / `Key Coverage` cell is 3-8 phrase-style words, comma-separated
10. [ ] Every `***Expected:***` line names a concrete observable
11. [ ] Tag ordering within each subsection is `[NEW] → [MOD] → [DEL] → [MOV]`, alphabetical within tag
12. [ ] Bold inline group labels present in any Changes subsection with 3+ entries sharing a folder
13. [ ] Pattern B subsections appear in the fixed order; empty ones omitted
14. [ ] QA groups appear in execution order (Setup first)
15. [ ] Documentation media-type blocks grouped by Target, then by Media type in fixed order

Any unchecked item is fixed before return.

## Correct vs incorrect examples

### Example 1: Missing legend on Pattern A with tags

**Incorrect** — tags used without the legend caption:

```markdown
**Changes:**

- **[NEW]** **`Hero.vue`** (`src/sections/`):
  - first hero section
```

**Correct** — legend appears between heading and entries:

```markdown
**Changes:**

> **[NEW]** new file · **[MOD]** modified file · **[DEL]** removed · **[MOV]** renamed or relocated

- **[NEW]** **`Hero.vue`** (`src/sections/`):
  - first hero section
```

### Example 2: Anonymous-column test table

**Incorrect** — third column is decorative-only, has no header name:

```markdown
| Test file | Tests |  |
|---|---|---|
| `foo.test.js` | 12 | ✅ |
```

**Correct** — every column named, status column ends with `Status`:

```markdown
| Test file | Covers | Tests | Status |
|---|---|---|---|
| `foo.test.js` | URL parsing, ID extraction | 12 | ✅ |
```

### Example 3: Sentence-style Expected outcome

**Incorrect** — vague, not observable:

```markdown
1. Open the page
      ***Expected:*** Page loads correctly with no errors.
```

**Correct** — names concrete observables:

```markdown
1. Open the page
      ***Expected:*** `<html lang="en">`, hero LCP image preloads, scroll-progress bar updates with scroll.
```

### Example 4: Sentence-style decision title

**Incorrect** — title is a full sentence:

```markdown
- **We decided to use CSS-only masonry instead of JavaScript columns**
  - ***Chose:*** ...
```

**Correct** — noun phrase title:

```markdown
- **CSS-only masonry**
  - ***Chose:*** ...
```

### Example 5: Multi-line Changes entry

**Incorrect** — sprawling prose inside a Changes entry:

```markdown
- **[MOD]** `src/router.js` — Replaced the previous Vue Router 3 setup with Vue Router 4. We also added the new locale-aware route for `/es` because the SEO architecture requires per-locale prerendered HTML, and routes now resolve through the new `locale-from-route.js` helper which we added in this PR.
```

**Correct** — one line + nested sub-bullets if multiple facts:

```markdown
- **[MOD]** `src/router.js` — vue-router 4 with `/` (EN) and `/es` (ES) routes
  - resolves locale via `src/i18n/locale-from-route.js`
  - no trailing slash on any route
```

### Example 6: Missing Prereqs blockquote

**Incorrect** — feature group jumps straight to steps:

```markdown
### Landing flow per locale

1. Visit `/`
      ***Expected:*** ...
```

**Correct** — Prereqs blockquote sets the context:

```markdown
### Landing flow per locale

> **Prereqs:** dev server (`npm run dev`) running at `http://localhost:9000`.

1. Visit `/`
      ***Expected:*** ...
```
