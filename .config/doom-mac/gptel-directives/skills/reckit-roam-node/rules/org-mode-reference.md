---
title: Org-Mode Formatting Reference — Syntax Rules for Programmatic Plan Generation
impact: HIGH
impactDescription: Org-mode has strict syntax rules for emphasis, property drawers, lists, checkboxes, tables, and code blocks. Violating these produces broken rendering in Emacs — invisible text, broken links, disconnected formulas, unindexed nodes.
tags: org-mode, syntax, emphasis, bold, italic, verbatim, code, property-drawer, list, checkbox, statistics-cookie, tags, table, tblfm, code-block, src, quote, org-roam, cross-reference, id-link, comment, pitfall, pre-post, rendering, emacs
---

This rule is the authoritative reference for org-mode syntax that affects programmatic plan-node generation. Every node produced by this skill is an `.org` file rendered in Emacs (Doom Emacs config). Violating org-mode syntax produces broken rendering — invisible text (bad emphasis), unindexed nodes (wrong property drawer), disconnected formulas (blank line before `#+TBLFM:`), or split lists (blank lines between items).

## File-Level Node Structure (Org-Roam)

The `:PROPERTIES:` drawer **must come first** in the file (zeroth section), before any `#+` keywords:

```org
:PROPERTIES:
:ID:       uuid-here
:END:
#+TITLE: ...
#+SUBTITLE: ...
```

**Never** put `#+TITLE:` before `:PROPERTIES:` — org-roam will not index the node.

## Emphasis Markers (PRE/POST Rules)

Emphasis (`*bold*`, `/italic/`, `=verbatim=`, `~code~`, `+strikethrough+`, `_underline_`) follows strict boundary rules:

- **Opening marker** must be preceded by: whitespace, `-`, `(`, `{`, `'`, `"`, or start of line.
- **Closing marker** must be followed by: whitespace, `-`, `.`, `,`, `;`, `:`, `!`, `?`, `'`, `)`, `}`, `[`, `"`, `\`, or end of line.
- **Contents** cannot begin or end with whitespace.

`*GIVEN*` works in `- *GIVEN* I am on a page,` because the `*` markers are surrounded by spaces. But `un*frigging*believable` will NOT bold — `n` is not a valid PRE character.

**Convention in RECKIT plan nodes:** Use `*bold*` for emphasis and requirement keywords. Use `=verbatim=` for code references (file paths, component names like `=<UiStatusDot>=`, CSS classes, variable names, npm scripts). Do NOT use `~code~` — RECKIT consistently uses `=verbatim=`.

## Property Drawer Placement

- **File-level:** First element in the file, before any content or keywords.
- **Headline-level:** Immediately after the headline (and optional DEADLINE/SCHEDULED line) — no blank lines between headline and drawer.
- Property drawers **cannot be nested** and **cannot contain blank lines**.

## List Continuity

- Items at the same indentation with no blank lines between them form a **single list**.
- A blank line between items **splits them into separate lists**.
- Nested lists use 2-space indentation per level.
- Bullet characters: `-` (preferred), `+`, or `*` (only at non-zero indentation to avoid confusion with headlines).
- Ordered lists: `1.`, `2.` etc.
- Description lists: `- Term :: Definition`.

**For REQUIREMENTS lists:** Keep all R-items and their GIVEN/WHEN/THEN sub-items in a continuous block without extra blank lines between items of the same R. One blank line between separate R groups is acceptable.

## Checkbox Syntax

Exact formatting required — no variations:
- Unchecked: `- [ ]` (dash, space, open-bracket, space, close-bracket).
- Checked: `- [X]` (uppercase X only).
- Partial: `- [-]`.

**Statistics cookies** (`[0/N] [0%]`) placed in the headline **only count direct children**, not deeply nested checkboxes. For nested task structures, each sub-group heading needs its own counter:

```org
* TODO PLAN TASKs [0/3] [0%]
** Phase 1 — Org Parser [0/2]
   - [ ] Build src/shared/utils/org.js
   - [ ] Add org.test.js
** Phase 2 — Channel [0/1]
   - [ ] Build use-context-channel.js
```

## Tags

- Tags go at the end of headlines: `* HEADING :tag1:tag2:`.
- Valid characters: letters, numbers, `_`, `@`, `#`, `%` — **no hyphens or spaces**.
- File-level tags: `#+FILETAGS: :RECKIT:PLAN:`.
- Tags are inherited by sub-headings.

## Tables

```org
| Column 1 | Column 2 | Column 3 |
|----------+----------+----------|
| data     | data     | data     |
```

- Rule rows use `|---+---+---|` (pipe-dash for row start, plus for column separators).
- `#+TBLFM:` formulas **must immediately follow** the table with no blank line.
- Alignment: `<l>` left, `<c>` center, `<r>` right (placed in a row below the header).

## Code Blocks

```org
#+begin_src javascript
const example = true;
#+end_src
```

- Lines starting with `*` or `#+` inside src/example blocks must be **comma-prefixed**: `,*`, `,#+`.
- Org strips the leading comma on access — use this when documenting org-mode syntax inside an org-mode example.
- Use `#+begin_quote` / `#+end_quote` for quotations.
- Common languages used in RECKIT plans: `javascript`, `js`, `vue`, `scss`, `bash`, `txt` (for ASCII trees), `org` (for org-syntax examples).

## Org-Roam Cross-References

```org
[[id:cabd1489-23cc-4ce3-9825-7b5a8eb065b9][Plan #context-screen]]
```

- Always use `id:UUID` links (not file links) — these survive file renames.
- Backlinks are automatically tracked by the org-roam SQLite database.
- When referencing another RECKIT plan node, search `~/.brain.d/roam-nodes/reckit/` for the target plan's `:ID:` property.
- The Index UUID is `93c9b466-676d-48bf-9d1b-ec8b93816b5d` — use this in every plan's `RELEVANT LINKs`.

## External Links

```org
[[https://github.com/Kyonax/reckit/tree/<branch>][Branch <branch-name>]]
[[https://github.com/Kyonax/reckit/pull/<N>][PR #<N>]]
```

These are HTTP links, not org-roam ID links. Use them for GitHub URLs, MDN docs, etc.

## Session-File References

The canonical session file lives outside org-roam (it's a `.md` file in the dot-files repo). Reference by **path + section number**, not by org-link:

```org
*Session file:* =dot-files/.config/doom-mac/gptel-directives/sessions/kyo-recording-automation.md=
  - §1.14 Performance Budget for OBS Browser Sources
  - §3.8 @kyonax_on_tech Brand
```

Inline references inside paragraphs use ``§1.14`` (backtick-quoted, no link). Decision references use `decision #132` (plain text). The reader knows where the session file lives.

## Comments

- Line comments: `# This is a comment` (hash at column 0, followed by space).
- Comment blocks: `#+begin_comment` / `#+end_comment`.
- Comments are **not exported** — safe to use for `# SOURCE:` annotations in templates.

## Common Pitfalls

| Pitfall                              | Problem                                                                                        | Fix                                     |
|--------------------------------------|------------------------------------------------------------------------------------------------|-----------------------------------------|
| Verbatim/code cannot nest markup     | `=*not bold inside verbatim*=` renders literally                                               | Keep emphasis and verbatim separate     |
| No blank line before property drawer | A blank line between a headline and its `:PROPERTIES:` drawer breaks the drawer                | Place drawer immediately after headline |
| Emphasis at word boundaries          | `/path/to/file/` will NOT italicize — the inner `/` chars break parsing                        | Use `=verbatim=` for file paths         |
| Statistics cookie scope              | `[0/7]` on a headline with 7 nested grandchildren but only 3 direct children will show `[0/3]` | Add sub-counters on intermediate items  |
| Table formula placement              | A blank line between a table and its `#+TBLFM:` disconnects the formula                        | No blank line between table and formula |
| Hyphen inside a tag                  | `:reckit-plan:` is invalid — hyphens forbidden in tag chars                                    | Use underscores: `:RECKIT_PLAN:` or split into two tags `:RECKIT:PLAN:` |
| Asterisk at column 0 inside src      | Renders as a real headline, breaks the code block                                              | Comma-prefix: `,*` (org strips on access) |

## RECKIT-Specific Conventions

- File-level tags: `:RECKIT:PLAN:` / `:RECKIT:BUG:` / `:RECKIT:RELEASE:` / `:RECKIT:INDEX:` / `:RECKIT:ARCHITECTURE:` / `:RECKIT:CONVENTIONS:`.
- Decision IDs in plan nodes use the prefix `D` (`*D1 — ...*`, `*D2 — ...*`).
- Requirement IDs use the prefix `R` (`*R1 — ...*`).
- Open question IDs use the prefix `Q` (`*Q1 — ...*`).
- Phase headings use `** Phase N — <Name>` under `* TODO PLAN TASKs`.
- Cross-link to architecture / conventions / index in every plan's `RELEVANT LINKs`.
