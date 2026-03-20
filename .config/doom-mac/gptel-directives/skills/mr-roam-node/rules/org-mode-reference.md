---
title: Org-Mode Formatting Reference — Syntax Rules for Programmatic Node Generation
impact: HIGH
impactDescription: Org-mode has strict syntax rules for emphasis, property drawers, lists, checkboxes, tables, and code blocks. Violating these produces broken rendering in Emacs — invisible text, broken links, disconnected formulas, and unindexed nodes.
tags: org-mode, syntax, emphasis, bold, italic, verbatim, code, property-drawer, list, checkbox, statistics-cookie, tags, table, tblfm, code-block, src, quote, org-roam, cross-reference, id-link, comment, pitfall, pre-post, rendering, emacs
---

This rule is the authoritative reference for org-mode syntax rules that affect programmatic node generation. Every node produced by this skill is an `.org` file rendered in Emacs. Violating org-mode syntax rules produces broken rendering — invisible text (bad emphasis markers), unindexed nodes (wrong property drawer placement), disconnected formulas (blank line before `#+TBLFM:`), or split lists (blank lines between items). This reference must be consulted whenever generating or modifying org-mode content.

## File-Level Node Structure (Org-Roam)

The `:PROPERTIES:` drawer **must come first** in the file (zeroth section), before any `#+` keywords. This is the org-roam file-level node convention:

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

- **Opening marker** must be preceded by: whitespace, `-`, `(`, `{`, `'`, `"`, or start of line
- **Closing marker** must be followed by: whitespace, `-`, `.`, `,`, `;`, `:`, `!`, `?`, `'`, `)`, `}`, `[`, `"`, `\`, or end of line
- **Contents** cannot begin or end with whitespace

This means `*GIVEN*` works in `- *GIVEN* I am on a page,` because the `*` markers are surrounded by spaces. But `un*frigging*believable` will NOT bold — `n` is not a valid PRE character.

**Convention in MR nodes:** Use `*bold*` for emphasis and AC keywords. Use `=verbatim=` for code references (component names like `=HcbLocationPageV2=`, CSS classes, variable names). Do NOT use `~code~` — the codebase consistently uses `=verbatim=` for code references.

## Property Drawer Placement

- **File-level:** First element in the file, before any content or keywords
- **Headline-level:** Immediately after the headline (and optional DEADLINE/SCHEDULED line) — no blank lines between headline and drawer
- Property drawers **cannot be nested** and **cannot contain blank lines**

## List Continuity

- Items at the same indentation with no blank lines between them form a **single list**
- A blank line between items **splits them into separate lists**
- Nested lists use 2-space indentation per level
- Bullet characters: `-` (preferred), `+`, or `*` (only at non-zero indentation to avoid confusion with headlines)
- Ordered lists: `1.`, `2.` etc.
- Description lists: `- Term :: Definition`

**For AC lists:** Keep all AC items and their GIVEN/WHEN/THEN sub-items in a continuous block without extra blank lines between items of the same AC. One blank line between separate AC groups is acceptable.

## Checkbox Syntax

Exact formatting required — no variations:
- Unchecked: `- [ ]` (dash, space, open-bracket, space, close-bracket)
- Checked: `- [X]` (uppercase X only)
- Partial: `- [-]`

**Statistics cookies** (`[0/N] [0%]`) placed in the headline **only count direct children**, not deeply nested checkboxes. For nested task structures, each sub-group heading needs its own counter:

```org
* TODO TICKET TASKs [0/3] [0%]
  - [ ] Component Scaffolding [0/2]
    - [ ] Create ComponentA.vue
    - [ ] Create ComponentB.vue
  - [ ] Styling [0/1]
    - [ ] Apply design tokens
  - [ ] Testing [0/1]
    - [ ] Write unit tests
```

## Tags

- Tags go at the end of headlines: `* HEADING :tag1:tag2:`
- Valid characters: letters, numbers, `_`, `@`, `#`, `%` — **no hyphens or spaces**
- File-level tags: `#+FILETAGS: :MR:TICKET:`
- Tags are inherited by sub-headings

## Tables

```org
| Column 1 | Column 2 | Column 3 |
|----------+----------+----------|
| data     | data     | data     |
```

- Rule rows use `|---+---+---|` (pipe-dash for row start, plus for column separators)
- `#+TBLFM:` formulas **must immediately follow** the table with no blank line
- Alignment: `<l>` left, `<c>` center, `<r>` right (placed in a row below the header)

## Code Blocks

```org
#+begin_src javascript
const example = true;
#+end_src
```

- Lines starting with `*` or `#+` inside src/example blocks must be **comma-prefixed**: `,*`, `,#+`
- Org strips the leading comma on access
- Use `#+begin_quote` / `#+end_quote` for quotations

## Org-Roam Cross-References

```org
[[id:51b7b82c-bbb4-4822-875a-ed548cffda10][Display Text]]
```

- Always use `id:UUID` links (not file links) — these survive file renames
- Backlinks are automatically tracked by the org-roam SQLite database
- When referencing another MR ticket node, search `~/.brain.d/roam-nodes/madison_reed/` for the target ticket's `:ID:` property

## Comments

- Line comments: `# This is a comment` (hash at column 0, followed by space)
- Comment blocks: `#+begin_comment` / `#+end_comment`
- Comments are **not exported** — safe to use for source annotations in templates

## Common Pitfalls

| Pitfall                              | Problem                                                                                        | Fix                                     |
|--------------------------------------|------------------------------------------------------------------------------------------------|-----------------------------------------|
| Verbatim/code cannot nest markup     | `=*not bold inside verbatim*=` renders literally                                               | Keep emphasis and verbatim separate     |
| No blank line before property drawer | A blank line between a headline and its `:PROPERTIES:` drawer breaks the drawer                | Place drawer immediately after headline |
| Emphasis at word boundaries          | `/path/to/file/` will NOT italicize — the inner `/` chars break parsing                        | Use `=verbatim=` for file paths         |
| Statistics cookie scope              | `[0/7]` on a headline with 7 nested grandchildren but only 3 direct children will show `[0/3]` | Add sub-counters on intermediate items  |
| Table formula placement              | A blank line between a table and its `#+TBLFM:` disconnects the formula                        | No blank line between table and formula |
