<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the `eng-onboarding` skills session — creation of three MR-specific Claude Code skills (`dotcom-dev`, `cms-tools`, `code-review`) added to the `MadisonReed/eng-onboarding` repository, plus the corresponding frontmatter standardization of all dot-files skills. Loaded at the start of every conversation to give the AI full context without re-discovering anything.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, patterns, and conventions that apply to ALL work in this session. | Before any task. Mandatory constraints. |
| **2. Session Overview** | High-level context: scope, decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-skill detail: files, structure, decisions, current state. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference file path table. | When reading, editing, or locating files. |
| **5. Last Interaction** | Short-term memory: last work, pending, resume points. | At conversation start — entry point. |
| **6. Activity Log** | Datetime-stamped, append-only audit trail. | When you need exact "what was done when". |

**Operational Rule:** Always read Section 5 first. Load relevant skills and apply Section 1 rules before any task.

**Key principle:** Data may appear in multiple sections with different framing — Section 1 as a rule, Section 2 as scope context, Section 3 as implementation detail. This is intentional.

**Roam node:** `~/.brain.d/roam-nodes/madison_reed/2026-05-20-143000-eng-onboarding-skills.org`
(UUID: `a3f2e7c1-8b5d-4f9a-b6e3-d1c4082f7a56`) — contains `* COMMIT MSG` and `* PR DESCRIPTION`.

**Destination repo:** `MadisonReed/eng-onboarding` (local clone: `/Volumes/dev-partition/github-madison-reed/eng-onboarding/`)

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `skill-architect` (skill structure), `mr-roam-node` (documentation), `session-memory` (architecture extraction), `pr-scribe` (PR authoring). Section 1 stores session-scoped patterns not yet promoted into those skills — staging area for knowledge that may graduate later.

### 1.1 Skill Directory Structure (Two-Agent Model)

Every skill follows this exact layout:

```
skill-name/
├── SKILL.md         frontmatter + command interface + help ASCII box
│                    + quick reference table + rule details below table
│                    + companion skills
├── AGENTS.md        why the skill exists, two-agent model rationale,
│                    rule loading strategy, when NOT to apply
├── config/
│   └── defaults.json  path resolution (env → config file → default),
│                       service defaults, dev commands, access URLs
├── rules/           atomic knowledge units (one topic per file)
│   └── *.md         frontmatter: title, impact, impactDescription, tags
├── references/      abstracted real-world examples and workflows
│   └── *.md         patterns, troubleshooting, common findings
└── scripts/         executable Node.js or shell scripts
    └── lib/         shared utilities (e.g., mongo.mjs wrapper)
```

`references/` and `scripts/` are only present when the skill needs them.

### 1.2 SKILL.md Frontmatter Pattern (openclaw)

```yaml
name: skill-name
description: >-
  Keyword-dense paragraph for Skill Analyzer semantic matching.
  Cover all major concepts, class names, function names, and APIs.
user-invocable: true
metadata:
  openclaw:
    emoji: 🖥️
    os: [darwin, linux]
    requires:
      bins: [node, npm]
```

**Rules:**
- `user-invocable: true` is mandatory on every skill.
- `metadata.openclaw.emoji` — one emoji representing the skill domain.
- `metadata.openclaw.os` — restrict to the platforms where the skill applies.
- `metadata.openclaw.requires.bins` — list binaries the user needs installed. Omit `requires` entirely when no binaries are needed.
- **Never include** `metadata.author`, `metadata.version`, or `metadata.team` — personal metadata has no role in skill routing or execution.
- Description format: always `>-` multi-line YAML, never single-line quoted string.

### 1.3 Portable Path Variable Pattern (from bootstrap PR #1)

Every bash block in a skill must begin with:
```bash
: "${MR_REPO_PATH:=$HOME/workarea/mr}"
: "${MR_SKILL_REPO_PATH:=$HOME/workarea/eng-onboarding}"
```
Resolution order: **env var → `~/.claude/mr-eng-onboarding-config.json` → default**.
Never hardcode developer home directories.

### 1.4 Conditional Prerequisites Discipline

Operational skills (like `cms-tools`) must distinguish between always-required and context-specific services. Stating everything as "required" creates unnecessary friction.

| Class | Definition | Display in status mode |
|---|---|---|
| Always required | Needed for every script invocation | `❌ STOPPED` if absent |
| Conditional | Needed only for specific operations | `— STOPPED (start only if…)` |

`cms-tools` example: MongoDB Docker = always required. Tophat (port 4000) = only when editing in Tophat UI. Website (port 3000) = only for `inspect-jsonld`.

### 1.5 Rule Selection — Not Project Detection

For MR-only skills, the project is always Madison Reed — there is nothing to detect. Use the framing "rules are selected based on which files changed" instead of "project detection."

- `code-review` framing: `website/src/vuescripts/` → frontend rules load; `mr_modules/` → backend rules load.
- `detection/` directory in `code-review/` is a legacy name; it functions as a rule-routing signal table only.
- Never use "two-tier detection," "project detection," or "brand detection" in MR-only skills.

### 1.6 Table Content Discipline

- Table cells: ≤ 100 characters.
- Long descriptions go in a named bullet list **immediately below the table**, not inside cells.
- Pattern: table rows are short labels/summaries; prose details follow as "Rule details:" or "Notes:".

### 1.7 Kyonax Removal Discipline

All skills in `eng-onboarding` must be MR-only. Before shipping any skill:
- `grep -r "kyonax\|Kyonax"` across SKILL.md, AGENTS.md, all rule files, detection files.
- Replace `kyonax.com` URLs with `example.com`; `@kyonax_on_tech` handles with `@yourbrand`.
- Remove `kyonax` rows from RULE_TEMPLATE.md category tables and detection signal tables.

### 1.8 Installation Pattern

```bash
cp -r <skill-dir> ~/.claude/skills/<skill-name>
```

Skills live in `eng-onboarding/` and are copied to `~/.claude/skills/` per engineer. `install.sh` should include `cp -r` entries for all skills. No symlinks in eng-onboarding — always copy. (dot-files skills use symlinks — that's separate.)

### 1.9 dot-files Skill Naming Convention

Skills in the dot-files repo follow these naming rules as of 2026-05-20:
- **No `mr-` prefix on generalized MR skills** — `dotcom-dev` (not `mr-dotcom-dev`), `cms-tools` (not `tophat-tools`).
- **Keep context prefix only when disambiguation is required** — `mr-roam-node` stays because `reckit-roam-node` exists in the same repo.
- **Directory name must match the `name:` field** in SKILL.md frontmatter.
- **Symlinks in `~/.claude/skills/`** must use the new canonical names after any rename.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Add three MR-only Claude Code skills to `MadisonReed/eng-onboarding`, following the two-agent skill architecture and the portable path-variable pattern established by bootstrap PR #1. Additionally standardize all dot-files skills with the openclaw frontmatter pattern and correct naming conventions.

### 2.2 Scope

| Deliverable | Files | Status |
|---|---|---|
| `dotcom-dev` skill (eng-onboarding) | 15 | ✅ Done — on `main` |
| `cms-tools` skill (eng-onboarding) | 40 | ✅ Done — on `main` |
| `code-review` skill (eng-onboarding) | 171 | ✅ Done — on `main` |
| `README.md` update (eng-onboarding) | 1 | ✅ Done — on `main` |
| Roam node + COMMIT MSG + PR DESCRIPTION | 1 | ✅ Done |
| dot-files SKILL.md frontmatter (all 11) | 11 | ✅ Done |
| dot-files `git mv` renames + symlinks | — | ✅ Done |
| Memory files updated | 3 | ✅ Done |
| Session file | 1 | ✅ This file |
| PR — `feat/add-mr-dev-skills` branch | — | ⏳ Branch ready, needs push + PR open |
| `install.sh` update (eng-onboarding) | 1 | ⏳ Pending |
| Smoke-test after install | — | ⏳ Pending |

**Note on git state (2026-05-20):** Commit `1b42972` was pushed directly to `origin/main` without a PR. Branch `feat/add-mr-dev-skills` was created from `bbd2d73` (parent commit) with the feat cherry-picked as `52dc170`. Push + PR open still pending.

### 2.3 Key Decisions

1. **(2026-05-20) DEC-001 — Destination: eng-onboarding repo.** Skills live in `MadisonReed/eng-onboarding`, not dot-files. MR org = MR skills. Installation follows same `cp -r` pattern as existing skills.

2. **(2026-05-20) DEC-002 — Portable path variables.** Adopted from bootstrap PR #1. Every bash block uses `${MR_REPO_PATH}` / `${MR_SKILL_REPO_PATH}` shims. No hardcoded developer paths.

3. **(2026-05-20) DEC-003 — MR-only scope, no detection tier.** `code-review` uses rule selection by changed file paths, not multi-project/brand detection. The `detection/` directory is a routing table, not detection logic.

4. **(2026-05-20) DEC-004 — cms-tools conditional prerequisites.** MongoDB Docker always required. Tophat and website are conditional — `—` in status mode, not `❌`.

5. **(2026-05-20) DEC-005 — references/ directories.** Each skill includes abstracted real-world examples from production sessions. Personal/local details removed.

6. **(2026-05-20) DEC-006 — Kyonax branding fully removed.** All Kyonax URLs, handles, detection rules removed. Replaced with `example.com` generics.

7. **(2026-05-20) DEC-007 — code-review rule count 132 (was 153).** `brand/kyonax/` directory (21 rules) dropped.

8. **(2026-05-20) DEC-008 — openclaw frontmatter standardized across all dot-files skills.** All 11 SKILL.md files: `user-invocable: true` added, `metadata.openclaw` (emoji + os + requires.bins) added, `metadata.author`/`version`/`team` removed, descriptions converted to `>-` multi-line YAML.

9. **(2026-05-20) DEC-009 — dot-files skill renames.** `mr-dotcom-dev` → `dotcom-dev` (directory + name field + symlink). `tophat-tools` → `cms-tools` (directory + name field + symlink). No `mr-` prefix on generalized MR skills.

### 2.4 Pending Work

- [ ] Push `feat/add-mr-dev-skills` and open PR (body is in roam node `* PR DESCRIPTION`).
- [ ] Update `install.sh` — add 3 `cp -r` entries after existing bootstrap + skill entries.
- [ ] Smoke-test: install skills, verify `/dotcom-dev`, `/cms-tools --help`, `/code-review` load.
- [ ] Update roam node task checkboxes when PR is created.

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 dotcom-dev (eng-onboarding)

**Created:** 2026-05-20 | **Status:** ✅ on `main`

**Purpose:** Frontend and fullstack standards reference for `website/`. Covers Pug, Vue 3 Options API, Vuex 4, Stylus, Express routing, SSR, DynamicYield, and Vitest.

**Command interface:** `/dotcom-dev [vue|store|template|styles|test|ssr|routing|dy]`

**Rules (10):** vue-patterns, pug-templates, utility-classes, spacing-utilities, typography-utilities, flexbox-layout, ssr-architecture, express-routing, dynamic-yield, testing-standards.

**References (2):** `component-patterns.md` (extraction, role="link" cards, cross-page hand-off, modal dispatch, CMS seeding, aria-label composition), `third-party-integration.md` (SDK DOM sibling constraint, lifecycle limits, IIFE wrap, localStorage suppression, svh viewport safety).

### 3.2 cms-tools (eng-onboarding)

**Created:** 2026-05-20 | **Status:** ✅ on `main`

**Purpose:** Operational toolkit for Tophat CMS (MongoDB-backed). Inspect, audit, and mutate via structured Node.js scripts with dry-run + backup safety rails.

**Command interface:** `/cms-tools [inspect <uri|id> | experiment <id|name> | jsonld <uri> | migrate | partial <mixin_key> | --status | --help]`

**Rules (9):** cms-data-model, template-field-schema, inspection-scripts, experiment-management, json-ld-management, content-migration, code-locator-scripts, partials, safety-and-conventions.

**Scripts (25):** `lib/mongo.mjs` (shared wrapper) + inspect (11), mutate (10), code-locator (3). All writes require `--confirm`.

**References (2):** `investigation-workflows.md` (6 recipes), `troubleshooting.md` (7 failure modes).

**MCP channels:** `tophat-cms` (localhost-gated CMS API CRUD), `log-watcher` (dev-server error stream).

### 3.3 code-review (eng-onboarding)

**Created:** 2026-05-20 | **Status:** ✅ on `main`

**Purpose:** Quality audit for the MR monorepo. 132 atomic rules, parallel Sonnet workers. Rules selected by which MR files changed — no project detection.

**Command interface:** `/code-review [PR #N | audit PR #N | --ada | --seo | --styles | --vue | --help]`

**Rule inventory:** ada (25), seo (13), script (9), code-style (6), mobile (5), third-party-sdk (5), vue3 (11), vue3-composition (3), express (3), mr-dotcom (40), mr-backend (12) = **132 total**.

**Scripts (16):** Pre-AI pipeline (8 scripts: detect through worker-prompt-builder), Post-AI (2: findings-dedup, format-findings), Analysis (6: diff-context, component-tree, lint-changed, test-changed, ci-local, pr-fetch).

**Reference (1):** `common-findings.md` — 15 recurring violations (5 ADA, 3 Vue 3, 4 styling, 2 script, 2 SEO).

### 3.4 dot-files Skills Standardization

**Updated:** 2026-05-20 | **Status:** ✅ Complete

All 11 dot-files skills updated in one pass. Changes applied uniformly:

| Was | Now |
|---|---|
| `metadata.author: X` | Removed |
| `metadata.version: "N.N.N"` | Removed |
| `metadata.team: X` | Removed |
| *(missing)* | `user-invocable: true` |
| *(missing)* | `metadata.openclaw` block (emoji, os, requires.bins) |
| Single-line quoted description | `>-` multi-line YAML block |

**Name field and directory changes:**

| Old directory | New directory | name: field change |
|---|---|---|
| `skills/mr-dotcom-dev/` | `skills/dotcom-dev/` | `mr-dotcom-dev` → `dotcom-dev` |
| `skills/tophat-tools/` | `skills/cms-tools/` | `tophat-tools` → `cms-tools` |

**Symlinks updated** (`~/.claude/skills/`):
- `mr-dotcom-dev` → `dotcom-dev` (→ `dot-files/.../skills/dotcom-dev`)
- `tophat-tools` → `cms-tools` (→ `dot-files/.../skills/cms-tools`)

**Memory files updated:** `MEMORY.md` (symlink entries), `reference_tophat_tools_skill.md` (renamed to `cms-tools`), `feedback_no_local_skills.md` (`/mr-dotcom-dev` → `/dotcom-dev`).

---

## SECTION 4: FILE INDEX

### eng-onboarding Skills (New — on `main`)
| Path | Contents |
|---|---|
| `/Volumes/dev-partition/github-madison-reed/eng-onboarding/dotcom-dev/` | Frontend dev skill (15 files) |
| `/Volumes/dev-partition/github-madison-reed/eng-onboarding/cms-tools/` | CMS tools skill (40 files) |
| `/Volumes/dev-partition/github-madison-reed/eng-onboarding/code-review/` | Code review skill (171 files) |
| `/Volumes/dev-partition/github-madison-reed/eng-onboarding/README.md` | Updated — skills table, install block, repo tree |

### dot-files Skills (Modified)
| Path | Change |
|---|---|
| `.../skills/dotcom-dev/SKILL.md` | Renamed from `mr-dotcom-dev/`; openclaw frontmatter added |
| `.../skills/cms-tools/SKILL.md` | Renamed from `tophat-tools/`; openclaw frontmatter added |
| `.../skills/code-review/SKILL.md` | openclaw frontmatter added; description reformatted |
| `.../skills/emacs-expert/SKILL.md` | openclaw frontmatter added |
| `.../skills/mr-roam-node/SKILL.md` | openclaw frontmatter added; COMMIT MSG + PR DESCRIPTION noted |
| `.../skills/pr-scribe/SKILL.md` | openclaw frontmatter added; author removed |
| `.../skills/reckit-roam-node/SKILL.md` | openclaw frontmatter added |
| `.../skills/seo-web-quality/SKILL.md` | openclaw frontmatter added; description enriched |
| `.../skills/session-memory/SKILL.md` | openclaw frontmatter added; @kyonax_on_tech removed |
| `.../skills/session-reset/SKILL.md` | openclaw frontmatter added; description enriched |
| `.../skills/skill-architect/SKILL.md` | openclaw frontmatter added; @kyonax_on_tech removed; description enriched |

### Session Documents
| Path | Contents |
|---|---|
| `~/.brain.d/roam-nodes/madison_reed/2026-05-20-143000-eng-onboarding-skills.org` | Roam node — UUID `a3f2e7c1-8b5d-4f9a-b6e3-d1c4082f7a56`, COMMIT MSG + PR DESCRIPTION |
| `~/.claude/projects/.../memory/MEMORY.md` | Skill symlink entries updated |
| `~/.claude/projects/.../memory/reference_tophat_tools_skill.md` | Renamed to reference `cms-tools` |
| `~/.claude/projects/.../memory/feedback_no_local_skills.md` | `/mr-dotcom-dev` → `/dotcom-dev` |
| This file | Session context block |

### Git State (eng-onboarding)
| Branch | Commit | Status |
|---|---|---|
| `main` (remote) | `1b42972` — feat: add dotcom-dev, cms-tools, and code-review skills | Pushed |
| `feat/add-mr-dev-skills` | `52dc170` — cherry-pick of `1b42972` | Local only — needs push |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last (2026-05-20)

- All 11 dot-files `SKILL.md` files updated: `user-invocable: true` added, `metadata.openclaw` added (emoji + os + requires.bins), author/version/team metadata removed, descriptions converted from single-line quoted strings to `>-` multi-line YAML, thin descriptions enriched (seo-web-quality, skill-architect, emacs-expert).
- `mr-dotcom-dev` → `dotcom-dev`: `name:` field updated, `git mv` executed, `~/.claude/skills/` symlink replaced.
- `tophat-tools` → `cms-tools`: `name:` field updated, `git mv` executed, `~/.claude/skills/` symlink replaced.
- Memory files updated (`MEMORY.md`, `reference_tophat_tools_skill.md`, `feedback_no_local_skills.md`) to reflect new skill names.
- Session reset performed (this file).

### Pending / Not yet started

- **Push branch + open PR:** `git push -u origin feat/add-mr-dev-skills`, then paste PR body from roam node `* PR DESCRIPTION` into `gh pr create`.
- **install.sh update:** Add 3 `cp -r` entries to `/Volumes/dev-partition/github-madison-reed/eng-onboarding/install.sh`.
- **Smoke-test:** `cp -r` all 3 skills, verify `/dotcom-dev`, `/cms-tools --help`, `/code-review` load in a Claude Code session inside the MR repo.
- **Roam node tasks:** Check off completed items.

### Where to resume

If asked to **push and open the PR**: `git push -u origin feat/add-mr-dev-skills` then copy the PR body from `* PR DESCRIPTION` in the roam node into `gh pr create`.

If asked to **update install.sh**: read `/Volumes/dev-partition/github-madison-reed/eng-onboarding/install.sh`, add after existing entries:
```bash
cp -r dotcom-dev  ~/.claude/skills/dotcom-dev
cp -r cms-tools   ~/.claude/skills/cms-tools
cp -r code-review ~/.claude/skills/code-review
```

If asked to **update a dot-files skill**: apply Section 1.2 frontmatter pattern (openclaw), 1.9 naming convention, and 1.6 table discipline.

If asked for a **new task**: check Section 2.4 for pending items; otherwise treat as a fresh request.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table. Newest row first.

| Datetime         | Duration | Type           | Reference | Description |
|------------------+----------+----------------+-----------+-------------|
| 2026-05-20 16:30 | —        | session-reset  | this      | Second compaction — dot-files skill updates, renames, symlinks, memory files |
| 2026-05-20 16:20 | 0.25h    | configuration  | this      | Memory files updated (MEMORY.md, reference_tophat_tools_skill, feedback_no_local_skills) |
| 2026-05-20 16:10 | 0.25h    | configuration  | this      | git mv mr-dotcom-dev→dotcom-dev, tophat-tools→cms-tools; symlinks replaced |
| 2026-05-20 15:45 | 0.5h     | refinement     | this      | All 11 dot-files SKILL.md frontmatter standardized — openclaw, user-invocable, descriptions, removed author/version |
| 2026-05-20 15:30 | —        | session-reset  | this      | First compaction — 3 eng-onboarding skills shipped, branch ready, PR body in roam node |
| 2026-05-20 15:15 | 0.25h    | configuration  | this      | Created feat/add-mr-dev-skills branch from bbd2d73, cherry-picked 1b42972 as 52dc170 |
| 2026-05-20 15:00 | 0.25h    | documentation  | this      | Added * COMMIT MSG + * PR DESCRIPTION to roam node; PR body complete |
| 2026-05-20 14:45 | 0.25h    | refinement     | this      | Final rule-selection-not-detection language pass — SKILL.md, AGENTS.md, detect.sh |
| 2026-05-20 14:30 | 0.25h    | documentation  | this      | Roam node + initial session file created; MEMORY.md updated |
| 2026-05-20 11:00 | 3.5h     | implementation | this      | Built 3 eng-onboarding skills: dotcom-dev (15), cms-tools (40), code-review (171). Bootstrap PR #1 patterns, Kyonax purge, table discipline, conditional prerequisites, README update. |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
