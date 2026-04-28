---
name: reckit-roam-node
description: "Generate and manage org-roam .org files for the RECKIT project (Kyonax/reckit) in ~/.brain.d/roam-nodes/reckit/. Creates consistent, structured org-mode plan, bug, and release nodes that document development plans, decisions, progress, and findings — without JIRA. State comes from git (branch, commits) and gh (PRs, tags) instead. Three template variants: Standard Plan (Feature), Bug Plan (debugging diary), Release Plan (triad + scope). Includes PROPERTIES block with UUID, metadata headers (TITLE, SUBTITLE, AUTHOR, EMAIL, DATE, FILETAGS, SOURCE_URL, LAST_UPDATE, EFFORT, TARGET_RELEASE, BRANCH, STATUS), TABLE OF CONTENTS, scoped REQUIREMENTS as GIVEN/WHEN/THEN, DESIGN AND ARCHITECTURE with DECISIONS log, phased TODO PLAN TASKs, DOCUMENTATION (STRUCTURE/DEPLOYMENT/TESTS/QA/FINDINGS), DELIVERABLEs, COMMENTs. Manages the two-layer index (BACKLOG with org-roam links + PLAN BOARD with 6 lanes: IN PLANNING / IN DEVELOPMENT / IN REVIEW / IN TEST / ALL RELEASED / SHELVED). Trigger when: user says 'create plan', 'create plan node', 'create reckit plan', 'roam plan', 'plan node', 'create plan for <slug>', 'update plan', 'reckit plan', 'log decision in plan', 'lock decision', 'plan board', 'reckit index', 'update reckit index', 'sync reckit index', 'create bug node for reckit', 'create release node for reckit', 'plan retrospective', 'fill findings'."
metadata:
  author: Kyonax
  version: "1.0.0"
---

# Reckit Roam Node Skill

Generates consistent org-roam `.org` files for the RECKIT project (`Kyonax/reckit`). Each node serves as structured documentation that tracks scope, decisions, phased progress, and post-implementation findings — and feeds the two-layer index dashboard. Mirrors the proven `mr-roam-node` skill architecture, but driven by **git + gh** instead of JIRA, and uses **plans** (self-driven units of work) instead of tickets.

## When to Apply

Reference these guidelines when:

- Creating a new plan node from scratch (Feature, Bug, or Release).
- Updating an existing plan with development progress, locked decisions, findings.
- Resolving an OPEN QUESTION → promoting it to a DECISION in the same node.
- Filling Phase 2 sections (DOCUMENTATION → STRUCTURE / DEPLOYMENT / TESTS / QA / FINDINGS).
- Cross-referencing the canonical session file decisions.
- Managing the index file — adding entries, moving plans between Plan Board lanes, syncing branch + PR state.
- Validating a `git diff` against a plan's REQUIREMENTS (development validation mode).
- Writing or fixing org-mode syntax in node files.

## When to Read Which Rules

| If working on...                                                     | Read these rules                                                            |
|----------------------------------------------------------------------|-----------------------------------------------------------------------------|
| Creating a new plan node (Feature / Bug / Release)                   | `rules/node-lifecycle.md` + `rules/templates.md` + `rules/gh-parsing.md`    |
| Extracting branch + PR state via git/gh                              | `rules/gh-parsing.md`                                                       |
| Choosing or filling a template variant                               | `rules/templates.md`                                                        |
| Fixing org-mode syntax or checking rendering rules                   | `rules/org-mode-reference.md`                                               |
| Updating the index file after any plan operation                     | `rules/index-management.md`                                                 |
| Validating development against a plan's requirements                 | `rules/node-lifecycle.md`                                                   |
| Updating an existing plan (progress, decisions, post-impl findings)  | `rules/node-lifecycle.md` + `rules/templates.md`                            |
| Understanding the two-layer index (BACKLOG + PLAN BOARD)             | `rules/index-management.md`                                                 |
| Moving a plan between Plan Board lanes or syncing git/gh state       | `rules/index-management.md`                                                 |
| Filling DOCUMENTATION sections (Phase 2 content)                     | `rules/writing-standards.md` + `rules/templates.md`                         |
| Writing content into any plan section (tone, clarity, terminology)   | `rules/writing-standards.md`                                                |

## Quick Reference

| Rule                 | Description                                                                                                                                                                                                                                                                |
|----------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `templates`          | Plan templates — base structure, three variants (Standard Plan / Bug Plan / Release Plan), metadata header rules, section formatting, file naming, UUID generation, variant selection.                                                                                     |
| `gh-parsing`         | Git + gh state extraction — current branch, open PRs, tags, recent commits. Maps git/gh state to Plan Board lanes. Reads decisions from the canonical session file. Replaces JIRA polling.                                                                                |
| `org-mode-reference` | Org-mode syntax — emphasis, property drawers, list continuity, checkbox + statistics-cookie counting, tags, tables, code blocks, org-roam ID links, common pitfalls.                                                                                                       |
| `index-management`   | Index file management — two-layer architecture (BACKLOG source-of-truth + PLAN BOARD workflow view), entry formats, git/gh state → lane mapping, parent-child nesting, checkbox lifecycle, mandatory 8-step update flow, edge cases (shelved plans, branch deletion).      |
| `node-lifecycle`     | Plan lifecycle and process — 7-step execution flow (identify → check existing → fetch git/gh state → determine variant → generate → write → update index), two-phase lifecycle (planning vs development), validation mode, update mode, quality checklist.                |
| `writing-standards`  | Writing standards — four guiding principles (clarity, accuracy, consistency, specificity), active voice, terse technical writing style, per-section writing rules (REQUIREMENTS, DECISIONS, DOCUMENTATION, FINDINGS, COMMENTs), terminology consistency, propose-before-fill workflow for Phase 2 sections. |

## Project Constants

| Constant            | Value                                                                            |
|---------------------|----------------------------------------------------------------------------------|
| **Repo**            | `Kyonax/reckit`                                                                  |
| **Local checkout**  | `/run/media/kyonax/Da_ Disk/dev/github-kyonax/kyo-recording-automation/`         |
| **Index file**      | `~/.brain.d/roam-nodes/2026-04-26-index_reckit.org`                              |
| **Index UUID**      | `93c9b466-676d-48bf-9d1b-ec8b93816b5d`                                           |
| **Plan files dir**  | `~/.brain.d/roam-nodes/reckit/`                                                  |
| **Session file**    | `dot-files/.config/doom-mac/gptel-directives/sessions/kyo-recording-automation.md` |
| **Author**          | Cristian D. Moreno - Kyonax (ORCID `0009-0006-4459-5538`)                       |
| **Author email**    | `kyonax.corp@gmail.com`                                                          |

## Absolute Prohibitions

**NEVER run any git or gh write command** unless the user explicitly and directly tells you to. The user manages all git operations manually. This skill *reads* git/gh state and *writes* org files — never the inverse. The repo's `CLAUDE.md` enforces this absolutely. This includes: `git commit`, `git push`, `git tag`, `git merge`, `git rebase`, `git reset --hard`, `git checkout -- .`, `git restore .`, `git clean`, `git stash drop`, `git branch -D`, `gh pr create`, `gh pr merge`, `gh release create`.
