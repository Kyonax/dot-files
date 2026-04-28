# Reckit Roam Node Skill — Architectural Guide

## Core Philosophy: Self-Driven Plans, Git as State Backbone

This skill bridges three systems: **git** (where branches, commits, tags live), **gh** (where PRs and releases live), and **org-roam** (where structured documentation lives). Unlike `mr-roam-node` — which translates JIRA tickets into nodes — this skill works with **self-driven plans**: units of work the user defines themselves, committed to a branch, shipped through a PR.

The fundamental principle is that **the user authors the plan during a planning conversation, then the org-roam node becomes the single source of truth** for scope, decisions, progress, and post-implementation findings.

This is a one-way flow:
- The user decides scope (no JIRA-style external authority).
- The plan node freezes scope on first commit.
- Git/gh state determines workflow lane (PLAN BOARD), but never overrides the plan content itself.

The only ongoing sync from external systems is **branch + PR state** (for index lane placement) and **PR merge** (for checkbox state). Content never syncs back from git or gh.

## The Two-Phase Plan Lifecycle

Every plan exists in two phases:

1. **Phase 1 — Planning:** The skill (in conversation with the user) drafts the SCOPE AND GOAL section, captures REQUIREMENTS as GIVEN/WHEN/THEN, sketches DESIGN AND ARCHITECTURE, logs initial DECISIONS, and creates phased TODO PLAN TASKs. The skill is the author. **Frozen at first commit on the plan's branch.**

2. **Phase 2 — Development:** The user fills DOCUMENTATION (STRUCTURE / DEPLOYMENT / TESTS / QA / FINDINGS), promotes OPEN QUESTIONS to DECISIONS, and updates the DELIVERABLEs section. The user is the author.

These phases have different validation rules:
- *Phase 1 content is the requirement* — what must be built. Frozen, version-controlled by the plan node.
- *Phase 2 content is the evidence* — what was built and how to verify it. Living document.

Validation cross-references both against `git diff` on the plan's branch.

## Plans vs Tickets — Why the Different Unit?

`mr-roam-node` uses **Tickets** because Madison Reed runs on JIRA. Tickets have a 1:1 mapping to JIRA records.

RECKIT runs on git + gh. There are no tickets. The smallest unit that makes sense is a **Plan** — a coherent change that:

- Has a name (`<slug>`).
- Lives on its own branch (typically).
- Ends with one or more PRs.
- Ships in a known release (`#+TARGET_RELEASE`).
- Can be referenced by other plans, by the session file, and by future-you.

A **Plan** can be a feature (most common), a bug fix (Bug variant), or a release (Release variant — the meta-plan that bundles other plans for a tagged release).

## The Index as a Dashboard

The index file is not just a list — it's a two-layer dashboard that mirrors git/gh workflow while maintaining org-roam's linking model:

- **BACKLOG** is the source of truth — it holds the org-roam `id:` links to actual plan node files.
- **PLAN BOARD** is a derived view — it holds internal `file:` references that point to BACKLOG anchors.

This architecture means the Plan Board can be completely regenerated from the BACKLOG + current git/gh state. The BACKLOG is the durable layer; the Plan Board is the ephemeral workflow layer.

### Lane derivation (no JIRA polling)

| Lane               | Derivation                                                              |
|--------------------|-------------------------------------------------------------------------|
| `IN PLANNING`      | Plan node exists; branch may or may not exist; no commits on plan branch |
| `IN DEVELOPMENT`   | Plan branch has ≥1 commit; no open PR yet                                |
| `IN REVIEW`        | `gh pr list --head <branch> --state open` returns a PR                   |
| `IN TEST`          | PR is approved but not yet merged (manual transient lane)                |
| `ALL RELEASED`     | PR merged into target branch; checkbox `[X]` permanent                   |
| `SHELVED`          | Manually parked; user-marked only                                        |

## Why `gh-parsing` Replaces `jira-parsing`

The biggest challenge in `mr-roam-node` is JIRA's lack of description standardization (6 format variants). RECKIT has no equivalent problem — the user writes plans directly in org-mode using this skill.

Instead, `gh-parsing` handles the inverse: extracting **state** from git + gh (current branch, recent commits, open PRs, merged tags) to keep the index accurate. It does not parse user input — it parses the project's external state.

## Connecting the Rules

| Rule                 | Role in the System                                                                  |
|----------------------|-------------------------------------------------------------------------------------|
| `templates`          | Defines the exact output structure — what the plan node looks like                  |
| `gh-parsing`         | Defines the state inputs — how to read git/gh, where the session file lives        |
| `org-mode-reference` | Defines the syntax constraints — how org-mode renders content                       |
| `index-management`   | Defines the dashboard operations — how the index stays in sync with git/gh         |
| `node-lifecycle`     | Defines the process — when each operation happens and in what order                |
| `writing-standards`  | Defines the content style — how each section reads (tone, voice, density)          |

The flow is: **read git/gh state** (gh-parsing) → **fill template** (templates) → **verify syntax** (org-mode-reference) → **update index** (index-management) → **validate later** (node-lifecycle).

## Relationship to the Canonical Session File

The session file at `dot-files/.config/doom-mac/gptel-directives/sessions/kyo-recording-automation.md` is the **single source of truth for the entire RECKIT project** — it tracks all sessions, decisions, architectural canon, and pending work. The roam nodes generated by this skill are **a dashboard view + per-plan deep-dives** on top of that. They are NOT a replacement.

When a plan's DECISIONS section captures a decision that is *globally relevant* (architectural canon, performance rule, naming convention), the session file should also receive that decision in §2.3 (Key Decisions). The skill includes a `Cross-ref:` field on each decision pointing to the session-file decision number once promoted to canon.

When a plan ships, the session file's §5 (Last Interaction) summarizes the work, and §2.4 (Pending Work) is updated. The plan node's DELIVERABLEs section cross-references the session-file lines.

## Relationship to Other Skills

- **`pr-scribe`** — When a plan reaches Phase 5 (PR), the user invokes `pr-scribe` (Kyonax brand, Pattern B Feature PR or Release PR). The plan's DECISIONS section feeds the TD-4FIELD blocks; the REQUIREMENTS feed the QA-HOW-TO-TEST groups.
- **`code-review`** — The Kyonax brand rules at `brand/kyonax/` (21 atomic rules) auto-load when reviewing the plan's PR. They enforce the §1.14 Performance Budget that this skill's Plan template references in REQUIREMENTS.
- **`session-reset`** — When the canonical session file gets compacted, the plan nodes survive (they are external dashboards). New session decisions reference plan-node UUIDs going forward.
