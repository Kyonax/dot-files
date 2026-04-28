---
title: Git + gh State Extraction — Branches, Commits, PRs, Tags, and Session-File Decisions
impact: CRITICAL
impactDescription: This rule replaces mr-roam-node's jira-parsing. RECKIT has no JIRA — instead, the Plan Board lane and the plan node's STATUS / SOURCE_URL / DELIVERABLEs are derived from git + gh state. Without this rule, the index drifts out of sync with reality.
tags: git, gh, github, state, branch, commit, pr, tag, session-file, decision, lane-derivation, status, parsing, extraction, source-url, deliverables
---

This rule defines how the skill reads external state — **git** for branches/commits/tags, **gh** for PRs and releases, and the **canonical session file** for global architectural decisions. It does NOT parse user input (the user authors the plan content directly). It parses the project's external state to keep the index dashboard accurate.

## Repo Constants

| Constant            | Value                                                                            |
|---------------------|----------------------------------------------------------------------------------|
| **Repo**            | `Kyonax/reckit`                                                                  |
| **Local checkout**  | `/run/media/kyonax/Da_ Disk/dev/github-kyonax/kyo-recording-automation/`         |
| **Default branch**  | `master`                                                                         |
| **Integration branch** | `dev`                                                                         |
| **Session file**    | `dot-files/.config/doom-mac/gptel-directives/sessions/kyo-recording-automation.md` |

All `git` commands run inside the local checkout; all `gh` commands target `Kyonax/reckit`.

## Git State Queries

### Current branch

```bash
git rev-parse --abbrev-ref HEAD
```

Maps to `#+BRANCH:` in the plan metadata. If the user is on `master` or `dev`, the active plan is likely a Release plan (or no plan).

### Branch existence + commits

```bash
git rev-parse --verify <branch>      # confirms branch exists locally
git log --oneline <branch> ^dev      # commits unique to the branch (not yet on dev)
git log -1 --format='%H %s' <branch> # latest commit (hash + subject)
```

A plan transitions from `IN PLANNING` → `IN DEVELOPMENT` when the branch has ≥1 commit beyond `dev`.

### Recent commits across the project

```bash
git log --oneline -20 --all --remotes
```

Use this to populate DELIVERABLEs `*Commits:*` field after the plan ships.

### Tags

```bash
git tag --list 'v*' --sort=-v:refname | head -10
```

Returns released versions. Plan nodes that ship in a release should reference the tag URL: `https://github.com/Kyonax/reckit/releases/tag/v<X.Y>`.

## gh State Queries

### Open PRs

```bash
gh pr list --json number,title,state,headRefName,baseRefName,url --limit 20
```

Shape:
```json
[
  {
    "number": 5,
    "title": "feat(kot): brand refinement + OBS FPS performance pass",
    "state": "OPEN",
    "headRefName": "feat-brand_kot",
    "baseRefName": "dev",
    "url": "https://github.com/Kyonax/reckit/pull/5"
  }
]
```

### PR for a specific branch

```bash
gh pr list --head <branch> --json number,state,url
```

Empty list → no PR yet → plan stays in `IN DEVELOPMENT`. Non-empty + state `OPEN` → plan moves to `IN REVIEW`.

### PR detailed state (review status)

```bash
gh pr view <N> --json state,reviewDecision,mergedAt,mergeStateStatus
```

`reviewDecision: "APPROVED"` + `mergedAt: null` → manually move plan to `IN TEST` if user opts in.
`mergedAt` not null → move plan to `ALL RELEASED` (checkbox `[X]` permanent).

### PR title naming convention reminder

| PR type                              | Format                                          |
|--------------------------------------|-------------------------------------------------|
| Feature branch → integration (`dev`) | `feat(<scope>): <lowercase summary>`             |
| Bug fix                              | `fix(<scope>): <lowercase summary>`              |
| Release (`dev` → `master`)           | `release: v<X.Y> — <Semantic Name>`              |

Plans with PRs that don't follow this convention should still be tracked, but flag for the user.

## Session-File Decision Extraction

The canonical session file at `dot-files/.config/doom-mac/gptel-directives/sessions/kyo-recording-automation.md` contains a numbered decisions log under §2.3 "Key Decisions". Each decision has shape:

```
N. **(YYYY-MM-DD)** [Decision narrative...]
```

When a plan node references a global decision, use:

```bash
grep -n '^[0-9]\+\. \*\*(' dot-files/.config/doom-mac/gptel-directives/sessions/kyo-recording-automation.md \
  | head -50
```

Or for a specific decision number:

```bash
awk -v n=132 '/^[0-9]+\. \*\*\(/ {if ($1 == n".") print}' \
  dot-files/.config/doom-mac/gptel-directives/sessions/kyo-recording-automation.md
```

Cross-reference in the plan with `*Cross-ref:* session-file decision #132`.

## Plan Board Lane Derivation

Mechanical mapping from git/gh state to lane:

| Lane               | Condition                                                                                  | Checkbox |
|--------------------|--------------------------------------------------------------------------------------------|----------|
| `IN PLANNING`      | Plan node exists; (no branch yet) OR (branch exists with 0 commits beyond `dev`)            | `[ ]`    |
| `IN DEVELOPMENT`   | Plan branch has ≥1 commit beyond `dev`; `gh pr list --head <branch>` returns empty          | `[ ]`    |
| `IN REVIEW`        | `gh pr list --head <branch> --state open` returns a PR                                      | `[ ]`    |
| `IN TEST`          | PR `reviewDecision: APPROVED` AND `mergedAt: null` (manual signal — user opts in)           | `[ ]`    |
| `ALL RELEASED`     | PR `mergedAt` not null OR plan's tag exists in `git tag` output                              | `[X]`    |
| `SHELVED`          | Manually marked by user — no derivation                                                    | `[ ]`    |

The skill SHOULD run the relevant `git` / `gh` query before every index update to confirm lane placement. The skill MUST NOT poll continuously — only at well-defined update moments (plan create, plan update, explicit "sync index" request).

## Status Field on Plan Node

`#+STATUS:` mirrors the lane. Update simultaneously with index updates.

| Lane             | `#+STATUS:`     |
|------------------|-----------------|
| `IN PLANNING`    | `PLANNING`      |
| `IN DEVELOPMENT` | `IN_DEV`        |
| `IN REVIEW`      | `IN_REVIEW`     |
| `IN TEST`        | `IN_TEST`       |
| `ALL RELEASED`   | `RELEASED`      |
| `SHELVED`        | `SHELVED`       |

## Source URL Field

`#+SOURCE_URL:` evolves with the plan:

| Phase                 | URL                                                              |
|-----------------------|------------------------------------------------------------------|
| `IN PLANNING`         | `https://github.com/Kyonax/reckit/tree/<branch>`                 |
| `IN DEVELOPMENT`      | (same — branch URL)                                              |
| `IN REVIEW` / later   | `https://github.com/Kyonax/reckit/pull/<N>` (replace branch URL) |
| Release plan + tag    | `https://github.com/Kyonax/reckit/releases/tag/v<X.Y>`           |

## DELIVERABLEs Auto-Fill

After a plan ships, fill DELIVERABLEs from git + gh:

```bash
# Branch
git rev-parse --abbrev-ref HEAD

# Commits on this branch (since branch base)
git log dev..<branch> --format='%h %s'

# PR number + merge date
gh pr view <N> --json number,mergedAt,title

# Files touched
git diff --name-only dev...<branch> | wc -l
git diff --name-only dev...<branch>

# Tests added (heuristic — file pattern)
git diff --name-only dev...<branch> | grep -E '\.test\.(js|ts|vue)$'

# Tag (if part of release)
git tag --contains <merge-commit>
```

## Failure Modes

- **Branch deleted upstream** — plan was probably abandoned. Move to `SHELVED` or remove from index after user confirmation. Never auto-delete.
- **PR closed without merge** — same. Plan goes `SHELVED` or back to `IN DEVELOPMENT` (if the user is going to retry).
- **Multiple PRs for same branch** — rare, but possible after force-push + re-PR. Use the latest open one for lane derivation.
- **Local-only branch** — the user hasn't pushed. Lane stays `IN DEVELOPMENT`; SOURCE_URL still uses the branch path (GitHub will 404 until push, but the canonical URL form is stable).
