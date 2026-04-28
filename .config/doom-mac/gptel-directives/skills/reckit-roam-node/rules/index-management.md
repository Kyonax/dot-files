---
title: Index File Management — Two-Layer Architecture, Update Flow, and Edge Cases
impact: CRITICAL
impactDescription: The index file is the central dashboard for all RECKIT plans. Without this rule, index entries become orphaned, Plan Board lanes desync from git/gh state, statistics cookies are wrong, and the two-layer architecture breaks — making the index unreliable as a workflow tool.
tags: index, management, backlog, plan-board, two-layer, architecture, update-flow, nesting, parent-child, checkbox, lifecycle, statistics-cookie, gh-state, lane-mapping, anchor, org-roam-link, file-link, edge-case, integrity, validation, mandatory, sync, reckit
---

This rule governs the RECKIT index file — the central dashboard for all plans. The index uses a two-layer architecture: BACKLOG (source of truth with org-roam links to plan files) and PLAN BOARD (workflow view with internal file references to BACKLOG anchors). **Every plan operation (create, update, refine, validate) MUST end with an index update — this is mandatory, never optional.**

## Index File

| Property        | Value                                                  |
|-----------------|--------------------------------------------------------|
| **Path**        | `~/.brain.d/roam-nodes/2026-04-26-index_reckit.org`     |
| **ID**          | `93c9b466-676d-48bf-9d1b-ec8b93816b5d`                 |
| **Title**       | "Index Reckit"                                         |
| **FILETAGS**    | `:RECKIT:INDEX:`                                       |

## Index Structure

```org
* DOCS & RESEARCH                  # Architecture, conventions, research nodes
* FEATURE BRANCHES                 # Long-lived multi-plan branches
* SESSIONS                         # Session log index (canonical lives in session file)
* IMPORTANT LINKs                  # GitHub repo, CHANGELOG, README, etc.
* QUICK NOTEs                      # Current branch, open PRs, next release

* PLAN BOARD
# TEMPLATE: [[file:./2026-04-26-index_reckit.org::plan-id][]]
** IN PLANNING [N/M]
** IN DEVELOPMENT [N/M]
** IN REVIEW [N/M]
** IN TEST [N/M]
** ALL RELEASED [N/M]
** SHELVED [N/M]

* BACKLOG [P%] [N/M]
```

The TOC mirrors this structure with `[[#section-id][SECTION NAME]]` links.

---

## Two-Layer Architecture

### Layer 1 — BACKLOG (Source of Truth)

- Contains org-roam `[[id:UUID][Title]]` links pointing to actual plan node files.
- Each entry has a named anchor `<<plan-<slug>>>` (or `<<bug-<slug>>>`, `<<release-v<X_Y>>>`).
- Checkbox state: `[X]` if `STATUS: RELEASED`, `[ ]` otherwise.
- **Only place where org-roam links to plan node files exist.**

### Layer 2 — PLAN BOARD (Workflow View)

- Contains internal `[[file:./2026-04-26-index_reckit.org::anchor-id][Title]]` references that point TO the BACKLOG anchors.
- Items organized by workflow lanes derived from git/gh state (see `gh-parsing.md`).
- Parent-child nesting mirrors plan dependencies (when one plan blocks another).
- **Derived from** the BACKLOG — never contains org-roam links.

### Data Flow

```
git branch state    → determines IN PLANNING vs IN DEVELOPMENT lane
gh pr state         → determines IN REVIEW lane
gh pr mergedAt      → determines ALL RELEASED lane
Plan node :ID:      → used in BACKLOG org-roam link [[id:UUID][...]]
BACKLOG <<anchor>>  → referenced by PLAN BOARD [[file:...::anchor][...]]
```

---

## BACKLOG Section

The single source of truth. Every plan that has a node file must have a BACKLOG entry.

**Entry format:**
```org
- [STATUS] [[id:UUID][NODE_TITLE]] <<ANCHOR>> :: Description
```

| Field            | Source                                                                                                            | Example                                                                       |
|------------------|-------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------|
| `STATUS`         | `[X]` if `#+STATUS:` is `RELEASED`, `[ ]` otherwise                                                              | `[ ]`                                                                         |
| `UUID`           | The `:ID:` from the node's `:PROPERTIES:` drawer                                                                  | `cabd1489-23cc-4ce3-9825-7b5a8eb065b9`                                        |
| `NODE_TITLE`     | The node's `#+TITLE:` value exactly                                                                               | `Plan #context-screen`                                                        |
| `ANCHOR`         | `<<plan-<slug>>>` for Standard, `<<bug-<slug>>>` for Bug, `<<release-v<X_Y>>>` for Release                       | `<<plan-context-screen>>`                                                     |
| `:: Description` | Short human-readable context                                                                                      | `:: News-style lower-third HUD with .org-authored content + landing-page control` |

**Example:**
```org
* BACKLOG [33%] [1/3]
- [ ] [[id:cabd1489-23cc-4ce3-9825-7b5a8eb065b9][Plan #context-screen]] <<plan-context-screen>> :: News-style lower-third HUD with .org-authored content + landing-page control
- [X] [[id:abcd1234-...][Plan #brand-kot-perf-pass]] <<plan-brand-kot-perf-pass>> :: Brand refinement + OBS FPS performance pass
- [ ] [[id:efgh5678-...][Plan #item-explain]] <<plan-item-explain>> :: Animation web source for product/concept reveal
```

---

## PLAN BOARD Section

A workflow view where each entry is an internal file reference pointing to a BACKLOG anchor.

**Entry format:**
```org
- [STATUS] [[file:./2026-04-26-index_reckit.org::ANCHOR][(TYPE) Short Title]] :: Optional context
```

| Field         | Description                                                                                          |
|---------------|------------------------------------------------------------------------------------------------------|
| `STATUS`      | `[X]` only in `ALL RELEASED` lane; `[ ]` in all other lanes                                         |
| `ANCHOR`      | Must match an existing `<<anchor>>` in BACKLOG: `plan-<slug>` / `bug-<slug>` / `release-v<X_Y>`     |
| `TYPE`        | `(PLAN)` for Standard, `(BUG)` for Bug, `(RELEASE)` for Release                                     |
| `Short Title` | Human-readable cleaned summary (≤ 60 chars)                                                          |
| `:: Context`  | Optional short note about purpose or status context                                                  |

**Example:**
```org
** IN REVIEW [0/2]
- [ ] [[file:./2026-04-26-index_reckit.org::plan-brand-kot-perf-pass][(PLAN) Brand Refinement + OBS FPS Perf Pass]] :: PR #5 open
- [ ] [[file:./2026-04-26-index_reckit.org::release-v0_4][(RELEASE) v0.4 — Architectural Baseline]] :: PR #4 open, triad pending on dev

** ALL RELEASED [3/3]
- [X] [[file:./2026-04-26-index_reckit.org::release-v0_3][(RELEASE) v0.3 — Vue App Baseline]]
- [X] [[file:./2026-04-26-index_reckit.org::plan-fixes-and-refinement-v3][(PLAN) Fixes + Refinement v3]]
- [X] [[file:./2026-04-26-index_reckit.org::release-v0_1][(RELEASE) v0.1 — Foundation]]
```

---

## Parent-Child Nesting

When a plan depends on another plan being released first, nest in the PLAN BOARD:

| Scenario                                              | PLAN BOARD Layout                                |
|-------------------------------------------------------|--------------------------------------------------|
| Parent and dependent both in **same lane**            | Dependent is indented sub-item under parent      |
| Parent and dependent in **different lanes**           | Each is a top-level item in its respective lane  |
| Dependent moves to a different lane than parent       | Un-nest: dependent becomes top-level in new lane |
| Dependent moves back to same lane as parent           | Re-nest: dependent becomes sub-item again        |

When moving a nested item, always check if it should be nested under an existing parent in the destination lane.

---

## Checkbox Lifecycle

### BACKLOG Checkboxes

- `[ ]` = plan is not yet released (any non-RELEASED status).
- `[X]` = plan is released (`#+STATUS: RELEASED`, PR merged or tag pushed).
- **`#+STATUS:` always overrides** — updated on every sync.

### PLAN BOARD Checkboxes

- In `ALL RELEASED` lane: always `[X]` (permanent).
- In other lanes: `[ ]` by default, but user may manually mark `[X]` as a **transient signal** meaning "I'm done with this phase".
- When git/gh state advances the plan to a new lane, the item **moves to the new lane with `[ ]`** — the user's `[X]` signal is consumed.
- **git/gh state always wins for lane placement** — checkbox state in non-RELEASED lanes never determines which lane an item belongs to.

### Lifecycle Example

```
1. Plan node created, no commits  → PLAN BOARD: IN PLANNING [ ]
2. First commit lands              → PLAN BOARD: IN DEVELOPMENT [ ]
3. PR opened                       → PLAN BOARD: IN REVIEW [ ]
4. User marks [X] locally          → PLAN BOARD: IN REVIEW [X]  (user signal — "I'm done reviewing")
5. PR merged                       → PLAN BOARD: ALL RELEASED [X]  (permanent)
                                     BACKLOG:    [X]               (permanent)
                                     #+STATUS:   RELEASED           (permanent)
```

---

## Index Update Flow (Mandatory 8-Step)

**CRITICAL: This flow MUST execute as the FINAL STEP of every plan operation (create, update, refine, validate). Never skip it.**

### Step 1: Gather Data

```
READ  → current index file (~/.brain.d/roam-nodes/2026-04-26-index_reckit.org)
READ  → plan node file :ID: property (UUID)
RUN   → git rev-parse --abbrev-ref HEAD
RUN   → git rev-list --count dev..<branch> 2>/dev/null  (commits on plan branch)
RUN   → gh pr list --head <branch> --json number,state,mergedAt
RUN   → gh pr view <N> --json reviewDecision (if PR exists)
RUN   → git tag --contains <merge-commit> (if PR merged)
```

### Step 2: Resolve Identifiers

| Data                | Derivation                                                                       | Example                                  |
|---------------------|----------------------------------------------------------------------------------|------------------------------------------|
| Anchor ID           | `plan-<slug>` for Standard, `bug-<slug>` for Bug, `release-v<X_Y>` for Release   | `plan-context-screen`                    |
| BACKLOG title       | Node's `#+TITLE:` exactly                                                        | `Plan #context-screen`                   |
| PLAN BOARD title    | `(TYPE)` + cleaned summary, ≤ 60 chars                                           | `(PLAN) Context-Screen Web Source`        |
| Description         | Short human-readable context                                                     | `News-style lower-third HUD ...`         |
| Plan UUID           | From plan node `:PROPERTIES: :ID:`                                              | `cabd1489-23cc-4ce3-9825-7b5a8eb065b9`   |

### Step 3: Update BACKLOG (Always First — Source of Truth)

1. Search BACKLOG section for existing anchor `<<{anchor-id}>>`.
2. **If NOT found** — create new entry, insert at the end of BACKLOG (before any blank trailing lines):
   ```org
   - [ ] [[id:{UUID}][{Backlog title}]] <<{anchor-id}>> :: {description}
   ```
3. **If found** — verify and update:
   - Confirm UUID matches plan node file `:ID:` (warn if mismatch).
   - Update title if node `#+TITLE:` changed.
   - Update description if subtitle/scope changed.
   - Update checkbox: `[X]` if `#+STATUS: RELEASED`, `[ ]` otherwise.

### Step 4: Determine Correct PLAN BOARD Lane

Apply the lane derivation table from `gh-parsing.md`:

| Lane             | Condition                                                                                       |
|------------------|-------------------------------------------------------------------------------------------------|
| `IN PLANNING`    | No branch OR branch with 0 commits beyond `dev`                                                |
| `IN DEVELOPMENT` | Branch has ≥1 commit, no open PR                                                                |
| `IN REVIEW`      | Open PR exists for this branch                                                                  |
| `IN TEST`        | PR approved AND not merged AND user opted in (manual transient lane)                           |
| `ALL RELEASED`   | PR merged OR tag exists containing the merge commit                                             |
| `SHELVED`        | Manually marked — never auto-derived                                                            |

### Step 5: Update PLAN BOARD (Derived from BACKLOG)

1. Search ALL lanes for any `file:...::anchor-id` reference.
2. Determine correct lane from Step 4.
3. **Three scenarios:**

| Scenario                        | Action                                                |
|---------------------------------|-------------------------------------------------------|
| Entry NOT found in any lane     | Create new entry in correct lane                      |
| Entry found in **wrong** lane   | Remove from old lane → add to correct lane with `[ ]` |
| Entry found in **correct** lane | Verify title/description text, leave checkbox as-is   |

4. **Nesting logic** (check plan's RELEVANT LINKs for parent-plan references):
   - If plan has a parent (depends on Plan #X to be released first):
     - Check if parent exists in the **same** destination lane.
     - If yes → nest as indented sub-item under parent.
     - If no → place as top-level item in the lane.
   - When moving an item, check for orphaned children in the old lane.

5. **`ALL RELEASED` special handling:**
   - Always set `[X]` on the entry.
   - If the item was a parent with nested children, each child checked independently against its own state.

### Step 6: Update Plan Node Metadata

Sync the plan node's metadata with the new state:

- `#+LAST_UPDATE:` → today's date.
- `#+STATUS:` → derived from lane (PLANNING / IN_DEV / IN_REVIEW / IN_TEST / RELEASED / SHELVED).
- `#+SOURCE_URL:` → if PR was just opened, switch from branch URL to PR URL.

### Step 7: Recalculate Statistics Cookies

- **PLAN BOARD lanes:** Each `** LANE [X/Y]` heading — count only **direct child** checkboxes (org-mode rule: nested sub-items under a list item are NOT counted, only top-level `- [ ]`/`- [X]` items directly under the heading).
- **BACKLOG:** `* BACKLOG [P%] [X/Y]` — count all direct items, compute percentage.

**Counting example:**
```org
** IN REVIEW [0/1]                  ← 1 direct child, 0 checked
- [ ] [[file:...::plan-A][(PLAN) ...]]
  - [ ] [[file:...::plan-B][(PLAN) ...]]    ← sub-item, NOT counted in [0/1]
```

### Step 8: Validate Integrity + Write

Before writing, verify:

- Every PLAN BOARD `file:...::anchor` has a matching `<<anchor>>` in BACKLOG.
- Every plan node UUID in BACKLOG matches an actual plan file's `:ID:`.
- No duplicate anchors exist in BACKLOG.
- No orphaned PLAN BOARD entries (referencing an anchor that doesn't exist).
- `QUICK NOTEs` section reflects current branch + open PRs.

Then write. Report what changed:

- Entries added/moved/updated.
- Lanes affected.
- Statistics cookies recalculated.
- Plan node metadata synced (`STATUS`, `SOURCE_URL`, `LAST_UPDATE`).

---

## Edge Cases

| Case                                              | Handling                                                                                                       |
|---------------------------------------------------|----------------------------------------------------------------------------------------------------------------|
| Plan has no node file yet                         | Create the plan first (UUID required for BACKLOG). Then run index update.                                      |
| Branch deleted upstream / locally                  | Move plan to `SHELVED` after user confirmation. Never auto-delete from BACKLOG (historical record).            |
| PR closed without merge                           | Move plan back to `IN DEVELOPMENT` (user is going to retry) or to `SHELVED` (parked). Ask the user.            |
| Plan reassigned to a different branch              | Update `#+BRANCH:` and `#+SOURCE_URL:`. Lane derivation re-runs from new branch.                              |
| Plan rebased on top of merged dev                  | No change — lane derivation uses `gh pr list --head <branch>`, branch state, and tag-contains.                |
| Parent in `ALL RELEASED` but dependent still open  | Dependent is top-level in its own lane. Does NOT nest under parent in `ALL RELEASED`.                          |
| Multiple dependents under same parent              | All dependents nest under parent if in same lane. Order alphabetically by slug.                                |
| Plan node file deleted manually                    | Flag to user for manual review — do not auto-remove from index.                                                |
| QUICK NOTEs out of date                            | Refresh on every index update — current branch, open PRs, next release. Cheap query, always current.          |
