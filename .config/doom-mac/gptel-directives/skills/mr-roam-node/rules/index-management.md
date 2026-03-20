---
title: Index File Management — Two-Layer Architecture, Update Flow, Nesting, and Edge Cases
impact: CRITICAL
impactDescription: The index file is the central dashboard for all tickets. Without this rule, index entries become orphaned, Sprint Board lanes desync from JIRA status, statistics cookies are wrong, and the two-layer architecture (BACKLOG + Sprint Board) breaks — making the index unreliable as a workflow tool.
tags: index, management, backlog, sprint-board, two-layer, architecture, update-flow, nesting, parent-child, checkbox, lifecycle, statistics-cookie, jira-status, lane-mapping, anchor, org-roam-link, file-link, edge-case, integrity, validation, mandatory, sync
---

This rule governs the index file — the central dashboard for all Madison Reed tickets. The index uses a two-layer architecture: BACKLOG (source of truth with org-roam links to node files) and SPRINT BOARD (workflow view with internal file references to Backlog anchors). **Every node operation (create, update, refine, validate) MUST end with an index update — this is mandatory, never optional.** Without this rule, entries become orphaned, lanes desync from JIRA, and the index becomes unreliable.

## Index File

| Property            | Value                                                                                                    |
|---------------------|----------------------------------------------------------------------------------------------------------|
| **Path**            | `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org`                                                |
| **ID**              | `7c2b1bc9-4a2e-4a64-b2e8-b36e2ba95106`                                                                   |
| **Title**           | "Index Madison Reed"                                                                                     |
| **FILETAGS**        | `:INDEX:MR:`                                                                                             |
| **Assignee filter** | Only tickets assigned to Cristian Moreno (`cristian.moreno@madison-reed.com`) appear in the Sprint Board |

## Index Structure

```org
* DA' DOCs                          # Links to docs index — do not modify
* IMPORTANT LINKs                   # External URLs (Jira, GitHub, Figma) — do not modify
* NOTEs                             # Misc notes — do not modify

* SPRINT BOARD TICKETs
# TEMPLATE: [[file:./2025-11-18-index_madison_reed.org::item-id][]]
** IN TODO [N/M]
** IN PROGRESS [N/M]
** IN CODE REVIEW [N/M]
** IN TEST [N/M]
** ALL DONE [N/M]

* BACKLOG [P%] [N/M]
```

The TOC mirrors this structure with `[[#section-id][SECTION NAME]]` links.

---

## Two-Layer Architecture

### Layer 1 — BACKLOG (Source of Truth)

- Contains org-roam `[[id:UUID][Title]]` links pointing to actual node files
- Each entry has a named anchor (`<<ticket-XXXX>>` or `<<bug-XXXX>>`) that serves as the target
- Checkbox state reflects JIRA resolution: `[X]` = done, `[ ]` = open
- This is the **only** place where org-roam links to node files exist

### Layer 2 — SPRINT BOARD (Workflow View)

- Contains internal `[[file:./2025-11-18-index_madison_reed.org::anchor-id][Title]]` references that point TO the Backlog anchors
- Items are organized by workflow lanes matching JIRA status
- Parent-child nesting mirrors JIRA issue links
- This layer is **derived from** the Backlog — it never contains org-roam links

### Data Flow

```
JIRA status       → determines Sprint Board lane
JIRA issue links  → determines parent-child nesting in Sprint Board
Node file :ID:    → used in Backlog org-roam link [[id:UUID][...]]
Backlog <<anchor>> → referenced by Sprint Board [[file:...::anchor][...]]
```

---

## BACKLOG Section (`* BACKLOG`)

The **single source of truth**. Every ticket that has a node file must have a BACKLOG entry.

**Entry format:**
```org
- [STATUS] [[id:UUID][NODE_TITLE]] <<ANCHOR>> :: Description
```

| Field            | Source                                                                     | Example                                           |
|------------------|----------------------------------------------------------------------------|---------------------------------------------------|
| `STATUS`         | `[X]` if JIRA status maps to ALL DONE, `[ ]` otherwise                     | `[ ]`                                             |
| `UUID`           | The `:ID:` from the node's `:PROPERTIES:` drawer                           | `fd48919d-36c8-402e-85d4-90d4f05fa8e0`            |
| `NODE_TITLE`     | The node's `#+TITLE:` value exactly                                        | `(BUG) Ticket #DOTCOMPB-7557`                     |
| `ANCHOR`         | Named target: `<<ticket-XXXX>>` for stories/tasks, `<<bug-XXXX>>` for bugs | `<<bug-7557>>`                                    |
| `:: Description` | Short human-readable context                                               | `:: ADA - Cannot tab to Book Services on desktop` |

**Example entries:**
```org
* BACKLOG [64%] [9/14]
- [ ] [[id:25e65871-4198-44b3-8334-db067b718d21][Ticket #DOTCOMPB-7289]] <<ticket-7289>> :: Implements the About Data on the hcb-locations
- [ ] [[id:fd48919d-36c8-402e-85d4-90d4f05fa8e0][(BUG) Ticket #DOTCOMPB-7557]] <<bug-7557>> :: ADA - Cannot tab to Book Services on desktop
- [X] [[id:7420d942-4293-496c-bafc-b321b8b4b1f5][Ticket #DOTCOMPB-7045]] <<ticket-7045>> :: Improve error validation on the Sign in modal
```

---

## Sprint Board Section (`* SPRINT BOARD TICKETs`)

A **workflow view** where each entry is an internal file reference pointing to a BACKLOG anchor.

**Entry format (from TEMPLATE line):**
```org
- [STATUS] [[file:./2025-11-18-index_madison_reed.org::ANCHOR][(TYPE) Short Title]] :: Optional context
```

| Field         | Description                                                                                       |
|---------------|---------------------------------------------------------------------------------------------------|
| `STATUS`      | `[X]` only in ALL DONE lane; `[ ]` in all other lanes                                             |
| `ANCHOR`      | Must match an existing `<<anchor>>` in BACKLOG: `ticket-XXXX` or `bug-XXXX`                       |
| `TYPE`        | `(TICKET)` for stories/tasks, `(BUG)` for bugs                                                    |
| `Short Title` | Human-readable cleaned summary (strip JIRA prefixes like `[Site Revolution]`, emoji, ticket refs) |
| `:: Context`  | Optional short note about purpose or status context                                               |

**Optional cross-references:** Sprint Board entries may include `<<cross-ref>>` radio targets (e.g., `<<dependency-1>>`, `<<child-1>>`) for linking related entries across lanes.

**Example:**
```org
** IN CODE REVIEW [0/1]
- [ ] [[file:./2025-11-18-index_madison_reed.org::ticket-7289][(TICKET) Specific Location page updates - HCB details]] :: location specific development
  - [ ] [[file:./2025-11-18-index_madison_reed.org::bug-7557][(BUG) ADA: Cannot tab to Book Services on desktop]] :: Keyboard nav issue found during group testing

** ALL DONE [9/9]
- [X] [[file:./2025-11-18-index_madison_reed.org::ticket-7045][(TICKET) Improve Error Validation on the Sign in Modal]] :: Update Error validation
```

---

## Parent-Child Nesting Rules

JIRA issue links determine parent-child relationships in the Sprint Board:

| Scenario                                    | Sprint Board Layout                              |
|---------------------------------------------|--------------------------------------------------|
| Parent and child in **same lane**           | Child is indented sub-item under parent          |
| Parent and child in **different lanes**     | Each is a top-level item in its respective lane  |
| Child moves to a different lane than parent | Un-nest: child becomes top-level in its new lane |
| Child moves back to same lane as parent     | Re-nest: child becomes sub-item again            |

When moving a nested item to a different lane, always check if it should be nested under an existing parent in the destination lane.

---

## JIRA Status → Sprint Board Lane Mapping

| JIRA Status                                              | Sprint Board Lane   | Checkbox |
|----------------------------------------------------------|---------------------|----------|
| `Tareas por hacer` (To Do) / `Backlog` / `Open`          | `** IN TODO`        | `[ ]`    |
| `En curso` (In Progress) / `In Development`              | `** IN PROGRESS`    | `[ ]`    |
| `In Code Review`                                         | `** IN CODE REVIEW` | `[ ]`    |
| `In Test` / `In QA` / `Ready for QA`                     | `** IN TEST`        | `[ ]`    |
| `Finalizada` (Done) / `Closed` / `Released` / `Resolved` | `** ALL DONE`       | `[X]`    |

**Fallback:** If status name is unrecognized, use the JIRA `statusCategory.key`: `new` → IN TODO, `indeterminate` → IN PROGRESS, `done` → ALL DONE.

---

## Checkbox Lifecycle

Checkboxes serve different purposes in each layer:

### BACKLOG Checkboxes

- `[ ]` = ticket is open in JIRA (any non-Done status)
- `[X]` = ticket is resolved in JIRA (Done/Closed/Released)
- **JIRA always overrides** — updated on every sync

### Sprint Board Checkboxes

- In `ALL DONE` lane: always `[X]` (permanent)
- In other lanes: `[ ]` by default, but user may manually mark `[X]` as a **transient signal** meaning "I'm done with this phase"
- When JIRA advances the ticket to a new status, the item **moves to the new lane with `[ ]`** — the user's `[X]` signal is "consumed"
- **JIRA always wins for lane placement** — checkbox state in non-DONE lanes never determines which lane an item belongs to

### Lifecycle Example

```
1. Ticket is "In Code Review"  → Sprint Board: IN CODE REVIEW [ ]
2. User marks [X] locally      → Sprint Board: IN CODE REVIEW [X]  (user signal)
3. JIRA moves to "In Test"     → Sprint Board: IN TEST [ ]         ([X] consumed, fresh state)
4. JIRA moves to "Done"        → Sprint Board: ALL DONE [X]        (permanent)
                                  Backlog: [X]                       (permanent)
```

---

## Index Update Flow (Mandatory)

**CRITICAL: This flow MUST execute as the FINAL STEP of every node operation (create, update, refine, validate). Never skip it.**

### Step 1: Gather Data

```
READ  → current index file (~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org)
FETCH → JIRA ticket via Atlassian MCP (status, type, summary, issue links, assignee)
READ  → node file :ID: property (UUID) from the node's :PROPERTIES: drawer
```

### Step 2: Resolve Identifiers

| Data               | Derivation                                                                                     | Example                                             |
|--------------------|------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| Anchor ID          | `bug-{number}` for Error type, `ticket-{number}` for Story/Task                                | `bug-7557`                                          |
| Backlog title      | Node's `#+TITLE:` exactly                                                                      | `(BUG) Ticket #DOTCOMPB-7557`                       |
| Sprint Board title | `(TYPE)` + cleaned JIRA summary (strip `[Site Revolution]`, emoji, date brackets, ticket refs) | `(BUG) ADA: Cannot tab to Book Services on desktop` |
| Description        | Short human-readable context for `::` suffix                                                   | `Keyboard nav issue found during group testing`     |
| Node UUID          | From node `:PROPERTIES: :ID:`                                                                  | `fd48919d-36c8-402e-85d4-90d4f05fa8e0`              |

### Step 3: Update BACKLOG (Always First — Source of Truth)

1. Search BACKLOG section for existing anchor `<<{anchor-id}>>`
2. **If NOT found** — create new entry:
   ```org
   - [ ] [[id:{UUID}][{Backlog title}]] <<{anchor-id}>> :: {description}
   ```
   Insert at the end of BACKLOG, before any blank lines.
3. **If found** — verify and update:
   - Confirm UUID matches node file `:ID:` (warn if mismatch)
   - Update title if node `#+TITLE:` changed
   - Update description if JIRA summary changed
   - Update checkbox: `[X]` if JIRA status maps to ALL DONE, `[ ]` otherwise

### Step 4: Determine Correct Sprint Board Lane

1. Get JIRA status name from ticket data
2. Map to Sprint Board lane using the JIRA Status → Sprint Board Lane Mapping table
3. If status name is unrecognized, use the JIRA `statusCategory`: `new` → IN TODO, `indeterminate` → IN PROGRESS, `done` → ALL DONE

### Step 5: Update Sprint Board (Derived from BACKLOG)

1. Search ALL lanes for any `file:...::anchor-id` reference
2. Determine correct lane from Step 4
3. **Three scenarios:**

| Scenario                        | Action                                                |
|---------------------------------|-------------------------------------------------------|
| Entry NOT found in any lane     | Create new entry in correct lane                      |
| Entry found in **wrong** lane   | Remove from old lane → add to correct lane with `[ ]` |
| Entry found in **correct** lane | Verify title/description text, leave checkbox as-is   |

4. **Nesting logic** (check JIRA issue links):
   - If ticket has a parent link (e.g., bug 7557 "created by" ticket 7289):
     - Check if parent exists in the **same** destination lane
     - If yes → nest as indented sub-item under parent
     - If no → place as top-level item in the lane
   - When moving an item, check for orphaned children in the old lane — if children remain, they become top-level items

5. **ALL DONE special handling:**
   - Always set `[X]` on the entry
   - If the item was a parent with nested children, each child should be checked independently against its own JIRA status

### Step 6: Recalculate Statistics Cookies

- **Sprint Board lanes:** Each `** LANE [X/Y]` heading — count only **direct child** checkboxes (org-mode rule: nested sub-items under a list item are NOT counted, only top-level `- [ ]`/`- [X]` items directly under the heading)
- **BACKLOG:** `* BACKLOG [P%] [X/Y]` — count all direct items, compute percentage

**Counting example:**
```org
** IN CODE REVIEW [0/1]          ← 1 direct child (ticket-7289), 0 checked
- [ ] [[file:...::ticket-7289][(TICKET) ...]]
  - [ ] [[file:...::bug-7557][(BUG) ...]]    ← sub-item, NOT counted in [0/1]
```

### Step 7: Validate Integrity

Before writing, verify:
- Every Sprint Board `file:...::anchor` has a matching `<<anchor>>` in BACKLOG
- Every open BACKLOG entry (for a ticket assigned to the user) has a Sprint Board entry
- No duplicate anchors exist in BACKLOG
- No orphaned Sprint Board entries (referencing an anchor that doesn't exist)

### Step 8: Write

Write the updated index file. Report what changed:
- Entries added/moved/updated
- Lanes affected
- Statistics cookies recalculated

---

## Edge Cases

| Case                                    | Handling                                                                                              |
|-----------------------------------------|-------------------------------------------------------------------------------------------------------|
| Ticket has no node file yet             | Create the node first (node UUID required for Backlog). Then run index update.                        |
| Ticket unassigned from user in JIRA     | Remove from Sprint Board (no longer in user's workflow). Keep in Backlog (historical record).         |
| Ticket reassigned back to user          | Re-add to Sprint Board in correct lane per current JIRA status.                                       |
| JIRA status not in mapping table        | Fall back to `statusCategory.key`: `new` → IN TODO, `indeterminate` → IN PROGRESS, `done` → ALL DONE. |
| Parent in ALL DONE but child still open | Child is top-level in its own lane. Does NOT nest under parent in ALL DONE.                           |
| Multiple children under same parent     | All children nest under parent if in same lane. Order by ticket number ascending.                     |
| Ticket deleted from JIRA                | Flag to user for manual review — do not auto-delete from index.                                       |
