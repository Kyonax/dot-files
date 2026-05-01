---
title: Per-Section Writing Rules and Component Documentation
impact: CRITICAL
impactDescription: Defines the mandatory writing rules for each of the 5 context block sections. Violating these rules produces context blocks where future AIs cannot distinguish reusable rules from implementation details, miss dependencies, or lose resume context.
tags: section, writing, rules, guidelines, overview, implementations, file-index, last-interaction, component, documentation, tree, dependency, cross-ticket, staging, scope, decisions, pending
---

This rule defines exactly what belongs in each of the 5 context block sections, how to write it, and what mandatory elements each section requires. Without these rules, sections blur together — reusable guidelines leak into implementation details, one-time decisions get promoted to global rules, and dependency graphs are incomplete.

## Section 1: Global Guidelines

**Purpose:** Staging area for session-specific constraints not yet captured in loaded skills.

**Structure:** Numbered subsections (1.1, 1.2, ...) with bullet lists. Each bullet is a single, enforceable rule with bold keyword label and concise explanation. Include inline code for specific identifiers.

**What belongs here:**
- Rules validated through reviews, bug fixes, or architectural discussions
- Patterns that emerged from this session/project specifically
- Conventions the user wants enforced that loaded skills don't cover

**What does NOT belong here:**
- General framework docs (that's what skills are for)
- Implementation details of specific items (that goes in Section 3)
- One-time fixes or item-specific decisions (that goes in Section 3)

**Mandatory elements:**
- **Intro blockquote** — Always starts with `> **Apply these rules to every task in this session.**` followed by the skill-staging explanation naming the specific loaded skills
- **Each rule must be enforceable** — not vague guidance like "write good code" but specific like "Always use brackets for if/else — no one-line conditionals"

**Graduation rule:** When a guideline in Section 1 has been stable across 3+ resets and applies beyond this session, it should be promoted into the corresponding skill's rule files and removed from Section 1. Add a one-line reference: `Promoted to [skill-domain] skill — [rule-topic]` so future sessions know it was moved, not lost.

**Architecture memory references:** When an architecture memory file exists for this session's domain, Section 1 guidelines that are fully documented there can use reference syntax instead of inline explanations. Keep 1-2 line summaries inline for readability; use `[session: filename > section > entry-id]` references for the full explanation (5+ lines). This prevents duplication and reduces context block size. Example:
```markdown
*   Self-contained landmarks — [session: site-revolution-architecture > design-patterns > dp-001]
```

---

## Section 2: Session Overview

**Purpose:** High-level orientation for a new AI session.

**Structure:**
- **2.1 Purpose** — Project/session description
- **2.2 Scope** — Table of all items in scope with status
- **2.3 Key Decisions (Session-Wide)** — Numbered list with rationale and dates
- **2.4 Pending Work** — Bullet list of outstanding items (with checkbox `[ ]` for tracking)

**Mandatory elements:**
- **Scope table** — Every item must have a row with current status. Update statuses on every reset.
- **Decision dates** — Every architectural decision must include its date: `**(YYYY-MM-DD)** Decision description — rationale`
- **Architectural decisions are session-wide only** — Decisions affecting only one item go in that item's Section 3 entry

**PR review resolutions:** Document these as a subsection (2.4 or 2.5) because they often establish new rules that get promoted to Section 1 in future resets.

---

## Section 3: Implementations

**Purpose:** Complete implementation reference per item. An AI should be able to resume work on any item by reading only Section 1 + the relevant Section 3 subsection.

**Structure per item:**

```markdown
### 3.N [Item Title]

**Created:** YYYY-MM-DD | **Last updated:** YYYY-MM-DD
**Status:** [Current status]

[Implementation details specific to this item's domain:]
- For code: component tree, file paths, store dependencies, key logic
- For skills: file structure, rule inventory, creation history, key decisions
- For documents: sections, templates, key content decisions
- For research: methodology, sources, findings, conclusions
```

### Component Documentation (Code Sessions)

When the session involves code (Vue components, React components, etc.), Section 3 has additional mandatory elements:

**Component tree diagram is mandatory** for every item involving more than one component:
```
TopLevelComponent (role description)
├── ChildComponentA              — short description (new/modified/pre-existing)
│   ├── GrandchildA1             — short description
│   └── GrandchildA2             — short description
└── ChildComponentB              — short description (new, TICKET-ID)
    └── LeafComponent            — short description
```

**Every component in the tree gets a documentation block** with these fields (when applicable):

| Field            | Required   | Description                                                       |
|------------------|------------|-------------------------------------------------------------------|
| **Path**         | Yes        | Exact file path from project root                                 |
| **Test**         | If exists  | Path to test file with test count                                 |
| **Role**         | Yes        | One-sentence description                                          |
| **Parent**       | Yes        | Which component renders it, props/events connection               |
| **Children**     | If any     | Child components with brief purpose                               |
| **Store**        | Yes        | State management modules accessed, or "None — fully props-driven" |
| **Emits**        | If any     | Custom events and their purpose                                   |
| **Key computed** | If notable | Named list of important computed properties                       |
| **Key methods**  | If notable | Named list of important methods                                   |

**Include dependency components even if not modified.** If a component is used by a modified component (parent, child, or sibling), include it with Path, Role, and connection point. The goal: the AI never needs to search the codebase to understand the component graph.

**Cross-item component modifications:** When a component is modified by multiple items, the first item's section has full documentation. Subsequent items reference back: "Changes for this item (see Section 3.N for full docs):" followed by only the delta.

### Key Decisions Table

Every non-trivial item should have a key decisions table:

```markdown
#### Key Decisions

| Decision | Date | Rationale |
|---|---|---|
| Decision description | YYYY-MM-DD | Why this approach was chosen |
```

---

## Section 4: File Index

**Purpose:** Flat lookup table. No descriptions, no context — just paths and associations.

**Structure:** One or more tables organized by category:

```markdown
### [Category] (New)
| File | Association |
|---|---|
| `path/to/file` | Item ID |

### [Category] (Modified)
| File | Association |
|---|---|
| `path/to/file` | Item ID |
```

**Common categories:** Components, Tests, Config, Documentation, Templates, Session Files

**Rules:**
- No duplicate entries
- Use consistent path format (absolute or project-relative, pick one per session)
- Every file mentioned in Section 3 must appear here

---

## Section 5: Last Interaction

**Purpose:** Short-term memory for session continuity. Always **fully replaced** on every reset — reflects only the most recent conversation, not accumulated history.

**Structure:**

```markdown
### What was done last
*   [3-5 bullet points — most recent conversation only]

### Pending / Not yet started
*   [All outstanding items — both session-scope and item-specific]

### Where to resume
If the user asks to continue **X**: [instructions].
If the user asks to start **Y**: [instructions].
If the user asks for a **new task**: check Section 2.4 (Pending Work).
```

**Rules:**
- Previous "last interactions" are absorbed into Sections 2 and 3 during merge
- Always include at least one resume path per open item
- Always include a fallback "new task" path pointing to Section 2.4

---

## Section 6: Activity Log

**Purpose:** Datetime-stamped, append-only chronological table of every meaningful event in the session — session resets, PR actions, commits, refinements, implementation, documentation, bug fixes. Authoritative input to time-tracking automation (e.g., `jira-tempo-hours`).

**Structure:** A single org-table-compatible markdown table with **5 fixed columns**:

```markdown
## SECTION 6: ACTIVITY LOG

| Datetime         | Duration | Type           | Reference        | Description |
|------------------+----------+----------------+------------------+-------------|
| YYYY-MM-DD HH:MM | Nh       | <type>         | <ref>            | One-line text |
```

**Mandatory elements:**
- **Datetime** in `YYYY-MM-DD HH:MM` (24-hour, host-local). No seconds, no timezone.
- **Duration** as `Nh`, `N.5h`, `Nm`, or `—` for instant events. Halves allowed.
- **Type** from the controlled vocabulary (`session-reset`, `pr-open`, `pr-update`, `pr-feedback`, `pr-review`, `pr-merge`, `commit`, `refinement`, `implementation`, `documentation`, `testing`, `qa`, `bug-fix`, `debugging`, `research`, `planning`, `meeting`, `configuration`, `migration`, `architecture-extract`, `other`). See `rules/activity-log.md` for the full vocabulary.
- **Reference** as a concrete value: ticket key (`DOTCOMPB-####`), PR (`PR #####`), commit (`commit <sha>`), `this` (current session file), or `n/a` only when truly nothing applies.
- **Description** as a past-tense, ≤ 100-char one-liner describing what was *done*.

**Sort order:** Newest first. New entries are *prepended*; existing rows are never modified.

**Rules:**
- Every session reset MUST add at least one row of type `session-reset` referencing `this`.
- Append-only — never edit or delete past rows. To correct an error, add a new row with `type: other` describing the correction.
- Not compressed under the standard compression protocol — monotonically grows but each row is small.
- Sessions pre-dating this rule get a single bootstrap row when Section 6 is first introduced; full entries are required from that reset forward.

**Cross-section consistency:**
- Section 2.3 decisions should reference back to the corresponding Activity Log row when relevant: `(2026-04-29) — see Activity Log row at 14:00`.
- Section 3 per-item `Last updated:` should match the most recent Activity Log row touching that item.
- Section 5 "What was done last" should mirror the top 3-5 Activity Log rows in abbreviated form.

See `rules/activity-log.md` for the complete schema, controlled vocabulary, when-to-add criteria, examples, and the validation checklist.

---

## Correct vs. Incorrect Examples

### Example 1: Global Guidelines vs Implementation Details

**Incorrect:** Implementation detail in Section 1.
```markdown
## SECTION 1: GLOBAL GUIDELINES
### 1.3 Search Component
*   SearchBar uses debounced input with 300ms delay and GraphQL query
```

**Correct:** Reusable rule in Section 1, implementation in Section 3.
```markdown
## SECTION 1: GLOBAL GUIDELINES
### 1.3 Input Debouncing
*   All search inputs must use 300ms debounce before triggering API calls

## SECTION 3: IMPLEMENTATIONS
### 3.2 Search Feature
*   **SearchBar:** Debounced input (300ms) triggers GraphQL `searchProducts` query
```

**Why:** The debounce rule applies everywhere and belongs in guidelines. The specific GraphQL query is implementation detail.

### Example 2: Section 5 Replacement

**Incorrect:** Accumulating history in Section 5.
```markdown
### What was done last
*   (Mar 5) Created SearchBar component
*   (Mar 8) Added pagination to results
*   (Mar 12) Fixed accessibility on filters ← most recent
```

**Correct:** Only the most recent conversation.
```markdown
### What was done last
*   Fixed accessibility on filter dropdowns — added aria-label and keyboard navigation
*   Updated SearchBar test suite — 12 tests now passing
```

**Why:** Section 5 is short-term memory. Historical items belong in Section 2 (decisions) or Section 3 (implementations). Accumulating makes Section 5 unreliable as a "where was I?" pointer.
