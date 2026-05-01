---
title: Context Block Architecture and 5-Section Structure
impact: CRITICAL
impactDescription: Defines the mandatory 5-section layout, delimiter format, file naming, and template that every context block must follow. Without this structure, a future AI session cannot reliably parse or navigate the compacted knowledge.
tags: context-block, architecture, sections, structure, template, delimiters, session-file, naming, format, layout, five-section, markdown, compaction
---

This rule defines the mandatory structure of a context block — the compacted knowledge unit inside a session file. Every context block follows a 5-section architecture designed so a future AI can load it and immediately understand scope, constraints, implementations, file locations, and where to resume. Violating this structure (merging sections, omitting the map table, skipping delimiters) makes the context block unparseable for future sessions.

## What Is a Context Block?

A context block is the output of a session reset — a structured markdown section inside a `.md` file that captures all accumulated knowledge from one or more AI conversations. It serves as the **single source of truth** for future sessions, allowing an AI to resume work with full context without re-discovering anything.

**Key properties:**
- Self-contained — a future AI reading ONLY the context block can continue work immediately
- Structured — always follows the 5-section architecture (never free-form prose)
- Delimited — enclosed in HTML comment markers for machine-parseable extraction
- Mergeable — existing context blocks can be updated with new knowledge without losing prior context

## Delimiter Format

Every context block is enclosed in HTML comments. These markers enable programmatic extraction:

```markdown
<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

[... all compacted knowledge goes here ...]

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
```

**Rules:**
- The outer markers (`INIT OF THE USER PROMPT`) wrap the entire session payload
- The inner markers (`DESCRIPTION AND USER CONTEXT`) wrap the actual knowledge content
- Two blank lines between the closing inner marker and closing outer marker
- Optional local variables block (for editor compatibility) may follow after the outer closing marker

## File Naming Convention

Session files live in a designated sessions directory. Names follow these rules:

| Convention                                              | Example                                                   |
|---------------------------------------------------------|-----------------------------------------------------------|
| Kebab-case, descriptive                                 | `site-revolution-redesign.md`                             |
| Include ticket ID if single-ticket                      | `dotcompb-7052-dynamic-yield.md`                          |
| Use subdirectory + `SESSION.md` for multi-file sessions | `my-project-session/SESSION.md`                           |
| Use descriptive topic name for cross-cutting sessions   | `frontend-redesign-master.md` (e.g., GPTel session files) |

## The 6-Section Architecture

Every context block is organized into **6 mandatory sections**, each serving a distinct purpose. Data may appear in multiple sections with different framing — this is intentional.

| Section                              | Purpose                                                                                     | When to Reference                                                                       |
|--------------------------------------|---------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| **1. Global Guidelines**             | Rules, patterns, and conventions that apply to ALL work in this session.                    | Before performing any task. These are mandatory constraints.                            |
| **2. Session Overview**              | High-level context: what this project/session is about, scope, decisions, and pending work. | When starting a new task — understand scope and status first.                           |
| **3. Feature/Skill Implementations** | Per-item detail: what was built, where files live, key decisions, and current state.        | When resuming work on a specific item, or when a new task relates to an existing one.   |
| **4. File Index**                    | Quick-reference table of all file paths relevant to this session.                           | When you need to read, edit, or reference a specific file without searching.            |
| **5. Last Interaction**              | Short-term memory: what was done last, what's pending, where to resume.                     | At the very start of a new conversation — this is your entry point for continuing work. |
| **6. Activity Log**                  | Datetime-stamped, append-only table of every meaningful event in the session.               | When you need precise "what was done when" — including for time-tracking automation.    |

> Section 6 (Activity Log) is mandatory as of v4.1. See `rules/activity-log.md` for the full schema, controlled vocabulary (`session-reset`, `pr-open`, `pr-feedback`, `commit`, `refinement`, `implementation`, `documentation`, `bug-fix`, etc.), and validation rules. Sessions that pre-date this rule must add a bootstrap row on their next reset.

**Framing principle:** Section 1 frames knowledge as a rule to follow. Section 2 frames it as context to understand. Section 3 frames it as an implementation to reference. Each section answers a different question about the same knowledge.

## Mandatory Opening Elements

Every context block must start with these elements in order:

1. **Description paragraph** — Direct prose (no heading) explaining what the file is and how to use it
2. **Section map table** — The 5-section table shown above, orienting the AI on first load
3. **Operational rules** — Any session-specific processing instructions (e.g., how to identify the latest user request)
4. **Key principle** — The multi-framing explanation ("Data may appear in multiple sections...")
5. **Compaction sources** — If the context block was built from multiple prior sessions, list them with dates

## Context Block Template (New Session)

```markdown
<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the [PROJECT/SESSION NAME] session. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read the sections in order on first load — after that, reference them by number as needed.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, patterns, and conventions that apply to ALL work. | Before any task. Mandatory constraints. |
| **2. Session Overview** | High-level context: scope, decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-item detail: what was built, decisions, state. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference file path table. | When reading, editing, or locating files. |
| **5. Last Interaction** | Short-term memory: last work, pending, resume points. | At conversation start — entry point. |
| **6. Activity Log** | Datetime-stamped table of every meaningful event. | When you need exact "what was done when". |

**Operational Rule:** [Session-specific instructions, e.g., how to identify latest request]

**Key principle:** Data may appear in multiple sections with different framing. This is intentional — each section answers a different question about the same knowledge.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** [Name loaded skills if applicable]. This section stores session-scoped patterns not yet captured in those skills — serving as a staging area for guidelines that will eventually be refined there.

### 1.1 [Category]
*   [Rules as bullet points]

---

## SECTION 2: SESSION OVERVIEW

> This section provides the overall context, purpose, and scope of the session.

### 2.1 Purpose
[Project/session description]

### 2.2 Scope
| Item | Type | Summary     | Status |
|------|------|-------------|--------|
| `ID` | Type | Description | Status |

### 2.3 Key Decisions (Session-Wide)
[Numbered list with rationale]

### 2.4 Pending Work
[Bullet list of outstanding items]

---

## SECTION 3: IMPLEMENTATIONS

> Each subsection documents a specific item's implementation.

### 3.N [Item Title]
**Created:** YYYY-MM-DD | **Last updated:** YYYY-MM-DD
**Status:** [Current status]
[Implementation details, decisions, structure]

---

## SECTION 4: FILE INDEX

> Quick reference for all files relevant to this session.

| File | Association |
|---|---|
| `path/to/file` | Item/ticket |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last
*   [Bullets — most recent conversation only]

### Pending / Not yet started
*   [All outstanding items]

### Where to resume
If the user asks to continue **X**: [instructions].
If the user asks for a **new task**: check Section 2.4.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session.
> See `rules/activity-log.md` for the full spec. Newest row first.

| Datetime         | Duration | Type           | Reference | Description |
|------------------+----------+----------------+-----------+-------------|
| YYYY-MM-DD HH:MM | Nh       | session-reset  | this      | First reset that established this section |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
```

## Correct vs. Incorrect Examples

### Example 1: Opening Structure

**Incorrect:** Starting with a markdown heading.
```markdown
# My Session Context
## Section 1: Guidelines
...
```

**Correct:** Starting with a description paragraph, then section map table.
```markdown
This file is the **single source of truth** for the Widget Redesign session...

| Section | Purpose | When to reference |
...
```

**Why:** The description paragraph + section map table gives the AI immediate orientation. A heading-first approach buries the navigation structure.

### Example 2: Section Boundaries

**Incorrect:** Merging implementation details into Global Guidelines.
```markdown
## SECTION 1: GLOBAL GUIDELINES
### 1.5 ComponentX Implementation
*   ComponentX uses a skeleton pattern with 3 loading states...
```

**Correct:** Guidelines in Section 1, implementation details in Section 3.
```markdown
## SECTION 1: GLOBAL GUIDELINES
### 1.5 Loading Patterns
*   All components must use skeleton loading states during async operations

## SECTION 3: IMPLEMENTATIONS
### 3.1 ComponentX
*   **Skeleton pattern:** Uses 3 loading states (initial, partial, complete)...
```

**Why:** Section 1 is for reusable rules that apply everywhere. Section 3 is for specific implementation details. Mixing them means the AI cannot distinguish "always do this" from "this is what we did here."
