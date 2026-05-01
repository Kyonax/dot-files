---
name: session-reset
description: >-
  Session reset, context compaction, architecture memory extraction, and session management for long-running AI development sessions. Compacts all accumulated knowledge (guidelines, rules, implementations, bug fixes, code review findings, architectural decisions, validated patterns) from the current conversation into a structured context block inside a session .md file. Optionally extracts reusable architectural knowledge (decisions, patterns, constraints, data flows) into persistent architecture memory files with cross-session reference syntax. Handles creating new session files, updating existing ones, compressing context blocks that exceed the 3693-line threshold, and writing architecture references into context blocks. Trigger when: user says 'reset session', 'compact session', 'session reset', 'save session', 'context block', 'update context block', 'compress context', 'extract architecture', 'architecture memory', or any variation of resetting/compacting/saving session context. Also trigger when the user mentions running out of context, needing to preserve knowledge, wanting to start fresh while keeping learnings, or extracting reusable patterns from sessions.
metadata:
  author: Kyonax
  version: "4.1.0"
---

# Session Reset Skill

Compacts all accumulated knowledge from AI conversations into a bounded, structured context block — a 5-section markdown document inside a session file. Optionally extracts reusable architectural knowledge into persistent architecture memory files with cross-session referencing. The context block serves as the single source of truth for future sessions, with a hard 3693-line limit enforced by a date-aware compression protocol.

## Core Principles

1. **Structured over free-form** — Every context block follows a mandatory 6-section architecture (Section 6 is the Activity Log added in v4.1)
2. **Bounded over unbounded** — Context blocks have a 3693-line limit with hierarchical compression
3. **Datetime over date** — Every event in the Activity Log carries an absolute `YYYY-MM-DD HH:MM`. Decisions and implementations carry at least a date.
4. **Audit-trail mandatory** — Every session reset adds at least one Activity Log row (`session-reset` type). PRs, commits, refinements, and other meaningful events are also logged.
5. **Preserve relevance** — Compression targets the oldest, least-referenced entries first; the Activity Log is never compressed (it's append-only and small per row).
6. **Never delete completely** — Compressed/removed entries leave tombstones with recovery pointers.

## When to Apply

Reference these guidelines when:

*   Performing a session reset (compacting conversation knowledge)
*   Creating a new session file from scratch
*   Updating an existing session file with new knowledge
*   Extracting reusable architectural knowledge into architecture memory files
*   Writing cross-session references in context blocks
*   Compressing a context block that exceeds 3693 lines
*   Writing or refining any of the 5 context block sections
*   Verifying a context block (with or without architecture extraction) before finalizing

## When to Read Which Rules

| If working on...                                             | Read these rules                                                  |
|--------------------------------------------------------------|-------------------------------------------------------------------|
| Performing a full session reset                              | `rules/execution-flow.md` + `rules/context-block-architecture.md` + `rules/activity-log.md` |
| Performing a session reset with architecture extraction      | `rules/execution-flow.md` (Step 2.5) + `rules/context-block-architecture.md` |
| Creating a new session file from scratch                     | `rules/context-block-architecture.md` + `rules/activity-log.md`   |
| Writing or refining specific sections of a context block     | `rules/section-writing-rules.md`                                  |
| Writing cross-session references in a context block          | `rules/execution-flow.md` (Step 4 — architecture references)      |
| Context block exceeds 3693 lines after merge                  | `rules/compression-protocol.md`                                   |
| Verifying a context block before writing to file             | `rules/quality-checklist.md` + `rules/activity-log.md` (validation) |
| Understanding the 6-section structure or template            | `rules/context-block-architecture.md`                             |
| Adding events to the Activity Log (resets, PRs, commits)     | `rules/activity-log.md`                                           |
| Documenting component trees and dependencies (code sessions) | `rules/section-writing-rules.md` (Section 3 component docs)       |
| Understanding compression priority, impact assessment, dates | `rules/compression-protocol.md`                                   |

## Quick Reference

| Rule                         | Description                                                                                                                                                                                                                                                                                                                                                                     |
|------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `context-block-architecture` | Mandatory 5-section layout (Guidelines, Overview, Implementations, File Index, Last Interaction), delimiter format, file naming convention, section map table, opening elements, and the full context block template for new sessions.                                                                                                                                          |
| `execution-flow`             | The 6-step session reset process: determine file, gather knowledge, extract architecture memory (optional Step 2.5 — reusable decisions/patterns/constraints into persistent memory files), merge with existing block, write formatted output with architecture references, write to file. Includes architecture memory file structure (6 sections), reference syntax (`[session: file > section > entry-id]`), extraction criteria, deduplication, and standalone file mode. |
| `section-writing-rules`      | Per-section writing rules: what belongs in each section, mandatory elements, component documentation fields (Path, Role, Parent, Store), tree diagrams, cross-item references, guideline graduation, architecture memory references in Section 1, and Section 5 replacement semantics.                                                                                          |
| `compression-protocol`       | 3693-line threshold, mandatory impact assessment (CRITICAL/HIGH/MEDIUM/LOW session value labels), two-axis priority (age × impact), 5-level compression hierarchy (graduated guidelines → oldest lowest-impact implementations → oldest decisions → file index trim → tombstoning), CRITICAL shielding, summary reference blocks, tombstone pattern, and compression log format. |
| `quality-checklist`          | Mandatory verification checklist run before every write: section completeness, date presence, status accuracy, cross-reference integrity, architecture memory validation (entry IDs, deduplication, extraction criteria), line count, compression log, and common failure patterns with fixes.                                                                                   |
| `activity-log`               | Mandatory Section 6 spec: append-only chronological table with `Datetime / Duration / Type / Reference / Description` columns. Defines the controlled vocabulary (`session-reset`, `pr-open`, `pr-feedback`, `commit`, `refinement`, `implementation`, `documentation`, `bug-fix`, etc.), datetime format (`YYYY-MM-DD HH:MM`, host-local), reference shapes (ticket / PR / commit / `this`), when to add entries, backwards compatibility for pre-existing sessions, and the validation checklist. Source of truth for time-tracking automation (e.g., `jira-tempo-hours`). |
