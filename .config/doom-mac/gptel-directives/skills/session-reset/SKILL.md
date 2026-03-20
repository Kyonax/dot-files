---
name: session-reset
description: >-
  Session reset, context compaction, and session management for long-running AI development sessions. Compacts all accumulated knowledge (guidelines, rules, implementations, bug fixes, code review findings, architectural decisions, validated patterns) from the current conversation into a structured context block inside a session .md file. Handles creating new session files, updating existing ones, and compressing context blocks that exceed the 3693-line threshold using a date-aware, hierarchy-based compression protocol. Trigger when: user says 'reset session', 'compact session', 'session reset', 'save session', 'context block', 'update context block', 'compress context', or any variation of resetting/compacting/saving session context. Also trigger when the user mentions running out of context, needing to preserve knowledge, or wanting to start fresh while keeping learnings.
metadata:
  author: Kyonax
  version: "3.1.0"
---

# Session Reset Skill

Compacts all accumulated knowledge from AI conversations into a bounded, structured context block — a 5-section markdown document inside a session file. The context block serves as the single source of truth for future sessions, with a hard 3693-line limit enforced by a date-aware compression protocol.

## Core Principles

1. **Structured over free-form** — Every context block follows a mandatory 5-section architecture
2. **Bounded over unbounded** — Context blocks have a 3693-line limit with hierarchical compression
3. **Dates as metadata** — Every decision, implementation, and status change carries an absolute date
4. **Preserve relevance** — Compression targets the oldest, least-referenced entries first
5. **Never delete completely** — Compressed/removed entries leave tombstones with recovery pointers

## When to Apply

Reference these guidelines when:

*   Performing a session reset (compacting conversation knowledge)
*   Creating a new session file from scratch
*   Updating an existing session file with new knowledge
*   Compressing a context block that exceeds 3693 lines
*   Writing or refining any of the 5 context block sections
*   Verifying a context block before finalizing

## When to Read Which Rules

| If working on...                                             | Read these rules                                                  |
|--------------------------------------------------------------|-------------------------------------------------------------------|
| Performing a full session reset                              | `rules/execution-flow.md` + `rules/context-block-architecture.md` |
| Creating a new session file from scratch                     | `rules/context-block-architecture.md`                             |
| Writing or refining specific sections of a context block     | `rules/section-writing-rules.md`                                  |
| Context block exceeds 3693 lines after merge                  | `rules/compression-protocol.md`                                   |
| Verifying a context block before writing to file             | `rules/quality-checklist.md`                                      |
| Understanding the 5-section structure or template            | `rules/context-block-architecture.md`                             |
| Documenting component trees and dependencies (code sessions) | `rules/section-writing-rules.md` (Section 3 component docs)       |
| Understanding compression priority, impact assessment, dates | `rules/compression-protocol.md`                                   |

## Quick Reference

| Rule                         | Description                                                                                                                                                                                                                                                                                                                                                                     |
|------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `context-block-architecture` | Mandatory 5-section layout (Guidelines, Overview, Implementations, File Index, Last Interaction), delimiter format, file naming convention, section map table, opening elements, and the full context block template for new sessions.                                                                                                                                          |
| `execution-flow`             | The 5-step session reset process: determine file, gather knowledge (categorized by section), merge with existing block, write formatted output, write to file. Includes standalone file mode for agents without conversation access (e.g., GPTel) and date tracking requirements.                                                                                               |
| `section-writing-rules`      | Per-section writing rules: what belongs in each section, mandatory elements, component documentation fields (Path, Role, Parent, Store), tree diagrams, cross-item references, guideline graduation, and Section 5 replacement semantics.                                                                                                                                       |
| `compression-protocol`       | 3693-line threshold, mandatory impact assessment (CRITICAL/HIGH/MEDIUM/LOW session value labels), two-axis priority (age × impact), 5-level compression hierarchy (graduated guidelines → oldest lowest-impact implementations → oldest decisions → file index trim → tombstoning), CRITICAL shielding, summary reference blocks, tombstone pattern, and compression log format. |
| `quality-checklist`          | Mandatory verification checklist run before every write: section completeness, date presence, status accuracy, cross-reference integrity, line count, compression log, and common failure patterns with fixes.                                                                                                                                                                  |
