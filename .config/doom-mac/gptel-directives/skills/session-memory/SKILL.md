---
name: session-memory
description: >-
  Architecture memory and context optimization system for AI sessions. Extracts reusable architectural decisions, design patterns, data flow insights, constraints, and abstractions from session context blocks into persistent, referenceable memory files. Use when performing a session reset with architecture extraction, creating or updating architecture memory files, referencing specific architectural knowledge from past sessions, resolving conflicting decisions across sessions, optimizing context loading for minimal token usage, or integrating architecture memory into the session-reset workflow. Also trigger when the user mentions 'extract architecture', 'session memory', 'architecture memory', 'reference a decision', 'cross-session reference', or 'persistent patterns'.
metadata:
  author: @kyonax_on_tech
  version: "1.1.0"
---

# Session Memory

Extracts reusable architectural knowledge from AI session context blocks into persistent, structured memory files. Enables precise cross-session referencing, context-efficient loading, and continuous architectural learning across sessions and projects.

## Core Principle: Reference Over Copy

Every piece of architectural knowledge should have exactly one authoritative location. All other uses should be references — not copies. This prevents context bloat, eliminates divergence, and ensures updates propagate automatically.

## Datetime Provenance

As of v1.1, every architecture memory entry tracks not just the date but the **datetime** of the source decision (when known). This integrates with the Activity Log system added to `session-reset` v4.1: when extracting an entry from a session, the source-session reference includes the Activity Log row datetime so consumers know exactly *when* the decision was made.

```markdown
**Source:** `site-revolution-redesign.md` — Activity Log 2026-04-29 14:00
```

When a source session has no Activity Log entry for the decision, fall back to the date alone (`Source: file.md (2026-04-29)`). Never fabricate a datetime.

## When to Apply

Reference these guidelines when:

*   Performing a session reset that produced architectural decisions worth preserving.
*   Creating a new architecture memory file from scratch.
*   Updating an existing architecture memory file with new knowledge.
*   Referencing specific architectural knowledge in a new session's context block.
*   Resolving conflicting decisions discovered across sessions.
*   Optimizing a context block to use references instead of duplicated knowledge.
*   Integrating the architecture extraction step into the `/session-reset` flow.
*   Deciding whether session knowledge is architectural (reusable) or implementation (session-scoped).
*   Consolidating rapid iteration sessions into stable architectural records.
*   Checking freshness of stored architectural knowledge before recommending it.

## When to Read Which Rules

| If working on... | Read these rules |
|---|---|
| Performing a session reset with architecture extraction | `rules/reset-integration.md` + `rules/extraction-criteria.md` |
| Deciding what to extract from a session | `rules/extraction-criteria.md` |
| Creating or updating an architecture memory file | `rules/session-file-template.md` |
| Referencing architectural knowledge in a context block | `rules/reference-syntax.md` |
| Resolving contradictory decisions or handling edge cases | `rules/conflict-resolution.md` |
| Optimizing context loading or deduplicating knowledge | `rules/context-optimization.md` |
| Understanding the 6-section memory file structure | `rules/session-file-template.md` |
| Writing cross-session or cross-file references | `rules/reference-syntax.md` |
| Checking if knowledge should be in memory vs. a skill | `rules/extraction-criteria.md` + `rules/context-optimization.md` |
| Consolidating rapid iteration sessions | `rules/conflict-resolution.md` |

## Quick Reference

| Rule | Description |
|---|---|
| `extraction-criteria` | What qualifies as architectural memory: 6 inclusion categories (decisions, patterns, data flows, abstractions, constraints, structures), exclusion criteria (code-review rules, one-off fixes, debugging noise), the extraction decision flowchart, and generalization rules for over-specific patterns. |
| `session-file-template` | Mandatory 6-section architecture memory file structure (Summary, Architecture Decisions, Design Patterns, Shared State & Data Flow, Constraints & Limitations, Reusable References), entry formats with required fields per section, ID conventions (`ad-`, `dp-`, `sf-`, `cl-`, `rr-` prefixes), and the complete new-file template. |
| `reference-syntax` | Cross-session reference syntax specification: `[session: filename > section > entry-id]` format, 4 resolution levels (file, section, entry, multi-entry), resolution protocol (locate → extract → validate), embedding patterns in context blocks, lazy loading for constrained budgets, and deduplication-check during extraction. |
| `reset-integration` | Updated 6-step session reset flow with Step 2.5 (Extract Architecture Memory): candidate identification, target file selection, deduplication against existing entries, write/update protocol, reference list generation for Step 4, and criteria for when to skip extraction. |
| `context-optimization` | 6 optimization rules: reference-over-copy principle, minimal initial context (loading waterfall), mandatory deduplication (cross-session and within-file), freshness tracking and staleness detection, granularity control (full/summary/tombstone), cross-domain isolation (coupling test), and token budget guidelines (<500 lines per file, <20 references per context block). |
| `conflict-resolution` | 5 edge case handlers: contradictory decisions (contested status + user resolution), partial/incomplete implementations (maturity tracking: decided → implemented-once → validated), over-specific patterns (generalize before storing), missing context (uncertain annotation, no inference), and rapid iteration sessions (consolidate to final decision, collapse iterations into alternatives). |
