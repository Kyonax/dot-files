# Session Reset — Architectural Guide

## Core Philosophy: Knowledge Compaction as a First-Class Operation

Session resets are not "save and quit" — they are **active knowledge engineering**. The goal is not to dump a conversation into a file, but to extract, abstract, structure, and compress knowledge so that a future AI can resume with full context in a fraction of the tokens.

This philosophy drives three key design choices:

1. **5-Section Architecture over free-form notes.** A flat document of bullet points is easy to write but impossible to navigate. The 5-section structure gives each piece of knowledge a defined home and a defined audience, so the AI knows exactly where to look for any type of information.

2. **Bounded compression over unbounded growth.** Context blocks have a hard line limit (3693 lines). This is not arbitrary — it ensures the context block fits within practical AI context windows with room for the actual conversation. The compression protocol is designed to preserve relevance, not just recency.

3. **Dates as first-class metadata.** Every decision, implementation, and status change carries a date. Dates serve two purposes: they enable the compression protocol (oldest entries compress first) and they give future AIs temporal context ("was this decision made before or after that bug fix?").

## The Compression Philosophy

The compression protocol uses a **two-axis priority system: age × impact.** Age alone is a dangerous heuristic — an old entry that establishes a pattern used by active work is more valuable than a recent entry for completed, unreferenced work.

Before any compression, every compressible entry gets an **impact assessment** — a session value label (CRITICAL, HIGH, MEDIUM, LOW) based on how essential it is to the session's coherence and the next likely request. This creates a protection hierarchy:

- **CRITICAL entries are shielded** — they survive all 5 compression levels regardless of age
- **HIGH entries are protected** — compressed only as a last resort, never tombstoned
- **MEDIUM entries follow normal age-first rules**
- **LOW entries are compressed first** within each level

The assessment evaluates five criteria: session focus alignment, active reference count, pattern establishment (does it define something other entries depend on?), recoverability (can the knowledge be found elsewhere?), and temporal relevance.

The 5-level hierarchy (graduated guidelines → oldest lowest-impact implementations → oldest decisions → file index → tombstoning) ensures the least-impactful compressions happen first. The system only reaches Level 5 (actual removal) when all softer compressions are exhausted.

**The tombstone pattern** is critical: never delete knowledge completely. Leave a breadcrumb — the item name, completion date, branch name, and a pointer to where the full history lives (git history, project documentation, external knowledge base, etc.). This costs 2-3 lines but prevents the "what happened to X?" gap that makes context blocks unreliable.

## The Pillars of Session Reset Engineering

### 1. Structure: The 5-Section Architecture
Every context block follows the same 5-section layout. This is non-negotiable — it enables both human and AI navigation. Each section has a distinct audience and purpose, and data may appear in multiple sections with different framing.
*   **Reference:** `rules/context-block-architecture.md`

### 2. Process: The Execution Flow
Session resets follow a mandatory 5-step process: determine file, gather knowledge, merge, write, output. The merge step is where compression decisions happen. Skipping the read-before-merge step causes data loss.
*   **Reference:** `rules/execution-flow.md`

### 3. Content: Section Writing Rules
Each section has specific rules about what belongs, what doesn't, and what mandatory elements are required. The distinction between Section 1 (reusable rules) and Section 3 (implementation details) is the most common source of errors.
*   **Reference:** `rules/section-writing-rules.md`

### 4. Sustainability: The Compression Protocol
Context blocks are bounded — they cannot grow forever. The compression protocol defines a 5-level hierarchy for reducing size while preserving maximum relevance for the next request. Dates drive compression priority.
*   **Reference:** `rules/compression-protocol.md`

### 5. Verification: The Quality Checklist
Every reset must pass a verification checklist before writing to file. Compression can create inconsistencies (orphaned entries, stale statuses, broken references) that the checklist catches.
*   **Reference:** `rules/quality-checklist.md`

## Why This Architecture?

The session reset skill exists because AI conversations are ephemeral but projects are not. Without structured compaction:

- Knowledge dies with each conversation
- The user must re-explain context every session
- Decisions get re-debated because no one remembers the rationale
- Implementation details get re-discovered through expensive codebase searches

The context block is the bridge between ephemeral conversations and persistent projects. The 5-section architecture, mandatory dates, and compression protocol ensure this bridge remains usable as sessions accumulate knowledge over weeks and months.
