# Session Memory — Architectural Guide

## Core Philosophy: Architecture Knowledge Is a First-Class Asset

Sessions are ephemeral. Architectural knowledge is not. The fundamental problem this skill solves is the gap between **session-scoped context** (everything needed to resume a specific conversation) and **persistent architectural knowledge** (decisions, patterns, constraints, and abstractions that inform ALL future work).

The existing session-reset skill solves the first problem — compacting conversation knowledge into structured context blocks for session continuity. This skill solves the second — extracting the subset of that knowledge that is **reusable across sessions** and storing it in a persistent, referenceable format.

### The Knowledge Lifecycle

```
Conversation → Session Context Block → Architecture Memory → Future Sessions
     ↑                    ↑                      ↑                    ↑
  Ephemeral        Session-scoped         Persistent           Referenced
  (lost on close)  (survives resets)   (survives projects)   (on-demand)
```

Without this skill, the lifecycle terminates at "Session Context Block" — each new session must re-discover patterns, re-debate decisions, and re-identify constraints that were already resolved.

## The Three Pillars of Session Memory

### 1. Extraction: What to Remember

Not everything in a session is worth remembering. The extraction criteria (see `rules/extraction-criteria.md`) define a clear boundary between **architectural knowledge** (reusable decisions, patterns, constraints) and **implementation noise** (one-off fixes, debugging steps, test counts).

The core test: "Would this knowledge influence a decision in a future session that works on a different feature?" Architecture decisions, design patterns, data flow insights, key abstractions, constraints, and reusable structures pass this test. Bug fixes, PR workflow details, and formatting preferences do not.

This filter is critical because unfiltered storage creates the same problem as no storage — the AI cannot distinguish high-value knowledge from noise, and context windows fill with low-signal content.

### 2. Structure: How to Remember

Architecture memory files follow a 6-section structure (see `rules/session-file-template.md`) designed for two purposes:

1. **Efficient AI context loading** — Each section can be loaded independently, and summaries enable quick orientation without full content
2. **Precise cross-session referencing** — Every entry has a unique, stable ID that enables the reference syntax system

The 6 sections map to the 6 types of architectural knowledge:
- **Decisions** (what was chosen and why)
- **Patterns** (how to build things)
- **Data flows** (where data comes from and how it moves)
- **Constraints** (what can't be done)
- **References** (what shape the data takes)
- **Summary** (orientation metadata)

Each section answers a different question, and a future session may need answers to only one of those questions. The section structure enables selective loading at the architecture memory level — the same principle that drives rule file atomicity in the skill system.

### 3. Retrieval: How to Use What Was Remembered

The reference syntax system (see `rules/reference-syntax.md`) is the bridge between stored knowledge and active sessions. It enables:

- **Precise extraction** — Load one specific decision, not the entire memory file
- **Context efficiency** — References cost 1 line; full entries cost 10-30 lines
- **Deduplication** — Knowledge stored once, referenced many times
- **Lazy loading** — Resolve references on-demand, not at session start

The reference syntax is deterministic and parseable: `[session: filename > section > entry-id]`. This enables programmatic resolution — an AI can find the exact entry without searching or guessing.

## Why Architecture Memory Is Separate From Skills

A natural question: "Why not put architectural knowledge in skill rule files?"

The answer is **temporal scope and project specificity**:

| Property | Skills | Architecture Memory |
|---|---|---|
| **Scope** | Universal patterns for a domain | Project-specific decisions and constraints |
| **Lifespan** | Permanent and stable | Evolves with the project |
| **Content** | Best practices, conventions, standards | Specific decisions with rationale and context |
| **Example** | "Use Options API in Vue 3" | "We chose CSS columns over CSS Grid for the photos masonry because Grid caused hydration mismatch" |

Skills teach HOW to do things in general. Architecture memory records WHAT was decided and WHY in a specific context. Both are needed — skills provide the foundation, architecture memory provides the project-specific refinements.

When an architecture memory entry becomes universal (it applies to every project, not just this one), it should be **graduated** into a skill rule file — following the same graduation pattern used for session guidelines in the session-reset system.

## The Feedback Loop

The architecture memory system creates a continuous learning loop:

```
Session 1: Discover pattern A → Extract to memory
Session 2: Reference pattern A → Validate it works in new context → Update validated-in
Session 3: Discover pattern A doesn't apply here → Flag exception → Refine pattern
Session 4: Pattern A is stable across 5+ uses → Graduate to skill rule file
```

Each session both consumes and contributes to the architectural knowledge base. Over time, the memory becomes increasingly precise — patterns are validated or refined, constraints are confirmed or relaxed, and decisions accumulate the evidence needed for confident reuse.

## Integration Architecture

This skill does not replace `/session-reset` — it extends it. The session-reset flow gains a new step (2.5: Extract Architecture Memory) that runs between knowledge gathering and merging. The context block writer in Step 4 uses references to architecture memory entries instead of duplicating knowledge inline.

The integration is designed to be **backwards-compatible**: existing session files without architecture memory continue to work exactly as before. The extraction step is optional for short or implementation-only sessions. Architecture memory files are additive — they never modify existing context blocks, only provide references that context blocks can use.

For the full integration flow, see `rules/reset-integration.md`.
