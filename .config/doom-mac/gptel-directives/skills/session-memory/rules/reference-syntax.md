---
title: Cross-Session Reference Syntax — Precise Knowledge Retrieval
impact: CRITICAL
impactDescription: Defines the deterministic reference syntax for pointing to specific knowledge within architecture memory files. Without a standard syntax, cross-session references are ambiguous free-text that cannot be programmatically resolved or efficiently loaded.
tags: reference, syntax, cross-session, pointer, section, entry, resolution, parsing, deterministic, context-window, extraction, loading, precise, retrieval
---

This rule defines the reference syntax system — a deterministic, parseable mechanism for pointing to specific architectural knowledge within and across architecture memory files. Without standardized references, sessions either load entire memory files (wasting context) or use vague pointers ("see the architecture file") that require manual search.

## Core Principle: Precise Extraction Over Full Context Injection

The reference system enables loading **only the specific knowledge needed** — a single decision, a single pattern, or a single constraint — without pulling the entire memory file into context. This is the architecture memory equivalent of selective loading in the skill system.

## Reference Syntax Specification

### Basic Format

```
[session: <filename> > <section> > <entry-id>]
```

### Components

| Component | Required | Format | Description |
|---|---|---|---|
| `session:` | Yes | Literal prefix | Identifies this as a session memory reference |
| `<filename>` | Yes | Kebab-case, no extension | Architecture memory file name without `.md` |
| `>` | Yes | Separator | Delimits hierarchy levels |
| `<section>` | Depends | Section name (kebab-case) | Target section within the file |
| `<entry-id>` | Depends | Entry ID from the section | Specific entry within the section |

### Resolution Levels

The reference syntax supports **four levels of precision**, from broadest to most specific:

| Level | Syntax | What It Loads | When to Use |
|---|---|---|---|
| **File** | `[session: site-revolution-architecture]` | Section 1 (Summary) only | Orientation — "what does this memory cover?" |
| **Section** | `[session: site-revolution-architecture > design-patterns]` | Entire Section 3 | "Show me all patterns from this project" |
| **Entry** | `[session: site-revolution-architecture > design-patterns > dp-001]` | One specific entry | "How does the self-contained landmarks pattern work?" |
| **Multi-entry** | `[session: site-revolution-architecture > constraints > cl-001, cl-003]` | Multiple specific entries | "Show me these two specific constraints" |

### Section Name Mapping

| Section # | Section Name for References |
|---|---|
| 1 | `summary` |
| 2 | `architecture-decisions` |
| 3 | `design-patterns` |
| 4 | `shared-state` |
| 5 | `constraints` |
| 6 | `reusable-references` |

---

## Resolution Protocol

When an AI encounters a reference, it must follow this resolution protocol:

### Step 1: Locate the File

1. Look in the configured sessions directory for `<filename>.md`
2. If not found, look for `<filename>-architecture.md`
3. If still not found, flag as unresolvable: `[UNRESOLVED: <filename> not found in sessions directory]`

### Step 2: Extract the Target

| If Level Is | Action |
|---|---|
| **File** | Read only Section 1 (Summary). Return the summary block. |
| **Section** | Read the target section header through the next section header (or EOF). Return the entire section. |
| **Entry** | Read from the entry's `### <id>` heading through the next `###` heading (or section boundary). Return the entry block. |
| **Multi-entry** | Resolve each entry ID independently. Return all matched entries in order. |

### Step 3: Validate

- If a section name doesn't match any section in the file → `[UNRESOLVED: section '<name>' not found in <filename>]`
- If an entry ID doesn't match any entry in the section → `[UNRESOLVED: entry '<id>' not found in <filename> > <section>]`
- Unresolved references must be surfaced to the user, never silently dropped

---

## Embedding References in Session Files

References can appear anywhere in a session context block to pull in architectural knowledge.

### In Section 1 (Guidelines)

```markdown
### 1.5 Accessibility
*   Self-contained landmarks — [session: site-revolution-architecture > design-patterns > dp-001]
*   Conditional aria-labelledby for slotted headings — [session: site-revolution-architecture > design-patterns > dp-007]
```

**Behavior:** When the AI loads this session's context block, it resolves each reference and injects the referenced entry inline (or reads it on-demand depending on context budget).

### In Section 2 (Decisions)

```markdown
### 2.3 Key Decisions
5. **(2026-04-25)** Adopted cookie-based cross-app state — [session: site-revolution-architecture > architecture-decisions > ad-005]
```

**Behavior:** The referenced decision provides full context (alternatives, consequences) without duplicating it in the session file.

### In Section 3 (Implementations)

```markdown
### 3.2 New Booking Flow
**Architecture constraint:** CMS Partials require SSR store bridging — [session: site-revolution-architecture > constraints > cl-002]
**Pattern used:** Experiment-gated redirect — [session: site-revolution-architecture > design-patterns > dp-004]
```

**Behavior:** Implementation sections reference architectural knowledge instead of re-explaining it.

---

## Cross-File References

References can point across architecture memory files:

```markdown
**Navigation pattern:** [session: navigation-redesign-architecture > design-patterns > dp-003]
**SSR constraint:** [session: mr-ssr-architecture > constraints > cl-001]
```

This enables a single session to compose knowledge from multiple architecture memory files without duplicating content.

---

## Reference Resolution in Context-Constrained Environments

When context budget is limited, references follow a **lazy loading** protocol:

| Budget Status | Resolution Strategy |
|---|---|
| **Ample** (>50% remaining) | Resolve all references inline — inject full entry content |
| **Moderate** (25-50% remaining) | Resolve on-demand — only resolve references when the AI needs the knowledge for the current task |
| **Tight** (<25% remaining) | Resolve minimally — load only entry titles and one-line summaries |
| **Critical** (<10% remaining) | Skip resolution — leave references as pointers for the user to expand |

---

## Creating References During Extraction

When the extraction process (see `reset-integration.md`) identifies knowledge that already exists in an architecture memory file, it creates a reference instead of duplicating the knowledge:

### Deduplication Check

```
For each extracted item:
    1. Search all architecture memory files for matching knowledge
    2. Match criteria: same decision/pattern/constraint with ≥80% semantic overlap
    3. If match found → create reference to existing entry
    4. If no match → create new entry in the appropriate memory file
```

### Reference vs. New Entry Decision

| Situation | Action |
|---|---|
| Extracted knowledge matches an existing entry exactly | Create reference to existing entry |
| Extracted knowledge extends an existing entry | Update existing entry + create reference |
| Extracted knowledge contradicts an existing entry | Flag as conflict (see `conflict-resolution.md`) |
| Extracted knowledge is entirely new | Create new entry with new ID |

---

## Correct vs. Incorrect Reference Usage

### Example 1: Precision Level

**Incorrect (too broad):**
```markdown
For SSR constraints, see [session: site-revolution-architecture]
```

**Correct (precise entry):**
```markdown
CMS Partials require SSR store bridging — [session: site-revolution-architecture > constraints > cl-002]
```

**Why:** The broad reference loads the entire summary section, requiring the AI to search for the relevant constraint. The precise reference loads exactly the 5-10 lines needed.

### Example 2: Deduplication

**Incorrect (duplicated knowledge):**
```markdown
## Session B Context Block
### 1.5 Accessibility
*   Self-contained landmarks: each component owns `role="region"` + `aria-labelledby`
    + heading `id` in its own template. Parent wrappers are purely structural with
    no ARIA attributes. [... 15 more lines explaining the full pattern ...]
```

**Correct (reference to existing):**
```markdown
## Session B Context Block
### 1.5 Accessibility
*   Self-contained landmarks — [session: site-revolution-architecture > design-patterns > dp-001]
```

**Why:** The pattern was already fully documented in the architecture memory file. Duplicating it in every session that uses it wastes tokens and creates divergence risk (if the pattern evolves, all copies must be updated vs. one source of truth).

### Example 3: Multi-Entry Reference

**Incorrect (multiple separate references for related entries):**
```markdown
See [session: mr-ssr-architecture > constraints > cl-001]
Also see [session: mr-ssr-architecture > constraints > cl-003]
And [session: mr-ssr-architecture > constraints > cl-005]
```

**Correct (batched multi-entry):**
```markdown
SSR constraints for this feature: [session: mr-ssr-architecture > constraints > cl-001, cl-003, cl-005]
```

**Why:** The multi-entry syntax allows a single resolution operation to fetch all three entries, reducing overhead and keeping references scannable.
