---
title: Context Optimization — Minimal Loading and Deduplication Rules
impact: HIGH
impactDescription: Defines rules for keeping initial context blocks minimal and high-signal by preferring references over copying, avoiding redundant data across sessions, and ensuring all stored data is reusable and relevant. Without these rules, context windows fill with duplicated, stale, or low-value knowledge.
tags: context, optimization, minimal, loading, deduplication, redundancy, reference, token, efficiency, signal, noise, initial-context, high-signal, stale, freshness, budget
---

This rule defines the context optimization strategy for the architecture memory system — how to keep AI context windows minimal and high-signal by preferring references over copying, avoiding redundant data across sessions, and ensuring every piece of stored knowledge justifies its token cost. Without optimization rules, architecture memory files grow unchecked and context blocks accumulate redundant copies of the same knowledge.

## Core Principle: Reference Over Copy

**Every piece of architectural knowledge should have exactly one authoritative location.** All other uses of that knowledge should be references (pointers), not copies. This is the architecture memory equivalent of the DRY principle.

```
One authoritative source + N references
    vs.
N copies that diverge over time
```

---

## Rule 1: Initial Context Must Be Minimal and High-Signal

When loading a session, the initial context injection should include:

| Include | Tokens (approx) | Purpose |
|---|---|---|
| Session context block (Section 5 first) | Varies | Resume point — where to continue |
| Architecture memory summaries (Section 1 only) | ~50-100 per file | Orientation — what knowledge is available |
| Active references from the context block | Resolved on-demand | Only load when the current task needs them |

**Do NOT pre-load:**
- Full architecture memory files
- All sections of all referenced memory files
- Architecture decisions from unrelated domains
- Constraint entries that don't affect the current task

### The Loading Waterfall

```
Level 0: Session context block Section 5 (resume point)
    ↓ AI reads what was done last and what's pending
Level 1: Session context block full (Sections 1-4)
    ↓ AI understands scope, guidelines, implementations
Level 2: Architecture memory summaries (Section 1 of each referenced file)
    ↓ AI knows what architectural knowledge is available
Level 3: Specific architecture entries (on-demand)
    ↓ AI resolves references only when the current task needs them
Level 4: Full architecture sections (rare)
    ↓ Only when performing broad architectural analysis or review
```

---

## Rule 2: Deduplication Is Mandatory

### Cross-Session Deduplication

The same architectural knowledge must NOT appear in multiple places:

| Duplication Type | Detection | Resolution |
|---|---|---|
| Same decision in two architecture memory files | Search by `title` + `decision` field similarity | Merge into the more specific file, reference from the other |
| Same pattern in context block AND architecture memory | Compare context block guidelines with memory file patterns | Replace inline with reference |
| Same constraint described differently | Compare `scope` + `constraint` fields | Merge descriptions, keep one entry |
| Same data flow in multiple session files | Compare `source` + `consumers` fields | Extract to architecture memory, reference from sessions |

### Within-File Deduplication

Architecture memory files must not contain redundant entries:

| Duplication Signal | Action |
|---|---|
| Two entries with overlapping `when-to-use` conditions | Merge into one with broader `when-to-use` |
| A constraint that's just the inverse of a decision | Keep only the decision (it implicitly defines the constraint) |
| A pattern and a decision about the same thing | Decision in Section 2, pattern in Section 3 — each frames it differently, both are valid |

---

## Rule 3: Freshness Tracking

Every piece of stored knowledge has a shelf life. Freshness tracking prevents stale entries from polluting context.

### Freshness Indicators

| Indicator | How to Track | Staleness Signal |
|---|---|---|
| **Last validated date** | `validated-in` field with dates | >3 months since last validation |
| **Source session age** | `source-session` field with date | Source session >6 months old with no updates |
| **Reference count** | How many sessions reference this entry | Zero references in 3+ sessions = candidate for archival |
| **Codebase drift** | Entry describes code that may have changed | Verify against current code before recommending |

### Freshness Check Protocol

Before recommending knowledge from an architecture memory entry:

```
1. Is the entry's source session older than 3 months?
   Yes → Verify the knowledge still holds by checking current code/docs
   No → Safe to recommend

2. Has the entry been referenced by a recent session?
   Yes → Likely still valid
   No → Check if the area of code is still active

3. Does the entry reference specific files, functions, or variables?
   Yes → Verify they still exist (file paths, function names change)
   No → More likely to still be valid (abstract patterns are stable)
```

---

## Rule 4: Granularity Control

Not all knowledge needs the same level of detail in memory. Granularity should match reuse frequency.

### Granularity Levels

| Level | Detail Stored | When to Use | Example |
|---|---|---|---|
| **Full** | Complete entry with all fields | Frequently referenced, broadly applicable | Self-contained landmarks pattern |
| **Summary** | Title + one-line description + reference to source | Occasionally referenced, narrowly applicable | A specific CSS fix pattern |
| **Tombstone** | Title + date + "archived" status | No longer actively referenced but historically relevant | A deprecated pattern replaced by a better one |

### Granularity Decision

```
For each architecture memory entry:
    ├── Referenced in 3+ sessions → Full detail
    ├── Referenced in 1-2 sessions → Full detail (still being validated)
    ├── Completed and not referenced for 3+ months → Compress to Summary
    └── Superseded by another entry → Compress to Tombstone
```

---

## Rule 5: Cross-Domain Isolation

Architecture memory files should not mix unrelated domains:

| Domain Scope | Separate File? | Rationale |
|---|---|---|
| SSR architecture + CMS Partials | Same file if always needed together | Tightly coupled — CMS Partials require SSR knowledge |
| Navigation redesign + Location page | Separate files | Independently referenced — nav work doesn't need location patterns |
| Booking flow + Payment processing | Separate files | Different data flows, different store modules |
| ADA patterns + Component design | Same file if ADA patterns ARE component patterns | ADA patterns here are component-level, not page-level audit |

### The Coupling Test

```
Should domains A and B share a file?
    ├── Would you ever load A without B? → Yes → Separate files
    ├── Would you ever load B without A? → Yes → Separate files
    ├── Are A and B always referenced together? → Yes → Same file
    └── Is one a subset of the other? → Yes → Same file (subset is a section)
```

---

## Rule 6: Token Budget Awareness

Architecture memory must respect practical AI context window limits.

### Budget Guidelines

| Memory File State | Guideline |
|---|---|
| Single architecture memory file | Aim for <500 lines per file (matches skill rule file guidance) |
| Summary section (Section 1) | Must be <30 lines — this is what gets loaded for orientation |
| Individual entry | Aim for 10-30 lines — enough for full context, not a novel |
| Total references in a context block | <20 references — beyond this, the resolution overhead exceeds the deduplication benefit |

### When Budget Is Exceeded

```
Architecture memory file > 500 lines?
    ├── Are there entries from 3+ distinct domains? → Split into domain-specific files
    ├── Are there entries older than 6 months with zero references? → Archive (tombstone)
    ├── Are there entries with >30 lines each? → Compress to 10-15 lines (remove examples, keep decision + rationale)
    └── Still over? → The file covers too broad a domain — split further
```

---

## Correct vs. Incorrect Optimization

### Example 1: Reference Over Copy

**Incorrect (copied knowledge):**
```markdown
## Session A Context Block
### 1.5 Accessibility
*   Self-contained landmarks: each component owns role="region"...
    [15 lines of full explanation]

## Session B Context Block
### 1.3 Accessibility
*   Self-contained landmarks: each component owns role="region"...
    [same 15 lines copied]
```

**Correct (referenced knowledge):**
```markdown
## Session A Context Block
### 1.5 Accessibility
*   Self-contained landmarks — [session: site-revolution-architecture > design-patterns > dp-001]

## Session B Context Block
### 1.3 Accessibility
*   Self-contained landmarks — [session: site-revolution-architecture > design-patterns > dp-001]
```

**Why:** 15 lines × 2 copies = 30 lines of duplicated context. With references, the knowledge is stored once (~20 lines in the memory file) and loaded once (on first resolution). Total savings grow with each additional session.

### Example 2: Minimal Initial Load

**Incorrect (pre-loading everything):**
```
Session start:
  Load full context block (300 lines)
  Load full architecture memory file A (400 lines)
  Load full architecture memory file B (350 lines)
  Total: 1050 lines before the user says anything
```

**Correct (lazy loading):**
```
Session start:
  Load context block Section 5 (20 lines) — resume point
  Load context block Sections 1-4 (280 lines) — full session context
  Load architecture memory summaries (15 lines each × 2 = 30 lines) — orientation
  Total: 330 lines before the user says anything
  Resolve references on-demand as tasks require them
```

**Why:** The incorrect approach fills 1050 lines of context before any work begins. The correct approach uses 330 lines and lazily loads the rest — a 68% reduction in initial context that preserves the same knowledge availability.
