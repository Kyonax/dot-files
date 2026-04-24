---
title: Integration with Session Reset — Extraction Pipeline and Updated Flow
impact: HIGH
impactDescription: Defines how the architecture extraction process integrates into the existing session-reset skill's 5-step flow. Without this integration, architectural knowledge is lost when sessions are reset — the context block captures everything but extracts nothing reusable.
tags: session-reset, integration, extraction, pipeline, flow, step, merge, update, create, architecture, memory, hook, session, compaction, knowledge-extraction
---

This rule defines how the architecture memory extraction process integrates into the existing `/session-reset` skill's 5-step flow. Without this integration, session resets compact knowledge into context blocks but never extract reusable architectural patterns into persistent memory files — meaning the same patterns must be re-discovered in every new session.

## The Problem This Solves

The existing session-reset flow (Step 1: Determine File → Step 2: Gather → Step 3: Merge → Step 4: Write → Step 5: Output) is designed to produce a single context block for session continuity. It does NOT:

- Identify which knowledge is reusable across sessions
- Extract architectural decisions into a separate persistent file
- Create references between the context block and architecture memory files
- Deduplicate knowledge that already exists in architecture memory

This integration adds a **Step 2.5** to the existing flow and modifies **Step 4** to use references.

## Updated Session Reset Flow (6 Steps)

```
Step 1: Determine Session File (unchanged)
    ↓ produces: file path + existing context block (if any)
Step 2: Gather Knowledge (unchanged)
    ↓ produces: categorized knowledge from current conversation
Step 2.5: Extract Architecture Memory (NEW)
    ↓ produces: new/updated entries in architecture memory file(s) + reference list
Step 3: Merge with Existing Context Block (unchanged)
    ↓ produces: unified, non-redundant context block content
Step 4: Write the Context Block (MODIFIED — uses references)
    ↓ produces: formatted markdown with architecture references instead of duplicated knowledge
Step 5: Write to File (unchanged)
    ↓ produces: final session file on disk
```

---

## Step 2.5: Extract Architecture Memory (Detailed)

This step runs AFTER knowledge gathering (Step 2) and BEFORE merging (Step 3).

### 2.5.1 — Identify Extraction Candidates

Scan all gathered knowledge from Step 2 against the extraction criteria (see `extraction-criteria.md`):

```
For each piece of gathered knowledge:
    Apply the extraction decision flowchart:
    1. Does it influence decisions in future sessions on different features? → candidate
    2. Is it already captured in a loaded skill? → skip
    3. Can it be derived from reading the current codebase trivially? → skip
    4. Is it generalizable? → extract and generalize
    5. Assign to category: decision / pattern / data-flow / abstraction / constraint / reference
```

**Output:** A list of extraction candidates, each tagged with:
- Category (maps to architecture memory section)
- Generalization needed (yes/no — if yes, abstract before storing)
- Existing match (reference to an entry in an architecture memory file, if any)

### 2.5.2 — Determine Target Architecture Memory File

| Situation | Action |
|---|---|
| An architecture memory file already exists for this domain | Use it |
| No architecture memory file exists, but 5+ candidates identified | Create one (name: `{session-domain}-architecture.md`) |
| No architecture memory file exists, <5 candidates | Hold — store in session context block, extract on next reset if pattern continues |
| Candidates span multiple domains | Split into separate architecture memory files per domain |

### 2.5.3 — Deduplicate Against Existing Entries

For each candidate, check if a matching entry already exists:

| Check | How |
|---|---|
| Exact match | Same decision/pattern with identical rationale → skip (already stored) |
| Semantic overlap ≥80% | Same core knowledge, different wording → update existing entry with any new details |
| Extension | New knowledge extends an existing entry (adds alternatives, new validated-in locations, new gotchas) → update existing entry |
| Contradiction | New knowledge contradicts an existing entry → flag for conflict resolution (see `conflict-resolution.md`) |
| No match | New knowledge entirely → create new entry |

### 2.5.4 — Write/Update Architecture Memory File

For new entries:
1. Assign the next available ID in the appropriate section (e.g., `ad-013`, `dp-009`)
2. Write the full entry following the format in `session-file-template.md`
3. Update Section 1 (Summary) — increment knowledge category counts, add source session

For updated entries:
1. Modify the existing entry in place
2. Update `Last updated` in Section 1
3. Add the current session to `Source sessions` list

### 2.5.5 — Generate Reference List

For every extracted/matched entry, generate a reference that will be used in Step 4:

```markdown
# Reference list for Step 4
- Self-contained landmarks pattern → [session: site-revolution-architecture > design-patterns > dp-001]
- CSS accordion decision → [session: site-revolution-architecture > architecture-decisions > ad-001]
- CMS parseUrl constraint → [session: site-revolution-architecture > constraints > cl-001]
```

This list is passed to Step 4 so the context block writer can use references instead of duplicating knowledge.

---

## Modified Step 4: Write Context Block with References

The standard session-reset Step 4 ("Write the Context Block") is modified:

### Before (Standard Session Reset)

Section 1 contains full inline knowledge:
```markdown
### 1.5 Accessibility
*   Self-contained landmarks: each component owns `role="region"` + `aria-labelledby`
    + heading `id` in its own template. Parent wrappers are purely structural...
    [15 more lines of full pattern explanation]
```

### After (With Architecture Memory Integration)

Section 1 uses references for knowledge that lives in architecture memory:
```markdown
### 1.5 Accessibility
*   Self-contained landmarks — [session: site-revolution-architecture > design-patterns > dp-001]
*   Conditional aria-labelledby for slotted headings — [session: site-revolution-architecture > design-patterns > dp-007]
*   WCAG 2.5.3 multi-content card pattern — [session: site-revolution-architecture > design-patterns > dp-012]
```

### What Gets Referenced vs. What Stays Inline

| Knowledge Type | Storage | Rationale |
|---|---|---|
| Generalizable pattern validated across 2+ features | Architecture memory (referenced) | Reusable across sessions |
| Session-specific guideline not yet validated broadly | Inline in context block | Still in staging — may not generalize |
| One-off decision for a specific ticket | Inline in context block | Not reusable, but needed for session continuity |
| Platform constraint affecting all future work | Architecture memory (referenced) | Critical reusable knowledge |
| Implementation detail for one component | Inline in context block | Not architectural — session-scoped |

### Reference Density Rule

**Do not reference everything.** The context block must still be readable as a standalone document. Rules of thumb:

- If a guideline can be stated in 1-2 lines, keep it inline even if a reference exists
- If a guideline requires 5+ lines to explain fully, use a reference
- Section 3 (Implementations) stays mostly inline — it's per-ticket detail, not architecture
- Section 2.3 (Decisions) references architecture decisions for rationale, keeps a one-line summary inline

---

## When to Skip Step 2.5

Step 2.5 is **optional** for:

| Situation | Reason |
|---|---|
| First session in a new project domain | Not enough knowledge to extract meaningfully — let patterns emerge |
| Session produced only implementation work (bug fixes, style changes) | No architectural decisions were made |
| Session was <30 minutes of simple tasks | Insufficient depth for architectural patterns |

Step 2.5 is **mandatory** for:

| Situation | Reason |
|---|---|
| Session produced 3+ architectural decisions | Decisions must be captured while rationale is fresh |
| Session validated or refined an existing pattern | Architecture memory entries need updating |
| Session discovered a new platform constraint | Constraints affect all future work |
| User explicitly requests architecture extraction | Direct instruction |

---

## Correct vs. Incorrect Integration

### Example 1: Step Ordering

**Incorrect:** Extracting architecture after writing the context block.
```
Step 2: Gather
Step 3: Merge
Step 4: Write context block (full inline knowledge)
Step 5: Output
Step 6: Extract architecture (afterthought)
```

**Correct:** Extracting before writing, so references are available.
```
Step 2: Gather
Step 2.5: Extract architecture → generates reference list
Step 3: Merge
Step 4: Write context block (uses references from 2.5)
Step 5: Output
```

**Why:** If extraction happens after the context block is written, the block contains duplicated knowledge instead of references. The block must be written WITH references already available.

### Example 2: Deduplication

**Incorrect:** Creating a new architecture decision that already exists.
```
Candidate: "CSS max-height accordion over TransitionExpand"
Check: [skipped]
Action: Create ad-042 with full entry
Result: ad-042 duplicates ad-001 from a previous extraction
```

**Correct:** Checking for existing entries first.
```
Candidate: "CSS max-height accordion over TransitionExpand"
Check: Search architecture memory → found ad-001 (exact match)
Action: Skip creation, add reference to ad-001
Update: Add current session to ad-001's validated-in list if new validation context
```

**Why:** Duplicate entries create divergence. When one is updated, the other becomes stale. The deduplication check ensures one source of truth per decision.
