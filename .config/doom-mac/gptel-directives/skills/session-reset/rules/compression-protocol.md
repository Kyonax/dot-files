---
title: Context Block Compression Protocol
impact: CRITICAL
impactDescription: Defines the mandatory compression flow when a context block exceeds 3693 lines. Without this protocol, context blocks grow unbounded until they exceed AI context windows, causing truncation and knowledge loss.
tags: compression, compaction, line-limit, threshold, hierarchy, priority, dates, aging, removal, reference, archive, overflow, trim, reduce, oldest-first, smart-removal, impact-assessment, session-value, critical, shielded, triage
---

This rule defines the mandatory compression protocol for context blocks that exceed 3693 lines of text. Without bounded compression, context blocks grow indefinitely — eventually exceeding AI context windows and causing truncation that loses the most recent (and most relevant) knowledge. The compression protocol ensures context blocks remain within budget while preserving maximum relevance for the next possible request.

## The Compression Threshold

**Maximum context block size: 3693 lines.** Every session reset must check the line count after merging. If the result exceeds 3693 lines, the compression protocol is mandatory before writing to file.

**Target:** At or below 3693 lines. Aim for less when possible — headroom allows the next session to add knowledge before triggering compression again.

**What counts:** All lines between `<!-- DESCRIPTION AND USER CONTEXT START -->` and `<!-- DESCRIPTION AND USER CONTEXT END -->`, including blank lines, table separators, and code blocks.

---

## Core Principle: Preserve Next-Request Context

The compression protocol's primary purpose is to **keep the context most likely to be needed by the next request.** Every compression decision must be evaluated against this question:

> "If the user's next message relates to active work, will the AI have enough context to respond correctly?"

If removing an entry would leave the AI unable to answer a likely follow-up — keep it. If an entry describes completed, stable, old work that's unlikely to be referenced — compress or remove it.

---

## Impact Assessment (Pre-Compression Step)

Before applying any compression, **every compressible entry must be evaluated and labeled** with a session value level. This assessment creates a two-axis priority system — **age × impact** — that prevents the protocol from blindly compressing old-but-critical knowledge.

**When to run:** After the merge step produces a context block exceeding 3693 lines, but **before** applying the compression hierarchy. The assessment is mandatory — compression without it risks destroying entries that provide essential session coherence.

### Session Value Levels

| Level        | Label      | Meaning                                                                                                                                                        | Compression Behavior                                                                        |
|--------------|------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------|
| `[CRITICAL]` | Shielded   | Entry provides context that the session **cannot function without**. Removing it would break consistency, lose a foundational decision, or orphan active work. | **Never compressed or removed.** Survives all 5 compression levels.                         |
| `[HIGH]`     | Protected  | Entry provides important context that is actively referenced or establishes patterns used by other entries.                                                    | Compressed only at Level 5 (last resort), and only to summary reference — never tombstoned. |
| `[MEDIUM]`   | Standard   | Entry provides useful context but is not actively referenced by current work. Completed, stable, self-contained.                                               | Compressed at Level 2+ following normal age-first rules.                                    |
| `[LOW]`      | Expendable | Entry is redundant, superseded, or covers work so far removed from the session's focus that its absence would not affect any likely future request.            | Compressed first within each level. Tombstoned at Level 5.                                  |

### How to Assess

Evaluate each entry against these criteria, in order of weight:

**1. Session Focus Alignment** — Does the entry directly relate to the session's stated purpose (Section 2.1)?
- Directly aligned → raises impact
- Tangentially related → neutral
- Unrelated to current focus → lowers impact

**2. Active Reference Count** — Is the entry referenced by other entries in the context block?
- Referenced by active (in-progress) items → CRITICAL or HIGH
- Referenced by completed items only → MEDIUM
- Not referenced by anything → MEDIUM or LOW

**3. Pattern Establishment** — Does the entry define a pattern, convention, or decision that other entries depend on?
- Establishes a Section 1 guideline → CRITICAL
- Establishes a pattern used by 2+ implementations → HIGH
- Self-contained, no downstream dependents → MEDIUM or LOW

**4. Recoverability** — Can the knowledge be recovered from external sources if removed?
- Only exists in this context block → raises impact by one level
- Also exists in git history, documentation, or skills → neutral
- Fully captured in a loaded skill → lowers impact (candidate for graduation)

**5. Temporal Relevance** — How likely is the next request to need this entry?
- Active work, in-progress → CRITICAL
- Completed but < 2 weeks old → HIGH
- Completed and > 2 weeks old → MEDIUM
- Completed and > 2 months old → LOW (unless other criteria raise it)

### Assessment Output Format

Label each compressible entry inline with its value level. The label goes after the section/subsection title:

```markdown
### 3.1 PROJ-1234: Gallery Redesign `[LOW]`
### 3.2 PROJ-5678: Search Feature `[CRITICAL]`
### 3.3 PROJ-9012: Auth Refactor `[MEDIUM]`

### 2.3 Key Decisions
1. **(2025-12-10)** Adopted standard frontend stack `[HIGH]`
2. **(2026-03-01)** Switched search to GraphQL `[CRITICAL]`
3. **(2025-11-15)** Legacy migration cleanup `[LOW]`

### 1.3 Testing Conventions `[MEDIUM]`
### 1.5 Accessibility Requirements `[HIGH]`
```

**Labels are transient** — they exist during the compression step to guide decisions. After compression is complete, remove the labels from entries that survived uncompressed. Compressed entries (summary references) and tombstones retain their label as metadata for future compression cycles:

```markdown
### 3.1 PROJ-1234: Gallery Redesign (COMPRESSED) `[LOW]`
**Created:** 2025-12-10 | **Completed:** 2026-01-15 | **Status:** Released
...
```

### Assessment for Each Section

| Section                          | What to assess                                 | Typical pattern                                                                          |
|----------------------------------|------------------------------------------------|------------------------------------------------------------------------------------------|
| **Section 1** (Guidelines)       | Each numbered subsection (1.1, 1.2, ...)       | Guidelines aligned with session focus → HIGH/CRITICAL. Generic or graduated → LOW.       |
| **Section 2.3** (Decisions)      | Each numbered decision                         | Decisions driving active work → CRITICAL. Old, fully-implemented decisions → MEDIUM/LOW. |
| **Section 2.4** (Pending)        | Each pending item                              | All pending items are inherently HIGH or CRITICAL (they represent future work).          |
| **Section 3** (Implementations)  | Each implementation subsection (3.1, 3.2, ...) | In-progress → CRITICAL. Recently completed → HIGH/MEDIUM. Old completed → MEDIUM/LOW.    |
| **Section 4** (File Index)       | Entries grouped by their Section 3 association | Inherits the impact level of the associated Section 3 entry.                             |
| **Section 5** (Last Interaction) | Never assessed                                 | Always fully replaced — not subject to compression.                                      |

---

## Compression Hierarchy (Priority Order)

When compression is needed, apply these strategies **in order**, stopping as soon as the line count drops below 3693. Never skip to a later strategy while an earlier one still has candidates.

**Impact-aware ordering:** Within each level, process entries in this order: `[LOW]` first, then `[MEDIUM]`, then `[HIGH]`. **Never compress `[CRITICAL]` entries** at Levels 1-4. At Level 5, `[CRITICAL]` entries are still shielded — only `[LOW]` and `[MEDIUM]` entries can be tombstoned.

### Level 1: Remove Graduated/Redundant Guidelines (Section 1)

**What to remove:** Guidelines that have already been promoted into a loaded skill's rule files, or that duplicate knowledge the AI already has from loaded skills.

**How to verify:** If the guideline is already enforced by a skill rule the session loads, it's redundant. Remove it and add a one-line pointer:

```markdown
*   ~~Utility class usage~~ — Promoted to frontend-dev skill (`rules/utility-classes.md`)
```

**Or simply remove with a grouped note at the end of Section 1:**
```markdown
> **Graduated rules (removed from this block):** Utility class usage → frontend-dev skill. Component naming → frontend-dev skill. PR format → workflow skill.
```

### Level 2: Compress Oldest Implementations (Section 3)

**What to compress:** Implementation entries for completed, stable items — starting from the **oldest `[LOW]`** entries, then oldest `[MEDIUM]`, then oldest `[HIGH]`. Skip `[CRITICAL]` entries entirely.

**How to compress:** Replace the full entry with a **summary reference block** — a 3-5 line compressed version that preserves enough to locate the original knowledge if needed later:

**Before (full entry, ~40 lines):**
```markdown
### 3.1 PROJ-1234: Gallery Redesign

**Created:** 2025-12-10 | **Last updated:** 2026-01-15
**Status:** Complete, merged, released

#### Component Tree
[... 15 lines ...]

#### Components Created / Modified
[... detailed blocks for 4 components ...]

#### Key Decisions
| Decision | Date | Rationale |
| Used CSS Grid over Flexbox | 2025-12-12 | 2D layout needed for gallery |
| Added skeleton loading | 2025-12-14 | Prevents CLS during image load |

#### Tests (8 total)
[... test summary ...]
```

**After (summary reference, ~5 lines):**
```markdown
### 3.1 PROJ-1234: Gallery Redesign (COMPRESSED)

**Created:** 2025-12-10 | **Completed:** 2026-01-15 | **Status:** Released
**Key decisions:** CSS Grid for 2D layout, skeleton loading for CLS prevention.
**Files:** 4 components in `src/components/gallery/` — see Section 4 file index. Full history in git branch `PROJ-1234`.
```

**Compression rules for implementations:**
- Always preserve: item title, dates (created/completed), status, branch name
- Always preserve: key decisions as a single comma-separated line
- Always preserve: file location pointer (directory or Section 4 reference)
- Remove: component trees, detailed component blocks, test details, PR body
- Remove: dependency component documentation for completed items

### Level 3: Compress Oldest Decisions (Section 2.3)

**What to compress:** Session-wide decisions that are old, fully implemented, and labeled `[LOW]` or `[MEDIUM]` — no longer actively referenced. Skip `[CRITICAL]` and `[HIGH]` decisions.

**How to compress:** Merge multiple related old decisions into a single summary line:

**Before:**
```markdown
1. **(2025-12-10)** Adopted Vue 3 Composition API for new components — better TypeScript support
2. **(2025-12-12)** Chose Pinia over Vuex for new stores — simpler API, better DX
3. **(2025-12-15)** Standardized on Vitest over Jest for frontend — faster, native ESM
```

**After:**
```markdown
1. **(2025-12 batch)** Adopted Vue 3 Composition API + Pinia + Vitest as standard frontend stack.
```

### Level 4: Trim File Index (Section 4)

**What to trim:** File entries for completed items whose implementations were compressed in Level 2.

**How to trim:** Replace individual file rows with a directory-level summary:

**Before:**
```markdown
| `src/components/gallery/GalleryGrid.vue` | PROJ-1234 |
| `src/components/gallery/GalleryItem.vue` | PROJ-1234 |
| `src/components/gallery/GalleryModal.vue` | PROJ-1234 |
| `src/components/gallery/GalleryGrid.test.js` | PROJ-1234 |
```

**After:**
```markdown
| `src/components/gallery/*` (4 files) | PROJ-1234 |
```

### Level 5: Remove Completed Non-Referenced Items (Section 3)

**Last resort.** Only if Levels 1-4 are exhausted and the block still exceeds 3693 lines.

**What to remove:** Section 3 entries that are:
- Labeled `[LOW]` or `[MEDIUM]` (never `[HIGH]` or `[CRITICAL]`)
- Completed AND released/merged
- NOT referenced by any active item in the session
- Older than the oldest active item

`[HIGH]` entries at Level 5 may be compressed to summary reference (Level 2 treatment) but **never tombstoned**. `[CRITICAL]` entries are fully shielded — they survive Level 5 intact.

**How to remove safely:** Replace with a **tombstone** — a single line that records the item existed and where to find its history:

```markdown
### 3.1 ~~PROJ-1234: Gallery Redesign~~ (ARCHIVED)
> Completed 2026-01-15. Branch: `PROJ-1234`. Git history and project documentation contain full context.
```

**Never remove an item if:**
- It is referenced by name in another Section 3 entry ("see Section 3.1 for full docs")
- It established a pattern still listed in Section 1
- Its status is anything other than complete/released/merged
- It is the only source of a key decision that hasn't been promoted to a skill

---

## Date Requirement

Every entry that can be compressed must have a date. The compression protocol depends on dates to determine priority — older entries compress first.

**If an entry lacks a date:**
1. Infer from context (git commits, conversation timestamps, surrounding entries)
2. Use approximate dates with `~` prefix: `~2026-02-10`
3. If no date can be inferred, treat as **current** (do not compress yet) — and add a date on the next reset when more context is available

---

## Compression Decision Flowchart

```
Context block > 3693 lines?
    │
    ├── No → Write as-is
    │
    └── Yes
        │
        ├── STEP 0: Impact Assessment
        │   Evaluate every compressible entry → assign [CRITICAL], [HIGH], [MEDIUM], [LOW]
        │   [CRITICAL] entries are shielded from all compression levels
        │
        └── Apply compression hierarchy (within each level: LOW first → MEDIUM → HIGH):
            │
            ├── Level 1: Remove graduated/redundant guidelines ([LOW] and [MEDIUM] only)
            │   └── Still over? Continue ↓
            │
            ├── Level 2: Compress oldest implementations ([LOW] first, then [MEDIUM], then [HIGH])
            │   └── Still over? Continue ↓
            │
            ├── Level 3: Compress oldest decisions ([LOW] and [MEDIUM] only)
            │   └── Still over? Continue ↓
            │
            ├── Level 4: Trim file index (inherits associated entry's impact)
            │   └── Still over? Continue ↓
            │
            ├── Level 5: Tombstone [LOW]/[MEDIUM] completed items. Compress [HIGH] to summary.
            │   └── Still over? Flag to user — session may need splitting
            │
            └── Remove transient [labels] from surviving uncompressed entries
```

**If Level 5 is exhausted and the block still exceeds 3693 lines:** Alert the user. The session likely covers too many domains and should be split into separate session files (one per domain/project).

---

## Compression Log

Every time compression is applied, add a note in the opening section of the context block (after the compaction sources list):

```markdown
**Compression applied:** YYYY-MM-DD — Levels 1-2. Impact assessment: 2 CRITICAL (shielded), 3 HIGH, 5 MEDIUM, 4 LOW. Compressed 3 implementations [LOW/MEDIUM] (ITEM-A, ITEM-B, ITEM-C). Removed 4 graduated guidelines. Reduced from 1,087 to 891 lines.
```

This log helps future sessions understand what was compressed and where to find the full history if needed.

---

## Correct vs. Incorrect Examples

### Example 1: Compression Priority

**Incorrect:** Compressing the most recent implementation to save space.
```
Block is 1,050 lines. Most recent item (3 days old, in progress) is compressed
to make room. Older completed items remain fully expanded.
```

**Correct:** Compressing the oldest completed implementation first.
```
Block is 1,050 lines. Oldest completed item (3 months old, released) is compressed
to summary reference. Recent in-progress items remain fully expanded.
```

**Why:** The next request is far more likely to reference recent, active work than old, completed work. Compress what's least likely to be needed next.

### Example 2: Removal vs. Reference

**Incorrect:** Deleting an old entry completely with no trace.
```markdown
### 3.1 [deleted — too old]
```

**Correct:** Leaving a tombstone with recovery pointers.
```markdown
### 3.1 ~~PROJ-1234: Gallery Redesign~~ (ARCHIVED)
> Completed 2026-01-15. Branch: `PROJ-1234`. Project documentation contains full context.
```

**Why:** Complete deletion loses the knowledge that the item ever existed. A tombstone preserves the thread — if the user ever asks "what happened with the gallery redesign?", the AI knows where to look without having the full details in context.

### Example 3: Impact-Aware Compression

**Incorrect:** Compressing by age alone, ignoring session value.
```
Block is 1,100 lines. Assessment skipped.
Oldest entry (PROJ-100, 4 months old) is compressed — but it established
the authentication pattern that 3 active items depend on. Active items
now reference a compressed summary that lacks the details they need.
```

**Correct:** Running impact assessment, then compressing by age within each impact tier.
```
Block is 1,100 lines. Assessment run:
- PROJ-100 (4 months old) → [CRITICAL] — establishes auth pattern, 3 active references
- PROJ-200 (3 months old) → [LOW] — completed, no references, fully in git
- PROJ-300 (2 months old) → [MEDIUM] — completed, one decision still in Section 1

Compression: PROJ-200 [LOW] tombstoned, PROJ-300 [MEDIUM] compressed to summary.
PROJ-100 [CRITICAL] preserved intact despite being the oldest.
```

**Why:** Age alone is a dangerous heuristic. An old entry that establishes a pattern used by active work is more valuable than a recent entry for completed, unreferenced work. The impact assessment prevents the protocol from destroying foundational context.
