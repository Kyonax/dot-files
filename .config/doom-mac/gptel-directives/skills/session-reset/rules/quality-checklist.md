---
title: Context Block Quality Checklist
impact: MEDIUM
impactDescription: Provides the mandatory verification checklist that must be run before finalizing every session reset. Skipping verification produces context blocks with missing sections, stale data, or broken cross-references.
tags: quality, checklist, verification, review, validate, finalize, sections, completeness, accuracy, cross-reference, dates, line-count
---

This rule provides the mandatory verification checklist that must be run before finalizing every session reset. Without verification, context blocks accumulate subtle errors — missing dates, stale statuses, broken cross-references, orphaned file index entries — that compound over successive resets until the context block becomes unreliable.

## When to Run

Run this checklist **after Step 4 (Write the Context Block)** and **before Step 5 (Write to File)**. Every item must pass. If any item fails, fix it before writing.

---

## Section 1: Global Guidelines

- [ ] All numbered guideline subsections are present and accurate
- [ ] Intro blockquote names the specific loaded skills for this session
- [ ] Each rule is enforceable (specific and actionable, not vague guidance)
- [ ] No implementation details leaked from Section 3
- [ ] Guidelines already captured in loaded skills are either removed or marked as graduated with a pointer
- [ ] No duplicate rules (same constraint stated in different subsections)

## Section 2: Session Overview

- [ ] Scope table has every item with **current** status (not stale from a previous reset)
- [ ] Every key decision includes a date in `(YYYY-MM-DD)` format
- [ ] Architectural decisions are session-wide, not item-specific
- [ ] Review resolutions are documented (if applicable)
- [ ] Pending work is up to date (completed items removed, new items added)

## Section 3: Implementations

- [ ] Every item has `Created:` and `Last updated:` dates
- [ ] Every item has a current `Status:` field
- [ ] Key decisions table includes dates for every decision (if table exists)
- [ ] Cross-item references point to the correct section numbers (e.g., "see Section 3.2")
- [ ] Compressed entries retain: title, dates, status, key decisions summary, file pointer
- [ ] Tombstoned entries retain: title, completion date, branch name, recovery pointer

### Code-Specific Checks (when applicable)
- [ ] Every item involving multiple components has a component tree diagram
- [ ] Every component in the tree has a documentation block (even pre-existing dependencies)
- [ ] Every component block includes at minimum: Path, Role, Parent, Store
- [ ] Test summaries include counts and coverage areas

## Section 4: File Index

- [ ] All files mentioned in Section 3 appear in the index
- [ ] No duplicate entries
- [ ] Compressed items have directory-level summaries (not individual files)
- [ ] File paths are consistent (all absolute or all project-relative)

## Section 5: Last Interaction

- [ ] Reflects ONLY the most recent conversation (not accumulated history)
- [ ] "What was done last" has 3-5 bullets
- [ ] "Where to resume" has conditional paths for each open item
- [ ] Includes a fallback "new task" path pointing to Section 2.4
- [ ] Pending items are comprehensive (session-scope + item-specific)

## Architecture Memory (When Step 2.5 Was Performed)

- [ ] Architecture memory file exists and is structurally valid (6 sections present)
- [ ] All new entries have unique, stable IDs with correct prefixes (`ad-`, `dp-`, `sf-`, `cl-`, `rr-`)
- [ ] Extracted knowledge passes the reusability test ("influences future sessions on different features")
- [ ] No implementation noise leaked into architecture memory (one-off fixes, debugging steps, test counts)
- [ ] Deduplication check was performed — no duplicate entries created
- [ ] Contradictions flagged as "contested" with cross-references (not silently overwritten)
- [ ] Architecture memory Summary (Section 1) updated with source session and knowledge counts
- [ ] Context block references resolve correctly (file exists, section exists, entry ID exists)
- [ ] Reference density is reasonable (<20 references per context block)
- [ ] Short guidelines (1-2 lines) kept inline even when a reference exists — references used only for 5+ line explanations

## Overall

- [ ] No raw conversation fragments remain — everything is abstracted
- [ ] A future AI reading ONLY the context block can continue work immediately
- [ ] All dates are absolute (YYYY-MM-DD), no relative dates ("yesterday", "last week")
- [ ] Line count is at or below 3693 lines (compression protocol applied if needed)
- [ ] Impact assessment was run before compression (if compression was needed)
- [ ] `[CRITICAL]` entries survived intact — none were compressed or tombstoned
- [ ] `[HIGH]` entries were not tombstoned (only compressed to summary at most)
- [ ] Transient `[labels]` removed from surviving uncompressed entries
- [ ] Compressed/tombstoned entries retain their impact label as metadata
- [ ] Compression log entry added with impact distribution (if compression was applied)
- [ ] Context block is enclosed in proper delimiter comments
- [ ] 5-section structure is intact (no merged, missing, or reordered sections)

---

## Common Failure Patterns

| Failure                                         | Where to look     | Fix                                                                   |
|-------------------------------------------------|-------------------|-----------------------------------------------------------------------|
| Stale status in scope table                     | Section 2.2       | Update status to match current reality                                |
| Missing dates on decisions                      | Sections 2.3, 3.N | Add `(YYYY-MM-DD)` or `(~YYYY-MM-DD)` if approximate                  |
| Section 5 has accumulated history               | Section 5         | Replace entirely with most recent conversation only                   |
| Orphaned file index entries                     | Section 4         | Remove entries for files no longer referenced in Section 3            |
| Guidelines duplicating loaded skills            | Section 1         | Remove and add graduation pointer                                     |
| Cross-reference points to wrong section         | Section 3         | Update `3.N` references after any reordering                          |
| Block exceeds 3693 lines                         | Overall           | Run impact assessment, then apply compression (Levels 1-5, LOW first) |
| CRITICAL entry was compressed                   | Section 3         | Restore it — CRITICAL entries must survive all compression levels     |
| Impact labels left on uncompressed entries      | Sections 1-4      | Remove transient labels after compression completes                   |
| Component tree missing for multi-component item | Section 3         | Add ASCII tree diagram                                                |
| Architecture reference points to nonexistent entry | Sections 1-3   | Verify entry ID exists in the target architecture memory file         |
| Implementation noise in architecture memory     | Arch. memory      | Apply extraction criteria — remove one-off fixes, debugging steps     |
| Duplicate entry in architecture memory          | Arch. memory      | Merge entries, update references to point to surviving entry          |
| Contradiction not flagged                       | Arch. memory      | Mark both entries as "contested", add cross-references                |

---

## Correct vs. Incorrect Examples

### Example 1: Post-Compression Verification

**Incorrect:** Writing to file immediately after compression without checking.
```
Step 3: Merge → 1,087 lines → Compress Levels 1-2 → 891 lines → Write to file
```

**Correct:** Running checklist after compression, before writing.
```
Step 3: Merge → 1,087 lines → Compress Levels 1-2 → 891 lines
Step 4: Run quality checklist → Found: 2 stale statuses, 1 orphaned file entry → Fixed
Step 5: Write to file (889 lines, all checks passing)
```

**Why:** Compression can create inconsistencies — a compressed implementation may leave orphaned file index entries or stale scope table rows. The checklist catches these.

### Example 2: Date Verification

**Incorrect:** Accepting entries without dates.
```markdown
### 2.3 Key Decisions
1. Switched to GraphQL for search
2. Added rate limiting to API
```

**Correct:** Flagging and fixing missing dates.
```markdown
### 2.3 Key Decisions
1. **(~2026-01-20)** Switched to GraphQL for search — approximate date, verify if needed
2. **(2026-03-10)** Added rate limiting to API — required for production stability
```

**Why:** The compression protocol relies on dates to determine which entries to compress first. Missing dates prevent correct priority ordering and may cause recent entries to be compressed before old ones.
