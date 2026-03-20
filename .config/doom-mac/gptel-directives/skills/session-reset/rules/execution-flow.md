---
title: Session Reset Execution Flow
impact: HIGH
impactDescription: Defines the mandatory 5-step process for performing a session reset — from determining the target file through writing the final output. Following the wrong order or skipping steps produces incomplete or corrupted context blocks.
tags: execution, flow, steps, process, reset, merge, gather, write, session, update, create, new-session, existing-session, compaction
---

This rule defines the mandatory 5-step process for performing a session reset. A session reset is a knowledge compaction operation — it extracts all accumulated knowledge from the current conversation, merges it with any existing context block, and writes a single refined output. Skipping steps (e.g., not reading the existing context block before merging) produces data loss or duplication.

## What Is a Session Reset?

A session reset is triggered when:
- A conversation grows long (approaching context limits)
- The user switches tools or sessions
- The user explicitly requests a reset (`reset session`, `compact session`, `save session`, `context block`, etc.)
- The user mentions running out of context or needing to preserve knowledge

The output is always a `.md` file containing a context block between delimiter comments.

## The 5 Steps (Mandatory Order)

```
Step 1: Determine Session File
    ↓ produces: file path + existing context block (if any)
Step 2: Gather Knowledge
    ↓ produces: categorized knowledge from current conversation
Step 3: Merge with Existing Context Block
    ↓ produces: unified, non-redundant context block content
Step 4: Write the Context Block
    ↓ produces: formatted markdown following all writing rules
Step 5: Write to File
    ↓ produces: final session file on disk
```

---

## Step 1: Determine Session File

**Ask the user:**
> "Does a session file already exist for this work? If yes, what is the file name? If not, I'll create one — what should it be called?"

**If the user provides a file name:** Read it and extract the existing context block (content between `<!-- DESCRIPTION AND USER CONTEXT START -->` and `<!-- DESCRIPTION AND USER CONTEXT END -->`).

**If no file exists:** Create a new one using the template from the context block architecture rule.

**Critical:** Never skip reading the existing context block. Overwriting without reading causes data loss.

---

## Step 2: Gather Knowledge

Collect ALL knowledge from the current conversation, organized by which section it belongs to:

### For Section 1 (Global Guidelines):
- Framework constraints, API styles, language/tooling choices
- Coding conventions (naming, field order, formatting)
- Architectural patterns (data flow, component design, state management)
- Styling rules (utility-first, design system, unit conventions)
- Accessibility requirements (ARIA, keyboard, semantic HTML)
- Performance rules (optimization patterns, SSR constraints)
- Testing conventions (forbidden patterns, mocking strategies)
- Workflow conventions (PR format, tracking, deployment)

### For Section 2 (Session Overview):
- Project/session purpose and scope
- All items in scope with type, summary, and current status
- Session-wide architectural decisions with rationale and **dates**
- Review resolutions (PR comments, feedback addressed)
- Pending work items not yet started

### For Section 3 (Implementations):
- Per-item: creation date, last update date, branch, current status
- File structure, component trees, dependency graphs
- Per-component/file: path, role, connections, key logic
- Key decisions tables with rationale and **dates**
- Test summaries with counts and coverage areas
- PR/commit information

### For Section 4 (File Index):
- All new files created with item association
- All modified files with item association
- Supporting files (documentation, templates, configs)

### For Section 5 (Last Interaction):
- What was completed most recently
- What is pending or not yet started
- Conditional resume instructions

**Date tracking rule:** Every feature, decision, implementation entry, and status change must include a date. Use absolute dates (YYYY-MM-DD), never relative ("last week", "yesterday"). Dates are critical for the compression protocol — older entries are compressed first.

---

## Step 3: Merge with Existing Context Block

**If an existing context block exists:**

1. **Keep** all guidelines and rules that are still accurate
2. **Update** any guidelines refined during this session
3. **Add** new guidelines, decisions, implementations, and patterns
4. **Remove** information that is no longer accurate or was superseded
5. **Preserve** the 5-section structure — never flatten or reorganize
6. **Replace** Section 5 completely — it always reflects only the most recent interaction
7. **Add dates** to any existing entries that lack them (use best estimate with `~` prefix, e.g., `~2026-02-10`)
8. **Check line count** — if the merged result exceeds 3693 lines, run the **Impact Assessment** first (label every compressible entry as `[CRITICAL]`, `[HIGH]`, `[MEDIUM]`, or `[LOW]` based on session focus alignment, active references, pattern establishment, recoverability, and temporal relevance), then apply the compression hierarchy: remove graduated guidelines first, then compress oldest lowest-impact implementations, then oldest decisions, then trim file index, then tombstone completed non-referenced items — always processing `[LOW]` before `[MEDIUM]` before `[HIGH]`, and never touching `[CRITICAL]` entries

**If no existing context block:**

1. Create the full 5-section structure from scratch using the template
2. Ensure all entries have dates from the current conversation

---

## Step 4: Write the Context Block

**Critical writing rules:**

1. **Start with description, not a title.** The context block opens with a direct paragraph. No `#` heading before the first paragraph.
2. **Section map table immediately after intro.** Orients the AI on first load.
3. **Operational rules after the table.** Session-specific processing instructions.
4. **Section 1 intro explains the skill-staging relationship.** Name the specific loaded skills and explain that Section 1 stores session-scoped patterns not yet in those skills.
5. **Be concise with prose** — use bullet points, bold labels, and structured sections. No filler.
6. **No raw conversation data** — abstract everything into guidelines, rules, and status.
7. **Reference actual code/artifacts when needed** — use inline code for identifiers.
8. **Section 5 is always fully replaced** — reflects only the most recent interaction.
9. **All entries must have dates** — creation dates, update dates, decision dates.

---

## Step 5: Write to File

Write the complete file with the context block. If updating an existing file, replace everything — the context block IS the file (plus delimiter comments and optional local variables for editor compatibility).

**Post-write verification:**
- Count total lines — if over compression threshold, the compression protocol should have been applied in Step 3
- Verify all 5 sections are present
- Verify Section 5 reflects only the current conversation

---

## Standalone File Mode (Agents Without Conversation Access — e.g., GPTel)

When this skill is used by an AI that only has access to the session file (not the full conversation), the flow simplifies:

### Input
The AI receives:
1. The session file (with existing context block at the top)
2. All conversation content below the context block (raw requests and responses)

### Operation
1. Read the existing context block between the delimiter comments
2. Read all content below `<!-- DESCRIPTION AND USER CONTEXT END -->` — this is the raw session data
3. Abstract all new knowledge from the raw session data
4. Merge into the existing context block following Step 3 rules
5. Apply compression protocol if result exceeds threshold
6. Output ONLY the updated context block (between delimiters)

### Output
Return ONLY the content between `<!-- DESCRIPTION AND USER CONTEXT START -->` and `<!-- DESCRIPTION AND USER CONTEXT END -->`. No commentary, no explanation. Just the refined context block.

---

## Correct vs. Incorrect Examples

### Example 1: Merge Order

**Incorrect:** Overwriting without reading existing context.
```
Step 1: User says "reset session"
Step 2: Gather knowledge from current conversation
Step 3: Write context block from scratch (ignoring existing file)
```

**Correct:** Reading and merging.
```
Step 1: Read existing session file, extract current context block
Step 2: Gather knowledge from current conversation
Step 3: Merge new knowledge into existing block (keep accurate, update refined, add new, remove superseded)
```

**Why:** Overwriting destroys all knowledge from previous sessions. The merge step preserves continuity.

### Example 2: Date Tracking

**Incorrect:** Entries without dates.
```markdown
### 2.3 Key Decisions
1. Switched from REST to GraphQL for the search API
2. Adopted skeleton loading pattern for all async components
```

**Correct:** Every entry dated.
```markdown
### 2.3 Key Decisions
1. **(2026-02-15)** Switched from REST to GraphQL for the search API — reduced payload by 60%
2. **(2026-03-01)** Adopted skeleton loading pattern for all async components — prevents layout shift
```

**Why:** Dates are critical for the compression protocol. When the context block exceeds the line threshold, older entries are compressed first. Without dates, the system cannot determine compression priority.
