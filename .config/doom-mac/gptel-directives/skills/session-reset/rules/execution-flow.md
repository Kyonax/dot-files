---
title: Session Reset Execution Flow
impact: HIGH
impactDescription: Defines the mandatory 6-step process for performing a session reset — from determining the target file through writing the final output, including optional architecture memory extraction. Following the wrong order or skipping steps produces incomplete or corrupted context blocks.
tags: execution, flow, steps, process, reset, merge, gather, write, session, update, create, new-session, existing-session, compaction, architecture, extraction, memory, reference
---

This rule defines the mandatory 6-step process for performing a session reset. A session reset is a knowledge compaction operation — it extracts all accumulated knowledge from the current conversation, optionally extracts reusable architectural knowledge into persistent memory files, merges the result with any existing context block, and writes a single refined output. Skipping steps (e.g., not reading the existing context block before merging) produces data loss or duplication.

## What Is a Session Reset?

A session reset is triggered when:
- A conversation grows long (approaching context limits)
- The user switches tools or sessions
- The user explicitly requests a reset (`reset session`, `compact session`, `save session`, `context block`, etc.)
- The user mentions running out of context or needing to preserve knowledge

The output is always a `.md` file containing a context block between delimiter comments.

## The 6 Steps (Mandatory Order)

```
Step 1: Determine Session File
    ↓ produces: file path + existing context block (if any)
Step 2: Gather Knowledge (including Activity Log entries — see Section 6)
    ↓ produces: categorized knowledge from current conversation
Step 2.5: Extract Architecture Memory (OPTIONAL — see criteria below)
    ↓ produces: new/updated entries in architecture memory file(s) + reference list
Step 3: Merge with Existing Context Block
    ↓ produces: unified, non-redundant context block content
Step 4: Write the Context Block (including Section 6 with at least one new
        `session-reset` row prepended to the Activity Log)
    ↓ produces: formatted markdown (with architecture references if Step 2.5 ran)
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

### For Section 6 (Activity Log):
- Every meaningful event in this session that has not yet been logged
- Each new entry: datetime (`YYYY-MM-DD HH:MM`, host-local — *not* a relative time), duration (`Nh` / `N.5h` / `Nm` / `—`), type (controlled vocabulary in `rules/activity-log.md`), reference (`DOTCOMPB-####` / `PR #####` / `commit <sha>` / `this`), one-line description
- At minimum, **this reset must add one `session-reset` row** referencing `this`
- New entries are *prepended* (newest first); existing rows are not modified
- See `rules/activity-log.md` for the full schema, vocabulary, and validation checklist

**Date tracking rule:** Every feature, decision, implementation entry, and status change must include a date. Use absolute dates (YYYY-MM-DD), never relative ("last week", "yesterday"). Dates are critical for the compression protocol — older entries are compressed first.

**Datetime tracking rule (Activity Log):** Section 6 entries require *both* date and time-of-day in `YYYY-MM-DD HH:MM` format. The time-of-day is what makes the session file a reliable input to time-tracking automation (e.g., `jira-tempo-hours`). If you genuinely don't know the time, omit the entry rather than fabricate one.

---

## Step 2.5: Extract Architecture Memory (Optional)

This step extracts **reusable architectural knowledge** from the gathered session data into persistent architecture memory files. Unlike the context block (which captures everything needed to resume this session), architecture memory captures only knowledge that informs future sessions across projects — decisions, patterns, constraints, and abstractions.

**When to run Step 2.5:**

| Run | Skip |
|---|---|
| Session produced 3+ architectural decisions | First session in a new project (let patterns emerge) |
| Session validated or refined an existing pattern | Session was only implementation work (bug fixes, style changes) |
| Session discovered a new platform constraint | Session was <30 minutes of simple tasks |
| User explicitly requests architecture extraction | |

**Step 2.5 sub-steps:**

1. **Identify extraction candidates** — Scan gathered knowledge for reusable architectural insights. The core test: "Would this knowledge influence a decision in a future session on a *different* feature?" If yes, it's a candidate.

   **What to extract:**
   - Architecture decisions (chose A over B, with rationale)
   - Design patterns (reusable structural approaches)
   - Data flow decisions (sources, transforms, destinations)
   - Key abstractions (global services, shared modules, dead/misleading variables)
   - Constraints and limitations (platform, SSR, ADA, business)
   - Reusable structures (API contracts, data shapes, schemas)

   **What NOT to extract:**
   - Code-review rules (belong in coding-standards skill)
   - One-off fixes (the fix is in the code)
   - Debugging steps (zero future value)
   - PR/git workflow details (process metadata)
   - Test counts (change constantly)

2. **Determine target architecture memory file** — If one exists for this domain, use it. If 5+ candidates are identified and no file exists, create one with `{session-domain}-architecture.md` naming. If <5 candidates, hold — store in the context block and extract on next reset if the pattern continues.

3. **Deduplicate against existing entries** — For each candidate, check if a matching entry already exists in architecture memory:
   - Exact match → skip (already stored)
   - Semantic overlap ≥80% → update existing entry with new details
   - Contradiction → flag for user resolution (mark both entries as "contested")
   - No match → create new entry

4. **Write/update architecture memory file** — New entries get the next available ID in their section (e.g., `ad-013`, `dp-009`). Updated entries are modified in place. Section 1 (Summary) is updated with new source session and knowledge counts.

5. **Generate reference list** — For every extracted/matched entry, produce a reference string for use in Step 4:
   ```
   [session: site-revolution-architecture > design-patterns > dp-001]
   [session: site-revolution-architecture > architecture-decisions > ad-005]
   ```

**Architecture memory file structure (6 sections):**

| Section | Content |
|---|---|
| 1. Summary | Domain, source sessions, knowledge category counts |
| 2. Architecture Decisions | Decisions with context, alternatives, consequences (ID: `ad-NNN`) |
| 3. Design Patterns | Reusable patterns with when-to-use, structure, anti-patterns (ID: `dp-NNN`) |
| 4. Shared State & Data Flow | Data sources, transforms, consumers, gotchas (ID: `sf-NNN`) |
| 5. Constraints & Limitations | Platform/SSR/ADA constraints with workarounds (ID: `cl-NNN`) |
| 6. Reusable References | API contracts, data shapes, schemas (ID: `rr-NNN`) |

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
10. **Use architecture references when Step 2.5 produced them.** Replace inline explanations of extracted knowledge with reference pointers. See below.

### Architecture References in Context Blocks (When Step 2.5 Ran)

If Step 2.5 extracted knowledge into architecture memory files, use **reference pointers** in the context block instead of duplicating the knowledge inline. The reference syntax is:

```
[session: <filename> > <section> > <entry-id>]
```

**Resolution levels:**
- `[session: site-revolution-architecture]` → loads Summary only
- `[session: site-revolution-architecture > design-patterns]` → loads entire section
- `[session: site-revolution-architecture > design-patterns > dp-001]` → loads one entry
- `[session: site-revolution-architecture > constraints > cl-001, cl-003]` → loads multiple entries

**Where to use references:**

| Context Block Section | Reference Usage |
|---|---|
| **Section 1** (Guidelines) | Reference patterns and constraints that are fully documented in architecture memory. Keep 1-2 line summaries inline for readability; use references for the full explanation. |
| **Section 2.3** (Decisions) | Reference architecture decisions for full rationale. Keep a one-line summary inline. |
| **Section 3** (Implementations) | Reference constraints and patterns relevant to the implementation. Most implementation detail stays inline (it's per-ticket, not architectural). |
| **Section 4** (File Index) | No references — file paths are always inline. |
| **Section 5** (Last Interaction) | No references — short-term memory is always inline. |

**Reference density rule:** Do not reference everything. If a guideline can be stated in 1-2 lines, keep it inline even if a reference exists. Use references when the full explanation is 5+ lines. Aim for <20 references per context block.

**Example — Section 1 with references:**
```markdown
### 1.5 Accessibility
*   Self-contained landmarks — [session: site-revolution-architecture > design-patterns > dp-001]
*   `aria-labelledby` must reference heading IDs, never root element IDs
*   WCAG 2.5.3 card pattern — [session: site-revolution-architecture > design-patterns > dp-012]
```

**Example — Section 2.3 with references:**
```markdown
### 2.3 Key Decisions
5. **(2026-03-30)** Cookie-based cross-app state — [session: site-revolution-architecture > architecture-decisions > ad-002]
```

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
