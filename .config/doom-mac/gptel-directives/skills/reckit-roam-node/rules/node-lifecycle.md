---
title: Plan Node Lifecycle — Execution Flow, Phases, Validation Mode, Update Mode, and Quality Checklist
impact: HIGH
impactDescription: Plan nodes have two distinct phases (planning conversation, then development fill) with different validation rules. Without this rule, the execution flow is wrong, validation misses critical checks, updates corrupt existing data, and quality verification is incomplete.
tags: lifecycle, execution-flow, phase, planning, development, validation, update, quality-checklist, git-diff, cross-reference, requirements, decisions, documentation, qa-instructions, unit-test, pass-fail, yaml-output, uuid, last-update, checkbox, deliverables
---

This rule governs the full lifecycle of a RECKIT plan node — from initial planning conversation through development to validation and post-merge update. Every plan has two distinct phases with different data sources and validation rules.

## Execution Flow

### Step 1: Identify the Plan

**Ask the user:**
> "What's the plan slug? (e.g., `context-screen`, `audio-meter-rewrite`, `bug-iframe-subpixel`)"

If the slug is already in the user's message, skip asking. Confirm the variant:

| Cue                                                                   | Variant   |
|-----------------------------------------------------------------------|-----------|
| New feature, refactor, new web source, new component                  | Standard  |
| User describes broken behavior, "the X is not working", repro steps    | Bug       |
| `dev` → `master` PR, version bump, semantic name like "Architectural Baseline" | Release   |

### Step 2: Check for Existing Node

Search `~/.brain.d/roam-nodes/reckit/` for a file matching the slug:

```bash
ls ~/.brain.d/roam-nodes/reckit/ | grep -i "<slug>"
```

- **If found:** Read the existing file. Ask: "A plan node already exists for `<slug>`. Update it (e.g., resolve open questions, log new decisions, fill documentation), or open for editing as-is?"
- **If not found:** Proceed to create a new node.

### Step 3: Fetch git/gh State

Run the queries from `gh-parsing.md`:

```bash
git rev-parse --abbrev-ref HEAD
git log --oneline dev..<branch> 2>/dev/null
gh pr list --head <branch> --json number,state,url
git tag --list 'v*' --sort=-v:refname | head -5
```

This determines:
- Current `#+BRANCH:` value.
- Current `#+STATUS:` and Plan Board lane.
- Current `#+SOURCE_URL:` (branch URL or PR URL).
- Recent tags for `#+TARGET_RELEASE:` defaults.

### Step 4: Determine Template Variant

| Variant   | `#+TITLE:`                          | `#+FILETAGS:`         | Key Sections                                                                |
|-----------|-------------------------------------|-----------------------|-----------------------------------------------------------------------------|
| Standard  | `Plan #<slug>`                      | `:RECKIT:PLAN:`       | SCOPE / DESIGN / DECISIONS / PHASES / DOCUMENTATION                         |
| Bug       | `(BUG) Plan #<slug>`                | `:RECKIT:BUG:`        | SYMPTOM / REPRO / EXPECTED / ACTUAL / ROOT CAUSE / NOTEs                    |
| Release   | `Release v<X.Y> — <Semantic Name>`   | `:RECKIT:RELEASE:`    | RELEASE SCOPE / TRIAD STATUS / RELEASE TASKs / RELEASE NOTES / UPGRADE NOTES |

Apply the appropriate template from `templates.md`.

### Step 5: Author the Plan (Phase 1 Conversation)

Unlike `mr-roam-node`, there is no external JIRA to fetch. The skill **drives the planning conversation** with the user to populate Phase 1 sections:

For **Standard** variant, walk the user through:

1. **Summary paragraph** — what is being built, why, scope boundaries.
2. **REQUIREMENTS** — capture each as `*R<n> — <title>*` with GIVEN/WHEN/THEN. Aim for 4–8 R-items. Skip if obviously trivial.
3. **OUT OF SCOPE** — what this plan explicitly does NOT cover.
4. **OPEN QUESTIONS** — unknowns to resolve before/during Phase 0 of TODO PLAN TASKs. Each gets a `*Q<n> — <question>*` with `*Recommendation:*`.
5. **DESIGN AND ARCHITECTURE** — sketch the architecture (components, data flow, files touched). Use ASCII trees for visual clarity.
6. **DECISIONS** — log each decision as `*D<n> — <title>*` with Choice / Why / Alternatives / Cross-ref. Decisions deferred to Phase 0 are marked `(deferred — see Q<n>)`.
7. **TRADE-OFFS** — costs paid by the chosen design.
8. **TODO PLAN TASKs** — phased breakdown. Phase 0 always = "Lock Decisions" if open questions exist. Subsequent phases are committable units.

For **Bug** variant:

1. **Bug context paragraph** — symptom, severity.
2. **STEPS TO REPRODUCE** — minimal repro path.
3. **EXPECTED RESULT** / **ACTUAL RESULT** — the gap.
4. **ROOT CAUSE** — leave empty if not yet investigated. Filled in INVESTIGATION NOTEs as work progresses.
5. **TODO PLAN TASKs** — typically: investigate → fix → regression test → verify.

For **Release** variant:

1. **WHAT'S INCLUDED** — list of plan UUIDs bundled in this release.
2. **TRIAD STATUS** — three checkboxes (`package.json`, `README.org`, `CHANGELOG.org`).
3. **RELEASE TYPE** — Architectural Baseline / Feature / Patch / Hotfix.
4. **RELEASE TASKs** — three groups (triad readiness, tag + push, post-release).

### Step 6: Write the File

**File path:** `~/.brain.d/roam-nodes/reckit/YYYY-MM-DD-HHMMSS-reckit_<slug>.org`

- Date/time = current timestamp.
- Slug = lowercase, underscores (`context_screen`, `bug_iframe_subpixel`, `release_v0_5`).
- Generate a new UUID for the `:ID:` property (`uuidgen`).
- Apply org-mode formatting per `org-mode-reference.md`.

### Step 7: Update the Index File

**Mandatory final step.** See the `index-management.md` 8-step update flow. This step is never optional — skipping it leaves the dashboard out of sync.

---

## Plan Lifecycle Phases

A plan node has **two distinct phases**.

### Phase 1: Planning (skill + user conversation)

The skill drives a planning conversation, then writes the initial node. **Frozen at first commit on the plan's branch.** Validates "what must be built".

| Section                         | Source                                          | Validation Role                                                  |
|---------------------------------|-------------------------------------------------|------------------------------------------------------------------|
| Metadata (TITLE, SUBTITLE, etc.) | User-stated + git/gh state                      | Identifies the plan, branch, target release                      |
| Body summary paragraph          | Planning conversation                           | Defines the goal                                                 |
| **REQUIREMENTS**                | Planning conversation (GIVEN/WHEN/THEN)          | **Primary validation source** — every R becomes PASS/FAIL at PR  |
| **OUT OF SCOPE**                | Planning conversation                           | Prevents scope creep at review time                              |
| **OPEN QUESTIONS**              | Planning conversation                           | Tracked through Phase 0 of TODO PLAN TASKs                       |
| **DESIGN AND ARCHITECTURE**     | Planning conversation                           | Validates implementation approach                                |
| **DECISIONS** (initial)         | Planning conversation + session-file references  | Captures rationale; deferred decisions marked `(deferred)`       |
| **TRADE-OFFS**                  | Planning conversation                           | Documents knowingly-paid costs                                   |
| **TODO PLAN TASKs** (phased)    | Planning conversation                           | Committable units; Phase 0 resolves OPEN QUESTIONS               |
| **MEDIA**                       | Mockups, captures (often pending at creation)   | Visual references                                                |
| **RELEVANT LINKs**              | Branch URL, index, architecture, conventions    | Reference material                                               |

### Phase 2: Development (user fills as work proceeds)

During implementation, the user fills these sections. They become the **secondary validation source** — what was built and how to verify it.

| Section                       | Content                                                          | Validation Role                                       |
|-------------------------------|------------------------------------------------------------------|-------------------------------------------------------|
| **DECISIONS** (resolved Q's)   | Promote OPEN QUESTIONS to numbered DECISIONS with full rationale | Validates decisions match requirements                |
| **TODO PLAN TASKs** (checked)  | Mark completed items `[X]`, update counters                      | Tracks progress                                       |
| **STRUCTURE AND FUNCTIONALITY** | Architecture description (Input/Process/Output)                  | Validates code structure matches design               |
| **DEPLOYMENT NOTEs**           | Release implications, config, CI changes                         | Deployment verification                               |
| **UNIT TEST COVERAGE**         | Test files added, scenarios covered                              | Validates test completeness                          |
| **QA INSTRUCTIONs**            | Step-by-step manual verification                                 | Manual QA guide                                       |
| **FINDINGS**                   | Retrospective: gotchas, surprises, follow-ups                    | Documents learning, feeds future plans                |
| **DELIVERABLEs**               | Branch, commits, PR, files touched, tag                          | Cross-references for future-you                       |
| **INVESTIGATION NOTEs** (Bug)  | Debugging diary, `** UPDATE` sub-headings                        | Documents investigation, root-cause discovery         |
| **COMMENTs**                   | Free-form notes that don't fit elsewhere                         | Catch-all                                             |

---

## Development Validation Mode

When the user says "validate plan", "check work matches plan", or "review against requirements", use the plan node as the **single source of truth** to verify git changes.

### Validation Flow

1. **Read the plan node** — extract ALL validation sources:
   - REQUIREMENTS (R1..Rn) — primary requirements.
   - DECISIONS (D1..Dn) — design choices that must hold.
   - OUT OF SCOPE — fail if implementation crosses these lines.
   - QA INSTRUCTIONs — testable behaviors (if filled).
   - UNIT TEST COVERAGE — expected test scenarios (if filled).

2. **Read git changes:**
   ```bash
   git diff dev...<branch>
   git log --oneline dev..<branch>
   git diff --name-only dev...<branch>
   ```

3. **Cross-reference** each validation item against the code changes:
   - For each REQUIREMENT: confirm the GIVEN/WHEN/THEN is satisfied by the diff.
   - For each DECISION: confirm the chosen approach is implemented (e.g., D1 says "BroadcastChannel over OBS WS" → grep for `BroadcastChannel` and absence of OBS WS event names).
   - For each OUT OF SCOPE item: confirm no scope creep.
   - For each QA INSTRUCTION: confirm the described behavior is achievable from the code.

4. **Report** — structured output with all checks.

### Validation Output Format

```yaml
plan: Plan #<slug>
node: ~/.brain.d/roam-nodes/reckit/YYYY-MM-DD-HHMMSS-reckit_<slug>.org
branch: <branch-name>

requirements:
  - r: "R1 — Dedicated web source for the CONTEXT scene"
    status: PASS | FAIL | UNCLEAR
    evidence: "Found @kyonax_on_tech/sources/hud/context-screen.vue and registered in sources.js:42"
  - r: "R2 — News-style lower-third layout"
    status: PASS
    evidence: "..."

decisions:
  - d: "D1 — BroadcastChannel over OBS WebSocket relay"
    status: PASS | FAIL | UNCLEAR
    evidence: "use-context-channel.js uses BroadcastChannel API; no OBS WS event subscriptions in the module"

out_of_scope_breaches:
  - item: "Multi-active context (showing two contexts simultaneously)"
    status: NONE
    evidence: "Only one active_slug field in BroadcastChannel state schema"

qa_instructions:
  - step: "Edit a .org file and verify HMR refresh"
    status: PASS
    evidence: "import.meta.glob with eager: true triggers Vite HMR on .org file change"

summary:
  requirements: "X/Y satisfied"
  decisions: "X/Y holding"
  scope_breaches: "X found"
  overall: "PASS | PARTIAL | FAIL"
```

---

## Plan Update Mode

When updating an existing plan (e.g., after a phase completes, after a decision is locked, after PR opens, after merge):

1. **Preserve the UUID** — never change the `:ID:` property.
2. **Update `#+LAST_UPDATE:`** to current date.
3. **Update `#+STATUS:` and `#+SOURCE_URL:`** if git/gh state changed (see `gh-parsing.md`).
4. **Resolve OPEN QUESTIONS** — when the user makes a decision, move the `Q<n>` from `[ ]` to `[X]` in OPEN QUESTIONS, and add the full DECISION as `*D<n>*` in the DECISIONS section.
5. **Update TODO checkboxes** — mark completed items `[X]`, update phase + total statistics cookies.
6. **Fill DOCUMENTATION sections** — add implementation details, deployment notes, test coverage, QA steps as work proceeds.
7. **Append FINDINGS** — note surprises, gotchas, follow-ups discovered during the work.
8. **Update DELIVERABLEs** — fill in branch, commits, PR number, tag (auto-fillable per `gh-parsing.md`).
9. **For Bugs:** add `** UPDATE` sub-headings under INVESTIGATION NOTEs for findings discovered after initial fix.
10. **Update the index file** — sync lane placement and statistics cookies (mandatory — see `index-management.md`).

---

## Quality Checklist

Before writing a plan node file, verify:

### Metadata

- [ ] UUID is generated and unique (no collision with existing nodes in `reckit/`).
- [ ] All metadata headers present and correctly formatted.
- [ ] `#+TITLE:` matches the plan key (`Plan #<slug>` / `(BUG) Plan #<slug>` / `Release v<X.Y> — <Name>`).
- [ ] `#+SUBTITLE:` is a clean one-liner — no emoji, no metadata duplication.
- [ ] `#+EMAIL:` is `kyonax.corp@gmail.com`.
- [ ] `#+SOURCE_URL:` points to the correct branch (or PR / tag for later phases).
- [ ] `#+EFFORT:` is one of `S` / `M` / `L`.
- [ ] `#+TARGET_RELEASE:` set (e.g. `v0.5`).
- [ ] `#+BRANCH:` matches actual `git rev-parse --abbrev-ref HEAD`.
- [ ] `#+STATUS:` matches derived Plan Board lane.
- [ ] No `#+SETUPFILE:`, no `#+STORY_POINTS:` (RECKIT-specific — not used).
- [ ] File name follows `YYYY-MM-DD-HHMMSS-reckit_<slug>.org` pattern.

### Validation Completeness (CRITICAL)

- [ ] Every requirement is structured as `*R<n> — <title>*` with GIVEN/WHEN/THEN.
- [ ] Every open question has a Recommendation line.
- [ ] Every decision has Choice / Why / Alternatives / Cross-ref fields.
- [ ] OUT OF SCOPE list captured (even if just "none — full scope shipped").
- [ ] DESIGN AND ARCHITECTURE has an ASCII data-flow diagram OR a clear prose description.
- [ ] PHASE 0 of TODO PLAN TASKs resolves all OPEN QUESTIONS (or no open questions exist at creation).
- [ ] RELEVANT LINKs include: branch URL, Index Reckit, Reckit Architecture, Reckit Naming Conventions, session-file path with relevant section numbers.
- [ ] Bug variant: STEPS TO REPRODUCE is numbered and minimal.

### Structure

- [ ] TOC links match actual section IDs (case + kebab).
- [ ] TODO tasks have correct checkbox syntax (`- [ ]` exactly) and statistics cookies on each phase + the parent heading.
- [ ] Org-mode comments (`#`) mark which sections are skill-authored vs user-filled (e.g., `# SOURCE: Frozen at first commit`, `# SOURCE: Filled during/after implementation`).
- [ ] DOCUMENTATION subsections (STRUCTURE / DEPLOYMENT / TESTS / QA / FINDINGS) are present but empty at creation (Phase 2 fill).
- [ ] DELIVERABLEs section present even if pending.

### Index File (MANDATORY — Never Skip)

- [ ] Index Update Flow executed as final step.
- [ ] BACKLOG entry exists with org-roam ID link `[[id:UUID][Title]]` and `<<anchor>>`.
- [ ] BACKLOG UUID matches plan node file `:ID:` property.
- [ ] BACKLOG checkbox matches `#+STATUS:` (`[X]` for `RELEASED`, `[ ]` otherwise).
- [ ] PLAN BOARD entry exists in the correct lane per derived state.
- [ ] PLAN BOARD entry uses `[[file:./2026-04-26-index_reckit.org::anchor][(TYPE) Title]]` format.
- [ ] PLAN BOARD nesting matches plan dependencies (child under parent if same lane, top-level if different lanes).
- [ ] No orphaned PLAN BOARD entries (every reference has a matching BACKLOG anchor).
- [ ] Statistics cookies recalculated on all affected lane headings (direct children only).
- [ ] BACKLOG statistics cookie recalculated.
- [ ] QUICK NOTEs reflects current branch, open PRs, next release.
- [ ] Integrity validated: no duplicate anchors, no missing references, every plan UUID points to an existing file.
