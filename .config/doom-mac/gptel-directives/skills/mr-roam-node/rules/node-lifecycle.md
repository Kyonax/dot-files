---
title: Node Lifecycle — Execution Flow, Phases, Validation Mode, Update Mode, and Quality Checklist
impact: HIGH
impactDescription: Nodes have two distinct phases (JIRA creation and developer fill) with different validation rules. Without this rule, the execution flow is wrong (steps skipped), validation misses critical checks, updates corrupt existing data, and quality verification is incomplete.
tags: lifecycle, execution-flow, phase, creation, development, validation, update, quality-checklist, git-diff, cross-reference, ticket-context, event-tracking, development-ac, qa-instructions, unit-test, pass-fail, yaml-output, uuid, last-update, checkbox, documentation, commit-msg
---

This rule governs the full lifecycle of a roam node — from initial creation through development to validation. Every node has two distinct phases with different data sources and validation rules. Understanding this lifecycle is critical because: creation must capture ALL JIRA data (nothing validation-relevant lost), updates must preserve UUIDs and existing content, and validation must cross-reference multiple node sections against git changes. Skipping any step produces incomplete or corrupted nodes.

## Execution Flow

### Step 1: Identify the Ticket

**Ask the user:**
> "Which JIRA ticket? (e.g., DOTCOMPB-7289)"

If the user already provided the ticket number in their message, skip asking.

### Step 2: Check for Existing Node

Search `~/.brain.d/roam-nodes/madison_reed/` for a file matching the ticket number pattern:

```
*-dotcompb_XXXX.org
```

- **If found:** Read the existing file. Ask: "A node already exists for this ticket. Update it with latest JIRA data, or open for editing?"
- **If not found:** Proceed to create a new node.

### Step 3: Fetch JIRA Ticket Data

Retrieve the ticket from JIRA via MCP Atlassian `getJiraIssue` with cloudId `983806ac-6a2a-439b-a0b9-43a41f78cb46`. Extract all fields as specified in the `jira-parsing` rule: summary, description, issue type, story points, comments, linked issues.

### Step 4: Determine Template Variant

| JIRA Issue Type                                      | Template     | Title Format                  |
|------------------------------------------------------|--------------|-------------------------------|
| Story, Task, Feature (`Historia`, `Tarea`, `Mejora`) | **Standard** | `Ticket #DOTCOMPB-XXXX`       |
| Bug (`Error`)                                        | **Bug**      | `(BUG) Ticket #DOTCOMPB-XXXX` |

Select the appropriate template as defined in the `templates` rule.

### Step 5: Generate the Node

Apply the appropriate template. Fill in all metadata, translate JIRA AC into org-mode GIVEN/WHEN/THEN format (per `jira-parsing` rule), and create TODO task checkboxes.

### Step 6: Write the File

**File path:** `~/.brain.d/roam-nodes/madison_reed/YYYY-MM-DD-HHMMSS-dotcompb_XXXX.org`

- Date/time = current timestamp at creation
- Ticket number = lowercase with underscore separator
- Generate a new UUID for the `:ID:` property

### Step 7: Update the Index File

After writing or updating a node, **always** update the index file. This step is mandatory — see the `index-management` rule for the full 8-step update flow.

---

## Node Lifecycle Phases

A roam node has **two distinct phases**. Understanding this is critical for both creation and validation.

### Phase 1: Creation (from JIRA)

At creation, the skill populates these sections from JIRA data. **Everything validation-relevant from the JIRA ticket must be captured here** — this is the contract the development must fulfill.

| Section                          | Source                         | Validation Role                                                      |
|----------------------------------|--------------------------------|----------------------------------------------------------------------|
| Metadata (TITLE, SUBTITLE, etc.) | JIRA fields                    | Identifies the ticket                                                |
| Body text (user story)           | JIRA description               | Defines the user-facing goal                                         |
| **TICKET CONTEXT**               | JIRA AC (all formats)          | **Primary validation source** — every AC becomes a PASS/FAIL check   |
| **EVENT TRACKING** (sub-section) | JIRA event tracking table      | **Validation-critical** — must verify segment events are implemented |
| MEDIA                            | JIRA image references, mockups | Visual references for development                                    |
| RELEVANT LINKs                   | JIRA links, Figma, Slack       | Reference material for development                                   |
| TODO TICKET TASKs (initial)      | Derived from JIRA AC           | High-level task tracking                                             |
| STEPs TO REPRODUCE (bugs)        | JIRA steps                     | Bug reproduction validation                                          |
| EXPECTED/ACTUAL RESULT (bugs)    | JIRA                           | Bug fix validation                                                   |

### Phase 2: Development (Developer Fills)

During implementation, the developer fills these sections. They become the **secondary validation source** — they capture what was actually built and how to verify it.

| Section                      | Content                                                                 | Validation Role                                        |
|------------------------------|-------------------------------------------------------------------------|--------------------------------------------------------|
| **DEVELOPMENT AC**           | Implementation-specific AC, component architecture, technical decisions | Validates implementation approach matches requirements |
| TODO TICKET TASKs (expanded) | Granular sub-tasks with checkboxes                                      | Tracks progress                                        |
| STRUCTURE AND FUNCTIONALITY  | Architecture description (Input/Process/Output pattern)                 | Validates code structure matches design                |
| DEPLOYMENT NOTEs             | CMS changes, config, feature flags                                      | Deployment verification                                |
| UNIT TEST COVERAGE           | Test descriptions per component                                         | Validates test completeness                            |
| QA INSTRUCTIONs              | Step-by-step QA verification                                            | Manual testing guide                                   |
| NOTEs (bugs)                 | Root cause analysis, debugging diary                                    | Documents investigation                                |
| COMMIT MSG                   | PR description                                                          | PR verification                                        |

---

## Development Validation Mode

When the user says "validate development" or "check if work matches ticket", use the node as the **single source of truth** to verify git changes comprehensively.

### Validation Flow

1. **Read the node** — extract ALL validation sources:
   - TICKET CONTEXT AC (GIVEN/WHEN/THEN) — primary requirements from JIRA
   - EVENT TRACKING table — segment events that must be implemented
   - DEVELOPMENT AC — implementation-specific requirements (if filled)
   - QA INSTRUCTIONs — testable behaviors (if filled)
   - UNIT TEST COVERAGE — expected test scenarios (if filled)
2. **Read git changes** — `git diff main...HEAD`, `git log main..HEAD`, list all changed files
3. **Cross-reference** each validation item against the code changes:
   - For each TICKET CONTEXT AC: check if the code changes satisfy the GIVEN/WHEN/THEN
   - For each EVENT TRACKING row: check if the segment event is implemented in the changed files
   - For each DEVELOPMENT AC item: verify the implementation approach matches
   - For QA INSTRUCTIONs: verify the described behaviors are achievable from the code
4. **Report** — structured output with all checks

### Validation Output Format

```yaml
ticket: DOTCOMPB-XXXX
node: ~/.brain.d/roam-nodes/madison_reed/YYYY-MM-DD-HHMMSS-dotcompb_XXXX.org

ticket_context_ac:
  - ac: "AC1 — [title]"
    status: PASS | FAIL | UNCLEAR
    evidence: "Brief explanation referencing specific files/lines"
  - ac: "AC2 — [title]"
    status: PASS
    evidence: "..."

event_tracking:
  - event: "MREvent (HCB Location Page – Book now clicked)"
    status: PASS | FAIL | UNCLEAR
    evidence: "Found trackMREvent call in HairColorBarLocationHeroV2.vue:45"

development_ac:
  - ac: "Create HcbLocationPageV2 component"
    status: PASS
    evidence: "File created at website/src/vuescripts/..."

summary:
  ticket_ac: "X/Y satisfied"
  events: "X/Y implemented"
  development_ac: "X/Y completed"
  overall: "PASS | PARTIAL | FAIL"
```

### Implication for Validation

When validating a `git diff` against a node:
1. **TICKET CONTEXT** AC → primary requirements (from JIRA)
2. **EVENT TRACKING** → verify segment/tracking events are implemented
3. **DEVELOPMENT AC** → verify implementation approach (developer-authored)
4. **QA INSTRUCTIONs** → verify testable behaviors (developer-authored)
5. **UNIT TEST COVERAGE** → verify test files exist and cover stated scenarios

---

## Node Update Mode

When updating an existing node (e.g., after development, after QA, after PR):

1. **Preserve the UUID** — never change the `:ID:` property
2. **Update `#+LAST_UPDATE:`** to current date
3. **Update TODO checkboxes** — mark completed items `[X]`, update counters `[N/M] [P%]`
4. **Fill DOCUMENTATION sections** — add implementation details, deployment notes, test coverage, QA steps
5. **Add COMMIT MSG section** if a PR was created
6. **Add NOTEs / UPDATE subsections** for additional findings during development
7. **Update the index file** — fetch current JIRA status and update the entry's checkbox and status field (see `index-management` rule)

---

## Quality Checklist

Before writing a node file, verify:

### Metadata
- [ ] UUID is generated and unique
- [ ] All metadata headers are present and correctly formatted
- [ ] TITLE matches the ticket key exactly (`Ticket #DOTCOMPB-XXXX` or `(BUG) Ticket #DOTCOMPB-XXXX`)
- [ ] SUBTITLE is cleaned (no emoji prefixes, no date brackets, no status markers)
- [ ] SOURCE_URL uses clean browse format: `https://madison-reed.atlassian.net/browse/DOTCOMPB-XXXX`
- [ ] STORY_POINTS matches JIRA (or marked as unknown)
- [ ] SETUPFILE path is correct (`../../latex/ticket-mr-export.org`)
- [ ] File name follows `YYYY-MM-DD-HHMMSS-dotcompb_XXXX.org` pattern

### Validation Completeness (CRITICAL)
- [ ] **ALL** JIRA AC are translated to GIVEN/WHEN/THEN format — nothing validation-relevant lost
- [ ] Event tracking table captured in EVENT TRACKING sub-section (if present in JIRA)
- [ ] Figma/design links captured in RELEVANT LINKs (if present in JIRA)
- [ ] Slack/resource links captured in RELEVANT LINKs (if present in JIRA)
- [ ] Bug cross-references use org-roam ID links to parent story nodes (if applicable)
- [ ] Template variant matches issue type (standard vs bug)

### Structure
- [ ] TOC links match actual section IDs
- [ ] TODO tasks have correct checkbox syntax and counters
- [ ] Org-mode comments (`#`) mark which sections are JIRA-sourced vs developer-filled
- [ ] DEVELOPMENT AC and DOCUMENTATION sections are present but empty (for developer to fill)

### Index File (MANDATORY — Never Skip)
- [ ] Index Update Flow executed as final step
- [ ] BACKLOG entry exists with org-roam ID link `[[id:UUID][Title]]` and `<<anchor>>`
- [ ] BACKLOG UUID matches node file `:ID:` property
- [ ] BACKLOG checkbox matches JIRA status (`[X]` for Done/Closed, `[ ]` otherwise)
- [ ] Sprint Board entry exists in the correct lane per JIRA status
- [ ] Sprint Board entry uses `[[file:./2025-11-18-index_madison_reed.org::anchor][(TYPE) Title]]` format
- [ ] Sprint Board nesting matches JIRA issue links (child under parent if same lane, top-level if different lanes)
- [ ] No orphaned Sprint Board entries (every reference has a matching BACKLOG anchor)
- [ ] Statistics cookies recalculated on all affected lane headings (`[checked/total]` — direct children only)
- [ ] BACKLOG statistics cookie recalculated (`[P%] [checked/total]`)
- [ ] Integrity validated: no duplicate anchors, no missing references
