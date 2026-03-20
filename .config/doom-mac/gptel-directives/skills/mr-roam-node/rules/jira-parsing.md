---
title: JIRA Ticket Parsing — Format Variants, AC Extraction, Content Translation, and Field Mapping
impact: CRITICAL
impactDescription: JIRA descriptions use 6 different format variants with no standardization. Without this rule, acceptance criteria are lost, event tracking tables are missed, links are dropped, and nodes become incomplete — making validation unreliable.
tags: jira, parsing, format, variant, acceptance-criteria, ac, given-when-then, italic-headers, scenario-based, grouped, numbered, flat, bug, content-translation, field-mapping, event-tracking, figma, links, user-story, summary, subtitle, story-points, issue-type, spanish, smartlink, markdown, org-mode
---

This rule governs how to parse JIRA ticket data and translate it into org-mode node content. JIRA descriptions in the DOTCOMPB project are **not standardized** — format varies by author, issue type, and ticket complexity. The skill must recognize 6 description format variants and normalize all acceptance criteria into a consistent GIVEN/WHEN/THEN pattern. Without this rule, validation-critical data (AC, event tracking tables, Figma links) is lost during translation.

## JIRA Data Extraction

Retrieve the ticket from JIRA (via MCP Atlassian `getJiraIssue` with cloudId `983806ac-6a2a-439b-a0b9-43a41f78cb46`, or user-provided data). Extract:

| JIRA Field    | Maps To                                     | Processing Rules                                                                                                                                                                                  |
|---------------|---------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Summary       | `#+SUBTITLE:`                               | **Strip prefixes:** remove emoji status markers (`🔴 Has Bugs - `, `🟡 ...`), date brackets (`[Thursday, 2/5]`, `[Wednesday, 2/18]`), ticket refs (`[6853]`). Keep only the semantic description. |
| Ticket Key    | `#+TITLE:`                                  | Format as `Ticket #DOTCOMPB-XXXX` (or `(BUG) Ticket #DOTCOMPB-XXXX` for bugs)                                                                                                                     |
| Story Points  | `#+STORY_POINTS:`                           | May require `customfield_10016` — if unavailable, ask the user.                                                                                                                                   |
| Description   | Body text + TICKET CONTEXT + RELEVANT LINKs | Parse per format variant analysis below. Extract: user story, AC, event tracking, scope, links.                                                                                                   |
| Issue Type    | Template variant                            | Spanish labels: `Historia` = Story, `Tarea` = Task, `Error` = Bug, `Mejora` = Enhancement. Use `issuetype.name`.                                                                                  |
| Comments      | `COMMENTs` section                          | Include relevant comments, skip automated/bot comments.                                                                                                                                           |
| Linked Issues | `RELEVANT LINKs` section                    | Fetch via `getJiraIssueRemoteIssueLinks` if needed. Cross-reference with existing roam nodes.                                                                                                     |

### User Story Derivation

If the JIRA description contains an explicit "As a... I want... So that..." statement, extract it directly as the body text paragraph. If there is no user story but an "In Scope" or introductory paragraph, use that. If neither exists, derive a one-sentence summary from the AC context and ask the user to confirm.

---

## Description Format Variants

This analysis is based on real JIRA-to-node comparisons across 5 ticket pairs (6853, 7045, 7052, 7149, 7289).

| Variant               | Typical Issue Type | Key Markers                                                                                                                                                                                                                 | Real Example                 |
|-----------------------|--------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------|
| **Italic Headers**    | Story (Historia)   | `_In Scope_` + `_ACs_` headers (italic). AC labels as `_AC1_`, `_AC2_`. GIVEN/WHEN/THEN as plain text lines.                                                                                                                | DOTCOMPB-6853, DOTCOMPB-7045 |
| **Scenario-Based**    | Story (Historia)   | `**User Story**` + `**Background / Context**` + `**Scenarios (Given / When / Then)**` with `**Scenario: Name**` sub-headers. May include separate `**Acceptance Criteria**` checklist table.                                | DOTCOMPB-7289                |
| **Grouped AC**        | Story (Historia)   | `**USER STORY**` + `**ACCEPTANCE CRITERIA**` with bold sub-category headers (`**Desktop Navigation**`, `**Mobile Navigation**`) followed by bullet lists. May include `**IMPLEMENTATION / DEV NOTES**`, `**OUT OF SCOPE**`. | DOTCOMPB-7463                |
| **Numbered Headings** | Story (Historia)   | `**As a / I want / So that**` (bold keywords) + `### Acceptance Criteria` + `#### 1. Category Name` (numbered h4 sub-categories) followed by bullet lists with bold Given/When/Then.                                        | DOTCOMPB-7052                |
| **Flat / Minimal**    | Task (Tarea)       | No section headers. AC as `**AC1:**` inline with action description. May include images.                                                                                                                                    | DOTCOMPB-7209                |
| **Bug Format**        | Bug (Error)        | `## Summary` + `## Description` + `## Steps to Reproduce` + `## Expected Result` + `## Actual Result` + `## Resources`.                                                                                                     | DOTCOMPB-7149                |

---

## AC Parsing Rules

The skill must detect which AC format a ticket uses and normalize ALL AC into the standard org-mode GIVEN/WHEN/THEN pattern. **No validation-relevant AC should be lost during translation.**

### 1. Italic Header AC

**Detect by:** `_ACs_` or `_AC1_` italic markers

- JIRA uses italic `_AC1_`, `_AC2_` as labels with GIVEN/WHEN/THEN on separate plain-text lines
- Translate to bold `*AC1*`, `*AC2*` with `*GIVEN*`/`*WHEN*`/`*THEN*`
- Include any bullet sub-items under THEN as nested list items

**Example JIRA (DOTCOMPB-7045):**
```
_AC2_
GIVEN I am signing in or creating an account,
WHEN I run into the following errors,
THEN I should see an error message and I should not be able to move forward
* Invalid email address
* Wrong password
```

**Becomes:**
```org
- *AC2*
  - *GIVEN* I am signing in or creating an account,
  - *WHEN* I run into the following errors,
  - *THEN* I should see an error message and I should not be able to move forward.
    - Invalid email address
    - Wrong password
```

### 2. Scenario-Based AC

**Detect by:** `**Scenario:` or `**Scenarios (Given / When / Then)**`

- Each `**Scenario: Name**` becomes `- *Scenario: Name*`
- Given/When/Then sentences from the prose map directly to `*GIVEN*`/`*WHEN*`/`*THEN*`
- **If JIRA has both Scenarios AND a separate AC checklist table** (like DOTCOMPB-7289), merge them — use the scenarios as the primary structure and ensure every checklist item is covered

**Example JIRA:**
```
**Scenario: Book Now CTA**
Given I am on a specific salon location page, when I click on the Book Now CTA, then I should be taken to the service selection screen.
```

**Becomes:**
```org
- *Scenario: Book Now CTA*
  - *GIVEN* I am on a specific salon location page,
  - *WHEN* I click on the Book Now CTA,
  - *THEN* I should be taken to the service selection screen.
```

### 3. Grouped Bullet AC

**Detect by:** `**ACCEPTANCE CRITERIA**` followed by bold sub-headers followed by bullet lists

- Each bold sub-category header becomes a scenario grouping
- Each bullet point becomes its own AC with derived GIVEN/WHEN/THEN

**Example JIRA (DOTCOMPB-7463):**
```
**Mobile Navigation**
* Mobile navigation does NOT use accordions; all categories are accessible via a flat navigation pattern.
* All navigation links are tappable with a minimum touch target of 44x44px.
```

**Becomes:**
```org
- *Scenario: Mobile Navigation*
  - *AC1 — Flat navigation pattern*
    - *GIVEN* I am viewing the mobile navigation,
    - *WHEN* I access product categories,
    - *THEN* all categories are accessible via a flat navigation pattern (no accordions).
  - *AC2 — Touch targets*
    - *GIVEN* I am interacting with mobile navigation links,
    - *WHEN* I tap a link,
    - *THEN* the touch target is at least 44x44px.
```

### 4. Numbered Heading AC

**Detect by:** `#### 1.` or `### **Acceptance Criteria**` with numbered sub-headings

- Similar to Grouped Bullet but uses markdown heading levels for categories
- Each numbered heading becomes a titled AC group
- Bold Given/When/Then keywords within bullets map directly

**Example JIRA (DOTCOMPB-7052):**
```
#### **2. User Interaction**
* **Given** the question prompt displays, **When** the user selects an answer, **Then** the template records the selection
```

**Becomes:**
```org
- *AC2 User Interaction*
  - *GIVEN* the question prompt displays,
  - *WHEN* the user selects an answer,
  - *THEN* the template records the selection.
```

### 5. Flat/Inline AC

**Detect by:** `**AC1:**` or `**ACN:**` pattern without surrounding structure

- Each `**ACN:**` becomes a separate AC entry
- Derive GIVEN/WHEN/THEN from the action description

**Example JIRA (DOTCOMPB-7209):**
```
**AC1:** Update the copy of the modal: - REMOVE the words "or rescheduled"
```

**Becomes:**
```org
- *AC1 — Remove reschedule copy*
  - *GIVEN* I am viewing the cancellation and no-show policy modal,
  - *WHEN* the modal is displayed,
  - *THEN* the copy does not include the words "or rescheduled".
```

---

## Content Translation Rules

When converting JIRA ticket content to org-mode:

| JIRA Element                                 | Org-Mode Equivalent                                                                                                                         | Notes                                                                          |
|----------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------|
| Bold text (`**text**`)                       | `*bold*`                                                                                                                                    | Ensure valid PRE/POST characters surround markers (whitespace, `-`, `(`, etc.) |
| Italic text (`_text_`)                       | `/italic/`                                                                                                                                  | Use for user story "As a..." statements                                        |
| Inline code (`` `code` ``)                   | `=code=`                                                                                                                                    | Convention: always use verbatim `=...=`, not `~...~`                           |
| Code block                                   | `#+begin_src lang` ... `#+end_src`                                                                                                          | Comma-escape lines starting with `*` or `#+` inside blocks                     |
| Bullet list                                  | `- item` (2-space indent per nesting level)                                                                                                 | No blank lines between items in the same list                                  |
| Numbered list                                | `1. item`                                                                                                                                   | Sequential numbering, 2-space indent for nesting                               |
| Link (`[text](url)`)                         | `[[url][text]]`                                                                                                                             | Double-bracket syntax                                                          |
| Smartlink (`<custom data-type="smartlink">`) | `[[url][text]]`                                                                                                                             | Extract URL from tag, generate descriptive text                                |
| Table                                        | Org table: `\|` cells, `\|---+---\|` rules                                                                                                  | `#+TBLFM:` must immediately follow table (no blank line)                       |
| Image/attachment (blob URL)                  | Note in MEDIA or RELEVANT LINKs                                                                                                             | Org-mode cannot embed JIRA blob images                                         |
| Heading (`## H2`, `### H3`)                  | `* H1`, `** H2`, `*** H3`                                                                                                                   | Stars at column 0, one space after stars                                       |
| Quote/callout                                | `#+begin_quote` ... `#+end_quote`                                                                                                           |                                                                                |
| JIRA mention                                 | Replace with name or org-roam ID link if node exists                                                                                        |                                                                                |
| JIRA ticket reference                        | `[[id:UUID][Ticket #DOTCOMPB-XXXX]]` if node exists, otherwise `[[https://madison-reed.atlassian.net/browse/DOTCOMPB-XXXX][DOTCOMPB-XXXX]]` | Always prefer org-roam ID links                                                |
| Horizontal rule (`---`)                      | Five or more dashes: `-----`                                                                                                                | Org-mode requires 5+ dashes for a horizontal rule                              |

---

## JIRA-to-Node Field Mapping Notes

- **Summary** → `#+SUBTITLE:` — Strip: emoji prefixes (`🔴 Has Bugs - `, `🟡 ...`), date/sprint brackets (`[Thursday, 2/5]`, `[Wednesday, 2/18]`), ticket cross-refs (`[6853]`). Keep semantic description only.
- **Story Points** — Not in the basic API response; may need `customfield_10016` or manual input. Ask user if not available.
- **Issue Type names** — JIRA instance uses Spanish labels (`Tarea` = Task, `Historia` = Story, `Error` = Bug). Map by `issuetype.name` or `issuetype.id`.
- **Assignee** — Available but not used in node metadata (node author is always the developer creating the node).
- **Status** — Used for index file entry checkbox and status label. Map `statusCategory.key`: `done` → `[X]`, others → `[ ]`.
- **Event Tracking tables** — **CRITICAL: must be preserved** as org tables in TICKET CONTEXT > EVENT TRACKING. These are validation-critical for verifying segment event implementation. Previously lost in all analyzed nodes.
- **Figma links** — Extract from JIRA description (may appear as plain URLs, `[text](url)`, or `<custom data-type="smartlink">` tags). Place in RELEVANT LINKs.
- **Slack/Resource links** — Extract from bug `## Resources` section or inline references. Place in RELEVANT LINKs.
- **Cross-references** — If a bug references a parent story (e.g., DOTCOMPB-7149 → 6853), search existing roam nodes for the parent UUID and create an org-roam ID link: `[[id:UUID][Ticket #DOTCOMPB-YYYY]]`.

## What JIRA Tickets Contain vs What Nodes Must Capture

| JIRA Content                     | Found In                             | Capture Priority | Maps To in Node                             |
|----------------------------------|--------------------------------------|------------------|---------------------------------------------|
| User Story ("As a...")           | Most stories (explicit or derivable) | **REQUIRED**     | Body text paragraph                         |
| In Scope / Background / Context  | Many tickets                         | **REQUIRED**     | Body text or TICKET CONTEXT intro           |
| Acceptance Criteria (any format) | All tickets                          | **CRITICAL**     | TICKET CONTEXT as GIVEN/WHEN/THEN           |
| Event Tracking table             | Most stories (7289, 7463, 7052)      | **CRITICAL**     | TICKET CONTEXT > EVENT TRACKING sub-section |
| Figma/Design links               | Most stories                         | **REQUIRED**     | RELEVANT LINKs                              |
| Slack/Resource links             | Bug tickets (7149)                   | **REQUIRED**     | RELEVANT LINKs                              |
| Image attachments                | Some tickets (6853, 7209)            | LOW              | Note reference in RELEVANT LINKs            |
| Implementation / Dev Notes       | Some tickets (7463)                  | OPTIONAL         | Omit or note in DEVELOPMENT AC              |
| Out of Scope                     | Some tickets (7463)                  | OPTIONAL         | Note in TICKET CONTEXT as boundary          |
