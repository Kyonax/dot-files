---
title: Node Templates — Base Structure, Standard and Bug Variants, Metadata and Formatting
impact: CRITICAL
impactDescription: Templates define the exact org-mode structure of every generated node. Without this rule, nodes have inconsistent metadata, wrong section layouts, missing fields, or incorrect formatting — making them unusable for validation and cross-referencing.
tags: template, standard, bug, base, metadata, formatting, structure, title, subtitle, author, filetags, source-url, date, story-points, setupfile, options, toc, file-naming, uuid, section-headers, screaming-case, checkbox, ac-format, given-when-then, org-roam, properties
---

This rule defines the exact templates used to generate org-roam node files from JIRA tickets. Every node must follow one of two template variants (Standard or Bug), both built on a shared base structure. Deviating from these templates produces nodes that break cross-referencing (wrong `:ID:` placement), fail validation (missing sections), or render incorrectly in Emacs (wrong org-mode syntax).

## Base Template

The user creates new node files from an Emacs org-capture template at `~/.brain.d/templates/new-node-project.org`. This template provides the **base layout** that must always be preserved. The skill **fills** this base — it does not replace it.

### Base Template Structure

```
#+TITLE → #+SUBTITLE → #+AUTHOR → #+EMAIL → #+DATE → #+FILETAGS →
#+SOURCE_URL → #+LAST_UPDATE → #+STORY_POINTS → #+OPTIONS → #+SETUPFILE
[Description paragraph]
* TABLE OF CONTENTs :toc:
  (nested links for all sections and sub-sections)
* ACCEPTANCE CRITERIA
  ** TICKET CONTEXT
  ** DEVELOPMENT AC
* MEDIA
* RELEVANT LINKs
* TODO TICKET TASKs
* DOCUMENTATION
  ** STRUCTURE AND FUNCTIONALITY
  ** DEPLOYMENT NOTEs
  ** UNIT TEST COVERAGE
  ** QA INSTRUCTIONs
* COMMENTs
```

The base template uses Emacs org-capture syntax (`%^{...}` for prompts, `%<...>` for timestamps). Org-roam automatically prepends the `:PROPERTIES: :ID: :END:` drawer — it is NOT in the template file itself.

### How the Skill Populates the Base

The base template already contains the full section structure. The skill's job is to **fill** these sections with JIRA data at creation, not restructure the file:

| Base Section                     | Skill Action at Creation                                                                                                                 | Developer Fills Later                              |
|----------------------------------|------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------|
| `** TICKET CONTEXT`              | Populate with all JIRA AC translated to GIVEN/WHEN/THEN. Add `*** EVENT TRACKING` sub-section with org table if JIRA has tracking specs. | —                                                  |
| `** DEVELOPMENT AC`              | Leave empty                                                                                                                              | Implementation-specific AC, architecture decisions |
| `MEDIA`                          | Reference JIRA image attachments if any                                                                                                  | Screenshots, mockups                               |
| `RELEVANT LINKs`                 | Populate with JIRA ticket link, Figma links, Slack links, cross-references                                                               | Additional links found during development          |
| `TODO TICKET TASKs`              | Create initial checkbox items from JIRA AC                                                                                               | Expand with granular sub-tasks                     |
| `** STRUCTURE AND FUNCTIONALITY` | Leave empty                                                                                                                              | Architecture description (Input/Process/Output)    |
| `** DEPLOYMENT NOTEs`            | Leave empty                                                                                                                              | CMS changes, config, feature flags                 |
| `** UNIT TEST COVERAGE`          | Leave empty                                                                                                                              | Test descriptions per component                    |
| `** QA INSTRUCTIONs`             | Leave empty                                                                                                                              | Step-by-step QA verification                       |
| `COMMENTs`                       | Include relevant JIRA comments                                                                                                           | Developer notes                                    |

**Rule:** When the user has already created a file from the base template, the skill must detect and work with the existing structure — filling sections in-place rather than regenerating the file.

---

## Standard Template (Story / Task / Feature)

```org
:PROPERTIES:
:ID:       [generated-uuid]
:END:
#+TITLE: Ticket #DOTCOMPB-XXXX
#+SUBTITLE: [JIRA Summary — one-line description]
#+AUTHOR: [[https://orcid.org/0009-0006-4459-5538][Cristian D. Moreno - Agile Engine]]
#+EMAIL: cristian.moreno@agileengine.com
#+DATE: [Mon DD, YYYY]
#+FILETAGS: :MR:TICKET:
#+SOURCE_URL: https://madison-reed.atlassian.net/browse/DOTCOMPB-XXXX
#+LAST_UPDATE: [Mon DD, YYYY]
#+STORY_POINTS: [N]
#+OPTIONS: toc:nil num:nil H:5
#+SETUPFILE: ../../latex/ticket-mr-export.org

[User story from JIRA description — first paragraph or "As a..." statement]

* TABLE OF CONTENTs :toc:
- [[#acceptance-criteria][ACCEPTANCE CRITERIA]]
  - [[#ticket-context][TICKET CONTEXT]]
  - [[#development-ac][DEVELOPMENT AC]]
- [[#media][MEDIA]]
- [[#relevant-links][RELEVANT LINKs]]
- [[#ticket-tasks][TICKET TASKs]]
- [[#documentation][DOCUMENTATION]]
  - [[#structure-and-functionality][STRUCTURE AND FUNCTIONALITY]]
  - [[#deployment-notes][DEPLOYMENT NOTEs]]
  - [[#unit-test-coverage][UNIT TEST COVERAGE]]
  - [[#qa-instructions][QA INSTRUCTIONs]]
- [[#comments][COMMENTs]]

* ACCEPTANCE CRITERIA
** TICKET CONTEXT
# SOURCE: Populated at creation from JIRA description. This is the source of truth for validation.
# All JIRA AC must be captured here — nothing validation-relevant should be lost.
[Translate ALL JIRA AC into GIVEN/WHEN/THEN format using org-mode bold markers.
 See jira-parsing rule for parsing rules per ticket format variant.]

- *AC1 [Short AC Title]*
  - *GIVEN* [precondition],
  - *WHEN* [action],
  - *THEN* [expected outcome].

- *AC2 [Short AC Title]*
  - *GIVEN* [precondition],
  - *WHEN* [action],
  - *THEN* [expected outcome].

[If JIRA has event tracking table, include it here as an org table:]

*** EVENT TRACKING
| Page/Section | Event Type | Event Name | Trigger | Properties | Values |
|---+---+---+---+---+---|
| [from JIRA] | [from JIRA] | [from JIRA] | [from JIRA] | [from JIRA] | [from JIRA] |

** DEVELOPMENT AC
# SOURCE: Left empty at creation. Developer fills during implementation.
# Tracks implementation-specific decisions, component architecture, and technical AC
# that emerge during development — NOT from the original ticket.

* MEDIA
# Screenshots, mockups, visual references.
# JIRA image attachments are referenced here (blob URLs cannot be embedded in org).

* RELEVANT LINKs
- [[https://madison-reed.atlassian.net/browse/DOTCOMPB-XXXX][Jira Ticket DOTCOMPB-XXXX]]
[MANDATORY: Capture ALL links from JIRA description:]
[- Figma/design links]
[- Linked JIRA issues (use org-roam ID links if node exists: [[id:UUID][Ticket #DOTCOMPB-YYYY]])]
[- Slack thread links]
[- External documentation/resource URLs]

* TODO TICKET TASKs [0/N] [0%]
# SOURCE: Initial items from JIRA AC at creation. Developer expands with sub-tasks during implementation.
[Create checkbox items from JIRA AC — each AC becomes a task group]
- [ ] AC1: [Task description]
- [ ] AC2: [Task description]

* DOCUMENTATION
# SOURCE: Left empty at creation. Developer fills during/after implementation.
** STRUCTURE AND FUNCTIONALITY
** DEPLOYMENT NOTEs
** UNIT TEST COVERAGE
** QA INSTRUCTIONs
* COMMENTs
```

---

## Bug Template

```org
:PROPERTIES:
:ID:       [generated-uuid]
:END:
#+TITLE: (BUG) Ticket #DOTCOMPB-XXXX
#+SUBTITLE: [JIRA Summary — bug description]
#+AUTHOR: [[https://orcid.org/0009-0006-4459-5538][Cristian D. Moreno - Agile Engine]]
#+EMAIL: cristian.moreno@agileengine.com
#+DATE: [Mon DD, YYYY]
#+FILETAGS: :MR:TICKET:
#+SOURCE_URL: https://madison-reed.atlassian.net/browse/DOTCOMPB-XXXX
#+LAST_UPDATE: [Mon DD, YYYY]
#+STORY_POINTS: [N]
#+OPTIONS: toc:nil num:nil H:5
#+SETUPFILE: ../../latex/ticket-mr-export.org

[Bug description from JIRA — context paragraph]

* TABLE OF CONTENTs :toc:
- [[#steps-to-reproduce][STEPs TO REPRODUCE]]
- [[#expected-result][EXPECTED RESULT]]
- [[#actual-result][ACTUAL RESULT]]
- [[#relevant-links][RELEVANT LINKs]]
- [[#ticket-tasks][TICKET TASKs]]
- [[#notes][NOTEs]]
- [[#comments][COMMENTs]]

* STEPs TO REPRODUCE
# SOURCE: Populated at creation from JIRA. These steps are validation-critical.
[Numbered list from JIRA reproduction steps]
1. [Step 1]
2. [Step 2]
3. [Step 3]

* EXPECTED RESULT
# SOURCE: Populated at creation from JIRA.
[What should happen]

* ACTUAL RESULT
# SOURCE: Populated at creation from JIRA.
[What actually happens — the bug behavior]

* RELEVANT LINKs
- [[https://madison-reed.atlassian.net/browse/DOTCOMPB-XXXX][Jira Ticket DOTCOMPB-XXXX]]
[If bug originated from another ticket, cross-reference with org-roam ID link:]
[- [[id:UUID][Ticket #DOTCOMPB-YYYY]] (parent story)]
[- Slack thread links from JIRA Resources section]
[- Any other URLs from JIRA description]

* TODO TICKET TASKs [0/N] [0%]
# SOURCE: Initial items at creation. Developer updates during investigation.
- [ ] Investigate root cause
- [ ] Implement fix
- [ ] Verify fix resolves the issue

* NOTEs
# SOURCE: Left empty at creation. Developer fills during investigation.
# This becomes a debugging diary: root cause analysis, code investigation,
# solution approach, and follow-up findings.
# Use ** UPDATE sub-headings for additional findings discovered after initial fix.

* COMMENTs
```

---

## Template Variant Selection

| JIRA Issue Type      | Template     | Title Format                  | Key Sections                                                         |
|----------------------|--------------|-------------------------------|----------------------------------------------------------------------|
| Story, Task, Feature | **Standard** | `Ticket #DOTCOMPB-XXXX`       | ACCEPTANCE CRITERIA (TICKET CONTEXT + DEVELOPMENT AC), DOCUMENTATION |
| Bug                  | **Bug**      | `(BUG) Ticket #DOTCOMPB-XXXX` | STEPs TO REPRODUCE, EXPECTED RESULT, ACTUAL RESULT                   |

JIRA issue type labels may be in Spanish: `Historia` = Story, `Tarea` = Task, `Error` = Bug, `Mejora` = Enhancement. Use `issuetype.name` to determine the variant.

---

## Metadata Header Rules

These rules ensure consistency across all nodes:

| Header            | Value / Format                                                                 | Notes                                                                                                   |
|-------------------|--------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------|
| `#+TITLE:`        | `Ticket #DOTCOMPB-XXXX` or `(BUG) Ticket #DOTCOMPB-XXXX`                       | Must match ticket key exactly                                                                           |
| `#+SUBTITLE:`     | Cleaned JIRA summary                                                           | Strip emoji prefixes, date brackets, status markers, ticket cross-refs. Keep semantic description only. |
| `#+AUTHOR:`       | `[[https://orcid.org/0009-0006-4459-5538][Cristian D. Moreno - Agile Engine]]` | Always ORCID-linked format                                                                              |
| `#+EMAIL:`        | `cristian.moreno@agileengine.com`                                              | Fixed value                                                                                             |
| `#+DATE:`         | `Mon DD, YYYY` (e.g., `Mar 02, 2026`)                                          | Creation date                                                                                           |
| `#+LAST_UPDATE:`  | `Mon DD, YYYY`                                                                 | Updated on every node modification                                                                      |
| `#+FILETAGS:`     | `:MR:TICKET:`                                                                  | Fixed value                                                                                             |
| `#+SOURCE_URL:`   | `https://madison-reed.atlassian.net/browse/DOTCOMPB-XXXX`                      | Clean browse format                                                                                     |
| `#+STORY_POINTS:` | Number from JIRA                                                               | May need `customfield_10016` — ask user if unavailable                                                  |
| `#+OPTIONS:`      | `toc:nil num:nil H:5`                                                          | Fixed value                                                                                             |
| `#+SETUPFILE:`    | `../../latex/ticket-mr-export.org`                                             | Fixed relative path                                                                                     |

## Section Formatting Rules

1. **TOC links** use `[[#kebab-case-id][DISPLAY NAME]]` format
2. **Section headers** use SCREAMING CASE with trailing `s` on plural words (e.g., `COMMENTs`, `RELEVANT LINKs`, `TICKET TASKs`)
3. **AC format** uses org-mode bold `*GIVEN*`, `*WHEN*`, `*THEN*` — NOT `=GIVEN=`
4. **TODO tasks** use `* TODO TICKET TASKs [0/N] [0%]` with checkbox syntax `- [ ]` / `- [X]`
5. **Priority markers** on sub-headings: `** [#A]` for high priority, `** [#B]` for normal
6. **Cross-references** to other nodes use org-roam ID links: `[[id:UUID][Display Text]]`
7. **Code blocks** use `#+begin_src LANG` / `#+end_src` for code, `#+begin_quote` for quotes

## File Naming

- Pattern: `YYYY-MM-DD-HHMMSS-dotcompb_XXXX.org`
- Bug tickets may optionally use `bug_dotcompb_XXXX` but the standard is `dotcompb_XXXX`
- All lowercase, underscores for ticket number separator

## UUID Generation

- Generate a standard v4 UUID for the `:ID:` property
- Each node must have a unique ID for org-roam cross-referencing
- The `:PROPERTIES:` drawer with `:ID:` **must come first** in the file, before any `#+` keywords (org-roam convention)
