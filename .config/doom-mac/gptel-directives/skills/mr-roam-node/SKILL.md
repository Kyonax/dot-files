---
name: mr-roam-node
description: "Generate and manage org-roam .org files for JIRA tickets in ~/.brain.d/roam-nodes/madison_reed/. Creates consistent, structured org-mode documentation nodes from JIRA ticket data. Handles two ticket types: feature/story (ACCEPTANCE CRITERIA with TICKET CONTEXT + DEVELOPMENT AC, DOCUMENTATION with Structure/Deployment/Tests/QA) and bug (STEPs TO REPRODUCE, EXPECTED/ACTUAL RESULT). Includes PROPERTIES block with UUID, metadata headers (TITLE, SUBTITLE, AUTHOR, EMAIL, DATE, FILETAGS, SOURCE_URL, LAST_UPDATE, STORY_POINTS), TABLE OF CONTENTS, TODO TICKET TASKS with org-mode checkboxes, RELEVANT LINKS, and COMMENTS. Supports creating new nodes, updating existing nodes, validating development against ticket acceptance criteria, and managing the two-layer index file (BACKLOG + Sprint Board). Trigger when: user says 'create node', 'create roam', 'roam node', 'ticket node', 'org node', 'create node for DOTCOMPB', 'update node', 'validate development', 'check ticket', 'update index', 'sync index', or any variation involving creating/updating org-roam documentation from JIRA tickets."
metadata:
  author: Kyonax
  version: "3.1.0"
---

# Roam Node Skill

Generates consistent org-roam `.org` files from JIRA tickets for the Madison Reed project. Each node serves as structured documentation that tracks acceptance criteria, development tasks, QA instructions, and can be used to validate whether a development matches the ticket requirements. The index file provides a two-layer dashboard (BACKLOG + Sprint Board) that stays in sync with JIRA status.

## When to Apply

Reference these guidelines when:

*   Creating a new org-roam node from a JIRA ticket.
*   Updating an existing node with development progress, QA instructions, or JIRA data refresh.
*   Validating a `git diff` against a node's acceptance criteria (development validation mode).
*   Managing the index file — adding entries, moving tickets between Sprint Board lanes, syncing JIRA status.
*   Parsing JIRA ticket descriptions (6 format variants) into normalized GIVEN/WHEN/THEN acceptance criteria.
*   Writing or fixing org-mode syntax in node files (emphasis, property drawers, lists, tables, checkboxes).
*   Filling Phase 2 sections (DEVELOPMENT AC, DOCUMENTATION, QA, tests) with development content.

## When to Read Which Rules

| If working on...                                                        | Read these rules                                                           |
|-------------------------------------------------------------------------|----------------------------------------------------------------------------|
| Creating a new node from a JIRA ticket                                  | `rules/node-lifecycle.md` + `rules/templates.md` + `rules/jira-parsing.md` |
| Parsing a JIRA ticket description or translating content to org-mode    | `rules/jira-parsing.md`                                                    |
| Choosing or filling a template (standard vs bug)                        | `rules/templates.md`                                                       |
| Fixing org-mode syntax or checking rendering rules                      | `rules/org-mode-reference.md`                                              |
| Updating the index file after any node operation                        | `rules/index-management.md`                                                |
| Validating development against a node's acceptance criteria             | `rules/node-lifecycle.md`                                                  |
| Updating an existing node (post-development, QA, PR)                    | `rules/node-lifecycle.md` + `rules/templates.md`                           |
| Understanding the two-layer index architecture (BACKLOG + Sprint Board) | `rules/index-management.md`                                                |
| Moving a ticket between Sprint Board lanes or syncing JIRA status       | `rules/index-management.md`                                                |
| Filling Phase 2 sections (DEVELOPMENT AC, DOCUMENTATION, QA, tests)     | `rules/writing-standards.md` + `rules/templates.md`                        |
| Writing content into any node section (tone, clarity, terminology)       | `rules/writing-standards.md`                                               |

## Quick Reference

| Rule                 | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|----------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `templates`          | Node templates — base structure, standard and bug variants, metadata header rules (TITLE, SUBTITLE, AUTHOR, FILETAGS, etc.), section formatting (SCREAMING CASE, TOC links, AC format), file naming pattern, UUID generation, template variant selection by JIRA issue type.                                                                                                                                                                                                                                         |
| `jira-parsing`       | JIRA ticket parsing — 6 description format variants (italic headers, scenario-based, grouped AC, numbered headings, flat/inline, bug format), AC extraction and normalization to GIVEN/WHEN/THEN, content translation rules (markdown to org-mode), field mapping (summary stripping, story points, issue type Spanish labels), event tracking table capture, Figma/Slack link extraction.                                                                                                                           |
| `org-mode-reference` | Org-mode formatting syntax — emphasis PRE/POST boundary rules, property drawer placement (file-level and headline-level), list continuity and nesting, checkbox syntax (`- [ ]`/`- [X]`), statistics cookies and counting scope, tags, table syntax and TBLFM placement, code blocks with comma-escaping, org-roam cross-references (`id:UUID` links), comments, common pitfalls.                                                                                                                                    |
| `index-management`   | Index file management — two-layer architecture (BACKLOG source of truth + Sprint Board workflow view), entry formats (org-roam links vs file links), JIRA status to lane mapping (Spanish/English status names), parent-child nesting rules, checkbox lifecycle (JIRA override, transient signals), mandatory 8-step update flow, statistics cookie recalculation, integrity validation, edge cases (unassigned, reassigned, deleted tickets).                                                                       |
| `node-lifecycle`     | Node lifecycle and process — 7-step execution flow (identify → check existing → fetch JIRA → determine template → generate → write → update index), two-phase lifecycle (creation from JIRA vs developer fill), development validation mode (cross-reference git diff against TICKET CONTEXT AC + EVENT TRACKING + DEVELOPMENT AC), YAML validation output format, node update mode (preserve UUID, update checkboxes, fill DOCUMENTATION), quality checklist (metadata, validation completeness, structure, index). |
| `writing-standards`  | Writing standards for node content — four guiding principles (clarity, accuracy, consistency, specificity), active voice rules, terse technical writing style, per-section writing rules (TICKET CONTEXT, DEVELOPMENT AC, STRUCTURE AND FUNCTIONALITY, DEPLOYMENT NOTES, UNIT TEST COVERAGE, QA INSTRUCTIONS, COMMENTS, COMMIT MSG), terminology consistency rules (use =verbatim= for code references, never switch terms), propose-before-fill workflow for Phase 2 sections. |
