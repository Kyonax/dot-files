<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the `.brain.d` Org-Roam Knowledge Management session. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read the sections in order on first load — after that, reference them by number as needed. The data is organized into 5 sections:

| Section                        | Purpose                                                                                              | When to reference                                                                       |
|--------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| **1. Global Guidelines**       | Org-Roam conventions, file naming, tagging, and structural rules that apply to ALL work.             | Before creating or editing any node. These are mandatory constraints.                   |
| **2. Session Overview**        | High-level context: what this brain contains, active domains, directory structure, pending work.      | When starting a new task — understand scope and structure first.                        |
| **3. Implementations**         | Per-domain detail: what each subdirectory covers, key nodes, active projects, infrastructure work.   | When working on a specific domain or needing to locate existing documentation.          |
| **4. File Index**              | Quick-reference of all major file paths: indices, templates, configs, LaTeX exports.                 | When you need to read, edit, or reference a specific file without searching.            |
| **5. Last Interaction**        | Short-term memory: what was done last, what's pending, where to resume.                              | At the very start of a new conversation — this is your entry point for continuing work. |

**Operational Rule:** Throughout the conversation, the user will give new tasks. **Always look for the last request, identified by a markdown title starting with `###`.** Any `###` title means it is the newest request. Based on that section, load relevant context and apply the rules from Section 1.

**Key principle:** Data may appear in multiple sections with different framing. Section 1 frames it as a rule to follow. Section 2 frames it as context to understand. Section 3 frames it as an implementation to reference. This is intentional — each section answers a different question about the same knowledge.

---

## SECTION 1: GLOBAL GUIDELINES & CONVENTIONS

> **Apply these rules to every task in this session.** The loaded skills for this project — `mr-roam-node` (org-roam node creation and documentation patterns) and `emacs-expert` (Doom Emacs configuration and workflow) — cover general org-roam and Emacs conventions. This section stores session-scoped patterns specific to the `.brain.d` repository that those skills don't enforce yet. Treat them as mandatory constraints alongside the skill rules.

### 1.1 File Naming & Structure

*   **Date-prefixed filenames:** All org files follow `YYYY-MM-DD-slug_name.org` format.
*   **Subdirectory placement:** Every node belongs in a subdirectory under `roam-nodes/` based on its domain (e.g., `knowledge/`, `madison_reed/`, `personal_stuff/`, `cyber_code_syndicate/`).
*   **Org-Roam ID required:** Every file MUST have a `:PROPERTIES:` drawer with `:ID:` at the top. This is how Org-Roam tracks and links nodes.
*   **FILETAGS:** Every file uses `#+FILETAGS:` for categorization (e.g., `:KYO:`, `:MR:`, `:CCS:`, `:INDEX:`).
*   **Index naming:** Index nodes use the pattern `index_*.org` and are tagged with `:INDEX:`.

### 1.2 Node Types & Templates

*   **Index nodes:** Entry points for a domain. Contain table of contents and `[[id:UUID][Display Text]]` links to child nodes. Created with capture key `x`.
*   **Documentation nodes:** Detailed write-ups on specific topics. Created from `templates/new-node-doc.org` with capture key `c`.
*   **Project/Ticket nodes:** Track JIRA tickets or project tasks. Created from `templates/new-node-project.org` with capture key `p`.
*   **Invoice nodes:** Created from `templates/new-node-invoice.org` with capture key `v`.
*   **Sentinel Inspection nodes:** Created from `templates/new-node-sentinel-inspection.org` with capture key `i`.

### 1.3 Metadata Headers

*   **Standard headers:** `#+TITLE:`, `#+SUBTITLE:`, `#+DESCRIPTION:`, `#+FILETAGS:`, `#+AUTHOR:`, `#+EMAIL:`.
*   **Author:** Always `Cristian D. Moreno - Zerønet Labs~\orcidlink{0009-0006-4459-5538}\affiliation{Senior Full-Stack Engineer, Zerønet Labs}`. This LaTeX-compatible format includes the ORCID link and affiliation inline. Applied to ALL nodes without exception.
*   **Email:** `kyonax.corp@gmail.com` (default), or `iam@kyonax.com` for personal/diary nodes.
*   **Header order:** TITLE → SUBTITLE → DESCRIPTION → FILETAGS → AUTHOR → EMAIL (mandatory).
*   **All six headers are required** on every index node. Non-index nodes require at minimum TITLE, FILETAGS, AUTHOR.

### 1.4 Linking & Graph Integrity

*   **Internal links:** Always use Org-Roam ID links: `[[id:UUID][Display Text]]`.
*   **Cross-domain linking:** Index nodes link to child nodes. Child nodes can back-link to indices.
*   **No orphan nodes:** Every node must be reachable from at least one index.
*   **Prefer linking over duplicating:** If knowledge exists in another node, link to it — don't copy.
*   **Orphan validation method:** To find true orphans, grep each node's `:ID:` across ALL `.org` files — not just indices. Nodes may be linked from child nodes (e.g., index → JS node → ReactJS node). Only nodes whose ID appears in no other file are true orphans.

### 1.5 Index Formatting & Content Lifecycle

*   **Section structure:** Index headings (`*`) get a short one-line description underneath. Child nodes are plain list items (`-`), never subheadings (`**`).
*   **Inline descriptions:** Every child link gets a `:: description` — concise, self-explanatory of the node's content.
*   **Markup rules:**
    *   `*bold*` for important nouns: companies, teams, technologies, platforms, key concepts.
    *   `/italic/` for enumerated categories or parenthetical notes.
    *   `=verbatim=` for literal values like brand handles (e.g., `=@is.kyonax=`).
    *   `~code~` for inline code references.
    *   Org comments (`#`) only when necessary — no decorative comments.
*   **FILETAGS format:** Compact, no spaces between tags: `:KYO:INDEX:` not `:KYO: :INDEX:`. Domain tag comes first, then `:INDEX:` (e.g., `:MR:INDEX:`, `:CCS:INDEX:`, `:KYO:ARCH:INDEX:`).
*   **Content lifecycle:** Important content earns its own roam node — never dump inline in an index. *Quick Notes* is a temporary scratch area; anything left there more than a few days without promotion gets removed.
*   **WORK & PROJECTs scope:** Professional work, side projects, personal projects — anything that generates income.
*   **Sensitive data:** Passwords and passphrases belong in `kyo-utils` repo, not in `.brain.d`. Bank accounts go under a general *FINANCE INFO* section, never bank-specific sections.
*   **Review labels:** Nodes pending future removal use `=[REVIEW: pending removal]=` in their `:: description`.

### 1.6 Infrastructure & Symlinks

*   **External disk dependency:** `~/.config/doom/` symlinks (`config.org`, `gptel-directives`, `snippets`, `templates`) originate from `/run/media/kyonax/Da_ Disk/dev/github-kyonax/dot-files/.config/doom-mac/`.
*   **Skills symlinks:** GPTel skills in `~/.config/doom/gptel-directives/skills/` are symlinked into `~/.claude/skills/` so both Doom Emacs GPTel and Claude Code share the same skill definitions.

### 1.7 Madison Reed Index — Sprint Board & Backlog Conventions

*   **Two-layer structure:** SPRINT BOARD uses `file:` links with `::anchor` references; BACKLOG uses `[[id:UUID][Title]]` links with `<<anchor>>` targets. Sprint board entries point to backlog anchors.
*   **JIRA status mapping:** Sprint board sections map to JIRA workflow: IN TODO, IN PROGRESS, IN CODE REVIEW, IN TEST, ALL DONE. Tickets move between sections as status changes.
*   **Backlog entries:** Format `- [X/space] [[id:UUID][(TYPE) Ticket #DOTCOMPB-NNNN]] <<type-NNNN>> :: Description`. Checked `[X]` = done, unchecked `[ ]` = open.
*   **Sprint board entries:** Format `- [X/space] [[file:./index_file.org::anchor][(TYPE) Short Title]] :: Description`.
*   **Counter updates:** Both `** ALL DONE [N/N]` and `* BACKLOG [%%] [N/N]` counters must be updated when adding/removing entries.
*   **JIRA verification:** When adding tickets to the index, verify current status via JIRA MCP (`madison-reed.atlassian.net` as cloudId) before placing in the correct section.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

`~/.brain.d/` is Kyonax's **Org-Roam personal knowledge management system** — a second brain built on Doom Emacs that consolidates professional work tracking, technical learning, personal documents, content creation strategy, and community management into a single, interconnected graph of 120+ org-mode files. Git remote: `https://github.com/kyonax/roam-brain-emacs`.

### 2.2 Scope

| Item                              | Type           | Summary                                                              | Status          |
|-----------------------------------|----------------|----------------------------------------------------------------------|-----------------|
| Repository exploration            | Discovery      | Full structure analysis of all 7 domain subdirectories               | **DONE**        |
| Doom Emacs config analysis        | Discovery      | Org-Roam, GPTel, LaTeX, keybinding config in `config.el`            | **DONE**        |
| GPTel skills → Claude symlinks    | Infrastructure | 7 skills symlinked from `gptel-directives/skills/` to `~/.claude/skills/` | **DONE**   |
| Session file creation             | Documentation  | This context block created following session-reset architecture      | **DONE**        |
| Session file reset (skill-based)  | Documentation  | Rewrite to comply with session-reset skill 5-section rules          | **DONE**        |
| Kyonax master index refactor      | Refactor       | Library-style reorganization: removed 12 inline sections, linked 4 domain indices | **DONE** |
| Index formatting rules            | Documentation  | Abstracted patterns from refactor decisions into Section 1.5        | **DONE**        |
| Index linking expansion           | Refactor       | Added CCS, Documentation, Arch Linux, Gentleman Staff to master index | **DONE**      |
| Full index refinement pass        | Refactor       | Applied Section 1 guidelines to all 8 linked indices                | **DONE**        |
| Coding Knowledge content extraction | Refactor     | Extracted 3 inline content blocks into separate nodes               | **DONE**        |
| Author orcidlink standardization  | Refactor       | Applied Zerønet Labs orcidlink author format to all 12 index/node files | **DONE**    |
| Session reset (2nd)               | Documentation  | Second session reset capturing all refinement work                  | **DONE**        |
| Orphan node discovery & validation | Audit         | Full graph trace to find true orphan nodes across 80+ .org files    | **DONE**        |
| MR index — JIRA ticket integration | Refactor      | Added 6 orphan JIRA ticket nodes to Madison Reed index via MCP      | **DONE**        |
| Therapy node creation             | New node       | Personal therapy tracking node with Carta de Sanacion linked         | **DONE**        |
| Session reset (3rd)               | Documentation  | Third session reset capturing orphan audit + JIRA integration work  | **DONE**        |

### 2.3 Key Decisions (Session-Wide)

1.  **(2026-04-05)** Session file named `brain-d-knowledge-management.md` — covers the full `.brain.d` repo, not a single ticket or feature. Cross-cutting session.
2.  **(2026-04-05)** Skills shared between GPTel and Claude Code via symlinks — single source of truth for skill definitions across both AI tools.
3.  **(2026-04-05)** Section 3 organized by domain (not by individual node) since this is a PKM session, not a code session. Each domain is a logical unit of work.
4.  **(2026-04-05)** Master index refactored to library-style: index is a catalog of domain indices, not a content dump. Removed 12 stale/inline sections. Initially linked 4 domain indices; later expanded to 8.
5.  **(2026-04-05)** Passwords/passphrases moved to `kyo-utils` repo. Bank accounts under general FINANCE INFO section.
6.  **(2026-04-05)** Index formatting convention established: plain list items (`-`) with `:: description`, `*bold*` for key nouns, `/italic/` for categories, compact FILETAGS.
7.  **(2026-04-05)** CCS placed under WORK & PROJECTs, Arch Linux under HOME & PERSONAL, Documentation as its own new section in master index.
8.  **(2026-04-05)** Gentleman Staff linked under WORK & PROJECTs with `=[REVIEW: pending removal]=` label — user intends to remove it in the future.
9.  **(2026-04-05)** Inline content extracted from Coding Knowledge index (441 → 38 lines): created 3 new nodes (Senior Interview Q&A, Design Patterns, JavaScript Yasnippets).
10. **(2026-04-05)** Author tag standardized to Zerønet Labs orcidlink format across ALL files: `Cristian D. Moreno - Zerønet Labs~\orcidlink{0009-0006-4459-5538}\affiliation{Senior Full-Stack Engineer, Zerønet Labs}`. Replaces previous `Cristian D. Moreno - Kyonax` convention.
11. **(2026-04-05)** FILETAGS ordering convention: domain tag first, then `:INDEX:` (e.g., `:MR:INDEX:` not `:INDEX:MR:`).
12. **(2026-04-05)** Completed tasks in indices should be cleaned up (e.g., Content Creation `[5/5]` social accounts task removed).
13. **(2026-04-05)** Empty index sections (NOTEs, TASKs with no content) should be removed rather than left as empty headings.
14. **(2026-04-05)** Doom Emacs Comprehensive Guide section removed from Documentation index — was inline TODO with empty sub-sections. Can be recreated as its own node if needed.
15. **(2026-04-05)** Orphan validation requires full graph trace — grep each node's `:ID:` across ALL `.org` files, not just index files. Nodes linked from child nodes (e.g., index → JS → ReactJS) are not orphans. Initial scan found 23 candidates; full trace reduced to 1 true orphan.
16. **(2026-04-05)** 5 Nav Redesign bugs (DOTCOMPB-7749, 7756, 7759, 7760, 7761) confirmed Done via JIRA MCP and added to MR index ALL DONE + BACKLOG as checked. DOTCOMPB-7290 added to BACKLOG unchecked — JIRA returned permission error.
17. **(2026-04-05)** DOTCOMPB-7759 has `:FASTFOLLOW:` FILETAG but JIRA confirms Finalizada — FILETAGS discrepancy pending user update.
18. **(2026-04-05)** Therapy node created for weekly psychological treatment tracking. Placed in `personal_stuff/` with `:KYO:PERSONAL:` tags. Linked from master index HOME & PERSONAL (now 6 items).
19. **(2026-04-05)** Carta de Sanacion - Culpa (only true orphan found) linked from new Terapia Psicologica node — graph integrity restored, zero orphans remaining.

### 2.4 Pending Work

- [ ] Update DOTCOMPB-7759 node FILETAGS from `:MR:TICKET:BUG:FASTFOLLOW:` to `:MR:TICKET:BUG:DONE:` — JIRA confirms Finalizada
- [ ] Verify DOTCOMPB-7290 JIRA status when access is restored — currently unchecked in BACKLOG due to permission error

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 Repository Exploration & Context Loading

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   Full directory structure analysis: 7 domain subdirectories under `roam-nodes/`, plus `latex/`, `agenda/`, `templates/`, `bookmarks/`, `media/`.
*   120+ org files identified across all domains (increased from 118 after content extraction).
*   Doom Emacs integration mapped: org-roam config (lines 371-452 in `config.el`), GPTel config (lines 675-806), capture templates, keybindings.
*   Key entry points identified: master index (`2023-04-19-index_kyonax.org`), domain-specific indices for all active domains.

### 3.2 GPTel Skills → Claude Code Symlinks (Infrastructure)

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Problem:** GPTel skills defined in `~/.config/doom/gptel-directives/skills/` were not available to Claude Code.
*   **Fix:** Created symlinks in `~/.claude/skills/` pointing to each skill directory:
    *   `code-review`, `emacs-expert`, `mr-dotcom-dev`, `mr-roam-node`, `seo-web-quality`, `session-reset`, `skill-architect`
*   **Result:** 8 total skills in `~/.claude/skills/` (7 GPTel + 1 pre-existing `omarchy`).

### 3.3 Kyonax Master Index Refactor & Expansion

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Phase 1 — Library-style refactor:** Reduced from 1065-line scratch pad to ~55-line catalog. Removed 12 stale/inline sections. Initially linked 4 domain indices (Madison Reed, Coding Knowledge, Content Creation, Diary).
*   **Phase 2 — Index expansion:** Added 4 more links:
    *   Arch Linux → HOME & PERSONAL section
    *   Cyber Code Syndicate → WORK & PROJECTs section
    *   Gentleman Staff → WORK & PROJECTs section (with `=[REVIEW: pending removal]=`)
    *   Documentation → New DOCUMENTATION section (between CONTENT CREATION and DIARY)
*   **Phase 3 — Therapy node addition (2026-04-05):** Added Terapia Psicologica to HOME & PERSONAL section (6th item).
*   **Final structure (8 sections):** HOME & PERSONAL (6 items), WORK & PROJECTs (3 items), KNOWLEDGE & LEARNING (1 item), CONTENT CREATION (1 item), DOCUMENTATION (1 item), DIARY (1 item), FINANCE INFO (table), QUICK NOTEs.

### 3.4 Full Index Refinement Pass

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

Applied Section 1 guidelines systematically to all 8 indices linked from Index Kyonax. Common fixes across multiple files:

*   **Headers:** Added missing `#+SUBTITLE:`, `#+DESCRIPTION:`, `#+EMAIL:`. Fixed header order to TITLE → SUBTITLE → DESCRIPTION → FILETAGS → AUTHOR → EMAIL. Uppercased lowercase headers.
*   **FILETAGS:** Fixed spacing (`:KYO: :INDEX:` → `:KYO:INDEX:`), fixed domain ordering (`:INDEX:MR:` → `:MR:INDEX:`), added missing `:INDEX:` tags, corrected wrong domain tags (Diary had `:MR:` instead of `:KYO:DIARY:`).
*   **Author:** Standardized to `Cristian D. Moreno - Kyonax` (later updated to orcidlink format).
*   **Structure:** Converted `**` subheading children to `-` list items with `:: description`. Added one-line section descriptions under all headings. Removed empty sections.
*   **Content lifecycle:** Removed completed `[5/5]` task from Content Creation. Removed inline DOOM EMACS guide TODO from Documentation.

Per-index specifics:
*   **Arch Linux:** Fixed generic subtitle, added `:KYO:` to FILETAGS.
*   **CCS:** Added SUBTITLE/DESCRIPTION, converted SOCIAL task from `**` to list item, added section descriptions.
*   **Documentation:** Full rewrite — fixed typo "PROYECTs", flattened CV nesting, removed inline Doom Emacs guide, reorganized into 4 clean sections.
*   **Madison Reed:** Added SUBTITLE/DESCRIPTION, fixed intro grammar ("actual" → "current", "my role be" → "my role is to be").
*   **Coding Knowledge:** Full rewrite — extracted 400+ lines of inline content into 3 new nodes (see Section 3.5), reorganized into 5 sections.
*   **Content Creation:** Full rewrite — consolidated brand sections into BRANDs list items, removed completed social accounts task.
*   **Gentleman Staff:** Added SUBTITLE/DESCRIPTION, removed empty NOTEs/TASKs sections, converted DA'DOCs to list item.
*   **Diary:** Added SUBTITLE/DESCRIPTION, fixed FILETAGS from `:INDEX:MR:` to `:KYO:DIARY:INDEX:`, fixed header order.

### 3.5 Content Extraction from Coding Knowledge

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Problem:** `index_coding_knowledge.org` was 441 lines with interview Q&A, design patterns, yasnippets, and code examples dumped inline — violating Section 1.5 content lifecycle rule.
*   **Fix:** Extracted into 3 new nodes under `roam-nodes/knowledge/`:
    *   `2026-04-05-senior_interview_qa.org` (ID: `ac1a16d7-df12-4cfa-835c-6f33a3d52ec7`) — Q&A for ReactJS, JS, NextJS, AWS, TypeScript, Storybook, CI/CD, Jest + interview prep checklist
    *   `2026-04-05-design_patterns.org` (ID: `b28c9d4e-0683-4906-b47d-d0cc84c6ed56`) — Creational, structural, behavioral patterns with React/Next examples
    *   `2026-04-05-javascript_yasnippets.org` (ID: `62c11856-d6e1-4f25-ae82-c6ef4de1a091`) — Emacs yasnippet templates for JS file/method documentation (CCS code style)
*   **Result:** Index reduced from 441 → 38 lines. 5 clean sections: CURRENTLY LEARNING, STUFF TO LEARN, INTERVIEW PREP, DESIGN PATTERNs, TOOLING.

### 3.6 Author Orcidlink Standardization

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Problem:** Author tags inconsistent across files — some had `Cristian D. Moreno - Kyonax`, others `Kyonax - Cristian Moreno`, Madison Reed had `Cristian D. Moreno - Agile Engine`. Diary had the orcidlink format but was the only one.
*   **Fix:** Applied `Cristian D. Moreno - Zerønet Labs~\orcidlink{0009-0006-4459-5538}\affiliation{Senior Full-Stack Engineer, Zerønet Labs}` to all 12 files:
    *   9 index files: Kyonax, Arch Linux, CCS, Documentation, Madison Reed, Coding Knowledge, Content Creation, Gentleman Staff, Diary
    *   3 extracted nodes: JS Yasnippets, Senior Interview Q&A, Design Patterns
*   **Session guideline updated:** Section 1.3 now mandates orcidlink format for all nodes.

### 3.7 Orphan Node Discovery & Graph Validation

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Problem:** User created new roam nodes (JIRA tickets, personal notes) that were not linked from any index — violating Section 1.4 "no orphan nodes" rule.
*   **Phase 1 — Initial scan:** Cross-referenced all `.org` file IDs against index files. Found 23 candidate orphans across knowledge (13), MR tickets (6), content creation (1), community (1), personal (1), master index (1, self-referential).
*   **Phase 2 — Full graph trace:** Grepped each candidate's `:ID:` across ALL `.org` files (not just indices). Discovered most were linked through child node chains:
    *   Knowledge nodes (ReactJS, JS, VueJS, etc.) → linked from Coding Knowledge index or from child nodes (e.g., JavaScript node links to ReactJS, NodeJS, NextJS, LeetCode)
    *   Brand Identity @is.kyonax → linked from `content_creation_is_kyonax.org` (child of Content Creation index)
    *   DOC GS Dynamic Roles → linked from `all_docs_gentleman_staff.org` (child of GS index)
    *   DOTCOMPB-6571 docs (2 nodes) → linked from `docs_madison.org` (child of MR index DA' DOCs)
*   **Result:** 23 candidates → **1 true orphan** (Carta de Sanacion - Culpa). 6 JIRA ticket nodes were also unlinked and needed integration (see Section 3.8).

### 3.8 Madison Reed Index — JIRA Ticket Integration

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Problem:** 6 JIRA ticket roam nodes existed but were not in the Madison Reed index.
*   **JIRA verification via MCP** (cloudId: `madison-reed.atlassian.net`):

| Ticket         | Type | JIRA Status      | Node ID                                |
|----------------|------|------------------|----------------------------------------|
| DOTCOMPB-7749  | Bug  | Finalizada (Done)| `a3f8c211-4e7b-4c12-9d85-3b7f2e910c44` |
| DOTCOMPB-7756  | Bug  | Finalizada (Done)| `b7d2e405-8a1c-4f3e-b692-5c8d9f120d77` |
| DOTCOMPB-7759  | Bug  | Finalizada (Done)| `c1e9b307-2f6a-4d8c-a541-7e3b6c204e88` |
| DOTCOMPB-7760  | Bug  | Finalizada (Done)| `d4a7f509-6e3b-4a1d-c823-9f2e8d316f99` |
| DOTCOMPB-7761  | Bug  | Finalizada (Done)| `e8b3c612-1f4d-4e7a-d954-2a5c7e428b00` |
| DOTCOMPB-7290  | N/A  | Error (no perm)  | `f8b24883-5442-4e45-9dea-55c0f4f0fc92` |

*   **Changes to `index_madison_reed.org`:**
    *   **ALL DONE section:** Added 5 Nav Redesign bugs as checked entries. Counter updated `[9/9]` → `[14/14]`.
    *   **BACKLOG section:** Added 5 Nav Redesign bugs as `[X]` + DOTCOMPB-7290 as `[ ]`. Counter updated `[9/21]` → `[14/27]`.
*   **All 5 Nav Redesign bugs** are children of DOTCOMPB-7463 (Navigation Redesign parent ticket).
*   **Pending:** DOTCOMPB-7759 FILETAGS say `:FASTFOLLOW:` but JIRA says Done. DOTCOMPB-7290 needs re-verification when JIRA access is restored.

### 3.9 Therapy Node Creation

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Context:** User follows weekly psychological therapy. The only true orphan node (Carta de Sanacion - Culpa) was a therapy exercise with no parent.
*   **Created:** `roam-nodes/personal_stuff/2026-04-05-terapia_psicologica.org` (ID: `4de20b88-5805-4d21-b430-13db7ebd8623`)
*   **FILETAGS:** `:KYO:PERSONAL:`
*   **Email:** `iam@kyonax.com` (personal node convention)
*   **Sections:**
    1.  CARTAS DE SANACION — healing letters (Carta de Sanacion - Culpa linked here)
    2.  ACUERDOS TERAPEUTICOS — commitments/agreements from sessions
    3.  SESIONES SEMANALES — chronological session log
    4.  REFLEXIONES — insights between sessions
    5.  NOTAS RAPIDAS — quick capture space
*   **Linked from:** Master index HOME & PERSONAL (6th item)
*   **Graph integrity:** Carta de Sanacion - Culpa now reachable via Master Index → Terapia Psicologica → Carta. Zero orphans remaining.

### 3.10 Domain: Madison Reed (38 nodes — ACTIVE)

**Created:** 2025-11-18 | **Last updated:** 2026-04-05
**Status:** ACTIVE

**Role:** Senior Frontend Engineer at Agile Engine, Dotcom Team.
**Tech Stack:** Vue 3 (Options API), Pug, Stylus, Vuex, Vitest.
**Index:** `roam-nodes/2025-11-18-index_madison_reed.org`
**Dedicated session file:** `sessions/site-revolution-redesign.md` (detailed Vue coding session with full component trees, ~117KB).

**Active work:**
*   Site Revolution Redesign — long-lived feature branch with HCB Booking Flow V2, Location Detail V2, Locations List
*   Navigation Redesign (DOTCOMPB-7463) — parent ticket with 5 completed child bugs (7749, 7756, 7759, 7760, 7761)
*   ADA accessibility compliance
*   Multiple JIRA tickets (DOTCOMPB-6943 through DOTCOMPB-7768)

**Index state (as of 2026-04-05):**
*   SPRINT BOARD: ALL DONE [14/14], IN PROGRESS [0/6], IN CODE REVIEW [0/2], IN TEST [0/3], IN TODO [0/1]
*   BACKLOG: [14/27] — 14 checked, 13 open

### 3.11 Domain: Knowledge Base (14 nodes — REFERENCE)

**Created:** 2024-12-18 | **Last updated:** 2026-04-05
**Status:** REFERENCE

**Index:** `roam-nodes/knowledge/2024-12-18-index_coding_knowledge.org`
**Topics:** JavaScript (ES6+, async, closures, event loop), ReactJS (hooks, performance), Next.js (SSR/SSG/ISR), Node.js, Ruby, Vue.js, AWS (S3, EC2, IAM, CI/CD), Design Patterns (Creational/Structural/Behavioral with React), Jest testing.
**Purpose:** Senior engineer interview preparation and continuous learning reference.
**Note:** 3 new nodes added (2026-04-05) via content extraction — Senior Interview Q&A, Design Patterns, JavaScript Yasnippets. Node count increased from 11 to 14.

### 3.12 Domain: Content Creation (2 nodes — ACTIVE)

**Created:** 2024-09-16 | **Last updated:** 2026-04-05
**Status:** ACTIVE

**Index:** `roam-nodes/2024-09-16-index_content_creation.org`
**Brands:** `@is.kyonax` (main personal), `@kyonax_on` (gaming/lore), `@kyonax_on_tech` (tech/programming), `Discord Creed` (Discord content), `Datomanía` (data/facts).
**Strategy:** Simultaneous shorts-based growth across YouTube, TikTok, Twitter, Instagram, Twitch, Facebook with cross-promotion.

### 3.13 Domain: Cyber Code Syndicate (4 nodes — IN DEVELOPMENT)

**Created:** 2025-11-01 | **Last updated:** 2026-04-05
**Status:** IN DEVELOPMENT

**Socials:** `@ccs_devhub` (X, GitHub, Instagram).
**Focus:** Free Software advocacy, open-source collaboration.
**Code style:** `snake_case` variables, `kebab-case` files, `UPPER_SNAKE_CASE` constants.
**Pending:** Manifesto creation, community launch.

### 3.14 Domain: Gentleman Staff (1 node — ACTIVE, REVIEW)

**Created:** 2025-12-23 | **Last updated:** 2026-04-05
**Status:** ACTIVE (pending removal from master index)

**Index:** `roam-nodes/2025-12-23-index_gentleman_staff.org`
**Role:** Discord server moderator for `@gentlemanprogramming` YouTube community.
**Note:** Linked to master index with `=[REVIEW: pending removal]=` label per user decision.

### 3.15 Domain: Personal (13 nodes — LIVING)

**Created:** ~2023-04-19 | **Last updated:** 2026-04-05
**Status:** LIVING

**Contents:** CVs (English, Spanish, JS-focused), family info (partner: Leidy Johana Guerrero), baby/family planning, career planning, birthday reminders, hiring interview notes, therapy tracking (Terapia Psicologica node with Carta de Sanacion - Culpa).
**Note:** Node count increased from 12 to 13 with addition of therapy node (2026-04-05).

### 3.16 Domain: Documentation (REFERENCE)

**Created:** 2024-09-16 | **Last updated:** 2026-04-05
**Status:** REFERENCE

**Index:** `roam-nodes/2024-09-16-index_documentation.org`
**Contents:** Brain Dump guide, Work Liquidation reference, Maritz/Softtek work docs, CVs (EN/ES), Invoice templates.
**Note:** Doom Emacs Comprehensive Guide section removed during refinement (was empty TODO stubs). Can be recreated as its own node when ready.

### 3.17 Domain: Omarchy / Arch Linux (ACTIVE — Separate Session)

**Created:** 2026-03-20 | **Last updated:** 2026-04-05
**Status:** ACTIVE (tracked in `sessions/omarchy-installation.md`)

**Summary:** Data recovery from broken Arch Linux system + Omarchy v3.4.2 installation. Phase 1 (recovery) complete, Phase 2a (ISO flash) in progress.
**Index:** `roam-nodes/2026-03-20-arch_linux.org`

---

## SECTION 4: FILE INDEX

### 4.1 Index Nodes

| File                                                              | Domain              |
|-------------------------------------------------------------------|---------------------|
| `roam-nodes/2023-04-19-index_kyonax.org`                          | Master index        |
| `roam-nodes/2025-11-18-index_madison_reed.org`                    | Madison Reed        |
| `roam-nodes/knowledge/2024-12-18-index_coding_knowledge.org`      | Knowledge base      |
| `roam-nodes/2024-09-16-index_content_creation.org`                | Content creation    |
| `roam-nodes/2024-09-16-index_documentation.org`                   | Documentation       |
| `roam-nodes/2025-12-23-index_gentleman_staff.org`                 | Gentleman Staff     |
| `roam-nodes/2026-02-15-index_diary.org`                           | Daily agenda/diary  |
| `roam-nodes/2026-03-20-arch_linux.org`                            | Arch/Omarchy        |
| `roam-nodes/cyber_code_syndicate/2025-10-14-cyber_code_syndicate.org` | CCS             |

### 4.2 Extracted Nodes (2026-04-05)

| File                                                           | Source                |
|----------------------------------------------------------------|-----------------------|
| `roam-nodes/knowledge/2026-04-05-senior_interview_qa.org`      | Coding Knowledge      |
| `roam-nodes/knowledge/2026-04-05-design_patterns.org`          | Coding Knowledge      |
| `roam-nodes/knowledge/2026-04-05-javascript_yasnippets.org`    | Coding Knowledge      |

### 4.3 New Nodes (2026-04-05)

| File                                                           | Domain                |
|----------------------------------------------------------------|-----------------------|
| `roam-nodes/personal_stuff/2026-04-05-terapia_psicologica.org` | Personal — therapy    |

### 4.4 JIRA Ticket Nodes Added to MR Index (2026-04-05)

| File                                                                | Ticket          |
|---------------------------------------------------------------------|-----------------|
| `roam-nodes/madison_reed/2026-03-25-dotcompb_7749.org`              | DOTCOMPB-7749   |
| `roam-nodes/madison_reed/2026-03-25-dotcompb_7756.org`              | DOTCOMPB-7756   |
| `roam-nodes/madison_reed/2026-03-25-dotcompb_7759.org`              | DOTCOMPB-7759   |
| `roam-nodes/madison_reed/2026-03-25-dotcompb_7760.org`              | DOTCOMPB-7760   |
| `roam-nodes/madison_reed/2026-03-25-dotcompb_7761.org`              | DOTCOMPB-7761   |
| `roam-nodes/madison_reed/2026-03-12-114716-dotcompb_7290.org`       | DOTCOMPB-7290   |

### 4.5 Templates

| File                                         | Key | Purpose                  |
|----------------------------------------------|-----|--------------------------|
| `templates/new-node-doc.org`                 | `c` | Documentation node       |
| `templates/new-node-project.org`             | `p` | JIRA ticket/project node |
| `templates/new-node-invoice.org`             | `v` | Invoice                  |
| `templates/new-node-sentinel-inspection.org` | `i` | Inspection report        |

### 4.6 LaTeX & Exports

| File                             | Purpose                     |
|----------------------------------|-----------------------------|
| `latex/setup_latex.org`          | Central LaTeX configuration |
| `latex/cv-latex-export.org`      | CV export template          |
| `latex/invoice-latex-export.org` | Invoice export template     |
| `latex/ticket-mr-export.org`     | Madison Reed ticket export  |
| `latex/latex-macros.tex`         | Reusable LaTeX macros       |

### 4.7 Infrastructure

| File                             | Purpose                     |
|----------------------------------|-----------------------------|
| `agenda/agenda.org`              | Daily/weekly tasks & events |
| `bookmarks/bookmarks`           | URL bookmarks               |
| `roam-nodes/.virtual_brain`     | Archived project links      |

### 4.8 Symlinks (Claude Code ← GPTel Skills)

| Source (`~/.config/doom/gptel-directives/skills/`) | Target (`~/.claude/skills/`) |
|----------------------------------------------------|------------------------------|
| `code-review/`                                     | `code-review`                |
| `emacs-expert/`                                    | `emacs-expert`               |
| `mr-dotcom-dev/`                                   | `mr-dotcom-dev`              |
| `mr-roam-node/`                                    | `mr-roam-node`               |
| `seo-web-quality/`                                 | `seo-web-quality`            |
| `session-reset/`                                   | `session-reset`              |
| `skill-architect/`                                 | `skill-architect`            |

### 4.9 Session Files (GPTel Directives)

| File                                                                     | Domain                 |
|--------------------------------------------------------------------------|------------------------|
| `~/.config/doom/gptel-directives/sessions/site-revolution-redesign.md`   | Madison Reed Vue dev   |
| `~/.config/doom/gptel-directives/sessions/omarchy-installation.md`       | Arch/Omarchy migration |
| `~/.config/doom/gptel-directives/sessions/skill-architecture-master.md`  | Skills system          |
| `~/.config/doom/gptel-directives/sessions/dotcompb-7052-dynamic-yield-email-sms.md` | MR ticket  |
| `~/.config/doom/gptel-directives/sessions/brain-d-knowledge-management.md` | THIS SESSION         |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last

*   Ran full orphan audit: scanned 80+ .org files, found 23 candidate orphans, traced full graph via ID grep — reduced to 1 true orphan (Carta de Sanacion - Culpa) + 6 unlinked JIRA ticket nodes.
*   Verified 6 JIRA tickets via MCP: 5 Nav Redesign bugs confirmed Finalizada, DOTCOMPB-7290 returned permission error.
*   Updated Madison Reed index: added 5 bugs to ALL DONE [14/14] + 6 entries to BACKLOG [14/27].
*   Created therapy node (`terapia_psicologica.org`) with 5 sections, linked Carta de Sanacion - Culpa, added to master index HOME & PERSONAL.
*   Performed 3rd session reset.

### Pending / Not yet started

*   Update DOTCOMPB-7759 FILETAGS from `:FASTFOLLOW:` to `:DONE:` (JIRA confirms Finalizada)
*   Re-verify DOTCOMPB-7290 JIRA status when access is restored

### Where to resume

If the user asks to **update a JIRA ticket status**: follow Section 1.7 conventions, verify via MCP, update both Sprint Board and Backlog sections in MR index.
If the user asks to **create a new node**: apply Section 1 naming/tagging rules + Section 1.3 orcidlink author + Section 1.5 formatting rules, use appropriate template (Section 1.2), place in correct subdirectory.
If the user asks to **edit an index**: follow Section 1.5 conventions (list items, `:: descriptions`, bold/italic markup, content lifecycle).
If the user asks to **work on a specific domain**: load the relevant Section 3 subsection for context, check the domain's index node.
If the user asks to **run an orphan audit**: follow Section 1.4 orphan validation method — grep IDs across all .org files, not just indices.
If the user asks to **remove Gentleman Staff**: remove the link from master index WORK & PROJECTs (it has the REVIEW label).
If the user asks about **infrastructure**: reference Section 3.2 (skill symlinks) for prior decisions.
If the user asks for a **new task**: check Section 2.4 (Pending Work).

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
