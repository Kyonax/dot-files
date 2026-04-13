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
*   **Subdirectory placement:** Every node belongs in a subdirectory under `roam-nodes/` based on its domain (e.g., `knowledge/`, `madison_reed/`, `personal_stuff/`, `cyber_code_syndicate/`, `content_creation/`).
*   **Index files at root:** All index nodes live at the root of `roam-nodes/`, never inside subdirectories. Only non-index child nodes go in subdirectories.
*   **Org-Roam ID required:** Every file MUST have a `:PROPERTIES:` drawer with `:ID:` at the top. This is how Org-Roam tracks and links nodes.
*   **FILETAGS:** Every file uses `#+FILETAGS:` for categorization (e.g., `:KYO:`, `:MR:`, `:CCS:`, `:INDEX:`).
*   **Index naming:** Index nodes use the pattern `index_*.org` and are tagged with `:INDEX:`.

### 1.2 Node Types & Templates

*   **Index nodes:** Entry points for a domain. Contain table of contents and `[[id:UUID][Display Text]]` links to child nodes. Created with capture key `x`.
*   **Documentation nodes:** Detailed write-ups on specific topics. Created from `templates/new-node-doc.org` with capture key `c`.
*   **About nodes:** Manifesto-style documents for groups/collectives (e.g., About CCS, About Synchronous). Follow the pattern: abstract, key terms, principles, members, conclusion.
*   **Project/Ticket nodes:** Track JIRA tickets or project tasks. Created from `templates/new-node-project.org` with capture key `p`.
*   **Invoice nodes:** Created from `templates/new-node-invoice.org` with capture key `v`.
*   **Sentinel Inspection nodes:** Created from `templates/new-node-sentinel-inspection.org` with capture key `i`.

### 1.3 Metadata Headers

*   **Standard headers:** `#+TITLE:`, `#+SUBTITLE:`, `#+DESCRIPTION:`, `#+FILETAGS:`, `#+AUTHOR:`, `#+EMAIL:`.
*   **Author:** Always `Cristian D. Moreno - Kyonax~\orcidlink{0009-0006-4459-5538}\affiliation{Senior Full-Stack Engineer, Zerønet Labs}`. This LaTeX-compatible format includes the ORCID link and affiliation inline. Applied to ALL nodes without exception.
*   **Email:** `kyonax.corp@gmail.com` (default), or `iam@kyonax.com` for personal/diary nodes.
*   **Header order:** TITLE → SUBTITLE → DESCRIPTION → FILETAGS → AUTHOR → EMAIL (mandatory).
*   **All six headers are required** on every index node. Non-index nodes require at minimum TITLE, FILETAGS, AUTHOR.

### 1.4 Linking & Graph Integrity

*   **Internal links:** Always use Org-Roam ID links: `[[id:UUID][Display Text]]`.
*   **Cross-domain linking:** Index nodes link to child nodes. Child nodes can back-link to indices.
*   **No orphan nodes:** Every node must be reachable from at least one index. Run periodic orphan audits.
*   **Prefer linking over duplicating:** If knowledge exists in another node, link to it — don't copy.
*   **Parenthetical deep links:** Use `/(more in [[id:UUID][Display Text]])/` to link to About/detail nodes inline.

### 1.5 Index Formatting & Content Lifecycle

*   **Section structure:** Index headings (`*`) get a short one-line description underneath. Child nodes are plain list items (`-`), never subheadings (`**`).
*   **Inline descriptions:** Every child link gets a `:: description` — concise, self-explanatory of the node's content. Keep descriptions general in index files; specifics belong in each roam node.
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

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

`~/.brain.d/` is Kyonax's **Org-Roam personal knowledge management system** — a second brain built on Doom Emacs that consolidates professional work tracking, technical learning, personal documents, content creation strategy, and community management into a single, interconnected graph of 120+ org-mode files. Git remote: `https://github.com/kyonax/roam-brain-emacs`.

### 2.2 Scope

| Item                                | Type           | Summary                                                                           | Status     |
|-------------------------------------|----------------|-----------------------------------------------------------------------------------|------------|
| Repository exploration              | Discovery      | Full structure analysis of all 7 domain subdirectories                            | **DONE**   |
| Doom Emacs config analysis          | Discovery      | Org-Roam, GPTel, LaTeX, keybinding config in `config.el`                         | **DONE**   |
| GPTel skills → Claude symlinks      | Infrastructure | 7 skills symlinked from `gptel-directives/skills/` to `~/.claude/skills/`        | **DONE**   |
| Session file creation               | Documentation  | This context block created following session-reset architecture                   | **DONE**   |
| Kyonax master index refactor        | Refactor       | Library-style reorganization: removed 12 inline sections, linked 8 domain indices | **DONE**   |
| Index formatting rules              | Documentation  | Abstracted patterns from refactor decisions into Section 1.5                      | **DONE**   |
| Full index refinement pass          | Refactor       | Applied Section 1 guidelines to all 8 linked indices                              | **DONE**   |
| Coding Knowledge content extraction | Refactor       | Extracted 3 inline content blocks into separate nodes                             | **DONE**   |
| Author orcidlink standardization    | Refactor       | Applied Zerønet Labs orcidlink author format to all index/node files              | **DONE**   |
| Orphan audit & cleanup              | Refactor       | Found 13 orphans, linked 9, deleted 4 stale files                                | **DONE**   |
| Index file consolidation            | Refactor       | Moved 3 index files from subdirectories to `roam-nodes/` root                    | **DONE**   |
| Documentation index expansion       | Refactor       | Added PERSONAL section (6 CVs) and Leidy to WORK & PROJECTs                      | **DONE**   |
| Content Creation index refactor     | Refactor       | 3-pillar restructure, Datomanía removed, Synchronous added, Facebook removed      | **DONE**   |
| Synchronous node creation           | Content        | Created About Synchronous node in `content_creation/`                             | **DONE**   |

### 2.3 Key Decisions (Session-Wide)

1.  **(2026-04-05)** Session file named `brain-d-knowledge-management.md` — covers the full `.brain.d` repo, not a single ticket or feature. Cross-cutting session.
2.  **(2026-04-05)** Skills shared between GPTel and Claude Code via symlinks — single source of truth for skill definitions across both AI tools.
3.  **(2026-04-05)** Section 3 organized by domain (not by individual node) since this is a PKM session, not a code session. Each domain is a logical unit of work.
4.  **(2026-04-05)** Master index refactored to library-style: catalog of domain indices. Removed 12 stale/inline sections. Expanded to 8 domain links.
5.  **(2026-04-05)** Passwords/passphrases moved to `kyo-utils` repo. Bank accounts under general FINANCE INFO section.
6.  **(2026-04-05)** Index formatting convention established: plain list items (`-`) with `:: description`, `*bold*` for key nouns, `/italic/` for categories, compact FILETAGS.
7.  **(2026-04-05)** CCS placed under WORK & PROJECTs, Arch Linux under HOME & PERSONAL, Documentation as its own section in master index.
8.  **(2026-04-05)** Gentleman Staff linked under WORK & PROJECTs with `=[REVIEW: pending removal]=` label.
9.  **(2026-04-05)** Author tag standardized to Zerønet Labs orcidlink format across ALL files.
10. **(2026-04-05)** FILETAGS ordering convention: domain tag first, then `:INDEX:`.
11. **(2026-04-05)** Completed tasks in indices should be cleaned up. Empty index sections should be removed.
12. **(2026-04-05)** Index files must live at root of `roam-nodes/`, not inside subdirectories. Moved Baby, Coding Knowledge, and CCS indices to root.
13. **(2026-04-05)** Orphan audit: 4 stale files deleted (work_postulations, 2x maritz manuals, the_title test template). 9 orphans linked to proper indices.
14. **(2026-04-05)** Documentation index restructured: CVs section replaced with PERSONAL (6 CVs total — 2 general + 4 role-specific). Leidy CV added to WORK & PROJECTs.
15. **(2026-04-05)** Content Creation restructured around 3 pillars: /personal development/, /technology/, /community/. Datomanía removed entirely. Facebook removed from all platform plans.
16. **(2026-04-05)** Synchronous cross-creator collective added — YouTube-only shared channel model. About node created following About CCS structure pattern.
17. **(2026-04-05)** Content Creation index uses GROUPs & COLLECTIVEs section for community initiatives (CCS, Synchronous) with parenthetical deep links to About nodes.
18. **(2026-04-05)** Index descriptions should be general, not Kyonax-specific. Specific info belongs in each roam node.

### 2.4 Pending Work

- [ ] No pending tasks — session up to date after full refinement pass

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 Repository Exploration & Context Loading

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   Full directory structure analysis: 7 domain subdirectories under `roam-nodes/`, plus `latex/`, `agenda/`, `templates/`, `bookmarks/`, `media/`.
*   120+ org files identified across all domains.
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

*   **Phase 1 — Library-style refactor:** Reduced from 1065-line scratch pad to ~55-line catalog. Removed 12 stale/inline sections.
*   **Phase 2 — Index expansion:** Linked 8 domain indices across sections. Added Dragon File - Personal to HOME & PERSONAL.
*   **Final structure (8 sections):** HOME & PERSONAL (5 items), WORK & PROJECTs (3 items), KNOWLEDGE & LEARNING (1 item), CONTENT CREATION (1 item), DOCUMENTATION (1 item), DIARY (1 item), FINANCE INFO (table), QUICK NOTEs.

### 3.4 Full Index Refinement Pass

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

Applied Section 1 guidelines systematically to all indices. Common fixes: headers, FILETAGS, structure conversion (`**` → `-` list items), section descriptions, empty section removal.

Per-index specifics:
*   **Arch Linux:** Fixed generic subtitle, added `:KYO:` to FILETAGS.
*   **CCS:** Added SUBTITLE/DESCRIPTION, converted SOCIAL task from `**` to list item.
*   **Documentation:** Full rewrite — fixed typo, flattened CV nesting, added PERSONAL section (6 CVs), added Leidy to WORK & PROJECTs. Removed inline Doom Emacs guide.
*   **Madison Reed:** Added SUBTITLE/DESCRIPTION, fixed intro grammar.
*   **Coding Knowledge:** Full rewrite — extracted 400+ lines into 3 new nodes, added VueJS/AWS/Ruby links. Reorganized into 5 sections.
*   **Content Creation:** Full rewrite — see Section 3.9.
*   **Gentleman Staff:** Added SUBTITLE/DESCRIPTION, removed empty sections, converted DA'DOCs to list item.
*   **Diary:** Added SUBTITLE/DESCRIPTION, fixed FILETAGS and header order.

### 3.5 Content Extraction from Coding Knowledge

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   Extracted into 3 new nodes under `roam-nodes/knowledge/`:
    *   `2026-04-05-senior_interview_qa.org` — Q&A for ReactJS, JS, NextJS, AWS, TypeScript, Storybook, CI/CD, Jest
    *   `2026-04-05-design_patterns.org` — Creational, structural, behavioral patterns with React/Next examples
    *   `2026-04-05-javascript_yasnippets.org` — Emacs yasnippet templates for JS file/method documentation

### 3.6 Orphan Audit & Cleanup

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Audit:** Scanned all 67 .org files across 7 subdirectories. Found 13 orphan nodes + 5 phantom links.
*   **Deleted (4 files):** `work_postulations.org` (empty), `maritz_manual_shoptrontheme.org` (stale), `manual_offcanvas_modal.org` (stale), `the_title.org` (empty test template with typo).
*   **Linked to Coding Knowledge (3):** AWS, Ruby, VueJS.
*   **Linked to Index Kyonax (1):** Dragon File - Personal → HOME & PERSONAL.
*   **Linked to Documentation (5):** Leidy CV → WORK & PROJECTs. 4 Cristian CVs (Full Stack ES, Frontend EN, Full Stack EN, Backend EN) → new PERSONAL section.
*   **2 older CVs re-linked:** General EN and General ES CVs restored to Documentation PERSONAL (were lost when CVs section was replaced).
*   **Phantom links (5):** Kyonax's Org Agenda File, DOC Work Liquidation, Maritz Documentation, Softtek Documentation, Invoice Cabezarota — referenced from indices but no files exist. Left as-is.

### 3.7 Index File Consolidation

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Rule established:** Index files must live at root of `roam-nodes/`, not inside subdirectories (Section 1.1).
*   **Moved 3 files:**
    *   `personal_stuff/2024-06-09-index_baby.org` → `roam-nodes/2024-06-09-index_baby.org`
    *   `knowledge/2024-12-18-index_coding_knowledge.org` → `roam-nodes/2024-12-18-index_coding_knowledge.org`
    *   `cyber_code_syndicate/2025-10-14-cyber_code_syndicate.org` → `roam-nodes/2025-10-14-index_cyber_code_syndicate.org` (also added `index_` prefix)
*   All org-roam links use `:ID:` so moves don't break references.

### 3.8 Domain: Madison Reed (32 nodes — ACTIVE)

**Created:** 2025-11-18 | **Last updated:** 2026-04-05
**Status:** ACTIVE

**Role:** Senior Frontend Engineer at Agile Engine, Dotcom Team.
**Tech Stack:** Vue 3 (Options API), Pug, Stylus, Vuex, Vitest.
**Index:** `roam-nodes/2025-11-18-index_madison_reed.org`
**Dedicated session file:** `sessions/site-revolution-redesign.md` (~117KB).

**Active work:**
*   Site Revolution Redesign — HCB Booking Flow V2, Location Detail V2, Locations List
*   Navigation Redesign
*   ADA accessibility compliance
*   Multiple JIRA tickets (DOTCOMPB-6943 through DOTCOMPB-7652)

### 3.9 Domain: Content Creation (3 nodes — ACTIVE)

**Created:** 2024-09-16 | **Last updated:** 2026-04-05
**Status:** ACTIVE

**Index:** `roam-nodes/2024-09-16-index_content_creation.org`
**Three pillars:** /personal development/, /technology/, /community/.
**Strategy:** Shorts-first simultaneous high-volume content, cross-promoting between brands.

**Active brands:**
*   `@is.kyonax` — Desarrollo personal, life vlogs, autoconocimiento, reviews
*   `@kyonax_on_tech` — Coding, AI, teaching, tech divulgation
*   CCS (`@ccs_devhub`) — Free Software community, ethical dev, Hispanic developer empowerment

**Groups & Collectives:**
*   **Cyber Code Syndicate** — Free Software community (About node: `cyber_code_syndicate/2025-12-16-about_cyber_code_syndicate_ccs.org`)
*   **Synchronous** — YouTube cross-creator collective for mutual audience growth via cross-pollination. Kyonax participates as *Synk Kyonax*. Handle: `@synchronous_tm`. About node: `content_creation/2026-04-05-about_synchronous.org`

**Paused:** KYO (gaming), Discord Creed — no production until explicitly resumed.
**Removed:** Datomanía — deleted from all references (2026-04-05).
**Platforms:** Facebook removed from all brand plans (2026-04-05).

### 3.10 Domain: Knowledge Base (17 nodes — REFERENCE)

**Created:** 2024-12-18 | **Last updated:** 2026-04-05
**Status:** REFERENCE

**Index:** `roam-nodes/2024-12-18-index_coding_knowledge.org` (moved to root from `knowledge/`)
**Topics:** JavaScript, ReactJS, Next.js, Node.js, Ruby, VueJS, AWS, Design Patterns, Jest.
**Purpose:** Senior engineer interview preparation and continuous learning reference.
**Note:** Node count increased from 11 → 17. 3 nodes extracted from inline content (2026-04-05), 3 orphans linked (AWS, Ruby, VueJS).

### 3.11 Domain: Cyber Code Syndicate (4 nodes — IN DEVELOPMENT)

**Created:** 2025-11-01 | **Last updated:** 2026-04-05
**Status:** IN DEVELOPMENT

**Index:** `roam-nodes/2025-10-14-index_cyber_code_syndicate.org` (moved to root, added `index_` prefix)
**Socials:** `@ccs_devhub` (X, GitHub, Instagram).
**Focus:** Free Software advocacy, open-source collaboration, Hispanic developer empowerment.
**Code style:** `snake_case` variables, `kebab-case` files, `UPPER_SNAKE_CASE` constants.
**Pending:** Manifesto creation, community launch.
**Cross-reference:** Also listed in Content Creation index GROUPs & COLLECTIVEs section.

### 3.12 Domain: Gentleman Staff (1 node — ACTIVE, REVIEW)

**Created:** 2025-12-23 | **Last updated:** 2026-04-05
**Status:** ACTIVE (pending removal from master index)

**Index:** `roam-nodes/2025-12-23-index_gentleman_staff.org`
**Role:** Discord server moderator for `@gentlemanprogramming` YouTube community.
**Note:** Linked with `=[REVIEW: pending removal]=` label.

### 3.13 Domain: Personal (12 nodes — LIVING)

**Created:** ~2023-04-19 | **Last updated:** 2026-04-05
**Status:** LIVING

**Contents:** CVs (6 total: 2 general + 4 role-specific, all linked in Documentation PERSONAL), family info, baby/family planning, Dragon File (credentials), birthday reminders, hiring interview notes.

### 3.14 Domain: Documentation (REFERENCE)

**Created:** 2024-09-16 | **Last updated:** 2026-04-05
**Status:** REFERENCE

**Index:** `roam-nodes/2024-09-16-index_documentation.org`
**Sections:** HOME & PERSONAL (Brain Dump guide, Work Liquidation), WORK & PROJECTs (Maritz, Softtek, Leidy CV), PERSONAL (6 Cristian CVs), INVOICE TEMPLATEs.
**Note:** Phantom links exist for Work Liquidation, Maritz, and Softtek (files never created or deleted).

### 3.15 Domain: Omarchy / Arch Linux (ACTIVE — Separate Session)

**Created:** 2026-03-20 | **Last updated:** 2026-04-05
**Status:** ACTIVE (tracked in `sessions/omarchy-installation.md`)

**Summary:** Data recovery from broken Arch Linux system + Omarchy v3.4.2 installation.
**Index:** `roam-nodes/2026-03-20-arch_linux.org`

---

## SECTION 4: FILE INDEX

### 4.1 Index Nodes

| File                                                    | Domain           |
|---------------------------------------------------------|------------------|
| `roam-nodes/2023-04-19-index_kyonax.org`                | Master index     |
| `roam-nodes/2025-11-18-index_madison_reed.org`          | Madison Reed     |
| `roam-nodes/2024-12-18-index_coding_knowledge.org`      | Knowledge base   |
| `roam-nodes/2024-09-16-index_content_creation.org`      | Content creation |
| `roam-nodes/2024-09-16-index_documentation.org`         | Documentation    |
| `roam-nodes/2025-12-23-index_gentleman_staff.org`       | Gentleman Staff  |
| `roam-nodes/2026-02-15-index_diary.org`                 | Diary            |
| `roam-nodes/2026-03-20-arch_linux.org`                  | Arch/Omarchy     |
| `roam-nodes/2025-10-14-index_cyber_code_syndicate.org`  | CCS              |
| `roam-nodes/2024-06-09-index_baby.org`                  | Baby             |
| `roam-nodes/2025-02-25-index_invoice_cristian_d_moreno.org` | Invoices     |

### 4.2 Content Creation Nodes

| File                                                                             | Association        |
|----------------------------------------------------------------------------------|--------------------|
| `roam-nodes/content_creation/2025-05-07-content_creation_is_kyonax.org`          | @is.kyonax         |
| `roam-nodes/content_creation/2025-02-10-content_creation_kyonax_on_tech.org`     | @kyonax_on_tech    |
| `roam-nodes/content_creation/2026-04-05-about_synchronous.org`                   | Synchronous        |
| `roam-nodes/cyber_code_syndicate/2025-12-16-233429-about_cyber_code_syndicate_ccs.org` | CCS          |

### 4.3 Knowledge Nodes (extracted 2026-04-05)

| File                                                        | Source           |
|-------------------------------------------------------------|------------------|
| `roam-nodes/knowledge/2026-04-05-senior_interview_qa.org`   | Coding Knowledge |
| `roam-nodes/knowledge/2026-04-05-design_patterns.org`       | Coding Knowledge |
| `roam-nodes/knowledge/2026-04-05-javascript_yasnippets.org` | Coding Knowledge |

### 4.4 Templates

| File                                         | Key | Purpose                  |
|----------------------------------------------|-----|--------------------------|
| `templates/new-node-doc.org`                 | `c` | Documentation node       |
| `templates/new-node-project.org`             | `p` | JIRA ticket/project node |
| `templates/new-node-invoice.org`             | `v` | Invoice                  |
| `templates/new-node-sentinel-inspection.org` | `i` | Inspection report        |

### 4.5 LaTeX & Exports

| File                             | Purpose                     |
|----------------------------------|-----------------------------|
| `latex/setup_latex.org`          | Central LaTeX configuration |
| `latex/cv-latex-export.org`      | CV export template          |
| `latex/invoice-latex-export.org` | Invoice export template     |
| `latex/ticket-mr-export.org`     | Madison Reed ticket export  |
| `latex/latex-macros.tex`         | Reusable LaTeX macros       |

### 4.6 Infrastructure

| File                             | Purpose                     |
|----------------------------------|-----------------------------|
| `agenda/agenda.org`              | Daily/weekly tasks & events |
| `bookmarks/bookmarks`           | URL bookmarks               |
| `roam-nodes/.virtual_brain`     | Archived project links      |

### 4.7 Symlinks (Claude Code ← GPTel Skills)

| Source (`~/.config/doom/gptel-directives/skills/`) | Target (`~/.claude/skills/`) |
|----------------------------------------------------|------------------------------|
| `code-review/`                                     | `code-review`                |
| `emacs-expert/`                                    | `emacs-expert`               |
| `mr-dotcom-dev/`                                   | `mr-dotcom-dev`              |
| `mr-roam-node/`                                    | `mr-roam-node`               |
| `seo-web-quality/`                                 | `seo-web-quality`            |
| `session-reset/`                                   | `session-reset`              |
| `skill-architect/`                                 | `skill-architect`            |

### 4.8 Session Files (GPTel Directives)

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

*   Ran full orphan audit: found 13 orphans, deleted 4 stale files, linked remaining 9 to proper indices.
*   Moved 3 index files from subdirectories to `roam-nodes/` root (Baby, Coding Knowledge, CCS).
*   Expanded Documentation index: added PERSONAL section (6 CVs) and Leidy to WORK & PROJECTs.
*   Refactored Content Creation index: 3 pillars, removed Datomanía, removed Facebook, added CCS and Synchronous.
*   Created About Synchronous roam node (`content_creation/2026-04-05-about_synchronous.org`) following About CCS structure.
*   Added GROUPs & COLLECTIVEs section to Content Creation index with CCS and Synchronous.

### Pending / Not yet started

*   No pending tasks — all work complete.

### Where to resume

If the user asks to **create a new node**: apply Section 1 naming/tagging rules + Section 1.3 orcidlink author + Section 1.5 formatting rules, use appropriate template (Section 1.2), place in correct subdirectory. Index goes at root.
If the user asks to **edit an index**: follow Section 1.5 conventions (list items, `:: descriptions`, bold/italic markup, general descriptions, content lifecycle).
If the user asks to **work on Content Creation**: load Section 3.9 for full brand/collective context. Check index and child nodes.
If the user asks to **work on a specific domain**: load the relevant Section 3 subsection, check the domain's index node.
If the user asks to **remove Gentleman Staff**: remove the link from master index WORK & PROJECTs (it has the REVIEW label).
If the user asks about **infrastructure**: reference Section 3.2 (skill symlinks).
If the user asks for a **new task**: check Section 2.4 (Pending Work).

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
