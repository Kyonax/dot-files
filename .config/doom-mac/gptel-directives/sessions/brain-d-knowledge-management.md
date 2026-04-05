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
*   **Subdirectory placement:** Every node belongs in a subdirectory under `roam-nodes/` based on its domain (e.g., `knowledge/`, `madison_reed/`, `personal_stuff/`).
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
*   **Author:** Always `Cristian D. Moreno - Kyonax`.
*   **Email:** `kyonax.corp@gmail.com`.
*   **ORCID:** `0009-0006-4459-5538`.

### 1.4 Linking & Graph Integrity

*   **Internal links:** Always use Org-Roam ID links: `[[id:UUID][Display Text]]`.
*   **Cross-domain linking:** Index nodes link to child nodes. Child nodes can back-link to indices.
*   **No orphan nodes:** Every node must be reachable from at least one index.
*   **Prefer linking over duplicating:** If knowledge exists in another node, link to it — don't copy.

### 1.5 Index Formatting & Content Lifecycle

*   **Section structure:** Index headings (`*`) get a short one-line description underneath. Child nodes are plain list items (`-`), never subheadings (`**`).
*   **Inline descriptions:** Every child link gets a `:: description` — concise, self-explanatory of the node's content.
*   **Markup rules:**
    *   `*bold*` for important nouns: companies, teams, technologies, platforms, key concepts.
    *   `/italic/` for enumerated categories or parenthetical notes.
    *   `=verbatim=` for literal values like brand handles (e.g., `=@is.kyonax=`).
    *   `~code~` for inline code references.
    *   Org comments (`#`) only when necessary — no decorative comments.
*   **FILETAGS format:** Compact, no spaces between tags: `:KYO:INDEX:` not `:KYO: :INDEX:`.
*   **Header order:** `#+TITLE:`, `#+SUBTITLE:`, `#+DESCRIPTION:`, `#+FILETAGS:`, `#+AUTHOR:`, `#+EMAIL:`.
*   **Content lifecycle:** Important content earns its own roam node — never dump inline in an index. *Quick Notes* is a temporary scratch area; anything left there more than a few days without promotion gets removed.
*   **WORK & PROJECTs scope:** Professional work, side projects, personal projects — anything that generates income.
*   **Sensitive data:** Passwords and passphrases belong in `kyo-utils` repo, not in `.brain.d`. Bank accounts go under a general *FINANCE INFO* section, never bank-specific sections.

### 1.6 Infrastructure & Symlinks

*   **External disk dependency:** `~/.config/doom/` symlinks (`config.org`, `gptel-directives`, `snippets`, `templates`) originate from `/run/media/kyonax/Da_ Disk/dev/github-kyonax/dot-files/.config/doom-mac/`.
*   **Skills symlinks:** GPTel skills in `~/.config/doom/gptel-directives/skills/` are symlinked into `~/.claude/skills/` so both Doom Emacs GPTel and Claude Code share the same skill definitions.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

`~/.brain.d/` is Kyonax's **Org-Roam personal knowledge management system** — a second brain built on Doom Emacs that consolidates professional work tracking, technical learning, personal documents, content creation strategy, and community management into a single, interconnected graph of 118+ org-mode files. Git remote: `https://github.com/kyonax/roam-brain-emacs`.

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

### 2.3 Key Decisions (Session-Wide)

1.  **(2026-04-05)** Session file named `brain-d-knowledge-management.md` — covers the full `.brain.d` repo, not a single ticket or feature. Cross-cutting session.
2.  **(2026-04-05)** Skills shared between GPTel and Claude Code via symlinks — single source of truth for skill definitions across both AI tools.
3.  **(2026-04-05)** Section 3 organized by domain (not by individual node) since this is a PKM session, not a code session. Each domain is a logical unit of work.
4.  **(2026-04-05)** Master index refactored to library-style: index is a catalog of domain indices, not a content dump. Removed 12 stale/inline sections. Linked Madison Reed, Coding Knowledge, Content Creation, and Diary as domain indices. Gentleman Staff and Arch Linux intentionally excluded.
5.  **(2026-04-05)** Passwords/passphrases moved to `kyo-utils` repo. Bank accounts under general FINANCE INFO section.
6.  **(2026-04-05)** Index formatting convention established: plain list items (`-`) with `:: description`, `*bold*` for key nouns, `/italic/` for categories, compact FILETAGS.

### 2.4 Pending Work

- [ ] No pending tasks — session initialized for future `.brain.d` work

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 Repository Exploration & Context Loading

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   Full directory structure analysis: 7 domain subdirectories under `roam-nodes/`, plus `latex/`, `agenda/`, `templates/`, `bookmarks/`, `media/`.
*   118+ org files identified across all domains.
*   Doom Emacs integration mapped: org-roam config (lines 371-452 in `config.el`), GPTel config (lines 675-806), capture templates, keybindings.
*   Key entry points identified: master index (`2023-04-19-index_kyonax.org`), domain-specific indices for all 6 active domains.

### 3.2 GPTel Skills → Claude Code Symlinks (Infrastructure)

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Problem:** GPTel skills defined in `~/.config/doom/gptel-directives/skills/` were not available to Claude Code.
*   **Fix:** Created symlinks in `~/.claude/skills/` pointing to each skill directory:
    *   `code-review`, `emacs-expert`, `mr-dotcom-dev`, `mr-roam-node`, `seo-web-quality`, `session-reset`, `skill-architect`
*   **Result:** 8 total skills in `~/.claude/skills/` (7 GPTel + 1 pre-existing `omarchy`).

### 3.3 Kyonax Master Index Refactor (Library-Style)

**Created:** 2026-04-05 | **Last updated:** 2026-04-05
**Status:** DONE

*   **Problem:** Master index (`2023-04-19-index_kyonax.org`) was a 1065-line scratch pad with GPTel conversations, code exercises, financial calculators, and personal notes dumped inline. No domain indices were linked.
*   **Fix:** Refactored to a clean library catalog (~55 lines):
    *   **Kept:** HOME & PERSONAL (3 existing child nodes), FINANCE INFO (bank accounts table), QUICK NOTEs (empty scratch area).
    *   **Added domain links:** Madison Reed, Coding Knowledge, Content Creation, Diary — each with `:: description`.
    *   **Removed 12 sections:** PASSPHRASEs (→ `kyo-utils`), KEYBINDINGs, EMACS BUILD, WORK, MMORPG, SKANDIA, BANCOLOMBIA (→ FINANCE INFO), NOTEs (tax), PENSAMIENTOs, DEUDAs, PAGAR/DEDUCCIONES/DECLARACION/ASEO, RUT+CEDULA.
    *   **Excluded from index:** Gentleman Staff, Arch Linux (user choice).
*   **Formatting rules established:** Section 1.5 now documents index conventions (list items, `::` descriptions, markup, content lifecycle).

### 3.4 Domain: Madison Reed (32 nodes — ACTIVE)

**Created:** 2025-11-18 | **Last updated:** 2026-04-05
**Status:** ACTIVE

**Role:** Senior Frontend Engineer at Agile Engine, Dotcom Team.
**Tech Stack:** Vue 3 (Options API), Pug, Stylus, Vuex, Vitest.
**Index:** `roam-nodes/2025-11-18-index_madison_reed.org`
**Dedicated session file:** `sessions/site-revolution-redesign.md` (detailed Vue coding session with full component trees, ~117KB).

**Active work:**
*   Site Revolution Redesign — long-lived feature branch with HCB Booking Flow V2, Location Detail V2, Locations List
*   Navigation Redesign
*   ADA accessibility compliance
*   Multiple JIRA tickets (DOTCOMPB-6943 through DOTCOMPB-7652)

### 3.5 Domain: Knowledge Base (11 nodes — REFERENCE)

**Created:** 2024-12-18 | **Last updated:** 2026-04-05
**Status:** REFERENCE

**Index:** `roam-nodes/2024-12-18-index_coding_knowledge.org`
**Topics:** JavaScript (ES6+, async, closures, event loop), ReactJS (hooks, performance), Next.js (SSR/SSG/ISR), Node.js, Ruby, Vue.js, AWS (S3, EC2, IAM, CI/CD), Design Patterns (Creational/Structural/Behavioral with React), Jest testing.
**Purpose:** Senior engineer interview preparation and continuous learning reference.

### 3.6 Domain: Content Creation (2 nodes — ACTIVE)

**Created:** 2024-09-16 | **Last updated:** 2026-04-05
**Status:** ACTIVE

**Index:** `roam-nodes/2024-09-16-index_content_creation.org`
**Brands:** `@is.kyonax` (main personal), `@kyonax_on` (gaming/lore), `@kyonax_on_tech` (tech/programming), `Discord Creed` (Discord content), `Datomanía` (data/facts).
**Strategy:** Simultaneous shorts-based growth across YouTube, TikTok, Twitter, Instagram, Twitch, Facebook with cross-promotion.

### 3.7 Domain: Cyber Code Syndicate (4 nodes — IN DEVELOPMENT)

**Created:** 2025-11-01 | **Last updated:** 2026-04-05
**Status:** IN DEVELOPMENT

**Socials:** `@ccs_devhub` (X, GitHub, Instagram).
**Focus:** Free Software advocacy, open-source collaboration.
**Code style:** `snake_case` variables, `kebab-case` files, `UPPER_SNAKE_CASE` constants.
**Pending:** Manifesto creation, community launch.

### 3.8 Domain: Gentleman Staff (1 node — ACTIVE)

**Created:** 2025-12-23 | **Last updated:** 2026-04-05
**Status:** ACTIVE

**Index:** `roam-nodes/2025-12-23-index_gentleman_staff.org`
**Role:** Discord server moderator for `@gentlemanprogramming` YouTube community.

### 3.9 Domain: Personal (12 nodes — LIVING)

**Created:** ~2023-04-19 | **Last updated:** 2026-04-05
**Status:** LIVING

**Contents:** CVs (English, Spanish, JS-focused), family info (partner: Leidy Johana Guerrero), baby/family planning, career planning, birthday reminders, hiring interview notes.

### 3.10 Domain: Omarchy / Arch Linux (ACTIVE — Separate Session)

**Created:** 2026-03-20 | **Last updated:** 2026-04-05
**Status:** ACTIVE (tracked in `sessions/omarchy-installation.md`)

**Summary:** Data recovery from broken Arch Linux system + Omarchy v3.4.2 installation. Phase 1 (recovery) complete, Phase 2a (ISO flash) in progress.
**Index:** `roam-nodes/2026-03-20-arch_linux.org`

---

## SECTION 4: FILE INDEX

### 4.1 Index Nodes

| File                                                    | Domain              |
|---------------------------------------------------------|---------------------|
| `roam-nodes/2023-04-19-index_kyonax.org`                | Master index        |
| `roam-nodes/2025-11-18-index_madison_reed.org`          | Madison Reed        |
| `roam-nodes/2024-12-18-index_coding_knowledge.org`      | Knowledge base      |
| `roam-nodes/2024-09-16-index_content_creation.org`      | Content creation    |
| `roam-nodes/2024-09-16-index_documentation.org`         | Documentation       |
| `roam-nodes/2025-12-23-index_gentleman_staff.org`       | Gentleman Staff     |
| `roam-nodes/2026-02-15-index_diary.org`                 | Daily agenda/diary  |
| `roam-nodes/2026-03-20-arch_linux.org`                  | Arch/Omarchy        |

### 4.2 Templates

| File                                         | Key | Purpose                  |
|----------------------------------------------|-----|--------------------------|
| `templates/new-node-doc.org`                 | `c` | Documentation node       |
| `templates/new-node-project.org`             | `p` | JIRA ticket/project node |
| `templates/new-node-invoice.org`             | `v` | Invoice                  |
| `templates/new-node-sentinel-inspection.org` | `i` | Inspection report        |

### 4.3 LaTeX & Exports

| File                             | Purpose                     |
|----------------------------------|-----------------------------|
| `latex/setup_latex.org`          | Central LaTeX configuration |
| `latex/cv-latex-export.org`      | CV export template          |
| `latex/invoice-latex-export.org` | Invoice export template     |
| `latex/ticket-mr-export.org`     | Madison Reed ticket export  |
| `latex/latex-macros.tex`         | Reusable LaTeX macros       |

### 4.4 Infrastructure

| File                             | Purpose                     |
|----------------------------------|-----------------------------|
| `agenda/agenda.org`              | Daily/weekly tasks & events |
| `bookmarks/bookmarks`           | URL bookmarks               |
| `roam-nodes/.virtual_brain`     | Archived project links      |

### 4.5 Symlinks (Claude Code ← GPTel Skills)

| Source (`~/.config/doom/gptel-directives/skills/`) | Target (`~/.claude/skills/`) |
|----------------------------------------------------|------------------------------|
| `code-review/`                                     | `code-review`                |
| `emacs-expert/`                                    | `emacs-expert`               |
| `mr-dotcom-dev/`                                   | `mr-dotcom-dev`              |
| `mr-roam-node/`                                    | `mr-roam-node`               |
| `seo-web-quality/`                                 | `seo-web-quality`            |
| `session-reset/`                                   | `session-reset`              |
| `skill-architect/`                                 | `skill-architect`            |

### 4.6 Session Files (GPTel Directives)

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

*   Refactored master index (`2023-04-19-index_kyonax.org`) from 1065-line scratch pad to ~55-line library catalog.
*   Removed 12 stale/inline sections (PASSPHRASEs, KEYBINDINGs, EMACS BUILD, SKANDIA, DEUDAs, etc.).
*   Linked 4 domain indices: Madison Reed, Coding Knowledge, Content Creation, Diary.
*   Added FINANCE INFO section with bank accounts table (replacing Bancolombia-specific section).
*   Established index formatting conventions and documented them in Section 1.5.
*   Removed external disk auto-mount details from session file (kept Da\_ Disk symlink origin reference).

### Pending / Not yet started

*   No pending tasks — session initialized for future `.brain.d` work

### Where to resume

If the user asks to **create a new node**: apply Section 1 naming/tagging rules + Section 1.5 formatting rules, use appropriate template (Section 1.2), place in correct subdirectory.
If the user asks to **edit an index**: follow Section 1.5 conventions (list items, `:: descriptions`, bold/italic markup, content lifecycle).
If the user asks to **work on a specific domain**: load the relevant Section 3 subsection for context, check the domain's index node.
If the user asks about **infrastructure**: reference Section 3.2 (skill symlinks) for prior decisions.
If the user asks for a **new task**: check Section 2.4 (Pending Work).

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
