<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for all skill creation, refinement, and architecture work. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read the sections in order on first load — after that, reference them by number as needed. The data is organized into 5 sections:

| Section                      | Purpose                                                                                                     | When to reference                                                                       |
|------------------------------|-------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| **1. Global Guidelines**     | Skill architecture rules, conventions, and constraints that apply to ALL skill work.                        | Before creating, refining, or reviewing any skill. These are mandatory constraints.     |
| **2. Session Overview**      | High-level context: what the skill system is, which skills exist, session-wide decisions, and pending work. | When starting a new skill task — understand scope and inventory first.                  |
| **3. Skill Implementations** | Per-skill detail: what was built, file structure, creation history, key decisions, and current state.       | When resuming work on a specific skill, or when a new skill relates to an existing one. |
| **4. File Index**            | Quick-reference table of all file paths (skills, rules, configs, sessions, roam nodes).                     | When you need to read, edit, or reference a specific file without searching.            |
| **5. Last Interaction**      | Short-term memory: what was done last, what's pending, where to resume.                                     | At the very start of a new conversation — this is your entry point for continuing work. |

**Operational Rule:** Throughout the conversation, the user will give new tasks. **Always look for the last request, identified by a markdown title starting with `###`.** Any `###` title means it is the newest request.

**Key principle:** Data may appear in multiple sections with different framing. Section 1 frames it as a rule to follow. Section 2 frames it as context to understand. Section 3 frames it as an implementation to reference. This is intentional — each section answers a different question about the same knowledge.

**Compaction sources (by modification date):**
1. `refine-agentic-brain_v1.md` (Feb 10) — GPTel agentic brain + madisonreed-dev skill creation
2. `madisonreed-dev-new_skill_dynamic_yield.md` (Feb 19) — Dynamic Yield rule addition
3. `seo-tool_madison-reed_17_02_2026.md` (Feb 20) — SEO analyzer CLI tool + seo-web-quality skill
4. `claude-code-skill-refinement/SESSION.md` (Mar 9) — Agent Skills Spec, multi-agent architecture
5. `create-refine-skills.md` (Mar 11) — Two-Agent Model codification, master blueprint
6. `roam-node-skill-creation.md` (Mar 11) — mr-roam-node skill creation & index management
7. **skill-architecture-master.md** (Mar 12) — skill-architect creation, containerization, domain-agnostic broadening, self-audit
8. **skill-architecture-master.md** (Mar 12) — session-reset v3.1.0 refinement, compression protocol, impact assessment
9. **skill-architecture-master.md** (Mar 12) — mr-roam-node v3.1.0 refinement: monolithic split into Three-File architecture + writing-standards rule
10. **skill-architecture-master.md** (Mar 12) — mr-dotcom-dev v3.0.0 refinement: session-driven audit, 3 major rewrites, 3 patches, boundary reorganization, health check

**Compression applied:** 2026-03-12 — Level 1. Graduated 13 guidelines (1.1–1.13) to `skill-architect` and `session-reset` skills. Reduced Section 1 from ~108 lines to ~18 lines. Overall reduced from 614 to ~530 lines.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every skill creation or refinement task.** The loaded skills for this domain — `skill-architect`, `session-reset`, `mr-roam-node`, `mr-dotcom-dev`, `code-review` — now cover the full skill engineering methodology. All 13 guidelines that previously lived here have been promoted into skill rule files. This section retains only session-specific operational context not captured in those skills.

> **Graduated rules (removed from this block):**
> - 1.1 Two-Agent Model → `skill-architect/rules/skill-structure.md`
> - 1.2 Agent Skills Specification → `skill-architect/rules/skill-structure.md` + `multi-agent-consumption.md`
> - 1.3 Three-File Structure → `skill-architect/rules/skill-structure.md`
> - 1.4 Skill Directory Structure → `skill-architect/rules/skill-structure.md`
> - 1.5 SKILL.md Description Writing → `skill-architect/rules/skillmd-routing.md`
> - 1.6 Rule File Writing → `skill-architect/rules/rule-writing.md`
> - 1.7 Agent-Agnostic → `skill-architect/rules/rule-writing.md` + `skill-structure.md`
> - 1.8 SOP Phases → `skill-architect/rules/creation-sop.md`
> - 1.9 Skills vs Rules vs CLAUDE.md → `skill-architect/rules/multi-agent-consumption.md`
> - 1.10 Source Material Transformation → `skill-architect/rules/creation-sop.md`
> - 1.11 Containerization → `skill-architect/rules/skill-structure.md` + `rule-writing.md`
> - 1.12 Domain-Agnostic Scope → broadened across all `skill-architect` rule files
> - 1.13 Self-Audit Practice → validated practice, documented in session history (Section 3.3, 3.8)

### 1.1 Session-Specific Operational Notes

*   **Skill source location:** All skills live in `gptel-directives/skills/` within the dot-files repo, symlinked to `~/.claude/skills/` for Claude Code access. Single source, dual consumption.
*   **Context budget:** ~2% of context window (fallback: 16,000 chars). Override: `SLASH_COMMAND_TOOL_CHAR_BUDGET=25000`. Check with `/context`.
*   **`rules/` over `references/`** — We use `rules/` with mandatory YAML frontmatter instead of spec's `references/`. Enables GPTel analyzer to build summaries for semantic routing. Both work identically in Claude Code.

---

## SECTION 2: SESSION OVERVIEW

> This section provides the overall context, purpose, and inventory of the skill system.

### 2.1 Purpose

This session tracks the design, creation, and refinement of AI skills — structured knowledge bases that give AI agents deep, domain-specific context while minimizing token waste. The system serves two AI environments: **Claude Code** (via `~/.claude/skills/` symlinks) and **GPTel** (via `gptel-directives/skills/` directory), using the same skill files for both.

### 2.2 Skills Inventory

| Skill                               | Version | Rules | Status | Notes                                                                                  |
|-------------------------------------|---------|-------|--------|----------------------------------------------------------------------------------------|
| `mr-dotcom-dev`                     | v3.0.0  | 10    | Active | MR frontend standards. Session-driven refinement complete. Health check: 12P/2W/0F    |
| `seo-web-quality`                   | v1.0.0  | 6     | Active | GOLD STANDARD for rule structure. Sourced from external project                        |
| `skill-architect`                   | v1.1.0  | 6     | Active | META-SKILL for skill engineering. Self-audited. Domain-agnostic. Containerized         |
| `session-reset`                     | v3.1.0  | 5     | Active | Context compaction, 5-section architecture, compression protocol, impact assess        |
| `mr-roam-node`                      | v3.1.0  | 6     | Active | Three-File architecture. Split from 1022-line monolithic. Writing standards rule added |
| `emacs-expert`                      | v1.0.0  | 2     | Active | Testing + naming conventions for Emacs config                                          |
| `code-review`                       | v1.0.0  | 0     | Active | Three-stage code review pipeline                                                       |

### 2.3 Key Architectural Decisions (Session-Wide)

1. **(2026-02-10)** **Two-Agent Model over monolithic context** — Separating Analyzer (routing) from Worker (execution) enables selective loading and token efficiency. All skills must serve both audiences.
2. **(2026-02-10)** **`rules/` over `references/`** — YAML frontmatter requirement enables GPTel analyzer to build summaries. Stricter structure = better routing accuracy.
3. **(2026-02-10)** **Skill symlinks for Claude Code** — Skills live in dot-files repo, symlinked to `~/.claude/skills/`. Single source, dual consumption.
4. **(2026-02-10)** **"Explicit over Implicit" as non-negotiable** — Established after the madisonreed-dev incident. Never summarize patterns when the Worker needs the full reference.
5. **(2026-03-09)** **Agent-agnostic content** — Skills must work in any AI that reads markdown. No tool-specific references in content.
6. **(2026-03-12)** **Containerization as formal principle** — All skills must be fully self-contained. No external references. Formalized in `skill-architect`.
7. **(2026-03-12)** **Domain-agnostic scope** — Skills are for any domain (code, writing, research, legal, business), not just programming.
8. **(2026-03-12)** **Self-audit after creation** — Every completed skill must be health-checked against its own guidelines before shipping.
9. **(2026-03-12)** **963-line context block limit** — Session files have a hard compression threshold. Enforced by `session-reset/rules/compression-protocol.md`.
10. **(2026-03-12)** **Impact assessment before compression** — Two-axis priority (age × impact). CRITICAL entries are shielded from all compression levels.
11. **(2026-03-12)** **Containerization exemption for inherently user-specific skills** — Skills like `mr-roam-node` that generate files for a specific user's personal knowledge base may contain hardcoded user paths, JIRA cloud IDs, and author identity. These are justified domain data, not portability violations.
12. **(2026-03-12)** **Session-driven skill refinement** — Skills like `mr-dotcom-dev` are built from the user's personal coding patterns (extracted from session files), NOT by abstracting the entire codebase. Continuous improvement via sessions. Utility classes/variables are the exception — verify those against codebase for accuracy.
13. **(2026-03-12)** **`madisonreed-dev` fully deprecated** — Canonical name is `mr-dotcom-dev`. The `madisonreed-dev` directory no longer exists. Symlink points to `mr-dotcom-dev`.

### 2.4 Pending Work

**mr-roam-node (testing):**
- [ ] End-to-end test: generate node from JIRA ticket using v3.1.0 rules
- [ ] Bug template test: generate bug node from real bug ticket
- [ ] Story Points custom field: `customfield_10016` returns null — investigate
- [ ] Bulk index sync: fetch all user-assigned tickets and sync full Sprint Board
- [ ] Index Update Flow end-to-end test

**SEO Analyzer CLI:**
- [ ] Refine `AGENTS.md` scoring rubric for more accurate scores
- [ ] Add contextual code block references to all report sections

**Existing skills audits:**
- [ ] Containerization audit on `seo-web-quality`
- [ ] Domain-agnostic audit on `seo-web-quality`, `emacs-expert`

---

## SECTION 3: SKILL IMPLEMENTATIONS

> Each subsection documents a specific skill's implementation: what was built, file structure, creation history, key decisions, and current state.

---

### 3.1 `mr-dotcom-dev`: MR Frontend Development Standards

**Created:** 2026-02-10 | **Last updated:** 2026-03-12
**Status:** Active, v3.0.0 — 10 rules, health check 12 PASS / 2 WARN / 0 FAIL

#### File Structure

```
mr-dotcom-dev/
├── SKILL.md                    # Router (57 lines), signal keywords, disambiguation, task recipes
├── AGENTS.md                   # Architectural guide — utility-first philosophy, component ecosystem, dependencies
└── rules/
    ├── spacing-utilities.md    # CRITICAL — Margin/padding em/pct, responsive breakpoints, xs- prefix
    ├── typography-utilities.md # CRITICAL — Fonts, ALL color systems (brand/text/cta/ui/feedback/cw/mister), Stylus variables, breakpoints, mixins
    ├── flexbox-layout.md       # HIGH — Flex containers, alignment, sizing, gap, wrap, aspect ratio boxes
    ├── utility-classes.md      # CRITICAL — Display, visibility, positioning, centering, sizing, overflow, borders, dividers, interactivity, a11y
    ├── vue-patterns.md         # CRITICAL — Options API, Vuex, modals, services, global/isDesktop, @ready, design patterns, a11y
    ├── pug-templates.md        # HIGH — Vue template Pug + page layout inheritance
    ├── testing-standards.md    # HIGH — Vitest/VTU, shallowMount, mocking, createMockStore, emit-before-redirect
    ├── ssr-architecture.md     # HIGH — SSR pipeline, Vite, entry-server/client, hydration
    ├── express-routing.md      # HIGH — Three-file pipeline, endpoints, views, module resolution
    └── dynamic-yield.md        # CRITICAL — DY campaigns, templates, Experience API, events
```

#### Creation History

1. **(2026-02-10)** Created as `madisonreed-dev` with 4 CSS utility rules (v1.0.0)
2. **(2026-02-19)** Added Dynamic Yield rule (v2.3.0)
3. **(2026-03-12)** Gap rules filled (vue-patterns, pug-templates, ssr-architecture, express-routing, testing-standards). Two proposed gaps (`mr-modules`, `git-pr-workflow`) discarded — not needed.
4. **(2026-03-12)** Session-driven refinement using `site-revolution-redesign.md` as pattern source:
   - **Typography:** Major rewrite — added all design system numbered color systems, Stylus variables, breakpoint variables/mixins, missing font sizes
   - **Flexbox:** Rewritten — scoped to flex + gap + aspect ratio. Display/visibility/sizing moved to utility-classes
   - **Utility-classes:** Rewritten — now owns display, visibility, positioning/centering, sizing, overflow, borders/dividers, interactivity, a11y
   - **Spacing:** Minor fix — added `xs-` prefix for em classes, noted px-based as legacy
   - **Vue-patterns:** 3 patches — centralized `global/isDesktop` getter, conditional `aria-labelledby` with `v-bind`, `@ready` event pattern
   - **Testing-standards:** 2 patches — `createMockStore(state, isDesktop)` pattern, emit-before-redirect testing
   - **SKILL.md + AGENTS.md:** Updated signal keywords, rule boundaries, version
5. **(2026-03-12)** Renamed `madisonreed-dev` → `mr-dotcom-dev`. Old name fully deprecated.
6. **(2026-03-12)** Health check: 12 PASS, 2 WARN (vue-patterns 543 lines, project-specific paths), 0 FAIL. Version bumped to v3.0.0.

#### Key Decisions

| Decision | Date | Rationale |
|---|---|---|
| Split utility classes into 4 atomic files | 2026-02-10 | Each utility domain is independently loadable |
| Exhaustive class tables over pattern formulas | 2026-02-10 | "Explicit over Implicit" — Worker cannot derive all classes from a formula |
| DY as separate CRITICAL rule | 2026-02-19 | Client-side injection not visible in codebase — AI needs explicit knowledge |
| Session-driven refinement over codebase abstraction | 2026-03-12 | Codebase has many patterns from many devs — skill reflects only Kyo's patterns. Utility classes/variables verified against codebase for accuracy |
| Discard `mr-modules` and `git-pr-workflow` rules | 2026-03-12 | `mr-modules` is backend (Kyo is frontend). `git-pr-workflow` already covered by CLAUDE.md + `/create-pr` skill + session files |
| Reorganize styling rule boundaries | 2026-03-12 | Flexbox scoped to flex+gap+aspect. Utility-classes absorbs display/visibility/positioning/sizing. Clean zero-overlap |
| Typography includes Stylus variables | 2026-03-12 | Same design system tokens used in both Pug (classes) and `<style>` (variables). Splitting would force loading two files for one color question |

---

### 3.2 `seo-web-quality`: SEO Standards (Gold Standard)

**Created:** 2026-02-20 | **Last updated:** 2026-02-20
**Status:** Active, v1.0.0 — Complete, serves as canonical reference for rule structure

#### File Structure

```
seo-web-quality/
├── SKILL.md
├── AGENTS.md
└── rules/
    ├── technical-seo.md        # HIGH
    ├── on-page-seo.md          # HIGH
    ├── structured-data.md      # MEDIUM
    ├── mobile-seo.md           # HIGH
    ├── international-seo.md    # MEDIUM
    └── audit-checklist.md      # MEDIUM
```

#### Key Decisions

| Decision                         | Date       | Rationale                                                                                                     |
|----------------------------------|------------|---------------------------------------------------------------------------------------------------------------|
| Disregard source format entirely | 2026-02-20 | Source material transformation principle — abstract and restructure, never replicate                          |
| 6 atomic rules for one domain    | 2026-02-20 | Each SEO sub-domain is independently loadable; avoids loading mobile-seo when user asks about structured data |

---

### 3.3 `skill-architect`: Meta-Skill for Skill Engineering

**Created:** 2026-03-12 | **Last updated:** 2026-03-12
**Status:** Active, v1.1.0 — Complete, self-audited, domain-agnostic, containerized

#### File Structure

```
skill-architect/
├── SKILL.md                          # Router (54 lines), author: @kyonax_on_tech
├── AGENTS.md                         # Architectural guide — Two-Agent philosophy, 6 Pillars
└── rules/
    ├── skill-structure.md            # CRITICAL — Three-File architecture, directory layout, containerization, progressive disclosure
    ├── skillmd-routing.md            # CRITICAL — SKILL.md writing, descriptions, routing tables, templates
    ├── rule-writing.md               # CRITICAL — Rule file writing, splitting, frontmatter, containerization
    ├── creation-sop.md               # HIGH — 5-phase creation process (ground truth → test)
    ├── refinement-patterns.md        # HIGH — Splitting, adding, promoting, restructuring, versioning
    └── multi-agent-consumption.md    # MEDIUM — Claude Code/GPTel/Cursor loading, symlinks, decision tree
```

#### Key Decisions

| Decision                                    | Date       | Rationale                                                                                                  |
|---------------------------------------------|------------|------------------------------------------------------------------------------------------------------------|
| 6 atomic rules (not fewer)                  | 2026-03-12 | Each concern is independently needed — writing a SKILL.md doesn't need creation SOP, and vice versa        |
| Containerization as formal guideline        | 2026-03-12 | Skills must be portable — external references become broken pointers on other systems                      |
| Domain-agnostic examples throughout         | 2026-03-12 | Skills are for any domain, not just code — multi-domain examples prevent code-centric bias                 |
| Self-audit after completion                 | 2026-03-12 | The skill defines its own quality standards — it must pass them. Caught 3 issue categories.                |

---

### 3.4 `mr-roam-node`: Org-Roam Node Generator for JIRA Tickets

**Created:** 2026-03-09 | **Last updated:** 2026-03-12
**Status:** Active, v3.1.0 — Three-File architecture, 6 rules, self-audited (27 PASS, 2 WARN, 0 FAIL)

#### File Structure

```
mr-roam-node/
├── SKILL.md                       # Router (50 lines), 11 routing rows, 6 Quick Reference entries
├── AGENTS.md                      # Philosophy — JIRA as source, org-roam as system of record, two-phase lifecycle
└── rules/
    ├── templates.md               # CRITICAL — Base/standard/bug templates, metadata headers, section formatting, file naming, UUID
    ├── jira-parsing.md            # CRITICAL — 6 format variants, AC parsing, content translation, field mapping, event tracking
    ├── org-mode-reference.md      # HIGH — Emphasis PRE/POST, property drawers, lists, checkboxes, tables, code blocks, pitfalls
    ├── index-management.md        # CRITICAL — Two-layer index (BACKLOG + Sprint Board), 8-step update flow, nesting, edge cases
    ├── node-lifecycle.md          # HIGH — 7-step execution flow, two-phase lifecycle, validation mode, update mode, quality checklist
    └── writing-standards.md       # HIGH — Tone/voice (clarity, accuracy, consistency, specificity), per-section writing rules, propose-before-fill workflow
```

#### Creation History

1. **(2026-03-09)** Initial skill created as monolithic SKILL.md (v1.0.0)
2. **(2026-03-11)** Refined to v2.3.0 — 12 sections covering all node operations, 1022 lines
3. **(2026-03-12)** Health check audit against `skill-architect` guidelines: 14 FAIL, 3 WARN, 2 PASS. Major issues: monolithic file (1022 lines, limit 500), no rules/, no routing table, no Quick Reference, no AGENTS.md.
4. **(2026-03-12)** Split into Three-File architecture: SKILL.md (router) + AGENTS.md + 5 atomic rule files. All technical details (JIRA format variants, org-mode syntax, index management, templates) graduated from SKILL.md into rule files.
5. **(2026-03-12)** Added `writing-standards.md` — new rule covering tone, voice, clarity, per-section writing rules, terminology consistency, and propose-before-fill workflow for Phase 2 sections.
6. **(2026-03-12)** Self-audit: 27 PASS, 2 WARN (containerization — justified for user-specific skill), 0 FAIL. Version bumped to v3.1.0.

#### Key Decisions

| Decision                                             | Date       | Rationale                                                                                                      |
|------------------------------------------------------|------------|----------------------------------------------------------------------------------------------------------------|
| 6 rules (not 4 as originally recommended)            | 2026-03-12 | Added `node-lifecycle.md` (execution + validation as one concern) and `writing-standards.md` (content quality)  |
| Containerization warnings accepted as justified      | 2026-03-12 | Skill is inherently user-specific (personal knowledge base + specific JIRA instance) — hardcoded paths are domain data |
| Writing standards as a separate rule                 | 2026-03-12 | Tone/voice/clarity is independently needed from templates or parsing — someone filling DEVELOPMENT AC doesn't need JIRA parsing rules |
| Propose-before-fill workflow for Phase 2 sections    | 2026-03-12 | Developer may have context not visible in code — proposing structure first catches misalignment before writing documentation |

#### Key Technical Details (Reference)

*   **Atlassian MCP CloudId:** `983806ac-6a2a-439b-a0b9-43a41f78cb46`
*   **JIRA tools:** `mcp__claude_ai_Atlassian__getJiraIssue`, `mcp__claude_ai_Atlassian__searchJiraIssuesUsingJql`
*   **Index file:** `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` (ID: `7c2b1bc9-4a2e-4a64-b2e8-b36e2ba95106`)
*   **Node files:** `~/.brain.d/roam-nodes/madison_reed/` (20 files, 6 JIRA-compared)

---

### 3.5 SEO Analyzer CLI Tool

**Created:** 2026-02-17 | **Last updated:** 2026-02-20
**Status:** Active — functional CLI tool, refinements pending

*Note: This is a standalone Node.js CLI tool, not a skill — but it uses the `seo-web-quality` skill's `AGENTS.md` as its AI brain.*

#### Architecture — The Hybrid Model

```
seo-analyzer.js          ai-analyzer.js          AGENTS.md
(Reconnaissance Agent)   (Communications Officer) (AI Brain)
        │                        │                     │
  Gathers factSheet ──────► Packages & sends ────► Scores & analyzes
  (100% programmatic)        to AI, parses JSON     (expert rulebook)
        │                        │                     │
  Writes .org report ◄───── Returns structured ◄── Returns JSON with
  with all sections           analysis object        scores + justifications
```

#### Key Principles

*   **Hybrid Model:** Script gathers facts; AI provides insight. Never mix responsibilities.
*   **Programmatic truth is non-negotiable** — all factSheet data must be 100% accurate
*   **`AGENTS.md` is the single source of analytical truth** — change scoring there, not in code

---

### 3.6 GPTel Agentic Pipeline (Emacs Integration)

**Created:** 2026-02-10 | **Last updated:** 2026-03-09
**Status:** Active — two-stage pipeline functional

#### Pipeline Flow

```
User prompt → SPC o l t
    │
    ▼
Stage 1: SKILL ANALYSIS (gemini-backup-flash)
    Input: skill-analyzer.md template + {{SKILL_SUMMARIES}} + {{PROMPT_TEXT}}
    Output: "// SKILLS: skill/rule, skill/rule"
    │
    ▼
Stage 2: WORKER EXECUTION (user's chosen model)
    Merge: static skills (from directive) + dynamic skills (from analyzer)
    Load: SKILL.md + AGENTS.md per skill (cached), then specified rules
    Inject: into directive between <!--SKILLS START--> and <!--SKILLS END-->
    Send: full payload to worker model
```

#### Skill Loading Formats

*   `skill-name` → SKILL.md + AGENTS.md (core only)
*   `skill-name/*` → core + ALL rules
*   `skill-name/rule-name` → core + one specific rule
*   **20% Rule:** Include rule if even partial semantic connection exists

---

### 3.7 Other Skills (Brief)

**`emacs-expert`** (v1.0.0) — 2 rules: `testing-rule` (CRITICAL), `naming-conventions` (HIGH). First skill created, served as template for all others.

**`code-review`** (v1.0.0) — Three-stage code review pipeline. No rules/ directory.

---

### 3.8 `session-reset`: Session Context Compaction

**Created:** ~2026-02-20 | **Last updated:** 2026-03-12
**Status:** Active, v3.1.0 — Complete, 5 rules, self-audited, containerized

#### File Structure

```
session-reset/
├── SKILL.md                              # Router (54 lines)
├── AGENTS.md                             # Philosophy — compaction as engineering, compression philosophy, 5 Pillars (63 lines)
└── rules/
    ├── context-block-architecture.md     # CRITICAL — 5-section structure, template, delimiters, file naming
    ├── execution-flow.md                 # HIGH — 5-step reset process, standalone file mode (e.g., GPTel)
    ├── section-writing-rules.md          # CRITICAL — Per-section writing rules, component docs, graduation
    ├── compression-protocol.md           # CRITICAL — 963-line limit, impact assessment, 5-level hierarchy
    └── quality-checklist.md              # MEDIUM — Verification checklist before every write
```

#### Key Decisions

| Decision                                         | Date       | Rationale                                                                                         |
|--------------------------------------------------|------------|---------------------------------------------------------------------------------------------------|
| 5 atomic rules (not fewer)                       | 2026-03-12 | Each concern is independently needed — compression protocol doesn't need section writing rules    |
| 963-line hard limit                              | 2026-03-12 | Ensures context blocks fit within practical AI context windows with room for conversation         |
| Two-axis compression (age × impact)              | 2026-03-12 | Age alone is dangerous — old-but-critical entries must be preserved                               |
| CRITICAL entries shielded from all levels        | 2026-03-12 | Foundational decisions referenced by active work survive all compression                         |
| Tombstone pattern (never fully delete)           | 2026-03-12 | 2-3 lines preserves thread for "what happened to X?" without full detail in context              |

---

## SECTION 4: FILE INDEX

> Quick reference for all files in the skill system.

### Skills (GPTel Source — Canonical Location)

| File                                                          | Skill           |
|---------------------------------------------------------------|-----------------|
| `.../skills/skill-architect/SKILL.md`                         | skill-architect |
| `.../skills/skill-architect/AGENTS.md`                        | skill-architect |
| `.../skills/skill-architect/rules/skill-structure.md`         | skill-architect |
| `.../skills/skill-architect/rules/skillmd-routing.md`         | skill-architect |
| `.../skills/skill-architect/rules/rule-writing.md`            | skill-architect |
| `.../skills/skill-architect/rules/creation-sop.md`            | skill-architect |
| `.../skills/skill-architect/rules/refinement-patterns.md`     | skill-architect |
| `.../skills/skill-architect/rules/multi-agent-consumption.md` | skill-architect |
| `.../skills/session-reset/SKILL.md`                           | session-reset   |
| `.../skills/session-reset/AGENTS.md`                          | session-reset   |
| `.../skills/session-reset/rules/context-block-architecture.md`| session-reset   |
| `.../skills/session-reset/rules/execution-flow.md`            | session-reset   |
| `.../skills/session-reset/rules/section-writing-rules.md`     | session-reset   |
| `.../skills/session-reset/rules/compression-protocol.md`      | session-reset   |
| `.../skills/session-reset/rules/quality-checklist.md`         | session-reset   |
| `.../skills/mr-dotcom-dev/SKILL.md`                           | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/AGENTS.md`                          | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/spacing-utilities.md`         | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/typography-utilities.md`      | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/flexbox-layout.md`            | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/utility-classes.md`           | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/vue-patterns.md`              | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/pug-templates.md`             | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/testing-standards.md`         | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/ssr-architecture.md`          | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/express-routing.md`           | mr-dotcom-dev |
| `.../skills/mr-dotcom-dev/rules/dynamic-yield.md`             | mr-dotcom-dev |
| `.../skills/seo-web-quality/SKILL.md`                         | seo-web-quality |
| `.../skills/seo-web-quality/AGENTS.md`                        | seo-web-quality |
| `.../skills/seo-web-quality/rules/technical-seo.md`           | seo-web-quality |
| `.../skills/seo-web-quality/rules/on-page-seo.md`             | seo-web-quality |
| `.../skills/seo-web-quality/rules/structured-data.md`         | seo-web-quality |
| `.../skills/seo-web-quality/rules/mobile-seo.md`              | seo-web-quality |
| `.../skills/seo-web-quality/rules/international-seo.md`       | seo-web-quality |
| `.../skills/seo-web-quality/rules/audit-checklist.md`         | seo-web-quality |
| `.../skills/mr-roam-node/SKILL.md`                            | mr-roam-node    |
| `.../skills/mr-roam-node/AGENTS.md`                           | mr-roam-node    |
| `.../skills/mr-roam-node/rules/templates.md`                  | mr-roam-node    |
| `.../skills/mr-roam-node/rules/jira-parsing.md`               | mr-roam-node    |
| `.../skills/mr-roam-node/rules/org-mode-reference.md`         | mr-roam-node    |
| `.../skills/mr-roam-node/rules/index-management.md`           | mr-roam-node    |
| `.../skills/mr-roam-node/rules/node-lifecycle.md`             | mr-roam-node    |
| `.../skills/mr-roam-node/rules/writing-standards.md`          | mr-roam-node    |
| `.../skills/emacs-expert/SKILL.md`                            | emacs-expert    |
| `.../skills/emacs-expert/AGENTS.md`                           | emacs-expert    |
| `.../skills/emacs-expert/rules/testing-rule.md`               | emacs-expert    |
| `.../skills/emacs-expert/rules/naming-conventions.md`         | emacs-expert    |

*(`...` = `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives`)*

### Skills (Claude Code Symlinks)

| Symlink                            | Target                       |
|------------------------------------|------------------------------|
| `~/.claude/skills/mr-dotcom-dev`   | `.../skills/mr-dotcom-dev`   |
| `~/.claude/skills/mr-roam-node`    | `.../skills/mr-roam-node`    |
| `~/.claude/skills/skill-architect` | `.../skills/skill-architect`  |
| `~/.claude/skills/session-reset`   | `.../skills/session-reset`   |

### GPTel Context & Config

| File                                                 | Purpose                     |
|------------------------------------------------------|-----------------------------|
| `.../context/agent/AGENTS.md`                        | Agent identity and behavior |
| `.../context/agent/IDENTITY.md`                      | Agent persona               |
| `.../context/agent/SOUL.md`                          | Agent core directives       |
| `.../context/memory/MEMORY.md`                       | Persistent memory           |
| `.../context/user/USER.md`                           | User profile                |
| `.../context/system/skill-analyzer.md`               | Analyzer prompt template    |
| `/Volumes/.../dot-files/.config/doom-mac/config.org` | GPTel Doom Emacs config     |

### Session & Roam

| File                                                      | Purpose               |
|-----------------------------------------------------------|-----------------------|
| `.../sessions/skill-architecture-master.md`               | **This session file** |
| `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` | Roam index file       |
| `~/.brain.d/templates/new-node-project.org`               | Roam base template    |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.** This section captures the most recent work and immediate next steps.

### What was done last

*   Refined `mr-dotcom-dev` from v2.3.0 → v3.0.0 using `site-revolution-redesign.md` session as the pattern source
*   Audited all 10 rules: 4 styling rules verified against codebase, 3 pattern rules cross-referenced against session, 3 infrastructure rules confirmed accurate
*   3 major rewrites: `typography-utilities.md` (all design system variables + color numbering), `flexbox-layout.md` (scoped to flex+gap+aspect), `utility-classes.md` (absorbs display/visibility/positioning/sizing)
*   3 patches: `vue-patterns.md` (+global/isDesktop, +conditional aria-labelledby, +@ready), `testing-standards.md` (+createMockStore, +emit-before-redirect), `spacing-utilities.md` (+xs- prefix, +legacy note)
*   Discarded 2 proposed gap rules (`mr-modules`, `git-pr-workflow`) — not needed
*   Health check: 12 PASS, 2 WARN (vue-patterns 543 lines, project-specific paths), 0 FAIL
*   Established session-driven refinement approach as formal decision (#12 in Section 2.3)

### Pending / Not yet started

*   **mr-roam-node:** End-to-end testing of v3.1.0 (generate node, generate bug node, index sync, story points investigation)
*   **SEO Analyzer:** Scoring rubric refinement + contextual code blocks in report
*   **Existing skills audits:** Containerization + domain-agnostic audit on `seo-web-quality`, `emacs-expert`

### Where to resume

If the user asks to **test mr-roam-node**: Generate a node from a real JIRA ticket using v3.1.0 rules. Verify all 6 rules produce correct output. Check Section 3.4 for key technical details (CloudId, index file path, etc.).
If the user asks to **create a new skill**: Use `skill-architect` (invoke with `/skill-architect`). Follow the 5-phase SOP from `rules/creation-sop.md`.
If the user asks to **audit existing skills**: Run containerization + domain-agnostic checks on `seo-web-quality` and `emacs-expert`.
If the user asks to **refine mr-dotcom-dev further**: Load the relevant session file, extract new patterns, and update the corresponding rule. Follow the continuous improvement approach (Section 2.3 #12).
If the user asks to **refine SEO analyzer**: Read the current `AGENTS.md` and `seo-analyzer.js` files, then apply Section 3.5 pending items.
If the user asks for a **new task**: Check Section 2.4 (Pending Work) for outstanding items.

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->

<!-- Local Variables: -->
<!-- gptel-model: gemini-pro-paid -->
<!-- gptel--backend-name: "Gemini Local" -->
<!-- gptel--bounds: nil -->
<!-- End: -->
