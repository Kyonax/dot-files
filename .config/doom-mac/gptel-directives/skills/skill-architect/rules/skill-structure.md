---
title: Three-File Architecture and Directory Layout
impact: CRITICAL
impactDescription: Defines the mandatory file structure, YAML contracts, and progressive disclosure model that every skill must follow. Violating this structure breaks the Two-Agent routing model.
tags: skill, structure, architecture, three-file, directory, layout, yaml, frontmatter, progressive-disclosure, two-agent-model, analyzer, worker
---

This rule defines the mandatory file structure, directory layout, and progressive disclosure model that every skill must follow. Without it, the Worker AI has no understanding of how skills are organized — leading to misplaced content (implementation details in SKILL.md, routing logic in rule files), broken containerization, and ineffective selective loading.

## The Two-Agent Model

Every skill is designed for a two-stage AI system:

| Agent              | Role                 | What it reads                                                        | Goal                                              |
|--------------------|----------------------|----------------------------------------------------------------------|---------------------------------------------------|
| **Skill Analyzer** | Fast routing model   | SKILL.md body + rule YAML frontmatter (`title`, `impactDescription`) | Decide which rules are relevant to the prompt     |
| **Worker**         | Main execution model | Selected rule files (full content)                                   | Perform the task with exhaustive domain knowledge |

**The Analyzer never sees rule content.** It only sees metadata summaries. This means:
- SKILL.md must contain enough keywords for accurate routing
- Rule frontmatter must accurately summarize the rule's full scope
- Rule content must be exhaustive — the Worker has no other source

## The Three File Types

### A. `SKILL.md` — The Router / Manifest

| Property             | Value                                                                                |
|----------------------|--------------------------------------------------------------------------------------|
| **Audience**         | Skill Analyzer AI                                                                    |
| **Purpose**          | Token-efficient index + routing instructions                                         |
| **Required**         | Yes — a skill cannot exist without it                                                |
| **Max size**         | 500 lines                                                                            |
| **Contains**         | YAML frontmatter, when-to-apply section, routing table, quick reference              |
| **Must NOT contain** | Implementation details, domain-specific examples, exhaustive lists, reference tables |

**YAML Frontmatter Contract (Required):**

```yaml
---
name: skill-name                    # Lowercase, hyphens, max 64 chars. Must match directory name.
description: >-                     # Max 1024 chars. What it does + WHEN to use it.
  Keyword-rich description...
metadata:                           # Optional
  author: name
  version: "1.0.0"
---
```

### B. `rules/*.md` — Atomic Knowledge Units

| Property             | Value                                                                                |
|----------------------|--------------------------------------------------------------------------------------|
| **Audience**         | Worker AI                                                                            |
| **Purpose**          | Single, authoritative, exhaustive source of truth for one topic                      |
| **Required**         | At least one rule file for the skill to be useful                                    |
| **Max size**         | No hard limit, but keep focused — split if covering multiple concerns                |
| **Contains**         | YAML frontmatter, exhaustive tables, correct/incorrect examples, complete references |
| **Must NOT contain** | Cross-rule dependencies (each rule must be self-contained)                           |

**YAML Frontmatter Contract (Required):**

```yaml
---
title: Clear, Actionable Title for This Rule
impact: CRITICAL | HIGH | MEDIUM | LOW
impactDescription: One concise sentence on why this rule matters and what it prevents.
tags: comma, separated, keywords, for, semantic, matching
---
```

**Impact Levels:**

| Level      | Meaning                                                                           | Example                                                                                                                                   |
|------------|-----------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------|
| `CRITICAL` | Violating this rule causes broken output, incorrect behavior, or wasted resources | Exhaustive reference list — Worker generates wrong items without it (e.g., all valid classes, all legal clauses, all compliance criteria) |
| `HIGH`     | Violating this rule causes suboptimal but functional output                       | Architectural conventions — output works but violates established patterns or causes subtle issues                                        |
| `MEDIUM`   | Violating this rule causes inconsistency or missed optimization                   | Process or formatting conventions — output is functional but inconsistent with standards                                                  |
| `LOW`      | Nice-to-have guidance that improves quality                                       | Stylistic preferences, documentation formatting, tone guidelines                                                                          |

### C. `AGENTS.md` — Architectural Guide (Optional)

| Property             | Value                                                                     |
|----------------------|---------------------------------------------------------------------------|
| **Audience**         | Worker AI                                                                 |
| **Purpose**          | Explains the "why" behind the rules — philosophy, strategy, rationale     |
| **Required**         | No — omit if the SKILL.md routing table + individual rules are sufficient |
| **Contains**         | High-level rationale, connections between rules, architectural philosophy |
| **Must NOT contain** | Exhaustive implementation details (that belongs in rules)                 |

## The Containerization Principle

**Every skill must be fully self-contained.** All knowledge, examples, references, and context required to understand and use the skill must live inside the skill's own directory. A skill must never depend on — or reference — external skills, external projects, specific user configurations, or system-specific paths.

| Rule                                                                          | Rationale                                                                                                                      |
|-------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------|
| No references to other skills by name                                         | A skill may be installed on a system that doesn't have those other skills. References become broken pointers.                  |
| No references to specific projects, repos, or codebases                       | The skill must work for any user in any environment. Use generic, illustrative examples instead.                               |
| No user-specific paths, usernames, or identities                              | The skill is portable. Paths like `/Users/john/...` or author-specific details break portability.                              |
| No references to external tools, CLIs, or scripts outside the skill directory | If the skill needs a helper, it goes in `scripts/`. If it needs reference data, it goes in `references/` or `assets/`.         |
| Examples must be generic and illustrative                                     | Use domain-neutral examples (e.g., "a CSS utility skill", "an SEO skill") rather than naming real skills.                      |
| All terminology must be defined inline                                        | If a term is specific to this skill's domain, define it within the skill — never assume the reader has context from elsewhere. |

**The test:** Clone the skill directory into a fresh system with no other skills installed. Can the AI read SKILL.md, load any rule, and produce correct output? If no — the skill has an external dependency that must be internalized.

**Why this matters:**
- Skills are shared, forked, and installed independently. A skill that references `my-project-dev` is useless to someone who doesn't have that skill.
- The Two-Agent Model requires that each rule file be the Worker's **sole source of truth**. If the truth lives outside the skill, the Worker has a gap.
- Agent-agnosticism (works in Claude Code, Cursor, Emacs, etc.) is only possible when the skill carries everything it needs.

## Directory Layout

```
skill-name/
├── SKILL.md              # REQUIRED — Frontmatter + routing instructions
├── AGENTS.md             # OPTIONAL — Architectural rationale
├── rules/                # On-demand atomic knowledge units
│   ├── rule-one.md       #   Self-contained, YAML frontmatter required
│   ├── rule-two.md       #   Loaded ONLY when SKILL.md routing table says to
│   └── rule-three.md     #   Each rule = one separable concern
├── references/           # OPTIONAL — Deep reference docs (Agent Skills Spec standard)
├── scripts/              # OPTIONAL — Executable helpers (Agent Skills Spec standard)
└── assets/               # OPTIONAL — Static resources (Agent Skills Spec standard)
```

**Naming conventions:**
- Directory name = skill name (lowercase, hyphens)
- Rule files: `kebab-case.md` describing the topic (e.g., `technical-seo.md`, `contract-templates.md`, `data-validation.md`)
- No prefixes needed on rule files — the directory provides the namespace

## Progressive Disclosure (Agent Skills Specification)

Skills load in three stages to minimize token waste:

| Stage               | What Loads                                   | When                                                 | Token Budget             |
|---------------------|----------------------------------------------|------------------------------------------------------|--------------------------|
| **1. Metadata**     | `name` + `description` from YAML frontmatter | Always (session start)                               | ~100 tokens per skill    |
| **2. Instructions** | Full `SKILL.md` body                         | When skill activates (invoked or auto-triggered)     | <5000 tokens, <500 lines |
| **3. Resources**    | Files in `rules/`, `references/`, etc.       | On demand — only when the agent decides to read them | Unlimited                |

**Stage 3 files are never auto-loaded.** The agent reads them via its file-reading capabilities only when SKILL.md tells it to. This is the mechanism that makes selective loading work — SKILL.md is the router that decides what the Worker sees.

## The "Explicit over Implicit" Principle

When writing rule files for the Worker:

| Do this                                                                            | Not this                                                          |
|------------------------------------------------------------------------------------|-------------------------------------------------------------------|
| List every available item in a table (classes, terms, criteria, options)           | Describe the naming formula and expect the AI to derive all items |
| Include the complete specification (API endpoints, legal clauses, checklist items) | Say "follows standard conventions"                                |
| Show every valid option for a configuration or decision                            | Say "supports standard options"                                   |
| Provide 3-5 correct/incorrect example pairs per major concept                      | Provide 1 example and say "and similar patterns"                  |

**Origin:** This principle was validated when a skill initially tried to document *naming patterns* (formulas) instead of listing all items explicitly. The Worker AI could not reliably reconstruct the full set of available items from the formula. The fix was to list every single item explicitly in tables. This applies equally to code classes, compliance criteria, writing guidelines, research methodologies, or any other enumerable domain knowledge.

## Correct vs. Incorrect Structure Examples

### Example 1: File Type Placement

**Incorrect:** Implementation details in SKILL.md.
```markdown
# SKILL.md
## All Available Consent Types
| Type | When Required | Basis |
| Explicit opt-in | Before collecting data | GDPR Art. 6 |
| Cookie consent | Before non-essential cookies | ePrivacy |
...
```

**Correct:** SKILL.md routes to the rule; the rule contains the details.
```markdown
# SKILL.md — routing table row
| Working on consent flows | `rules/data-privacy.md` |

# rules/data-privacy.md — contains the actual table
## All Available Consent Types
| Type | When Required | Basis |
...
```

**Why:** SKILL.md is a router for the Analyzer, not a reference for the Worker. Exhaustive content in SKILL.md wastes tokens on every activation and bypasses selective loading.

### Example 2: Containerization

**Incorrect:** Rule references an external skill.
```markdown
For the full list of spacing classes, see the `css-utilities` skill.
```

**Correct:** Rule inlines all needed knowledge.
```markdown
## Available Spacing Classes
| Class | Value | Usage |
| mt-50 | 0.5em | Medium top margin |
| mt-100 | 1em | Standard top margin |
...
```

**Why:** The target skill may not exist on the user's system. Every rule must be the Worker's sole source of truth — if the truth lives outside the skill, the Worker has a gap.
