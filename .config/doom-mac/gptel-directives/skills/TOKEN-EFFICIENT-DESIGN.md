# Token-Efficient Skill Design

## The Problem

An LLM context window is not free. Every token loaded into it has three costs:

1. **Latency** — more tokens in = slower response.
2. **Money** — input tokens are billed per call, multiplied across every parallel worker.
3. **Accuracy** — a model reviewing 150 rules about CSS, routing, SEO, and scripts will miss the heading-hierarchy bug that a model reading 6 targeted rules would catch.

The third cost is the one that justifies the entire architecture. Noise doesn't just cost money — it degrades quality. Every design decision in this skill system exists to put the **minimum correct context** in front of the model at the moment it needs it.

---

## The Foundation: Two-Agent Model

Every skill is designed for a two-stage AI system.

| Agent | Role | What It Reads | Goal |
|---|---|---|---|
| **Skill Analyzer** | Fast, cheap routing model | `SKILL.md` body + rule YAML frontmatter (`title`, `impactDescription`) | Decide which rules are relevant |
| **Worker** | Main execution model | Selected rule files (full content) | Perform the task with exhaustive knowledge |

**The Analyzer never sees rule content.** It reads only metadata summaries. The Worker never reads rules it doesn't need — the Analyzer and the routing table decide that in advance.

This separation has a hard implication: `SKILL.md` must be optimized for the Analyzer (keyword-dense, accurate descriptions), and rule files must be optimized for the Worker (exhaustive, explicit, self-contained). A file that tries to serve both purposes serves neither.

---

## Progressive Disclosure: The Three-Stage Load Model

Skills don't dump everything into context at once. Loading happens in three stages:

```
Stage 1 — Metadata  (~100 tokens / skill)
  name + description from YAML frontmatter
  Loaded at session start for every skill.
  Used by the Analyzer to decide: "is this skill relevant?"

Stage 2 — Instructions  (<5000 tokens)
  Full SKILL.md body
  Loaded only when the skill activates (invoked or auto-triggered).
  Contains: when-to-apply, routing table, quick reference.

Stage 3 — Resources  (on-demand, unbounded)
  Individual rule files from rules/
  Loaded only when the SKILL.md routing table tells the agent to read them.
  Never auto-loaded. The agent reads them via file-read when directed.
```

Stage 3 is where selective loading actually happens. The routing table in `SKILL.md` is the mechanism — it maps task context to file paths. If the routing table is vague or over-broad, Stage 3 loads everything. If it's precise, the Worker gets only what it needs.

---

## Core Principles

### 1. One Rule Per File — The Unit of Loading

A monolithic rules document forces the model to load every rule every time, regardless of what's in the code. Atomic rules — one concern per `.md` file — make selective loading possible at file granularity.

The splitting test: _"Can I imagine a task that needs concern A but not concern B?"_
- Yes → they are separate files.
- No → they can stay together.

```
Monolithic compliance.md (GDPR + WCAG + ToS)
  → loaded for a GDPR task → 60% irrelevant content

Split:
  data-privacy.md       → loaded for GDPR task only
  accessibility-reqs.md → loaded for WCAG task only
  terms-of-service.md   → loaded for ToS task only
```

A rule file that covers two independently-needed concerns wastes tokens on every task that needs only one of them.

---

### 2. The SKILL.md Routing Table — How Rules Actually Load

Rule files are **not auto-loaded**. They are read by the Worker only when `SKILL.md` explicitly directs it to via the routing table. This is the mechanism that makes selective loading work.

```markdown
## When to Read Which Rules

| If working on...              | Read these rules                      |
|-------------------------------|---------------------------------------|
| Creating a new skill          | rules/creation-sop.md + rules/skill-structure.md |
| Writing a SKILL.md file       | rules/skillmd-routing.md              |
| Writing a rule file           | rules/rule-writing.md                 |
```

Rules for an effective routing table:
- Every rule file must appear in at least one row.
- Be specific about task context — vague rows cause over-loading.
- A routing row that says "load all rules" defeats the entire architecture.
- Rows are ordered by task frequency (most common first).

The Quick Reference table beneath it is what the GPTel Analyzer reads for semantic routing. Descriptions must be keyword-dense summaries of the rule's actual content — not its title, not a plan for it.

---

### 3. Tags as a Filtering Mechanism — Not Metadata

In skills with automation scripts, rule files carry a `tags` frontmatter field:

```yaml
tags: aria-labelledby, v-if, h2, wcag-1.3.1
```

These tags are **code-greppable keywords** — CSS properties, HTML attributes, framework directives, function names — things that literally appear in source code diffs.

The rule selection script runs two passes:

```
Pass 1 — Directory-level
  Which rule directories are relevant to this project?
  (detected from tech stack, file paths, or explicit signals)

Pass 2 — Tag-level
  Within those directories, which rules have tags that appear in the diff?
```

Pass 2 is the token-saver. A project with 60 accessibility rules won't load all 60. If the diff touches `aria-labelledby` and `v-if`, only the rules tagged with those keywords reach the worker.

**Tags must be authored as code artifacts, not descriptions.** A tag like `accessibility` is useless — it won't appear in a diff. A tag like `aria-labelledby` will match the exact string a developer writes.

---

### 4. Explicit over Implicit — The Worker Has No Other Source

Rule files are the Worker's **sole source of truth**. There is no fallback, no other file to check. This creates an obligation: rule content must be exhaustive.

| Do this | Not this |
|---|---|
| List every available item in a table | Describe the naming formula and expect AI to derive values |
| Include the complete specification | Say "follows standard conventions" |
| Show every valid option | Say "supports standard options" |
| 3-5 correct/incorrect example pairs | 1 example and "and similar patterns" |

This principle was validated when a skill initially documented utility class *naming patterns* instead of listing all classes explicitly. The Worker could not reliably reconstruct the full set from the formula. The fix was to list every item in a table. Formulas produce guesses. Tables produce correct output.

---

### 5. Containerization — Self-Contained or Broken

Every skill must work in isolation on any system. It must never reference external skills by name, specific project paths, user identifiers, or tools not bundled inside the skill directory.

| Violation | Fix |
|---|---|
| "See the `css-utilities` skill for spacing classes" | Inline the spacing class table inside this rule |
| Reference to `/Users/john/projects/...` | Use placeholder paths |
| Dependency on a CLI tool not in `scripts/` | Bundle the helper in `scripts/` |
| Jargon defined only outside this skill | Define the term inline |

**The test:** Clone the skill directory to a fresh system with zero other skills installed. Can the AI produce correct output from any rule file alone? If not, it has an external dependency that must be internalized.

---

### 6. Shell Scripts for Deterministic Work

Detection and selection are deterministic problems. Given a git remote URL, a set of changed file paths, a `package.json`, and a diff — the correct rule set can be computed without any AI involvement.

Delegating deterministic work to shell scripts means:

- **Zero AI tokens spent on what has a known answer.** The model never reasons about "which rules apply" — that was resolved before it ran.
- **Reproducible results.** Same inputs always produce the same rule set, independent of model temperature.
- **Inspectable pipeline.** Each script has one job and emits JSON to stdout. Output can be read, debugged, and overridden without touching AI code.

```
detect.sh           → what project/stack/context is this? → detection.json
select-rules.sh     → which rules are relevant? (2-pass)  → rule paths
worker-dispatch.sh  → how do we split rules across workers? → workers.json
worker-prompt-builder.sh → assemble per-worker targeted context → context.json
```

AI runs only after all of that is resolved.

---

### 7. Worker Isolation and Section Splitting

Workers have no cross-dependencies. Each one:
- Receives its category's INDEX.md (framing) + its tag-matched rule files.
- Receives only the code section relevant to its rules (not full files).
- Emits YAML findings independently.

A 600-line Vue SFC split into template/script/style gives each worker ~200 focused lines. An accessibility worker reviewing `<template>` for ARIA issues isn't distracted by Vuex action logic.

Isolation also enables parallelism. A 6-worker review takes roughly the same time as a 1-worker review with far broader coverage. Post-AI scripts merge and deduplicate YAML outputs — the presenter AI resolves, not discovers.

---

## Compound Effect: Each Stage Narrows

Each principle is additive:

| Stage | What's eliminated |
|---|---|
| Routing table | Rules not needed for this task |
| Tag-based selection (Pass 2) | Rules not relevant to the diff |
| Section splitting | Unrelated code sections within the same file |
| Worker isolation | Other categories' rules and code |
| Pre-AI scripts | All detection and selection inference cost |

The system doesn't make the model smarter. It removes the conditions that make it fail.

---

---

# How to Create a Skill

## The 5-Phase SOP (Mandatory Order)

```
Phase 1: Ground Truth Mining
    ↓ produces: raw patterns, domain samples, source material
Phase 2: Categorization
    ↓ produces: list of atomic rule file names with scopes
Phase 3: Write Rules  ← WORKER FIRST
    ↓ produces: complete rules/*.md files
Phase 4: Write SKILL.md  ← ROUTER LAST
    ↓ produces: accurate routing table and quick reference
Phase 5: Test & Iterate
    ↓ produces: validated, production-ready skill
```

**Why this order is non-negotiable:** Writing `SKILL.md` first produces a routing table based on *planned* content, not *actual* content. Categories shift during rule writing — topics merge, scope narrows, edge cases surface. A routing table written from plans will have keyword mismatches and Quick Reference descriptions that don't reflect what the rules actually contain.

---

### Phase 1: Ground Truth Mining

Gather all raw source material for the skill's domain before writing a single rule.

- Read representative artifacts: code files, documents, policies, specifications, checklists.
- Note conventions: naming patterns, terminology, formatting standards, structural choices.
- Surface tribal knowledge: patterns practitioners follow that aren't documented anywhere.
- For external source material: **abstract the domain knowledge, discard the source structure**. Never replicate the format of an external document — extract what matters and restructure it into the three-file blueprint.

**Output:** A collection of raw patterns and domain samples. Unstructured, but comprehensive.

---

### Phase 2: Categorization

Break the raw material into atomic, independent categories. Each category becomes one rule file.

Apply the splitting test to every proposed group:
> "Can I imagine a task that needs this category but not another category in the same group?"
> - Yes → they are separate rules.
> - No → they can stay together.

Name each category with a descriptive `kebab-case` name that describes its specific scope, not its parent domain.

| Domain | Categories | Splitting Logic |
|---|---|---|
| SEO | `technical-seo`, `on-page-seo`, `structured-data`, `mobile-seo` | Each sub-discipline is independently needed |
| Compliance | `data-privacy`, `accessibility-requirements`, `terms-of-service` | Each compliance area is independently needed |
| Document generator | `index-management`, `data-parsing`, `format-reference`, `templates` | Each generation phase is independently needed |

**Output:** List of rule file names with scopes and impact levels.

---

### Phase 3: Write Rules (Worker First)

Create complete, exhaustive rule files. For each:

1. Write YAML frontmatter: `title`, `impact`, `impactDescription`, `tags`.
2. Write an opening paragraph: scope + consequence of violation.
3. Write an exhaustive body: tables for enumerable data, correct/incorrect example pairs, complete references.
4. Verify self-containment: can this rule be understood without reading any other rule?
5. Verify atomicity: does it cover exactly one separable concern?

**Frontmatter contract:**
```yaml
---
title: Clear, Actionable Title Describing Scope
impact: CRITICAL | HIGH | MEDIUM | LOW
impactDescription: One sentence on what goes wrong WITHOUT this rule.
tags: comma, separated, code-greppable, keywords
---
```

`impactDescription` must explain the failure mode, not just the topic.
- Bad: `"Covers spacing."`
- Good: `"Eliminates magic numbers and ensures all spacing follows the design system scale."`

**Output:** Complete `rules/*.md` files.

---

### Phase 4: Write SKILL.md (Router Last)

Now that the rules exist, write the router. Re-read each rule file first — descriptions written from memory of Phase 2 plans are often inaccurate.

**SKILL.md has one job:** tell the Analyzer which skill to activate, and tell the Worker which rules to load.

```yaml
---
name: skill-name
description: >-
  Keyword-dense description. What it does AND specific trigger conditions.
  Include action verbs and concrete context. Max 1024 chars.
metadata:
  version: "1.0.0"
---
```

The `description` field is the primary triggering mechanism for every agent. It must be specific, keyword-dense, and slightly assertive — agents tend to under-trigger skills with vague descriptions.

Required body sections in order:
1. One-line summary of what the skill provides.
2. **When to Apply** — specific scenarios as bullet points.
3. **Routing table** — maps task context to rule file paths.
4. **Quick Reference** — one keyword-dense description per rule (Analyzer reads this).

**SKILL.md must stay under 500 lines.** If it grows past that, implementation details have leaked in from rule files.

**Output:** Complete `SKILL.md`.

---

### Phase 5: Test & Iterate

1. Invoke the skill manually via `/skill-name` and confirm it loads.
2. Test with realistic prompts — does the right rule load? Does an unrelated rule stay unloaded?
3. Try edge cases: prompts that should load only one specific rule.
4. If the skill doesn't activate → add keywords to `description`.
5. If wrong rules load → tighten routing table conditions.
6. If output is wrong → add more examples or explicit data to the rule.
7. If too much loads → split rules further.

---

## When to Add Automation Scripts

Not every skill needs a `scripts/` directory. The decision depends on whether the skill's work involves **deterministic problems that don't benefit from AI reasoning**.

**Add scripts when the skill needs to:**
- Detect context from the environment (git state, file paths, package.json, diff content).
- Select or filter knowledge files before the AI runs.
- Process or transform tool output (JSON merging, deduplication, formatting).
- Gather structured context the AI will use but doesn't need to derive itself.

**Skip scripts when the skill is:**
- A pure knowledge reference (the AI reads rules and applies them to content the user provides directly).
- An invocable command with a well-defined human-facing flow.
- A domain guide with no environmental detection needed.

### The Pipeline Pattern

When scripts are appropriate, they follow a strict pipeline design:

```
Each script:
  - Has one job
  - Reads from stdin or explicit file arguments
  - Writes structured JSON to stdout
  - Fails fast with a non-zero exit code and a clear error message
  - Never modifies state (detection/selection scripts are read-only)

Pipeline:
  script-a.sh → json-a → script-b.sh < json-a → json-b → AI worker
```

This composability means every stage is independently inspectable and testable. If the AI produces unexpected output, each pipeline stage can be run in isolation to find where the problem entered.

### Pre-AI vs Post-AI Scripts

Scripts fall into two categories:

**Pre-AI scripts** — run before any AI worker. Their job is to narrow what the AI sees.

| Script type | Job | Output |
|---|---|---|
| Detection | Read environment signals → classify context | `detection.json` |
| Rule selection | Filter rule paths by context + diff keywords | Line-separated file paths |
| Worker dispatch | Group rules → assign workers | `workers.json` |
| Context assembly | Gather code sections, cross-refs, prior reviews | Per-worker context JSON |

**Post-AI scripts** — run after AI workers complete. Their job is to clean and format AI output.

| Script type | Job | Output |
|---|---|---|
| Deduplication | Merge cross-worker findings, remove repeats | Cleaned findings YAML |
| Formatting | Sort by severity, format into readable output | Markdown report |

### Script Design Rules

1. **One job per script.** A script that detects context AND selects rules is two scripts waiting to happen.
2. **JSON to stdout.** Every script's output is machine-readable by the next stage and human-readable for debugging.
3. **Graceful fallbacks.** Missing git, no package.json, unrecognized project — all fall back to `"generic"` rather than failing. Detection scripts must never block execution.
4. **No AI calls inside scripts.** Scripts are the deterministic layer. As soon as a script needs to "reason" about something, that work belongs in the AI layer.
5. **Pass data forward, not decisions.** Scripts provide signals and context. The AI decides what to do with them.

### What a Detection Script Actually Does

A detection script answers: *"Given this repository, what kind of project are we working with?"*

It reads observable signals — git remote URL, changed file paths, package.json dependencies — and produces a structured JSON classification. No AI needed because the signals have deterministic mappings:

```
git remote URL   →  which organization/brand owns this repo
changed file paths →  which sub-project (frontend, backend, mobile)
package.json deps  →  which tech stack (vue, react, express)
```

That classification feeds the rule selection script, which uses it to choose which rule directories to include in Pass 1, before tag matching narrows further in Pass 2.

### The Full Automated Pipeline (Reference)

When a skill implements full automation, the stages look like this:

```
─── PRE-AI ─────────────────────────────────────────────────────────────
detect.sh
  Reads: git remote, changed file paths, package.json
  Emits: detection.json  { brand, project, techStack, signals }

select-rules.sh < detection.json [--context diff.json]
  Pass 1: directory-level selection from detection results
  Pass 2: tag matching against diff keywords (optional)
  Emits: one rule-file path per line

worker-dispatch.sh
  Reads: rule paths from stdin
  Groups by parent directory, merges small groups, caps at N workers
  Emits: workers.json  { workers: [{ id, rules[], category }] }

context-assembly scripts (sfc-split, crossref, pr-digest, etc.)
  Read: source files, PR metadata
  Emits: sections.json, crossref.json, digest.json

worker-prompt-builder.sh
  Reads: workers.json + all context JSON
  Emits: per-worker targeted context JSON

─── AI (parallel workers) ──────────────────────────────────────────────
Each worker receives:
  - Category INDEX.md (structural framing)
  - Only its tag-matched rule files
  - Only its relevant code sections
  - Cross-reference hints
  - Already-flagged items from prior reviews
Each worker emits: YAML findings

─── POST-AI ─────────────────────────────────────────────────────────────
findings-dedup.sh
  Reads: all worker YAML findings + prior review digest
  Deduplicates across workers and against already-addressed items
  Emits: cleaned-findings.yaml

format-findings.sh
  Reads: cleaned-findings.yaml
  Sorts by severity, formats into readable output
  Emits: findings.md
```

---

## Skill Creation Checklist

### Structure
- [ ] `SKILL.md` exists with valid YAML frontmatter (`name`, `description`)
- [ ] `rules/` directory exists with at least one rule file
- [ ] `AGENTS.md` exists if the skill has complex architectural rationale (optional otherwise)
- [ ] `scripts/` directory exists only if the skill has deterministic environment detection or processing to do

### Rules
- [ ] Every rule file has all 4 frontmatter fields (`title`, `impact`, `impactDescription`, `tags`)
- [ ] `impactDescription` explains the failure mode, not just the topic
- [ ] Every rule covers exactly one separable concern (splitting test passes)
- [ ] Every rule is self-contained — no unexplained references to other rules
- [ ] Enumerable data is in tables, not prose
- [ ] At least 2 correct/incorrect example pairs per major concept
- [ ] No references to external skills, specific projects, or user-specific paths

### SKILL.md
- [ ] `description` is keyword-dense, includes trigger conditions and action verbs
- [ ] Routing table includes every rule file in at least one row
- [ ] Quick Reference descriptions match actual rule content (re-read rules to verify)
- [ ] SKILL.md is under 500 lines

### Scripts (if present)
- [ ] Each script has one job
- [ ] Each script emits JSON to stdout
- [ ] Detection scripts have graceful fallbacks for unrecognized environments
- [ ] No script makes AI calls
- [ ] Pre-AI scripts narrow context; Post-AI scripts clean output

### Routing was written LAST
- [ ] SKILL.md routing table was written after rule files were complete
- [ ] Quick Reference descriptions were written from actual rule content, not plans
