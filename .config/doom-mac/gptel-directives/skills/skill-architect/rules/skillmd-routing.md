---
title: Writing Effective SKILL.md Files — Routing and Triggering
impact: CRITICAL
impactDescription: The SKILL.md file determines whether a skill activates and which rules load. A poorly written SKILL.md makes the entire skill unreliable — either never triggering or loading wrong rules.
tags: skill, skillmd, routing, description, frontmatter, trigger, activation, routing-table, quick-reference, keywords, token-efficiency
---

This rule governs how to write effective SKILL.md files — the single most important file in any skill. A poorly written SKILL.md causes the skill to never activate (bad description), load wrong rules (bad routing table), or waste tokens (leaking content that belongs in rule files). Every routing failure traces back to this file.

## The SKILL.md Contract

SKILL.md serves exactly two functions:
1. **Triggering** — Its `description` field determines whether any agent activates the skill for a given prompt
2. **Routing** — Its body tells the agent which rule files to read based on the task context

It must NOT contain implementation details, exhaustive lists, or code examples. Those belong in rule files. SKILL.md is a map, not the territory.

## Writing the Description Field (Triggering)

The `description` in YAML frontmatter is the **primary triggering mechanism** — it determines whether any agent (Claude Code, GPTel, Cursor) activates the skill.

### Rules for Effective Descriptions

1. **Be keyword-dense** — Include concrete terms that match what users actually type
2. **Be slightly "pushy"** — Claude tends to under-trigger skills; assertive descriptions help
3. **Include BOTH** what the skill does AND specific trigger conditions/contexts
4. **Prefer single-line** — Claude Code's indexer doesn't always parse YAML multiline correctly
5. **Max 1024 characters** — Agent Skills Specification limit

### Good vs. Bad Descriptions

**Good — keyword-dense, specific triggers:**
```yaml
description: >-
  Create, refine, update, and split AI skills following the Two-Agent Model
  and Agent Skills Specification. Use when creating a new skill, adding rules,
  splitting monolithic skills, writing SKILL.md routing tables, writing rule
  YAML frontmatter, or reviewing skill structure for token efficiency.
```

**Bad — too vague:**
```yaml
description: Helps with AI skill management and creation.
```

**Bad — too narrow:**
```yaml
description: Creates SKILL.md files for AI agents.
```

### Keyword Strategy

Include keywords for:
- **Actions:** create, refine, update, split, review, audit, write, add, remove
- **Objects:** skill, rule, SKILL.md, frontmatter, routing table, description
- **Contexts:** "when creating a new skill", "when adding rules to an existing skill"
- **Domain terms:** Two-Agent Model, progressive disclosure, token efficiency, selective loading

## Writing the Body (Routing)

### Required Sections (in order)

#### 1. One-Line Summary
A single paragraph explaining what the skill provides. No heading needed — it follows the YAML frontmatter block directly or sits under the skill title.

#### 2. Core Principle (Optional)
If the skill has one overriding principle that governs all rules, state it prominently. This orients the Worker before it reads any rules.

#### 3. When to Apply
Bullet list of specific scenarios where this skill is relevant. Use "Reference these guidelines when:" as the lead-in.

#### 4. Routing Table — "When to Read Which Rules"
**This is the most important section.** It maps task contexts to specific rule files.

**Pattern: Context-Based Routing Table**
```markdown
| If working on... | Read these rules |
|---|---|
| Creating a new skill end-to-end | `rules/creation-sop.md` + `rules/skill-structure.md` |
| Writing a SKILL.md file | `rules/skillmd-routing.md` |
| Writing a rule file | `rules/rule-writing.md` |
```

**Rules for routing tables:**
- Every rule file MUST appear in at least one row
- A task may require multiple rule files — list them all with `+`
- Be specific about the task context — vague rows cause over-loading
- Order rows by frequency (most common tasks first)

**Anti-pattern: Routing table that loads everything**
```markdown
| If working on... | Read these rules |
|---|---|
| Any skill work | All rules |
```
This defeats the purpose of selective loading.

#### 5. Quick Reference Table
Maps each rule name to a keyword-rich description. This is what the GPTel Analyzer reads to make routing decisions.

```markdown
| Rule | Description |
|---|---|
| `rule-name` | Dense, keyword-rich summary of the full scope of this rule file. |
```

**Rules for Quick Reference descriptions:**
- Must summarize the FULL scope of the rule — not just the title
- Include specific terms that appear in the rule's content
- The Analyzer uses these descriptions for semantic matching — accuracy is critical
- Each description should be 1-2 sentences, ~100-150 characters
- **IMPORTANT:** If a description exceeds ~15 words, put a short summary in the cell and add a `[Details](#anchor)` link to a named block below the table. Long cells break alignment and hurt readability for both humans and AI

## Size Constraints

| Metric                  | Limit                    | Rationale                                        |
|-------------------------|--------------------------|--------------------------------------------------|
| Total lines             | <500                     | Stays within Stage 2 token budget (~5000 tokens) |
| Description field       | <1024 chars              | Agent Skills Specification limit                 |
| Quick Reference entries | Match rule count exactly | Every rule must be indexed                       |

**If SKILL.md approaches 500 lines:** You have too much content in it. Move implementation details, examples, and exhaustive references into rule files. SKILL.md should only contain routing logic.

## Complete SKILL.md Template

```markdown
---
name: skill-name
description: >-
  Keyword-dense description of what this skill does and when to use it.
  Include concrete trigger conditions and action verbs.
metadata:
  author: Name
  version: "1.0.0"
---

# Skill Title

One-paragraph summary of what this skill provides and its core principle.

## When to Apply

Reference these guidelines when:

*   Specific scenario 1.
*   Specific scenario 2.
*   Specific scenario 3.

## When to Read Which Rules

| If working on... | Read these rules |
|---|---|
| Task context 1 | `rules/rule-a.md` + `rules/rule-b.md` |
| Task context 2 | `rules/rule-c.md` |
| Task context 3 | `rules/rule-a.md` + `rules/rule-d.md` |

## Quick Reference

| Rule | Description |
|---|---|
| `rule-a` | Keyword-rich description summarizing the full scope of this rule file. |
| `rule-b` | Keyword-rich description summarizing the full scope of this rule file. |
| `rule-c` | Keyword-rich description summarizing the full scope of this rule file. |
| `rule-d` | Keyword-rich description summarizing the full scope of this rule file. |
```
