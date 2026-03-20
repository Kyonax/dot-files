---
name: skill-architect
description: >-
  Create, refine, update, and split AI skills following the Two-Agent Model and Agent Skills Specification. Use when creating a new skill from scratch, adding rules to an existing skill, splitting a monolithic skill into atomic rules, writing SKILL.md routing tables, writing rule file YAML frontmatter, reviewing skill structure for token efficiency, or refining skill descriptions for better triggering. Covers the full skill lifecycle from ground truth mining through testing and iteration.
metadata:
  author: @kyonax_on_tech
  version: "1.1.0"
---

# Skill Architect Guide

Provides the complete methodology for engineering AI skills — structured knowledge bases that give AI agents deep, domain-specific context while minimizing token waste. Every skill must serve two audiences: a fast Analyzer that routes, and a powerful Worker that executes.

## Core Principle: Selective Loading

**Rules are optional resources, not auto-loaded content.** The SKILL.md router determines which rule files an agent reads based on the task at hand. Each rule file must be atomic enough that loading it for an unrelated task would be pure waste. If a rule covers two separable concerns, it must be split. The cost of an unnecessary rule load is wasted tokens and diluted focus; the cost of a missing rule is wrong output. Optimize for precision.

## When to Apply

Reference these guidelines when:

*   Creating a new skill from scratch (any domain).
*   Adding new rule files to an existing skill.
*   Splitting a monolithic skill file into atomic rules.
*   Writing or refining SKILL.md frontmatter descriptions for better agent triggering.
*   Writing SKILL.md routing tables that map tasks to rule files.
*   Writing rule file YAML frontmatter (`title`, `impact`, `impactDescription`, `tags`).
*   Reviewing an existing skill for token efficiency, accuracy, or completeness.
*   Deciding whether knowledge belongs in CLAUDE.md, `.claude/rules/`, or a skill.

## When to Read Which Rules

| If working on...                                              | Read these rules                                     |
|---------------------------------------------------------------|------------------------------------------------------|
| Creating a new skill end-to-end                               | `rules/creation-sop.md` + `rules/skill-structure.md` |
| Writing or refining a SKILL.md file                           | `rules/skillmd-routing.md`                           |
| Writing or refining a rule file (`rules/*.md`)                | `rules/rule-writing.md`                              |
| Understanding the Three-File architecture or directory layout | `rules/skill-structure.md`                           |
| Splitting a monolithic skill or refactoring existing rules    | `rules/refinement-patterns.md`                       |
| Setting up skills for Claude Code, GPTel, or other agents     | `rules/multi-agent-consumption.md`                   |
| Understanding why selective loading matters                   | `rules/rule-writing.md` (Section 1)                  |
| Ensuring a skill has no external dependencies                 | `rules/skill-structure.md` + `rules/rule-writing.md` |

## Quick Reference

| Rule                      | Description                                                                                                                                                                                                                                                        |
|---------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `skill-structure`         | Three-File architecture (SKILL.md, rules/*.md, AGENTS.md), directory layout, YAML frontmatter contracts, progressive disclosure stages, the Two-Agent Model, and the Containerization Principle (no external references — all data inside the skill).              |
| `skillmd-routing`         | How to write SKILL.md: YAML frontmatter with keyword-dense descriptions, routing tables that map tasks to rule files, size limits (<500 lines), and patterns for effective agent triggering.                                                                       |
| `rule-writing`            | How to write rules/*.md: mandatory YAML frontmatter, exhaustive self-contained content, atomic splitting strategy, selective loading principles, containerization rule (no external references), correct-vs-incorrect examples, and table-driven reference format. |
| `creation-sop`            | The 5-phase Standard Operating Procedure for creating a skill from scratch: ground truth mining, categorization, rule writing (Worker first), SKILL.md writing (router last), testing and iteration.                                                               |
| `refinement-patterns`     | How to update, refine, split, and evolve existing skills: identifying split candidates, preserving routing accuracy during refactors, version bumping, and promoting session guidelines into skill rules.                                                          |
| `multi-agent-consumption` | How different AI agents (Claude Code, GPTel, Cursor) discover, load, and consume skills. Covers symlinks, context budgets, skill-vs-rules-vs-CLAUDE.md decision tree, and the GPTel analyzer pipeline.                                                             |
