# Skill Architect — Architectural Guide

## Core Philosophy: The Two-Agent Model

Every skill exists to serve a two-stage AI system. Understanding this separation is the single most important concept in skill engineering.

1. **The Skill Analyzer** — A fast, low-cost model that reads high-level summaries (SKILL.md body + rule YAML frontmatter) to decide which knowledge files are relevant to the user's prompt. It never sees rule content — only metadata.
2. **The Worker** — The main, powerful model that receives only the selected rule files to perform the task. It needs complete, explicit, exhaustive references — not summaries or formulas.

**The guiding principle is "Explicit over Implicit."** The Analyzer gets the map. The Worker gets the territory. Never ask the Worker to derive information from a pattern when you can give it the full table.

This principle was validated in practice when a skill initially tried to document utility class *patterns* (naming formulas) instead of listing all classes explicitly. The Worker AI could not reliably reconstruct the full set of available tools from the formula alone. The fix was to list every single item in tables.

## Why Selective Loading Matters

Skills exist because dumping all knowledge into every request wastes tokens and dilutes focus. The entire architecture is designed around one goal: **load only what the task needs, nothing more.**

This creates a chain of responsibilities:

*   **SKILL.md** must be a precise router — its routing table determines what gets loaded. A vague routing table causes over-loading (wasted tokens) or under-loading (missing context).
*   **Rule files** must be atomic — each covering exactly one separable concern. If a rule file contains two topics that could be needed independently, it must be split. The cost of loading an irrelevant 200-line rule file is real.
*   **Rule frontmatter** must be keyword-accurate — routing analyzers read `title` + `impactDescription` to make routing decisions. Inaccurate frontmatter causes wrong rules to load.

## The Pillars of Skill Engineering

### 1. Structure: The Three-File Architecture
Every skill is composed of SKILL.md (router), rules/*.md (knowledge), and optionally AGENTS.md (rationale). Each file type has a distinct audience and purpose. Mixing these purposes (e.g., putting implementation details in SKILL.md) breaks the routing model.
*   **Reference:** `rules/skill-structure.md`

### 2. Routing: The SKILL.md Contract
The SKILL.md file is the most important file in any skill. Its description determines whether the skill activates. Its routing table determines which rules load. A poorly written SKILL.md makes the entire skill unreliable.
*   **Reference:** `rules/skillmd-routing.md`

### 3. Knowledge: Atomic Rule Files
Rule files are where domain expertise lives. They must be exhaustive (the Worker's only source of truth), self-contained (no cross-rule dependencies), atomic (one concern per file), and **containerized** (no references to external skills, projects, or system-specific data). A skill directory must carry everything the Worker needs — if it were cloned to a fresh system with no other skills, it must still work. The "Explicit over Implicit" and "Containerization" principles live here.
*   **Reference:** `rules/rule-writing.md`

### 4. Process: The Creation SOP
Skills are built in a specific order — rules first, router last. This ensures the routing table accurately reflects what was actually written, not what was planned. Ground truth comes from real source material — codebases, documentation, workflows, domain expertise, research — not assumptions.
*   **Reference:** `rules/creation-sop.md`

### 5. Evolution: Refinement Patterns
Skills are living documents. They grow as new patterns emerge, split when they become monolithic, and absorb session-specific guidelines when those guidelines prove stable. Understanding when and how to evolve a skill is as important as creating one.
*   **Reference:** `rules/refinement-patterns.md`

### 6. Consumption: Multi-Agent Compatibility
The same skill files serve Claude Code, GPTel, Cursor, and any future AI that reads markdown. Skills must be agent-agnostic in content while understanding how each agent discovers and loads them.
*   **Reference:** `rules/multi-agent-consumption.md`
