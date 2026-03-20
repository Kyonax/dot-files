---
title: Multi-Agent Skill Consumption — Claude Code, GPTel, and Beyond
impact: MEDIUM
impactDescription: Skills must work across multiple AI agents without modification. Incorrect setup (broken symlinks, agent-specific content) causes skills to fail silently in some environments.
tags: skill, agent, claude-code, gptel, cursor, symlink, consumption, loading, discovery, context-budget, rules, claudemd, decision-tree, multi-agent, agnostic
---

This rule governs how skills are discovered, loaded, and consumed across different AI agents — Claude Code, GPTel, Cursor, and beyond. Without it, skills may be set up with broken symlinks, include agent-specific content that fails in other environments, or place knowledge in the wrong location (CLAUDE.md vs rules vs skills), wasting tokens or missing context.

## Agent-Agnostic Content Rule

**Skill content (SKILL.md, rules/*.md, AGENTS.md) must never contain agent-specific references.** No tool names (e.g., `Read`, `Glob`, `Bash`), no editor-specific function names, no IDE-specific syntax.

The same skill files are consumed by Claude Code, GPTel, Cursor, and any future AI that reads markdown. Agent-specific setup (symlinks, configuration) happens outside the skill content.

---

## How Each Agent Loads Skills

### Claude Code

| Stage              | What Happens                                                                                            |
|--------------------|---------------------------------------------------------------------------------------------------------|
| **Discovery**      | Scans `.claude/skills/*/SKILL.md` (project) and `~/.claude/skills/*/SKILL.md` (global) at session start |
| **Stage 1**        | Reads `name` + `description` from YAML frontmatter (~100 tokens)                                        |
| **Activation**     | Auto-triggered by description matching against user prompt, or manually via `/skill-name`               |
| **Stage 2**        | Loads full SKILL.md body when activated                                                                 |
| **Stage 3**        | Reads rule files via `Read` tool when SKILL.md routing says to                                          |
| **Context budget** | ~2% of context window (fallback: 16,000 chars). Override: `SLASH_COMMAND_TOOL_CHAR_BUDGET=25000`        |
| **Check usage**    | Run `/context` to see if skills are excluded for budget reasons                                         |

**Claude Code skill locations:**
- **Project skills:** `<project-root>/.claude/skills/skill-name/SKILL.md`
- **Global skills:** `~/.claude/skills/skill-name/SKILL.md`
- Global skills are available in all projects. Project skills only in that project.

### Emacs-Based Agents (GPTel, etc.)

| Stage         | What Happens                                                                   |
|---------------|--------------------------------------------------------------------------------|
| **Discovery** | Custom Elisp function scans skill directories at send time                     |
| **Stage 1**   | Reads full SKILL.md + all rule YAML frontmatter (`title`, `impactDescription`) |
| **Routing**   | A fast, cheap model analyzes the user's prompt against skill summaries         |
| **Output**    | Returns a structured declaration of which skills/rules to load                 |
| **Stage 2+3** | Selected files loaded into worker context                                      |

**Common loading formats (Emacs agents):**
- `skill-name` → SKILL.md + AGENTS.md (core only)
- `skill-name/*` → core + ALL rules
- `skill-name/rule-name` → core + one specific rule
- **20% Rule:** Analyzer includes a rule if even partial semantic connection exists

### Other Agents (Cursor, Windsurf, etc.)

Follow the Agent Skills Specification pattern:
1. Scan for `SKILL.md` files in the configured skills directory
2. Read `description` from frontmatter for activation
3. Load full SKILL.md on activation
4. Read referenced files on demand

---

## Symlink Setup for Multi-Agent Consumption

When skills are used by multiple agents, they should live in ONE canonical location and be symlinked to each agent's skill directory:

```
CANONICAL LOCATION (your version-controlled repo):
/path/to/your/skills-repo/skills/skill-name/

SYMLINKS (one per agent):
~/.claude/skills/skill-name       → canonical location   (Claude Code)
~/.cursor/skills/skill-name       → canonical location   (Cursor, if applicable)
/path/to/emacs/skills/skill-name  → canonical location   (Emacs agents)
```

**Creating a symlink:**
```bash
ln -s /path/to/canonical/skills/skill-name \
      ~/.claude/skills/skill-name
```

**Verifying a symlink:**
```bash
ls -la ~/.claude/skills/skill-name
# Should show: skill-name -> /path/to/canonical/skills/skill-name
```

**Rules for symlinks:**
- Never duplicate skill files — always symlink from a single canonical source
- The canonical source should be version-controlled (e.g., in a dot-files repo)
- All agents read through symlinks transparently
- If the skill name differs between agents, use the target agent's preferred name for the symlink

---

## Skills vs Rules vs CLAUDE.md Decision Tree

When deciding where to put knowledge:

```
Does the AI need this on EVERY request?
├── YES → Does it apply to specific file types only?
│   ├── YES → .claude/rules/*.md (with paths: frontmatter)
│   └── NO  → CLAUDE.md
└── NO  → Is it a self-contained domain of knowledge?
    ├── YES → Skill (SKILL.md + rules/*.md)
    └── NO  → Session context block (temporary, promote to skill when stable)
```

**Detailed comparison:**

| Feature            | `CLAUDE.md`                           | `.claude/rules/*.md`               | `.claude/skills/*/SKILL.md`                   |
|--------------------|---------------------------------------|------------------------------------|-----------------------------------------------|
| **Loads**          | Every request (full content)          | Every request (or path-filtered)   | Description at start, full content on trigger |
| **Best for**       | Project-wide constants, infra details | Always-on file-type conventions    | On-demand reference, workflows, commands      |
| **Selective**      | No — always full content              | Yes — `paths:` frontmatter filters | Yes — progressive disclosure                  |
| **Invocable**      | No                                    | No                                 | Yes — `/skill-name`                           |
| **Token cost**     | High (every request)                  | Medium (filtered)                  | Low (description only until used)             |
| **Editable scope** | Per-project                           | Per-project                        | Global (via symlinks) or per-project          |

**Examples:**

| Knowledge                                     | Where It Belongs                           | Why                                            |
|-----------------------------------------------|--------------------------------------------|------------------------------------------------|
| "Node 18, npm 10, Python 3.12"                | `CLAUDE.md`                                | Needed on every request for any task           |
| "Components use Options API, not Composition" | `CLAUDE.md`                                | Always-on framework constraint                 |
| "Use brackets for if/else in all JS files"    | `.claude/rules/` (with `paths: "**/*.js"`) | Only needed when touching JS files             |
| "All 47 spacing utility classes with values"  | Skill rule file                            | Only needed when working on styling            |
| "GDPR consent requirements by data category"  | Skill rule file                            | Only needed when working on privacy compliance |
| "How to create a PR with ticket integration"  | Skill (invocable via `/create-pr`)         | Only needed when creating PRs                  |
| "Component patterns discovered this session"  | Session context block                      | Not yet proven stable enough for a skill       |

---

## Correct vs. Incorrect Multi-Agent Examples

### Example 1: Agent-Agnostic Content

**Incorrect:** Embedding agent-specific tool names in rule content.
```markdown
# rules/data-validation.md
Use the `Read` tool to load the schema file, then use `Bash` to run validation.
```

**Correct:** Describing the action without agent-specific references.
```markdown
# rules/data-validation.md
Load the schema file and run validation against the input data.
```

**Why:** `Read` and `Bash` are Claude Code tool names. GPTel, Cursor, and other agents use different mechanisms. Skill content must work across all agents.

### Example 2: Knowledge Placement

**Incorrect:** Putting selectively-needed knowledge in CLAUDE.md.
```markdown
# CLAUDE.md
## All 47 Spacing Utility Classes
| Class | Value | Usage |
... (200 lines loaded on EVERY request)
```

**Correct:** Putting it in a skill rule file.
```markdown
# skills/css-utilities/rules/spacing-utilities.md
## All 47 Spacing Utility Classes
| Class | Value | Usage |
... (loaded only when styling tasks activate the skill)
```

**Why:** CLAUDE.md loads on every request. A 200-line reference table wastes tokens on 95% of tasks that don't need it. Skill rule files load on demand via progressive disclosure.

---

## Context Budget Management

If skills are being excluded due to context budget constraints:

1. **Check with `/context`** — See which skills are loaded vs excluded
2. **Increase budget:** Set `SLASH_COMMAND_TOOL_CHAR_BUDGET=25000` in environment
3. **Reduce SKILL.md size** — Move any content that leaked into SKILL.md back into rule files
4. **Improve description density** — Shorter descriptions with higher keyword density trigger better
5. **Split large skills** — If a skill's SKILL.md is near 500 lines, consider splitting into two skills
