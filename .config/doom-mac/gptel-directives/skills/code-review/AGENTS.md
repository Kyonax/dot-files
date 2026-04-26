# Code Review Skill — Architecture & Worker Protocol

## One Rule Per File

Each rule is a standalone `.md` file. Workers load only the rules they need.

## Directory = Worker Boundary

Each subdirectory maps to one potential worker category. The dispatcher lists directories, groups small ones (min 5 rules), and assigns one worker per group. No cross-directory dependencies — workers are fully isolated.

## Three-Tier Detection Cascade

```
brand (git remote)  →  project (file paths)  →  tech-stack (package.json)
        ↓                      ↓                         ↓
  brand/kyonax/         project/mr-dotcom/        framework/vue3/
  (or none)             (or none)                 (or none)
        ↓                      ↓                         ↓
  ┌─────────────────────────────────────────────────────────┐
  │  universal/** (always)  +  matched tiers  =  rule set   │
  └─────────────────────────────────────────────────────────┘
```

## Rule File Format

Every rule file uses this frontmatter + body structure:

```markdown
---
id: rule-{tier}-{category}-{number}
title: Short Descriptive Title
severity: CRITICAL | HIGH | MEDIUM | LOW
tags: comma, separated, code-greppable, keywords
---

One-sentence rule statement.

### Apply
- When to check this rule (2-3 bullets)

### Skip
- When this rule does NOT apply (1-2 bullets)

### Bad
code example showing violation

### Good
code example showing correct implementation

### Edge
Non-obvious scenario with explanation (1-2 sentences)
```

**Tag authoring:** Tags must be greppable against code diffs — use CSS properties (`aria-labelledby`, `display-flex`), HTML attributes (`role`, `tabindex`), directives (`v-if`, `v-for`), function names (`trackEvent`, `dispatch`).

## Adding a New Rule

1. Create `{tier}/{category}/rule-name.md` following the template
2. Add row to `{tier}/{category}/INDEX.md`
3. Add row to `SKILL.md` catalog table
4. Ensure `tags` field has ≥2 code-greppable keywords

## Adding a New Project

1. Create `project/{name}/` with subdirectories per category
2. Add detection signal to `detection/project.md`
3. Write `INDEX.md` per subdirectory
4. Add project to `SKILL.md` detection table

## Adding a New Brand

1. Create `brand/{name}/` with rule files
2. Add detection signal to `detection/brand.md`
3. Write `brand/{name}/INDEX.md`

## Adding a New Framework

1. Create `framework/{name}/` with rule files
2. Add detection signal to `detection/tech-stack.md`
3. Write `framework/{name}/INDEX.md`

## Worker Protocol

Each worker receives (assembled by `worker-prompt-builder.sh`):
1. `INDEX.md` — category context + instructions
2. Rule files — only the tag-matched ones
3. **Targeted code sections** — only the relevant SFC section(s) from `sfc-split.sh`, NOT full files
4. **Cross-reference hints** — one-line summaries from `variable-crossref.sh` (e.g., "template uses: [signedInDuringSession, googleLoading]")
5. **Already-flagged list** — resolved/pending items from `pr-review-digest.sh` for the worker's file scope
6. **Filtered diff hunks** — only hunks within the worker's section line ranges

Worker output format (YAML per finding):
```yaml
- rule: rule-u-ada-004
  file: InfoPage.vue
  line: 20
  severity: HIGH
  problem: Heading skip h1 → h3
  before: "h3.title Contact Info"
  after: "h2.title Contact Info"
```

Return `NO VIOLATIONS` if clean.

## Pre-AI Pipeline (shell scripts)

Run these BEFORE launching AI workers:

```
pr-fetch.sh --full-body → raw PR data
  ↓
pr-review-digest.sh → classified comments (resolved/pending/dismissed)
sfc-split.sh → SFC sections (template, script, style separately)
variable-crossref.sh → template↔script usage map
  ↓
worker-prompt-builder.sh → per-worker targeted context JSON
```

## Post-AI Pipeline (shell scripts)

Run these AFTER workers complete:

```
findings-dedup.sh → cross-worker merge + digest filtering
  ↓
format-findings.sh → sorted, formatted markdown for presenter
```

## Model Routing

| Role | Model |
|---|---|
| Dispatcher | Opus |
| Workers | Sonnet |
| Presenter | Opus |
