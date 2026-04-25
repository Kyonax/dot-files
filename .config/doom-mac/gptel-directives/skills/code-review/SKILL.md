---
name: code-review
description: "Code review and quality analysis for any web/JS project. Three-tier detection (brand → project → tech-stack) with 135 atomic rules (one per file). Parallel worker dispatch. Covers: ADA accessibility, code style, script patterns, mobile viewport, third-party SDK, Vue 3, Express, plus brand/project-specific rules. Trigger: 'review this code', 'code review', 'review PR #N', 'audit PR #N', 'check ADA', 'check styles', 'quality check'."
metadata:
  author: Kyonax
---

# Code Review Skill

Structural code quality analysis with three-tier detection (brand → project → tech-stack), atomic one-rule-per-file architecture, and parallel worker dispatch. Works on any web/JS project.

## When to Apply

- Reviewing Vue, React, Express, or any JS/TS web project
- Running pre-PR quality gates or post-implementation reviews
- Auditing ADA accessibility, mobile viewport, or third-party SDK integration
- Checking code style, naming, or architectural consistency

## How to Trigger

- "review this code" / "code review" / "review PR #123"
- "check ADA" / "check accessibility" / "check styles"
- "audit PR #123" (report mode — no code changes)

## Detection Flow (3 tiers)

```
1. Brand:     git remote → brand/kyonax/ or none
2. Project:   changed file paths → project/mr-dotcom/ or none
3. Tech-stack: package.json → framework/vue3/ or none
4. Always:    universal/** (loaded for every project)
```

See `detection/brand.md`, `detection/project.md`, `detection/tech-stack.md` for full signal tables.

## Review Modes

| Mode | Trigger | Flow |
|---|---|---|
| **Full review** | "code review", 3+ files | Scripts → CI gate → parallel workers → one-by-one resolution |
| **PR review** | "review PR #123" | Fetch PR diff → same flow as full review |
| **PR audit** | "audit PR #123" | Fetch PR → workers → markdown report (no code changes) |
| **Quick review** | "check this file" | Single worker inline, ~3000 tokens |
| **Scoped** | "check ADA only" | Load only the specified category |

## Rule Catalog

135 rules across 4 tiers, 15 directories.

### Universal (always loaded) — 45 rules

| Directory | Rules | Coverage |
|---|---|---|
| `universal/ada/` | 20 | Landmarks, headings, focus, ARIA, live regions, keyboard |
| `universal/code-style/` | 6 | Comments, spacing, JSDoc, minimal-touch |
| `universal/script/` | 9 | Unused vars, optional chaining, error domains, singletons |
| `universal/mobile/` | 5 | 100dvh, safe-area, scroll, overscroll |
| `universal/third-party-sdk/` | 5 | DOM scope, polling, cleanup, :deep(), ARIA conditional |

### Framework (loaded when detected) — 17 rules

| Directory | Rules | Detected by |
|---|---|---|
| `framework/vue3/` | 11 | `vue` in package.json |
| `framework/vue3-composition/` | 3 | `<script setup>` or Composition API imports |
| `framework/express/` | 3 | `express` in package.json |

### Brand (loaded when remote matches) — 21 rules

| Directory | Rules | Detected by |
|---|---|---|
| `brand/kyonax/` | 21 | Remote `Kyonax/*` — FPS discipline, OBS-WS, hot paths |

### Project (loaded when file paths match) — 52 rules

| Directory | Rules | Detected by |
|---|---|---|
| `project/mr-dotcom/template/` | 6 | Paths in `website/src/vuescripts/` |
| `project/mr-dotcom/styling/` | 13 | Paths in `website/src/vuescripts/` |
| `project/mr-dotcom/naming/` | 5 | Paths in `website/src/vuescripts/` |
| `project/mr-dotcom/images-tracking/` | 8 | Paths in `website/src/vuescripts/` |
| `project/mr-dotcom/components/` | 8 | Paths in `website/src/vuescripts/` |
| `project/mr-backend/` | 12 | Paths in `mr_modules/` |

## Worker Dispatch

Each directory = one potential worker. Small dirs merge (min 5 rules). Max 8 workers.

```
Pre-AI (zero-token):
  detect.sh → brand/project/stack
  select-rules.sh → filtered rule paths (two-pass: directory + tag matching)
  worker-dispatch.sh → worker assignments JSON
  sfc-split.sh → template/script/style sections per file
  variable-crossref.sh → template↔script cross-reference map
  pr-review-digest.sh → existing PR comments classified
  worker-prompt-builder.sh → per-worker targeted context JSON

AI workers (Sonnet, targeted context only):
  Each worker: INDEX.md + relevant rules + ONLY its section (not full SFC) → YAML findings

Post-AI (zero-token):
  findings-dedup.sh → cross-worker + digest de-duplication
  format-findings.sh → sorted, formatted markdown

AI presenter → one-by-one: implement or skip?
```

## Scripts

| Script | Purpose |
|---|---|
| `scripts/detect.sh` | 3-tier detection → JSON |
| `scripts/list-changed.sh` | Changed files with metadata → JSON |
| `scripts/pr-fetch.sh` | PR metadata + diff + existing reviews → JSON (`--full-body` for complete comment bodies) |
| `scripts/select-rules.sh` | Two-pass rule selection → file paths |
| `scripts/worker-dispatch.sh` | Rule grouping → worker assignments JSON |
| `scripts/diff-context.sh` | Structured diff + imports + store deps → JSON |
| `scripts/component-tree.sh` | Vue component hierarchy → JSON |
| `scripts/lint-changed.sh` | ESLint on changed files → JSON |
| `scripts/test-changed.sh` | Test runner on changed files → JSON |
| `scripts/ci-local.sh` | Discover + run CI workflows locally → JSON |
| `scripts/pr-review-digest.sh` | PR review comments → resolved/pending/dismissed digest JSON |
| `scripts/sfc-split.sh` | Vue SFC → template/script/style sections JSON |
| `scripts/variable-crossref.sh` | Vue SFC → template↔script cross-reference map JSON |
| `scripts/worker-prompt-builder.sh` | All pre-computed JSON → per-worker targeted context JSON |
| `scripts/findings-dedup.sh` | Raw YAML findings → de-duplicated YAML (cross-worker + digest) |
| `scripts/format-findings.sh` | De-duplicated YAML → formatted findings markdown |

## 6-Stage Review Flow

```
STAGE 0: PR FETCH   (shell, zero tokens) → pr.json + diff file
STAGE 1: DISCOVERY  (shell, zero tokens) → detection.json, files.json, context.json, sections.json, crossref.json, digest.json
STAGE 2: CI GATE    (shell, zero tokens) → fix deterministic issues first
STAGE 3: TRIAGE     (shell, zero tokens) → workers.json + per-worker targeted prompts
STAGE 4: AI REVIEW  (parallel Sonnet workers, targeted context only) → raw-findings.yaml
STAGE 5: FORMAT     (shell, zero tokens) → dedup against digest + sort → findings.md
STAGE 6: RESOLUTION (Opus presenter) → one-by-one implement/skip
```

## Output Format

```
Finding N/Total | Severity | Rule ID | File:Line

Problem: one sentence.

Before:
  L<num>: code

After:
  L<num>: fixed code

Implement or skip?
```

## Severity Levels

| Level | Meaning |
|---|---|
| CRITICAL | Breaks functionality, runtime errors, security |
| HIGH | Degrades accessibility, performance, UX |
| MEDIUM | Convention violation, maintainability |
| LOW | Minor consistency |

## Architecture

## Token Budget (PR Review)

| Component | Without Scripts | With Scripts | Savings |
|---|---|---|---|
| PR comments | ~15K (raw) | ~2K (digest) | ~13K |
| SFC code per worker | ~20K (full) | ~7K (split) | ~13K |
| Cross-ref false positives | ~3K (re-review) | 0K | ~3K |
| Worker context assembly | ~15K (all data) | ~5K (targeted) | ~10K |
| Duplicate findings | ~2K | 0K | ~2K |
| **Total** | **~69K** | **~24-28K** | **~41K (59-65%)** |

## Architecture

See `AGENTS.md` for: why one-rule-per-file, worker protocol, token optimization architecture, how to add rules/projects/brands, model routing.
See `RULE_TEMPLATE.md` for: rule file format, ID convention, tag guidelines, severity guide.
