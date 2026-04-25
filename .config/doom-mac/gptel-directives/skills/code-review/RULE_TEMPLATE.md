# Rule File Template

Copy this template when creating a new rule. Keep the body under 200 tokens (excluding examples).

```markdown
---
id: rule-{tier}-{cat}-{NNN}
title: Short Descriptive Title
severity: CRITICAL | HIGH | MEDIUM | LOW
tags: keyword1, keyword2, keyword3
---

One-sentence rule statement. What must be true.

### Apply
- Condition when this rule applies
- Another condition

### Skip
- When this rule does NOT apply

### Bad
\`\`\`lang
code showing violation
\`\`\`

### Good
\`\`\`lang
code showing correct implementation
\`\`\`

### Edge
Non-obvious scenario. Explain in 1-2 sentences.
```

## ID Convention

`rule-{tier}-{category}-{number}`

| Tier prefix | Meaning |
|---|---|
| `u` | universal (always loaded) |
| `fw` | framework (Vue 3, Express, etc.) |
| `br` | brand (Kyonax, etc.) |
| `pj` | project (mr-dotcom, mr-backend, etc.) |

| Category examples | Prefix |
|---|---|
| ada | `ada` |
| code-style | `cs` |
| script | `sc` |
| mobile | `mob` |
| sdk | `sdk` |
| vue3 | `vue` |
| vue3-composition | `vca` |
| express | `exp` |
| kyonax | `kyo` |
| mr-dotcom | `mrd` |
| mr-backend | `mrb` |

Example: `rule-u-ada-004` = universal / ada / rule #4.

## Tag Guidelines

Tags are used by `select-rules.sh` for two-pass relevance filtering. They must be **code-greppable** — words that appear in actual source code diffs.

**Good tags:** `aria-labelledby`, `v-if`, `role-alert`, `focus-visible`, `display-flex`, `trackEvent`, `h1`, `h2`
**Bad tags:** `accessibility`, `performance`, `best-practice` (too abstract, won't match code)

Minimum 2 tags per rule. Maximum 8.

## Severity Guide

| Level | Meaning |
|---|---|
| CRITICAL | Breaks functionality, causes runtime errors, security vulnerability |
| HIGH | Degrades accessibility, performance, or user experience significantly |
| MEDIUM | Convention violation hurting maintainability or readability |
| LOW | Minor consistency issue, cosmetic |
