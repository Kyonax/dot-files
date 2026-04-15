---
name: code-review
description: "Code review and quality analysis. Detects architecture violations, performance issues, accessibility gaps, styling inconsistencies, naming breaks, and unused code. Auto-detects tech stack and loads domain skills (e.g., mr-dotcom-dev for Vue/Pug/Stylus/Vuex). Supports parallel subagent review for large rulesets. Trigger: 'review this code', 'code review', 'review PR', 'check quality', 'audit code', 'style check', 'find issues', or any code quality/accessibility audit request."
metadata:
  author: Kyonax
  version: "2.0.0"
---

# Code Review Skill

Structural code quality analysis beyond linting — architectural patterns, design principles, performance, accessibility, testability, and project-specific conventions.

## When to Apply

Reference these guidelines when:

*   Reviewing Vue components, Pug templates, Stylus styles, or Vuex stores in the MR website
*   Running a pre-PR quality gate or post-implementation review
*   Checking ADA/accessibility compliance on any web component
*   Auditing code style, naming, or architectural consistency
*   Evaluating backend Express routes, controllers, or webservices

## When to Read Which Rules

| If reviewing...                                | Read                                                            |
|------------------------------------------------|-----------------------------------------------------------------|
| Any file in `website/src/vuescripts/`          | `rules/mr-review-checklist.md` (82 rules, 9 categories)         |
| Express routes or controllers in `mr_modules/` | `rules/mr-review-checklist.md` § Backend / API only             |
| Non-MR codebase                                | Use universal categories in Stage 2 below (no rule file needed) |

## Quick Reference

| Rule File                      | Description                                                              |
|--------------------------------|--------------------------------------------------------------------------|
| `rules/mr-review-checklist.md` | 82-rule MR Vue checklist (9 categories). [Details](#mr-review-checklist) |

#### mr-review-checklist

45 core + 22 AD (senior reviewer) + 14 SG (session-graduated) rules. 9 categories: Template, Script, Styling, Naming, ADA, Images+Tracking, Code Style, MrBtn+Components, Backend/API. Each category has inline examples for complex rules. Maps to the 8-category parallel subagent flow.

---

## Review Modes

| User Request                                 | Mode                  | Flow                                                             |
|----------------------------------------------|-----------------------|------------------------------------------------------------------|
| "Full review", "code review", 3+ files       | **Parallel subagent** | 8 category agents in parallel → summary → interactive resolution |
| "Quick review", 1-2 files, specific category | **Standard pipeline** | Context detection → analysis → report                            |
| "Check ADA only", "check styles"             | **Scoped pipeline**   | Context detection → single category → report                     |

---

## Parallel Subagent Review Flow

Use when reviewing 3+ files against the full checklist (82 rules). Each subagent focuses on 4-15 rules, eliminating false positives from rule confusion. Execution: ~3-5 min per round.

### Step 1: Show Checklist

Load `rules/mr-review-checklist.md`. Display the Categories table so the user sees what's being checked.

### Step 2: Launch 8 Subagents in Parallel

Split rules into 8 categories. Launch ALL in a single message as parallel agents.

| # | Category           | Rules                             |
|---|--------------------|-----------------------------------|
| 1 | Template           | 1-4, sg-1, ad-6, ad-7             |
| 2 | Script Structure   | 5-13, sg-2 to sg-5, ad-4, ad-15, ad-23, ad-24 |
| 3 | Styling            | 14-21, sg-6 to sg-8, ad-14, ad-16 |
| 4 | Naming             | 22-26                             |
| 5 | ADA Accessibility  | 27-33, sg-9 to sg-12, ad-1, ad-17 |
| 6 | Images + Tracking  | 34-38, sg-13, ad-9, ad-13         |
| 7 | Code Style         | 39-41, ad-4, ad-15, ad-20         |
| 8 | MrBtn + Components | 42-45, sg-14, ad-11, ad-12, ad-22 |

**Each subagent receives:**
- Full rule descriptions for its category (not just numbers)
- ALL file paths to review
- Output format: YAML per finding (rule, file, line, severity, problem, before/after code)
- Instruction: return `NO VIOLATIONS` if clean

### Step 3: Collect Results → Summary Table

As subagents complete, build a tally:

```
| Category | Result |
|---|---|
| Template | NO VIOLATIONS |
| Script | 3 findings (1 MEDIUM, 2 LOW) |
| Styling | 7 findings (2 HIGH, 3 MEDIUM, 2 LOW) |
```

### Step 4: Interactive Resolution (One-by-One)

**MANDATORY: Present findings ONE AT A TIME.** Never batch. Never show the next finding until the user responds. Sort by severity (CRITICAL → LOW).

**For each finding, show this exact structure:**

---

**Finding N/Total** | **Severity** | **Rule** | **File:Line**

**Problem:** One-sentence description.

**Before:**
```lang
L<num>: context line above
L<num>: problematic line(s)
L<num>: context line below
```

**After:**
```lang
corrected code (copy-paste ready)
```

> Pre-existing? If yes, flag: "This is pre-existing code (ad-5: minimal-touch). Skip recommended."

**Implement or skip?**

---

**Flow:**
1. Show finding with before/after code blocks (real line numbers, 2 context lines)
2. Wait for user response
3. If **implement** → apply the fix immediately using Edit tool, then show next finding
4. If **skip** → show next finding
5. Repeat until all findings resolved

### Step 5: Test Run

After all findings resolved, run the full test suite. Confirm no regressions.

---

## Standard Pipeline (Quick Reviews)

Three stages, never skip any.

### Stage 1: Context Detection

Identify tech stack from the code:

- Language, framework, template engine, style system, state management, test framework
- **MR monorepo indicators:** path contains `website/src/vuescripts/` or `mr_modules/`, imports use `@components`/`@store` aliases, uses `trackMREvent`, `lang="pug"` + scoped Stylus

**Load domain skills based on detection:**

| Detection                                      | Load                                                          |
|------------------------------------------------|---------------------------------------------------------------|
| Vue 3 + Options API + Pug + Stylus + Vuex (MR) | `mr-dotcom-dev` (full skill) + `rules/mr-review-checklist.md` |
| Express / Node.js in MR monorepo               | `mr-dotcom-dev/rules/express-routing.md`                      |
| Test files in MR monorepo                      | `mr-dotcom-dev/rules/testing-standards.md`                    |

### Stage 2: Analysis

Review against ALL applicable categories. Work systematically — do not cherry-pick.

**Universal (any codebase):**
- **Architecture (CRITICAL):** Single responsibility, proper data flow, feature abstraction, no god objects
- **Performance (HIGH):** No unnecessary re-computation, efficient event handling, module-level constants
- **Testability (HIGH):** No inline complex logic, no magic numbers, DRY, no unused code, modern syntax
- **Accessibility (HIGH):** Semantic HTML, ARIA attributes, keyboard navigability, no nested interactives
- **Code Style (LOW):** Consistent naming, formatting, file structure

**Domain-specific:** Apply loaded skill rules (e.g., MR checklist — 82 rules across 9 categories).

### Stage 3: Report + Interactive Resolution

**Always output a summary table first**, then resolve findings one-by-one.

**Step A — Summary table:**

| # | Severity | Rule | File             | Location | Problem                                            |
|---|----------|------|------------------|----------|----------------------------------------------------|
| 1 | CRITICAL | sg-4 | HeroV2.vue       | L21-L23  | Uses `window.resize` instead of `matchMedia`       |
| 2 | HIGH     | 27   | AboutSection.vue | L5       | `aria-labelledby` on parent wrapper without `role` |

**Step B — One-by-one resolution (same flow as Step 4 of parallel review):**

Present each finding with before/after code blocks. Wait for user response before proceeding.

---

**Finding 1/2** | **CRITICAL** | **sg-4** | **HeroV2.vue:L21-L23**

**Problem:** Uses `window.addEventListener('resize')` for responsive detection.

**Before:**
```javascript
L20: mounted() {
L21:   window.addEventListener('resize', this.checkWidth);
L22: },
```

**After:**
```javascript
mounted() {
  this.mobileQuery = window.matchMedia('(max-width: 559px)');
  this.mobileQuery.addEventListener('change', this.onMediaChange);
},
```

**Implement or skip?**

---

**Severity levels:**

| Level    | Meaning                                             |
|----------|-----------------------------------------------------|
| CRITICAL | Breaks architecture, causes bugs, runtime failures  |
| HIGH     | Degrades performance, accessibility, or testability |
| MEDIUM   | Convention violation hurting maintainability        |
| LOW      | Minor consistency issue                             |

**No issues found:** Return the `No Conflicts` ASCII art banner.

---

## Quality Principles

1. **One finding at a time** — NEVER batch findings. Show one, wait for response, then next. This is the most important UX rule
2. **Before/after code blocks on every finding** — always show the actual code diff with real line numbers
3. **"Implement or skip?"** — always end each finding with this question. No other options
4. Report only genuine issues — when unsure, don't flag
5. One report per root cause — group nearby same-issue lines
6. Include 2 context lines before/after with real `L<number>:` prefixes
7. Every `after` block must be copy-paste ready
8. Don't report what linters catch — focus on architectural/semantic issues
9. Respect project conventions over external "best practice"
