---
name: code-review
description: "Code review, code quality analysis, style consistency audit, and architectural compliance check. Analyzes source code files against established patterns, design principles, and best practices. Detects architecture violations, performance issues, accessibility gaps, testability problems, styling inconsistencies, naming convention breaks, and unused code. Supports any language or framework — performs a context detection stage to identify the tech stack and load relevant domain skills (e.g., mr-dotcom-dev for Vue/Pug/Stylus/Vuex). Trigger when: user says 'review this code', 'code review', 'review PR', 'check quality', 'audit code', 'review component', 'style check', 'consistency check', 'find issues', or any variation of reviewing, auditing, or checking code quality. Also trigger when reviewing pull requests, checking ADA/accessibility compliance, or evaluating code before merging."
metadata:
  author: Kyonax
  version: "1.0.0"
---

# Code Review Skill

Structured code quality analysis that goes beyond linting — enforcing architectural patterns, design principles, performance best practices, accessibility, and testability.

## Quick Reference — Project-Specific Checklists

| Rule File | Scope | When to Load |
|---|---|---|
| `rules/mr-review-checklist.md` | 45-rule checklist + 22 Andris review patterns for Madison Reed Vue components | When reviewing any file in `website/src/vuescripts/` |

---

## Parallel Subagent Review Flow (Validated 2026-04-03)

When reviewing multiple files against a large checklist (e.g., the 45-rule MR checklist + 22 Andris patterns), use this parallel subagent architecture for thoroughness and efficiency. Validated on DOTCOMPB-7712 (6 files, 67 rules, 2 full rounds).

### Step 1: Present the Checklist

Before launching subagents, show the user the FULL rule table so they can see what's being checked. Load `rules/mr-review-checklist.md` and display both tables (45 rules + 22 Andris patterns).

### Step 2: Group Rules into Categories

Split the checklist into 8 focused categories. Each category becomes one subagent:

| # | Category | Rules | Focus |
|---|---|---|---|
| 1 | **Template** | Rules 1-4, andris-6, andris-7 | Pug, headings, v-if guards, h1 per page, no redundant div |
| 2 | **Script Structure** | Rules 5-13 | Options API, field order, computed alpha, Vuex helpers, constants, optional chaining, no inline logic |
| 3 | **Styling** | Rules 14-21, andris-14, andris-16 | Scoped Stylus, utility-first, flex in Stylus only, max-at-tweak, design vars, CSS alpha, nesting, third-party comments |
| 4 | **Naming** | Rules 22-26 | Root prefix, redundant prefixes, card children, grid columns, no BEM |
| 5 | **ADA Accessibility** | Rules 27-33, andris-1 | Self-contained landmarks, aria-hidden, role=list, heading levels, focus-visible, nested interactives, duplicate landmarks, alt text |
| 6 | **Images + Tracking** | Rules 34-38, andris-9, andris-13 | ImgBox, skeleton, :deep(img), trackMREvent vs trackMREventAndRedirect, method traceability |
| 7 | **Code Style** | Rules 39-41, andris-2-5, andris-15, andris-20 | Blank lines, no info comments, minimal-touch, traceable methods, one-line expressions, clean strings |
| 8 | **MrBtn + Components** | Rules 42-45, andris-8, andris-11, andris-12, andris-22 | MrBtn variants, :deep(.mrbtn), shared level, thin wrapper, no hardcoded routes, extract don't bloat |

### Step 3: Launch All 8 Subagents in Parallel

Each subagent receives:
- The specific rules it's checking (full descriptions, not just numbers)
- The list of ALL file paths to review
- The output format (YAML per violation with rule, file, location, severity, problem, before/after)
- Instruction: return `NO VIOLATIONS` if clean

All 8 launch in a single message as background agents. They run simultaneously, each reading all files but checking only its assigned rules.

### Step 4: Collect Results and Build Summary Table

As subagents complete, build a running tally:

```
| Category | Result |
|---|---|
| Template | NO VIOLATIONS |
| Script | 3 findings (1 MEDIUM, 2 LOW) |
| Styling | 7 findings (2 HIGH, 3 MEDIUM, 2 LOW) |
| ...etc |
```

### Step 5: Interactive One-by-One Resolution

Present findings **one at a time**, ordered by severity (CRITICAL → HIGH → MEDIUM → LOW). For each:

1. Show the rule, file, line, severity, and problem
2. Note if pre-existing (not introduced by the current work) — reference andris-5 (minimal-touch)
3. Ask: "Implement or skip?"
4. If implement → apply the fix immediately
5. If skip → move to next
6. After implementing, verify related items (e.g., if fixing alphabetization in one file, verify all files)

### Step 6: Final Test Run

After all findings are resolved, run the full test suite to confirm no regressions.

### Performance Notes

- **Round 1** (DOTCOMPB-7712, 2026-04-03): 6 files, 30 findings total, 5 implemented, 25 skipped. Tests: 75 passing.
- **Round 2** (same session, post-fixes): 6 files, 18 findings total (reduced from 30 due to round 1 fixes), 7 implemented, 10 skipped. Tests: 81 passing.
- **Total execution time**: ~3-5 minutes per round (8 subagents in parallel). Much faster than sequential single-agent review.
- **Key benefit**: Each subagent focuses on 4-12 rules only, producing zero false positives from rule confusion. Sequential reviews of 67 rules in one pass tend to miss edge cases or misapply rules.

### When to Use This Flow

- Reviewing 3+ files against 20+ rules
- Pre-PR comprehensive review
- Post-implementation quality gate
- When the user explicitly asks for a "full" or "extensive" code review

For quick reviews (1-2 files, specific category), use the standard 3-stage pipeline instead.

---

## Execution Flow

Every code review follows a **three-stage pipeline**. Do not skip stages.

### Stage 1: Context Detection

Before reviewing any code, identify the project context. This determines which rules apply.

**Analyze the code to determine:**

1. **Language(s):** JavaScript, TypeScript, Python, HTML, CSS, etc.
2. **Framework:** Vue, React, Angular, Express, etc.
3. **Template engine:** Pug, JSX, Handlebars, plain HTML, etc.
4. **Style system:** Stylus, SCSS, CSS, Tailwind, utility-first, etc.
5. **State management:** Vuex, Pinia, Redux, etc.
6. **Test framework:** Vitest, Jest, Mocha, etc.
7. **Project context:** File path, directory structure, imports

**Skill cross-referencing:**

After identifying the tech stack, check if any loaded skills provide domain-specific rules for that stack. This is critical — generic review catches generic issues, but domain skills catch project-specific violations.

| If you detect... | Load and apply rules from... |
|---|---|
| Vue 3 + Options API + Pug + Stylus + Vuex | `mr-dotcom-dev` (full skill — vue-patterns, styling rules, testing-standards) |
| Vue 3 + Composition API | Generic Vue best practices (no MR-specific rules apply) |
| Express routes / Node.js server code | `mr-dotcom-dev/rules/express-routing.md` if in MR monorepo |
| Test files (`.test.js`, `.spec.js`) | `mr-dotcom-dev/rules/testing-standards.md` if in MR monorepo |
| Dynamic Yield templates | `mr-dotcom-dev/rules/dynamic-yield.md` |
| SEO / HTML quality | `seo-web-quality` skill if available |

**How to detect MR monorepo context:**
- File path contains `website/src/vuescripts/` or `mr_modules/` or `website/src/routing/`
- Imports use `@components`, `@services`, `@store`, `@utilities` aliases
- Component uses `trackMREvent`, `mapState('colorbar',...)`, or similar MR patterns
- Template uses `lang="pug"` with Stylus scoped styles

**Output of Stage 1:** A brief statement of detected context, listed before the review findings:

```yaml
# Context Detection
- language: JavaScript (Vue 3 SFC)
- framework: Vue 3 Options API
- template: Pug
- styles: Scoped Stylus
- state: Vuex 4 (colorbar module)
- project: Madison Reed website (mr-dotcom-dev rules apply)
- skills_loaded: [mr-dotcom-dev/vue-patterns, mr-dotcom-dev/testing-standards]
```

---

### Stage 2: Analysis

With context established, analyze the code against ALL applicable rule categories. Work through each category systematically — do not cherry-pick.

#### Universal Categories (Always Apply)

These apply to ANY codebase regardless of framework:

**Architecture & Design (CRITICAL)**
- Single Responsibility — each function/method/component does one thing
- Proper data flow — parent prepares data, children receive clean props
- Feature abstraction — complex features with internal state must be extracted
- No god objects — components/modules with too many responsibilities

**Performance & Efficiency (HIGH)**
- No unnecessary re-computation — cache computed values
- No expensive operations in render paths
- Efficient event handling — debounce/throttle where needed, prefer modern APIs
- Module-level constants for static data

**Testability & Maintainability (HIGH)**
- No inline complex logic in templates/event handlers
- No magic numbers — extract named constants
- DRY — no repeated logic (extract to methods/functions)
- No unused code — dead variables, methods, imports
- Modern syntax — optional chaining over utility wrappers

**Accessibility (HIGH)**
- Semantic HTML elements over generic containers
- ARIA roles and attributes on interactive elements
- Keyboard navigability on non-native interactive elements
- No nested interactive elements (e.g., `<a>` inside `role="button"`)
- Native focusable elements for actions (buttons, not styled divs/anchors without href)

**Code Style & Consistency (LOW)**
- Consistent naming conventions across the file
- Consistent formatting (indentation, ordering)
- File structure follows established patterns

#### Domain-Specific Categories (From Loaded Skills)

When a domain skill is detected in Stage 1, add its specific rules. For example, when `mr-dotcom-dev` applies:

**MR Architecture (CRITICAL)**
- Options API mandate — no Composition API or `<script setup>`
- Canonical field order: `name → components → emits → props → data → computed → watch → lifecycle → methods`
- Parent-child communication via custom events, not `@hook:` listeners
- `:deep()` only for simple targeted overrides, prefer props API
- Modal system via Vuex dispatch, never `v-if`
- CMS data validation in parent computed (filter entries without valid `.image.url`)

**MR Performance (HIGH)**
- `window.matchMedia` over `window.resize` for responsive detection
- matchMedia stored in `data`/`mounted`, not module-level (SSR safety)
- Module-level constants for all numeric literals and static config

**MR Testability (HIGH)**
- No inline event handlers with computed arguments
- Methods must be extractable and directly testable
- No snapshot tests (`toMatchSnapshot` forbidden)

**MR Accessibility (HIGH)**
- `MrBtn` uses native `<button>` — no `tag="a"` without valid `href`
- No nested interactive elements — use `<div aria-hidden="true">` for decorative overlays
- `trackMREvent` / `trackMREventAndRedirect` for all user interactions
- `v-if` guards on all `ImgBox` where data may be null (skeleton pattern)

**MR Styling (MEDIUM)**
- Pug templates + scoped Stylus mandatory
- Utility-first — scoped styles only for structural/pseudo/transitions
- Revised unit convention: `rem`/`em` for spacing, `px` for small fixed elements (borders)
- Design system variables — no hardcoded hex colors
- Breakpoint mixins preferred; keep hardcoded `px` if no exact variable exists
- Alphabetize CSS properties and computed properties

---

### Stage 3: Report

Compile findings into the structured output format.

#### Output Format

Return findings as a YAML array. Each issue follows this structure:

```yaml
- category: "[Category Name]"
  severity: CRITICAL | HIGH | MEDIUM | LOW
  location: "Lstart–Lend"
  problem: "One-sentence summary of the issue."
  reasoning: "Why this violates the guidelines. Reference the specific principle."
  before: |
    ```[language]
    # Minimal snippet: problematic line(s) + 2 context lines before/after
    # Prefix each line with L<number>:
    L21: mounted() {
    L22:   this.checkIsMobile();
    L23:   window.addEventListener('resize', this.checkIsMobile); // ← Issue
    L24: },
    ```
  after: |
    ```[language]
    # Corrected code snippet
    mounted() {
      this.mobileMediaQuery = window.matchMedia('(max-width: 559px)');
      this.handleMediaQueryChange(this.mobileMediaQuery);
      this.mobileMediaQuery.addEventListener('change', this.handleMediaQueryChange);
    },
    ```
```

#### Severity Guide

| Severity | Meaning | Examples |
|---|---|---|
| **CRITICAL** | Breaks architecture, creates bugs, or causes runtime failures | Wrong API style, nested interactives, `@hook:mounted`, missing SSR guard |
| **HIGH** | Degrades performance, accessibility, or testability significantly | `window.resize` instead of `matchMedia`, inline event logic, missing ARIA, no `v-if` guard |
| **MEDIUM** | Style/convention violation that hurts maintainability | Wrong unit convention, missing utility class, hardcoded hex color |
| **LOW** | Minor consistency issue | Alphabetization, naming convention, missing curly braces |

#### Ordering

Report issues in severity order: CRITICAL first, then HIGH, MEDIUM, LOW.

#### No Issues Found

If the analysis finds zero violations, return ONLY:

```r
 _   _        ____             __ _ _      _
| \ | | ___  / ___|___  _ __  / _| (_) ___| |_ ___
|  \| |/ _ \| |   / _ \| '_ \| |_| | |/ __| __/ __|
| |\  | (_) | |__| (_) | | | |  _| | | (__| |_\__ \
|_| \_|\___/ \____\___/|_| |_|_| |_|_|\___|\__|___/
```

---

## Interactive Mode

When triggered from an interactive conversation (Claude Code, GPTel chat), present findings **one at a time** for user approval:

1. Show the first issue with full before/after
2. Ask: "Implement this fix, or skip?"
3. If implement → apply the fix, then show the next issue
4. If skip → move to the next issue
5. Continue until all issues are reviewed

This prevents overwhelming the user and gives them control over each change.

---

## Review Scope Control

The user may limit the review scope. Respect these constraints:

| User says... | Review scope |
|---|---|
| "Review this component" | Full review — all categories |
| "Check ADA / accessibility" | Only Accessibility category |
| "Check styles / styling" | Only Styling category |
| "Check performance" | Only Performance category |
| "Quick review" | CRITICAL and HIGH only, skip MEDIUM/LOW |
| "Full review" | All categories, all severities |

---

## Quality Principles

1. **Report only genuine issues.** Do not flag correct code. If you're unsure whether something is a violation, err on the side of not reporting.
2. **One report per issue.** If multiple nearby lines share the same root problem, group them.
3. **Always include context lines.** Show 2 lines before and after the problematic code.
4. **Prefix lines with real line numbers.** Use `L<number>:` format from the actual file.
5. **Provide actionable fixes.** Every `after` block must be copy-paste ready.
6. **Don't report what linters catch.** Focus on architectural, design, and semantic issues — not missing semicolons or trailing whitespace.
7. **Respect project conventions.** If the project has established patterns (even if they differ from "best practice"), enforce consistency with those patterns rather than imposing external standards.
