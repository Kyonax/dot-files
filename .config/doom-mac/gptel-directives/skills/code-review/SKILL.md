---
name: code-review
description: "Code review, code quality analysis, style consistency audit, and architectural compliance check. Analyzes source code files against established patterns, design principles, and best practices. Detects architecture violations, performance issues, accessibility gaps, testability problems, styling inconsistencies, naming convention breaks, and unused code. Supports any language or framework — performs a context detection stage to identify the tech stack and load relevant domain skills (e.g., mr-dotcom-dev for Vue/Pug/Stylus/Vuex). Trigger when: user says 'review this code', 'code review', 'review PR', 'check quality', 'audit code', 'review component', 'style check', 'consistency check', 'find issues', or any variation of reviewing, auditing, or checking code quality. Also trigger when reviewing pull requests, checking ADA/accessibility compliance, or evaluating code before merging."
metadata:
  author: Kyonax
  version: "1.0.0"
---

# Code Review Skill

Structured code quality analysis that goes beyond linting — enforcing architectural patterns, design principles, performance best practices, accessibility, and testability.

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
