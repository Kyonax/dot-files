---
name: code-review
description: "Code review and quality analysis. Detects architecture violations, performance issues, accessibility gaps, styling inconsistencies, naming breaks, and unused code. Auto-detects brand (Kyonax/RECKIT/OBS vs MadisonReed vs generic) AND tech stack (Vue/Pug/Stylus/Vuex), loading matching rule files — e.g., brand-kyonax.md for OBS Browser-Source FPS discipline, mr-review-checklist.md for MR Vue conventions. Supports parallel subagent review for large rulesets. Trigger: 'review this code', 'code review', 'review PR', 'check quality', 'audit code', 'style check', 'find issues', 'RECKIT review', 'OBS HUD perf', or any code quality/accessibility audit request."
metadata:
  author: Kyonax
  version: "2.1.0"
---

# Code Review Skill

Structural code quality analysis beyond linting — architectural patterns, design principles, performance, accessibility, testability, and brand-scoped + tech-stack-scoped conventions. Brand detection runs first (OBS FPS discipline for Kyonax, no brand rule for MadisonReed, conservative fallback otherwise); tech-stack domain detection runs second. Both sets of rules can apply to the same review.

## When to Apply

Reference these guidelines when:

*   Reviewing any Vue component, JS module, or SCSS file in a Kyonax / RECKIT / OBS Browser Source codebase
*   Reviewing Vue components, Pug templates, Stylus styles, or Vuex stores in the MR website
*   Running a pre-PR quality gate or post-implementation review
*   Checking ADA/accessibility compliance on any web component
*   Auditing code style, naming, or architectural consistency
*   Evaluating backend Express routes, controllers, or webservices
*   Auditing HUD overlays, widgets, or composables that drive OBS WebSocket subscriptions for FPS-regression risk

## When to Read Which Rules

**Step 0 — Always first: brand detection.** Load `rules/brand-detection.md` on every review to identify the owning brand, then load the matching `brand-<name>.md` (if any). Brand rules and domain rules both apply — they address orthogonal axes.

| If reviewing...                                                                                | Read                                                                                                                             |
|------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------|
| **Any review (Step 0 — brand detection)**                                                      | `rules/brand-detection.md`                                                                                                       |
| Any Kyonax-owned repo (remote `Kyonax/*`, or `@<brand>/` folders, or `use-obs-websocket.js`)   | `rules/brand-kyonax.md` + any tech-stack rule matched below                                                                      |
| HUD overlays or OBS-WS composables specifically                                                | `rules/brand-kyonax.md` (Sections A–F are the FPS budget)                                                                        |
| Any file in `website/src/vuescripts/`                                                          | `rules/mr-review-checklist.md` (82 rules, 9 categories) — MR has no brand rule                                                   |
| Express routes or controllers in `mr_modules/`                                                 | `rules/mr-review-checklist.md` § Backend / API only                                                                              |
| Non-Kyonax, non-MR codebase                                                                    | Generic fallback per `rules/brand-detection.md` + universal categories in Stage 2 below                                          |

## Quick Reference

| Rule File                      | Description                                                                                                                                  |
|--------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------|
| `rules/brand-detection.md`     | Brand auto-identification from git remote + user override + repo-local indicators. Always Step 0. [Details](#brand-detection)                |
| `rules/brand-kyonax.md`        | Kyonax/RECKIT/OBS review discipline — OBS FPS budget, opt-in CSS effects, singleton composables, hot-path allocation rules. [Details](#brand-kyonax) |
| `rules/mr-review-checklist.md` | 82-rule MR Vue checklist (9 categories). [Details](#mr-review-checklist)                                                                     |

#### brand-detection

Detection signals in order: (1) `git remote get-url origin` → match `Kyonax/*` → `brand-kyonax.md`, `MadisonReed/*` → no brand rule (tech-stack only), anything else → generic fallback; (2) explicit user override ("apply RECKIT rules", "generic only"); (3) repo-local indicators (`@<brand>/` folders, `sources.js` with hud/animation/scene types, OBS-WS composables, Tier 1 headers). Precedence: brand rules and domain rules are orthogonal and both apply when relevant. Ends with "correct vs incorrect" detection examples.

#### brand-kyonax

Kyonax/RECKIT/OBS Browser Source review discipline — reverse-engineered from a real FPS regression (the `cyberpunk-glow` mixin applied broadly to gold-text tanked OBS fps on low-core hardware). Seven rule sections: (A) CSS cost — never broadcast `filter`/`box-shadow`/`text-shadow` via utility classes, opt-in via `--hud-halo`/`--hud-glow` tokens, animate only `transform`/`opacity`, split static-from-animated onto separate layers, `contain: layout paint` on HUD sub-trees; (B) OBS WebSocket call budget — every WS composable is a module-level singleton with identity test and no `onUnmounted` cleanup, event-driven over rAF, throttle emits; (C) Zero-allocation hot path — preallocated `Float32Array`, precomputed lookup tables (`JITTER_TABLE`, `SCALE_STRINGS`), classic `for` loops, hardcode known targets; (D) Vue reactivity boundary — bypass reactivity in per-frame hot paths via template refs + direct DOM writes, write-threshold skip, expose a `tick` counter; (E) Event listener hygiene — debounce burst-prone `window.*` listeners, clear timers on unmount; (F) Pre-merge checklist with 8 questions; (G) General Kyonax conventions (kebab-case emits, snake_case props, kebab-case filenames, Vite aliases, no relative imports, colors in SCSS not JS, no-git-write discipline). Severity calibration: FPS regression risk = CRITICAL.

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

Four stages, never skip any. Brand detection is always Stage 0.

### Stage 0: Brand Detection

Before any other analysis, identify the owning brand. Load `rules/brand-detection.md` and apply its signals:

1. `git remote get-url origin` → match against brand catalog.
2. Explicit user override (e.g., "apply RECKIT rules", "skip brand rules").
3. Repo-local indicators (`@<brand>/` folders, OBS-WS composables, Tier 1 headers).

Record the detected brand in internal working state. Load the matching `brand-<name>.md` if any:

| Detected brand | Load |
|---|---|
| Kyonax (RECKIT, OBS overlays, any `Kyonax/*` repo) | `rules/brand-kyonax.md` |
| MadisonReed | No brand rule — proceed directly to Stage 1 tech-stack detection |
| Generic fallback | No brand rule — universal categories only |

**Brand rules do not replace tech-stack domain rules — both apply when relevant.** A Kyonax Vue 3 HUD file loads `brand-kyonax.md` for perf discipline AND whatever Vue 3 conventions the reviewer knows. A MadisonReed Vue+Pug file loads `mr-review-checklist.md` for tech-stack conventions with no brand overlay.

### Stage 1: Tech-Stack Context Detection

Identify tech stack from the code:

- Language, framework, template engine, style system, state management, test framework
- **MR monorepo indicators:** path contains `website/src/vuescripts/` or `mr_modules/`, imports use `@components`/`@store` aliases, uses `trackMREvent`, `lang="pug"` + scoped Stylus
- **Kyonax RECKIT indicators:** path contains `@<brand>/sources/{hud,animation,scene}/`, imports from `@hud/`, `@widgets/`, `@composables/use-obs-websocket.js`, `<script setup>` with Vue 3 Composition API, SCSS with `--hud-*` custom properties

**Load domain skills / rule files based on detection:**

| Detection                                      | Load                                                          |
|------------------------------------------------|---------------------------------------------------------------|
| Vue 3 + Options API + Pug + Stylus + Vuex (MR) | `mr-dotcom-dev` (full skill) + `rules/mr-review-checklist.md` |
| Express / Node.js in MR monorepo               | `mr-dotcom-dev/rules/express-routing.md`                      |
| Test files in MR monorepo                      | `mr-dotcom-dev/rules/testing-standards.md`                    |
| Vue 3 + Composition API + SCSS + OBS (RECKIT)  | `rules/brand-kyonax.md` (already loaded in Stage 0 for this brand) |

### Stage 2: Analysis

Review against ALL applicable categories. Work systematically — do not cherry-pick. Three axes stack: universal → brand → tech-stack. A single finding can cite any of them; cite the most specific rule that applies.

**Universal (any codebase):**
- **Architecture (CRITICAL):** Single responsibility, proper data flow, feature abstraction, no god objects
- **Performance (HIGH):** No unnecessary re-computation, efficient event handling, module-level constants
- **Testability (HIGH):** No inline complex logic, no magic numbers, DRY, no unused code, modern syntax
- **Accessibility (HIGH):** Semantic HTML, ARIA attributes, keyboard navigability, no nested interactives
- **Code Style (LOW):** Consistent naming, formatting, file structure

**Brand-specific (if a brand rule was loaded in Stage 0):** Apply the brand rule's sections. For `brand-kyonax.md` the axes are:
- CSS cost (Section A) — CRITICAL on animated filters / broad shadow utilities
- OBS WebSocket call budget (Section B) — CRITICAL on non-singleton WS composables
- Zero-allocation hot path (Section C) — HIGH on per-event allocations
- Vue reactivity boundary (Section D) — CRITICAL on reactive per-frame bindings
- Event listener hygiene (Section E) — HIGH on un-debounced burst listeners
- Pre-merge checklist (Section F) — run all 8 questions against every HUD-touching change
- General conventions (Section G) — MEDIUM / LOW

**Tech-stack domain-specific:** Apply loaded domain rule (e.g., MR checklist — 82 rules across 9 categories).

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
