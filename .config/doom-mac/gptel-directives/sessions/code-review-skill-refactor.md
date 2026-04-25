<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

# Code Review Skill Refactor — Planning Session

## Purpose

Refactor the `code-review` skill into an atomic, worker-friendly architecture where **each rule is a standalone file** that any AI agent (Claude, GPTel, or other) can load independently. Enable 3-tier detection (brand → project → tech-stack) and efficient parallel subagent dispatch by loading only the specific rule files each worker needs.

---

## Design Principles

1. **One rule = one file.** Every rule is self-contained with: description, examples, edge cases, when to apply, when NOT to apply. Workers load exactly the rules they need — no wasted tokens.
2. **Worker-splittable.** The directory structure IS the dispatch map. Each subdirectory = one potential worker category. A dispatcher reads the index, selects directories, and assigns them to parallel workers.
3. **Progressive loading.** Universal rules load always. Framework/brand/project rules load only when detected. A worker never sees rules outside its scope.
4. **Human-readable index.** `SKILL.md` is a flat lookup table — rule ID → file path → one-line description. A human or AI can scan it in seconds.
5. **No duplication.** Each rule lives in exactly one file. Cross-references use rule IDs, not copy-paste.

---

## Current State

173 rules across 9 files. Rules are bundled by category (84 rules in one `mr-review-checklist.md`). Workers receive entire category files — most tokens are irrelevant to the specific finding.

---

## Target Architecture

### Directory Structure

```
code-review/
├── SKILL.md                              # Index: detection flow + rule catalog + worker dispatch
├── AGENTS.md                             # Architecture rationale + worker protocol
│
├── detection/                            # Step 0-2: what to load
│   ├── brand.md                          # Git remote → brand (Kyonax | MR | generic)
│   ├── project.md                        # File paths → project (mr-dotcom | mr-backend | obs-hud | generic)
│   └── tech-stack.md                     # package.json + extensions → framework (Vue3 | React | Express)
│
├── universal/                            # Always loaded. Any web/JS project.
│   ├── ada/                              # Worker: ADA Accessibility
│   │   ├── landmark-self-contained.md    # rule-u-ada-001
│   │   ├── aria-hidden-decorative.md     # rule-u-ada-002
│   │   ├── role-list-styled.md           # rule-u-ada-003
│   │   ├── heading-hierarchy.md          # rule-u-ada-004 (+CR-LESSON-7)
│   │   ├── focus-visible.md              # rule-u-ada-005
│   │   ├── no-nested-interactive.md      # rule-u-ada-006
│   │   ├── no-duplicate-landmarks.md     # rule-u-ada-007
│   │   ├── keyboard-nav-custom.md        # rule-u-ada-008
│   │   ├── aria-expanded-coerce.md       # rule-u-ada-009
│   │   ├── dynamic-aria-label.md         # rule-u-ada-010
│   │   ├── no-fake-interactive.md        # rule-u-ada-011
│   │   ├── meaningful-alt-text.md        # rule-u-ada-012
│   │   ├── error-message-context.md      # rule-u-ada-013
│   │   ├── live-region-outside-vif.md    # rule-u-ada-014 (+CR-LESSON-2, CR-LESSON-9)
│   │   ├── focus-return-collapse.md      # rule-u-ada-015
│   │   ├── aria-controls-pair.md         # rule-u-ada-016
│   │   ├── role-link-cards.md            # rule-u-ada-017
│   │   ├── focus-on-dom-swap.md          # rule-u-ada-018 (+CR-LESSON-8)
│   │   ├── icon-aria-hidden-button.md    # rule-u-ada-019 (+CR-LESSON-10)
│   │   └── sticky-subheader-offset.md    # rule-u-ada-020
│   │
│   ├── code-style/                       # Worker: Code Style
│   │   ├── blank-line-spacing.md         # rule-u-cs-001
│   │   ├── no-info-comments.md           # rule-u-cs-002
│   │   ├── minimal-touch.md              # rule-u-cs-003
│   │   ├── simple-expressions.md         # rule-u-cs-004
│   │   ├── clean-strings.md             # rule-u-cs-005
│   │   └── jsdoc-block-description.md    # rule-u-cs-006 (+CR-LESSON-6)
│   │
│   ├── script/                           # Worker: Script Patterns
│   │   ├── no-unused-vars.md             # rule-u-sc-001
│   │   ├── optional-chaining.md          # rule-u-sc-002
│   │   ├── module-constants.md           # rule-u-sc-003
│   │   ├── no-inline-event-logic.md      # rule-u-sc-004
│   │   ├── extract-repeated-logic.md     # rule-u-sc-005
│   │   ├── cache-repeated-lookups.md     # rule-u-sc-006
│   │   ├── separate-error-domains.md     # rule-u-sc-007 (+CR-LESSON-1)
│   │   ├── singleton-no-race.md          # rule-u-sc-008 (+CR-LESSON-3)
│   │   └── concurrent-guard-return.md    # rule-u-sc-009 (+CR-LESSON-5)
│   │
│   ├── mobile/                           # Worker: Mobile Viewport
│   │   ├── 100dvh-not-100vh.md           # rule-u-mob-001
│   │   ├── safe-area-viewport-fit.md     # rule-u-mob-002
│   │   ├── single-scroll-panel.md        # rule-u-mob-003
│   │   ├── overflow-auto.md              # rule-u-mob-004
│   │   └── overscroll-contain.md         # rule-u-mob-005
│   │
│   └── third-party-sdk/                  # Worker: SDK Integration
│       ├── sdk-dom-scope.md              # rule-u-sdk-001
│       ├── sdk-content-detect.md         # rule-u-sdk-002
│       ├── sdk-listener-cleanup.md       # rule-u-sdk-003
│       ├── sdk-deep-exception.md         # rule-u-sdk-004
│       └── sdk-aria-conditional.md       # rule-u-sdk-005
│
├── framework/                            # Loaded when tech-stack matches
│   ├── vue3/                             # Worker: Vue 3 Patterns
│   │   ├── vif-guard-null.md             # rule-fw-vue-001
│   │   ├── options-api-field-order.md    # rule-fw-vue-002
│   │   ├── explicit-emits.md             # rule-fw-vue-003
│   │   ├── computed-alphabetized.md      # rule-fw-vue-004
│   │   ├── vuex-helpers-top.md           # rule-fw-vue-005
│   │   ├── props-before-deep.md          # rule-fw-vue-006
│   │   ├── parent-prepares-data.md       # rule-fw-vue-007
│   │   ├── matchmedia-not-resize.md      # rule-fw-vue-008
│   │   ├── template-method-trace.md      # rule-fw-vue-009
│   │   ├── function-props-async.md       # rule-fw-vue-010 (+CR-LESSON-4)
│   │   └── deep-override-comment.md      # rule-fw-vue-011
│   │
│   ├── vue3-composition/                 # Worker: Composition API (additive to vue3/)
│   │   ├── script-setup-conventions.md   # rule-fw-vca-001
│   │   ├── composable-over-mixin.md      # rule-fw-vca-002
│   │   └── ref-reactive-patterns.md      # rule-fw-vca-003
│   │
│   └── express/                          # Worker: Express/Node
│       ├── no-console-use-log.md         # rule-fw-exp-001
│       ├── error-codes-not-html.md       # rule-fw-exp-002
│       └── no-string-match-message.md    # rule-fw-exp-003
│
├── brand/                                # Loaded when brand matches
│   └── kyonax/                           # Worker: Kyonax FPS Discipline
│       ├── css-cost-no-broadcast.md      # rule-br-kyo-001
│       ├── css-cost-group-halo.md        # rule-br-kyo-002
│       ├── css-animate-transform-only.md # rule-br-kyo-003
│       ├── css-split-static-animated.md  # rule-br-kyo-004
│       ├── css-contain-layout-paint.md   # rule-br-kyo-005
│       ├── obs-ws-singleton.md           # rule-br-kyo-006
│       ├── obs-ws-identity-test.md       # rule-br-kyo-007
│       ├── obs-ws-no-unmount-cleanup.md  # rule-br-kyo-008
│       ├── obs-event-driven.md           # rule-br-kyo-009
│       ├── obs-throttle-emit.md          # rule-br-kyo-010
│       ├── hotpath-preallocate.md        # rule-br-kyo-011
│       ├── hotpath-lookup-tables.md      # rule-br-kyo-012
│       ├── hotpath-classic-for.md        # rule-br-kyo-013
│       ├── hotpath-hardcode-targets.md   # rule-br-kyo-014
│       ├── reactivity-bypass-hotpath.md  # rule-br-kyo-015
│       ├── reactivity-write-threshold.md # rule-br-kyo-016
│       ├── reactivity-tick-counter.md    # rule-br-kyo-017
│       ├── listener-debounce-burst.md    # rule-br-kyo-018
│       ├── listener-cleanup-unmount.md   # rule-br-kyo-019
│       ├── premerge-checklist.md         # rule-br-kyo-020 (8 questions)
│       └── conventions.md               # rule-br-kyo-021 (kebab emits, aliases, colors in SCSS)
│
└── project/                              # Loaded when project matches
    ├── mr-dotcom/                        # Worker categories for MR website
    │   ├── template/                     # Worker: MR Template
    │   │   ├── pug-only.md              # rule-pj-mrd-001
    │   │   ├── kebab-case-tags.md       # rule-pj-mrd-002
    │   │   ├── heading-inline-text.md   # rule-pj-mrd-003
    │   │   ├── curly-braces-if.md       # rule-pj-mrd-004
    │   │   ├── single-h1-page.md        # rule-pj-mrd-005
    │   │   └── omit-redundant-div.md    # rule-pj-mrd-006
    │   │
    │   ├── styling/                      # Worker: MR Styling
    │   │   ├── scoped-stylus.md         # rule-pj-mrd-010
    │   │   ├── utility-first.md         # rule-pj-mrd-011
    │   │   ├── flex-in-stylus.md        # rule-pj-mrd-012
    │   │   ├── max-at-tweak.md          # rule-pj-mrd-013
    │   │   ├── design-system-vars.md    # rule-pj-mrd-014
    │   │   ├── css-alphabetized.md      # rule-pj-mrd-015
    │   │   ├── no-stylus-spacing.md     # rule-pj-mrd-016
    │   │   ├── nesting-mirrors.md       # rule-pj-mrd-017
    │   │   ├── font-in-template.md      # rule-pj-mrd-018
    │   │   ├── upper-f-secondary.md     # rule-pj-mrd-019
    │   │   ├── rem-em-units.md          # rule-pj-mrd-020
    │   │   ├── styles-inside-sfc.md     # rule-pj-mrd-021
    │   │   └── comment-3p-overrides.md  # rule-pj-mrd-022
    │   │
    │   ├── naming/                       # Worker: MR Naming
    │   │   ├── short-root-prefix.md     # rule-pj-mrd-030
    │   │   ├── no-redundant-prefix.md   # rule-pj-mrd-031
    │   │   ├── card-children.md         # rule-pj-mrd-032
    │   │   ├── grid-columns.md          # rule-pj-mrd-033
    │   │   └── no-bem.md               # rule-pj-mrd-034
    │   │
    │   ├── images-tracking/              # Worker: MR Images + Tracking
    │   │   ├── imgbox-always.md         # rule-pj-mrd-040
    │   │   ├── skeleton-imgbox.md       # rule-pj-mrd-041
    │   │   ├── deep-img-aspect.md       # rule-pj-mrd-042
    │   │   ├── tracking-function.md     # rule-pj-mrd-043
    │   │   ├── cms-svg-imgbox.md        # rule-pj-mrd-044
    │   │   ├── no-static-images.md      # rule-pj-mrd-045
    │   │   ├── experiment-exposure.md   # rule-pj-mrd-046
    │   │   └── double-tracking.md       # rule-pj-mrd-047 (+CR-LESSON from NIT-3)
    │   │
    │   └── components/                   # Worker: MR Components
    │       ├── mrbtn-variants.md        # rule-pj-mrd-050
    │       ├── mrbtn-deep-override.md   # rule-pj-mrd-051
    │       ├── shared-component-level.md # rule-pj-mrd-052
    │       ├── thin-wrapper.md          # rule-pj-mrd-053
    │       ├── self-sufficient-spacing.md # rule-pj-mrd-054
    │       ├── no-hardcoded-routes.md   # rule-pj-mrd-055
    │       ├── extract-dont-bloat.md    # rule-pj-mrd-056
    │       └── existing-validation.md   # rule-pj-mrd-057
    │
    ├── mr-backend/                       # Worker: MR Backend (Express + CMS + SSR)
    │   ├── cms-partial-deps.md          # rule-pj-mrb-001
    │   ├── cms-parseurl.md              # rule-pj-mrb-002
    │   ├── cms-route-cache.md           # rule-pj-mrb-003
    │   ├── requrl-coupling.md           # rule-pj-mrb-004
    │   ├── express-child-route.md       # rule-pj-mrb-005
    │   ├── componentless-parent.md      # rule-pj-mrb-006
    │   ├── global-ssr-reg.md            # rule-pj-mrb-007
    │   ├── css-only-responsive.md       # rule-pj-mrb-008
    │   ├── ssr-timezone.md              # rule-pj-mrb-009
    │   ├── cookie-state-xfer.md         # rule-pj-mrb-010
    │   ├── track-nav-select.md          # rule-pj-mrb-011
    │   └── breadcrumb-canonical.md      # rule-pj-mrb-012
    │
    └── kyonax-obs-hud/                   # Worker: OBS HUD-specific (inherits brand/kyonax/)
        └── README.md                     # "Load all brand/kyonax/ rules. No additional project rules."
```

### Rule File Template

Every rule file follows this exact structure:

```markdown
---
id: rule-u-ada-004
title: Heading Hierarchy — No Level Skips
tier: universal
category: ada
severity: HIGH
tags: heading, h1, h2, h3, hierarchy, wcag-1.3.1, document-outline
---

## Rule

Heading levels must not skip. `h1` → `h2` → `h3`. Never `h1` → `h3`.
Sibling sections at the same depth use the same heading level.

## When to Apply

- Any HTML/template with heading elements (`h1`–`h6`)
- Components that render section headings
- Pages with multiple content sections

## When NOT to Apply

- Headings inside third-party SDK-injected DOM (not under your control)
- Heading-like elements that are not actual `<h*>` tags (e.g., styled `<p>` or `<span>`)

## Examples

### Bad
```pug
h1 Page Title
h3 Section Title  //- SKIPS h2
```

### Good
```pug
h1 Page Title
h2 Section Title
```

### Edge Case: Sibling Sections
```pug
h1 YOUR DETAILS
section
  h2 Express Sign In    //- parallel section
section
  h2 Contact Info        //- same level, NOT h3
```

When the first section is conditionally hidden (`v-if`), the second section
must still be `h2` — it's a direct child of `h1`, not nested under the first section.

## Origin

- WCAG 1.3.1 Info and Relationships (Level A)
- CR-LESSON-7: Validated during a booking flow review where parallel sections used different heading levels
```

---

## Worker Dispatch Protocol

### Step 1: Detection
```
brand-detection.md → brand (kyonax | mr | generic)
project-detection.md → project (mr-dotcom | mr-backend | obs-hud | generic)
tech-stack-detection.md → tech-stack (vue3 | vue3-composition | express | react | generic)
```

### Step 2: Rule Collection
```
rules = universal/**/*.md                          # always
      + framework/{detected-stack}/**/*.md         # if framework detected
      + brand/{detected-brand}/**/*.md             # if brand detected
      + project/{detected-project}/**/*.md         # if project detected
```

### Step 3: Worker Assignment
Each subdirectory = one worker. The dispatcher:
1. Lists all collected rule directories
2. Groups by directory (each dir = one category)
3. Launches one subagent per directory with ONLY the rule files in that directory
4. Each subagent receives: rule files + file paths to review + output format

```
Example: Brand project with Vue 3 (3 files changed)

Workers launched (parallel):
├── Worker 1: universal/ada/ (20 rules)
├── Worker 2: universal/code-style/ (6 rules)
├── Worker 3: universal/script/ (9 rules)
├── Worker 4: universal/mobile/ (5 rules)
├── Worker 5: framework/vue3/ (11 rules)
├── Worker 6: project/{detected}/template/ (6 rules)
├── Worker 7: project/{detected}/styling/ (13 rules)
└── Worker 8: project/{detected}/components/ (8 rules)

Total: 8 workers, 78 rules. Each worker sees only its ~6-20 rules.
```

```
Example: Generic React project review

Workers launched (parallel):
├── Worker 1: universal/ada/ (20 rules)
├── Worker 2: universal/code-style/ (6 rules)
├── Worker 3: universal/script/ (9 rules)
└── Worker 4: universal/mobile/ (5 rules)

Total: 4 workers, 40 rules. No framework/brand/project rules loaded.
```

### Step 4: Merge + Resolution
- Collect all findings from all workers
- Deduplicate (same file:line from different workers)
- Sort by severity (CRITICAL → LOW)
- Present one-by-one to user: before/after + "implement or skip?"

---

## Implementation Phases — Detailed Step-by-Step

### Phase 1: Scaffolding (directories + templates + AGENTS.md)

**1.1** Create the full directory tree:
```bash
mkdir -p code-review/{detection,scripts}
mkdir -p code-review/universal/{ada,code-style,script,mobile,third-party-sdk}
mkdir -p code-review/framework/{vue3,vue3-composition,express}
mkdir -p code-review/brand/kyonax
mkdir -p code-review/project/{mr-dotcom/{template,styling,naming,images-tracking,components},mr-backend,kyonax-obs-hud}
```

**1.2** Write `AGENTS.md` — architecture rationale, worker protocol, how to add rules/projects/brands.

**1.3** Write `RULE_TEMPLATE.md` — the canonical template every rule file must follow. Include:
- Frontmatter schema: `id`, `title`, `tier`, `category`, `severity`, `tags`
- Body sections: Rule, Apply, Skip, Bad, Good, Edge, Origin
- Tag authoring guide: tags must be code-greppable (CSS properties, HTML attributes, Vue directives, function names)
- Token budget: under 200 tokens for the rule body (excluding examples)

**1.4** Checkpoint: verify directory tree exists, templates are valid.

---

### Phase 2: Extract universal rules (45 rules → 5 directories)

For each rule below: read the source file, extract the rule, write to its own file using the template. **Never delete the source file yet** — keep originals as read-only reference until Phase 9 validation.

**2.1 — universal/ada/ (20 rules)**

Read source: `advanced-ada.md` (7 rules) + `mr-review-checklist.md` ADA section (13 rules: 27-33, sg-9 to sg-12, ad-1, ad-17) + CR-LESSONS (4 rules: CR-LESSON-2, 7, 8, 9, 10).

| Step | Source Rule | Target File | Tags | Edge Cases |
|---|---|---|---|---|
| 2.1.1 | Rule 27 | `landmark-self-contained.md` | landmark, role-region, aria-labelledby, section | Component renders inside another landmark — don't double-wrap |
| 2.1.2 | Rule 28 | `aria-hidden-decorative.md` | aria-hidden, decorative, img, icon | Icon inside a labeled button is decorative (CR-LESSON-10). Icon standalone with no adjacent text is NOT decorative |
| 2.1.3 | Rule 29 | `role-list-styled.md` | role-list, ul, list-style-none | Safari VoiceOver strips list semantics on `list-style: none` — `role="list"` restores it |
| 2.1.4 | Rule 30 + CR-LESSON-7 | `heading-hierarchy.md` | heading, h1, h2, h3, wcag-1.3.1 | Sibling sections at same depth = same level. When `v-if` hides first heading, second stays at its level |
| 2.1.5 | Rule 31 + adv-ada-5 | `focus-visible.md` | focus-visible, focus, outline, keyboard | `:focus-visible` not `:focus` — avoids mouse-click distractions. Custom buttons with `.btn-reset` or equivalent that remove default outline MUST add this |
| 2.1.6 | Rule 32 | `no-nested-interactive.md` | nested, interactive, button, a, link | `<a>` inside `<button>` or `role="button"` inside `<a>` — both invalid. Wrapping a card in `<a>` with internal buttons = nested |
| 2.1.7 | Rule 33 | `no-duplicate-landmarks.md` | duplicate, landmark, region, nav | Two `<nav>` on same page need distinct `aria-label` to differentiate |
| 2.1.8 | sg-9 | `keyboard-nav-custom.md` | keyboard, tabindex, keydown, enter, space | Non-native elements (div, span) acting as buttons need `tabindex="0"` + `@keydown.enter` + `@keydown.space`. Native `<button>` already handles this |
| 2.1.9 | sg-10 | `aria-expanded-coerce.md` | aria-expanded, toggle, boolean | Use `!!` to coerce: `:aria-expanded="!!stateVar"`. Without `!!`, `undefined` renders as missing attribute (not `false`) |
| 2.1.10 | sg-11 | `dynamic-aria-label.md` | aria-label, dynamic, repeated-links | Identical link text for different targets (e.g., multiple "Book" buttons) — each needs unique `aria-label` with context |
| 2.1.11 | sg-12 | `no-fake-interactive.md` | fake, role, tracking, div-button | Don't add `role="button"` to tracking-only elements. If it's not interactive for the user, it's not interactive for AT |
| 2.1.12 | ad-1 | `meaningful-alt-text.md` | alt, alt-text, image, decorative | `alt=""` only for purely decorative. Functional images (logos, icons with meaning) need descriptive alt. CMS images: use CMS alt_text field |
| 2.1.13 | ad-17 | `error-message-context.md` | error, message, context, validation | Error text must match the user's action context. "Invalid input" is bad. "Phone number must be 10 digits" is good |
| 2.1.14 | adv-ada-1 + CR-LESSON-2 + CR-LESSON-9 | `live-region-outside-vif.md` | aria-live, role-alert, v-if, live-region | Container must be persistent in DOM (no `v-if`). Use interaction flag (`signedInDuringSession` pattern) to prevent spurious announcements on mount. Trade-off: some AT may announce empty — acceptable, screen readers skip empty elements |
| 2.1.15 | adv-ada-2 + CR-LESSON-8 | `focus-return-collapse.md` | focus, return, collapse, v-if, dom-swap | When `v-if`/`v-else` removes the element user just interacted with: `nextTick(() => targetElement?.focus())`. Target = next logical element (first form field, parent heading, etc.) |
| 2.1.16 | adv-ada-3 | `aria-controls-pair.md` | aria-controls, aria-expanded, id | `aria-expanded` MUST pair with `aria-controls` pointing to the controlled region's `id` |
| 2.1.17 | adv-ada-4 | `role-link-cards.md` | role-link, card, wcag-2.5.3, tabindex | Multi-content clickable cards: `div[role="link"]` + `tabindex="0"` + `@keydown.enter`. Avoids WCAG 2.5.3 "Label in Name" scanner flags on nested `<a>` |
| 2.1.18 | CR-LESSON-10 | `icon-aria-hidden-button.md` | icon, aria-hidden, button, decorative | Any icon (`<svg>`, icon component) inside a button/link that already has `aria-label` or visible text = decorative → `aria-hidden="true"` |
| 2.1.19 | adv-ada-6 | `sticky-subheader-offset.md` | sticky, subheader, resize-observer, offset | Sticky elements below a dynamic-height header: use `ResizeObserver` on header → update `top` via `:style` binding. NOT CSS var (requires manual sync) |
| 2.1.20 | adv-ada-7 | `deep-override-comment.md` | deep, override, comment, sdk, design-system | `:deep()` overrides on third-party or design system components must include a comment explaining WHY and identifying the SDK/version |

**2.1.21** Write `universal/ada/INDEX.md` — table of all 20 rules with ID, file, summary, severity.

**2.1 Checkpoint:** 20 files + 1 INDEX.md in `universal/ada/`. Verify each file has valid frontmatter, tags, examples.

---

**2.2 — universal/code-style/ (6 rules)**

Read source: `mr-review-checklist.md` rules 39-41, ad-15, ad-20 + CR-LESSON-6.

| Step | Source | Target | Tags | Edge Cases |
|---|---|---|---|---|
| 2.2.1 | Rule 39 | `blank-line-spacing.md` | blank-line, spacing, whitespace | One blank between logical blocks. No consecutive blanks. No zero-blank between unrelated blocks |
| 2.2.2 | Rule 40 | `no-info-comments.md` | comment, jsdoc, todo | Keep: `// TODO:`, `// eslint-disable`, genuinely non-obvious logic. Remove: restating what code does, empty `/** */` |
| 2.2.3 | Rule 41 | `minimal-touch.md` | minimal-touch, format, diff | Only format lines you're modifying. Don't reformat entire files. Exception: if the file has zero formatting and you're touching >50% of lines |
| 2.2.4 | ad-15 | `simple-expressions.md` | expression, one-line, return | Don't multi-line a trivial return. `return isActive ? 'yes' : 'no';` stays one line |
| 2.2.5 | ad-20 | `clean-strings.md` | string, whitespace, code, flag | No whitespace in programmatic strings (codes, flags, enum values). `'ACTIVE'` not `' ACTIVE '` |
| 2.2.6 | CR-LESSON-6 | `jsdoc-block-description.md` | jsdoc, description, param, returns | JSDoc with `@param`/`@returns` tags MUST have a description line before the tags. Tags alone fail most linters. Type casing: `Function` (capital F), not `function` |

**2.2.7** Write `universal/code-style/INDEX.md`.

---

**2.3 — universal/script/ (9 rules)**

Read source: `mr-review-checklist.md` rules 10-13, ad-23, ad-24 + CR-LESSONS 1, 3, 5.

| Step | Source | Target | Tags | Edge Cases |
|---|---|---|---|---|
| 2.3.1 | Rule 10 | `no-unused-vars.md` | unused, import, variable, dead-code | Imports for type-only usage in JSDoc: keep if referenced. Imports for side effects (CSS, polyfills): keep |
| 2.3.2 | Rule 11 | `optional-chaining.md` | optional-chaining, null, undefined | Always `foo?.bar` over `foo && foo.bar`. Exception: when you need to distinguish `null` from `undefined` (rare) |
| 2.3.3 | Rule 12 | `module-constants.md` | const, magic-number, static | Static non-reactive data → `const` outside component/function. Error messages, retry limits, default widths, API paths |
| 2.3.4 | Rule 13 | `no-inline-event-logic.md` | inline, event, click, handler | `@click="doThing"` not `@click="isOpen = !isOpen"`. One method call per event handler |
| 2.3.5 | ad-23 | `extract-repeated-logic.md` | repeated, extract, helper, dry | 3+ identical lines in different functions → extract to helper. Name the helper after what it does, not where it's called from |
| 2.3.6 | ad-24 | `cache-repeated-lookups.md` | cache, lookup, property, const | `const imagery = product?.imagery; imagery?.swatch || imagery?.alt` — cache the intermediate. Especially in computed properties and loops |
| 2.3.7 | CR-LESSON-1 | `separate-error-domains.md` | error, async, try-catch, domain | When two sequential awaits serve different user-facing purposes (e.g., auth + data fetch), wrap each in its own try/catch. Don't let a data-fetch error surface as an auth error |
| 2.3.8 | CR-LESSON-3 | `singleton-no-race.md` | singleton, global, flag, race-condition | Global singleton flags (e.g., `window.libInitialized`) are NOT race conditions when consumers can't co-exist on the same page. Verify routing/component tree before flagging. Only flag if two consumers can mount simultaneously |
| 2.3.9 | CR-LESSON-5 | `concurrent-guard-return.md` | concurrent, guard, return, undefined | Vuex/store actions with "skip if already running" guards must return current state, not bare `return` (which gives `undefined`). Callers depend on the return value |

**2.3.10** Write `universal/script/INDEX.md`.

---

**2.4 — universal/mobile/ (5 rules)**

Read source: `mobile-viewport.md` (all 5 rules). These are already well-isolated — convert to individual files.

| Step | Source | Target | Tags |
|---|---|---|---|
| 2.4.1 | mob-1 | `100dvh-not-100vh.md` | 100dvh, 100vh, viewport, ios-safari, mobile |
| 2.4.2 | mob-2 | `safe-area-viewport-fit.md` | safe-area, env, viewport-fit, cover, ios |
| 2.4.3 | mob-3 | `single-scroll-panel.md` | scroll, overflow, panel, mobile |
| 2.4.4 | mob-4 | `overflow-auto.md` | overflow-y, auto, scroll, gutter |
| 2.4.5 | mob-5 | `overscroll-contain.md` | overscroll, contain, overlay, modal |

**2.4.6** Write `universal/mobile/INDEX.md`.

---

**2.5 — universal/third-party-sdk/ (5 rules)**

Read source: `third-party-sdk.md` (all 5). Already well-isolated.

| Step | Source | Target | Tags |
|---|---|---|---|
| 2.5.1 | sdk-1 | `sdk-dom-scope.md` | sdk, deep, refs, dom, scope |
| 2.5.2 | sdk-2 | `sdk-content-detect.md` | sdk, poll, timeout, content, hide |
| 2.5.3 | sdk-3 | `sdk-listener-cleanup.md` | sdk, listener, cleanup, unmount, memory-leak |
| 2.5.4 | sdk-4 | `sdk-deep-exception.md` | sdk, deep, exception, override |
| 2.5.5 | sdk-5 | `sdk-aria-conditional.md` | sdk, aria, conditional, load-state |

**2.5.6** Write `universal/third-party-sdk/INDEX.md`.

**Phase 2 Checkpoint:** 45 rule files + 5 INDEX.md files across 5 universal directories. Run: count files, verify frontmatter, verify no brand-specific content in any universal rule.

---

### Phase 3: Extract framework rules (17 rules → 3 directories)

**3.1 — framework/vue3/ (11 rules)**

Read source: `mr-review-checklist.md` rules 4-9, sg-2 to sg-5, ad-4 + CR-LESSON-4.

**Critical: strip all brand-specific references.** These rules mention MR-specific patterns — generalize them:
- `mapState('customer', [...])` → "Vuex/Pinia helpers at top of computed/methods sections"
- `getObjProperty` → "legacy safe-access utilities" (generic)
- `global/isDesktop` → "centralized responsive getter" (generic)
- No mention of specific components (MrBtn, MrInput, ImgBox) — those go to project rules

| Step | Source | Target | Tags | Brand Content to Strip |
|---|---|---|---|---|
| 3.1.1 | Rule 4 | `vif-guard-null.md` | v-if, guard, null, data | None — already generic |
| 3.1.2 | Rule 5 | `api-style-consistency.md` | options-api, composition-api, script-setup | Remove "Options API only" — make configurable. Note: project rule can enforce one or the other |
| 3.1.3 | Rule 6 | `field-order.md` | field-order, name, props, data, computed, methods | Remove MR-specific order if it differs from Vue style guide. Use Vue official recommended order |
| 3.1.4 | Rule 7 | `explicit-emits.md` | emits, defineEmits, event | None — already generic |
| 3.1.5 | Rule 8 | `computed-alphabetized.md` | computed, alphabetized, order | None — already generic |
| 3.1.6 | Rule 9 | `store-helpers-top.md` | vuex, pinia, mapState, mapGetters, mapActions | Generalize: "State management helpers at top of computed/methods sections" — works for Vuex and Pinia |
| 3.1.7 | sg-2 | `props-before-deep.md` | props, deep, styling, child | None — already generic |
| 3.1.8 | sg-3 | `parent-prepares-data.md` | parent, computed, props, preparation | None — already generic |
| 3.1.9 | sg-4 | `matchmedia-not-resize.md` | matchMedia, resize, window, responsive | Strip `global/isDesktop` reference — generalize to "use a centralized responsive getter or composable" |
| 3.1.10 | ad-4 | `template-method-trace.md` | template, method, trace, handler | None — already generic |
| 3.1.11 | CR-LESSON-4 | `function-props-async.md` | function-prop, emit, async, callback | Function props valid when callback returns data from async flow. `emit` can't return values. Example uses generic component names |

**3.1.12** Write `framework/vue3/INDEX.md`.

---

**3.2 — framework/vue3-composition/ (3 rules)**

No direct source — these are new rules extracted from session guidelines and common Composition API patterns.

| Step | Target | Tags |
|---|---|---|
| 3.2.1 | `script-setup-conventions.md` | script-setup, composition-api, ref, computed |
| 3.2.2 | `composable-over-mixin.md` | composable, mixin, use, shared-logic |
| 3.2.3 | `ref-reactive-patterns.md` | ref, reactive, watch, computed |

**3.2.4** Write `framework/vue3-composition/INDEX.md`.

---

**3.3 — framework/express/ (3 rules)**

Read source: `mr-review-checklist.md` backend rules ad-18, ad-19, ad-21.

**Strip MR-specific references:** No mention of `Log` module, specific error codes, or MR controller patterns.

| Step | Source | Target | Tags | Brand Content to Strip |
|---|---|---|---|---|
| 3.3.1 | ad-18 | `no-console-use-logger.md` | console, logger, error, structured | Remove "use `Log` module" — generalize to "use project logger" |
| 3.3.2 | ad-19 | `error-codes-not-html.md` | error, response, code, html, status | None — already generic pattern |
| 3.3.3 | ad-21 | `no-string-match-message.md` | string, match, message, error-code | None — already generic pattern |

**3.3.4** Write `framework/express/INDEX.md`.

**Phase 3 Checkpoint:** 17 rule files + 3 INDEX.md. Verify: ZERO brand-specific component names, ZERO project-specific paths, ZERO tool-specific names. All examples use generic component names (`BaseButton`, `UserCard`, `AppModal`).

---

### Phase 4: Extract brand rules (21 rules → 1 directory)

**4.1 — brand/kyonax/ (21 rules)**

Read source: `brand-kyonax.md` sections A-G.

These rules are inherently Kyonax-specific (OBS FPS discipline, HUD overlays). They keep all brand references — that's the correct tier.

| Step | Source | Target | Tags |
|---|---|---|---|
| 4.1.1 | A1 | `css-cost-no-broadcast.md` | filter, box-shadow, text-shadow, broadcast, hud |
| 4.1.2 | A2 | `css-cost-group-halo.md` | halo, container, group, filter |
| 4.1.3 | A3 | `css-animate-transform-only.md` | animate, transform, opacity, keyframe |
| 4.1.4 | A4 | `css-split-static-animated.md` | static, animated, layer, split |
| 4.1.5 | A5 | `css-contain-layout-paint.md` | contain, layout, paint, hud-group |
| 4.1.6 | B1 | `obs-ws-singleton.md` | obs, websocket, singleton, module-level |
| 4.1.7 | B2 | `obs-ws-identity-test.md` | singleton, identity, test, expect-be |
| 4.1.8 | B3 | `obs-ws-no-unmount-cleanup.md` | singleton, unmount, cleanup, page-lifetime |
| 4.1.9 | B4 | `obs-event-driven.md` | event-driven, raf, requestAnimationFrame, clock |
| 4.1.10 | B5 | `obs-throttle-emit.md` | throttle, emit, 10hz, diagnostic |
| 4.1.11 | C1 | `hotpath-preallocate.md` | preallocate, typed-array, float32, mutate |
| 4.1.12 | C2 | `hotpath-lookup-tables.md` | lookup, table, precompute, jitter, scale-strings |
| 4.1.13 | C3 | `hotpath-classic-for.md` | for-loop, classic, array-find, destructuring |
| 4.1.14 | C4 | `hotpath-hardcode-targets.md` | hardcode, target, configurable, mic-aux |
| 4.1.15 | D1 | `reactivity-bypass-hotpath.md` | reactivity, bypass, template-ref, dom-write |
| 4.1.16 | D2 | `reactivity-write-threshold.md` | write-threshold, skip, math-abs, 0.01 |
| 4.1.17 | D3 | `reactivity-tick-counter.md` | tick, counter, ref, watch, non-reactive |
| 4.1.18 | E1 | `listener-debounce-burst.md` | debounce, resize, scroll, setTimeout, 100ms |
| 4.1.19 | E2 | `listener-cleanup-unmount.md` | listener, cleanup, unmount, clearTimeout |
| 4.1.20 | F1-F8 | `premerge-checklist.md` | checklist, premerge, fps, regression |
| 4.1.21 | G1-G8 | `conventions.md` | kebab, emit, alias, colors-scss, no-git-write |

**4.1.22** Write `brand/kyonax/INDEX.md`.

**Phase 4 Checkpoint:** 21 rule files + 1 INDEX.md. All rules keep Kyonax/OBS/HUD references — correct tier.

---

### Phase 5: Extract project rules (52 rules → 6 directories)

**5.1 — project/mr-dotcom/template/ (6 rules)**

Read source: `mr-review-checklist.md` rules 1-3, sg-1, ad-6, ad-7.

These are MR Dotcom-specific: Pug templates, MR component naming. **Keep all MR references** — this is the project tier.

| Step | Source | Target | Tags | MR-Specific Content (KEEP) |
|---|---|---|---|---|
| 5.1.1 | Rule 1 | `pug-only.md` | pug, template, html, lang | "All templates use Pug (`lang='pug'`)" |
| 5.1.2 | Rule 2 | `kebab-case-tags.md` | kebab, component, tag, pug | Examples: `mr-btn` not `MrBtn`, `mr-icon` not `MrIcon` |
| 5.1.3 | Rule 3 | `heading-inline-text.md` | heading, inline, pug, multiline | Pug-specific: `h2 Title` not `h2\n  | Title` |
| 5.1.4 | sg-1 | `curly-braces-if.md` | curly, braces, if, one-line | `if (x) { return; }` not `if (x) return;` |
| 5.1.5 | ad-6 | `single-h1-page.md` | h1, page, section, heading | "Section components use `h2` or lower" — references MR page structure |
| 5.1.6 | ad-7 | `omit-redundant-div.md` | div, pug, redundant, semantic | `.my-class` auto-creates `<div>`. Only add tag name for non-divs (`section.my-class`) |

**5.1.7** Write `project/mr-dotcom/template/INDEX.md`.

---

**5.2 — project/mr-dotcom/styling/ (13 rules)**

Read source: `mr-review-checklist.md` rules 14-21, sg-6-8, ad-14, ad-16.

**All MR-specific:** Stylus, MR design system variables (`brand-color-*`, `ui-color-*`), MR utility classes (`.xs-mb-100m`, `.max-at-tweak`), MR breakpoints.

| Step | Source | Target | Tags | MR-Specific Content (KEEP) |
|---|---|---|---|---|
| 5.2.1 | Rule 14 | `scoped-stylus.md` | scoped, stylus, style, lang | `<style scoped lang="stylus">` — MR uses Stylus, not SCSS/CSS |
| 5.2.2 | Rule 15 | `utility-first.md` | utility, class, breakpoint, spacing | Full MR utility system: `.xs-mb-100m`, `.lg-pt-200m`. Breakpoint prefixes mandatory |
| 5.2.3 | Rule 16 | `flex-in-stylus.md` | flex, stylus, utility, direction | "Keep flex layout in Stylus, not utility classes" — MR convention |
| 5.2.4 | Rule 17 | `max-at-tweak.md` | max-at-tweak, font, responsive | `.max-at-tweak` mandatory on every responsive font class — MR design system |
| 5.2.5 | Rule 18 | `design-system-vars.md` | color, variable, hex, brand-color | `brand-color-*`, `cta-color-*`, `text-color-*`, `ui-color-*` — MR palette |
| 5.2.6 | Rule 19 | `css-alphabetized.md` | alphabetized, css, properties, order | Alphabetize within each Stylus block |
| 5.2.7 | Rule 20 | `no-stylus-spacing.md` | spacing, padding, margin, utility | Don't use `padding` in Stylus when `.xs-py-75m` utility exists |
| 5.2.8 | Rule 21 | `nesting-mirrors.md` | nesting, stylus, hierarchy, template | Stylus nesting must mirror Pug template hierarchy |
| 5.2.9 | sg-6 | `font-in-template.md` | font, size, color, template, stylus | "Font sizes, font-family, text color in templates only — never in `<style>`" — MR convention |
| 5.2.10 | sg-7 | `upper-f-secondary.md` | upper, f-secondary, kapra, heading | `.upper` mandatory on `.f-secondary` (Kapra Neue font) — MR typography |
| 5.2.11 | sg-8 | `rem-em-units.md` | rem, em, px, border, shadow | `rem`/`em` for spacing, `px` for borders/shadows only — MR convention |
| 5.2.12 | ad-14 | `styles-inside-sfc.md` | styles, sfc, external, styl | No external `.styl`/`.css` files — all styles inside the SFC |
| 5.2.13 | ad-16 | `comment-3p-overrides.md` | comment, deep, override, sdk | Comment `:deep()` overrides identifying the SDK/version |

**5.2.14** Write `project/mr-dotcom/styling/INDEX.md`.

---

**5.3 — project/mr-dotcom/naming/ (5 rules)**

| Step | Source | Target | Tags | MR Content (KEEP) |
|---|---|---|---|---|
| 5.3.1 | Rule 22 | `short-root-prefix.md` | prefix, root, class, short | `.hcb-hero-v2` not `.hair-color-bar-location-hero-v2` |
| 5.3.2 | Rule 23 | `no-redundant-prefix.md` | redundant, prefix, parent, child | Remove parent prefix when already scoped |
| 5.3.3 | Rule 24 | `card-children.md` | card, children, parent-element | `.service-card`, `.service-image` pattern |
| 5.3.4 | Rule 25 | `grid-columns.md` | grid, column, role, naming | `.main-column`, `.sidebar-column` |
| 5.3.5 | Rule 26 | `no-bem.md` | bem, double-underscore, double-dash | No `__` or `--` — hyphens only |

**5.3.6** Write `project/mr-dotcom/naming/INDEX.md`.

---

**5.4 — project/mr-dotcom/images-tracking/ (8 rules)**

| Step | Source | Target | Tags | MR Content (KEEP) |
|---|---|---|---|---|
| 5.4.1 | Rule 34 | `imgbox-always.md` | ImgBox, img, image, component | "Use `ImgBox` for all images — no raw `<img>`" — MR component |
| 5.4.2 | Rule 35 | `skeleton-imgbox.md` | skeleton, imgbox, deep, background | `:deep(.image-box) background-color ui-color-4` — MR skeleton pattern |
| 5.4.3 | Rule 36 | `deep-img-aspect.md` | deep, img, aspect-ratio, border-radius | `:deep(img)` for aspect-ratio, object-fit on MR ImgBox |
| 5.4.4 | Rule 37 | `tracking-function.md` | trackMREvent, trackMREventAndRedirect, navigation | Correct tracking function per navigation type — MR segment tracking |
| 5.4.5 | sg-13 | `cms-svg-imgbox.md` | cms, svg, imgbox, mr-icon | CMS SVG icons via `ImgBox`, not `mr-icon` — MR CMS pattern |
| 5.4.6 | ad-9 | `no-static-images.md` | static, image, repo, cms | No PNG/JPG/GIF in repo — use CMS. SVG icons only |
| 5.4.7 | ad-13 | `experiment-exposure.md` | experiment, exposure, tracking, ab-test | Track A/B experiment exposure explicitly — MR experiment system |
| 5.4.8 | NIT-3 lesson | `double-tracking.md` | tracking, double, duplicate, event | When a helper function (e.g., `openSignInModal`) internally tracks an event, don't also track it explicitly before calling the helper. Check helper internals |

**5.4.9** Write `project/mr-dotcom/images-tracking/INDEX.md`.

---

**5.5 — project/mr-dotcom/components/ (8 rules)**

| Step | Source | Target | Tags | MR Content (KEEP) |
|---|---|---|---|---|
| 5.5.1 | Rule 42 | `mrbtn-variants.md` | MrBtn, variant, secondary, tertiary | Use MrBtn variants before custom CSS — MR button system |
| 5.5.2 | Rule 43 | `mrbtn-deep-override.md` | MrBtn, deep, mrbtn, color | `:deep(.mrbtn)` for color overrides — MR MrBtn internals |
| 5.5.3 | Rule 44 | `shared-component-level.md` | shared, component, level, booking | Place components at appropriate shared level — MR component tree |
| 5.5.4 | Rule 45 | `thin-wrapper.md` | wrapper, mobile, shared, content | Thin wrapper pattern: mobile wrapper + shared content — MR responsive pattern |
| 5.5.5 | sg-14 | `self-sufficient-spacing.md` | spacing, component, parent, wrapper | Component handles own spacing internally — MR convention |
| 5.5.6 | ad-11 | `no-hardcoded-routes.md` | route, hardcoded, path, reusable | No hardcoded route paths in reusable components — MR routing |
| 5.5.7 | ad-12 | `extract-dont-bloat.md` | extract, component, bloat, feature | New feature = new component — MR component philosophy |
| 5.5.8 | ad-22 | `existing-validation.md` | validation, system, parallel, vuelidate | Use existing validation systems — MR uses Vuelidate |

**5.5.9** Write `project/mr-dotcom/components/INDEX.md`.

---

**5.6 — project/mr-backend/ (12 rules)**

Read source: `cms-ssr-routing.md` (all 12 rules). All MR backend-specific.

| Step | Source | Target | Tags |
|---|---|---|---|
| 5.6.1 | cms-1 | `cms-partial-deps.md` | cms, partial, serverPrefetch, store |
| 5.6.2 | cms-2 | `cms-parseurl.md` | cms, parseUrl, urlParameterList, tophat |
| 5.6.3 | cms-3 | `cms-route-cache.md` | cms, route, cache, redis, PARAM_ROUTES_INVALID |
| 5.6.4 | cms-4 | `requrl-coupling.md` | req-url, ssr, hydration, mismatch |
| 5.6.5 | cms-5 | `express-child-route.md` | express, child-route, optional-param |
| 5.6.6 | cms-6 | `componentless-parent.md` | componentless, parent, vue-router, double-render |
| 5.6.7 | cms-7 | `global-ssr-reg.md` | global, component, ssr, registration |
| 5.6.8 | cms-8 | `css-only-responsive.md` | css, responsive, matchMedia, hydration |
| 5.6.9 | cms-9 | `ssr-timezone.md` | ssr, timezone, computed, utc |
| 5.6.10 | cms-10 | `cookie-state-xfer.md` | cookie, state, cross-app, vuex |
| 5.6.11 | cms-11 | `track-nav-select.md` | tracking, navigation, redirect, cross-context |
| 5.6.12 | cms-12 | `breadcrumb-canonical.md` | breadcrumb, canonical, url, routes |

**5.6.13** Write `project/mr-backend/INDEX.md`.

**Phase 5 Checkpoint:** 52 rule files + 6 INDEX.md across 6 project directories. Verify: all MR-specific content preserved (MrBtn, ImgBox, Stylus, Pug, trackMREvent, design system vars). NONE of this content leaked into universal or framework tiers.

---

### Phase 6: Detection files

**6.1** Write `detection/brand.md`:
- Git remote URL parsing
- Explicit user override
- Repo-local indicators (brand folders, config files)
- Fallback: generic

**6.2** Write `detection/project.md`:
- Changed file path matching
- Directory structure signals
- Package.json scripts/commands as hints
- Fallback: generic

**6.3** Write `detection/tech-stack.md`:
- `package.json` dependency parsing (vue, react, express, next, nuxt)
- File extension distribution (.vue, .jsx, .tsx)
- Config file detection (vite.config, webpack.config, tsconfig)
- Fallback: generic JS

**Phase 6 Checkpoint:** 3 detection files. Test each with: MR repo, Kyonax repo, generic Vue repo, generic React repo.

---

### Phase 7: Scripts

**7.1** Write all 10 scripts (see Automation Scripts section above).
**7.2** Test each script on the MR repo.
**7.3** Test `detect.sh` + `select-rules.sh` pipeline: verify correct rules selected for:
- MR Dotcom Vue file change → universal + vue3 + vue3-composition + mr-dotcom
- MR Backend controller change → universal + express + mr-backend
- Kyonax HUD component change → universal + vue3 + vue3-composition + kyonax + kyonax-obs-hud
- Generic React project → universal only

---

### Phase 8: SKILL.md + INDEX.md files

**8.1** Write `SKILL.md` with:
- Entry-point description
- Trigger keywords
- 3-tier detection flow with examples
- Complete rule catalog (flat table, ~183 rows)
- Worker dispatch protocol
- Review modes
- Output format

**8.2** Verify every directory has its `INDEX.md` (15 directories = 15 INDEX.md files).

---

### Phase 9: Validation

**9.1** Count: every rule from original files accounted for.
```
Original: 84 (MR checklist) + 7 (advanced-ada) + 5 (mobile) + 5 (SDK) + 47 (Kyonax) + 12 (CMS/SSR) + 3 (experiments) + 10 (component-patterns) = 173
+ 10 CR-LESSONS = 183 target
Actual files: count universal/**/*.md + framework/**/*.md + brand/**/*.md + project/**/*.md (excluding INDEX.md)
```

**9.2** Zero duplicates: `grep -r "^id:" rules/ | sort | uniq -d` → must be empty.

**9.3** Brand isolation: `grep -ri "MrBtn\|ImgBox\|Stylus\|Pug\|trackMREvent\|PilkoLint" universal/ framework/` → must be empty.

**9.4** Tag coverage: for each rule file, verify `tags` field has ≥2 code-greppable keywords.

**9.5** Delete old monolithic files: `mr-review-checklist.md`, `brand-kyonax.md`, `cms-ssr-routing.md`, `component-patterns.md`, `experiment-patterns.md`, `mobile-viewport.md`, `advanced-ada.md`, `third-party-sdk.md`.

**9.6** End-to-end test: run the full review flow (detect → select → dispatch → review → format) on a real PR diff.

---

### Phase 10: Documentation + Cleanup

**10.1** Update session files referencing old rule IDs.
**10.2** Update `SKILL.md` Quick Reference table with new file paths.
**10.3** Write migration note in `AGENTS.md`: "old rule IDs → new rule IDs" mapping table.

### Phase 7: SKILL.md + AGENTS.md + Directory INDEX.md files

**SKILL.md** — the entry point. Must be self-sufficient for any AI to understand and use the skill without reading anything else first:
- What the skill does (one paragraph)
- How to trigger it (commands, keywords)
- 3-tier detection flow (step-by-step with examples)
- Complete rule catalog as flat table: `rule ID | file path | one-line description | tier | severity`
- Worker dispatch protocol (how to split work, how many workers, what each gets)
- Review modes (parallel, standard, scoped) with when to use each
- Output format (finding structure, before/after, implement-or-skip)

**AGENTS.md** — architecture rationale for AI agents consuming this skill:
- Why one-rule-per-file (token efficiency, worker isolation)
- Why directory = worker boundary (no cross-category dependencies)
- How detection cascades (brand → project → tech-stack → rule collection)
- How to add a new rule (create file, add to INDEX.md, add to SKILL.md catalog)
- How to add a new project/brand (create directory, add detection signal)
- How to onboard a new framework (create directory under `framework/`)

**Directory INDEX.md files** — each directory gets a lean index that a worker or human can scan:

```markdown
# universal/ada/ — Accessibility Rules

20 rules. Always loaded. Any web project.

| ID | File | Summary | Severity |
|---|---|---|---|
| rule-u-ada-001 | landmark-self-contained.md | Landmarks own their heading + aria-labelledby in same component | HIGH |
| rule-u-ada-002 | aria-hidden-decorative.md | aria-hidden="true" on decorative images next to text | MEDIUM |
| ... | ... | ... | ... |

**Worker instructions:** Review all files in the PR against these 20 rules.
Report findings as YAML. Return `NO VIOLATIONS` if clean.
```

Every directory (`universal/ada/`, `universal/code-style/`, `framework/vue3/`, `project/mr-dotcom/template/`, etc.) has its own `INDEX.md` that:
1. Names the category and scope (one line)
2. Lists rule count
3. Flat table: ID → file → summary → severity
4. Worker instructions (what the subagent receiving this directory should do)

This means a dispatcher can:
1. Read `SKILL.md` for detection + rule catalog
2. Collect the relevant directories
3. For each directory, read only its `INDEX.md` + the specific rule files needed
4. Hand `INDEX.md` + rule files to a worker as a self-contained package

### Phase 8: Validation
- Count: all 173 original rules accounted for + 10 CR-LESSONS = 183 rules
- No duplicates across tiers
- Detection test cases for each brand/project/tech-stack combo
- Worker dispatch test: correct rule files per scenario

---

## Automation Scripts

All scripts live in `code-review/scripts/`. They are shell scripts (bash) that any AI calls BEFORE reasoning — pre-computing what would otherwise cost tokens.

### Directory

```
code-review/scripts/
├── detect.sh                # Detection: brand + project + tech-stack → JSON
├── list-changed.sh          # Detection: changed files with metadata → JSON
├── pr-fetch.sh              # Detection: pull PR metadata + diff + existing reviews → JSON
├── select-rules.sh          # Rule selection: detection output → rule file paths (two-pass)
├── worker-dispatch.sh       # Rule selection: rule list → worker assignment JSON
├── diff-context.sh          # Pre-review: structured diff + context per file (local or PR)
├── component-tree.sh        # Pre-review: Vue component hierarchy + deps
├── lint-changed.sh          # Validation: ESLint on changed files → JSON errors
├── test-changed.sh          # Validation: test runner on changed files → JSON results
├── ci-local.sh              # Validation: discover + run ALL CI workflows locally → JSON
└── format-findings.sh       # Post-review: raw YAML → formatted findings (or report)
```

### Script Specifications

#### `detect.sh` — Three-Tier Detection
```bash
# Usage: bash scripts/detect.sh [--repo-root /path]
# Output: JSON to stdout
{
  "brand": "acme-corp",              # detected brand or "generic"
  "project": "acme-webapp",          # detected project or "generic"
  "techStack": ["vue3", "vue3-composition"],
  "remote": "git@github.com:AcmeCorp/webapp.git",
  "signals": {
    "brand": "remote-url: AcmeCorp/*",
    "project": "changed-paths: src/components/",
    "techStack": "package.json: vue@3.x"
  }
}
```
- Reads `git remote get-url origin` for brand
- Reads changed file paths (`git diff --name-only`) for project
- Reads `package.json` dependencies for tech-stack
- Falls back gracefully: missing git → generic, missing package.json → generic

#### `list-changed.sh` — Changed Files with Metadata
```bash
# Usage: bash scripts/list-changed.sh [--base master]
# Output: JSON to stdout
{
  "files": [
    { "path": "src/composables/useAuth.js", "status": "modified", "ext": ".js", "dir": "src/composables" },
    { "path": "src/assets/icons/logo.svg", "status": "untracked", "ext": ".svg", "dir": "src/assets/icons" }
  ],
  "summary": { "modified": 4, "untracked": 1, "total": 5 },
  "extensions": [".js", ".vue", ".svg"]
}
```
- Combines `git diff --name-only` (staged + unstaged) + `git ls-files --others`
- Classifies each: modified, added, untracked, deleted
- Extracts extension and directory for filtering

#### `select-rules.sh` — Rule File Selection
```bash
# Usage: bash scripts/select-rules.sh < detect-output.json
# Output: newline-separated file paths to stdout
universal/ada/landmark-self-contained.md
universal/ada/heading-hierarchy.md
universal/code-style/no-info-comments.md
framework/vue3/vif-guard-null.md
project/{detected}/template/template-lang.md
...
```
- Reads detection JSON from stdin
- Collects: `universal/**/*.md` (always) + `framework/{stack}/**/*.md` + `brand/{brand}/**/*.md` + `project/{project}/**/*.md`
- Excludes `INDEX.md` files (those are for workers, not rule content)
- Outputs one path per line for piping

#### `worker-dispatch.sh` — Worker Assignment
```bash
# Usage: bash scripts/select-rules.sh < detect.json | bash scripts/worker-dispatch.sh
# Output: JSON to stdout
{
  "workers": [
    {
      "id": "universal-ada",
      "category": "ADA Accessibility",
      "index": "universal/ada/INDEX.md",
      "rules": ["universal/ada/landmark-self-contained.md", "universal/ada/heading-hierarchy.md", ...],
      "ruleCount": 20
    },
    {
      "id": "project-webapp-styling",
      "category": "Project Styling",
      "index": "project/{detected}/styling/INDEX.md",
      "rules": ["project/{detected}/styling/scoped-styles.md", ...],
      "ruleCount": 13
    }
  ],
  "totalWorkers": 8,
  "totalRules": 78
}
```
- Reads rule paths from stdin
- Groups by parent directory
- Outputs structured worker assignments
- Each worker gets: its `INDEX.md` path + its rule file paths

#### `diff-context.sh` — Structured Diff + Context
```bash
# Usage: bash scripts/diff-context.sh [--base master] [--context 5]
# Output: JSON to stdout
{
  "files": [
    {
      "path": "src/composables/useAuth.js",
      "language": "javascript",
      "diff": "--- a/...\n+++ b/...\n@@ -30,10 +30,15 @@...",
      "imports": ["vue", "vuex", "@services/apiSvc"],
      "exports": ["useAuth"],
      "componentName": null,
      "storeModules": ["user"],
      "lineCount": 85,
      "changedLines": [30, 31, 32, 45, 46]
    }
  ]
}
```
- For each changed file: git diff + import extraction + export detection
- For Vue files: component name, store modules (grep `mapState`/`mapGetters`/`useStore`), props, emits
- For JS files: imports, exports, function names
- Changed line numbers for precise finding locations

#### `component-tree.sh` — Vue Component Hierarchy
```bash
# Usage: bash scripts/component-tree.sh <file.vue> [--depth 2]
# Output: JSON to stdout
{
  "root": "InfoPage.vue",
  "children": [
    { "name": "SignInOptions", "path": "../components/SignInOptions", "import": "local" },
    { "name": "MrSidebarLayout", "path": "@components/MrSidebarLayout", "import": "alias" },
    { "name": "MrInput", "path": "@components/mrInput", "import": "alias" },
    { "name": "BaseButton", "path": "@components/BaseButton", "import": "alias" }
  ],
  "composables": ["useAuth", "useVuelidate"],
  "store": { "reads": ["customer.cdata", "hairColorBarBooking.location"], "dispatches": ["customer/refreshCustomerCdata"] }
}
```
- Parses Vue SFC imports for child components
- Extracts composable usage (`use*` pattern)
- Extracts Vuex store reads/dispatches from `mapState`, `mapGetters`, `store.state`, `store.dispatch`

#### `lint-changed.sh` — ESLint on Changed Files
```bash
# Usage: bash scripts/lint-changed.sh [--format json]
# Output: JSON to stdout (or plain text without --format)
{
  "passed": false,
  "errors": 2,
  "warnings": 15,
  "findings": [
    { "file": "useGoogleSignIn.js", "line": 40, "rule": "jsdoc/require-description", "severity": "error", "message": "Missing JSDoc block description." }
  ]
}
```
- Runs `npx eslint` on changed `.js`/`.vue` files
- Outputs structured JSON for AI consumption
- Matches what CI lint checks (project-specific linter)

#### `test-changed.sh` — Test Runner on Changed Files
```bash
# Usage: bash scripts/test-changed.sh [--framework vitest|jest]
# Output: JSON to stdout
{
  "passed": true,
  "total": 55,
  "passed_count": 55,
  "failed_count": 0,
  "files": [
    { "file": "useGoogleSignIn.test.js", "tests": 18, "passed": 18, "failed": 0 }
  ]
}
```
- Auto-detects test framework from `package.json` (vitest, jest, mocha)
- Finds test files matching changed source files (`*.test.js`, `*.spec.js`)
- Runs only relevant tests, outputs structured results

#### `ci-local.sh` — Discover + Run ALL CI Workflows Locally
```bash
# Usage: bash scripts/ci-local.sh [--repo-root /path]
# Output: JSON to stdout
{
  "workflows_found": [".github/workflows/pilkolint.yml", ".github/workflows/tests.yml", ".github/workflows/security.yml"],
  "results": [
    {
      "workflow": "pilkolint",
      "status": "failed",
      "errors": [
        { "file": "useGoogleSignIn.js", "line": 40, "message": "Missing JSDoc block description.", "rule": "jsdoc/require-description" }
      ],
      "warnings": []
    },
    {
      "workflow": "tests",
      "status": "passed",
      "summary": "55 tests, 3 files, all passing"
    },
    {
      "workflow": "security",
      "status": "passed",
      "summary": "No vulnerabilities found"
    }
  ],
  "overall": "failed",
  "actionRequired": [
    { "workflow": "pilkolint", "count": 2, "severity": "error" }
  ]
}
```
- Discovers ALL `.github/workflows/*.yml` files
- Parses each workflow to identify the run commands (e.g., `npm run lint`, `npm run test`, `npm audit`)
- Runs each command locally in sequence
- Captures stdout/stderr, parses for errors/warnings
- Outputs structured JSON with per-workflow status + actionable items
- Works for ANY project — reads the workflow YAML to discover what to run, doesn't hardcode commands
- Skips workflows that require secrets/cloud (flags as "skipped: requires CI environment")

#### `format-findings.sh` — Raw YAML → Formatted Findings
```bash
# Usage: bash scripts/format-findings.sh < raw-findings.yaml
# Output: Markdown to stdout
---
**Finding 1/5** | **HIGH** | **rule-u-ada-004** | **InfoPage.vue:20**

**Problem:** Heading hierarchy skip — h1 → h3, missing h2.

**Before:**
```pug
L20: h3.contact-info-title Contact Info
```

**After:**
```pug
L20: h2.contact-info-title Contact Info
```

**Implement or skip?**
---
```
- Reads raw YAML findings from workers
- Deduplicates (same file:line from different workers)
- Sorts by severity (CRITICAL → LOW)
- Formats into the standard finding structure with real line numbers + context
- Outputs ready-to-present Markdown

### Script Integration in Review Flow

```
AI receives: "review this PR"

Step 1 (zero-token):
  $ bash scripts/detect.sh > /tmp/detect.json
  $ bash scripts/list-changed.sh > /tmp/files.json
  $ bash scripts/select-rules.sh < /tmp/detect.json > /tmp/rules.txt
  $ bash scripts/worker-dispatch.sh < /tmp/rules.txt > /tmp/workers.json
  $ bash scripts/diff-context.sh > /tmp/context.json
  $ bash scripts/ci-local.sh > /tmp/ci.json

Step 2 (AI reads pre-computed JSON):
  - Reads /tmp/workers.json → knows how many workers to launch
  - Reads /tmp/context.json → knows what changed without reading full files
  - Reads /tmp/ci.json → knows CI issues to address first

Step 3 (parallel workers — each gets minimal context):
  - Worker N receives: its INDEX.md + rule files + relevant diffs from context.json

Step 4 (merge + format — zero-token):
  $ bash scripts/format-findings.sh < /tmp/raw-findings.yaml > /tmp/findings.md

Step 5 (AI presents findings one-by-one from formatted markdown)
```

Token savings: Steps 1, 4 cost zero AI tokens. Step 2 reads structured JSON instead of raw files. Step 3 workers get only their rules. Total estimated savings: **60-70% fewer tokens** vs current approach.

---

## Architecture Efficiency Refinements

Based on research into multi-agent orchestration, Claude Code subagent best practices, and static analysis architectures.

Sources:
- [Claude Code Sub-Agents: Parallel vs Sequential Patterns](https://claudefa.st/blog/guide/agents/sub-agent-best-practices)
- [Optimizing Agentic Workflows using Meta-tools](https://arxiv.org/html/2601.22037v2)
- [The 2026 Guide to Agentic Workflow Architectures](https://www.stackai.com/blog/the-2026-guide-to-agentic-workflow-architectures)
- [AI Agent Token Budget Management](https://www.mindstudio.ai/blog/ai-agent-token-budget-management-claude-code)

### Principle 1: Shell Pre-computation Over AI Reasoning

Every discoverable fact computed by shell scripts BEFORE AI touches it. AI receives structured JSON — never raw files for discovery.

```
ZERO-TOKEN LAYER (shell scripts)
┌─────────────────────────────────┐
│ detect.sh → brand/project/stack │
│ list-changed.sh → files         │
│ select-rules.sh → rule paths    │
│ worker-dispatch.sh → assignments│
│ diff-context.sh → structured    │
│ ci-local.sh → CI issues         │
└──────────────┬──────────────────┘
               │ JSON
AI DISPATCHER (reads JSON, ~500 tokens)
               │ rule files + diffs
PARALLEL WORKERS (each ~1500 tokens input)
               │ raw YAML findings
ZERO-TOKEN (format-findings.sh)
               │ formatted markdown
AI PRESENTER (one-by-one to user)
```

### Principle 2: Rule File Token Budget

Each rule file: **under 200 tokens** essential content. Dense, scannable, zero filler:

```markdown
---
id: rule-u-ada-004
title: Heading Hierarchy
severity: HIGH
tags: heading, h1, h2, h3, wcag-1.3.1
---

No heading level skips. h1→h2→h3. Siblings at same depth = same level.

### Apply
- Any template with h1-h6 elements

### Skip
- Third-party SDK DOM

### Bad
h1 Title → h3 Section  //- skips h2

### Good
h1 Title → h2 Section A → h2 Section B  //- parallel

### Edge
When v-if hides first h2, second stays h2 (not h3).
```

~120 tokens. A worker loading 20 rules = ~2400 tokens for rules + ~2000 for diffs = **~4400 total**.

### Principle 3: Smart Worker Grouping

Launching a subagent costs ~500 tokens overhead. Rules:
- **Min 5 rules per worker.** Smaller dirs merge with nearest related.
- **Max 25 rules per worker.** Larger dirs split.
- **Max 8 workers.** Beyond 8, merge smallest.

`worker-dispatch.sh` handles merging automatically.

### Principle 4: Model Routing Per Worker

| Role | Model | Cost |
|---|---|---|
| Dispatcher | Opus | Complex reasoning |
| Rule workers | Sonnet | Pattern matching (5x cheaper) |
| Format script | Shell | Zero tokens |
| Presenter | Opus | User interaction |

**~70% cost reduction** vs all-Opus.

### Principle 5: Two-Pass Rule Selection

**Pass 1 (shell):** Directory-level based on detection tiers.
**Pass 2 (shell):** Tag-based relevance filtering — each rule's `tags` frontmatter matched against keywords extracted from diff context.

```bash
# Diff contains: "aria-labelledby", "v-if", "h2"
# rule-u-ada-004 tags: "heading, h1, h2, h3, wcag-1.3.1"
# "h2" matches → INCLUDE
# rule-u-ada-009 tags: "aria-expanded, toggle"
# No match → EXCLUDE
```

Reduces 20-rule directory to ~8 relevant rules. **60% fewer tokens per worker.**

### Principle 6: Incremental Review Cache

```
.tasks/code-review-cache/
├── useGoogleSignIn.js.sha256     # content hash
├── useGoogleSignIn.js.findings   # cached YAML
```

If file hash matches cache → skip re-review. Only re-review files that changed since last review. **75% savings on iterative reviews.**

### Principle 7: CI-First — Deterministic Before Semantic

Run `ci-local.sh` FIRST. Present deterministic issues (lint, tests) to user before launching AI workers. Prevents AI from re-discovering what a shell script already found.

```
1. ci-local.sh → 2 lint errors found
2. User fixes lint errors
3. NOW launch AI workers for semantic review
```

---

## Review Workflow — Execution Order

The review flow has 6 stages. Each stage feeds the next. No stage can be skipped. The output of each stage is reusable — if the user asks for a re-review after fixing, only stages 4-6 re-run (stages 1-3 are cached).

```
STAGE 1: DISCOVERY (shell, zero tokens)
  ↓ produces: detection.json, files.json, context.json
STAGE 2: CI GATE (shell, zero tokens)
  ↓ produces: ci-results.json — deterministic issues
STAGE 3: TRIAGE (shell, zero tokens)
  ↓ produces: workers.json — rule selection + worker assignments
STAGE 4: AI REVIEW (parallel workers, Sonnet)
  ↓ produces: raw-findings.yaml
STAGE 5: FORMAT (shell, zero tokens)
  ↓ produces: findings.md — sorted, deduplicated, formatted
STAGE 6: RESOLUTION (AI presenter, Opus)
  ↓ produces: decisions (implement/skip per finding)
```

### Stage 1: Discovery — What Are We Reviewing?

**Runs:** `detect.sh` + `list-changed.sh` + `diff-context.sh` + `component-tree.sh`

**Output:** 4 JSON files describing the project, changed files, diffs, and component relationships.

```bash
bash scripts/detect.sh > /tmp/cr-detect.json
bash scripts/list-changed.sh > /tmp/cr-files.json
bash scripts/diff-context.sh > /tmp/cr-context.json
# component-tree.sh runs only for .vue files in the changed list
```

**Why first:** Everything downstream depends on knowing WHAT changed and WHERE. Detection determines which rules load. Context determines which rules are relevant. Component tree determines cross-file implications.

**Reusable:** These files don't change until the code changes. Re-reviews after fixing a finding skip this stage entirely — read from cache.

### Stage 2: CI Gate — Fix Deterministic Issues First

**Runs:** `ci-local.sh` (discovers workflows, runs lint + tests locally)

**Output:** `ci-results.json` with errors/warnings per workflow.

```bash
bash scripts/ci-local.sh > /tmp/cr-ci.json
```

**Decision point:** If CI has errors:
```
AI reads ci-results.json → presents errors to user:
  "2 lint errors found (JSDoc block description on lines 40, 98).
   Fix these before semantic review? [y/n]"

User fixes → re-run ci-local.sh → passes → proceed to Stage 3
```

**Why second:** Deterministic issues (lint, type errors, test failures) are cheaper to find via shell than via AI workers. Fixing them first means AI workers don't waste tokens re-flagging the same issues. Common pattern: a CI linter flags JSDoc errors that the AI review also catches — double the token cost for the same finding.

**Reusable:** If user fixes lint but not tests, only re-run the failed workflow. `ci-local.sh` supports `--workflow pilkolint` to target a specific check.

### Stage 3: Triage — Which Rules, Which Workers?

**Runs:** `select-rules.sh` (two-pass) + `worker-dispatch.sh`

**Output:** `workers.json` with exact rule files and diffs per worker.

```bash
bash scripts/select-rules.sh --context /tmp/cr-context.json < /tmp/cr-detect.json > /tmp/cr-rules.txt
bash scripts/worker-dispatch.sh --context /tmp/cr-context.json < /tmp/cr-rules.txt > /tmp/cr-workers.json
```

**Two-pass selection detail:**

Pass 1 — directory selection (from detection):
```
detect.json says: brand=acme, project=acme-webapp, techStack=[vue3, vue3-composition]
→ Collect: universal/** + framework/vue3/** + framework/vue3-composition/** + project/acme-webapp/**
→ Result: 15 directories, ~100 rule files
```

Pass 2 — tag relevance filtering (from diff context):
```
context.json says diff keywords: [aria-labelledby, v-if, h2, role-alert, trackEvent, button, :deep, flex, padding]
→ For each rule file, extract tags from frontmatter
→ Match tags against diff keywords (word-boundary grep)
→ Keep only rules where ≥1 tag matches
→ Result: ~45 rule files (55% filtered out)
```

Worker grouping:
```
45 rules across 12 directories → group to 6 workers (min 5, max 25 per worker)
→ Worker 1: universal/ada (12 relevant of 20) 
→ Worker 2: universal/code-style + universal/script (merged, 8 relevant)
→ Worker 3: framework/vue3 + vue3-composition (merged, 7 relevant)
→ Worker 4: project/{detected}/template + styling (merged, 10 relevant)
→ Worker 5: project/{detected}/naming + images (merged, 5 relevant)
→ Worker 6: project/{detected}/components (3 relevant) → merged into Worker 5
→ Final: 5 workers, ~45 rules total
```

**Why third:** Rules must be selected AFTER CI gate passes (no point reviewing code that will change due to lint fixes). And selection needs context.json from Stage 1.

**Reusable:** If user fixes a finding and asks to re-check, re-run ONLY the worker whose rule produced the finding — don't re-dispatch all workers.

### Stage 4: AI Review — Parallel Workers

**Runs:** N parallel subagents (Sonnet model), each receiving:
1. Its `INDEX.md` (category context + worker instructions)
2. Its filtered rule files (only the tag-matched ones)
3. Relevant diffs from `context.json` (only files that match the worker's rule scope)

```
Each worker prompt:
  "You are a code review worker for [category]. Review these diffs against these rules.
   Output YAML per finding: { rule, file, line, severity, problem, before, after }.
   Output NO VIOLATIONS if clean."
```

**Worker isolation:** Each worker sees ONLY its rules and ONLY the relevant diffs. A styling worker doesn't see ADA rules. An ADA worker doesn't see the full file — only the diff hunks.

**Why fourth:** This is the expensive step (AI tokens). By running stages 1-3 first, we minimized what workers need to process. By filtering rules, each worker has ~8-12 rules instead of ~20. By filtering diffs, workers see only relevant hunks.

**Reusable:** Worker outputs are cached per file hash + rule set. If re-reviewing after a fix, only the affected worker re-runs. Other workers' findings are replayed from cache.

### Stage 5: Format — Merge + Deduplicate + Sort

**Runs:** `format-findings.sh`

```bash
cat /tmp/cr-worker-*.yaml | bash scripts/format-findings.sh > /tmp/cr-findings.md
```

**What it does:**
1. Merges all worker YAML outputs
2. Deduplicates (same file:line from different workers — e.g., an ADA rule and a component rule both flag the same heading)
3. Sorts by severity: CRITICAL → HIGH → MEDIUM → LOW
4. Adds pre-existing flag: if the flagged line is NOT in the diff (unchanged code), mark as "pre-existing (ad-5 minimal-touch — skip recommended)"
5. Formats into the standard finding structure with real line numbers, before/after code blocks
6. Numbers findings: "Finding 1/N"

**Why fifth:** Shell formatting is zero tokens. The AI presenter in Stage 6 receives ready-to-present Markdown — no reasoning needed to format.

**Reusable:** If adding findings from a targeted re-review, merge with the existing findings.md (update the specific finding, keep others).

### Stage 6: Resolution — One-by-One with User

**Runs:** AI presenter (Opus) reads `findings.md` and presents each finding:

```
Finding 1/7 | HIGH | rule-u-ada-004 | InfoPage.vue:20

Problem: Heading hierarchy skip — h1 → h3, missing h2.

Before:
  L20: h3.contact-info-title Contact Info

After:
  L20: h2.contact-info-title Contact Info

Implement or skip?
```

**User responds:** "implement" → AI applies fix via Edit tool → marks finding as resolved → next finding.
**User responds:** "skip" → AI records decision with reason → next finding.

**After all findings resolved:**
- Re-run `ci-local.sh` to verify fixes don't break lint/tests
- Re-run ONLY the workers whose findings were implemented (targeted re-check)
- If clean → "Review complete. N findings implemented, M skipped."

**Why last:** This is the interactive step. Everything before it is automated or parallelized. The user only engages with curated, formatted, severity-sorted findings — maximum productivity.

### Re-Review Flow (After Fixes)

When the user says "review again" or "check the fixes":

```
STAGE 1: SKIP (cached, code might have changed → quick hash check)
  → If any file hash changed: re-run diff-context.sh for THOSE files only
STAGE 2: RE-RUN ci-local.sh (quick, validates fixes)
  → If passes: proceed
  → If fails: present new CI errors first
STAGE 3: RE-TRIAGE
  → Only select rules for the CHANGED files (not all files)
  → Check cache: skip rules already reviewed on unchanged files
STAGE 4: RE-REVIEW
  → Launch ONLY workers with rules matching changed files
  → Other workers' findings: replay from cache
STAGE 5: RE-FORMAT
  → Merge new findings with cached findings
  → Remove findings that were implemented and confirmed fixed
STAGE 6: RESOLUTION
  → Present only NEW findings (not re-present resolved ones)
```

**Token savings on re-review:** ~75% less than full review. Only changed files are re-processed.

### PR Review Mode

For "review PR #123" or "review this PR" — pulls the PR diff from GitHub and runs the full review flow against it.

**Trigger:** User provides a PR number or URL. Must be in the repo (or a fork).

```
STAGE 0: PR FETCH (shell, zero tokens)
  $ bash scripts/pr-fetch.sh 20652
  ↓ produces: pr-meta.json + pr-diff (the actual changed files as if local)

STAGE 1-6: Same as local review — but using PR diff instead of git diff
```

#### `pr-fetch.sh` — Pull PR Changes for Review

```bash
# Usage: bash scripts/pr-fetch.sh <pr-number> [--repo owner/repo]
# Output: pr-meta.json + writes changed file diffs to /tmp/cr-pr-diff/
{
  "pr": 20652,
  "title": "[DOTCOMPB-7942]: Add Google Sign-On",
  "author": "Kyonax",
  "base": "master",
  "head": "DOTCOMPB-7942",
  "state": "open",
  "reviewers": ["andris310"],
  "labels": ["DOTCOM TEAM"],
  "files_changed": 9,
  "additions": 450,
  "deletions": 120,
  "files": [
    { "path": "src/composables/useAuth.js", "status": "modified", "additions": 30, "deletions": 5 }
  ],
  "existing_reviews": [
    { "author": "claude", "state": "COMMENTED", "findings": 6 },
    { "author": "mrminionbot", "state": "COMMENTED", "findings": 5 },
    { "author": "codecov", "state": "COMMENTED" }
  ],
  "ci_checks": [
    { "name": "PilkoLint", "status": "failure", "conclusion": "failure" },
    { "name": "tests", "status": "completed", "conclusion": "success" }
  ]
}
```

**What it does:**
1. `gh pr view <number> --json` — fetches PR metadata (title, author, base, labels, reviewers, CI checks)
2. `gh pr diff <number>` — fetches the actual diff
3. `gh api repos/{owner}/{repo}/pulls/{number}/comments` — fetches existing review comments
4. `gh api repos/{owner}/{repo}/pulls/{number}/reviews` — fetches review summaries
5. Parses existing reviews to avoid re-flagging issues already identified by bots
6. Parses CI check status to identify deterministic failures
7. Writes structured metadata to `pr-meta.json`
8. Writes the diff to `/tmp/cr-pr-diff/` in the same format `diff-context.sh` expects

**Key difference from local review:** The PR diff comes from GitHub API, not local `git diff`. Everything else is identical — same detection, same rules, same workers.

#### PR-Aware Enhancements

**De-duplication against existing reviews:**

`select-rules.sh` receives `pr-meta.json` and checks `existing_reviews`. If a bot already flagged a specific issue (e.g., "Missing JSDoc block description"), the script can:
- Tag the finding as `already-flagged-by: {reviewer}` 
- The presenter groups these separately: "Already identified by other reviewers (N findings)" — collapsed by default
- New findings from our review are presented first

**CI-aware triage:**

If `ci_checks` shows failures, Stage 2 (CI Gate) reads those instead of running `ci-local.sh`:
```
ci_checks says: PilkoLint = failure
→ Skip ci-local.sh (CI already ran remotely)
→ Fetch PilkoLint's inline comments from existing_reviews
→ Present: "CI found 2 lint errors (already commented by PilkoLint). Fix first?"
```

**Report mode vs Interactive mode:**

| Mode | Trigger | Output |
|---|---|---|
| Interactive | "review PR #123" | One-by-one findings, implement/skip |
| Report | "audit PR #123" or "report PR #123" | Full markdown report, no code changes |

Report mode skips Stage 6 (Resolution) and instead outputs a structured report:

```markdown
# Code Review Report — PR #20652

## Summary
- **Files reviewed:** 9
- **Rules checked:** 45 (across 5 workers)
- **Findings:** 7 new + 11 already flagged by other reviewers
- **Severity breakdown:** 2 HIGH, 3 MEDIUM, 2 LOW

## New Findings (not yet flagged)

### Finding 1 | HIGH | rule-u-ada-004 | InfoPage.vue:20
Heading hierarchy skip — h1 → h3, missing h2.
...

## Already Flagged (by other reviewers)

<details>
<summary>11 findings already identified (click to expand)</summary>

| Reviewer | Finding | File | Status |
|---|---|---|---|
| claude | Double tracking event | InfoPage.vue:219 | Unresolved |
| mrminionbot | Focus lost after sign-in | SignInOptions.vue:59 | Unresolved |
| PilkoLint | Missing JSDoc description | useGoogleSignIn.js:40 | Unresolved |
...

</details>

## CI Status

| Check | Status | Details |
|---|---|---|
| PilkoLint | ❌ Failed | 2 lint errors |
| Tests | ✅ Passed | 55/55 |
| Security | ✅ Passed | |

## Recommendation
Fix the 2 HIGH findings and 2 CI lint errors before merge.
```

This report can be:
- Saved to `.tasks/code-review-reports/PR-20652.md`
- Posted as a PR comment (if user requests)
- Used as input for a follow-up interactive review ("let's fix the HIGH findings")

#### PR Review Flow Integration

```
User: "review PR #20652"

STAGE 0 (zero-token):
  $ bash scripts/pr-fetch.sh 20652 > /tmp/cr-pr-meta.json

STAGE 1 (zero-token — uses PR diff instead of local diff):
  $ bash scripts/detect.sh > /tmp/cr-detect.json
  $ bash scripts/diff-context.sh --pr-diff /tmp/cr-pr-diff/ > /tmp/cr-context.json

STAGE 2 (zero-token — reads CI from PR metadata):
  → pr-meta.json says PilkoLint failed
  → Present CI failures first: "Fix 2 lint errors before full review?"

STAGE 3-6: Same as local review

BONUS: De-duplicate against existing PR comments
  → 11 findings already flagged → grouped separately
  → Only NEW findings presented for action
```

### Emergency/Quick Review Mode

For "just check this one file" or "quick review":

```
STAGE 1: detect.sh + diff-context.sh (one file only)
STAGE 2: SKIP ci-local.sh (user trusts their lint)
STAGE 3: select-rules.sh → tag-filter for ONE file's diff keywords
         → Typically 10-15 relevant rules
STAGE 4: SINGLE worker (Sonnet, inline — no subagent overhead)
STAGE 5: format inline (AI formats, ~100 tokens)
STAGE 6: Present findings
```

Total: ~3000 tokens. Fast, focused, minimal overhead.

---

## Migration Path

1. Old `mr-review-checklist.md` → keep as read-only archive, then delete
2. Old `brand-kyonax.md` → keep as read-only archive, then delete
3. SKILL.md → full rewrite (backward-incompatible, intentional)
4. Session files referencing old rules → update to new rule IDs

---

## Metrics

| Metric | Current | Target |
|---|---|---|
| Rule files | 9 monolithic | ~100 atomic (one per rule) |
| Avg tokens per worker | ~5000 | ~1500 (two-pass selection) |
| Detection tiers | 2 (brand + tech) | 3 (brand + project + tech) |
| Max workers | 8 (hardcoded) | 4-8 (adaptive, auto-merged) |
| Projects supported | 2 (MR + Kyonax) | Unlimited (generic fallback) |
| Framework support | Implicit | Explicit (Vue 3, React, Express) |
| Token cost per review | ~40K (all Opus) | ~12K (Sonnet workers + shell pre-compute) |
| Iterative re-review | Full re-process | Cache-based, ~25% of original |
| CI issue detection | AI discovers | Shell pre-computes, AI skips |

---

## Completion Checklist (2026-04-24)

### Phase 1: Scaffolding — COMPLETE
- [x] Directory tree created: `universal/`, `framework/`, `brand/`, `project/`, `detection/`, `scripts/`
- [x] `AGENTS.md` written — architecture rationale, worker protocol, extension guide
- [x] `RULE_TEMPLATE.md` written — canonical frontmatter schema, body sections, tag guide, token budget

### Phase 2: Universal Rules (45 rules) — COMPLETE
- [x] `universal/ada/` — 21 rule files + INDEX.md (plan: 20, actual: 21 — `deep-override-comment.md` moved here from framework)
- [x] `universal/code-style/` — 6 rule files + INDEX.md
- [x] `universal/script/` — 9 rule files + INDEX.md
- [x] `universal/mobile/` — 5 rule files + INDEX.md
- [x] `universal/third-party-sdk/` — 5 rule files + INDEX.md
- [x] Brand isolation verified: zero MR-specific references (MrBtn, ImgBox, Stylus, Pug, trackMREvent) in universal tier

### Phase 3: Framework Rules (17 rules) — COMPLETE
- [x] `framework/vue3/` — 11 rule files + INDEX.md
- [x] `framework/vue3-composition/` — 3 rule files + INDEX.md
- [x] `framework/express/` — 3 rule files + INDEX.md
- [x] Brand isolation verified: zero project-specific paths or component names

### Phase 4: Brand Rules (21 rules) — COMPLETE
- [x] `brand/kyonax/` — 21 rule files + INDEX.md
- [x] All Kyonax/OBS/HUD references preserved (correct tier)

### Phase 5: Project Rules (52 rules) — COMPLETE
- [x] `project/mr-dotcom/template/` — 6 rule files + INDEX.md
- [x] `project/mr-dotcom/styling/` — 13 rule files + INDEX.md
- [x] `project/mr-dotcom/naming/` — 5 rule files + INDEX.md
- [x] `project/mr-dotcom/images-tracking/` — 8 rule files + INDEX.md
- [x] `project/mr-dotcom/components/` — 8 rule files + INDEX.md
- [x] `project/mr-backend/` — 12 rule files + INDEX.md
- [x] All MR-specific content preserved (Pug, Stylus, MrBtn, ImgBox, trackMREvent, design system vars)

### Phase 6: Detection Files (3) — COMPLETE
- [x] `detection/brand.md` — git remote URL parsing, user override, repo-local indicators
- [x] `detection/project.md` — changed file path matching, multi-project support
- [x] `detection/tech-stack.md` — package.json deps, file extensions, config files, Composition API detection

### Phase 7: Scripts (11) — COMPLETE
- [x] `scripts/detect.sh` — 3-tier detection → JSON
- [x] `scripts/list-changed.sh` — changed files with metadata → JSON
- [x] `scripts/pr-fetch.sh` — PR metadata + diff + existing reviews → JSON
- [x] `scripts/select-rules.sh` — two-pass rule selection → file paths
- [x] `scripts/worker-dispatch.sh` — rule grouping → worker assignment JSON
- [x] `scripts/diff-context.sh` — structured diff + imports + store deps → JSON
- [x] `scripts/component-tree.sh` — Vue component hierarchy → JSON
- [x] `scripts/lint-changed.sh` — ESLint on changed files → JSON
- [x] `scripts/test-changed.sh` — test runner on changed files → JSON
- [x] `scripts/ci-local.sh` — discover + run CI workflows locally → JSON
- [x] `scripts/format-findings.sh` — raw YAML → formatted findings markdown

### Phase 8: SKILL.md Rewrite — COMPLETE
- [x] Entry-point description, trigger keywords
- [x] 3-tier detection flow with step-by-step
- [x] Complete rule catalog (135 rules across 4 tiers, 15 directories)
- [x] Worker dispatch protocol (min 5 / max 25 rules, max 8 workers)
- [x] Review modes (full, PR, PR audit, quick, scoped)
- [x] 6-stage review workflow documented
- [x] Output format (finding structure, severity levels)

### Phase 9: Validation — COMPLETE
- [x] File count: 135 rule files (target: 135)
- [x] INDEX files: 15 (one per directory)
- [x] Zero duplicate rule IDs
- [x] Zero brand leaks in universal/framework tiers
- [x] Tag coverage: all rules have ≥2 code-greppable tags

### Phase 10: Cleanup — COMPLETE
- [x] Old monolithic files archived to `rules/_archive/` (9 files)
- [x] Top-level docs updated (SKILL.md, AGENTS.md, RULE_TEMPLATE.md)

### Final Counts

| Category | Plan | Actual | Status |
|---|---|---|---|
| Rule files | 135 | 135 | Match |
| INDEX.md files | 15 | 15 | Match |
| Scripts | 11 | 11 | Match |
| Detection files | 3 | 3 | Match |
| Top-level docs | 3 | 3 | Match |
| Archived old files | 9 | 9 | Match |
| **Total deliverables** | **176** | **176** | **Complete** |

### Remaining / Not Implemented

| Item | Status | Notes |
|---|---|---|
| Incremental review cache (Principle 6) | NOT BUILT | Documented in plan. `.tasks/code-review-cache/` file hash → cached findings. Build when iterative reviews become a real workflow |
| End-to-end live test on real PR | NOT RUN | Scripts created, structure validated. Full pipeline (detect → select → dispatch → workers → format → present) not tested on a live PR yet |
| `kyonax-obs-hud/` project directory | PLACEHOLDER | `README.md` only — says "Load all brand/kyonax/ rules. No additional project rules." No OBS-specific project rules exist yet |
| Old archive deletion | DEFERRED | `rules/_archive/` has 9 old files. Safe to delete after confirming end-to-end flow works. No active references depend on them |
| Session file cross-reference updates | DEFERRED | Other session files may reference old rule IDs (e.g., `mr-review-checklist.md` rule numbers). Low priority — old files still exist in archive |

---

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
