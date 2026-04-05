---
title: Madison Reed Code Review Checklist (45 Rules + Andris Patterns)
impact: HIGH
impactDescription: The definitive checklist for reviewing Vue components in the Madison Reed website. Combines session-validated rules with Andris review patterns extracted from 56 PR comments.
tags: code-review, checklist, rules, andris, vue, pug, stylus, vuex, accessibility, naming, tracking, styling, template, script
---

## Code Review Checklist (45 Rules)

Reference this table when running `/code-review` on any component in the MR website. Each row maps to a source rule. Load the referenced skill/section before reviewing.

| # | Category | Pattern | Source |
|---|---|---|---|
| 1 | **Template** | All templates use Pug (`lang="pug"`), no HTML | `mr-dotcom-dev/rules/pug-templates.md` |
| 2 | **Template** | Vue components in kebab-case in Pug (`mr-btn`, not `MrBtn`) — project uses PascalCase for local refs (skip) | `mr-dotcom-dev/rules/pug-templates.md` |
| 3 | **Template** | Heading inline text (`h2.classes Title`), not multi-line `\|` | Session §1.2 |
| 4 | **Template** | `v-if` guards on all sections/images where data may be null | Session §1.3 |
| 5 | **Script** | Options API only — no Composition API, no `<script setup>` | Session §1.1 |
| 6 | **Script** | Canonical field order: `name -> components -> emits -> props -> data -> computed -> watch -> lifecycle -> methods` | Session §1.3 |
| 7 | **Script** | `emits` declared explicitly when component emits events | Session §1.3 |
| 8 | **Script** | Computed properties alphabetized | Session §1.3 |
| 9 | **Script** | Vuex helpers at top of `computed`/`methods` | andris-10 |
| 10 | **Script** | No unused imports, methods, computed, data | Session §1.3 |
| 11 | **Script** | Optional chaining (`?.`) always — no `getObjProperty` | Session §1.3 |
| 12 | **Script** | Module-level constants for static data | Session §1.3 |
| 13 | **Script** | No inline event logic — `@click` calls a single method | Session §1.3 |
| 14 | **Styling** | Scoped Stylus (`style scoped lang="stylus"`) | Session §1.2 |
| 15 | **Styling** | Utility-first for padding, margin, color, font-size, text-align, border-radius, font-weight, text-transform. **Padding/margin utility classes MUST use breakpoint prefix** (`.xs-pt-50m` not `.pt-50m`). Mobile-first: always use `xs-` as the base. | Session §1.2 |
| 16 | **Styling** | Do NOT use utility classes for flex layout — keep in Stylus | Memory: `feedback_no_flex_utility.md` |
| 17 | **Styling** | `.max-at-tweak` mandatory on every responsive font class | Session §1.2 |
| 18 | **Styling** | Design system variables for colors — no hardcoded hex unless no match | Session §1.2 |
| 19 | **Styling** | CSS properties alphabetized within style blocks | Session §1.2 |
| 20 | **Styling** | No padding/margin in Stylus when utility handles it. Stylus only for `@media` overrides | Session §1.2 |
| 21 | **Styling** | Stylus nesting mirrors template hierarchy | `mr-style` |
| 22 | **Naming** | Root class: short component prefix | Session §1.14 |
| 23 | **Naming** | Remove redundant prefixes from parent context | Session §1.14 |
| 24 | **Naming** | Card children: `{parent}-{element}` pattern | Session §1.15 |
| 25 | **Naming** | Grid columns: `{role}-column` | Session §1.15 |
| 26 | **Naming** | No BEM `__` or `--` — hyphens only | `mr-style` |
| 27 | **ADA** | Self-contained landmarks in same component | Session §1.5 |
| 28 | **ADA** | `aria-hidden="true"` on decorative images adjacent to text | Session §1.5 |
| 29 | **ADA** | `role="list"` on `<ul>` with `list-style: none` | ADA review finding |
| 30 | **ADA** | Heading levels: no jumps (`h1` -> `h2` -> `h3`) | WCAG 1.3.1 |
| 31 | **ADA** | `:focus-visible` outline on custom interactive elements | Session §1.5 |
| 32 | **ADA** | No nested interactive elements | Session §1.5 |
| 33 | **ADA** | No duplicate landmarks when nested | ADA review finding |
| 34 | **Images** | Use `ImgBox` for all images — no raw `<img>` | Session §1.8 |
| 35 | **Images** | Skeleton via `:deep(.image-box)` with `background-color ui-color-4` | Session §1.8 |
| 36 | **Images** | `:deep(img)` for aspect-ratio, border-radius, object-fit | Session §1.8 |
| 37 | **Tracking** | `trackMREvent` for fire-and-forget, `trackMREventAndRedirect` for navigation | Session §1.7 |
| 38 | **Tracking** | Every template method must be traceable | andris-4 |
| 39 | **Code Style** | One blank line between logical blocks | andris-3 |
| 40 | **Code Style** | No info-only comments | andris-2 |
| 41 | **Code Style** | Minimal-touch on unrelated code | andris-5 |
| 42 | **MrBtn** | Use variants (`secondary`, `tertiary`, `light`) before custom CSS | andris-8 |
| 43 | **MrBtn** | `:deep(.mrbtn)` for color overrides | Code review pattern |
| 44 | **Components** | Shared components at appropriate shared level | Decision 37 |
| 45 | **Components** | Thin wrapper pattern: mobile wrapper + shared content | Architecture decision |

## Andris Review Patterns (22 Rules)

Extracted from 56 review comments by `andris310` across 10 PRs (#19523-#20064). 34 comments (61%) were already covered by the 45-rule checklist. These 22 are patterns not covered elsewhere.

### Frontend Architecture & Templates

| # | Pattern |
|---|---|
| andris-6 | Single `<h1>` per page. Section components use `<h2>` or lower. Components must never assume they own the page title. |
| andris-7 | Omit redundant `div` in Pug. `.my-class` auto-creates a `<div>`. Only include the tag name for non-div elements (`section.my-class`, `span.label`). |
| andris-8 | Use existing MrBtn variants. `MrBtn` default uses `cta-color-1`. Before adding custom button CSS, check if `secondary`, `tertiary`, or `light` variant already works. |
| andris-12 | Extract, don't bloat. When adding new functionality, create a standalone component instead of cramming mode flags into an existing one. |
| andris-15 | Keep simple expressions on one line. Don't break trivial return statements or short values across multiple lines. |

### Code Quality & Style

| # | Pattern |
|---|---|
| andris-2 | No info-only comments. Don't add comments that merely describe what code does. Keep only: `// TODO:` with ticket refs, `// eslint-disable`, explanations of genuinely non-obvious logic. |
| andris-3 | One blank line between logical blocks. Exactly one blank line between methods, computed properties, data fields, template sections, and style rules. No consecutive blanks, no zero-separation. |
| andris-4 | Template methods must be traceable. Every method called in the Pug template must be defined in `methods`, mapped via `mapActions`/`mapMutations`, or be a known global mixin method. |
| andris-5 | No formatting-only changes in unrelated files. Don't include auto-lint/format corrections on files with no functional changes. |
| andris-20 | Clean programmatic strings. No whitespace in programmatic string values (error codes, flags). If a string is used once and is self-explanatory, inline it. |

### Component Design

| # | Pattern |
|---|---|
| andris-10 | Vuex helpers at top of sections. `...mapState()`/`...mapGetters()` go first in `computed`. `...mapActions()`/`...mapMutations()` go first in `methods`. Local definitions follow, alphabetized. |
| andris-11 | No hardcoded route paths in reusable components. Use `$route.path`, a prop, or a URL key. Hardcoded routes create hidden coupling. Exception: page-specific (non-reusable) components. |
| andris-14 | Styles inside the SFC. All Vue component styles live in `<style scoped lang="stylus">`. No external `.styl`/`.css` files. |
| andris-16 | Comment third-party CSS overrides. When overriding library classes (Swiper, Google Maps, Stripe) in `<style>`, add a comment identifying the library. |
| andris-22 | Use existing validation systems. When a component has Vuelidate or another validation system, integrate new rules into it. Don't add a parallel error display mechanism. |

### Accessibility & UX

| # | Pattern |
|---|---|
| andris-1 | Meaningful alt text. Never use `alt=""` unless the image is purely decorative. Content images must have descriptive alt text. |
| andris-17 | Verify error messages match user intent. Before implementing error messages, verify the copy aligns with the user's action context. |

### Backend / API Patterns

| # | Pattern |
|---|---|
| andris-18 | No `console.error` on backend. Use `log.error()` for server logging and return structured error responses. |
| andris-19 | Backend returns error codes, not HTML. Return structured codes (`MISSING_INFO`, `UPDATE_ERROR`) and let the Vue component handle display. |
| andris-21 | Never string-match response messages. Don't check `response.message.includes('...')` for control flow. Use `response.success`, `response.errorCode`, or HTTP status codes. |

### Images & Assets

| # | Pattern |
|---|---|
| andris-9 | No static images in the codebase. Never commit PNG/JPG/GIF to the repo. Use CMS Media Gallery (Tophat) for all images. Only exception: SVG icons in `src/assets/svg-icons/`. |

### Experiments

| # | Pattern |
|---|---|
| andris-13 | Track experiment exposure explicitly. When implementing A/B experiments, explicitly call tracking when the customer sees a variant. Branching logic alone is not enough. |
