---
title: Madison Reed Vue Component Review Checklist — 84 Rules Across 9 Categories
impact: HIGH
impactDescription: The definitive checklist for reviewing Vue components in the MR website. Without it, reviews miss project-specific conventions validated through 56+ PR comments, 10+ code review cycles, and session-graduated patterns.
tags: code-review, checklist, vue, pug, stylus, vuex, accessibility, naming, tracking, styling, template, script, components, mrbtn, backend, madison-reed, options-api, utility-classes, aria, keyboard, responsive, breakpoint, spacing
---

Definitive checklist for reviewing Vue components in the Madison Reed website. 84 rules organized into 9 categories that map to the parallel subagent review flow in SKILL.md. Each category is self-contained.

Rules `1-45` are core checklist. Rules `ad-*` are senior reviewer patterns (56 PR comments, 10 PRs). Rules `sg-*` are session-graduated patterns (validated across 10+ components, 3+ code review cycles).

## Categories

| # | Category           | Rules | Section                                    |
|---|--------------------|-------|--------------------------------------------|
| 1 | Template           | 7     | [Template](#1-template)                    |
| 2 | Script Structure   | 17    | [Script Structure](#2-script-structure)    |
| 3 | Styling            | 13    | [Styling](#3-styling)                      |
| 4 | Naming             | 5     | [Naming](#4-naming)                        |
| 5 | ADA Accessibility  | 13    | [ADA Accessibility](#5-ada-accessibility)  |
| 6 | Images + Tracking  | 8     | [Images + Tracking](#6-images--tracking)   |
| 7 | Code Style         | 6     | [Code Style](#7-code-style)                |
| 8 | MrBtn + Components | 8     | [MrBtn + Components](#8-mrbtn--components) |
| 9 | Backend / API      | 4     | [Backend / API](#9-backend--api)           |

---

## 1. Template

| Rule | Pattern                                                         |
|------|-----------------------------------------------------------------|
| 1    | All templates use Pug (`lang="pug"`), no HTML                   |
| 2    | Vue components in kebab-case in Pug (`mr-btn` not `MrBtn`)      |
| 3    | Heading text inline, not multi-line `\|`. [Details](#rule-3)    |
| 4    | `v-if` guards on all sections/images where data may be null     |
| sg-1 | Always use curly braces for `if` — no one-line conditionals     |
| ad-6 | Single `<h1>` per page — section components use `<h2>` or lower |
| ad-7 | Omit redundant `div` in Pug. [Details](#ad-7)                   |

#### Rule 3

Use inline heading text: `h2.f-secondary.upper My Title`

**Incorrect:** `h2.f-secondary.upper` + newline + `| My Title`
**Correct:** `h2.f-secondary.upper My Title`

Dynamic interpolation: `h2.upper about {{ location.name }}`

#### AD-7

`.my-class` auto-creates a `<div>`. Only include the tag name for non-div elements.

**Incorrect:** `div.my-class`
**Correct:** `.my-class`
**Non-div:** `section.my-class`, `span.label`

---

## 2. Script Structure

| Rule  | Pattern                                                             |
|-------|---------------------------------------------------------------------|
| 5     | Options API only — no Composition API, no `<script setup>`          |
| 6     | Canonical field order. [Details](#rule-6)                           |
| 7     | `emits` declared explicitly when component emits events             |
| 8     | Computed properties alphabetized                                    |
| 9     | Vuex helpers at top of sections. [Details](#rule-9)                 |
| 10    | No unused imports, methods, computed, data                          |
| 11    | Optional chaining (`?.`) always — no `getObjProperty`               |
| 12    | Module-level constants for static data. [Details](#rule-12)         |
| 13    | No inline event logic — `@click` calls a single method              |
| sg-2  | Props API before `:deep()` for child styling                        |
| sg-3  | Parent prepares data in computed before passing props               |
| sg-4  | `window.resize` forbidden — use `matchMedia`. [Details](#sg-4)      |
| sg-5  | Centralized `global/isDesktop` for 960px+. [Details](#sg-5)         |
| ad-4  | Every Pug method must trace to `methods`, `mapActions`, or a mixin  |
| ad-15 | Keep simple expressions on one line — no multi-line trivial returns |
| ad-23 | Extract repeated logic blocks into a reusable method. [Details](#ad-23) |
| ad-24 | Cache repeated property lookups into a `const`. [Details](#ad-24) |

#### Rule 6

`name → components → emits → props → data → computed → watch → lifecycle → methods`

#### Rule 9

`...mapState()`/`...mapGetters()` go **first** in `computed`.
`...mapActions()`/`...mapMutations()` go **first** in `methods`.
Local definitions follow, alphabetized.

#### Rule 12

Module-level `UPPER_SNAKE_CASE` constants for non-reactive data. Expose to templates via `camelCase` computed wrapper.

**Incorrect:** `v-for="i in 3"` (magic number in template)

**Correct:**
```javascript
const SKELETON_CARD_COUNT = 3;
export default {
  computed: {
    skeletonCardCount() { return SKELETON_CARD_COUNT; },
  },
};
// Template: v-for="i in skeletonCardCount"
```

#### SG-4

Use `window.matchMedia` for responsive detection. Store `MediaQueryList` in `data` (SSR-safe — `window` undefined during SSR). Add listener in `mounted`, remove in `beforeUnmount`.

**Incorrect:**
```javascript
mounted() { window.addEventListener('resize', this.checkWidth); }
```

**Correct:**
```javascript
mounted() {
  this.mobileQuery = window.matchMedia('(max-width: 559px)');
  this.mobileQuery.addEventListener('change', this.onMediaChange);
}
```

#### SG-5

For show/hide at the standard desktop threshold (960px+), use `mapGetters('global', ['isDesktop'])`. Only use local `matchMedia` when the breakpoint differs from 960px.

#### AD-23

When the same logic block appears 2+ times in a method (e.g., deduplication, filtering, mapping), extract it into a named method. The method should be self-explanatory without a comment.

**Incorrect:**
```javascript
(productType.products || []).forEach(product => {
  if (!seen.has(product.id)) {
    seen.add(product.id);
    products.push(product);
  }
});
(subCategory.products || []).forEach(product => {
  if (!seen.has(product.id)) {   // same block repeated
    seen.add(product.id);
    products.push(product);
  }
});
```

**Correct:**
```javascript
addUniqueProducts(source, target, seen) {
  (source || []).forEach(product => {
    if (!seen.has(product.id)) {
      seen.add(product.id);
      target.push(product);
    }
  });
},
// Usage:
this.addUniqueProducts(productType.products, products, seen);
this.addUniqueProducts(subCategory.products, products, seen);
```

#### AD-24

When a method accesses the same nested property chain 2+ times, cache it in a `const`. Reduces visual noise, improves readability, and avoids repeated optional chaining.

**Incorrect:**
```javascript
const image = product?.imagery?.toneSwatch
  || product?.imagery?.alternative
  || product?.imagery?.standard;
```

**Correct:**
```javascript
const imagery = product?.imagery;
const image = imagery?.toneSwatch || imagery?.alternative || imagery?.standard;
```

---

## 3. Styling

| Rule  | Pattern                                                                 |
|-------|-------------------------------------------------------------------------|
| 14    | Scoped Stylus (`<style scoped lang="stylus">`)                          |
| 15    | Utility-first with breakpoint prefixes. [Details](#rule-15)             |
| 16    | Do NOT use utility classes for flex — keep flex layout in Stylus        |
| 17    | `.max-at-tweak` mandatory on every responsive font class                |
| 18    | Design system variables for colors — no hardcoded hex unless no match   |
| 19    | CSS properties alphabetized within style blocks                         |
| 20    | No padding/margin in Stylus when a utility class handles it             |
| 21    | Stylus nesting mirrors template hierarchy                               |
| sg-6  | Font sizes, font-family, text color in templates only. [Details](#sg-6) |
| sg-7  | `.upper` mandatory on every `.f-secondary` heading                      |
| sg-8  | Units: `rem`/`em` for spacing, `px` for borders/shadows only            |
| ad-14 | All styles inside the SFC — no external `.styl`/`.css` files            |
| ad-16 | Comment third-party CSS overrides (Swiper, Maps, Stripe)                |

#### Rule 15

Utility-first: if a utility class exists for a CSS property, use it instead of Stylus. Stylus only for structural layouts, pseudo-elements, transitions, `:deep()`, and custom media queries.

**Common non-spacing replacements:**

| Stylus                  | Utility                  |
|-------------------------|--------------------------|
| `overflow hidden`       | `.overflow-hidden`       |
| `width 100%`            | `.full-width`            |
| `height 100%`           | `.full-height`           |
| `flex 1`                | `.flex-1`                |
| `margin 0 auto`         | `.div-center`            |
| `font-weight 600/700`   | `.semi-bold` / `.bold`   |
| `display inline-block`  | `.inline-block`          |
| `flex-direction column` | Keep in Stylus (Rule 16) |

#### Rule 15a: Padding/Margin Breakpoint Prefix Rule

**All padding/margin utility classes MUST use a breakpoint prefix.** The unprefixed form (`.pt-50m`) and the `xs-` form (`.xs-pt-50m`) are both generated inside `@media mq-mobile-plus` and are functionally equivalent — but `xs-` is mandatory for consistency and explicit mobile-first intent.

**Breakpoint prefixes:**

| Prefix | Media Query          | Breakpoint                                 |
|--------|----------------------|--------------------------------------------|
| `xs-`  | `mq-mobile-plus`     | Base / mobile (always use this as default) |
| `sm-`  | `mq-tablet-plus`     | 560px+                                     |
| `md-`  | `mq-desktop-md-plus` | 760px+                                     |
| `lg-`  | `mq-desktop-plus`    | 960px+                                     |
| `xl-`  | `mq-max`             | 1200px+                                    |

**Available properties:**

| Type        | Prefixes                                       | Example                      |
|-------------|------------------------------------------------|------------------------------|
| Individual  | `pt`, `pr`, `pb`, `pl`, `mt`, `mr`, `mb`, `ml` | `.xs-pt-50m`, `.lg-mb-100m`  |
| Shorthand   | `py`, `px`, `my`, `mx`                         | `.xs-py-150m`, `.xl-px-400m` |
| Auto margin | `ml-auto`, `mr-auto`                           | `.xs-ml-auto`                |

**Value scale and unit calculation:**

| Values | Unit suffix | Formula | Example |
|---|---|---|---|
| 0, 10, 15, 25, 30, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 350, 400, 450, 500, 600, 700 | `m` (em) | `value / 100 = em` | `50m` = 0.5em, `100m` = 1em, `200m` = 2em |
| Same scale | `pct` (%) | `value / 10 = %` | `50pct` = 5%, `100pct` = 10% |

**Incorrect:**
```pug
//- Missing breakpoint prefix
.content.pt-50m.pb-100m.px-200m
```

**Correct:**
```pug
//- Explicit xs- prefix on all spacing utilities
.content.xs-pt-50m.xs-pb-100m.xs-px-200m
```

**Responsive override pattern:**
```pug
//- Mobile padding, larger on desktop
.section.xs-py-100m.lg-py-200m.xs-px-50m.xl-px-400m
```

#### SG-6

All `font-size`, `font-family`, and text `color` must use utility classes in Pug (`.xs-f-small`, `.bold`, `.text-color-2`). Never in `<style>` blocks — those belong to the utility class system.

#### SG-7

Kapra Neue (`.f-secondary`) is an uppercase font. Pattern: `h2.color-mr-purple.f-secondary.sm-f-xxlarge.max-at-tweak.upper`

---

## 4. Naming

| Rule | Pattern                                                                         |
|------|---------------------------------------------------------------------------------|
| 22   | Short root class prefix (`.hcb-hero-v2` not `.hair-color-bar-location-hero-v2`) |
| 23   | Remove redundant prefixes from parent context                                   |
| 24   | Card children: `{parent}-{element}` (`.service-card`, `.service-image`)         |
| 25   | Grid columns: `{role}-column` (`.main-column`, `.sidebar-column`)               |
| 26   | No BEM `__` or `--` — hyphens only                                              |

---

## 5. ADA Accessibility

| Rule  | Pattern                                                              |
|-------|----------------------------------------------------------------------|
| 27    | Self-contained landmarks — same component. [Details](#rule-27)       |
| 28    | `aria-hidden="true"` on decorative images adjacent to text           |
| 29    | `role="list"` on `<ul>` with `list-style: none`                      |
| 30    | Heading levels: no jumps (`h1 → h2 → h3`, never skip)                |
| 31    | `:focus-visible` outline on custom interactive elements              |
| 32    | No nested interactive elements (`<a>` inside `role="button"`)        |
| 33    | No duplicate landmarks when components nest                          |
| sg-9  | Keyboard nav on non-native interactives. [Details](#sg-9)            |
| sg-10 | `aria-expanded` with `!!` on toggle buttons. [Details](#sg-10)       |
| sg-11 | Dynamic `aria-label` for repeated identical links. [Details](#sg-11) |
| sg-12 | No fake interactive roles on tracking-only elements                  |
| ad-1  | Meaningful alt text — never `alt=""` unless purely decorative        |
| ad-17 | Verify error message copy matches user's action context              |

#### Rule 27

Component owns its landmark: `role="region"` + `aria-labelledby` + heading `id` in the **same template**. Parent wrappers are purely structural — NO ARIA attributes.

**Incorrect — parent owns ARIA:**
```pug
.section-wrapper(aria-labelledby="my-title")
  MyComponent
```

**Correct — component owns ARIA:**
```pug
//- MyComponent.vue
.my-component(v-if="hasData" role="region" aria-labelledby="my-section-title")
  h2#my-section-title.color-mr-purple.f-secondary Title Text
```

Key constraints:
- `aria-labelledby` → heading IDs only, never root element IDs
- Root elements use classes, not IDs
- `v-if` on root guards heading + aria together

#### SG-9

Non-native interactive elements need: `tabindex="0"` + `@keydown.enter.prevent` + `@keydown.space.prevent`.

#### SG-10

Buttons controlling expandable content: `:aria-expanded="!!stateVar"`. The `!!` coerces `undefined` → `false` so the attribute always renders.

#### SG-11

When multiple identical-text links exist (e.g., multiple "Book Now"), add context via `aria-label`:

```pug
a(:aria-label="`Book ${service.name}`") Book Now
```

---

## 6. Images + Tracking

| Rule  | Pattern                                                            |
|-------|--------------------------------------------------------------------|
| 34    | `ImgBox` for all images — no raw `<img>`                           |
| 35    | Skeleton: `:deep(.image-box)` + `background-color ui-color-4`      |
| 36    | `:deep(img)` for aspect-ratio, border-radius, object-fit           |
| 37    | Correct tracking function per navigation type. [Details](#rule-37) |
| 38    | Every template method must trace to `methods` or `mapActions`      |
| sg-13 | CMS SVG icons via `ImgBox`, not `mr-icon`. [Details](#sg-13)       |
| ad-9  | No static images (PNG/JPG/GIF) in repo — use CMS. SVG icons only   |
| ad-13 | Track A/B experiment exposure explicitly, not just branching       |

#### Rule 37

| Function                                    | Use When           | Behavior                    |
|---------------------------------------------|--------------------|-----------------------------|
| `trackMREvent(name, props)`                 | User stays on page | Fire-and-forget             |
| `trackMREventAndRedirect(name, url, props)` | Hard redirect      | 300ms delay, then navigates |

**Anti-pattern:** `trackMREvent()` + `goToPath()` — event may not flush before navigation. Use `trackMREventAndRedirect` for any hard redirect.

#### SG-13

CMS SVG icons come in `{ icon: { file_type: 'svg+xml', svg_data } }` format. Use `ImgBox(:media-obj="item.icon")` — it detects `isNewSvg` internally. Use `mr-icon` only for locally bundled SVGs from `src/assets/svg-icons/`.

---

## 7. Code Style

| Rule  | Pattern                                                            |
|-------|--------------------------------------------------------------------|
| 39    | One blank line between logical blocks — no consecutive, no zero    |
| 40    | No info-only comments. [Details](#rule-40)                         |
| 41    | Minimal-touch: format only lines you're modifying                  |
| ad-4  | Every Pug method must trace to `methods`, `mapActions`, or a mixin |
| ad-15 | Simple expressions on one line — no multi-line trivial returns     |
| ad-20 | Clean programmatic strings — no whitespace in codes/flags          |

#### Rule 40

Don't add comments that describe what code does. Keep only:
- `// TODO:` with ticket refs
- `// eslint-disable` with reason
- Genuinely non-obvious logic explanations

---

## 8. MrBtn + Components

| Rule  | Pattern                                                                 |
|-------|-------------------------------------------------------------------------|
| 42    | Use MrBtn variants (`secondary`, `tertiary`, `light`) before custom CSS |
| 43    | `:deep(.mrbtn)` for color overrides on MrBtn                            |
| 44    | Shared components at appropriate shared level                           |
| 45    | Thin wrapper pattern: mobile wrapper + shared content                   |
| sg-14 | Self-sufficient component spacing. [Details](#sg-14)                    |
| ad-11 | No hardcoded route paths in reusable components. [Details](#ad-11)      |
| ad-12 | Extract, don't bloat — new feature = new component                      |
| ad-22 | Use existing validation systems, don't add parallel ones                |

#### SG-14

Every component handles its own spacing internally. Parent page does NOT add wrapper divs or spacing classes around imported components.

**Incorrect:**
```pug
.main-column
  .about-section.pb-100m
    HairColorBarLocationAbout
```

**Correct:**
```pug
//- Component root owns its spacing
.main-column
  HairColorBarLocationAbout
//- Inside HairColorBarLocationAbout.vue:
.hcb-about.pb-150m
```

#### AD-11

Use `$route.path`, a prop, or a URL key. Hardcoded routes create hidden coupling. **Exception:** page-specific (non-reusable) components may hardcode paths.

---

## 9. Backend / API

Applies to Express routes, controllers, or webservices — not Vue components.

| Rule  | Pattern                                                       |
|-------|---------------------------------------------------------------|
| ad-18 | No `console.error` — use `log.error()` + structured responses |
| ad-19 | Return error codes, not HTML. [Details](#ad-19)               |
| ad-21 | Never string-match `response.message` for control flow        |
| ad-9  | No static images in repo (also applies to backend templates)  |

#### AD-19

Return structured codes (`MISSING_INFO`, `UPDATE_ERROR`) and let the Vue component handle display. Use `response.success`, `response.errorCode`, or HTTP status codes for control flow.
