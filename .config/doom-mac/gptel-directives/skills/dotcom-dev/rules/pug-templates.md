---
title: Pug Template Conventions and Layout Patterns
impact: HIGH
impactDescription: Defines how all page layouts and Vue component templates are written using Pug, ensuring consistent template structure across the website.
tags: pug, template, layout, extends, block, includes, views, inheritance, kebab-case, vue-template
---

## Pug Template Conventions and Layout Patterns

All templates in the MR website use **Pug** — both Vue component templates (`template(lang="pug")`) and server-rendered page layouts (`website/src/views/*.pug`).

---

### 1. Vue Component Templates (In `.vue` Files)

Vue templates always use Pug syntax:

```pug
template(lang="pug")
.component-root.flex.flex-col.gap-md
  h2.f-secondary.upper.xs-f-large.color-mr-purple {{ title }}

  .content-area.flex-1.overflow-auto
    slot(name="content")

  .actions.flex.space-between.no-shrink
    mr-btn(@click="$emit('cancel')" variant="secondary") Cancel
    mr-btn(@click="handleSubmit" :disabled="loading") Save
```

**Key conventions:**
- Vue components referenced in **kebab-case**: `mr-btn`, `address-book`, `swiper-carousel`
- Props passed as Pug attributes: `mr-btn(:disabled="loading" variant="primary")`
- Dynamic binding with `:` prefix: `:class="{ active: isActive }"`
- Event handlers with `@`: `@click="handleClick"`, `@update:modelValue="onUpdate"`
- Slots: `slot(name="content")` for named slots, plain `slot` for default
- Text content with `|` prefix or inline: `span Text here` or `span {{ dynamicText }}`
- Conditional rendering: `div(v-if="condition")`, `div(v-else)`
- List rendering: `li(v-for="item in items" :key="item.id")`

---

### 2. Page Layout Templates (`website/src/views/`)

Page-level Pug files use **template inheritance** to compose full HTML pages.

**Base layout** (`vue-layout.pug`):
```pug
doctype html
html
  head
    block title
    block headerScripts
    block styles
  body
    block content
    block footerScripts
```

**Page template** (e.g., `vue-cart-checkout.pug`):
```pug
extends ../vue-layout.pug

block title
  title Madison Reed - Cart

block append headerScripts
  include ../../shared/amazonPayScript.pug
  script(type='text/javascript', src=config.braintree.clientScriptSrc)

block append content
  cart-and-checkout
```

**Key patterns:**
- `extends` — Inherit from a base layout template
- `block` — Define/override a named content section
- `block append` — Add to an existing block (don't replace it)
- `include` — Include another Pug file inline
- Vue components are rendered as custom elements: `cart-and-checkout`

---

### 3. Server-Side Variables in Pug

Pug templates have access to Express `res.locals` and `config`:

```pug
//- Conditional rendering based on config
if config.env === 'production'
  script(src=config.analytics.src)

//- Iterating over server data
each item in products
  .product-card
    h3= item.name
    p= item.price

//- Interpolation
title= pageTitle + ' | Madison Reed'
```

---

### 4. Utility Class Composition in Pug

The primary styling method is composing utility classes directly on Pug elements:

```pug
//- Correct: Utility class composition
.hero-section.flex.flex-col.align-center.py-200m.sm-py-300m.text-center
  h1.f-secondary.upper.xs-f-xxlarge.lg-f-billboard.color-mr-purple Our Story
  p.f-primary.xs-f-medium.md-f-large.color-slate.mt-50m.max-width-rel-60
    | Discover the Madison Reed difference.

//- Incorrect: Custom CSS for layout/spacing
.hero-section
  h1 Our Story
  //- Don't do this — no custom styles for spacing/typography
```

---

### 5. Common Patterns

**Conditional classes:**
```pug
.item(:class="{ 'active': isSelected, 'disabled': isDisabled }")
```

**Dynamic components:**
```pug
component(:is="currentComponent" v-bind="componentProps")
```

**Slots with fallback:**
```pug
slot(name="header")
  //- Fallback content if no slot provided
  h2 Default Title
```

**Self-closing components (no children):**
```pug
mr-icon(name="close" size="16")
mr-spinner-veil(:show="loading")
```
