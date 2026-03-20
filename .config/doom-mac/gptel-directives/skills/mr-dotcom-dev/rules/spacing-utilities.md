---
title: Spacing Utility Classes — Margin and Padding System
impact: CRITICAL
impactDescription: Eliminates magic numbers and ensures consistent responsive spacing across all components using the design system's scale.
tags: spacing, margin, padding, responsive, breakpoints, em, percent, utility, css, stylus, pug, mt, mb, ml, mr, mx, my, pt, pb, pl, pr, px, py, auto
---

## Spacing Utility Classes — Margin and Padding System

All element spacing (`margin` and `padding`) **must** be controlled via utility classes. Writing custom spacing values in component stylesheets is prohibited — it violates the design system's scale and responsive strategy.

---

### 1. Available Spacing Values (`{val}`)

`0, 10, 15, 25, 30, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 350, 400, 450, 500, 600, 700`

---

### 2. Spacing in `em` Units (suffix: `m`)

CSS value = `{val} / 100` (e.g., `.mt-100m` = `margin-top: 1em`, `.pt-50m` = `padding-top: 0.5em`).

#### Margin (`em`)

| Direction | Default (no prefix) | Mobile (`xs-`) | Tablet (`sm-`) | Desktop MD (`md-`) | Desktop LG (`lg-`) | Desktop XL (`xl-`) |
|---|---|---|---|---|---|---|
| **Top** | `.mt-{val}m` | `.xs-mt-{val}m` | `.sm-mt-{val}m` | `.md-mt-{val}m` | `.lg-mt-{val}m` | `.xl-mt-{val}m` |
| **Right** | `.mr-{val}m` | `.xs-mr-{val}m` | `.sm-mr-{val}m` | `.md-mr-{val}m` | `.lg-mr-{val}m` | `.xl-mr-{val}m` |
| **Bottom** | `.mb-{val}m` | `.xs-mb-{val}m` | `.sm-mb-{val}m` | `.md-mb-{val}m` | `.lg-mb-{val}m` | `.xl-mb-{val}m` |
| **Left** | `.ml-{val}m` | `.xs-ml-{val}m` | `.sm-ml-{val}m` | `.md-ml-{val}m` | `.lg-ml-{val}m` | `.xl-ml-{val}m` |
| **Horizontal (x)** | `.mx-{val}m` | `.xs-mx-{val}m` | `.sm-mx-{val}m` | `.md-mx-{val}m` | `.lg-mx-{val}m` | `.xl-mx-{val}m` |
| **Vertical (y)** | `.my-{val}m` | `.xs-my-{val}m` | `.sm-my-{val}m` | `.md-my-{val}m` | `.lg-my-{val}m` | `.xl-my-{val}m` |

#### Padding (`em`)

| Direction | Default (no prefix) | Mobile (`xs-`) | Tablet (`sm-`) | Desktop MD (`md-`) | Desktop LG (`lg-`) | Desktop XL (`xl-`) |
|---|---|---|---|---|---|---|
| **Top** | `.pt-{val}m` | `.xs-pt-{val}m` | `.sm-pt-{val}m` | `.md-pt-{val}m` | `.lg-pt-{val}m` | `.xl-pt-{val}m` |
| **Right** | `.pr-{val}m` | `.xs-pr-{val}m` | `.sm-pr-{val}m` | `.md-pr-{val}m` | `.lg-pr-{val}m` | `.xl-pr-{val}m` |
| **Bottom** | `.pb-{val}m` | `.xs-pb-{val}m` | `.sm-pb-{val}m` | `.md-pb-{val}m` | `.lg-pb-{val}m` | `.xl-pb-{val}m` |
| **Left** | `.pl-{val}m` | `.xs-pl-{val}m` | `.sm-pl-{val}m` | `.md-pl-{val}m` | `.lg-pl-{val}m` | `.xl-pl-{val}m` |
| **Horizontal (x)** | `.px-{val}m` | `.xs-px-{val}m` | `.sm-px-{val}m` | `.md-px-{val}m` | `.lg-px-{val}m` | `.xl-px-{val}m` |
| **Vertical (y)** | `.py-{val}m` | `.xs-py-{val}m` | `.sm-py-{val}m` | `.md-py-{val}m` | `.lg-py-{val}m` | `.xl-py-{val}m` |

> **Note:** Pixel-based spacing classes (`.mt-20`, `.mb-30`, etc.) exist in `layouts.styl` as legacy. Prefer the em-based `{val}m` pattern for all new work.

---

### 3. Spacing in `%` Units (suffix: `pct`)

CSS value = `{val} / 10` (e.g., `.mt-100pct` = `margin-top: 10%`).

#### Margin (`%`)

| Direction | Mobile (`xs-`) | Tablet (`sm-`) | Desktop MD (`md-`) | Desktop LG (`lg-`) | Desktop XL (`xl-`) |
|---|---|---|---|---|---|
| **Top** | `.xs-mt-{val}pct` | `.sm-mt-{val}pct` | `.md-mt-{val}pct` | `.lg-mt-{val}pct` | `.xl-mt-{val}pct` |
| **Right** | `.xs-mr-{val}pct` | `.sm-mr-{val}pct` | `.md-mr-{val}pct` | `.lg-mr-{val}pct` | `.xl-mr-{val}pct` |
| **Bottom** | `.xs-mb-{val}pct` | `.sm-mb-{val}pct` | `.md-mb-{val}pct` | `.lg-mb-{val}pct` | `.xl-mb-{val}pct` |
| **Left** | `.xs-ml-{val}pct` | `.sm-ml-{val}pct` | `.md-ml-{val}pct` | `.lg-ml-{val}pct` | `.xl-ml-{val}pct` |
| **Horizontal (x)** | `.xs-mx-{val}pct` | `.sm-mx-{val}pct` | `.md-mx-{val}pct` | `.lg-mx-{val}pct` | `.xl-mx-{val}pct` |
| **Vertical (y)** | `.xs-my-{val}pct` | `.sm-my-{val}pct` | `.md-my-{val}pct` | `.lg-my-{val}pct` | `.xl-my-{val}pct` |

#### Padding (`%`)

| Direction | Mobile (`xs-`) | Tablet (`sm-`) | Desktop MD (`md-`) | Desktop LG (`lg-`) | Desktop XL (`xl-`) |
|---|---|---|---|---|---|
| **Top** | `.xs-pt-{val}pct` | `.sm-pt-{val}pct` | `.md-pt-{val}pct` | `.lg-pt-{val}pct` | `.xl-pt-{val}pct` |
| **Right** | `.xs-pr-{val}pct` | `.sm-pr-{val}pct` | `.md-pr-{val}pct` | `.lg-pr-{val}pct` | `.xl-pr-{val}pct` |
| **Bottom** | `.xs-pb-{val}pct` | `.sm-pb-{val}pct` | `.md-pb-{val}pct` | `.lg-pb-{val}pct` | `.xl-pb-{val}pct` |
| **Left** | `.xs-pl-{val}pct` | `.sm-pl-{val}pct` | `.md-pl-{val}pct` | `.lg-pl-{val}pct` | `.xl-pl-{val}pct` |
| **Horizontal (x)** | `.xs-px-{val}pct` | `.sm-px-{val}pct` | `.md-px-{val}pct` | `.lg-px-{val}pct` | `.xl-px-{val}pct` |
| **Vertical (y)** | `.xs-py-{val}pct` | `.sm-py-{val}pct` | `.md-py-{val}pct` | `.lg-py-{val}pct` | `.xl-py-{val}pct` |

---

### 4. Auto Margin

For centering fixed-width block elements:
- `.xs-ml-auto` → `margin-left: auto`
- `.xs-mr-auto` → `margin-right: auto`

---

### 5. Usage Examples

**Incorrect — custom spacing in stylesheet:**
```stylus
.info-box
  margin-bottom: 1.5em
  padding: 1em 0.5em
```

**Correct — utility classes in Pug:**
```pug
.info-box.mb-150m.py-100m.px-50m
  p Informational content...
```

**Responsive spacing (mobile-first, override at breakpoints):**
```pug
//- 1em padding on mobile, 2em on tablet+
.main-content.p-100m.sm-p-200m
  p Content here.
```
