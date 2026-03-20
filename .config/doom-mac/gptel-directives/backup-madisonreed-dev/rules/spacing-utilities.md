---
title: Mandate Spacing Utility Classes
impact: CRITICAL
impactDescription: Ensures consistent spacing across the application, adheres to the design system's scale, and improves responsive design implementation.
tags: css, styling, spacing, margin, padding, utility-first, responsive
---

## Mandate Spacing Utility Classes
All element spacing (`margin` and `padding`) **must** be controlled via the provided utility classes. Writing custom, one-off spacing values in component-specific stylesheets is strictly prohibited as it violates the design system's established scale and responsive strategy.

---

### 1. Spacing System Overview
The spacing system is built on a numeric scale and is fully responsive. Classes are available for `margin` and `padding` in both `em` and `%` units.

**Available Numeric Values (`{val}`):**
`0, 10, 15, 25, 30, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 350, 400, 450, 500, 600, 700`

---

### 2. Spacing in `em` Units (suffix: `m`)
These are the most common classes for spacing. The CSS value is calculated as `[value] / 100`. (e.g., `.mt-100m` = `margin-top: 1em`).

#### Margin (`em`)
| Direction          | Mobile / Default (`xs-`) | Tablet (`sm-`)  | Desktop MD (`md-`) | Desktop LG (`lg-`) | Desktop XL (`xl-`) |
|:-------------------|:-------------------------|:----------------|:-------------------|:-------------------|:-------------------|
| **Top**            | `.mt-{val}m`             | `.sm-mt-{val}m` | `.md-mt-{val}m`    | `.lg-mt-{val}m`    | `.xl-mt-{val}m`    |
| **Right**          | `.mr-{val}m`             | `.sm-mr-{val}m` | `.md-mr-{val}m`    | `.lg-mr-{val}m`    | `.xl-mr-{val}m`    |
| **Bottom**         | `.mb-{val}m`             | `.sm-mb-{val}m` | `.md-mb-{val}m`    | `.lg-mb-{val}m`    | `.xl-mb-{val}m`    |
| **Left**           | `.ml-{val}m`             | `.sm-ml-{val}m` | `.md-ml-{val}m`    | `.lg-ml-{val}m`    | `.xl-ml-{val}m`    |
| **Horizontal (x)** | `.mx-{val}m`             | `.sm-mx-{val}m` | `.md-mx-{val}m`    | `.lg-mx-{val}m`    | `.xl-mx-{val}m`    |
| **Vertical (y)**   | `.my-{val}m`             | `.sm-my-{val}m` | `.md-my-{val}m`    | `.lg-my-{val}m`    | `.xl-my-{val}m`    |

#### Padding (`em`)
| Direction          | Mobile / Default (`xs-`) | Tablet (`sm-`)  | Desktop MD (`md-`) | Desktop LG (`lg-`) | Desktop XL (`xl-`) |
|:-------------------|:-------------------------|:----------------|:-------------------|:-------------------|:-------------------|
| **Top**            | `.pt-{val}m`             | `.sm-pt-{val}m` | `.md-pt-{val}m`    | `.lg-pt-{val}m`    | `.xl-pt-{val}m`    |
| **Right**          | `.pr-{val}m`             | `.sm-pr-{val}m` | `.md-pr-{val}m`    | `.lg-pr-{val}m`    | `.xl-pr-{val}m`    |
| **Bottom**         | `.pb-{val}m`             | `.sm-pb-{val}m` | `.md-pb-{val}m`    | `.lg-pb-{val}m`    | `.xl-pb-{val}m`    |
| **Left**           | `.pl-{val}m`             | `.sm-pl-{val}m` | `.md-pl-{val}m`    | `.lg-pl-{val}m`    | `.xl-pl-{val}m`    |
| **Horizontal (x)** | `.px-{val}m`             | `.sm-px-{val}m` | `.md-px-{val}m`    | `.lg-px-{val}m`    | `.xl-px-{val}m`    |
| **Vertical (y)**   | `.py-{val}m`             | `.sm-py-{val}m` | `.md-py-{val}m`    | `.lg-py-{val}m`    | `.xl-py-{val}m`    |

---

### 3. Spacing in `%` Units (suffix: `pct`)
These classes are used for percentage-based spacing, typically for layout purposes. The CSS value is calculated as `[value] / 10`. (e.g., `.mt-100pct` = `margin-top: 10%`).

#### Margin (`%`)
| Direction          | Mobile (`xs-`)    | Tablet (`sm-`)    | Desktop MD (`md-`) | Desktop LG (`lg-`) | Desktop XL (`xl-`) |
|:-------------------|:------------------|:------------------|:-------------------|:-------------------|:-------------------|
| **Top**            | `.xs-mt-{val}pct` | `.sm-mt-{val}pct` | `.md-mt-{val}pct`  | `.lg-mt-{val}pct`  | `.xl-mt-{val}pct`  |
| **Right**          | `.xs-mr-{val}pct` | `.sm-mr-{val}pct` | `.md-mr-{val}pct`  | `.lg-mr-{val}pct`  | `.xl-mr-{val}pct`  |
| **Bottom**         | `.xs-mb-{val}pct` | `.sm-mb-{val}pct` | `.md-mb-{val}pct`  | `.lg-mb-{val}pct`  | `.xl-mb-{val}pct`  |
| **Left**           | `.xs-ml-{val}pct` | `.sm-ml-{val}pct` | `.md-ml-{val}pct`  | `.lg-ml-{val}pct`  | `.xl-ml-{val}pct`  |
| **Horizontal (x)** | `.xs-mx-{val}pct` | `.sm-mx-{val}pct` | `.md-mx-{val}pct`  | `.lg-mx-{val}pct`  | `.xl-mx-{val}pct`  |
| **Vertical (y)**   | `.xs-my-{val}pct` | `.sm-my-{val}pct` | `.md-my-{val}pct`  | `.lg-my-{val}pct`  | `.xl-my-{val}pct`  |

#### Padding (`%`)
| Direction          | Mobile (`xs-`)    | Tablet (`sm-`)    | Desktop MD (`md-`) | Desktop LG (`lg-`) | Desktop XL (`xl-`) |
|:-------------------|:------------------|:------------------|:-------------------|:-------------------|:-------------------|
| **Top**            | `.xs-pt-{val}pct` | `.sm-pt-{val}pct` | `.md-pt-{val}pct`  | `.lg-pt-{val}pct`  | `.xl-pt-{val}pct`  |
| **Right**          | `.xs-pr-{val}pct` | `.sm-pr-{val}pct` | `.md-pr-{val}pct`  | `.lg-pr-{val}pct`  | `.xl-pr-{val}pct`  |
| **Bottom**         | `.xs-pb-{val}pct` | `.sm-pb-{val}pct` | `.md-pb-{val}pct`  | `.lg-pb-{val}pct`  | `.xl-pb-{val}pct`  |
| **Left**           | `.xs-pl-{val}pct` | `.sm-pl-{val}pct` | `.md-pl-{val}pct`  | `.lg-pl-{val}pct`  | `.xl-pl-{val}pct`  |
| **Horizontal (x)** | `.xs-px-{val}pct` | `.sm-px-{val}pct` | `.md-px-{val}pct`  | `.lg-px-{val}pct`  | `.xl-px-{val}pct`  |
| **Vertical (y)**   | `.xs-py-{val}pct` | `.sm-py-{val}pct` | `.md-py-{val}pct`  | `.lg-py-{val}pct`  | `.xl-py-{val}pct`  |

---

### 4. Special Utilities
*   **Auto Margin:** For centering fixed-width block elements.
    *   `.xs-ml-auto` (`margin-left: auto`)
    *   `.xs-mr-auto` (`margin-right: auto`)

---

### 5. Usage Examples

#### Basic Spacing
**Incorrect:** Defining spacing in a component's stylesheet.

```stylus
// components/InfoBox.styl
.info-box
  margin-bottom: 1.5em  // Should be .mb-150m
  padding: 1em 0.5em    // Should be .py-100m.px-50m
```

**Correct:** Composing utility classes in the Pug template.

```pug
.info-box.mb-150m.py-100m.px-50m
  p Informational content...
```

#### Responsive Spacing
**Incorrect:** Using `@media` queries for spacing.

```stylus
// components/MainContent.styl
.main-content
  padding: 1em
  @media (min-width: 560px) // mq-tablet-plus
    padding: 2em
```

**Correct:** Applying mobile-first classes and overriding them with breakpoint-specific classes.

```pug
// Has 1em padding on mobile, which increases to 2em on tablet and up.
.main-content.p-100m.sm-p-200m
  p Main application content goes here.
```
