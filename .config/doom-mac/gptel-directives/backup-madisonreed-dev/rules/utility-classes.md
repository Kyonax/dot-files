---
title: Mandate General Utility Classes for Styling
impact: CRITICAL
impactDescription: Ensures UI consistency, reduces redundant CSS, and promotes faster development by using a shared set of helper classes.
tags: css, styling, utility-first, vue, stylus, pug, design-system
---

## Mandate General Utility Classes for Styling
At Madison Reed, visual consistency and development efficiency are paramount. To achieve this, all visual styling of components and elements **must primarily be done through predefined utility classes**. Custom, component-specific CSS is heavily discouraged.

This rule file covers general-purpose utilities. For more specific systems, refer to the dedicated rule files:

*   **Spacing (`margin`, `padding`):** See `rules/spacing-utilities.md`
*   **Typography & Color:** See `rules/typography-utilities.md`
*   **Flexbox & Layout:** See `rules/flexbox-layout.md`

---

### 1. Positioning and Transform
Use these classes for floating, absolute positioning helpers, and rotations.

| Class Name(s)                 | Purpose                                                               |
|:------------------------------|:----------------------------------------------------------------------|
| `.float-right`, `.pull-right` | `float: right`                                                        |
| `.float-left`, `.pull-left`   | `float: left`                                                         |
| `.v-center`                   | Vertically centers an absolutely positioned element.                  |
| `.h-center`                   | Horizontally centers an absolutely positioned element.                |
| `.hv-center`                  | Vertically and horizontally centers an absolutely positioned element. |
| `.rotate-90`                  | `transform: rotate(90deg)`                                            |
| `.rotate-180`                 | `transform: rotate(180deg)`                                           |
| `.rotate-270`                 | `transform: rotate(270deg)`                                           |
| `.vertical-align-top`         | `vertical-align: top`                                                 |
| `.vertical-align-bottom`      | `vertical-align: bottom`                                              |

**Example:**
```pug
.relative-container
  // This div will be perfectly centered within its parent
  .content.hv-center
```

---

### 2. Text Alignment & Decoration
While most typography is in its own file, general alignment and decoration utilities are listed here.

| Class Name       | Purpose                                             |
|:-----------------|:----------------------------------------------------|
| `.text-right`    | `text-align: right`                                 |
| `.text-left`     | `text-align: left`                                  |
| `.text-center`   | `text-align: center`                                |
| `.text-justify`  | `text-align: justify`                               |
| `.no-decoration` | Removes text decoration (e.g., underline on links). |
| `.break-word`    | `word-wrap: break-word`                             |

**Responsive Text Alignment:**
These classes apply alignment at a specific breakpoint and up.
*   `.xs-text-left`, `.xs-text-right`, `.xs-text-center`
*   `.sm-text-left`, `.sm-text-right`, `.sm-text-center`
*   `.md-text-left`, `.md-text-right`, `.md-text-center`
*   `.lg-text-left`, `.lg-text-right`, `.lg-text-center`
*   `.xl-text-left`, `.xl-text-right`, `.xl-text-center`

---

### 3. Interactivity, State, and Animation
Control user interaction states and apply simple animations.

| Class Name                | Purpose                                                              |
|:--------------------------|:---------------------------------------------------------------------|
| `.clickable`              | `cursor: pointer`                                                    |
| `.not-clickable`          | `cursor: default`                                                    |
| `.non-selectable`         | `user-select: none`                                                  |
| `.focusable`              | Applies a standard focus ring when the element is focused.           |
| `.non-focusable`          | Removes the outline on focus.                                        |
| `.focused`                | Permanently applies the focus ring style.                            |
| `.shake`                  | Applies a single horizontal shake animation (e.g., for form errors). |
| `.animate-spin`           | Applies an infinite counter-clockwise spin animation.                |
| `.animate-spin-clockwise` | Applies an infinite clockwise spin animation.                        |

---

### 4. Borders, Overflow, and Shadows
| Class Name             | Purpose                                                    |
|:-----------------------|:-----------------------------------------------------------|
| `.border-radius-{num}` | Applies `border-radius: {num}px`. Available nums: 1-8, 12. |
| `.input.no-border`     | Removes the border from an input element.                  |
| `.overflow-visible`    | `overflow: visible !important`                             |
| `.overflow-auto`       | `overflow: auto !important`                                |
| `.overflow-hidden`     | `overflow: hidden !important`                              |
| `.shadow-box`          | Applies a standard inner box shadow.                       |

---

### 5. Accessibility
Use these classes to provide information to assistive technologies while visually hiding elements.

| Class Name                 | Purpose                                                           |
|:---------------------------|:------------------------------------------------------------------|
| `.ada-tooltip`, `.tooltip` | Visually hides text, making it available only to screen readers.  |
| `.hiddenButPresent`        | Hides an element visually but keeps it in the accessibility tree. |

---

### Workflow Guidance
*   **Check Specific Rules First:** Before using a class from this file, verify it doesn't belong to a more specific system like Spacing, Typography, or Flexbox.
*   **Compose for Behavior:** Combine these classes to add layers of functionality, such as making a positioned element clickable (`.hv-center.clickable`).
*   **Prioritize Accessibility:** Use the accessibility classes to ensure your UI is usable for everyone, especially for icons or visual elements that need textual descriptions.
