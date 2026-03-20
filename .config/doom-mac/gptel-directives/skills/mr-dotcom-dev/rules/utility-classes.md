---
title: General Utility Classes — Display, Positioning, Sizing, Borders, Interactivity, Accessibility
impact: CRITICAL
impactDescription: Provides the shared toolkit of helper classes for display, visibility, positioning, sizing, borders, interactivity, and accessibility — preventing one-off CSS duplication.
tags: utility, display, visibility, hide, show, positioning, center, float, sizing, width, height, max-width, overflow, border-radius, divider, interactivity, clickable, animation, accessibility, ada, scroll, css, pug, stylus
---

## General Utility Classes — Display, Positioning, Sizing, Borders, Interactivity, Accessibility

A catalog of general-purpose helpers for common styling needs not covered by the spacing, typography, or flexbox systems.

For specific systems, see:
- **Spacing (margin/padding):** `rules/spacing-utilities.md`
- **Typography, Color & Variables:** `rules/typography-utilities.md`
- **Flexbox, Gap & Aspect Ratio:** `rules/flexbox-layout.md`

---

### 1. Display

| Class                 | CSS                                          |
|-----------------------|----------------------------------------------|
| `.block`              | `display: block`                             |
| `.inline-block`       | `display: inline-block; vertical-align: top` |
| `.disp-inline-block`  | `display: inline-block !important`           |
| `.disp-inline`        | `display: inline !important`                 |
| `.disp-block`         | `display: block !important`                  |
| `.disp-inherit`       | `display: inherit !important`                |
| `.disp-initial`       | `display: initial !important`                |
| `.display-table`      | `display: table`                             |
| `.display-table-cell` | `display: table-cell`                        |

---

### 2. Visibility and Show/Hide

**Basic:**

| Class                  | CSS                           | Description                           |
|------------------------|-------------------------------|---------------------------------------|
| `.hide`                | `display: none !important`    | Hidden from layout and screen readers |
| `.hidden`              | `visibility: hidden`          | Invisible but occupies space          |
| `.ninja`               | `visibility: hidden`          | Alias for `.hidden`                   |
| `.no-scroll`           | `overflow: hidden !important` | Prevents scrolling on element         |
| `.no-scroll-on-mobile` | `overflow: hidden !important` | Mobile-only scroll lock               |
| `.non-selectable`      | `user-select: none`           | Prevents text selection               |

**Responsive Show/Hide:**

| Hide at Breakpoint+ | Show ONLY at Breakpoint | Target     |
|---------------------|-------------------------|------------|
| `.xs-hide`          | `.xs-only`              | Mobile     |
| `.sm-hide`          | `.sm-only`              | Tablet     |
| `.md-hide`          | `.md-only`              | Desktop MD |
| `.lg-hide`          | `.lg-only`              | Desktop LG |
| `.xl-hide`          | `.xl-only`              | Desktop XL |

---

### 3. Positioning and Centering

**Absolute centering (requires `position: relative` on parent):**

| Class        | CSS                                                                         | Description       |
|--------------|-----------------------------------------------------------------------------|-------------------|
| `.v-center`  | `position: absolute; top: 50%; transform: translateY(-50%)`                 | Vertical center   |
| `.h-center`  | `position: absolute; left: 50%; transform: translateX(-50%)`                | Horizontal center |
| `.hv-center` | `position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%)` | Both axes         |

**Responsive centering:** `.xs-v-center`, `.sm-v-center`, `.md-v-center`, `.lg-v-center`, `.xl-v-center`

**Position helpers:**

| Class                | CSS                                                                                   |
|----------------------|---------------------------------------------------------------------------------------|
| `.relative-position` | `position: relative`                                                                  |
| `.absolute-wrap`     | `position: absolute; top: 0; bottom: 0; left: 0; right: 0`                            |
| `.fixed-wrap`        | `position: fixed; top: 0; bottom: 0; left: 0; right: 0; z-index: 200; overflow: auto` |

**Block centering:**

| Class                | CSS                                           |
|----------------------|-----------------------------------------------|
| `.div-center`        | `margin: 0 auto`                              |
| `.m-auto`            | `display: block; margin: 0 auto; float: none` |
| `.content-max-width` | `margin: 0 auto` + responsive max-width       |

**Vertical alignment:**

| Class                    | CSS                      |
|--------------------------|--------------------------|
| `.vertical-align-top`    | `vertical-align: top`    |
| `.vertical-align-bottom` | `vertical-align: bottom` |

---

### 4. Float

| Class                          | CSS                       |
|--------------------------------|---------------------------|
| `.float-right` / `.pull-right` | `float: right`            |
| `.float-left` / `.pull-left`   | `float: left`             |
| `.pull-left-important`         | `float: left !important`  |
| `.pull-right-important`        | `float: right !important` |

**Responsive float:** `.xs-right`, `.sm-right`, `.md-right`, `.lg-right`, `.xl-right` → `float: right` at breakpoint.

---

### 5. Sizing — Width

| Class         | CSS           |
|---------------|---------------|
| `.full-width` | `width: 100%` |
| `.container`  | `width: 100%` |

**Max-width (absolute):** `.max-width-{10,20,30,40,50,60,70,80,90}` → design-system-calculated max-width
**Max-width (relative):** `.max-width-rel-{10,20,30,40,50,60,70,80,90}` → `max-width: {value}%`

**Responsive max-width:** `.{xs,sm,md,lg,xl}-max-width-{10-90}` and `.{xs,sm,md,lg,xl}-max-width-rel-{10-90}`

---

### 6. Sizing — Height

| Class          | CSS            |
|----------------|----------------|
| `.full-height` | `height: 100%` |

**Percentage height:** `.height-{10,20,30,40,50,60,70,80,90}-pct` → `height: {value}%`

**Responsive height:** `.{xs,sm,md,lg,xl}-full-height` and `.{xs,sm,md,lg,xl}-height-{10-90}-pct`

---

### 7. Overflow

| Class               | CSS                            |
|---------------------|--------------------------------|
| `.overflow-visible` | `overflow: visible !important` |
| `.overflow-auto`    | `overflow: auto !important`    |
| `.overflow-hidden`  | `overflow: hidden !important`  |

---

### 8. Text Alignment

| Class           | CSS                   |
|-----------------|-----------------------|
| `.text-right`   | `text-align: right`   |
| `.text-left`    | `text-align: left`    |
| `.text-center`  | `text-align: center`  |
| `.text-justify` | `text-align: justify` |

**Responsive text alignment:** `.{xs,sm,md,lg,xl}-text-{left,right,center}` (applies at breakpoint and up).

---

### 9. Borders, Radius, and Dividers

**Border radius:** `.border-radius-{1,2,3,4,5,6,7,8,12}` → `border-radius: {value}px`

**Dividers:**

| Class                   | CSS                                   |
|-------------------------|---------------------------------------|
| `.bottom-divider`       | `border-bottom: 1px solid color-gray` |
| `.bottom-divider-light` | `border-bottom: 1px solid ui-color-4` |
| `.left-divider`         | `border-left: 1px solid color-gray`   |
| `.left-divider-dotted`  | `border-left: 1px dashed color-gray`  |
| `.right-divider`        | `border-right: 1px solid color-gray`  |
| `.right-divider-dotted` | `border-right: 1px dashed color-gray` |

**Other:**

| Class              | CSS                                |
|--------------------|------------------------------------|
| `.input.no-border` | Removes border from input elements |
| `.shadow-box`      | Standard inner box shadow          |

---

### 10. Rotation and Transform

| Class         | CSS                         |
|---------------|-----------------------------|
| `.rotate-90`  | `transform: rotate(90deg)`  |
| `.rotate-180` | `transform: rotate(180deg)` |
| `.rotate-270` | `transform: rotate(270deg)` |

---

### 11. Interactivity, State, and Animation

| Class                     | CSS/Purpose                                     |
|---------------------------|-------------------------------------------------|
| `.clickable`              | `cursor: pointer`                               |
| `.not-clickable`          | `cursor: default`                               |
| `.non-selectable`         | `user-select: none`                             |
| `.focusable`              | Standard focus ring on `:focus`                 |
| `.non-focusable`          | Removes outline on focus                        |
| `.focused`                | Permanently applies focus ring style            |
| `.shake`                  | Single horizontal shake animation (form errors) |
| `.animate-spin`           | Infinite counter-clockwise spin                 |
| `.animate-spin-clockwise` | Infinite clockwise spin                         |

---

### 12. Content Utilities

| Class               | Purpose                                                                                                                                                               |
|---------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `.rich-text-format` | Styles raw HTML content (e.g., from CMS) with consistent brand typography for `p`, `ul`, `li`, `a`, `strong`, `em` tags. Apply to a wrapper div around injected HTML. |

---

### 13. Accessibility

| Class                       | Purpose                                                                  |
|-----------------------------|--------------------------------------------------------------------------|
| `.ada-tooltip` / `.tooltip` | Visually hides text — available only to screen readers                   |
| `.hiddenButPresent`         | Hides element visually (1px clip) but keeps it in the accessibility tree |

---

### 14. Workflow Guidance

- **Check specific systems first:** Verify the class doesn't belong to spacing, typography, or flexbox rules before using one from here.
- **Compose for behavior:** Combine classes for layered functionality (e.g., `.hv-center.clickable`).
- **Prioritize accessibility:** Use `.ada-tooltip` or `.hiddenButPresent` for icons and visual elements that need textual descriptions for screen readers.
