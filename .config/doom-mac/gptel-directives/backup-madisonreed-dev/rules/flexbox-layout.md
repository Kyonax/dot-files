---
title: Mandate Flexbox & Layout Utility Classes
impact: HIGH
impactDescription: Ensures consistent, responsive, and maintainable component layouts by leveraging the shared design system instead of custom CSS.
tags: css, layout, flexbox, responsive, grid, utility-first
---

## Mandate Flexbox & Layout Utility Classes
All component and page layouts, including flexbox behavior, element display, visibility, and sizing, **must** be controlled via the provided utility classes. Writing custom layout properties like `display: flex`, `justify-content`, `width`, or using media queries for layout changes is strictly prohibited.

---

### 1. Flexbox Containers and Direction
Use these classes to create flex containers and define their flow direction.

| Class Name(s) | CSS Property & Value     | Description                               |
|:--------------|:-------------------------|:------------------------------------------|
| `.flex`       | `display: flex`          | Creates a flex container.                 |
| `.flex-row`   | `flex-direction: row`    | Aligns flex items horizontally (default). |
| `.flex-col`   | `flex-direction: column` | Stacks flex items vertically.             |

#### Responsive Direction
You can change the flex direction at different breakpoints.

| Class Name     | Breakpoint Target            |
|:---------------|:-----------------------------|
| `.xs-flex-col` | Mobile (`mq-mobile`)         |
| `.xs-flex-row` | Mobile (`mq-mobile`)         |
| `.sm-flex-col` | Tablet (`mq-tablet`)         |
| `.sm-flex-row` | Tablet (`mq-tablet`)         |
| `.md-flex-col` | Desktop MD (`mq-desktop-md`) |
| `.md-flex-row` | Desktop MD (`mq-desktop-md`) |
| `.lg-flex-col` | Desktop (`mq-desktop`)       |
| `.lg-flex-row` | Desktop (`mq-desktop`)       |

---

### 2. Alignment and Justification

#### Main Axis (Justify Content)
Controls alignment along the main axis (`row` or `column`).

| Class Name       | CSS Property & Value             | Description                                                          |
|:-----------------|:---------------------------------|:---------------------------------------------------------------------|
| `.space-start`   | `justify-content: flex-start`    | Items align to the start of the container.                           |
| `.space-end`     | `justify-content: flex-end`      | Items align to the end of the container.                             |
| `.space-center`  | `justify-content: center`        | Items align to the center of the container.                          |
| `.space-between` | `justify-content: space-between` | Items are evenly distributed; first and last items are at the edges. |
| `.space-evenly`  | `justify-content: space-evenly`  | Items are evenly distributed with equal space around them.           |

#### Cross Axis (Align Items)
Controls alignment along the cross axis.

| Class Name        | CSS Property & Value    | Description                              |
|:------------------|:------------------------|:-----------------------------------------|
| `.align-center`   | `align-items: center`   | Items are centered along the cross axis. |
| `.align-baseline` | `align-items: baseline` | Items align their text baselines.        |

---

### 3. Flex Item Sizing (Grow & Shrink)
Apply these classes to the **direct children** of a flex container to control how they grow and shrink to fill available space.

| Class Name   | CSS Property & Value | Description                                                                        |
|:-------------|:---------------------|:-----------------------------------------------------------------------------------|
| `.flex-1`    | `flex-grow: 1`       | Item grows to fill space relative to other items (e.g., takes up 1 part).          |
| `.flex-2`    | `flex-grow: 2`       | Item grows to be twice as large as a `.flex-1` item.                               |
| `.flex-3`    | `flex-grow: 3`       | Item grows to be three times as large as a `.flex-1` item.                         |
| `.flex-fill` | `flex-grow: 100`     | A shorthand for an item that should aggressively grow to fill all remaining space. |
| `.no-shrink` | `flex-shrink: 0`     | Prevents the flex item from shrinking smaller than its content size.               |

---

### 4. Flex Wrap
Control whether flex items wrap onto multiple lines. The legacy `.flex-box` class defaults to `wrap`.

| Class Name | CSS Property & Value | Description                                    |
|:-----------|:---------------------|:-----------------------------------------------|
| `.nowrap`  | `flex-wrap: nowrap`  | Prevents items from wrapping to the next line. |

---

### 5. Gap
Controls the space between flex items.

| Class Name(s) | Value     | Description                                                       |
|:--------------|:----------|:------------------------------------------------------------------|
| `.gap-{num}`  | `{num}px` | Sets gap from `0px` to `30px`. Example: `.gap-10` is `gap: 10px`. |
| `.gap-xs`     | `4px`     | Pre-defined extra-small gap.                                      |
| `.gap-sm`     | `8px`     | Pre-defined small gap.                                            |
| `.gap-md`     | `16px`    | Pre-defined medium gap.                                           |
| `.gap-lg`     | `24px`    | Pre-defined large gap.                                            |

---

### 6. Sizing and Display

#### Display
| Class Name      | CSS Property & Value       |
|:----------------|:---------------------------|
| `.block`        | `display: block`           |
| `.inline-block` | `display: inline-block`    |
| `.hide`         | `display: none !important` |
| `.hidden`       | `visibility: hidden`       |

#### Sizing
| Class Name             | CSS Property & Value                                      |
|:-----------------------|:----------------------------------------------------------|
| `.full-width`          | `width: 100%`                                             |
| `.full-height`         | `height: 100%`                                            |
| `.max-width-rel-{num}` | `max-width: {num}%` (e.g., `.max-width-rel-50` for `50%`) |
| `.height-{num}-pct`    | `height: {num}%` (e.g., `.height-50-pct` for `50%`)       |

---

### 7. Responsive Visibility
Use these classes to show or hide elements at specific breakpoints. **This is the preferred method for responsive display changes.**

| Hide at Breakpoint and Up | Show ONLY at Breakpoint | Breakpoint Target |
|:--------------------------|:------------------------|:------------------|
| `.xs-hide`                | `.xs-only`              | Mobile            |
| `.sm-hide`                | `.sm-only`              | Tablet            |
| `.md-hide`                | `.md-only`              | Desktop MD        |
| `.lg-hide`                | `.lg-only`              | Desktop           |
| `.xl-hide`                | `.xl-only`              | Desktop XL        |

---

### 8. Usage Example with Flex Item Sizing
**Incorrect:** Using `calc()` or custom CSS to create a flexible content area.

```stylus
// components/ModalLayout.styl
.modal-content
  // Complex calculation to subtract header/footer height
  height: calc(100% - 120px)
```

**Correct:** Using a flex column layout and allowing the content area to grow and fill the available space with `.flex-1`.

```pug
// A full-height modal layout
.modal-container.flex.flex-col.full-height
  // Header with a fixed height, does not shrink
  header.modal-header.no-shrink
    h2 Modal Title

  // Main content area grows to fill all available vertical space
  main.modal-content.flex-1.overflow-auto
    p Your scrollable content goes here...

  // Footer with a fixed height, does not shrink
  footer.modal-footer.no-shrink
    button Close
```
