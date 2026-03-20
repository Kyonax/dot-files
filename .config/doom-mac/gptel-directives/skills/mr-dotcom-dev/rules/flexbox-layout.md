---
title: Flexbox, Gap, and Aspect Ratio Utility Classes
impact: HIGH
impactDescription: Ensures consistent, responsive component layouts using the shared flexbox and aspect ratio utility system instead of custom CSS.
tags: flexbox, layout, flex, gap, wrap, aspect-ratio, a-box, align, justify, direction, responsive, css, utility
---

## Flexbox, Gap, and Aspect Ratio Utility Classes

All flexbox layouts — containers, direction, alignment, sizing, gap, and aspect ratios — **must** use utility classes. Writing custom `display: flex`, `justify-content`, or aspect ratio padding hacks in component stylesheets is prohibited.

---

### 1. Flex Containers and Direction

| Class | CSS | Description |
|---|---|---|
| `.flex` | `display: flex` | Creates a flex container |
| `.flex-row` | `display: flex; flex-direction: row; width: 100%` | Horizontal flow with full width |
| `.flex-col` | `display: flex; flex-direction: column` | Vertical stack |

**Responsive direction:**

| Class | Breakpoint |
|---|---|
| `.xs-flex-col` / `.xs-flex-row` | Mobile |
| `.sm-flex-col` / `.sm-flex-row` | Tablet |
| `.md-flex-col` / `.md-flex-row` | Desktop MD |
| `.lg-flex-col` / `.lg-flex-row` | Desktop LG |

---

### 2. Alignment and Justification

**Main axis (justify-content):**

| Class | CSS |
|---|---|
| `.space-start` | `justify-content: flex-start` |
| `.space-end` | `justify-content: flex-end` |
| `.space-center` | `justify-content: center` |
| `.space-between` | `justify-content: space-between` |
| `.space-evenly` | `justify-content: space-evenly` |

**Cross axis (align-items):**

| Class | CSS |
|---|---|
| `.align-center` | `align-items: center` |
| `.align-baseline` | `align-items: baseline` |

---

### 3. Flex Item Sizing

Apply to **direct children** of a flex container.

| Class | CSS | Description |
|---|---|---|
| `.flex-1` | `flex-grow: 1` | Grows to fill 1 part |
| `.flex-2` | `flex-grow: 2` | Twice `.flex-1` |
| `.flex-3` | `flex-grow: 3` | Three times `.flex-1` |
| `.flex-fill` | `flex-grow: 100` | Aggressively fill all space |
| `.flex-auto` | `flex: 0 0 auto` | Intrinsic size, no grow/shrink |
| `.no-shrink` | `flex-shrink: 0` | Never shrink below content size |

---

### 4. Flex Wrap

| Class | CSS |
|---|---|
| `.nowrap` | `flex-wrap: nowrap` |

---

### 5. Gap

**Named gaps:**

| Class | Value |
|---|---|
| `.gap-xs` | `4px` |
| `.gap-sm` | `8px` |
| `.gap-md` | `16px` |
| `.gap-lg` | `24px` |

**Numeric gaps:** `.gap-0` through `.gap-30` → `gap: {0-30}px` (all 31 integer values).

---

### 6. Legacy `.flex-box` System

The `.flex-box` class creates a wrapped, space-between flex container. It has its own modifier system:

| Class | CSS |
|---|---|
| `.flex-box` | `display: flex; align-items: flex-start; flex-wrap: wrap; justify-content: space-between` |
| `.flex-box.center-major-axis` | `justify-content: center` |
| `.flex-box.flex-end-major-axis` | `justify-content: flex-end` |
| `.flex-box.flex-start-major-axis` | `justify-content: flex-start` |
| `.flex-box.center-minor-axis` | `align-items: center` |
| `.flex-box.column` | `flex-direction: column` |
| `.flex-box.nowrap` | `flex-wrap: nowrap` |

Child sizing within `.flex-box`: `.flex-1`, `.flex-2`, `.flex-3`, `.flex-fill`, `.no-shrink` (same as Section 3).

---

### 7. Aspect Ratio Boxes

Container-based aspect ratios using padding-bottom technique.

**Base class:** `.a-box` — establishes the aspect ratio container.
**Content class:** `.a-box-content` — positions content absolutely within the container.

**Supported ratios:** `1x1`, `1x2`, `1x3`, `1x4`, `2x1`, `2x3`, `3x1`, `3x2`, `3x4`, `4x1`, `4x3`, `4x5`, `7x2`, `8x5`, `10x9`, `12x5`, `13x5`, `16x9`

**Responsive pattern:** `.a-box-{bp}-{ratio}`

| Prefix | Breakpoint |
|---|---|
| `.a-box-xs-{ratio}` | Mobile |
| `.a-box-sm-{ratio}` | Tablet |
| `.a-box-md-{ratio}` | Desktop MD |
| `.a-box-lg-{ratio}` | Desktop LG |
| `.a-box-xl-{ratio}` | XL |

Content positioning also has responsive variants: `.xs-a-box-content` through `.xl-a-box-content`.

**Usage:**
```pug
.a-box.a-box-xs-1x1.a-box-md-16x9
  .a-box-content
    img-box(:src="imageUrl" alt="Hero image")
```

---

### 8. Usage Example

**Incorrect — custom CSS:**
```stylus
.modal-content
  display flex
  flex-direction column
  height calc(100% - 120px)
```

**Correct — flex layout with utilities:**
```pug
.modal-container.flex.flex-col.full-height
  header.modal-header.no-shrink
    h2 Modal Title
  main.modal-content.flex-1.overflow-auto
    p Scrollable content here...
  footer.modal-footer.no-shrink
    mr-btn(@click="close") Close
```
