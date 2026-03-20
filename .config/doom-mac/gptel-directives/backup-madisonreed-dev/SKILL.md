---
name: madisonreed-dev
description: Provides a comprehensive guide to Madison Reed's frontend development standards, covering Vue, Pug, Stylus, NodeJS, and Dynamic Yield. Ensures adherence to company code guidelines, utility classes, ESLint rules, and design patterns for robust e-commerce development.
metadata:
  author: Kyonax
  version: "1.2.0"
---

# Madison Reed Development Skill Guide
This skill consolidates Madison Reed's frontend development standards. It is the primary reference for building consistent, maintainable, and efficient UI components using Vue, Pug, and Stylus.

## When to Apply
Reference these rules for any frontend development task at Madison Reed, including creating new components, refactoring existing code, or reviewing pull requests to ensure adherence to established patterns.

## Quick Reference
This is an index of all available rules. The analyzer AI uses these descriptions to load the correct context for your request.

### Styling & UI (`CRITICAL`)
- **`madisonreed-dev/spacing-utilities`**: **Spacing System.** Governs all `margin` & `padding`. Details the complete formula for responsive (`sm-`), directional (`-t`, `-r`, `-b`, `-l`), axis-based (`-x`, `-y`), and unit-specific (`-m` for `em`, `-pct` for `%`) spacing classes.
- **`madisonreed-dev/typography-utilities`**: **Typography & Color.** Controls all text styling: font families (`f-primary`), responsive sizes (`xs-f-large`), weights (`bold`), the full brand color palette (`.color-mr-purple`), text transformation (`.upper`), decoration (`.underline`), and letter-spacing.
- **`madisonreed-dev/flexbox-layout`**: **Flexbox & Layout.** Defines all layouts via flexbox containers (`.flex`), direction (`.flex-col`), alignment (`.space-between`), item sizing/growth (`.flex-1`, `.no-shrink`), wrapping (`.nowrap`), gap (`.gap-md`), and responsive visibility (`.sm-hide`).
- **`madisonreed-dev/utility-classes`**: **General Utilities.** A catalog of general-purpose helpers for absolute positioning (`.v-center`), interactivity/state (`.clickable`, `.focused`), borders (`.border-radius-4`), overflow (`.overflow-hidden`), animations (`.shake`), and accessibility (`.ada-tooltip`).
- **`madisonreed-dev/dynamic-yield`**: **Personalization & A/B Testing.** Governs all Dynamic Yield (DY) integration. Details the core architecture (Campaigns, Templates), the primary Experience API (`/choose` endpoint), event tracking (`dyType`), user identification (CUID), and critical guidelines for template development and modification.

## How to Use
Read the individual rule files for detailed class lists and implementation examples:

*   `./rules/utility-classes.md`
*   `./rules/spacing-utilities.md`
*   `./rules/typography-utilities.md`
*   `./rules/flexbox-layout.md`

## Full Compiled Document
For the complete guide with all rules expanded and additional philosophy, consult `AGENTS.md`.
