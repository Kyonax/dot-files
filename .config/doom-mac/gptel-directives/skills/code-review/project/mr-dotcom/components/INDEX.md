---
id: index-pj-mrd-components
title: Components Index
category: components
scope: project/mr-dotcom
---

# Components

Rules governing Vue component architecture, MrBtn usage, component extraction, spacing ownership, validation, and reusability patterns in Madison Reed Dotcom.

## Rules

| ID | File | Title | Severity |
|---|---|---|---|
| rule-pj-mrd-050 | mrbtn-variants.md | Use MrBtn Variants Before Custom CSS | MEDIUM |
| rule-pj-mrd-051 | mrbtn-deep-override.md | MrBtn Color Override via Deep Selector | MEDIUM |
| rule-pj-mrd-052 | shared-component-level.md | Shared Components at Appropriate Level | MEDIUM |
| rule-pj-mrd-053 | thin-wrapper.md | Thin Wrapper Pattern for Responsive Layouts | MEDIUM |
| rule-pj-mrd-054 | self-sufficient-spacing.md | Self-Sufficient Component Spacing | MEDIUM |
| rule-pj-mrd-055 | no-hardcoded-routes.md | No Hardcoded Routes in Reusable Components | MEDIUM |
| rule-pj-mrd-056 | extract-dont-bloat.md | Extract New Features Into New Components | MEDIUM |
| rule-pj-mrd-057 | existing-validation.md | Use Existing Validation Systems | MEDIUM |

## Quick Reference

- **mrbtn-variants** -- Use MrBtn built-in variants (secondary, tertiary, light) before writing custom button CSS
- **mrbtn-deep-override** -- Color overrides on MrBtn via :deep(.mrbtn) selector, not classes on the component tag
- **shared-component-level** -- Place shared components at directory level matching reuse scope (booking/, components/)
- **thin-wrapper** -- Mobile wrapper + shared content components pattern for structurally different responsive layouts
- **self-sufficient-spacing** -- Components own their internal spacing, parents do not add wrapper divs for margins
- **no-hardcoded-routes** -- Reusable components accept route paths as props, no hardcoded /path strings
- **extract-dont-bloat** -- New feature = new component, extract instead of adding unrelated state to existing components
- **existing-validation** -- Use Vuelidate for all form validation, do not build parallel validation logic
