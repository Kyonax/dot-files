---
id: index-pj-mrd-naming
title: Naming Conventions Index
category: naming
scope: project/mr-dotcom
---

# Naming Conventions

Rules governing CSS class naming, prefixes, and structural naming patterns in Madison Reed Vue components.

## Rules

| ID | File | Title | Severity |
|---|---|---|---|
| rule-pj-mrd-030 | short-root-prefix.md | Short Root Class Prefix | MEDIUM |
| rule-pj-mrd-031 | no-redundant-prefix.md | No Redundant Parent Prefix in Scoped Children | MEDIUM |
| rule-pj-mrd-032 | card-children.md | Card Children Naming Pattern | MEDIUM |
| rule-pj-mrd-033 | grid-columns.md | Grid Column Role Naming | MEDIUM |
| rule-pj-mrd-034 | no-bem.md | No BEM Double-Underscore or Double-Dash | MEDIUM |

## Quick Reference

- **short-root-prefix** -- Abbreviated root class prefix (.hcb-hero-v2 not .hair-color-bar-location-hero-v2), component root element naming
- **no-redundant-prefix** -- Remove parent name repetition from child classes when scoped styles provide isolation
- **card-children** -- Card child elements follow {parent}-{element} pattern (.service-card, .service-image, .service-cta)
- **grid-columns** -- Grid/flex columns named by role ({role}-column: .main-column, .sidebar-column)
- **no-bem** -- Hyphens only, no BEM double-underscore (__) or double-dash (--) conventions
