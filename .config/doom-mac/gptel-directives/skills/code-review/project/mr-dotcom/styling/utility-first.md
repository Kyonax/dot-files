---
id: rule-pj-mrd-011
title: Utility-First with Breakpoint Prefixes
severity: CRITICAL
tags: utility, class, breakpoint, spacing, xs, sm, md, lg
---

Use utility classes with breakpoint prefixes for spacing and layout; if a utility exists, prefer it over writing Stylus.

### Apply
- All spacing (margin, padding), visibility, and responsive layout in Pug templates

### Skip
- Properties with no utility equivalent (e.g., complex animations, transitions, transforms)

### Bad
```pug
.section-wrapper
//- then in Stylus:
//- .section-wrapper
//-   margin-bottom 16px
//-   padding-top 32px
```

### Good
```pug
.section-wrapper.xs-mb-100m.lg-pt-200m
```

### Edge
Utility class naming follows the pattern `.{breakpoint}-{property}-{value}m` where breakpoints are `xs`, `sm`, `md`, `lg`. Common properties: `mb` (margin-bottom), `mt` (margin-top), `pb` (padding-bottom), `pt` (padding-top), `py` (padding y-axis), `px` (padding x-axis). Values map to the spacing scale (50m, 75m, 100m, 150m, 200m, etc.). When multiple breakpoints are needed, chain them: `.xs-mb-100m.lg-mb-200m`.
