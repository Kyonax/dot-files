---
id: rule-pj-mrd-013
title: .max-at-tweak Mandatory on Font Classes
severity: CRITICAL
tags: max-at-tweak, font, responsive, class
---

`.max-at-tweak` is mandatory on every responsive font utility class to cap font scaling at the tweak breakpoint.

### Apply
- Every element that uses a responsive font size class (`.xs-f-small`, `.xs-f-base`, `.xs-f-large`, `.xs-f-display`, etc.)

### Skip
- Elements that intentionally need unbounded font scaling (rare; must be commented)

### Bad
```pug
h2.xs-f-large.lg-f-display Section Title
p.xs-f-small.md-f-base Description text
```

### Good
```pug
h2.xs-f-large.lg-f-display.max-at-tweak Section Title
p.xs-f-small.md-f-base.max-at-tweak Description text
```

### Edge
If an element uses only a single non-responsive font class (e.g., `.f-small` without a breakpoint prefix), `.max-at-tweak` is still required to prevent unexpected scaling on ultra-wide viewports.
