---
id: rule-pj-mrd-005
title: Single h1 Per Page
severity: MEDIUM
tags: h1, page, section, heading, single
---

Each page must contain exactly one h1 element; section-level components must use h2 or lower.

### Apply
- All page-level and section-level Vue components under `website/src/vuescripts/`

### Skip
- Modal or overlay components that represent a separate document context (e.g., AppModal)

### Bad
```pug
//- PageComponent.vue
h1 Welcome to Madison Reed

//- HeroSection.vue (child of PageComponent)
h1 Find Your Perfect Color
```

### Good
```pug
//- PageComponent.vue
h1 Welcome to Madison Reed

//- HeroSection.vue (child of PageComponent)
h2 Find Your Perfect Color
```

### Edge
If a component can serve as both a standalone page and an embedded section, accept the heading level as a prop with a default of h2. The page-level parent is responsible for the single h1.
