---
id: rule-pj-mrd-031
title: No Redundant Parent Prefix in Scoped Children
severity: MEDIUM
tags: redundant, prefix, parent, child, scoped
---

Child elements inside a scoped component must not repeat the parent prefix when nesting already establishes scope.

### Apply
- Child class names inside a scoped style Vue component

### Skip
- Unscoped styles where the parent prefix is needed for specificity

### Bad
```javascript
<template lang="pug">
.service-card
  .service-card-image
    ImgBox(:src="img")
  .service-card-title
    h3 {{ title }}
  .service-card-cta
    MrBtn(:label="cta")
</template>
```

### Good
```javascript
<template lang="pug">
.service-card
  .service-image
    ImgBox(:src="img")
  .service-title
    h3 {{ title }}
  .service-cta
    MrBtn(:label="cta")
</template>
```

### Edge
If a child is reused across multiple parents within the same file, keep the parent prefix to avoid ambiguity. This is rare in single-component files.
