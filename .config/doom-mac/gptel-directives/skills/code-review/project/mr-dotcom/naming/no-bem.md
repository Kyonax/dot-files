---
id: rule-pj-mrd-034
title: No BEM Double-Underscore or Double-Dash
severity: MEDIUM
tags: bem, double-underscore, double-dash, hyphens
---

Never use BEM conventions (__ or --) in class names; use single hyphens only.

### Apply
- All class names in Vue templates and Stylus files

### Skip
- Third-party library classes that use BEM internally (do not rename those)

### Bad
```javascript
<template lang="pug">
.service-card
  .service-card__image
    ImgBox(:src="img")
  .service-card__title.service-card__title--highlighted
    h3 {{ title }}
</template>
```

### Good
```javascript
<template lang="pug">
.service-card
  .service-image
    ImgBox(:src="img")
  .service-title.highlighted
    h3 {{ title }}
</template>
```

### Edge
If overriding a third-party component that uses BEM internally, use :deep() to target its BEM classes rather than renaming them. Only MR-authored classes must follow the hyphens-only rule.
