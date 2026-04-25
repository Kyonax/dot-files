---
id: rule-pj-mrd-032
title: Card Children Naming Pattern
severity: MEDIUM
tags: card, children, parent-element, naming
---

Card-style component children must follow the {parent}-{element} naming pattern.

### Apply
- Any card-like component (product card, service card, location card, etc.)

### Skip
- Components that are not card-like layouts (modals, full-page sections)

### Bad
```javascript
<template lang="pug">
.service-card
  .img-wrapper
    ImgBox(:src="img")
  .text-area
    h3 {{ title }}
  .button-section
    MrBtn(:label="cta")
</template>
```

### Good
```javascript
<template lang="pug">
.service-card
  .service-image
    ImgBox(:src="img")
  .service-content
    h3 {{ title }}
  .service-cta
    MrBtn(:label="cta")
</template>
```

### Edge
Deeply nested children (3+ levels) can use the immediate parent as the prefix rather than the root card name, e.g., `.service-content > .content-details` instead of `.service-card-content-details`.
