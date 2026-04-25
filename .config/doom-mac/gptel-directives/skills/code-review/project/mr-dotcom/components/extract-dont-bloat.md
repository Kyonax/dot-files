---
id: rule-pj-mrd-056
title: Extract New Features Into New Components
severity: MEDIUM
tags: extract, component, bloat, feature, single-responsibility
---

New features must be extracted into new components; do not bloat existing components with unrelated functionality.

### Apply
- When adding a new feature or distinct UI section to an existing page

### Skip
- Minor enhancements to existing functionality (adding a prop, fixing a label)

### Bad
```javascript
// Adding promo banner logic into an existing hero component
// HairColorBarHero.vue grows from 150 to 350 lines
export default {
  data() {
    return {
      // existing hero data...
      promoBannerVisible: false,
      promoCode: '',
      promoExpiry: null,
      promoProducts: [],
    };
  },
};
```

### Good
```javascript
// New component for the new feature
// PromoBanner.vue handles its own state and logic

// HairColorBarHero.vue stays focused, imports the new component
<template lang="pug">
.hcb-hero
  PromoBanner(v-if="showPromo" :promoData="promoData")
  // existing hero content...
</template>
```

### Edge
If the new feature is deeply intertwined with the existing component's state (e.g., a toggle that affects the hero's layout), use provide/inject or a shared Vuex module rather than stuffing everything into one component.
