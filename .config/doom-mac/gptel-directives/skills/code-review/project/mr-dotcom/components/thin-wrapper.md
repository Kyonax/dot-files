---
id: rule-pj-mrd-053
title: Thin Wrapper Pattern for Responsive Layouts
severity: MEDIUM
tags: wrapper, mobile, shared, content, responsive
---

Use a thin wrapper component for mobile/desktop layout differences that shares content components rather than duplicating markup.

### Apply
- Components that need substantially different layouts between mobile and desktop

### Skip
- Components where CSS media queries alone handle the responsive differences

### Bad
```javascript
<template lang="pug">
// Duplicated content in both branches
.hero(v-if="isMobile")
  h1 {{ title }}
  p {{ description }}
  ImgBox(:src="image")
  MrBtn(:label="cta")
.hero-desktop(v-else)
  .hero-left
    h1 {{ title }}
    p {{ description }}
    MrBtn(:label="cta")
  .hero-right
    ImgBox(:src="image")
</template>
```

### Good
```javascript
// Parent: thin wrapper handles layout
<template lang="pug">
HeroMobile(v-if="isMobile" :data="heroData")
HeroDesktop(v-else :data="heroData")
</template>

// Both HeroMobile and HeroDesktop use shared child components:
// HeroContent, HeroImage, HeroCta
```

### Edge
If the layout difference is minor (e.g., flex-direction change), a single component with CSS media queries is simpler than the wrapper pattern. Reserve thin wrappers for structurally different layouts.
