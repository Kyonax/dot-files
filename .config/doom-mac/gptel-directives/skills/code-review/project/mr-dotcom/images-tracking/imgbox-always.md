---
id: rule-pj-mrd-040
title: Use ImgBox for All Images
severity: HIGH
tags: ImgBox, img, image, component
---

All images must use the ImgBox component; raw `<img>` tags are forbidden.

### Apply
- Every image rendered in a Vue template

### Skip
- Images inside third-party libraries that manage their own rendering

### Bad
```javascript
<template lang="pug">
.hero-banner
  img(:src="bannerUrl" :alt="bannerAlt")
</template>
```

### Good
```javascript
<template lang="pug">
.hero-banner
  ImgBox(:src="bannerUrl" :alt="bannerAlt")
</template>
```

### Edge
When rendering user-generated HTML via `v-html`, raw `<img>` tags are unavoidable. In that case, ensure the CMS content is already optimized. For all author-controlled templates, ImgBox is mandatory.
