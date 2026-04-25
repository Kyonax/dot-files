---
id: rule-pj-mrd-045
title: No Static Images in Repo
severity: HIGH
tags: static, image, repo, cms, svg
---

No static raster images (PNG, JPG, GIF) in the repository; all images must come from the CMS. Only SVG icons belong in the assets directory.

### Apply
- Any new image asset being added to the codebase

### Skip
- SVG icons that are part of the design system and stored in the assets directory

### Bad
```javascript
// Adding a new banner image to the repo
// website/src/assets/images/spring-promo-banner.jpg

<template lang="pug">
ImgBox(src="/assets/images/spring-promo-banner.jpg" alt="Spring Promo")
</template>
```

### Good
```javascript
// Image hosted in CMS, URL provided via CMS data

<template lang="pug">
ImgBox(:src="cmsData.springPromoBanner" :alt="cmsData.springPromoAlt")
</template>
```

### Edge
Placeholder or fallback images used during development (local only) are acceptable but must never be committed. Use CMS fallback URLs for production fallbacks.
