---
id: rule-pj-mrd-044
title: CMS SVG Icons via ImgBox
severity: MEDIUM
tags: cms, svg, imgbox, mr-icon, isNewSvg
---

CMS-hosted SVG icons must be rendered via ImgBox, not `mr-icon`; ImgBox detects `isNewSvg` internally.

### Apply
- SVG icons sourced from the CMS (dynamic URLs)

### Skip
- Static SVG icons bundled in the repo assets directory (those use inline SVG or mr-icon)

### Bad
```javascript
<template lang="pug">
mr-icon(:icon="cmsIconUrl" :isNewSvg="true")
</template>
```

### Good
```javascript
<template lang="pug">
ImgBox(:src="cmsIconUrl" :alt="iconAlt")
</template>
```

### Edge
If the SVG needs interactive behavior (hover color change, animation), consider inlining it instead. ImgBox renders SVGs as `<img>` tags, which do not support CSS targeting of internal SVG elements.
