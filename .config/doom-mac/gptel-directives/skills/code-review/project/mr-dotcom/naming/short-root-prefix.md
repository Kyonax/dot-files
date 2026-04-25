---
id: rule-pj-mrd-030
title: Short Root Class Prefix
severity: MEDIUM
tags: prefix, root, class, short, naming
---

Root class must use a short abbreviated prefix, not the full component name spelled out.

### Apply
- Every Vue component's root element class name

### Skip
- Component names that are already short (1-2 words)

### Bad
```javascript
<template lang="pug">
.hair-color-bar-location-hero-v2
  .hair-color-bar-location-hero-v2-content
    h2 Find a Hair Color Bar
</template>
```

### Good
```javascript
<template lang="pug">
.hcb-hero-v2
  .hcb-hero-v2-content
    h2 Find a Hair Color Bar
</template>
```

### Edge
When multiple components share similar abbreviations (e.g., .hcb-hero vs .hcb-header), add a distinguishing suffix to the prefix rather than expanding back to the full name.
