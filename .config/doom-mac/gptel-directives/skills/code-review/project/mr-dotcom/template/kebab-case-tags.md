---
id: rule-pj-mrd-002
title: Kebab-Case Component Tags
severity: MEDIUM
tags: kebab, component, tag, pug, mr-btn, mr-icon
---

Vue components in Pug templates must use kebab-case tag names, not PascalCase.

### Apply
- All component references inside `<template lang="pug">` blocks

### Skip
- JavaScript imports and component registration (PascalCase is correct there)

### Bad
```pug
MrBtn(@click="submit") Save
MrIcon(name="check")
ImgBox(:src="url")
```

### Good
```pug
mr-btn(@click="submit") Save
mr-icon(name="check")
img-box(:src="url")
```

### Edge
HTML native elements (div, span, section) are already lowercase. This rule applies only to custom Vue components like mr-btn, mr-icon, img-box, app-modal, and all project-specific components.
