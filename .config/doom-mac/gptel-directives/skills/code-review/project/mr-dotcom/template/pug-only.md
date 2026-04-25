---
id: rule-pj-mrd-001
title: Pug-Only Templates
severity: MEDIUM
tags: pug, template, html, lang
---

All Vue SFC templates must use Pug (`lang="pug"`); raw HTML templates are not permitted.

### Apply
- Every `<template>` block in a `.vue` file under `website/src/vuescripts/`

### Skip
- Non-Vue files (plain HTML pages, Pug layout files in `website/src/views/`)

### Bad
```vue
<template>
  <div class="my-component">
    <h2>Title</h2>
  </div>
</template>
```

### Good
```vue
<template lang="pug">
.my-component
  h2 Title
</template>
```

### Edge
Legacy components may still use HTML templates. When touching such a component, convert the template to Pug as part of the change unless the diff would be unreviewably large.
