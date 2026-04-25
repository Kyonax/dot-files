---
id: rule-fw-vue-010
title: Template Method Must Trace to Definition
severity: MEDIUM
tags: template, method, trace, handler, undefined
---

Every method referenced in the template must trace back to a definition in `methods`, `mapActions`, or a `<script setup>` function declaration.

### Apply
- Any `@click`, `@input`, `@submit`, or other event handler in the template
- Any method call inside template expressions (e.g., `{{ formatDate(item.date) }}`)

### Skip
- Built-in Vue directives that do not reference custom methods (e.g., `v-model`, `v-show`)
- Inline arrow functions in the template that do not call external methods

### Bad
```html
<template>
  <button @click="submitForm">Submit</button>
</template>

<script>
export default {
  methods: {
    // submitForm is never defined
    validateForm() { /* ... */ },
  },
};
</script>
```

### Good
```html
<template>
  <button @click="submitForm">Submit</button>
</template>

<script>
export default {
  methods: {
    submitForm() {
      if (this.validateForm()) { /* ... */ }
    },
    validateForm() { /* ... */ },
  },
};
</script>
```

### Edge
In `<script setup>`, any `const` or `function` declaration is automatically available in the template. Ensure the function is actually declared and not just imported but unused, or typo'd in the template binding.
