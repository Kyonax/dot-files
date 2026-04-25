---
id: rule-fw-vue-007
title: Props Before :deep() for Child Styling
severity: HIGH
tags: props, deep, styling, child, scoped
---

Use the child component's props API to customize its appearance before resorting to `:deep()` selectors that pierce scoped styles.

### Apply
- Any time you need to change a child component's visual appearance from the parent
- When a child component already exposes styling props (variant, size, color, class overrides)

### Skip
- Third-party components with no styling props or slots
- One-off layout adjustments where adding a prop to the child would be over-engineering

### Bad
```html
<template>
  <BaseButton label="Submit" />
</template>

<style scoped>
:deep(.base-button) {
  background-color: var(--color-primary);
  border-radius: 8px;
  font-size: 1.125rem;
}
</style>
```

### Good
```html
<template>
  <BaseButton
    label="Submit"
    variant="primary"
    size="lg"
    :border-radius="8"
  />
</template>
```

### Edge
`:deep()` is acceptable when the child component does not expose the needed styling hook and modifying the child is out of scope. Document why `:deep()` was necessary with a comment referencing the limitation.
