---
id: rule-fw-vue-004
title: Declare Emits Explicitly
severity: MEDIUM
tags: emits, defineEmits, event, emit
---

Declare the `emits` option (or `defineEmits` in `<script setup>`) explicitly when a component emits events.

### Apply
- Any component that calls `$emit()` or `emit()` anywhere in its template or script
- Components that forward native events as custom events

### Skip
- Components that use Function prop callbacks instead of `emit()` for parent communication
- Components that never emit any events

### Bad
```javascript
// Options API — emits not declared
export default {
  name: 'SearchInput',
  methods: {
    onInput(event) {
      this.$emit('search', event.target.value);
    },
  },
};
```

### Good
```javascript
// Options API
export default {
  name: 'SearchInput',
  emits: ['search'],
  methods: {
    onInput(event) {
      this.$emit('search', event.target.value);
    },
  },
};

// Composition API — <script setup>
const emit = defineEmits(['search']);
function onInput(event) {
  emit('search', event.target.value);
}
```

### Edge
When using Function prop callbacks (e.g., `onSubmit: { type: Function }`) instead of `emit()`, the `emits` declaration is not needed. Function props are a valid alternative pattern, especially when the child needs a return value from the parent handler (see `rule-fw-vue-011`).
