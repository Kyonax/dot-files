---
id: rule-fw-vca-003
title: ref() vs reactive() Usage Patterns
severity: MEDIUM
tags: ref, reactive, watch, computed, primitive, object
---

Use `ref()` for primitives and `reactive()` for objects; do not wrap a `reactive()` object inside `ref()`.

### Apply
- All `<script setup>` and Composition API state declarations
- When converting Options API `data()` properties to Composition API

### Skip
- Store state managed by Vuex/Pinia (uses its own reactivity system)
- Computed properties (always use `computed()`)

### Bad
```javascript
import { ref, reactive } from 'vue';

// reactive for a primitive — won't work as expected
const count = reactive(0);

// ref wrapping a reactive — double-wrapping, unnecessary
const state = ref(reactive({ name: '', email: '' }));

// reactive when ref would be simpler
const isLoading = reactive({ value: true });
```

### Good
```javascript
import { ref, reactive } from 'vue';

// ref for primitives
const count = ref(0);
const isLoading = ref(true);
const userName = ref('');

// reactive for objects
const form = reactive({ name: '', email: '', phone: '' });
const filters = reactive({ category: null, sortBy: 'date', page: 1 });
```

### Edge
`ref()` can hold objects too and is often preferred for consistency (always use `.value`). The key anti-pattern is double-wrapping `ref(reactive(...))` or using `reactive()` on primitives. If a team standardizes on `ref()` for everything, that is acceptable; the rule prohibits the double-wrap and primitive-reactive misuse.
