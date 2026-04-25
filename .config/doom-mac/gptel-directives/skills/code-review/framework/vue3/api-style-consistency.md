---
id: rule-fw-vue-002
title: Consistent API Style
severity: MEDIUM
tags: options-api, composition-api, script-setup, consistency
---

Maintain a consistent API style (Options API vs Composition API) within the same codebase area; do not mix styles arbitrarily.

### Apply
- New components added to a directory where all siblings use the same API style
- Refactors that touch a component's script block

### Skip
- Gradual migrations where the team has explicitly documented a transition strategy
- Isolated utility components that benefit from a different style than the surrounding code

### Bad
```javascript
// ComponentA.vue in /features/checkout/ — Options API
export default {
  data() { return { total: 0 }; },
  methods: { calculate() { /* ... */ } },
};

// ComponentB.vue in /features/checkout/ — Composition API
import { ref } from 'vue';
const total = ref(0);
function calculate() { /* ... */ }
```

### Good
```javascript
// ComponentA.vue in /features/checkout/ — Options API
export default {
  data() { return { total: 0 }; },
  methods: { calculate() { /* ... */ } },
};

// ComponentB.vue in /features/checkout/ — Options API (matches siblings)
export default {
  data() { return { items: [] }; },
  methods: { loadItems() { /* ... */ } },
};
```

### Edge
A project rule can enforce one style globally — this rule only flags inconsistency within the same codebase area. If the project is mid-migration, defer to the documented migration plan rather than flagging every mismatch.
