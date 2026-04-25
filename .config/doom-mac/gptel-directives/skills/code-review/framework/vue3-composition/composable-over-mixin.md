---
id: rule-fw-vca-002
title: Composables Over Mixins
severity: HIGH
tags: composable, mixin, use, shared-logic, extract
---

Use composables (`use*` functions) for shared logic; never create new mixins. Existing mixins are legacy and should be migrated when feasible.

### Apply
- Any new shared logic that needs to be reused across components
- When refactoring or extending a component that currently uses a mixin

### Skip
- Existing mixin usage that is stable and not being modified in the current change
- Third-party library mixins that cannot be replaced

### Bad
```javascript
// New mixin — DO NOT create
export const paginationMixin = {
  data() {
    return { page: 1, pageSize: 20 };
  },
  computed: {
    offset() { return (this.page - 1) * this.pageSize; },
  },
  methods: {
    nextPage() { this.page++; },
    prevPage() { if (this.page > 1) { this.page--; } },
  },
};
```

### Good
```javascript
// Composable
import { ref, computed } from 'vue';

export function usePagination(initialPageSize = 20) {
  const page = ref(1);
  const pageSize = ref(initialPageSize);
  const offset = computed(() => (page.value - 1) * pageSize.value);

  function nextPage() { page.value++; }
  function prevPage() { if (page.value > 1) { page.value--; } }

  return { page, pageSize, offset, nextPage, prevPage };
}
```

### Edge
When touching a component that uses an existing mixin, consider refactoring the mixin logic into a composable if the scope of the change allows and the mixin is used by a manageable number of components. If the mixin is deeply entangled across dozens of components, note it as tech debt rather than blocking the current PR.
