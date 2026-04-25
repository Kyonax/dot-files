---
id: rule-fw-vue-011
title: Function Props for Async Callbacks
severity: MEDIUM
tags: function-prop, emit, async, callback, return
---

Function props are a valid pattern for async callbacks where the child needs data back from the parent handler; `emit()` cannot return values from async parent listeners.

### Apply
- When the child component needs the result of an async operation handled by the parent
- When the child needs to await a parent action before continuing its own logic

### Skip
- Simple fire-and-forget events (use `emit()` instead)
- Synchronous parent-child communication that does not require a return value

### Bad
```javascript
// Child emits and cannot get a result back
// ChildForm.vue
export default {
  emits: ['save'],
  methods: {
    async handleSave() {
      this.$emit('save', this.formData);
      // Cannot know if save succeeded or get the saved ID back
      this.navigateAway(); // Might navigate before save completes
    },
  },
};
```

### Good
```javascript
// Child uses a function prop and awaits the result
// ChildForm.vue
export default {
  props: {
    onSave: { type: Function, required: true },
  },
  methods: {
    async handleSave() {
      const result = await this.onSave(this.formData);
      if (result.success) {
        this.navigateAway();
      }
    },
  },
};

// Parent passes the async handler
// ParentPage.vue
// <ChildForm :on-save="saveRecord" />
export default {
  methods: {
    async saveRecord(data) {
      const response = await api.save(data);
      return { success: response.ok, id: response.id };
    },
  },
};
```

### Edge
Prefer `emit()` for simple fire-and-forget events where the child does not care about the outcome. Use Function props specifically when the child needs data back from the parent or must await completion. Mixing both patterns in the same component is acceptable when different interactions have different needs.
