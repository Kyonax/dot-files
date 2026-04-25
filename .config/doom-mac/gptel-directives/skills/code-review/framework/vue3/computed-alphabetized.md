---
id: rule-fw-vue-005
title: Alphabetize Computed Properties
severity: LOW
tags: computed, alphabetized, order
---

Alphabetize computed properties within the script section for predictable scanning.

### Apply
- Components with 4 or more computed properties
- When adding a new computed property to an existing block

### Skip
- Components with 3 or fewer computed properties where order is already clear
- Composition API files where computeds are grouped by feature rather than listed in a block

### Bad
```javascript
computed: {
  totalPrice() { return this.price * this.quantity; },
  formattedDate() { return formatDate(this.date); },
  isAvailable() { return this.stock > 0; },
  customerName() { return `${this.first} ${this.last}`; },
},
```

### Good
```javascript
computed: {
  customerName() { return `${this.first} ${this.last}`; },
  formattedDate() { return formatDate(this.date); },
  isAvailable() { return this.stock > 0; },
  totalPrice() { return this.price * this.quantity; },
},
```

### Edge
Store-mapped computeds (e.g., `...mapState()`, `...mapGetters()`) should appear before local computeds per `rule-fw-vue-006`, then each sub-group alphabetized independently.
