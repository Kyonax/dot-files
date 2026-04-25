---
id: rule-fw-vue-006
title: Store Helpers at Top of Section
severity: MEDIUM
tags: vuex, pinia, mapState, mapGetters, mapActions, storeToRefs
---

State management helpers (mapState, mapGetters, mapActions, storeToRefs) must appear at the top of their section, with local definitions following below.

### Apply
- `computed` blocks that mix store-mapped and local computed properties
- `methods` blocks that mix store-mapped actions and local methods
- Composition API files using `storeToRefs` or store action destructuring

### Skip
- Components that have no store bindings
- Files where all computeds/methods are store-mapped (no local definitions to separate)

### Bad
```javascript
computed: {
  fullName() { return `${this.first} ${this.last}`; },
  ...mapState('cart', ['items', 'total']),
  isValid() { return this.items.length > 0; },
  ...mapGetters('auth', ['isLoggedIn']),
},
```

### Good
```javascript
computed: {
  ...mapState('cart', ['items', 'total']),
  ...mapGetters('auth', ['isLoggedIn']),
  fullName() { return `${this.first} ${this.last}`; },
  isValid() { return this.items.length > 0; },
},
```

### Edge
This applies to both Vuex (mapState, mapGetters, mapActions, mapMutations) and Pinia (storeToRefs, direct store method destructuring). The principle is the same: externally-sourced bindings first, local definitions second.
