---
id: rule-u-sc-009
title: Concurrent Guard Return
severity: MEDIUM
tags: concurrent, guard, return, undefined, store
---

Store actions that use a "skip if already running" guard must return the current state or a meaningful value, not a bare `return` which resolves to `undefined`.

### Apply
- Vuex/Pinia actions, Redux thunks, or any async function with an early-exit guard that callers `await`

### Skip
- Fire-and-forget actions where no caller ever reads the return value

### Bad
```javascript
const actions = {
  async loadItems({ state, commit }) {
    if (state.loading) {
      return; // Caller gets undefined — may break downstream logic
    }

    commit('setLoading', true);
    const items = await fetchItems();
    commit('setItems', items);
    commit('setLoading', false);
    return items;
  },
};

// Caller
const items = await store.dispatch('loadItems');
renderList(items); // items is undefined when guard triggers — runtime error
```

### Good
```javascript
const actions = {
  async loadItems({ state, commit }) {
    if (state.loading) {
      return state.items; // Return current state so callers always get a usable value
    }

    commit('setLoading', true);
    const items = await fetchItems();
    commit('setItems', items);
    commit('setLoading', false);
    return items;
  },
};

// Caller
const items = await store.dispatch('loadItems');
renderList(items); // Always receives an array, whether fresh or cached
```

### Edge
If the guarded action returns a different type than the normal path (e.g., normal returns `items[]` but guard returns `null`), document the contract explicitly or normalize both paths to return the same shape.
