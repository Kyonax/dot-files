---
id: rule-u-sc-003
title: Module Constants
severity: MEDIUM
tags: const, magic-number, static
---

Extract static, non-reactive data (error messages, retry limits, default values) into named constants at module scope outside the component or function.

### Apply
- Magic numbers or repeated string literals used in logic
- Configuration values like timeouts, limits, or default labels

### Skip
- Values that depend on runtime state or reactive data
- One-off literals whose meaning is obvious from context (e.g., `index === 0`)

### Bad
```javascript
export default {
  methods: {
    async fetchData() {
      for (let i = 0; i < 3; i++) {
        try {
          return await api.get('/data');
        } catch (e) {
          if (i === 2) throw new Error('Failed after retries');
          await sleep(1000);
        }
      }
    },
  },
};
```

### Good
```javascript
const MAX_RETRIES = 3;
const RETRY_DELAY_MS = 1000;
const RETRY_EXHAUSTED_MSG = 'Failed after retries';

export default {
  methods: {
    async fetchData() {
      for (let i = 0; i < MAX_RETRIES; i++) {
        try {
          return await api.get('/data');
        } catch (e) {
          if (i === MAX_RETRIES - 1) throw new Error(RETRY_EXHAUSTED_MSG);
          await sleep(RETRY_DELAY_MS);
        }
      }
    },
  },
};
```

### Edge
Constants that are only used inside a single function and have self-documenting names (e.g., `const isLastAttempt = i === MAX_RETRIES - 1`) are fine as local `const` declarations — no need to hoist trivial intermediates to module scope.
