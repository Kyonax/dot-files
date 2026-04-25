---
id: rule-br-kyo-007
title: Singleton Identity Contract Test
severity: HIGH
tags: singleton, identity, test, expect-be
---

Every singleton composable must have an identity contract test: `const a = useX(); const b = useX(); expect(a).toBe(b)`.

### Apply
- Any composable that follows the module-level singleton pattern (rule-br-kyo-006).

### Skip
- Composables that are explicitly documented as per-instance (non-singleton).

### Bad
```javascript
// No identity test — singleton contract is unverified
describe('useObsAudio', () => {
  it('returns levels', () => {
    const { levels } = useObsAudio();
    expect(levels.value).toEqual([]);
  });
});
```

### Good
```javascript
describe('useObsAudio', () => {
  it('returns the same instance on every call', () => {
    const a = useObsAudio();
    const b = useObsAudio();
    expect(a).toBe(b);
  });
});
```

### Edge
If the test file resets module state between tests (e.g., `vi.resetModules()`), the identity test must run within a single `it()` block without module resets in between.
