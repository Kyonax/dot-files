---
id: rule-u-cs-005
title: Clean Strings
severity: LOW
tags: string, whitespace, code, flag
---

Programmatic strings such as enum values, status codes, feature flags, and lookup keys must contain no leading, trailing, or internal whitespace.

### Apply
- String literals used as identifiers, keys, enum values, or flags

### Skip
- User-facing display strings, log messages, or template content where whitespace is intentional

### Bad
```javascript
const STATUS = ' active';
const FEATURE_FLAG = 'enable_dark_mode ';
const key = ' user:preferences ';

if (role === ' admin') {
  grantAccess();
}
```

### Good
```javascript
const STATUS = 'active';
const FEATURE_FLAG = 'enable_dark_mode';
const key = 'user:preferences';

if (role === 'admin') {
  grantAccess();
}
```

### Edge
Strings intentionally containing spaces (e.g., compound display names like `'New York'`) are fine — this rule targets machine-consumed identifiers only.
