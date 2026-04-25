---
id: rule-u-cs-003
title: Minimal Touch
severity: MEDIUM
tags: minimal-touch, format, diff
---

Only format or refactor lines you are actively modifying; do not reformat untouched code in the same file.

### Apply
- Any PR or commit that changes functionality — limit cosmetic changes to the lines being touched

### Skip
- Dedicated formatting-only commits where the entire file is intentionally reformatted

### Bad
```javascript
// Diff shows 200 lines changed, but only 3 lines are functional
-  const x=1
-  const y = 2
-  const z=   3
+  const x = 1;
+  const y = 2;
+  const z = 3;
+  const result = compute(x, y, z); // <-- actual change
```

### Good
```javascript
// Diff shows only the lines that matter
   const x=1
   const y = 2
-  const z=   3
+  const z = 3;
+  const result = compute(x, y, z);
```

### Edge
Exception: if more than 50% of the file is being changed, a full reformat in the same commit is acceptable since the diff is already large.
