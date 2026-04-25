---
id: rule-u-cs-001
title: Blank Line Spacing
severity: LOW
tags: blank-line, spacing, whitespace
---

Use exactly one blank line to separate logical blocks; never leave consecutive blank lines or zero blank lines between unrelated blocks.

### Apply
- Between function declarations, class methods, or distinct logical sections within a function

### Skip
- Tightly related one-liners that form a single logical unit (e.g., variable declarations used together on the next line)

### Bad
```javascript
function fetchUser(id) {
  const url = buildUrl(id);


  const response = await fetch(url);

  return response.json();
}
function formatUser(user) {
  return `${user.first} ${user.last}`;
}
```

### Good
```javascript
function fetchUser(id) {
  const url = buildUrl(id);

  const response = await fetch(url);
  return response.json();
}

function formatUser(user) {
  return `${user.first} ${user.last}`;
}
```

### Edge
Consecutive blank lines inside template literals or multiline strings are content, not formatting — leave them alone.
