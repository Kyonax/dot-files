---
id: rule-u-ada-003
title: Role List on Styled Lists
severity: MEDIUM
tags: role-list, ul, list-style-none
---

Add role="list" to any ul or ol that has list-style: none applied via CSS.

### Apply
- Any `<ul>` or `<ol>` with `list-style: none` or `list-style-type: none` in its stylesheet

### Skip
- Lists that retain their default bullet or number styling

### Bad
```html
<ul class="no-bullets">
  <li>Item one</li>
  <li>Item two</li>
</ul>

<style>
.no-bullets { list-style: none; }
</style>
```

### Good
```html
<ul role="list" class="no-bullets">
  <li>Item one</li>
  <li>Item two</li>
</ul>

<style>
.no-bullets { list-style: none; }
</style>
```

### Edge
Safari VoiceOver strips list semantics from any `<ul>` or `<ol>` with `list-style: none`. Adding `role="list"` explicitly restores the semantic announcement. This is a well-documented Safari behavior, not a spec violation, but it affects a significant portion of assistive technology users.
