---
id: rule-pj-mrd-003
title: Heading Text Inline in Pug
severity: MEDIUM
tags: heading, inline, pug, multiline
---

Heading text content must appear inline on the same Pug line as the tag, not on a separate line with a pipe character.

### Apply
- All heading tags (h1, h2, h3, h4, h5, h6) in Pug templates

### Skip
- Headings that contain dynamic bindings or child elements requiring multiple lines

### Bad
```pug
h2
  | Title Text

h3
  | Section Heading
```

### Good
```pug
h2 Title Text

h3 Section Heading
```

### Edge
When a heading mixes static text with a dynamic span or interpolation, the heading tag itself should still start on one line. Nested elements are acceptable children:
```pug
h2
  span.highlight {{ dynamicTitle }}
  |  — Subtitle
```
