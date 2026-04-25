---
id: rule-pj-mrd-015
title: Alphabetized CSS Properties
severity: LOW
tags: alphabetized, css, properties, order, stylus
---

CSS properties should be alphabetized within each Stylus selector block.

### Apply
- All Stylus blocks in Vue SFC `<style>` sections

### Skip
- Vendor-prefixed property groups that must stay together for readability

### Bad
```stylus
.card
  padding 1rem
  background brand-color-white
  border-radius 0.5rem
  align-items center
  display flex
```

### Good
```stylus
.card
  align-items center
  background brand-color-white
  border-radius 0.5rem
  display flex
  padding 1rem
```

### Edge
Shorthand properties (e.g., `border`) sort by their shorthand name, not by their longhand expansions. Nested selectors and pseudo-selectors appear after all properties of the parent block.
