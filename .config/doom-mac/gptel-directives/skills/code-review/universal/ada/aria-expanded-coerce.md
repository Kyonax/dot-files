---
id: rule-u-ada-009
title: Coerce Aria Expanded Values
severity: MEDIUM
tags: aria-expanded, toggle, boolean, coerce
---

Use !! (double negation) to coerce aria-expanded binding values to ensure a boolean string is always rendered.

### Apply
- Any element with a dynamic `:aria-expanded` binding tied to a reactive variable

### Skip
- Static aria-expanded attributes with hardcoded "true" or "false" string values

### Bad
```html
<button :aria-expanded="isOpen" @click="isOpen = !isOpen">
  Toggle Panel
</button>

<script>
// If isOpen starts as undefined, aria-expanded attribute is missing entirely
export default {
  data() {
    return { isOpen: undefined };
  },
};
</script>
```

### Good
```html
<button :aria-expanded="!!isOpen" @click="isOpen = !isOpen">
  Toggle Panel
</button>

<script>
export default {
  data() {
    return { isOpen: false };
  },
};
</script>
```

### Edge
Without the `!!` coercion, an `undefined` or `null` value causes Vue to omit the aria-expanded attribute entirely rather than rendering `aria-expanded="false"`. Screen readers interpret a missing aria-expanded as "this element is not expandable," which is semantically different from "this element is collapsed." Always coerce to guarantee the attribute is present with a boolean string value.
