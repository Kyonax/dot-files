---
id: rule-u-sdk-005
title: Conditional aria-labelledby for SDK Headings
severity: MEDIUM
tags: sdk, aria, conditional, load-state, labelledby
---

Only set `aria-labelledby` after the SDK content and its heading element actually exist in the DOM; remove it if the SDK fails to load.

### Apply
- Any container with `aria-labelledby` pointing to a heading that is rendered by a third-party SDK

### Skip
- Headings that are part of the component template and always render regardless of SDK state

### Bad
```html
<!-- aria-labelledby always set — dangling reference if SDK never loads -->
<section aria-labelledby="sdk-section-heading">
  <div ref="sdkContainer"></div>
</section>
```

### Good
```html
<section :aria-labelledby="sdkLoaded ? 'sdk-section-heading' : undefined">
  <div ref="sdkContainer"></div>
</section>
```

### Edge
If the SDK fails to load, the `aria-labelledby` reference becomes dangling — pointing to an element that does not exist. Screen readers handle dangling references inconsistently: some ignore the attribute, others announce nothing, and some read the raw ID string. Always remove the attribute on failure to avoid unpredictable assistive technology behavior.
