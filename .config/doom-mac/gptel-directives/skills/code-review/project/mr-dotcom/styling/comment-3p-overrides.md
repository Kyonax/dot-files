---
id: rule-pj-mrd-022
title: Comment :deep() Overrides
severity: LOW
tags: comment, deep, override, sdk, version
---

Every `:deep()` selector override must include a comment identifying the third-party SDK or child component and its version.

### Apply
- All `:deep()` selectors in Stylus blocks

### Skip
- `:deep()` targeting the project's own child components (still recommended but not required)

### Bad
```stylus
:deep(.StripeElement)
  border 1px solid ui-color-border
  border-radius 0.25rem
```

### Good
```stylus
// Override: Stripe.js v3 Elements — input frame styling
:deep(.StripeElement)
  border 1px solid ui-color-border
  border-radius 0.25rem
```

### Edge
When the SDK version is unknown or auto-updated, note the approximate date instead: `// Override: DY carousel widget — as of 2025-Q1`. The goal is traceability so future developers can evaluate whether the override is still needed after an SDK upgrade.
