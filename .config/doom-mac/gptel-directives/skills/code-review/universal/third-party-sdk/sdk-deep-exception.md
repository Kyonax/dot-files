---
id: rule-u-sdk-004
title: ":deep() Is the Validated Exception for SDK DOM"
severity: MEDIUM
tags: sdk, deep, exception, override, scoped
---

`:deep()` is the validated exception for styling SDK-injected DOM because the SDK provides no props API for customization — scoped `:deep()` selectors are the only way to override its styles.

### Apply
- Any scoped `<style>` block that needs to override styles on DOM elements injected by a third-party SDK inside the component

### Skip
- SDK elements that expose a theming/props API for style customization (use the API instead of CSS overrides)

### Bad
```css
/* Global unscoped override — leaks to every instance */
.sdk-widget .sdk-button {
  background: #333;
}
```

### Good
```css
/* Scoped deep override — contained to this component */
.sdk-container :deep(.sdk-widget .sdk-button) {
  background: #333;
}
```

### Edge
Version-gate all `:deep()` overrides with a comment noting the SDK version they were tested against (e.g., `/* PaymentSDK v3.2 — class .sdk-button */`). SDK updates may rename or restructure internal class names, silently breaking overrides without warning.
