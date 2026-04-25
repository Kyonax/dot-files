---
id: rule-u-sdk-003
title: Clean Up SDK Event Listeners on Unmount
severity: MEDIUM
tags: sdk, listener, cleanup, unmount, memory-leak
---

Track SDK event listeners and call `removeEventListener` in `beforeUnmount` (Options API) or `onBeforeUnmount` (Composition API) to prevent memory leaks.

### Apply
- Any component that registers event listeners on SDK-injected elements, the window, or the document as part of SDK integration

### Skip
- SDK integrations that manage their own listener lifecycle via a documented `destroy()` or `dispose()` method that the component already calls on unmount

### Bad
```js
mounted() {
  window.addEventListener('message', this.handleSdkMessage);
},
// No cleanup — listener persists after component is destroyed
```

### Good
```js
mounted() {
  this._sdkHandlers = {
    message: this.handleSdkMessage,
  };
  window.addEventListener('message', this._sdkHandlers.message);
},
beforeUnmount() {
  window.removeEventListener('message', this._sdkHandlers.message);
  this._sdkHandlers = null;
},
```

### Edge
Store listener references in non-reactive storage (a plain object property like `this._handlers`, not a `ref()` or `data()` property). Wrapping function references in Vue reactivity adds proxy overhead on every event dispatch and serves no purpose since the references are never rendered.
