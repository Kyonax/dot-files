---
id: rule-u-ada-014
title: Live Region Outside v-if
severity: HIGH
tags: aria-live, role-alert, v-if, live-region, conditional
---

`aria-live` containers must be persistent in the DOM and never placed inside `v-if` or other conditional rendering blocks; use an interaction flag to prevent unwanted mount announcements.

### Apply
- Any element with `aria-live="polite"`, `aria-live="assertive"`, or `role="alert"`
- Dynamic status messages (success, error, loading states)

### Skip
- Static content that never changes after mount

### Bad
```html
<!-- Live region created inside v-if — AT misses it or announces on mount -->
<div v-if="isSignedIn" aria-live="polite">
  You are signed in
</div>
```

### Good
```html
<!-- Live region is always in the DOM; content is conditionally populated -->
<div aria-live="polite">
  <span v-if="actionTaken && isSignedIn">You are signed in</span>
</div>
```

```javascript
// actionTaken starts false, flips true only on user interaction
data() {
  return {
    actionTaken: false,
  };
},
methods: {
  handleSignIn() {
    this.actionTaken = true;
    // ...sign-in logic
  },
},
```

### Edge
Use a boolean flag (e.g., `actionTaken`) that starts `false` and flips to `true` only when the user acts. This prevents announcements like "You are signed in" firing on page mount for users who are already authenticated. The live region container must exist in the DOM before its content changes, or assistive technology will not detect the update.
