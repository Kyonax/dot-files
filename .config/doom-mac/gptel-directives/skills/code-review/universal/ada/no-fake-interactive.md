---
id: rule-u-ada-011
title: No Fake Interactive Elements
severity: MEDIUM
tags: fake, role, tracking, div-button
---

Don't add `role="button"` (or other interactive roles) to elements that exist solely for analytics tracking and are not interactive for the user.

### Apply
- Element has `role="button"` or `role="link"` but no click handler that performs a user-facing action
- Element wraps tracking-only logic (e.g., impression pixels, analytics hooks)

### Skip
- Element genuinely performs a user-facing action on click/keypress

### Bad
```html
<!-- Tracking wrapper announced as a button to screen readers -->
<div role="button" data-track="hero-impression" class="tracking-pixel">
  <img src="banner.jpg" alt="Spring sale" />
</div>
```

### Good
```html
<!-- No interactive role — element is purely for analytics -->
<div data-track="hero-impression" class="tracking-pixel">
  <img src="banner.jpg" alt="Spring sale" />
</div>
```

### Edge
If an element is not interactive for the user, don't make it interactive for assistive technology. Adding `role="button"` to a tracking-only `<div>` creates an expectation that pressing Enter/Space will do something, which frustrates keyboard and screen reader users when nothing happens.
