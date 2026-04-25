---
id: rule-pj-mrd-052
title: Shared Components at Appropriate Level
severity: MEDIUM
tags: shared, component, level, booking, reusable
---

Place shared components at the appropriate directory level matching their reuse scope.

### Apply
- Any component used by more than one parent component

### Skip
- Single-use components that are tightly coupled to one parent

### Bad
```javascript
// Component used by multiple booking pages but placed inside a specific page directory
// website/src/vuescripts/pages/booking/confirmation/SharedBookingSummary.vue
// Imported by: BookingConfirmation.vue, BookingReview.vue, BookingReceipt.vue
```

### Good
```javascript
// Shared booking component placed at the booking level
// website/src/vuescripts/pages/booking/SharedBookingSummary.vue
// Or for site-wide reuse:
// website/src/vuescripts/components/SharedBookingSummary.vue
```

### Edge
If a component starts as single-use and later needs sharing, move it up to the appropriate level and update all imports. Do not create a proxy re-export from the old location.
