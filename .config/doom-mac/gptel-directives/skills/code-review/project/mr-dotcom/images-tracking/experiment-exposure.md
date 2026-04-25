---
id: rule-pj-mrd-046
title: Explicit A/B Experiment Exposure Tracking
severity: MEDIUM
tags: experiment, exposure, tracking, ab-test, segment
---

Track A/B experiment exposure explicitly with a dedicated tracking call; do not rely on conditional branching alone.

### Apply
- Every component that renders differently based on an experiment variant

### Skip
- Server-side experiment allocations that are already tracked by the middleware

### Bad
```javascript
computed: {
  isVariantB() {
    return this.experiment === 'B';
  },
},
// No exposure event fired -- analytics has no record the user saw variant B
```

### Good
```javascript
mounted() {
  if (this.experiment) {
    trackMREvent('Experiment Exposure', {
      experimentName: 'homepage-hero-redesign',
      variant: this.experiment,
    });
  }
},
```

### Edge
For experiments that render below the fold, consider firing the exposure event on intersection (visibility) rather than on mount, to avoid inflating exposure counts for users who never scroll to the experiment.
