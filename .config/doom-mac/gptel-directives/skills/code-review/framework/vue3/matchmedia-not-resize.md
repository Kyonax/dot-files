---
id: rule-fw-vue-009
title: Use matchMedia Instead of Resize Listener
severity: HIGH
tags: matchMedia, resize, window, responsive, mediaquery
---

Use `window.matchMedia()` instead of `window.addEventListener('resize')` for responsive behavior; store the `MediaQueryList` reference, add a `change` listener in `mounted`, and remove it in `beforeUnmount`.

### Apply
- Any component that conditionally renders or changes behavior based on viewport width
- Logic that toggles between mobile/tablet/desktop layouts at runtime

### Skip
- Resize-dependent calculations that need the exact pixel width on every frame (e.g., canvas drawing, drag handles)
- Server-side rendering contexts where `window` is unavailable

### Bad
```javascript
export default {
  data() {
    return { isMobile: false };
  },
  mounted() {
    window.addEventListener('resize', this.checkWidth);
    this.checkWidth();
  },
  beforeUnmount() {
    window.removeEventListener('resize', this.checkWidth);
  },
  methods: {
    checkWidth() {
      this.isMobile = window.innerWidth < 768;
    },
  },
};
```

### Good
```javascript
export default {
  data() {
    return {
      isMobile: false,
      mql: null,
    };
  },
  mounted() {
    this.mql = window.matchMedia('(max-width: 767px)');
    this.isMobile = this.mql.matches;
    this.mql.addEventListener('change', this.onMediaChange);
  },
  beforeUnmount() {
    this.mql?.removeEventListener('change', this.onMediaChange);
  },
  methods: {
    onMediaChange(event) {
      this.isMobile = event.matches;
    },
  },
};
```

### Edge
For centralized responsive state shared across many components, extract the `matchMedia` logic into a composable (e.g., `useMediaQuery`) or a store getter, rather than duplicating the listener in every component.
