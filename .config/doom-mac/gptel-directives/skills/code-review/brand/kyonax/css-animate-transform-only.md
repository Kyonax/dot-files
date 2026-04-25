---
id: rule-br-kyo-003
title: Animate Only Transform and Opacity
severity: CRITICAL
tags: animate, transform, opacity, keyframe, width, height
---

Animate only `transform` and `opacity` in keyframes and transitions; never animate `width`, `height`, `box-shadow`, `filter`, or `background`.

### Apply
- Any `@keyframes` block, CSS `transition`, or Vue `<transition>` that touches a property other than transform or opacity.

### Skip
- One-shot entrance animations (e.g., page load fade) that run once and are removed — FPS budget is not sustained.

### Bad
```css
@keyframes pulse-glow {
  0%   { box-shadow: 0 0 4px var(--accent); }
  100% { box-shadow: 0 0 20px var(--accent); }
}

.widget {
  transition: width 0.3s ease, filter 0.2s;
}
```

### Good
```css
@keyframes pulse-glow {
  0%   { transform: scale(1); opacity: 0.6; }
  100% { transform: scale(1.05); opacity: 1; }
}

.widget {
  transition: transform 0.3s ease, opacity 0.2s;
}
```

### Edge
`will-change: transform` is acceptable as a compositor hint but should be removed after animation ends to free GPU memory. Do not set `will-change` permanently on dozens of elements.
