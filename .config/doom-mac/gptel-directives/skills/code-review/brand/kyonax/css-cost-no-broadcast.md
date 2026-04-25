---
id: rule-br-kyo-001
title: No Broadcast Filter/Shadow via Utility Classes
severity: CRITICAL
tags: filter, box-shadow, text-shadow, broadcast, hud, utility
---

Never apply filter, box-shadow, or text-shadow via broad utility classes; opt-in per element via CSS custom properties (`--hud-halo`, `--hud-glow`).

### Apply
- Any `.hud-*` utility class that sets filter, box-shadow, or text-shadow on all descendants or via a wildcard selector.

### Skip
- A scoped component `<style scoped>` that applies shadow/filter to a single known element.

### Bad
```css
/* Broadcasts expensive filter to every child */
.hud-glow * {
  filter: blur(4px);
  box-shadow: 0 0 12px var(--accent);
}
```

### Good
```css
/* Opt-in per element via custom property */
.hud-element {
  filter: var(--hud-halo, none);
  box-shadow: var(--hud-glow, none);
}

/* Enable on specific element only */
.scene-active .meter-ring {
  --hud-halo: blur(4px);
  --hud-glow: 0 0 12px var(--accent);
}
```

### Edge
A single utility that targets one element by ID is acceptable, but prefer the custom-property pattern for consistency and composability across the HUD layer.
