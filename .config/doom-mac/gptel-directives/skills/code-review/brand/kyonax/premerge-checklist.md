---
id: rule-br-kyo-020
title: Pre-Merge FPS Regression Checklist
severity: HIGH
tags: checklist, premerge, fps, regression
---

Run this 8-question checklist before merging any Kyonax/RECKIT/OBS code to catch FPS regressions.

### Apply
- Every PR that touches HUD components, OBS composables, animation CSS, or hot-path JavaScript.

### Skip
- Documentation-only or config-only changes with no runtime impact.

### Checklist

1. **Broad filter/shadow?** Does any CSS rule apply `filter`, `box-shadow`, or `text-shadow` via a wildcard or utility class? (rule-br-kyo-001)
2. **Animate non-transform/opacity?** Does any `@keyframes` or `transition` animate a property other than `transform` or `opacity`? (rule-br-kyo-003)
3. **Keyframe with static layer?** Does any element have both a static decoration (box-shadow/filter) and an animation on the same node? (rule-br-kyo-004)
4. **OBS handler outside singleton?** Does any OBS-WS event handler live outside a module-level singleton composable? (rule-br-kyo-006)
5. **Per-frame allocation?** Does any hot-path code allocate arrays, objects, or strings on every tick? (rule-br-kyo-011)
6. **Emit on every tick?** Does any component `$emit` at the full OBS event rate (>10 Hz) without throttling? (rule-br-kyo-010)
7. **No contain: layout paint?** Is any frequently-updating HUD region missing `contain: layout paint`? (rule-br-kyo-005)
8. **Undebounced burst listener?** Is any `resize`/`scroll` listener firing without debounce? (rule-br-kyo-018)

### Bad
```
Merging without checking — "it works on my machine" with no FPS verification.
```

### Good
```
All 8 questions answered NO. FPS budget verified in OBS preview at 1080p60.
```

### Edge
If a checklist item is YES but intentionally accepted (e.g., a one-shot entrance animation), document the exception in the PR description with measured FPS impact.
