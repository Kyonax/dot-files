# brand/kyonax/ — Kyonax FPS Discipline

21 rules. Loaded when brand detected as Kyonax/RECKIT.

| ID | File | Summary | Severity |
|---|---|---|---|
| rule-br-kyo-001 | css-cost-no-broadcast.md | Never broadcast filter/box-shadow/text-shadow via utility classes; opt-in per element via CSS custom properties | CRITICAL |
| rule-br-kyo-002 | css-cost-group-halo.md | Group halo on single container (HudFrame), not per-leaf element | HIGH |
| rule-br-kyo-003 | css-animate-transform-only.md | Animate only transform and opacity; never animate width, height, box-shadow, filter, background | CRITICAL |
| rule-br-kyo-004 | css-split-static-animated.md | Split static decoration from animated onto separate DOM elements | HIGH |
| rule-br-kyo-005 | css-contain-layout-paint.md | Add contain: layout paint on HUD groups/widgets/frequently-updating regions | HIGH |
| rule-br-kyo-006 | obs-ws-singleton.md | Every OBS-WS composable is module-level singleton (let shared_state = null) | CRITICAL |
| rule-br-kyo-007 | obs-ws-identity-test.md | Singleton identity contract: const a = useX(); const b = useX(); expect(a).toBe(b) | HIGH |
| rule-br-kyo-008 | obs-ws-no-unmount-cleanup.md | No onUnmounted cleanup on singletons — they live for page lifetime | CRITICAL |
| rule-br-kyo-009 | obs-event-driven.md | Event-driven over requestAnimationFrame when source already emits at ~50 Hz | HIGH |
| rule-br-kyo-010 | obs-throttle-emit.md | Throttle $emit from hot paths to ~10 Hz for diagnostic readouts | HIGH |
| rule-br-kyo-011 | hotpath-preallocate.md | Preallocate typed arrays (Float32Array) at setup, mutate in place every tick | CRITICAL |
| rule-br-kyo-012 | hotpath-lookup-tables.md | Precompute lookup tables; replace per-frame Math.random() and string concat | HIGH |
| rule-br-kyo-013 | hotpath-classic-for.md | Classic for(let i=0;i<len;i++) in hot paths; no Array.find/map/forEach/destructuring | HIGH |
| rule-br-kyo-014 | hotpath-hardcode-targets.md | Hardcode known targets when configurable option is never exercised | MEDIUM |
| rule-br-kyo-015 | reactivity-bypass-hotpath.md | Bypass Vue reactivity in per-frame hot paths; template refs + direct DOM writes | CRITICAL |
| rule-br-kyo-016 | reactivity-write-threshold.md | Write-threshold skip: if |next - last| < 0.01, skip the DOM write | HIGH |
| rule-br-kyo-017 | reactivity-tick-counter.md | Expose tick: ref(0) counter; consumers watch(tick) + read non-reactive data synchronously | HIGH |
| rule-br-kyo-018 | listener-debounce-burst.md | Debounce burst-prone window.* listeners (resize, scroll) by ~100 ms | HIGH |
| rule-br-kyo-019 | listener-cleanup-unmount.md | Clear timers on unmount via onUnmounted guard (non-singleton components only) | MEDIUM |
| rule-br-kyo-020 | premerge-checklist.md | 8-question pre-merge FPS regression checklist | HIGH |
| rule-br-kyo-021 | conventions.md | Kyonax conventions: kebab emits, snake_case props, aliases only, colors in SCSS, no git-write | MEDIUM |

**Worker instructions:** Review changed files against Kyonax FPS discipline rules. CRITICAL findings = potential FPS regression. Report YAML. Return `NO VIOLATIONS` if clean.
