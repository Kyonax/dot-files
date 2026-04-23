---
title: Kyonax Brand — OBS Browser-Source & RECKIT Review Discipline
impact: CRITICAL
impactDescription: Kyonax HUD overlays run as OBS Browser Sources composited live over video at 60 fps on low-core-processor hardware. Without brand-specific review discipline, reviewers miss the FPS-killing patterns that look fine in a dev browser but crush the compositor in OBS — broad CSS filter/shadow utilities, per-frame allocations, non-composited animations, N OBS-WS subscriptions for N consumers. Every one of these was a real regression in RECKIT Session 10; this rule codifies the discipline learned.
tags: kyonax, reckit, obs, browser-source, hud, overlay, performance, fps, vue, vue3, composition-api, singleton, composable, gpu, composited, transform, opacity, containment, hot-path, allocation, float32array, typed-array, drop-shadow, text-shadow, filter, debounce, websocket, input-volume-meters, ram, low-core, cyberpunk-glow
---

This rule governs code review for any Kyonax-owned codebase — primarily RECKIT (the OBS capture-time automation toolkit at `Kyonax/reckit`) and any future OBS HUD/overlay projects, but also applies to any Kyonax web project where render-time cost matters. The discipline below was reverse-engineered from a real FPS regression: a `cyberpunk-glow` SCSS mixin applied via CSS-class inheritance to every `--clr-primary-100` text element crushed OBS source fps on a multi-core low-clock machine. Removing the utility and replacing it with opt-in design tokens restored fps. The rules codify that lesson and extend it to every other surface that touches per-frame cost.

## Scope

Load this rule when brand detection (`brand-detection.md`) identifies the repo as Kyonax — or when the user explicitly invokes RECKIT / OBS / HUD / Kyonax perf rules.

Applies to every file in the repo. The perf sections (A–E) especially apply to:

- Files under `@<brand>/` folders (brand HUD sources).
- Files under `src/shared/` that HUD sources import (components, composables, widgets).
- SCSS files (`src/app/scss/**`).
- Composables that own OBS WebSocket subscriptions.

Apply general Kyonax conventions (Section G) to every file.

## Severity calibration for Kyonax findings

| Level | Meaning in Kyonax context | Typical examples |
|---|---|---|
| **CRITICAL** | FPS regression risk, broken OBS WS contract, or re-introduces a removed anti-pattern | Animated `filter`/`box-shadow` inside `@keyframes`, re-introducing the `cyberpunk-glow` mixin, new OBS-WS composable without singleton pattern |
| **HIGH** | Measurable hot-path cost or architectural contract violation | Per-event allocation in hot loop, untyped array for per-frame data, `onUnmounted` cleanup on a singleton |
| **MEDIUM** | Convention break that hurts maintainability but not runtime | Wrong filename kind (Rule G), snake_case emit name, missing `contain: layout paint` on a frequently-updating sub-tree |
| **LOW** | Minor consistency or polish | Comment style, color-token harmonization opportunity |

Report only genuine issues — when unsure whether a pattern is on the hot path, ask the author about update frequency before assigning a severity.

## Section A — CSS cost: don't paint what you can't afford

RECKIT HUDs are OBS Browser Sources. OBS composites the browser's rendered frame over a separate Video Capture AFTER the browser paints. This means the browser cannot see the camera pixels — `mix-blend-mode`, `backdrop-filter`, and `color-mix()` with a dynamic backdrop all do nothing in OBS even if they "work" in dev. Every legibility/glow effect must be **drawn**, which means rasterization cost at paint time. Applied broadly, that cost kills fps.

| # | Rule | Severity | Why |
|---|---|---|---|
| A1 | **Never apply `filter: drop-shadow(...)` chains, multi-layer `box-shadow`, or `text-shadow` chains via a utility/mixin that hits every element of a color class.** Halo/glow is opt-in per element, via CSS custom properties declared at `:root` (`--hud-halo`, `--hud-halo-text`, `--hud-glow`). | CRITICAL | Every matching element rasterizes its own filter layer. With dozens of gold labels on one HUD, the compositor pays N layer-rasterization costs per frame. This was the original RECKIT FPS killer. |
| A2 | **Group halo on a single container when possible.** `filter: drop-shadow()` rasterizes the subtree as one layer — apply once at `.<hud>-overlay` or `.hud-frame`, not per leaf. | HIGH | One filter pass cost vs N filter passes. Per-leaf filter wins only when you need a leaf-specific color or intensity that differs from the container. |
| A3 | **Animate only `transform` (translate/scale/rotate) and `opacity`.** Never animate `width`, `height`, `top`/`left`/`right`/`bottom`, `box-shadow`, `filter`, `background`, or anything else that triggers layout or re-rasterization per frame. | CRITICAL | Transform + opacity use the GPU compositor. Everything else hits main-thread layout/paint. At 60 fps this difference is the line between smooth and dropped frames. |
| A4 | **Split static decoration from animated decoration onto separate layers.** When a shape needs both a dark halo (expensive) and a pulse (cheap), keep them on different DOM elements and animate only the cheap one. | HIGH | Lets the browser cache the expensive rasterization while only the cheap layer's alpha changes per frame. Example pattern: outer span owns static `box-shadow`; inner `.glow` span owns animated `opacity`. |
| A5 | **Add `contain: layout paint`** to any HUD group, widget root, or frequently-updating region. Do **not** use `contain: strict` or `contain: size` — they constrain layout too aggressively for responsive brand canvases. | HIGH | Isolates paint and layout. A bar-height update in an audio meter cannot force a layout pass on unrelated HUD labels when the meter's root carries `contain: layout paint`. |

### Section A — correct vs incorrect

**Incorrect** — animated filter inside `@keyframes` (A3 + A4 violation):
```scss
.session-date {
  color: var(--clr-primary-100);
  animation: glow-pulse 2s ease-in-out infinite;
}

@keyframes glow-pulse {
  0%, 100% { filter: drop-shadow(0 0 3px gold); }
  50%      { filter: drop-shadow(0 0 8px gold); }
}
```

**Correct** — static halo on root + animated opacity on inner layer:
```scss
.session-date {
  position: relative;
  color: var(--clr-primary-100);
  text-shadow: var(--hud-halo-text), var(--hud-glow);
}

.session-date::after {
  content: '';
  position: absolute;
  inset: 0;
  pointer-events: none;
  box-shadow: var(--hud-glow);
  opacity: 0;
  animation: breathe 2s ease-in-out infinite;
}

@keyframes breathe {
  0%, 100% { opacity: 1; }
  50%      { opacity: 0.35; }
}
```

**Why:** The halo/glow tokens are static. The `@keyframes` only touches `opacity` — GPU-composited, no re-rasterization per frame.

---

**Incorrect** — blanket color-based glow utility (A1 violation):
```scss
@mixin cyberpunk-glow($color, $blur, $spread) {
  box-shadow: 0 0 $spread $blur $color;
  animation: pulse 1.5s ease-in-out infinite alternate;
}

[style*="var(--clr-primary-100)"],
.gold-text {
  @include cyberpunk-glow(var(--clr-primary-100), 15px, 5px);
}
```

**Correct** — opt-in per element via custom property:
```scss
// In src/app/scss/abstracts/_theme.scss (global tokens only):
:root {
  --hud-glow-color: var(--clr-primary-100);
  --hud-glow:
    0 0 1px color-mix(in srgb, var(--hud-glow-color) 75%, transparent),
    0 0 5px color-mix(in srgb, var(--hud-glow-color) 50%, transparent);
}

// Per-consumer, only where you want the glow:
.hud-text--primary {
  text-shadow: var(--hud-halo-text), var(--hud-glow);
}
```

**Why:** The token exists globally but attaches to zero elements until a specific consumer opts in. No cascade surprises, no N× rasterization.

## Section B — OBS WebSocket call budget

OBS WS events like `InputVolumeMeters` fire at ~50 Hz. If N mounted consumers each register their own handler, every event pays N callback invocations + N reactive writes. Collapse to one subscription per event per page.

| # | Rule | Severity | Why |
|---|---|---|---|
| B1 | **Every OBS-WS-consuming composable is a module-level singleton.** Pattern: `let shared_state = null;` at module scope; first call registers the WS handler + builds state; subsequent calls return the same object. Applies to `useObsWebsocket`, `useRecordingStatus`, `useSceneName`, `useAudioAnalyzer`, and any future `use*` that subscribes to WS events. | CRITICAL | N consumers → N handler registrations → N callback invocations per event. Singleton collapses to 1. |
| B2 | **Singleton identity contract** — `const a = useX(); const b = useX(); expect(a).toBe(b)`. Every OBS-WS composable must have a test asserting the identity. | HIGH | Prevents accidental parameter-drift from breaking the singleton later. A caller that passes options in a way that branches `shared_state` breaks the contract silently. |
| B3 | **No `onUnmounted` cleanup on singletons.** Singletons live for the page lifetime. Cleanup would fire when the last consumer unmounts, tearing down the shared subscription for everyone. | CRITICAL | A hidden per-consumer `onUnmounted(() => obs.off(...))` on a singleton breaks every other consumer. Remove on sight. |
| B4 | **Event-driven beats rAF when the source already emits.** If OBS fires an event at 50 Hz, drive the UI off that event — do not run a parallel `requestAnimationFrame` loop that reads the same state. | HIGH | Two clocks compete; the rAF loop pays a wake-up cost even when no OBS update arrived. One clock, one update cadence. |
| B5 | **Throttle `$emit` from hot paths.** Cross-boundary Vue emits that fire per event should throttle to ~10 Hz for diagnostic readouts. Gate with `performance.now()` deltas. | HIGH | Vue emits walk the reactive graph; 50 Hz diagnostic emits consume budget reserved for actual UI updates. |

### Section B — correct vs incorrect

**Incorrect** — per-consumer subscription (B1 violation):
```javascript
// cam-log.vue
import { useRecordingStatus } from '@composables/use-recording-status.js';
const { is_recording } = useRecordingStatus({ obs, connected });

// another-overlay.vue
const { is_recording } = useRecordingStatus({ obs, connected });
// → 2 handler registrations, 2 callback invocations per RecordStateChanged event
```

**Correct** — module-level singleton:
```javascript
// use-recording-status.js
let shared_state = null;

export function useRecordingStatus() {
  if (shared_state) {
    return shared_state;
  }

  const { obs, connected } = useObsWebsocket();
  const is_recording = ref(false);
  // ... handler registration happens once here ...

  shared_state = { is_recording, /* ... */ };
  return shared_state;
}
```

**Why:** First caller wires the WS; every subsequent caller returns the same object. One handler, N subscribers.

---

**Incorrect** — rAF loop reading event-driven state (B4 violation):
```javascript
function renderFrame() {
  const level = latest_peak * GAIN;
  levels.value = computeBands(level);
  animation_id = requestAnimationFrame(renderFrame);
}
obs.on('InputVolumeMeters', (e) => { latest_peak = extractPeak(e); });
requestAnimationFrame(renderFrame);
```

**Correct** — compute in the event handler:
```javascript
obs.on('InputVolumeMeters', (e) => {
  const peak = extractPeak(e);
  // write directly to the preallocated buffer + increment tick
  updateBandsInPlace(peak);
  tick.value++;
});
```

**Why:** The rAF loop runs even when no OBS data changed, paying wake-up cost for nothing. Driving off the event gives one clock tied to real change.

## Section C — Zero-allocation hot path

A hot path (code that runs per OBS event or per frame) must allocate zero objects. Typed arrays + precomputed lookups + classic `for` loops + no destructuring.

| # | Rule | Severity | Why |
|---|---|---|---|
| C1 | **Preallocate typed arrays at setup.** `new Float32Array(bar_count)` once, mutate in place every tick. Never allocate a new `Array(...)` or object literal per event. | CRITICAL | Per-event allocation triggers GC pauses that drop frames. Typed arrays stay in a single heap slot and never trigger minor GC from mutation. |
| C2 | **Precompute lookup tables.** Replace per-frame `Math.random()` with a seeded ring table (`JITTER_TABLE: Float32Array(256)`). Replace per-frame string concat with a quantized string table (`SCALE_STRINGS`: 101 precomputed `scaleY(0.00)`...`scaleY(1.00)` strings). | HIGH | Precompute once at module load; pay zero computation cost per frame. String concat + GC interaction is especially expensive. |
| C3 | **Classic `for (let i = 0; i < len; i++)` in hot paths.** No `Array.find` / `Array.map` / `Array.forEach` / destructuring on large per-event inputs. Break out of loops as soon as the target is found. | HIGH | Iterator callbacks allocate closures; destructuring allocates binding records. Classic `for` + `break` is the baseline cost. |
| C4 | **Hardcode known targets when possible.** If only one OBS input matters to this composable, remove the configurable `source_name` option and hardcode the target name. | MEDIUM | Drops the per-event `find()` call + the fallback branch. The option was never exercised anyway. |

### Section C — correct vs incorrect

**Incorrect** — per-event allocation + destructuring + find (C1 + C3 + C4 violation):
```javascript
function handleVolumeMeters(event) {
  const { inputs } = event;
  const target = inputs.find((input) => input.inputName === source_name)
    || inputs.find((input) => input.inputLevelsMul?.length > 0);
  if (!target) return;
  const channel_levels = target.inputLevelsMul;
  const peak = Math.max(...channel_levels.map((ch) => ch[1] || 0));
  const result = new Array(bar_count).fill(peak); // new allocation per event
  levels.value = result;
}
```

**Correct** — typed array, classic loop, hardcoded target:
```javascript
const TARGET_SOURCE = 'Mic/Aux';
const PEAK_INDEX = 1;
const levels = new Float32Array(bar_count); // allocated ONCE

function handleVolumeMeters(event) {
  const inputs = event?.inputs;
  if (!inputs || inputs.length === 0) return;

  let target = null;
  for (let i = 0; i < inputs.length; i++) {
    const input = inputs[i];
    if (input.inputName === TARGET_SOURCE
        && input.inputLevelsMul
        && input.inputLevelsMul.length > 0) {
      target = input;
      break;
    }
  }
  if (!target) return;

  const channels = target.inputLevelsMul;
  let peak = 0;
  for (let i = 0; i < channels.length; i++) {
    const value = channels[i][PEAK_INDEX] || 0;
    if (value > peak) peak = value;
  }

  for (let i = 0; i < bar_count; i++) {
    levels[i] = peak; // mutate in place — zero allocation
  }
  tick.value++;
}
```

**Why:** Zero allocation per event. Breaks out of the input scan as soon as the target matches. `levels` is the same heap object across the app's lifetime.

## Section D — Vue reactivity boundary

Use reactivity for **state**. Use direct DOM writes for **per-frame paint**.

| # | Rule | Severity | Why |
|---|---|---|---|
| D1 | **Bypass Vue reactivity in per-frame hot paths.** Write to the DOM directly via template refs (`el.style.transform = ...`). Reactivity's dep graph + patch scheduling overhead is too expensive when 16 siblings update at 50 Hz. | CRITICAL | Reactive `:style` binding on 16 bars × 50 Hz = 800 patch invocations/sec. Direct DOM writes skip the entire reactivity machinery. |
| D2 | **Write-threshold skip.** Before a DOM write, compare against the last-written value and skip if `Math.abs(next − last) < threshold` (typical threshold: 0.01 for normalized 0–1 values). | HIGH | At typical audio levels, 30–40% of would-be writes are perceptual no-ops. Skipping them removes pointless style recalc. |
| D3 | **Expose a `tick: ref(0)` counter** from an event-driven composable. Consumers `watch(tick)` and read non-reactive data (`levels`, `active`, etc.) synchronously. | HIGH | One reactive write per event (the counter bump) vs N reactive writes for N values. Same information reaches consumers; much less reactivity graph churn. |

### Section D — correct vs incorrect

**Incorrect** — reactive `:style` binding on a per-frame visualizer (D1 violation):
```vue
<template>
  <div
    v-for="(level, index) in levels"
    :key="index"
    class="bar"
    :style="{ height: `${scaleLevel(level)}px` }"
  />
</template>

<script setup>
// levels is a reactive ref<number[]> updated every OBS event at ~50 Hz
</script>
```

**Correct** — static template + direct DOM writes in `watch(tick)`:
```vue
<template>
  <div
    v-for="i in bar_count"
    :key="i"
    ref="bar_els"
    class="bar"
  />
</template>

<script setup>
const SCALE_STRINGS = Array.from(
  { length: 101 },
  (_, i) => `scaleY(${(i / 100).toFixed(2)})`,
);

const { levels, tick } = useAudioAnalyzer();
const bar_els = ref([]);
const last_scale = new Float32Array(bar_count);
last_scale.fill(-1);

watch(tick, () => {
  const els = bar_els.value;
  for (let i = 0; i < els.length; i++) {
    const scale = levels[i] < 0.05 ? 0.05 : levels[i];
    if (Math.abs(scale - last_scale[i]) < 0.01) continue;
    last_scale[i] = scale;
    const idx = Math.round(scale * 100);
    els[i].style.transform = SCALE_STRINGS[idx];
  }
});
</script>
```

**Why:** Template is static after mount. The `watch(tick)` callback reads non-reactive typed-array data and writes only to elements whose value actually changed. Compositor-friendly `transform: scaleY()` replaces layout-triggering `height`.

## Section E — Event listener hygiene

| # | Rule | Severity | Why |
|---|---|---|---|
| E1 | **Debounce burst-prone `window.*` listeners** (`resize`, `scroll`) by ~100 ms using `setTimeout` + `clearTimeout`. | HIGH | `window.resize` fires ~60 Hz during drag. Expensive handlers (layout recompute, CSS custom-property writes) on every fire multiply the cost by 60×. One trailing compute is enough. |
| E2 | **Clear timers on unmount.** `onUnmounted(() => { if (resize_timer) clearTimeout(resize_timer); })`. | MEDIUM | Orphan timers leak into the next route and can fire against unmounted refs. |

### Section E — correct vs incorrect

**Incorrect** — unthrottled resize handler:
```javascript
onMounted(() => {
  window.addEventListener('resize', applyScale);
});
onUnmounted(() => {
  window.removeEventListener('resize', applyScale);
});
```

**Correct** — 100 ms trailing debounce with cleanup:
```javascript
const RESIZE_DEBOUNCE_MS = 100;
let resize_timer = null;

function handleResize() {
  if (resize_timer) clearTimeout(resize_timer);
  resize_timer = setTimeout(applyScale, RESIZE_DEBOUNCE_MS);
}

onMounted(() => {
  window.addEventListener('resize', handleResize);
});
onUnmounted(() => {
  window.removeEventListener('resize', handleResize);
  if (resize_timer) clearTimeout(resize_timer);
});
```

## Section F — Pre-merge review checklist

Walk every HUD-touching change through this checklist before approving. Any "yes" to questions 1–7 means the change needs refactoring or justification.

| # | Question | Action if "yes" | Flagged section |
|---|---|---|---|
| F1 | Does this add a `filter` / `box-shadow` / `text-shadow` on a broad selector (utility class, tag selector, `*`, attribute-based)? | Refactor to opt-in via custom property. | A1 |
| F2 | Does this animate anything other than `transform` or `opacity`? | Refactor the animation property. | A3 |
| F3 | Does this keyframe include an expensive static layer (filter/box-shadow/filter)? | Split static from animated onto separate elements. | A4 |
| F4 | Does this register an OBS WebSocket event handler outside a module-level singleton? | Move into a singleton composable. | B1 |
| F5 | Does this allocate per frame (new arrays, new objects, string concat, destructuring on large inputs)? | Preallocate typed array, precompute lookup table, use classic `for`. | C1, C2, C3 |
| F6 | Does this `$emit` on every tick / event? | Throttle to ~10 Hz with `performance.now()` gating. | B5 |
| F7 | Is this a frequently-updating sub-tree without `contain: layout paint`? | Add containment. | A5 |
| F8 | Does this own a burst-prone `window.*` event listener without a debounce? | Add `setTimeout` + `clearTimeout` debounce. | E1 |

## Section G — General Kyonax code conventions

These are lightweight conventions to flag at MEDIUM severity (convention breaks, not runtime bugs).

| # | Convention | Severity |
|---|---|---|
| G1 | **Vue emit event names are kebab-case** (`consume-trigger`, not `consume_trigger`). Props remain snake_case per project Rule I (`:is_open`, `:elapsed_time`). | MEDIUM |
| G2 | **Filenames are kebab-case** and describe purpose, never repeat the folder kind. `@modals/base-modal.vue` is redundant (alias already says "modal"); correct is `@modals/base.vue`. | MEDIUM |
| G3 | **Single-word filenames win** when the word fully conveys the thing (`hero.vue`, `frame.vue`, `icon.vue`, `chip.vue`, `badge.vue`, `card.vue`). Multi-word is justified only when single word leaves an "X of what?" question (`data-point.vue`, `status-dot.vue`). | MEDIUM |
| G4 | **No relative `../` imports.** All imports use Vite aliases (`@shared`, `@views`, `@app`, `@assets`, `@sections`, `@elements`, `@modals`, `@ui`, `@hud`, `@widgets`, `@composables`). | MEDIUM |
| G5 | **Brand colors live in `@<brand>/styles/_theme.scss` only.** `brand.js` must not carry a `colors` field — enforced by `brand-loader.test.js` assertion. | HIGH |
| G6 | **Utils are topic-based libraries**, never one-function-per-file. Group related pure helpers by topic (`markup.js`, `timecode.js`, `dom.js`). Composables are exempt (one `useX` per file). | MEDIUM |
| G7 | **Default to writing no comments** — CLAUDE.md guidance. Only add a comment when the WHY is non-obvious (hidden constraint, subtle invariant, workaround for a specific bug). Never narrate WHAT the code does. | LOW |
| G8 | **Never run `git commit` / `git push` / `gh pr create` from inside a review or edit flow.** The repo's `CLAUDE.md` forbids it absolutely. Reviewers and code generators write to files and leave git operations to the human. | CRITICAL |

## Fallback when a Kyonax file doesn't look like a HUD overlay

This brand rule loads for any Kyonax repo, but not every file in a Kyonax repo is a HUD. If the file under review is:

- A build config (`vite.config.js`, `eslint.config.mjs`, `package.json`)
- A test file (`*.test.js`)
- A data registry (`sources.js`, `brand.js`)
- A documentation file (`.org`, `.md`)
- A non-visual util (`markup.js`, `timecode.js`)

...then the per-frame perf rules (Sections A–E) do not apply as CRITICAL. Apply Section G (general conventions) only, and downgrade any Section A–E finding to informational unless the file is imported into a HUD source.

The test: trace the file's import graph. If it's reachable from a file in `@<brand>/sources/{hud,animation,scene}/`, Sections A–E are in scope at full severity. Otherwise Section G applies alone.
