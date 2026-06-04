# kyo-web-online — Proximity UX & Flare Animation Architecture

## Summary

Architectural decisions and reusable patterns extracted from the kyo-web-online Session 6 proximity hover system and flare animation cleanup (2026-05-29). These patterns apply to any project using CSS custom properties to drive hover effects progressively.

---

## Architecture Decisions

### ad-001 — Edge-Distance vs Center-Distance for Proximity

**Decision:** Use edge-distance formula for proximity activation, not center-distance.

**Formula:**
```js
const dx = Math.max(0, Math.max(r.left - x, x - (r.left + r.width)));
const dy = Math.max(0, Math.max(r.top  - y, y - (r.top  + r.height)));
const d  = Math.sqrt(dx ** 2 + dy ** 2);
const t  = Math.max(0, 1 - d / THRESHOLD);
```

**Why center failed:** Large cards (600px+ wide) placed the cursor at the card edge 300px from the center — beyond the 180px threshold — so `t` was always 0 and no effect fired. Center-based proximity only works reliably for small elements (skills items ~80px worked; experience cards ~600px didn't).

**Edge behavior:** Cursor inside element → `dx=0, dy=0, d=0, t=1` (full effect). Cursor `THRESHOLD` px beyond any edge → `t=0`. Works correctly for any element size.

---

### ad-002 — CSS Cascade Switch Restarts Animation Timeline

**Decision:** Never put any `animation-*` property on a `:hover` or `:focus-visible` rule targeting a `::before` pseudo-element that has an active CSS animation.

**Why:** When a `:hover` rule activates and becomes the "winning rule" for `animation-play-state`, the CSS engine interprets the cascade change as a new animation event and restarts the timeline — even if the value is identical to what the in-viewport rule was already providing (`running` → `running`). This causes a visible position reset on the animated `background-position` sweep.

**Correct pattern:** Only `[data-in-viewport="true"] .element-flare:not(.is-static)::before { animation-play-state: running; }` controls play state. No hover rule may touch any `animation-*` property.

**The `animation-name` trick (now deleted):** The original `flare-breathe-restart` keyframe (identical to `flare-breathe`, different name) was an intentional restart mechanism — changing `animation-name` forces position-0 restart. This was removed when proximity hover made it jarring.

---

### ad-003 — CSS Custom Property Driven Hover Effects

**Decision:** Wire all hover effects through a `--prox` CSS custom property so they activate gradually on cursor approach rather than binary on direct hover.

**Pattern:**
```scss
// JS sets: el.style.setProperty('--prox', t.toFixed(3))

// Border color
border-color: color-mix(in srgb, var(--clr-primary-100) calc(var(--prox, 0) * 100%), var(--clr-border-100));

// Transform
transform: translateY(calc(-4px * var(--prox, 0)));

// Flare opacity
--element-flare-opacity: calc(0.04 + var(--prox, 0) * 0.26);

// Background gradient tint
background: linear-gradient(135deg,
  color-mix(in srgb, var(--clr-primary-100) calc(var(--prox, 0) * 8%), transparent) 0%,
  color-mix(in srgb, var(--clr-neutral-500) 80%, transparent) 100%
);
```

**Key insight:** Keep existing `:hover` rules as fallback — at `--prox=1` (cursor inside element) the values are mathematically identical, so no conflict. The CSS transition on the base property handles smooth falloff on cursor leave.

**Background gradient transition:** `transition: background 0.25s ease` works in Chrome 118+/Firefox 117+/Safari 16.4+ when both sides are same-structure gradients.

---

## Design Patterns

### dp-001 — useProximityHover Composable Pattern

```js
export default function useProximityHover(containerRef, selector) {
  if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) return;

  let _items = [], _rects = [], _raf = null, _last = 0, _dirty = true;

  function _cache() { /* querySelectorAll + getBoundingClientRect per item */ }
  function _invalidate() { _dirty = true; }
  function _reset() { _items.forEach(el => el.style.removeProperty('--prox')); }

  function _onMove(e) {
    if (performance.now() - _last < 16) return;  // 60fps gate
    if (_dirty) _cache();
    if (_raf) cancelAnimationFrame(_raf);
    _raf = requestAnimationFrame(() => {
      _last = performance.now();
      // edge-distance formula per item → el.style.setProperty('--prox', t)
    });
  }

  onMounted(() => {
    root.addEventListener('pointermove', _onMove, { passive: true });
    root.addEventListener('pointerleave', _reset);
    window.addEventListener('scroll', _invalidate, { passive: true });
    new ResizeObserver(_invalidate).observe(root);
  });
}
```

**Selector syntax:** Comma-separated CSS selectors work: `'.card.has-modal, .featured-item:not(.is-static)'`

---

## Shared State & Data Flow

### sf-001 — --prox Variable Flow

```
JS pointermove event
  → edge-distance calc per element
  → el.style.setProperty('--prox', t)  [inline style, highest specificity]
    → CSS reads var(--prox, 0) in color-mix(), calc(), custom properties
      → Visual effects update at 60fps
        → Child elements inherit --prox via CSS custom property inheritance
          → Role color, glyph slide work without separate JS targeting
```

**CSS custom property inheritance:** `--prox` set on `.experience-section__card` is automatically available to all descendant elements (`__role`, `__view-more-glyph`, etc.) via CSS cascade inheritance. No need to target children in JS.

---

## Constraints & Limitations

### cl-001 — Gradient Transition Browser Support

`transition: background` with `linear-gradient()` interpolation requires same-structure gradients (same stop count, same gradient type). Supported in Chrome 118+, Firefox 117+, Safari 16.4+. For older browsers, the transition may jump discretely.

### cl-002 — Flare Animation Only Via In-Viewport Rule

The `[data-in-viewport="true"]` rule is the SOLE controller of `animation-play-state`. This is enforced by `useInViewport` composable which sets `data-in-viewport="true"` on the section root via IntersectionObserver. Any hover rule touching `animation-*` will break the no-restart guarantee.

### cl-003 — is-static Semantics After Proximity

After 2026-05-29: `is-static` on `element-flare` means "truly non-interactive — no proximity effects, no hover flare." In kyo-web-online this applies only to: project cards with no modal AND no link, featured items with no link. It does NOT apply to experience cards anymore (all 6 are active).

---

## Reusable References

### rr-001 — Proximity Hover in kyo-web-online

- Composable: `src/composables/use-proximity-hover.js`
- Used in: `experience.vue`, `now-projects-section.vue`, `skills.vue`
- Threshold: 180px from element edge
- CSS vars set: `--prox` (0→1)

### rr-002 — Element-Flare Opacity Scale (kyo-web-online)

| Element | Base opacity | Max proximity opacity | On hover |
|---|---|---|---|
| Experience primary card | 0.04 | 0.30 (calc 0.04 + prox*0.26) | 0.30 |
| Experience neutral cards | 0 | 0.08 (calc prox*0.08) | 0.08 |
| Project cards (has-modal) | 0 | 0.06 (calc prox*0.06) | 0.06 |
| Featured items | 0 | 0.06 (calc prox*0.06) | 0.06 |
| Skills items | 0.05 | 0.20 (calc 0.05 + prox*0.15) | n/a |
