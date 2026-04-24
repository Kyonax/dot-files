---
title: Mobile Viewport — Dynamic Viewport Units, Safe Area, Scroll Containers
impact: HIGH
impactDescription: Mobile viewport bugs cause untappable UI elements, scroll trapping, and dead CSS. These issues only manifest on real iOS Safari devices and are invisible in desktop browser DevTools mobile simulation.
tags: mobile, viewport, 100dvh, 100vh, ios, safari, safe-area, env, viewport-fit, scroll, overflow, overscroll, panel, nav, drawer, overlay, dynamic-viewport
---

Rules for reviewing mobile viewport behavior in fullscreen overlays, navigation panels, and scroll containers. The MR site's viewport meta tag does NOT include `viewport-fit=cover`, which has cascading implications for `env(safe-area-inset-*)` usage. These rules were validated across 5 navigation-related tickets and an iOS Safari tappability bug.

## Rules

| # | Rule | Severity | Section |
|---|---|---|---|
| 1 | **100dvh-mobile** — Use `100dvh` not `100vh` for full-viewport mobile layouts | HIGH | [Details](#100dvh-mobile) |
| 2 | **env-safearea-req** — `env(safe-area-inset-*)` requires `viewport-fit=cover` | HIGH | [Details](#env-safearea-req) |
| 3 | **single-scroll-panel** — One scroll container per mobile panel | MEDIUM | [Details](#single-scroll-panel) |
| 4 | **overflow-auto** — `overflow-y: auto` not `overflow-y: scroll` | LOW | [Details](#overflow-auto) |
| 5 | **overscroll-contain** — `overscroll-behavior: contain` in overlay scroll containers | MEDIUM | [Details](#overscroll-contain) |

---

## 100dvh-mobile

On iOS Safari, `100vh` includes the area behind the URL bar, Dynamic Island, and home indicator. Content sized to `100vh` extends past the visible viewport, making bottom elements untappable. `100dvh` (Dynamic Viewport Height) matches the actual visible area.

### Correct vs incorrect

**Incorrect:**
```stylus
.fullscreen-overlay
  height 100vh  // includes hidden area behind iOS chrome
```

**Correct:**
```stylus
.fullscreen-overlay
  height 100dvh  // actual visible viewport
```

### Browser support

iOS Safari 15.4+, Chrome 108+, Firefox 101+. Older browsers ignore `dvh` and use the preceding `vh` declaration if provided as a fallback.

### Exception

Desktop-only full-viewport layouts are unaffected (`100dvh` === `100vh` on desktop). The rule is specifically a mobile concern.

**Validated:** DOTCOMPB-7903 — "Shop All" link untappable on mobile. `SiteNavMobileV2.vue` `100vh` → `100dvh`. Decision #74 superseded all prior `env()` safe-area padding hacks.

---

## env-safearea-req

`env(safe-area-inset-bottom)` and all `env(safe-area-inset-*)` functions resolve to `0px` unless the page's `<meta name="viewport">` includes `viewport-fit=cover`. All 6 MR Pug skeleton files set `content='width=device-width, initial-scale=1'` WITHOUT `viewport-fit=cover`. Any `env()` usage in the MR codebase is currently dead CSS.

### Correct vs incorrect

**Incorrect** — dead CSS, resolves to just `4em`:
```stylus
padding-bottom calc(4em + env(safe-area-inset-bottom))
```

**Correct** — remove the dead `env()`:
```stylus
padding-bottom 4em
// OR use 100dvh for container sizing instead of padding hacks
```

### Exception

If a future ticket adds `viewport-fit=cover` to the site-wide viewport meta, `env()` values would start resolving. That is a site-wide architectural decision with cascading effects on all pages.

**Validated:** DOTCOMPB-7903 — removed `env()` from `SiteNavShopContent` (2 media queries) and `SiteNavMobileWrapper` (1 rule). All were no-ops.

---

## single-scroll-panel

In multi-panel mobile layouts (slide-in nav, wizard, accordion), each content panel must own exactly ONE scroll container (`overflow-y: auto`). Structural wrapper elements must NEVER set `overflow-y`. Setting scroll on both wrapper and child creates nested scroll traps and breaks iOS momentum scrolling.

### Correct vs incorrect

**Incorrect** — nested scroll containers:
```stylus
.mobile-wrapper
  overflow-y auto  // wrapper should NOT scroll
  .panel-content
    overflow-y auto  // creates nested scroll
```

**Correct** — wrapper is structural, panel owns scroll:
```stylus
.mobile-wrapper
  // NO overflow-y, NO height, NO flex

.panel-content
  flex 1
  overflow-y auto
```

### Edge cases

- **MainNav**: root element IS the scroll container (no child `.nav-content`). Different from SubNav/AboutNav which use a `.nav-content` child.
- **ShopContent**: scroll only applies at `@media mq-desktop-md-less`. On desktop, CSS Grid layout takes over.
- Adding `display: flex`, `flex-direction: column`, or `height: 100%` to `SiteNavMobileWrapper` BREAKS ShopContent scroll. The wrapper must remain a simple structural container.

**Validated:** DOTCOMPB-7763 — 4 mobile nav panels normalized. Each panel has exactly one scroll owner.

---

## overflow-auto

Always use `overflow-y: auto` for scroll containers, never `overflow-y: scroll`. The `scroll` value forces a visible scrollbar gutter even when content does not overflow.

### Correct vs incorrect

**Incorrect:**
```stylus
.nav-content
  overflow-y scroll  // always shows scrollbar gutter
```

**Correct:**
```stylus
.nav-content
  overflow-y auto  // scrollbar only when needed
```

**Validated:** SubNav and AboutNav changed from `scroll` to `auto` during nav scroll normalization (DOTCOMPB-7763).

---

## overscroll-contain

Scroll containers inside fullscreen overlays (modals, drawers, mobile nav) must set `overscroll-behavior: contain`. Without it, reaching the scroll boundary causes the locked `<body>` behind the overlay to scroll (scroll chaining), breaking the contained experience on iOS.

### Correct vs incorrect

**Incorrect:**
```stylus
.overlay-scroll-container
  overflow-y auto
  // scroll leaks to body at boundaries
```

**Correct:**
```stylus
.overlay-scroll-container
  overflow-y auto
  overscroll-behavior contain
```

**Validated:** `SiteNavMobileV2.vue` applies `overscroll-behavior: contain` on `.site-nav-mobile-content` class (DOTCOMPB-7763).
