---
title: Advanced ADA Accessibility — Live Regions, Focus Management, ARIA Patterns, Sticky Offsets
impact: HIGH
impactDescription: These accessibility patterns are invisible when violated — screen readers silently drop announcements, keyboard focus falls to body, aria-controls points to nothing, and sticky elements overlap content. Standard automated scanners do not catch these issues; only manual AT testing and code review detect them.
tags: accessibility, ada, wcag, aria-live, live-region, v-if, focus, collapse, aria-controls, aria-expanded, role-link, wcag-2-5-3, label-in-name, focus-visible, sticky, sub-header, ResizeObserver, deep, override, comment, screen-reader, keyboard, navigation
---

Advanced accessibility rules that go beyond the base ADA checklist (rules 27-33, sg-9 to sg-12 in `mr-review-checklist.md`). These patterns were discovered through Andris PR review comments, Level Access ADA scanner findings, and user QA testing across DOTCOMPB-7466 (Shade Shop), DOTCOMPB-7290 (Location Services/FAQs), and DOTCOMPB-7463 (Navigation). Each rule documents the specific reviewer source and the failure mode it prevents.

## Rules

| # | Rule | Severity | Section |
|---|---|---|---|
| 1 | **live-region-outside-vif** — aria-live outside v-if + interaction flag | HIGH | [Details](#live-region-outside-vif) |
| 2 | **focus-return-collapse** — Return focus to toggle on collapse | HIGH | [Details](#focus-return-collapse) |
| 3 | **aria-controls-pair** — aria-controls mandatory with aria-expanded | MEDIUM | [Details](#aria-controls-pair) |
| 4 | **ada-rolelink-cards** — div[role="link"] for multi-content navigable cards | MEDIUM | [Details](#ada-rolelink-cards) |
| 5 | **focus-visible-rule** — :focus-visible not :focus for keyboard outlines | MEDIUM | [Details](#focus-visible-rule) |
| 6 | **sticky-subheader** — ResizeObserver on .sticky-header-wrap.is-sticky | HIGH | [Details](#sticky-subheader) |
| 7 | **deep-override-comment** — Comment :deep() overrides on design system/SDK components | LOW | [Details](#deep-override-comment) |

---

## live-region-outside-vif

`aria-live` regions MUST be rendered from component mount, outside any `v-if` block. ATs observe mutations to live regions already present and empty in the DOM. If created with text already populated (URL params), the announcement is silently dropped.

Additionally, use a `filtersEverUsed` flag to gate announcements — only fire after user interaction, not on initial load with pre-populated state.

**Incorrect:**
```pug
.results(v-if="hasProducts")
  .hiddenButPresent(aria-live="polite") {{ resultsAnnouncement }}
```

**Correct:**
```pug
.shade-shop-page
  .hiddenButPresent(aria-live="polite") {{ resultsAnnouncement }}
  .results(v-if="hasProducts")
```

```javascript
data() { return { filtersEverUsed: false }; },
computed: {
  resultsAnnouncement() {
    if (!this.filtersEverUsed) { return ''; } // silent on first load
    return `${count} shade${count === 1 ? '' : 's'} found.`;
  },
},
methods: {
  toggleFilter(value) {
    this.filtersEverUsed = true; // flips on first interaction
  },
},
```

**Exception:** Announcements that SHOULD fire on page load (error states) can be inside `v-if` if the gate transitions false→true AFTER mount.

**Source:** Andris ADA review L17, PR #20512. Implemented in commit `41c4a25`.

---

## focus-return-collapse

When a collapse toggle destroys the focused DOM node (e.g., "See Less" removes expanded cards), return focus to the toggle button via `$event.currentTarget.focus()` in `$nextTick`. Template must pass `$event`.

**Incorrect:**
```javascript
toggleSectionExpand(sectionType) {
  this.expandedSections[sectionType] = !this.expandedSections[sectionType];
  // focus falls to <body> when expanded cards are removed
}
```

**Correct:**
```javascript
toggleSectionExpand(sectionType, event) {
  const wasExpanded = this.expandedSections[sectionType];
  this.expandedSections[sectionType] = !wasExpanded;
  if (wasExpanded && event?.currentTarget) {
    this.$nextTick(() => event.currentTarget.focus());
  }
}
```

Template: `@click="toggleSectionExpand(section.type, $event)"`

Use `currentTarget` (the button), not `target`. Guard `event?.currentTarget` for safety. Only focus on collapse (`wasExpanded`), not expand.

**Exception:** When toggle hides via `v-show` or `max-height: 0` (element still in DOM), focus return is unnecessary.

**Source:** Andris ADA review L432, PR #20512.

---

## aria-controls-pair

Buttons with `aria-expanded` MUST also have `aria-controls` pointing to the controlled region's `id`. Without it, SR users can hear "expanded" but cannot navigate to the content.

**Incorrect:**
```pug
MrBtn(:aria-expanded="!!isExpanded" @click="toggle") Show More
.content(v-show="isExpanded")
```

**Correct:**
```pug
MrBtn(:aria-expanded="!!isExpanded" aria-controls="details-panel" @click="toggle") Show More
.content#details-panel(role="region" :aria-hidden="!isExpanded")
```

Dynamic IDs when pattern repeats: `:id="'grid-' + type"` + `:aria-controls="'grid-' + type"`.

**Extends:** sg-10 (which covers `aria-expanded` with `!!` coercion but not `aria-controls`).

**Source:** Andris ADA review L52, PR #20512.

---

## ada-rolelink-cards

Multi-content navigable cards: use `div[role="link"]` with `tabindex="0"` + `@keydown.enter` when WCAG 2.5.3 scanners flag `<a>`. Native `<a>` wrapping name+description+tags → concatenated accessible name → 2.5.3 "Label in Name" violation (Level A).

**Incorrect:**
```pug
a.card(:href="url" :aria-label="`View ${name}`")
  span.name {{ name }}
  span.desc {{ description }}
  span.tag {{ coverage }}
```

**Correct:**
```pug
.card(role="link" tabindex="0" @click="navigate" @keydown.enter="navigate")
  span.name {{ name }}
  span.desc {{ description }}
  span.tag {{ coverage }}
```

**Tradeoff:** Loses right-click "Open in new tab", Ctrl+click, browser status bar preview. Acceptable when navigation uses `trackMREventAndRedirect`.

**Exception:** Simple links with single text child don't need `role="link"`.

Also covers: removing `aria-label` from links where label text deviates from visible text (same WCAG 2.5.3 conflict). Drop `aria-label`; rely on sentence context for 2.4.4 Link Purpose in Context.

**Source:** Level Access scanner, Andris ADA review L33/L66/L5, PR #20512. 2.5.3 Level A overrides 4.1.2 recommendation.

---

## focus-visible-rule

Interactive elements: `:focus-visible` not `:focus` for keyboard outlines. `:focus` fires on mouse clicks, creating distracting outlines for mouse users.

**Incorrect:**
```stylus
.card
  &:focus
    outline 2px solid cta-color-1
```

**Correct:**
```stylus
.card
  &:focus-visible
    outline 2px solid cta-color-1
```

Two valid approaches: (a) `.focusable` utility class for standard outline, (b) custom `:focus-visible` in Stylus for non-standard styles.

**Source:** DOTCOMPB-7463 bug fixes (SiteNavShopContent), Andris L38 PR #20512.

---

## sticky-subheader

Sticky sub-headers below the global nav: observe `.sticky-header-wrap.is-sticky` height via `ResizeObserver` for dynamic `top` offset. Do NOT use `--mr-navigation-height` (only measures nav, not full wrap including banners) or `--mr-sticky-header-height` (dead variable — never set anywhere). Use `:style` binding, not CSS variable.

**Pattern:**
```javascript
setupStickyHeaderOffset() {
  if (typeof window === 'undefined' || typeof ResizeObserver === 'undefined') { return; }
  const wrap = document.querySelector('.sticky-header-wrap.is-sticky');
  if (!wrap) { return; } // experiment A or no sticky scaffold
  this.stickyHeaderOffset = wrap.getBoundingClientRect().height;
  this.stickyHeaderObserver = new ResizeObserver(() => {
    this.stickyHeaderOffset = wrap.getBoundingClientRect().height;
  });
  this.stickyHeaderObserver.observe(wrap);
}
```

Template: `.sticky-header(:style="{ top: stickyHeaderOffset + 'px' }")`

Check `.is-sticky` via selector (not experiment name) — decouples from experiment changes in SsrApp.vue. Disconnect observer in `beforeUnmount`.

**Source:** User QA on dotcom-feat.mdsnrd.com/shop/brown, decisions #93-96.

---

## deep-override-comment

`:deep()` overrides on design system components (MrBtn, ImgBox) or SDK DOM must include a comment explaining WHY. Extends ad-16 to Vue scoped style piercing.

**Incorrect:**
```stylus
:deep(.mrbtn)
  &:hover
    color ui-color-1
```

**Correct:**
```stylus
// MrBtn secondary: hover bg is cta-color-1, default text also cta-color-1 → invisible
:deep(.mrbtn)
  &:hover
    color ui-color-1
```

**Source:** Andris guideline #16 (56 PR comments). Reinforced by DashHudson SDK overrides (DOTCOMPB-7527) and SiteNavShopContent MrBtn fix (DOTCOMPB-7463).
