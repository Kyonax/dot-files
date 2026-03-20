---
title: Typography, Color, and Design System Variables
impact: CRITICAL
impactDescription: Ensures all text and color adheres to brand guidelines. Covers both utility classes for Pug templates and Stylus variables for style blocks.
tags: typography, font, color, text, size, weight, family, brand, palette, responsive, upper, letter-spacing, stylus, pug, css, utility, variables, brand-color, ui-color, cta-color, text-color, feedback-color, breakpoint, mixin
---

## Typography, Color, and Design System Variables

All text styling and color **must** use utility classes (in Pug templates) or design system variables (in `<style>` blocks). Never hardcode hex colors or font values. This rule covers both systems.

---

### 1. Font Family Classes (Pug Templates)

| Class | Font | Use Case |
|---|---|---|
| `.f-primary` | Averta Regular | Default body and UI text |
| `.f-primary.bold` | Averta Bold | Bolded primary text |
| `.f-secondary` | Kapra Neue | Headings — **always use with `.upper`** (includes `text-transform: uppercase`) |
| `.f-script` | Betterworks | Decorative script for special occasions |
| `.f-domaine-display-condensed` | Domaine Display Condensed | Serif display headings |
| `.f-domaine-display-condensed.medium` | Domaine Display Condensed 500 | Medium-weight serif display |
| `.didot` | Didot | Legacy serif (`.bold`, `.italic` modifiers available) |
| `.electra` | Electra | Legacy serif |
| `.kapra-neue` | Kapra Neue Medium | Alternative Kapra (modifiers: `.pro-xtr`, `.extra-light-condensed`, `.semi-bold-condensed`) |

---

### 2. Font Family Variables (Stylus `<style>` Blocks)

Auto-injected via Vite — no import needed.

| Variable | Value |
|---|---|
| `f-primary` | `'Averta-Regular', sans-serif` |
| `f-primary-bold` | `'Averta-Bold', sans-serif` |
| `f-secondary` | `'Kapra Neue', sans-serif` |
| `f-script` | `'Betterworks', sans-serif` |
| `font-family-primary-thin` | `'Averta-Thin', sans-serif` |
| `font-family-primary-light` | `'Averta-Light', sans-serif` |
| `font-family-primary-regular` | `'Averta-Regular', sans-serif` |
| `font-family-primary-semi-bold` | `'Averta-Semibold', sans-serif` |
| `font-family-primary-bold` | `'Averta-Bold', sans-serif` |
| `font-family-didot` | `Didot, 'Didot LT STD', serif` |
| `font-family-electra` | `'Electra', 'Didot', serif` |
| `font-family-kapra-neue-medium` | `'Kapra Neue', sans-serif` |
| `font-family-kapra-pro-xtr` | `'Kapra Neue Pro Xtr', sans-serif` |
| `font-family-kapra-extra-light-condensed` | `'Kapra Neue ExtraLight Condensed', sans-serif` |
| `font-family-kapra-semi-bold-condensed` | `'Kapra Neue SemiBold Condensed', sans-serif` |

---

### 3. Font Size Classes (Responsive, Mobile-First)

Apply a base size (e.g., `.xs-f-medium`) and override at larger breakpoints (e.g., `.md-f-large`).

| Size Name | Base Class | Responsive Pattern | px Value |
|---|---|---|---|
| xxx-small | `.font-xxxsmall` | `.{xs,sm,md,lg,xl}-f-xxxsmall` | 8px |
| xx-small | `.font-xxsmall` | `.{xs,sm,md,lg,xl}-f-xxsmall` | 10px |
| x-small | `.font-xsmall` | `.{xs,sm,md,lg,xl}-f-xsmall` | 12px |
| small | `.font-small` | `.{xs,sm,md,lg,xl}-f-small` | 14px |
| medium | `.font-medium` | `.{xs,sm,md,lg,xl}-f-medium` | 16px |
| x-medium | `.font-xmedium` | `.{xs,sm,md,lg,xl}-f-xmedium` | 18px |
| large | `.font-large` | `.{xs,sm,md,lg,xl}-f-large` | 20px |
| x-large | `.font-xlarge` | `.{xs,sm,md,lg,xl}-f-xlarge` | 24px |
| xx-large | `.font-xxlarge` | `.{xs,sm,md,lg,xl}-f-xxlarge` | 28px |
| xxx-large | `.font-xxxlarge` | `.{xs,sm,md,lg,xl}-f-xxxlarge` | 32px |
| grande | `.font-grande` | `.{xs,sm,md,lg,xl}-f-grande` | 36px |
| x-grande | `.font-xgrande` | `.{xs,sm,md,lg,xl}-f-xgrande` | 40px |
| xx-grande | `.font-xxgrande` | `.{xs,sm,md,lg,xl}-f-xxgrande` | 46px |
| xxx-grande | `.font-xxxgrande` | `.{xs,sm,md,lg,xl}-f-xxxgrande` | 52px |
| xxxx-grande | `.font-xxxxgrande` | `.{xs,sm,md,lg,xl}-f-xxxxgrande` | 60px |
| poster | `.font-poster` | `.{xs,sm,md,lg,xl}-f-poster` | 72px |
| x-poster | `.font-xposter` | `.{xs,sm,md,lg,xl}-f-xposter` | 100px |
| billboard | `.font-billboard` | `.{xs,sm,md,lg,xl}-f-billboard` | 128px |

**Auto-applied line heights by size:**
- `xxxsmall` through `xmedium` → `line-height-medium` (1.5em)
- `large` through `xxxlarge` → `line-height-small` (1.3em)
- `grande` and above → `line-height-xsmall` (1.1em)

**Responsive font classes use flex sizing** that scales proportionally within each breakpoint range.

---

### 4. Font Size Variables (Stylus)

| Variable | Value |
|---|---|
| `font-size-xxxsmall` | `8px` |
| `font-size-xxsmall` | `10px` |
| `font-size-xsmall` | `12px` |
| `font-size-small` | `14px` |
| `font-size-medium` | `16px` |
| `font-size-xmedium` | `18px` |
| `font-size-large` | `20px` |
| `font-size-xlarge` | `24px` |
| `font-size-xxlarge` | `28px` |
| `font-size-xxxlarge` | `32px` |
| `font-size-grande` | `36px` |
| `font-size-xgrande` | `40px` |
| `font-size-xxgrande` | `46px` |
| `font-size-xxxgrande` | `52px` |
| `font-size-xxxxgrande` | `60px` |
| `font-size-poster` | `72px` |
| `font-size-xposter` | `100px` |
| `font-size-billboard` | `128px` |

---

### 5. Font Weight and Style Classes

| Class | Effect |
|---|---|
| `.bold` | Bold weight (uses `f-primary-bold`) |
| `.semi-bold` | Semi-bold (uses `font-family-primary-semi-bold`) |
| `.normal` | Normal weight |
| `.thin` | Thin weight (uses `font-family-primary-thin`) |
| `.strike` | `text-decoration: line-through` |
| `.underline` | `text-decoration: underline` |
| `.no-decoration` | `text-decoration: none !important` |

---

### 6. Text Color Classes

**Never use hex codes directly.** Always use color classes in templates or color variables in styles.

#### Design System Numbered Colors (Preferred)

**Brand Colors:**

| Class | Hex | Use |
|---|---|---|
| `.brand-color-1` | `#3A2D4A` | Primary brand purple |
| `.brand-color-2` | `#735e73` | Secondary brand |
| `.brand-color-3` | `#41273b` | Dark plum |
| `.brand-color-4` | `#b666a9` | Bright orchid |
| `.brand-color-5` | `#58456d` | Muted purple |
| `.brand-color-6` | `#846da8` | Lavender purple |
| `.brand-color-7` | `#8E8ABC` | Soft periwinkle |
| `.brand-color-8` | `#53284F` | Deep plum |

All have `-bg` variants for backgrounds (e.g., `.brand-color-1-bg`).

**Text Colors:**

| Class | Hex | Use |
|---|---|---|
| `.text-color-1` | `#343434` | Primary text |
| `.text-color-2` | `#434343` | Secondary text |
| `.text-color-3` | `#7E727E` | Muted text |
| `.text-color-4` | `#666666` | Light text |
| `.text-color-5` | `#8d9091` | Subtle text |

**CTA Colors:**

| Class | Hex | Use |
|---|---|---|
| `.cta-color-1` | `#911885` | Primary CTA |
| `.cta-color-2` | `#600057` | CTA hover/active |
| `.cta-color-3` | `#bfbfbf` | Disabled CTA |
| `.cta-color-4` | `#853585` | Secondary CTA |
| `.cta-color-5` | `#4d0046` | Dark CTA |

All have `-bg` variants.

**UI Colors:**

| Class | Hex | Use |
|---|---|---|
| `.ui-color-1` | `#fff` | White |
| `.ui-color-2` | `#fbfbfb` | Near-white |
| `.ui-color-3` | `#f7f7f8` | Light gray bg |
| `.ui-color-4` | `#eaeaea` | Borders, dividers |
| `.ui-color-5` | `#e0e0e0` | Medium border |
| `.ui-color-6` | `#a9a9a9` | Muted gray |
| `.ui-color-7` | `#ae9faa` | Plum gray |
| `.ui-color-8` | `#d0c8cd` | Light plum |
| `.ui-color-9` | `#000000` | Black |
| `.ui-color-10` | `#E5DEE5` | Soft lavender |
| `.ui-color-11` | `#F7F3F2` | Warm white |
| `.ui-color-14` | `#D7D7D7` | Light silver |

All have `-bg` variants.

**Feedback Colors:**

| Class | Hex | Use |
|---|---|---|
| `.feedback-color-1` | `#d10022` | Error/danger |
| `.feedback-color-2` | `#ffdede` | Error background |
| `.feedback-color-3` | `#d7f0e6` | Success background |

All have `-bg` variants.

**Color Wonder Colors:**

| Class | Range | Note |
|---|---|---|
| `.cw-color-1` through `.cw-color-12` | Various | Color Wonder product line. All have `-bg` variants. |

**Mister Colors:**

| Class | Hex | Use |
|---|---|---|
| `.mister-color-1` | `#434343` | Mister primary |
| `.mister-color-2` | `#485b60` | Mister secondary |
| `.mister-color-3` | `#656565` | Mister muted |
| `.mister-color-4` | `#f3f3f3` | Mister background |

All have `-bg` variants.

#### Legacy Named Colors (Still Available)

| Class | Maps To | Note |
|---|---|---|
| `.color-mr-purple` | `brand-color-1` | Primary brand |
| `.color-mr-purple-light` | `#6A3159` | Lighter brand |
| `.color-mr-purple-dark` | darkened `color-mr-purple` | Darker brand |
| `.color-primary` | `color-mr-purple` | Alias |
| `.color-primary-light` | `color-mr-purple-light` | Alias |
| `.color-secondary` | `brand-color-2` | Alias |
| `.color-mr-black` | `#343434` | Primary text |
| `.color-slate` | `#434343` | Dark gray text |
| `.color-slate-light` | lighter gray | Secondary text |
| `.color-smoke` | purple-gray | Muted |
| `.color-accent` | reddish-pink | Accent |
| `.color-black` | `#000` | Pure black |
| `.color-white` | `#fff` | Pure white |
| `.color-gray` | medium gray | General gray |
| `.color-gray-light` | light gray | Light gray |
| `.color-gray-med` | medium gray | Medium gray |
| `.color-fog` | `#cac8c7` | Soft neutral |
| `.color-fog-light` | lighter fog | Light neutral |
| `.color-triple-nines` | `#999` | Mid gray |
| `.color-font-error` / `.color-red` | `#f00` | Errors |
| `.color-green` | `#108043` | Success |
| `.color-blue` / `.color-mr-blue` | `brand-color-2` | Informational |

---

### 7. Color Variables (Stylus `<style>` Blocks)

Use these in scoped Stylus — auto-injected, no import needed.

**Brand:** `brand-color-1` through `brand-color-8`
**Text:** `text-color-1` through `text-color-5`
**CTA:** `cta-color-1` through `cta-color-5`
**UI:** `ui-color-1` through `ui-color-11`, `ui-color-14`
**Feedback:** `feedback-color-1` through `feedback-color-3`
**Legacy named:** `color-mr-purple`, `color-mr-purple-light`, `color-mr-purple-dark`, `color-slate`, `color-fog`, `color-white`, `color-black`, `color-gray`, `mr-purple` (alias for `brand-color-1`)

```stylus
// Example usage in <style scoped lang="stylus">
.card
  background-color ui-color-3
  border 1px solid ui-color-5
  color text-color-1

.cta-button
  background-color cta-color-1
  &:hover
    background-color cta-color-2
```

---

### 8. Text Transformation and Spacing Classes

| Class | Purpose |
|---|---|
| `.upper` | `text-transform: uppercase !important` |
| `.lower` | `text-transform: lowercase !important` |
| `.capitalize` | `text-transform: capitalize` |
| `.header` | `text-transform: uppercase` + `letter-spacing: 0.13em` + `line-height-small` |
| `.ellipsis` | Truncates with `...` (`text-overflow: ellipsis` + `overflow: hidden` + `white-space: nowrap`) |
| `.break-word` | `word-wrap: break-word` |

---

### 9. Letter Spacing Classes and Variables

**Classes:**

| Class | Value |
|---|---|
| `.zero-letter-spacing` | `0` |
| `.narrow-letter-spacing` | `-0.03em` |
| `.regular-letter-spacing` | `0.03em` |
| `.semi-wide-letter-spacing` | `0.1em` |
| `.wide-letter-spacing` | `0.2em` |

**Stylus Variables:**

| Variable | Value |
|---|---|
| `letter-spacing-narrow` | `-0.03em` |
| `letter-spacing-regular` | `0.03em` |
| `letter-spacing-semi-wide` | `0.1em` |
| `letter-spacing-wide` | `0.2em` |

---

### 10. Line Height Classes and Variables

**Classes:**

| Class | Value |
|---|---|
| `.line-height-small` | `1.3em` |
| `.line-height-medium` | `1.5em` |

**Stylus Variables:**

| Variable | Value |
|---|---|
| `line-height-xsmall` | `1.1em` |
| `line-height-small` | `1.3em` |
| `line-height-medium` | `1.5em` |

---

### 11. Breakpoint Variables (Stylus)

Used in `<style>` blocks for custom media queries. Auto-injected.

| Variable | Value | Breakpoint |
|---|---|---|
| `bp-mobile-min` | `320px` | Mobile minimum |
| `bp-mobile-tweak` | `440px` | Mobile tweak |
| `bp-mobile-max` | `559px` | Mobile maximum |
| `bp-tablet-min` | `560px` | Tablet minimum |
| `bp-tablet-tweak` | `660px` | Tablet tweak |
| `bp-tablet-max` | `759px` | Tablet maximum |
| `bp-desktop-md-min` | `760px` | Desktop MD minimum |
| `bp-desktop-md-max` | `959px` | Desktop MD maximum |
| `bp-desktop-min` | `960px` | Desktop minimum |
| `bp-desktop-max` | `1299px` | Desktop maximum |

**Breakpoint Mixins (use in media queries):**

| Mixin | Range |
|---|---|
| `mq-mobile` | `≤ bp-mobile-max` (559px) |
| `mq-mobile-plus` | `≥ bp-mobile-min` (320px) |
| `mq-tablet` | `bp-tablet-min` to `bp-tablet-max` |
| `mq-tablet-plus` | `≥ bp-tablet-min` (560px) |
| `mq-desktop-md` | `bp-desktop-md-min` to `bp-desktop-md-max` |
| `mq-desktop-md-plus` | `≥ bp-desktop-md-min` (760px) |
| `mq-desktop` | `bp-desktop-min` to `bp-desktop-max` |
| `mq-desktop-plus` | `≥ bp-desktop-min` (960px) |
| `mq-max` | `≥ 1300px` |

```stylus
// Example usage
.hero
  padding 1em
  @media mq-tablet-plus
    padding 2em
  @media mq-desktop-plus
    padding 3em
```

---

### 12. Responsive Breakpoint Prefixes (Summary)

All utility classes support these prefixes for responsive overrides:

| Prefix | Breakpoint | Media Query |
|---|---|---|
| `xs-` | Mobile (320px+) | `mq-mobile-plus` |
| `sm-` | Tablet (560px+) | `mq-tablet-plus` |
| `md-` | Desktop MD (760px+) | `mq-desktop-md-plus` |
| `lg-` | Desktop (960px+) | `mq-desktop-plus` |
| `xl-` | Large (1300px+) | `mq-max` |

---

### 13. Usage Examples

**Incorrect — custom CSS:**
```stylus
.page-title
  font-family 'Kapra Neue', sans-serif
  text-transform uppercase
  font-size 24px
  color #343434
  letter-spacing 0.1em
  @media (min-width: 960px)
    font-size 32px
```

**Correct — utility composition in Pug:**
```pug
h1.f-secondary.upper.bold.xs-f-xlarge.lg-f-xxxlarge.color-mr-black.semi-wide-letter-spacing
  | Our New Collection
```

**Correct — design system variables in Stylus:**
```stylus
.card-header
  border-bottom 1px solid ui-color-4
  color text-color-1
  font-size font-size-large
  @media mq-desktop-plus
    font-size font-size-xlarge
```
