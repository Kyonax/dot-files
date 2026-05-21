---
id: rule-u-seo-012
title: Images Declare Width/Height or aspect-ratio
severity: MEDIUM
tags: <img, width=, height=, aspect-ratio, img-aspect, UiImage
---

Every `<img>` or image wrapper component must declare both `width` and `height` attributes OR carry an explicit `aspect-ratio` CSS rule, so the browser reserves layout space before the image loads. Prevents CLS (Cumulative Layout Shift) and stops Google's image-aspect-ratio audit from flagging the page.

### Apply
- Any `<img>` element in HTML, Vue templates, JSX, Pug
- Image wrapper components (`<UiImage>`, `<NuxtImg>`, `<Image>`, `<NextImage>`) that ultimately render `<img>`
- Background-image patterns using `<div>` + `style="background-image:"` if the container has no fixed size

### Skip
- Decorative SVG inlined via `v-html` / `dangerouslySetInnerHTML` (vector, no CLS)
- Icons rendered via `mask-image` or font glyphs

### Bad
```html
<img :src="hero_src" alt="Portrait" />
```

### Good
```html
<UiImage
  img="kyonax_portrait"
  :alt="alt"
  aspect="3 / 4"
  :size="{ sm: 240, md: 300, lg: 360 }"
  fit="cover"
/>
```

### Edge
When the displayed aspect ratio differs from the source image's natural aspect (e.g., a 1:1 portrait cropped into a 3:4 frame via `fit: cover`), Google's image audit may still flag mismatch even though the layout is reserved correctly — that's a *source asset* problem, not a markup problem. Decide between re-exporting the source at the display ratio or accepting the audit warning. The `aspect-ratio` CSS property is widely supported (Safari 15+, all modern Chromium / Firefox) — the only reason to fall back to width/height attrs is for `<img>` outside a wrapper component that styles the box.
