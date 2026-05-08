<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the **kyo-web-online Vue 3 migration + Landing Redesign** session. Load it at the start of every conversation to gain full context without re-discovering anything. Read sections in order on first load — after that, reference them by number as needed. The migration started as a multi-phase rewrite of the user's portfolio (`kyo-web-online`) from a custom Web Components + Webpack stack onto Vue 3 + Vite. As of 2026-05-08, **Phases 0–6 are COMPLETE**, **Phase 7 (perf polish) is in flight**, **Phase 8 (CI/cleanup) is mostly done — only size-limit + Lighthouse CI remain**, and the **Landing Redesign** has settled into its production form (HudNav → Hero → Skills → Experience → NowProjects → SiteFooter) after 7+ polish rounds. The hero CTAs are now built from UiLink/UiButton `cyber` + `cyber-outline` variants (no more bespoke hero rules). All dead SFCs, dead SCSS partials, dead i18n keys, and one-shot migration scripts were swept this round. Vimeo is feature-flagged off; CCS signature is `▣`; ORCID badge sits beside the CCS MEMBER tag; cyberpunk HUD decorations live in every section; the footer signature card renders runtime browser data.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | CCS standards, reckit-alignment, color rule, scripts-first, CSS API surfaces (icon-glyph, brand-icon, ccs-glyph, heart-glyph, hud-deco, state-grid, element-flare, viewport units), Nerd Font + SVG icon strategy, image pipeline, accessibility floor, single-page landing patterns, performance rules. | Before any task. Mandatory constraints. |
| **2. Session Overview** | Project scope, 8-phase plan + landing-redesign track with status, key decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-deliverable detail: 6 plan docs + 14 scripts + UI primitives + sections + composables + reference repos + landing widgets/sections + new state model + 16 brand SVGs + dynamic footer manifest. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference path table for every plan doc, script, primitive, section, brand SVG, and source file. | When reading, editing, or locating files. |
| **5. Last Interaction** | What was just completed (2026-05-08 Phase-8 cleanup: dead SFCs / dead SCSS partials / migration scripts / orphaned i18n keys deleted; CTA abstraction into UiLink/UiButton variants); pending work; entry point for resuming. | At conversation start. |
| **6. Activity Log** | Datetime-stamped table of every meaningful event in this session. | When you need exact "what was done when". |

**Operational Rule:** When the user references a plan document by name (e.g. "the perf doc"), open it directly from §4. When they ask "where are we?", read §5 first, then check §2.2 for phase status. When they ask to run something, check the script catalogue (§3.6) — never invent a command. **Hard rule: NEVER run any git command** — the user handles git themselves (memory `feedback_no_git_commands.md`).

**Key principle:** Data may appear in multiple sections with different framing. §1 frames knowledge as a *rule to follow*; §2 as *context to understand*; §3 as an *implementation to reference*. Each section answers a different question about the same knowledge.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `session-reset` (this file), `mr-roam-node`, `reckit-roam-node`, `code-review`, `skill-architect`, `pr-scribe`. This section stores session-scoped patterns. The six plan documents (§3.1–3.6) carry the deeper rationale.

### 1.1 Reference repositories — read-only context
*   `/home/kyonax/Documents/github-kyonax/kyo-web-online-old/` is the **pre-migration mirror**. NEVER write to it. Use it to verify legacy behavior. Ideas flow old → new.
*   `/home/kyonax/Documents/github-kyonax/reckit/` is the **canonical pattern reference** — Vue 3 + Vite + same SCSS palette + reckit naming conventions (Rules A–J). Mirror eslint config, alias map, 7-1 SCSS layout. NEVER write to it.
*   Roam knowledge nodes for reckit: `~/.brain.d/roam-nodes/reckit/2026-04-17-reckit_architecture.org` and `~/.brain.d/roam-nodes/reckit/2026-04-20-reckit_naming_conventions.org`. **Read these once before writing any new SFC.**

### 1.2 CCS code standards (browser JS + Vue 3 SFC)
*   functions = `camelCase`; variables = `snake_case`; constants = `UPPER_SNAKE_CASE`; classes = `PascalCase`; filenames = `kebab-case`.
*   Component import binding = `PascalCase` (matches the kind suffix/prefix from reckit Rules A–J).
*   `<script setup>` only — Options API forbidden via lint rule.
*   `snake_case` props are accepted (CCS convention; reckit Rule I).
*   Every committed `.js` / `.mjs` / `.vue` file carries the CCS license preamble (`Copyright (c) 2026 Cristian D. Moreno — @Kyonax / GPL-2.0-only — see LICENSE`). CI gates this.

### 1.3 Naming conventions (reckit Rules A–J)
*   Rule A — `.js` imports `vue` → composable; pure → util.
*   Rule B — `.vue` calls a composable / has side effects → widget; otherwise → plain component.
*   Rule C — domain vocabulary → `components/<domain>/`; agnostic primitive → `components/ui/`.
*   Rule D — used by 2+ views → `shared` / `components`; single view → `views/components/<kind>/`.
*   Rule E — extract only when (a) `v-for`, (b) 2+ parents, (c) named top-level page section, (d) generic shell.
*   Rule F — folders exist only when grouping 2+ files of the same kind.
*   Rule G — filename never repeats the kind (`@modals/base.vue`, NOT `@modals/base-modal.vue`); the binding layer (`<BaseModal>`) carries the kind.
*   Rule H — every kind-folder gets a Vite alias + matching ESLint resolver entry at folder-creation time.
*   Rule I — template tags are PascalCase to match the import binding.
*   Rule J — `utils/` and `data/` are topic-based libraries (`timecode.js`, `markup.js`), never one-function-per-file. Composables exempt — one `useX.js` per hook.

### 1.4 Vite alias registry (16 aliases, all under `src/`)
`@app`, `@views`, `@sections`, `@elements`, `@modals`, `@components`, `@ui`, `@widgets`, `@composables`, `@utils`, `@data`, `@workers`, `@i18n`, `@config`, `@scss`, `@assets`, `@fonts`. Zero relative parent imports (`../../`) anywhere. ESLint enforces. Validated by `node scripts/check-aliases.mjs`.

### 1.5 SCSS theming
*   7-1 architecture trimmed to the only directories actually used: `abstracts/` + `base/`. `layout/` and `components/` were deleted in Phase 8 cleanup (2026-05-08) because the only files in them (`_persistent-data.scss`, `_content-data.scss`, `_marquee.scss`) were dead 2-col layout selectors.
*   `src/scss/main.scss` is now: `@use "abstracts/variables"; @use "abstracts/mixins"; @use "abstracts/theme"; @use "base";` — nothing else.
*   `additionalData: '@use "@scss/abstracts" as *;\n'` in Vite config — every SFC gets `$breakpoints`, mixins, and tokens for free.
*   `_theme.scss` (which emits CSS) is **NOT** forwarded by `abstracts/_index.scss` — it loads once via `main.scss`. If forwarded, every SFC re-emits `:root`.
*   Token consumption priority: P1 `var(--clr-*)` > P2 SCSS `map.get` (compile-time only) > P3 mixin invocations.
*   **8 color families** (was 7): primary, secondary, neutral, border, success, warning, error, **accent** (NEW 2026-05-07 — magenta `hsl(316, 90%, 60%)` for the WORKING_ON project state). Plus 2 off-palette brand tokens added 2026-05-08: `--clr-orcid-bg` (`#a6ce39`) and `--clr-orcid-fg` (`#ffffff`) for the ORCID badge.
*   Typography: 8 sizes × 3 breakpoint tiers + 4 breakpoints. Mobile (small) tier was substantially bumped 2026-05-08 — `--fs-100: 0.95rem` (was 0.625), `--fs-200: 1.05rem`, `--fs-300: 1.15rem`, `--fs-400: 1.25rem`, `--fs-500: 1.625rem`, `--fs-600: 2rem`, `--fs-700: 2.375rem`, `--fs-800: 3.125rem`. Tablet/desktop tiers untouched.
*   Body baseline (`_typography.scss`): `font-size: var(--fs-300); line-height: 1.6` (unitless — scales with descendants).
*   **Mixed-decls discipline (Sass deprecation, fixed 2026-05-08):** plain declarations may NOT appear after a nested rule (selector or `@include`) inside the same block — Sass deprecates this pattern because future CSS spec changes its meaning. Pattern to follow inside every `.foo { ... }` block: (1) all plain declarations first, (2) any `@include min/max-media-query` that ALSO sets only declarations on `&` may stay anywhere, but `@include`s containing nested selectors are nested rules and trigger the warning, (3) all nested selectors (`&__bar`, `&::before`, `:deep(...)`, sibling rules) at the bottom. The `npm run build` and `npm run dev` outputs flag every offender with file + line — fix by moving the declaration up.

### 1.6 Color usage rule (60/30/10)
*   ~60% **neutral** (`--clr-neutral-*`) — backgrounds, body text, structure.
*   ~30% **primary** (`--clr-primary-100` canonical) — emphasis, headlines, CTAs.
*   ~10% **accent / state** (`--clr-{success,warning,error,secondary,accent}-100`) — semantic state only.
*   `--clr-border-100` is a constant overlay — NOT counted in the split.
*   Forbidden: hardcoded hex / hsl literals in SFC `<style>` blocks (including in *comments* — the gate scans by text); `--clr-secondary-*` for non-brand emphasis; state colors for non-state purposes.
*   Off-palette brand tokens (`--clr-orcid-*`) live in `_theme.scss :root` so SFCs only ever reference them via `var()`.
*   Enforced by `node scripts/check-color-usage.mjs` (warning on distribution, blocking on literals).

### 1.7 Translation rules (vue-i18n)
*   `src/data/snippets.js` is the source of truth (ESM, default-exported `TRANSLATIONS`).
*   Three template patterns: P1 `{{ t('...') }}`; P2 `<i18n-t keypath="..." tag="...">` with named slots; P3 `v-html="t('...')"` for opaque HTML in `src/i18n/raw-html-keys.js` (current allowlist: `landing.nav.logo`, `landing.hero.summary`, `landing.hero.tag`, `landing.footer.signoff`, plus the legacy content-data inline-tag entries).
*   `useLanguage()` composable owns locale state. `?language=` URL param + `localStorage['kyo:lang']` + `navigator.language` + `DEFAULT_LANGUAGE` fallback chain.
*   Locale switch via `history.replaceState` — never full reload. `<html lang>` updated on every `setLanguage()`.
*   Banned: any `[trans="..."]`, `kyo:language-changed` listener, direct `TRANSLATIONS` import outside `i18n/messages.js`. Validated by `check-trans-attrs.mjs`.

### 1.8 Worker rules
*   `class-scheduler.worker.js` is **deleted**. 5 use-sites became `.element-flare` `--element-flare-delay`.
*   `now-project.worker.js` is **rewritten** — parse `Intl.DateTimeFormat` once on message receipt; tick at 1 Hz; accepts `{cmd: 'pause'|'resume'}`; drops the milliseconds field.
*   `useProjectCountdowns()` owns lifecycle, wires `visibilitychange` → pause/resume. Powers the now-shipping cards' deadline countdowns.
*   **Local 1Hz tick for WORKING_ON cards (2026-05-08):** `now-projects-section.vue` runs its own `setInterval(1000)` ref + `_format_elapsed_segments()` for STARTED IN count-up timers (no worker — count-up is simple subtraction; the worker was built for countdown only). Cleaned up in `onBeforeUnmount`.

### 1.9 Performance rules
*   LCP image gets `<link rel="preload" as="image" imagesrcset>` injected via custom `transformIndexHtml` Vite plugin. `width`/`height` attributes; `fetchpriority="high"`; `decoding="async"`.
*   All other images: `loading="lazy"`, `decoding="async"`, AVIF + WebP + JPG fallback chain.
*   Vimeo gated by `FEATURES.vimeo.enabled` (build-time flag); facade pattern; `<link rel="preconnect">` only when both `enabled` and `preconnect` are true. **Currently disabled** (zero Vimeo bytes shipped).
*   Fonts: TTF → WOFF2 + per-family unicode-range subset; `font-display: swap`; preload 2 hero fonts.
*   **Viewport units:** `100svh` (small viewport height) — never reflows on mobile chrome show/hide. Pattern: `height: 100vh; height: 100svh;` (legacy fallback first). **Hero no longer uses `min-height: 100svh`** — broke on tablet, reverted; section sizes to natural content + padding (2026-05-08).
*   `cyberpunk-glow` mixin uses single shared `@keyframes kyo-glow-pulse` + `filter: drop-shadow` (compositable).
*   **Image conversion quality bumped 2026-05-08:** `convert-images.mjs` `WEBP_QUALITY 75 → 90`, `AVIF_QUALITY 50 → 75`.
*   **`prefers-reduced-motion` global rule (2026-05-07 audit):** `_global.scss` honors the OS-level setting — collapses all animations + transitions to `0.01ms` and disables smooth-scroll. The landing has many infinite animations (~30 element-flare pseudos, grid loaders, scan-line); this is non-negotiable.
*   **Compositor-thread animations:** state-grid cells animate `opacity` only (not `background`), so no paint-thread work. Animations on `transform` and `opacity` are GPU-composited and free. Anything touching `background` / `color` / `width` / `height` triggers paint and should be used sparingly. Note: `width` / `height` IS used in the secondary-CTA corner-grow animation — accepted because it's a one-shot hover with cleanup, not infinite.
*   **`content-visibility: auto` is OFF.** Tested 2026-05-07 on the four heaviest grids (now-shipping cards, featured grid, skills categories, experience timeline). It implies `contain: paint` even when on-screen, which CLIPS hover-translateY lifts and element-flare halos at the grid edge. Reverted everywhere; rely on the cheaper opacity-based animations + reduced-motion media query instead.

### 1.10 Scripts-first principle (`SCRIPTS_AUTOMATION.md`)
*   Every mechanical / repeatable task has a script under `scripts/`. Always check the script catalogue (§3.6) before doing manual work.
*   `node scripts/precheck.mjs` is the composite gate — runs all six validators with PASS/FAIL summary. Wired as `prebuild` and CI gate.
*   `scripts/convert-images.mjs` (sharp-based) is wired as `predev` and `prebuild` — generates WebP (q=90) + AVIF (q=75) variants beside every JPG/PNG in `src/assets/app/`.

### 1.11 Phase ordering — never skip
*   Phases 0–6 complete on `develop` (2026-05-05 → 2026-05-06).
*   Each phase ends with both a working `npm run dev` AND a working `npm run build`.
*   The deep-dive docs (§3) are canonical for their phase. The main plan (§3.1) is the table of contents.
*   The Landing Redesign track (`vue-migration` branch, 2026-05-07 → 2026-05-08) supersedes Phase 6's two-column composition but does not change Phase 7/8 deliverables.

### 1.12 Feature flags (build-time, tree-shakeable)
*   `src/config/features.js` is the single source of truth.
*   Flag pattern: master switch + sub-options (e.g. `vimeo.{enabled, facade, poster, preconnect}`).
*   Env override via `VITE_<NAME>_ENABLED=false` exposed through Vite `define`.
*   Disabled features are tree-shaken — zero bytes shipped.
*   **Current state:** `vimeo.enabled = false`, `vimeo.preconnect = false`. Flip to `true` when a new intro video is ready.

### 1.13 UI primitive composition
*   The landing is composed from **7 UI primitives** under `@ui/`: `UiCard`, `UiLink`, `UiButton`, `UiImage`, `UiIcon`, `UiSectionHeading`, **`BrandIcon`**.
*   `UiButton` and `UiLink` mirror APIs: `variant`, `size` (`sm | md | lg`), `flareDelay`.
*   **`UiLink` variants** (validator): `'primary' | 'secondary' | 'ghost' | 'card' | 'cyber' | 'cyber-outline'`. **`UiButton` variants:** same set minus `card`. The `cyber` + `cyber-outline` pair was added 2026-05-08 to absorb the bespoke hero CTA design (§1.29).
*   **`UiLink` primary/secondary** use `inline-flex; align-items: center; justify-content: center; gap: 0.4rem; line-height: 1` — mirrors `UiButton`. Fix for icon+text misalignment in the hero CTAs.
*   **Universal flex centering rule:** every interactive primitive must use `inline-flex` + `place-items: center` + `line-height: 1`. Native `<button>` UA defaults misalign content baseline-style with custom fonts.

### 1.14 BrandIcon vs Nerd Font glyph strategy
*   **Default:** Nerd Font glyph (codepoint ≥ U+E000) inside a `<span class="icon-glyph">`. The bundled `SymbolsNerdFontMono` ships with every Nerd Font glyph (~1 MB). Zero extra requests.
*   **Exception — use `BrandIcon` when:** the bundled Nerd Font lacks an accurate / current logo. **16 brand SVGs** currently authored (2026-05-08): `x`, `next`, `vue`, `jest`, `tiktok`, `css`, `node`, `express`, `symfony`, `vite`, `nest`, `postgresql`, `mongodb`, `githubactions`, `ts`, `orcid`. Plus `BRAND_ICON_IDS` in skills.vue → `{'css', 'ts', 'next', 'vue', 'jest', 'node', 'express', 'symfony', 'vite', 'nest', 'postgresql', 'mongodb', 'githubactions'}` (orcid is consumed directly by the hero badge, not the skills grid).
*   `BrandIcon` lives at `@ui/brand-icon.vue`. SVG sources live in `src/assets/brands/<name>.svg`. The component uses `import.meta.glob('@assets/brands/*.svg', { eager: true, query: '?raw', import: 'default' })` so SVG markup is inlined at build time and inherits `currentColor`. **Filename must match the tech ID** — e.g. `ts.svg` (not `typescript.svg`) because the basename is the lookup key.
*   SVG authoring rules: `viewBox="0 0 24 24"` square canvas; fills use `currentColor` (no hardcoded `#fill`); file is `kebab-case.svg` matching the BrandIcon `name` prop.
*   **All paths are pulled verbatim from Simple Icons** (`https://cdn.jsdelivr.net/npm/simple-icons@latest/icons/<slug>.svg`). When `fill-rule="evenodd"` matters (Next.js, Express, Jest), it goes on the `<path>` element (not the `<svg>` — propagation isn't reliable across browsers).
*   Sizing: `.brand-icon--lg` = 1.5rem; `.brand-icon--xl` = 2rem. Same modifier scale as `.icon-glyph`. Both apply `transform: translateY(-0.08em)` so Nerd Font glyphs and inline SVGs share the same visual baseline (override locally when nesting inside flex pills — see ORCID chip).

### 1.15 Glyph storage rule — encoding-deterministic only
*   Inline-typed PUA characters in source files (`''` typed as the raw glyph) are **not encoding-deterministic** — when the file is round-tripped through some editors, the bytes can be silently lost or replaced. Verified failure: typed glyphs occasionally save as empty strings or replacement chars.
*   **Use `'\uXXXX'` JS escape sequences** for every Nerd Font glyph in `.vue` and `.js` files. They survive round-trips byte-for-byte.
*   To verify a file's glyphs after editing: `python3 -c "import sys; s=open(sys.argv[1]).read(); [print(f'L{s[:i].count(chr(10))+1} U+{ord(c):04X}') for i,c in enumerate(s) if ord(c)>=0xE000]" <file>`. Every printed codepoint should match the intended glyph.
*   **Tilde scrub for SpaceMono Spanish strings (2026-05-07):** Spanish HUD labels rendered in SpaceMono drop tildes (the tilde glyph reads as misaligned in that mono font). Confirmed swaps: `MENÚ → MENU`, `UBICACIÓN → UBICACION`, `BOGOTÁ → BOGOTA`, `INICIÓ EN → INICIO EN`, `CONTÁCTAME → CONTACTAME`. Geomanist body copy keeps tildes.

### 1.16 Element-flare CSS API (cyberpunk halo on borders)
*   `.element-flare` triggers an animated gradient halo via `::before` pseudo-element. Emitted directly in `_theme.scss`.
*   Tunable via CSS custom properties on the host: `--element-flare-color`, `--element-flare-speed` (24s), `--element-flare-spread` (1px), `--element-flare-breath-duration` (6s), `--element-flare-opacity` (default `0.45`), `--element-flare-delay` (stagger).
*   **Opacity tier convention:** default 0.45 → primary-colored cards 0.12 (now-projects main + featured) → skills items 0.08 rest, 0.12 hover (very subtle). Primary at full opacity is too aggressive against a black background; tier down on saturated colors.
*   **Bleed-fix combo:** `gap: 1.25rem` + `isolation: isolate` + `--element-flare-spread: 2px` for adjacent flare-containing elements.

### 1.17 Nerd Font handling — unicode-range is critical
*   Bundled `SymbolsNerdFontMono` is the icon source. Glyphs live in PUA (`U+E000-F8FF`, `U+F0000-FFFFD`, `U+10000-10FFFF`).
*   `font-face` mixin parameterized: `@include font-face($name, $path, $weight, $style, $range: "latin")`.
*   **`SymbolsNerdFontMono` MUST declare `$range: "icons"`** — otherwise the default Latin range silently swallows the glyph codepoints (tofu boxes).
*   **`SymbolsNerdFontMono` MUST come first in any font-family chain that needs glyphs.**

### 1.18 `.icon-glyph` utility (Nerd Font centering primitive)
*   Global utility class in `_theme.scss`. Applies to any `<span>` wrapping a Nerd Font codepoint.
*   `inline-flex` + `1em × 1em` + `align-items/justify-content: center` + `vertical-align: middle` + `flex-shrink: 0` + `transform: translateY(-0.18em)` (pushed up further 2026-05-07 to align with text x-height).
*   Sizing: `--icon-glyph-size` (default `1em`); `.icon-glyph--lg` (1.5rem); `.icon-glyph--xl` (2rem).
*   **Use everywhere** there's a Nerd Font glyph. Never hand-roll glyph centering.
*   **Footer override:** `.site-footer__social-icon { transform: translateY(0) }` cancels the global lift inside the 44×44 social grid cells where the lift reads as "floating high".

### 1.19 `.ccs-glyph` utility (CCS signature scaling)
*   Wraps the `▣` (U+25A3) signature so it matches the visual weight of adjacent SpaceMono Latin characters. Without this, the tiny inner-square glyph reads as a footnote-sized dot.
*   `font-size: 1.75em; line-height: 0; vertical-align: -0.08em; display: inline-block` (in `_theme.scss`).
*   Used in: `kyo-web.landing.hero.tag` (en+es snippets, via v-html) and the `FRAME // ▣-001` literal in `hero.vue` visual-meta.
*   The `landing.hero.tag` key is on the RAW_HTML_KEYS allowlist so the embedded `<span class="ccs-glyph">` survives v-html.

### 1.20 `.heart-glyph` utility (footer signoff accent)
*   Wraps `♥` (U+2665) inside the `MADE WITH L♥VE.` brand-line in the footer signoff. Lifts it to lowercase x-height and recolors it to primary-yellow without affecting the surrounding text.
*   Targeted via `:deep(.heart-glyph)` inside `.site-footer__signoff-text`: `display: inline-block; font-size: 0.95em; line-height: 1; vertical-align: -0.06em; margin: 0 0.04em; color: var(--clr-primary-100)`.
*   `landing.footer.signoff` is on the RAW_HTML_KEYS allowlist so the embedded span survives v-html.

### 1.21 `.state-grid` micro-loader primitive (NEW unified 2026-05-07 audit)
*   3×3 grid of 2px cells with 1px gaps (~8×8 total). Lives in `src/scss/abstracts/_theme.scss` as a single global utility — used by the hero CCS-MEMBER tag and the now-shipping project status badges.
*   **Performance choice:** cells animate `opacity` (0.18 → 1 → 0.18) on a 1.6s cycle, NOT `background` color. Opacity is GPU-composited; background is paint-thread. With up to 6 NowShipping cards × 9 cells = 54 simultaneous paint-property animations, the original `background` cycling triggered full-section repaints every frame on lower-end devices. The opacity-only approach keeps the staggered diagonal wave intact while staying entirely on the compositor.
*   Stagger delays form a diagonal wave: `delay = (row + col) × 100ms`, range 0–400ms.
*   **`--state-color` CSS var** controls cell color — set on the parent card (`var(--clr-${status_color}-100)`). Default fallback: `var(--clr-primary-100)`.
*   Companion: `.state-square` (project-local, in `now-projects-section.vue`) — a static 8×8 filled square in the same color, used by featured cards (no animation — featured states are stable).

### 1.22 `.hud-deco` decoration utility (cyberpunk ambience)
*   Section-level corner labels + giant kanji watermarks. Lives in `_theme.scss`.
*   Variants: `.hud-deco--tl/--tr/--bl/--br` (corner anchor at 1.25rem inset from section edge); `.hud-deco--watermark` (large faint kanji behind section title).
*   Always `aria-hidden="true"`, `pointer-events: none`, never animated. Section root needs `position: relative` AND `overflow: hidden` (to clip giant watermarks).
*   **Opacity:** corner labels at `0.32` (legible at arm's length but clearly decorative); watermarks at `0.04` (atmospheric texture).
*   Watermark sizes: `8rem` mobile, `12rem` md+. Geomanist 900 weight, primary color.
*   **Per-section content (avoid duplicating any visible text):**
    - **Hero**: TR `// HANDSHAKE :: VERIFIED`, BL `// VECTOR :: KYO-001`. NO watermark (Japanese watermark removed per user feedback). On `max-md` both corners are pulled flush with section edges (`top: 0.6rem` / `bottom: 0.6rem`) since hero uses tight 3rem padding.
    - **Skills**: TR `// SYNC :: 22 NODES` (stack count), BL `// デベロッパー`, WM `開発者` (kaihatsu-sha — developer).
    - **Experience**: TR `// LOG :: VERIFIED`, BL `// 進化`, WM `過去` (kako — past).
    - **NowProjects**: TR `// PIPELINE :: OPEN`, BL `// 未来`, WM `未来` (mirai — future).
    - **Footer**: TL `// BEACON :: ON`, TR `// CHANNEL :: CCS // KYONAX // ZERONET` (md+ only via `.site-footer__deco-channel`).

### 1.23 Image pipeline (sharp-based, NOT vite-imagetools)
*   `vite-imagetools` was removed (intercepted `.avif` imports). Replacement: `scripts/convert-images.mjs` walks `src/assets/app/*.{jpg,jpeg,png}` and generates `.webp` (q=90) + `.avif` (q=75) beside each source. Wired as `predev` and `prebuild`.
*   LCP preload uses a custom `transformIndexHtml` Vite plugin in `vite.config.js`.

### 1.24 Single-page landing layout
*   Single-column scroll flow: `HudNav` (sticky) → `HeroSection` (`#hero`) → `SkillsSection` (`#skills`) → `ExperienceSection` (`#experience`) → `NowProjectsSection` (`#projects`) → `SiteFooter` (`#contact`).
*   `html { scroll-behavior: smooth; scroll-padding-top: 4.5rem }` in `_global.scss` to clear the sticky nav on anchor jumps.
*   `HudNav` uses `IntersectionObserver` (rootMargin `-45% 0px -45% 0px` — widened from `-40%/-55%` 2026-05-07 so the active band is 10% tall, more reliable trigger). Plus a `scrollY < 80` fallback that forces `active = 'hero'` near the top (the observer alone could miss this state on initial load).
*   Scroll progress bar inside the nav is driven by `scrollY / (scrollHeight - clientHeight)` exposed as `--progress` CSS var.
*   **Active link underline:** `transform: scaleX(0/0.55/1)` on `::after` pseudo (rest/hover/active) with `transform-origin: left center` and `transition: transform 0.35s cubic-bezier(0.4, 0, 0.2, 1)`. GPU-composited; reverses cleanly mid-click.
*   **Mobile drawer** (`< md`): full-width panel with backdrop blur matching the scrolled bar, single-tap rows. Animated underline hidden — each row is its own click target.
*   **Mobile bar layout (2026-05-08 final):** `grid-template-columns: auto 1fr auto; gap: 0; padding: 0.6rem 1rem` (symmetric). Brand carries `margin-left: 0.5rem`. Hamburger is 44×44 at `size="md"` with its own border. The middle `1fr` column distributes spacing — no grid `gap` because that pushed the hamburger inward.
*   Section content is bounded by `max-width: 1280px; margin: 0 auto` with `padding: 5rem 1.5rem` (md: `6rem 2rem`). Each section starts with an index tag (`// 02`), a Geomanist title, a SpaceMono subtitle, and a divider border.

### 1.25 ADA dropdown menu pattern
*   For locale picker / context menus: use `role="menu"` on the popup `<ul>` and `role="menuitemradio"` (with `aria-checked`) on each option rendered as a real `<button type="button">`.
*   Trigger button: `aria-haspopup="menu"`, `aria-expanded`, `aria-controls` matched to the menu's `id`.
*   Keyboard contract: `ArrowDown`/`Enter`/`Space` on trigger opens menu and focuses active option. Within menu: `ArrowUp`/`ArrowDown` cycle, `Home`/`End` jump, `Enter`/`Space` select, `Escape` closes (returns focus to trigger), `Tab` closes without trapping.
*   Click-outside via `useClickOutside`. Focus return to trigger on close (`trigger.value.$el.focus()`).
*   Avoid decorative pseudo-elements like `::before { content: "› " }` on focusable items — they add layout uncertainty and can cause text wrap on narrow widths.

### 1.26 Whole-card click pattern
*   When a card represents a single navigation target (project tile, featured tile), wrap the entire card in `<a>` and let the anchor get `cursor: pointer` for free. Avoid placing a small "VIEW" link inside an info block — users expect the whole card to be clickable.
*   Use `<li><a class="card">...</a></li>` so the list semantics (`role="list"`) survive. Set `display: contents` on the `<li>` if needed for the grid layout.
*   **Polymorphic root for URL-less cards (NEW 2026-05-08):** `<component :is="card.has_link ? 'a' : 'div'">`. The `<div>` branch omits `href/target/rel` and gets `is-static` class which kills `cursor: pointer` and the hover transform/border. Footer for url-less cards becomes a dashed `// ENDPOINT :: CLASSIFIED` chip (via `kyo-web.landing.projects.no-link` i18n key).

### 1.27 Layout — `.page-grid` (legacy, fully removed 2026-05-08)
*   The legacy two-column SCSS partials (`_persistent-data.scss`, `_content-data.scss`, `_marquee.scss`) plus their `_index.scss` and the empty `layout/` + `components/` directories were **deleted** in Phase 8 cleanup. `main.scss` no longer `@use`s them. No more dead CSS targeting `#persistent-data` / `.main-data`.

### 1.28 Accessibility floor
*   Every scrollable region needs `tabindex="0"` + `role="region"` + `aria-label`. Landing sections all carry `role="region"` + `aria-label="<title>"`.
*   `:focus-visible` outline globally in `_global.scss`. `.sr-only` utility for screen-reader-only text.
*   Buttons get real `<button>` (`UiButton`); links get real `<a>` (`UiLink`). No `<div role="button">` styled-as-anchor anti-patterns.

### 1.29 Hero CTA design (now in UiLink/UiButton primitives, 2026-05-08)
*   **The angular cyber + corner-grow design moved out of `hero.vue` into the `cyber` + `cyber-outline` variants on UiLink + UiButton.** Hero now consumes them via `<UiLink variant="cyber" ...>` and `<UiLink variant="cyber-outline" ...>`. The bespoke `__cta-primary` / `__cta-secondary` SCSS blocks and the `.hero__cta` shared class were deleted along with the dead `&__watermark` rule.
*   **`cyber` variant (CV download / primary):** angular frame via `clip-path: polygon(0 0, calc(100% - 14px) 0, 100% 14px, 100% 100%, 14px 100%, 0 calc(100% - 14px))` (cuts top-right + bottom-left corners). SpaceMono uppercase, 0.16em letter-spacing. Hover: subtle 2px lift + primary tint (`color-mix 12% primary on neutral-500`), no full color flip, no flare/sweep.
*   **`cyber-outline` variant (Contact / secondary):** corner-grow-into-full-border animation. Two pseudo brackets at TL + BR (14×14 default), animate `width 0.4s cubic-bezier(0.4, 0, 0.2, 1)` then `height 0.4s ... 0.2s` (staggered) to `100%+2px`. Edges draw, halves meet at center → full border. Only `width`/`height` animate, so CSS interpolates back to rest size on mouse-out — partial states reverse cleanly. Hover also fades text to primary.

### 1.30 ORCID badge (hero, sibling of CCS MEMBER tag)
*   Lives in `.hero__tag-row` (a flex container alongside `.hero__tag`) — `display: inline-flex; align-items: stretch; gap: 0.5rem`. Both pills identical height, identical SpaceMono / fs-200 / 0.4rem 0.8rem padding / square corners.
*   ORCID brand colors faded to ~55% via `color-mix(in srgb, var(--clr-orcid-bg) 55%, transparent)` on text + border. Keeps the chip visually subordinate to the neutral CCS MEMBER tag beside it.
*   Background: `color-mix(in srgb, var(--clr-neutral-500) 60%, transparent)` (same as CCS MEMBER).
*   **No-visual-hover pattern (matches CCS MEMBER):** every interactive state (`:hover`, `:focus`, `:focus-visible`, `:active`) pinned to the resting visual. Only `cursor: pointer` indicates clickability.
*   Inline icon: `font-size: 1.1em; transform: translateY(0.06em)` (slight downward nudge — overrides global `.brand-icon { translateY(-0.08em) }` because flex centering wins inside the chip).
*   Target URL: `https://orcid.org/0009-0006-4459-5538`. ORCID brand SVG at `src/assets/brands/orcid.svg`.

### 1.31 Project state model (NEW 2026-05-07)
*   **`PROJECT_STATUS`** map in `src/data/projects.js` — 9 states across two surfaces:
    - **Now-shipping (5):** `WORKING_ON` (accent magenta — current employer/contract), `DONE` (success green), `IN_PROGRESS` (primary yellow), `ON_HOLD` (warning orange), `ON_TODO` (secondary blue).
    - **Featured (4):** `LIVE` (success green), `DEPRECATED` (error red), `UPDATING` (primary yellow), `RELEASE` (secondary blue).
*   **`NOW_STATUS_PRIORITY`** drives sort order. WORKING_ON tops the list (priority 0). Within same status, closest deadline first.
*   **Card body stays uniform.** State color is **scoped narrowly**:
    - **NowShipping:** only the status badge (label + state-grid loader) carries `--state-color`. Border, background gradient, version chip, segment chips, link hover, flare — all stay `var(--clr-primary-100)`. So every NowShipping card has the same yellow body; the only thing that changes between states is the badge.
    - **Featured:** state color carries on the **square indicator + status label + version chip only**. Border + flare default to neutral; hover bumps to primary. So Featured cards: body uniform → only square / label / version reflect LIVE / DEPRECATED / UPDATING / RELEASE.
*   **Caps:** `NOW_MAX = 6`, `FEATURED_MAX = 9`.
*   **Version chip on every card.** For WORKING_ON entries the field is repurposed for work modality (`REMOTE`, `HYBRID`, etc.).
*   **`description` field** (added to PROJECTS schema 2026-05-07): when present, overrides the deadline-derived milestone label (so a WORKING_ON card can carry "CLIENT MADISON REED" without abusing the `deadlines` map).

### 1.32 Project countdown rendering
*   Three rows inside `__countdown`:
    - **Row 1 (`__countdown-head`):** `ENDS IN` label + the deadline as a human-readable date. Both styled identically (SpaceMono, fs-200, 0.12em letter-spacing, primary color) — read as one continuous phrase.
    - **Row 2:** segment chips (`Xd · NNh · NNm · NNs`).
    - **Row 3 (`__countdown-tz`):** `// BOGOTA // GMT-05` (smaller, dimmer SpaceMono).
*   Date format via `Intl.DateTimeFormat` — `timeZone: 'America/Bogota'`, locale-aware (`en-US` / `es-CO`), output uppercased to match HUD register. Deadlines authored as Bogotá local; parsed by appending `GMT-0500`.
*   **Card numbers prefixed with `#`** (e.g. `#01`).
*   **WORKING_ON cards: STARTED IN count-up timer** (same shape, label = `STARTED IN`, date = start date, segments = elapsed time ticking at 1Hz). When the user changes status to `DONE` (or removes the entry), the card drops out of the WORKING_ON tier on next render.
*   **Card footer:** github icon (`.icon-glyph--lg`) + "VIEW REPO" + external-link icon (`--icon-glyph-size: 0.85em` — slightly smaller than default for visual hierarchy). 0.85rem gap between icon and text.

### 1.33 Footer SYS // SIGNATURE manifest (NEW 2026-05-08)
*   **Replaces the static signature line** with a `<dl>`-rendered manifest of runtime browser data. Decorative — opacity 0.55, fs-100, neutral-300 labels / neutral-200 values.
*   Grid: `repeat(auto-fit, minmax(140px, 1fr))` — 1 col phone, 2-3 tablet, 6 desktop.
*   **Fields (every value comes from a real browser API — no curated brand strings here):**
    - HOST — `window.location.host`
    - PATH — `window.location.pathname`
    - LOCALE — reactive on `useI18n().locale`
    - LANG — `navigator.language`
    - VIEWPORT — `window.innerWidth × window.innerHeight`, ref-tracked via passive `resize` listener (cleaned up in `onBeforeUnmount`)
    - TZ — `Intl.DateTimeFormat().resolvedOptions().timeZone`
*   **SSR/static-prerender safety:** refs start empty, hydrate in `onMounted`; `Intl` evaluation is safe at module load.
*   **Above the manifest:** prose tagline (`landing.footer.signoff` i18n key, on RAW_HTML_KEYS): "Built with Vue 3 + Vite + vue-i18n + Workers + Sharp + SCSS 7-1 — MADE WITH L♥VE." (Spanish: "Construido con Vue 3 + Vite + … — MADE WITH L♥VE." — same English close-tag in both locales). The `♥` is wrapped in `<span class="heart-glyph">`.
*   Card sits in the brand block, full-width on every viewport (the brand block always spans `grid-column: 1 / -1` of the footer top grid).

### 1.34 Footer composition (final 2026-05-08)
*   `.site-footer__top` is a 2-column grid (`1fr 1fr`) on every viewport; column gap widens from `1.25rem → 3rem` at `min-md`. The brand block (logo + signature card) always spans both columns via `& > :first-child { grid-column: 1 / -1 }`. Channels + socials sit side-by-side underneath.
*   Brand block stacks vertically: `.site-footer__logo` (full-width, `max-width: 480px` desktop, none mobile) → `.site-footer__signoff` (also full-width).
*   Logo SVG recoloring: filter chain transforms black source → primary-yellow.
*   `// END OF TRANSMISSION` divider has `margin-top: 14rem` (mobile) / `16rem` (md) for breathing room. The bottom row (© + DESIGNED BY) has `margin-top: 3rem` (mobile) / `4rem` (md) clear of the divider.

---

## SECTION 2: SESSION OVERVIEW

> Project context, scope, and current phase status.

### 2.1 Purpose
Migrate the user's portfolio site at `/home/kyonax/Documents/github-kyonax/kyo-web-online/` from native Web Components + vanilla JS + Webpack 5 to **Vue 3 (`<script setup>`, Composition API) + Vite**, then redesign the layout as a futuristic single-page recruiter-grade landing while preserving:

1. SCSS architecture (7-1, identical palette to reckit, every token by name).
2. Translation system (vue-i18n@9, three template patterns, fixed 12 latent bugs).
3. Web-worker logic where it pays for itself (`now-project.worker.js`).
4. Reckit's proven code conventions (lint config, alias map, naming Rules A–J, CI workflow).
5. Major performance gains (LCP −1.6s, fonts −90%, largest image −85%).

The migration lives on branch `vue-migration`. Reference repository at `../kyo-web-online-old/` is the pre-migration mirror.

### 2.2 Scope
| Item | Type | Summary | Status |
|---|---|---|---|
| `Phase 0` | baseline | `audit-baseline.mjs` snapshot before changes | **DONE** (2026-05-05) |
| `Phase 1` | tooling | Vite + plugin-vue + 16-alias map + reckit ESLint + Vitest + `.editorconfig` | **DONE** (2026-05-05) |
| `Phase 2` | scss | 7-1 SCSS to `src/scss/`; `large` typography emit; `cyberpunk-glow` → `filter: drop-shadow`; TTF → WOFF2 subset | **DONE** (2026-05-06) |
| `Phase 3` | i18n | vue-i18n + `useLanguage` composable; migrated 58 `[trans=]`; deleted legacy translation plugins | **DONE** (2026-05-05) |
| `Phase 4` | workers | Deleted `class-scheduler`; rewrote `now-project.worker.js`; built `useProjectCountdowns` | **DONE** (2026-05-05) |
| `Phase 5` | components | UI primitives (`UiCard`, `UiLink`, `UiButton`, `UiImage`, `UiIcon`, `UiSectionHeading`, `BrandIcon`) + domain components | **DONE** (2026-05-06) |
| `Phase 6` | composition | `App.vue` + section SFCs; responsive `.page-grid`; full-viewport `100svh`; accessibility passes | **SUPERSEDED by Landing Redesign** (2026-05-07) |
| `Phase 7` | head/perf | Unhead meta tags via `useSeoHead`; LCP `<link rel="preload">` via custom Vite plugin; sharp-based image pipeline (now q=90 webp / q=75 avif) | **IN PROGRESS** (2026-05-06 → 2026-05-08) |
| `Phase 8` | cleanup | Delete `webpack.config.js`, `translation-webpack-plugin.js`, `cheerio` dep, dead SFCs, dead SCSS partials, migration scripts, orphaned i18n keys; mirror reckit's CI; size-limit + Lighthouse CI; CCS license headers | **MOSTLY DONE** (2026-05-08) — only size-limit + Lighthouse CI remain |
| `Landing Redesign` | layout | Single-page futuristic landing; HudNav + Hero + Skills + Experience + NowProjects + SiteFooter; BrandIcon SVG primitive; ADA dropdown; Vimeo flagged off | **DONE** (2026-05-07), polish ongoing 2026-05-08 |
| `Projects state model` | feature | NowShipping (5 states + 6-card cap + sort) + Featured (4 states + 9-card cap), version chips, count-up STARTED IN timer for WORKING_ON, polymorphic URL-less cards | **DONE** (2026-05-07) |
| `Footer dynamic manifest` | feature | SYS // SIGNATURE replaced with browser-state readout (HOST/PATH/LOCALE/LANG/VIEWPORT/TZ); prose tagline ending `MADE WITH L♥VE.` | **DONE** (2026-05-08) |
| `HUD decorations` | feature | Global `.hud-deco` utility; corner labels + giant kanji watermarks per section; opacity 0.32 corners / 0.04 watermarks | **DONE** (2026-05-08) |
| `ORCID badge` | feature | Sibling of CCS MEMBER tag in hero; ORCID brand SVG; faded brand colors; no-visual-hover pattern | **DONE** (2026-05-08) |

### 2.3 Key Decisions (Session-Wide)

1.  **(2026-05-05)** **Vite over Webpack.** Native worker support, faster HMR, custom translation plugin disappears.
2.  **(2026-05-05)** **vue-i18n@9 with `legacy: false`** (Composition API).
3.  **(2026-05-05)** **Delete `class-scheduler.worker.js`** entirely. 5 use-sites became `.element-flare` `--element-flare-delay`.
4.  **(2026-05-05)** **Keep + rewrite `now-project.worker.js`** — parse-once + 1 Hz tick + visibility-pause.
5.  **(2026-05-05)** **Mirror reckit verbatim** — eslint config, naming Rules A–J, alias map, 7-1 SCSS.
6.  **(2026-05-05)** **60/30/10 color usage rule.**
7.  **(2026-05-05)** **Vimeo as a feature flag** — tree-shaken when disabled.
8.  **(2026-05-05)** **Scripts-first** — automation under `scripts/`, composite `precheck.mjs` is the CI gate.
9.  **(2026-05-05)** **Reckit roam nodes are the tiebreaker** for naming-convention ambiguity.
10. **(2026-05-06)** **Removed `vite-imagetools`** — replaced with `scripts/convert-images.mjs` (sharp pre-build).
11. **(2026-05-06)** **Element-flare emitted directly (NOT via mixin)** — Sass modern compiler dropped nested `&::before`.
12. **(2026-05-06)** **Parameterized `font-face` mixin with `$range`** — fixed PUA glyphs being range-stripped.
13. **(2026-05-06)** **Adopted `100svh`** for full-viewport layouts (later removed from hero — broke on tablet).
14. **(2026-05-06)** **`.icon-glyph` utility class** — universal Nerd Font glyph centering primitive.
15. **(2026-05-06)** **Universal `.ui-button` flex centering.**
16. **(2026-05-07)** **Single-page landing replaces 2-column.** `App.vue` rewritten; `BannerSection`, `PersistentDataSection`, `ContentDataSection` no longer composed.
17. **(2026-05-07)** **HudNav with IntersectionObserver active link** + scroll-progress bar; CV button removed from nav (only language toggle + menu trigger remain). Active-link underline switched to `transform: scaleX` (GPU-composited).
18. **(2026-05-07)** **Vimeo `enabled = false` + `preconnect = false`** — zero Vimeo bytes shipping until a new video is recorded.
19. **(2026-05-07)** **All 6 PROJECTS entries originally future-dated (2026-Jul through 2026-Dec).** Replaced 2026-05-07 by the new state model + AGILE ENGINE WORKING_ON entry.
20. **(2026-05-07)** **BrandIcon component for inaccurate Nerd Fonts.** Inlines SVGs from `src/assets/brands/*.svg` via `import.meta.glob` raw query. Started with 5 brand SVGs; **expanded to 16** by 2026-05-08 (added css, node, express, symfony, vite, nest, postgresql, mongodb, githubactions, ts, orcid).
21. **(2026-05-07)** **Glyphs stored as `\uXXXX` JS escapes**, not raw inline chars.
22. **(2026-05-07)** **ADA-compliant LanguageToggle** — `role="menu"` + real `<button role="menuitemradio">` per option, full keyboard nav, focus return on close.
23. **(2026-05-07)** **Element-flare default opacity reduced** from 0.9 → 0.45 globally; primary-colored cards 0.12; skills items even lower (0.08 / 0.12).
24. **(2026-05-07)** **CCS signature: `▣` (U+25A3 WHITE SQUARE CONTAINING SMALL SQUARE).** Visual fidelity to actual CCS org logo (nested stepped square). Wired into `tag` (en+es snippets) and the `FRAME // ▣-001` visual-meta literal in `hero.vue`. The `京` in `A.K.A. KYONAX京` is *personal Kyonax brand* — does **not** change. Sized via `.ccs-glyph` utility (font-size: 1.75em; vertical-align: -0.08em).
25. **(2026-05-07)** **Project state model rewrite.** 5 NowShipping states + 4 Featured states. New `accent` color family (magenta, hsl(316, 90%, 60%)) for WORKING_ON. NOW_MAX=6, FEATURED_MAX=9. NOW_STATUS_PRIORITY for sort. Version chip on every card. Per-card `--state-color` CSS var; state color narrowly scoped (status badge only on NowShipping; square+label+version on Featured). Card body uniform across states.
26. **(2026-05-07)** **3×3 state-grid loader replaces all status dots.** Unified primitive in `_theme.scss`. Animates `opacity` (GPU-composited), not `background` color (paint-thread). 54+9 paint-property animations eliminated.
27. **(2026-05-07)** **Polymorphic project card root.** `<component :is="card.has_link ? 'a' : 'div'">`. URL-less cards render as `<div>` with `is-static` class (no cursor: pointer, no hover-lift) and a `// ENDPOINT :: CLASSIFIED` alt-text footer.
28. **(2026-05-07)** **STARTED IN count-up timer for WORKING_ON cards.** Same countdown UI shape, but counts elapsed time from start date. Local `setInterval(1000)` ref (cleaned up in `onBeforeUnmount`). Browser auto-throttles to ≥1Hz when tab backgrounded.
29. **(2026-05-07)** **Performance audit:** added global `prefers-reduced-motion` rule. Switched grid loaders from `background` cycling to `opacity` cycling. Tested `content-visibility: auto` on heavy grids — REJECTED because it implies `contain: paint` even when on-screen, which clipped hover lifts and element-flare halos.
30. **(2026-05-08)** **CCS MEMBER tag is a clickable `<a>` to `https://github.com/ccs-devhub`.** No visual hover changes — only cursor signals clickability. Pattern reused for ORCID badge.
31. **(2026-05-08)** **Hero copy final form.** Summary opens "8 years of experience delivering **scalable, adaptable, and high-performance** web solutions for national and international clients. Currently Frontend Engineer at **AgileEngine** for **Madison Reed**. Founder & Lead Engineer at **Zerønet Labs**. Skilled on workflows, architecture, performance optimization, and migrations that cut technical debt." (Same shape in es). Lead key deleted; alias `A.K.A. KYONAX京` (text-transform uppercase, smaller relative font); years-value gets `<span class="hero__stat-suffix">YEARS|AÑOS</span>` smaller superscript-ish suffix.
32. **(2026-05-08)** **Stat labels rewritten** (en/es): `EXPERIENCE OF` / `EXPERIENCIA DE`; `TECH SKILLS` (both locales); `PROJECTS` / `PROYECTOS`. Tilde scrub: `MENÚ → MENU`, `UBICACIÓN → UBICACION`, `BOGOTÁ → BOGOTA`, `INICIÓ EN → INICIO EN`, `CONTÁCTAME → CONTACTAME`.
33. **(2026-05-08)** **Image swap.** New IMG_6550.png portrait → `kyonax_multiverse_characters` variants regenerated (5 JPG sizes 100/300/600/900/canonical + WebP/AVIF for each). Source PNG removed from Downloads. Conversion quality bumped (`WEBP_QUALITY 75 → 90`, `AVIF_QUALITY 50 → 75`).
34. **(2026-05-08)** **Brand SVGs refreshed via Simple Icons.** Re-fetched next, express, jest, symfony with `fill-rule="evenodd"` on the `<path>` (not the `<svg>` — propagation isn't reliable). Added 5 new techs (vite, nest, postgresql, mongodb, githubactions) plus typescript (renamed `ts.svg` so the BrandIcon basename glob matches the `'ts'` tech id).
35. **(2026-05-08)** **TECHNOLOGIES expanded from 17 → 22.** Added: vite, nest, postgresql, mongodb, githubactions. Categories: Frontend (9), Backend (8), DevOps (5).
36. **(2026-05-08)** **Footer dynamic SYS // SIGNATURE.** Replaced static prose with `<dl>`-rendered manifest of runtime browser data (HOST/PATH/LOCALE/LANG/VIEWPORT/TZ). Prose tagline above ends `MADE WITH L♥VE.` (same English in both locales — universal brand sign-off). `♥` wrapped in `.heart-glyph` for primary-color tinting + x-height alignment.
37. **(2026-05-08)** **Cyberpunk HUD decorations.** Global `.hud-deco` utility; per-section corner labels + kanji watermarks. Hero: HANDSHAKE / VECTOR (no Japanese watermark per user request). Skills: SYNC / デベロッパー / 開発者. Experience: LOG / 進化 / 過去. NowProjects: PIPELINE / 未来 / 未来. Footer: BEACON / CHANNEL.
38. **(2026-05-08)** **ORCID badge sibling to CCS MEMBER.** `.hero__tag-row` flex container. Both pills identical height/padding/font. ORCID brand colors faded to ~55% opacity; transparent-dark bg; same no-visual-hover pattern as CCS MEMBER. New `--clr-orcid-bg` (#a6ce39) + `--clr-orcid-fg` (#fff) tokens in `_theme.scss`.
39. **(2026-05-08)** **Visual-meta line below image** changed `EXP. 8 YEARS | 2018 - 2026` → `@KYONAX_ON_TECH` (Twitter handle, plain text, not clickable). The years stat already shows the experience.
40. **(2026-05-08)** **Mobile/tablet responsive overhaul.** Mobile typography tier substantially bumped (`--fs-100: 0.625 → 0.95`, etc — see §1.5). Hero image circular-width bug fixed (the `display: flex` on `.hero__visual-frame` made `.ui-image` flex item collapse to min-content; switched to `display: block` + explicit `:deep(.ui-image) { width: 100% }` on max-md). Hero portrait aspect-ratio overridden to `1/1` on max-md with `max-width: 320px`. Tablet (768-1023) now matches mobile single-column layout (image-first via `order: -1` extended to max-md). Footer mobile two-column (brand spans full row, channels+socials side-by-side). Hero scroll-hint hidden on mobile. Mobile bar `gap: 0 + symmetric padding 0.6rem 1rem` (after iterating through asymmetric attempts — the issue was the grid `gap`, not the padding).
41. **(2026-05-08)** **Section subtitles bumped + breathable.** All three section subtitles (skills/experience/projects): `font-size: var(--fs-300) → var(--fs-400)`, `line-height: 1.6`, `letter-spacing: 0.012em`, `word-spacing: 0.04em`, color → `var(--clr-neutral-100)` (slightly stronger contrast).
42. **(2026-05-08)** **AGILE ENGINE WORKING_ON sample card.** name `AGILE ENGINE`, `description: 'CLIENT MADISON REED'`, `version: 'REMOTE'` (modality), `started: 'Nov 03 09:00:00 2024'`, no URL. Renders top-most via NOW_STATUS_PRIORITY=0. The `description` field overrides the default deadline-derived label.
43. **(2026-05-08)** **Sass `mixed-decls` warnings cleared.** Reordered plain declarations to come BEFORE any nested rules in four blocks: `.hero` root (declarations + `@include min-media-query(md) { padding }` first; `@include max-media-query(md) { :deep(.hud-deco--*) }` last), `.hero__scroll-hint` (declarations first; `@include max-media-query(md) { display: none }` after; sibling `&:hover` and `.icon-glyph` last), `.site-footer` root (declarations + min-md `@include` first; `&__deco-channel` nested rule after), `.site-footer__logo` (`filter:` chain before `@include max-media-query(md)`). Build + dev now silent; precheck still ✓ all six. Pattern documented in §1.5.
44. **(2026-05-08)** **CTA abstraction into UiLink + UiButton.** Added `cyber` (clip-path angular frame) + `cyber-outline` (corner-grow-into-full-border) variants. Validators extended: UiLink `['primary','secondary','ghost','card','cyber','cyber-outline']`; UiButton same set minus `card`. Hero `__cta-primary` / `__cta-secondary` blocks + the `.hero__cta` shared class deleted. Two CTAs that lived as bespoke hero rules now lean on the primitives.
45. **(2026-05-08)** **Phase 8 cleanup batch 1 — dead-code sweep.** Deleted: 6 dead SFCs (`banner.vue`, `persistent-data.vue`, `content-data.vue`, `widgets/now-projects.vue`, `vimeo-video.vue`, `tech-stack.vue`); 3 dead SCSS partials (`_persistent-data.scss`, `_content-data.scss`, `_marquee.scss`) + their parent `_index.scss` + empty `layout/` and `components/` SCSS directories; 4 one-shot migration scripts (`migrate-snippets-to-esm.mjs`, `migrate-trans-attrs.mjs`, `scaffold-sfc.mjs`, `audit-baseline.mjs`); the `vite-imagetools` transitive dep (replaced with direct `sharp ^0.34.5`). `package.json` `scripts` slimmed: `migrate:*`, `scaffold:sfc`, `audit:baseline` removed. `main.scss` simplified to 4 `@use` lines.
46. **(2026-05-08)** **i18n pruning to live keys only.** `src/data/snippets.js` 382 → 245 lines: dropped `widget.discount`, `component.marquee`, `persistent-data.{user.*,profession,important-data}`, `contact.copy`, `content-data.{about-me.title, download.title, experience.label, intro-title, description, todo-pr, feature, now}`, all 5×2 `experience.<id>.tools-label`, legacy `footer.{main-profile,terms-brief,follow-main,follow-tech}`, legacy `now-projects.{ends-in,ended}`. `src/i18n/raw-html-keys.js` 49 → 32 lines: allowlist now covers only live HTML strings (`about-me.description`, 5×3 experience cards, `landing.{nav.logo, hero.tag, hero.summary, footer.signoff}`).
47. **(2026-05-08)** **Comment sweep across SCSS + Vue files.** Removed status / update / "what does this do" commentary; preserved CCS license preambles (gated by `check-license-headers.mjs`) and the few comments that document non-obvious *why*. Default convention going forward: write no comments unless the WHY is non-obvious.

### 2.4 Pending Work
*   **Phase 7 polish:** verify every image variant emits in `dist/` after a clean build; confirm the `transformIndexHtml` LCP preload tag lands in built `index.html`; sweep remaining SFCs for `loading="lazy"` + `decoding="async"` on non-LCP images.
*   **Phase 8 — remaining items only:** wire `size-limit` and Lighthouse CI jobs into `.github/workflows/ci.yml` (the existing 4-job workflow covers eslint / precheck / vitest / build). Everything else from Phase 8's original scope (webpack files, cheerio, dead SFCs, dead SCSS, migration scripts, license headers) is **already done** as of 2026-05-08.
*   **Source PNG re-import for portrait:** the existing `kyonax_multiverse_characters.jpg` was once-encoded at q=82 from the original IMG_6550.png (now deleted). For best quality, drop IMG_6550.png back into Downloads and re-run `node scripts/convert-images.mjs --force` so all variants regenerate from the lossless source.
*   **Architecture extraction:** consider extracting BrandIcon strategy + glyph-encoding rule + ADA dropdown pattern + `.state-grid` opacity-animation pattern + polymorphic-card root pattern + dynamic-manifest pattern + UI-primitive `cyber`/`cyber-outline` variant pattern into a persistent architecture memory file once they survive a couple more sessions without changes.
*   **Smoke test on real devices:** the new landing's `100svh`-free hero + sticky nav + brand SVGs combo + dynamic manifest deserves a manual pass on iPhone Safari + Chrome Android.

---

## SECTION 3: IMPLEMENTATIONS

> Per-deliverable detail. Each plan document is canonical for its phase; the scripts are the execution layer; the landing widgets/sections are the current page composition.

### 3.1 `VUE_MIGRATION_PLAN.md` (top-level plan)
**Created:** 2026-05-05 | **Last updated:** 2026-05-05
**Status:** Authored; phases 0–6 executed against it; Landing Redesign supersedes Phase 6's composition only.
**Path:** `/home/kyonax/Documents/github-kyonax/kyo-web-online/VUE_MIGRATION_PLAN.md`

Top-level plan. References every deep-dive companion. Architectural target tree (reckit kind-folder layout). Risk register. Feature-flag pattern. 8-phase plan. Testing checklist. Effort estimate ~6.5–7 days.

### 3.2 `TRANSLATION_MIGRATION.md` (i18n deep dive)
**Created:** 2026-05-05 | **Status:** Implemented (Phase 3 complete).
**Path:** `/home/kyonax/Documents/github-kyonax/kyo-web-online/TRANSLATION_MIGRATION.md`

Phase 3 detail. Eight sections: audit, 12 latent bugs, target design, improvements, step-by-step plan, file diff, testing checklist, decisions.

### 3.3 `PERFORMANCE_MIGRATION.md` (perf deep dive)
**Created:** 2026-05-05 | **Last updated:** 2026-05-08
**Status:** Mostly implemented (Phase 7 in progress); image-pipeline strategy revised; quality bumped 2026-05-08.
**Path:** `/home/kyonax/Documents/github-kyonax/kyo-web-online/PERFORMANCE_MIGRATION.md`

Workers + image strategy + fonts + Vimeo + cyberpunk-glow refactor + scroll/resize modernization + bundle compression + perf budgets. Plus the 2026-05-07 audit results (prefers-reduced-motion, opacity-based animations, content-visibility rejection).

### 3.4 `SASS_THEMING_MIGRATION.md` (SCSS theming deep dive)
**Created:** 2026-05-05 | **Last updated:** 2026-05-08
**Status:** Implemented (Phase 2 complete); palette extended (accent + orcid tokens); typography small tier rebumped.
**Path:** `/home/kyonax/Documents/github-kyonax/kyo-web-online/SASS_THEMING_MIGRATION.md`

Token verification, architecture mirroring reckit, `additionalData` injection, scope migration map, 60/30/10 rule, Phase 2 step-by-step, bug fixes.

### 3.5 `CODE_STANDARDS_MIGRATION.md` (lint + naming deep dive)
**Created:** 2026-05-05 | **Status:** Implemented (Phase 1 complete).
**Path:** `/home/kyonax/Documents/github-kyonax/kyo-web-online/CODE_STANDARDS_MIGRATION.md`

Full ESLint flat config, reckit Rules A–J, 16-alias Vite map, SFC + composable templates, CCS license-header convention, CI workflow plan.

### 3.6 `SCRIPTS_AUTOMATION.md` + `scripts/` (automation layer)
**Created:** 2026-05-05 | **Last updated:** 2026-05-08 (`convert-images.mjs` quality bump).

#### 3.6.1 Validation gates (always-runnable)
| Script | What it does | Wired in `precheck`? |
|---|---|---|
| `check-i18n.mjs` | Locale parity + RAW_HTML_KEYS allowlist | yes |
| `check-i18n-keys.mjs` | Every `t('...')` resolves to a key | yes |
| `check-trans-attrs.mjs` | Zero banned references in `src/` | yes |
| `check-color-usage.mjs` | 60/30/10 audit + zero hardcoded color literals (scans `<style>` blocks AND comments) | yes |
| `check-aliases.mjs` | Vite ↔ ESLint resolver alias sync | yes |
| `check-license-headers.mjs` | CCS preamble on every file | yes |
| `precheck.mjs` | Composite gate | (composite) |

#### 3.6.2 Build pipeline scripts (Phase 8 cleanup deleted the migration one-shots)
| Script | What it does | When to run |
|---|---|---|
| `convert-fonts.sh` | Phase 2 TTF → WOFF2 | Phase 2 once |
| `convert-images.mjs` | Sharp WebP (q=90) + AVIF (q=75) generator | Every dev/build (predev/prebuild) |

> **Deleted 2026-05-08** (Phase 8 batch 1): `audit-baseline.mjs`, `migrate-snippets-to-esm.mjs`, `migrate-trans-attrs.mjs`, `scaffold-sfc.mjs`. They were one-shot helpers for already-completed phases; removing them also dropped the `audit:baseline`, `migrate:snippets`, `migrate:trans`, `scaffold:sfc` `package.json` entries.

#### 3.6.3 Shared helper
*   `_lib.mjs` — file walking, colored output, CI exit, CCS-header detection.

### 3.7 UI primitives (`src/components/ui/`)
**Created:** 2026-05-06 | **Status:** All 7 in production; consumed by landing.

| Primitive | Path | Role |
|---|---|---|
| `UiCard` | `src/components/ui/card.vue` | Bordered container with `padding` and `as` props |
| `UiLink` | `src/components/ui/link.vue` | `<a>` with `variant` (`primary`\|`secondary`\|`ghost`\|`card`\|`cyber`\|`cyber-outline`), `size`, `external`, `flareDelay`, `download` |
| `UiButton` | `src/components/ui/button.vue` | `<button>` mirroring `UiLink` API; `variant` (same set minus `card`) |
| `UiImage` | `src/components/ui/image.vue` | Wraps `BlastImage`; aspect/size/fit/position/scale/framed/sizes/eager |
| `UiIcon` | `src/components/ui/icon.vue` | SVG icon wrapper for static art |
| `UiSectionHeading` | `src/components/ui/section-heading.vue` | Accessible heading with level + variant |
| `BrandIcon` | `src/components/ui/brand-icon.vue` | Inline SVG primitive for brand logos. Reads `src/assets/brands/*.svg` via `import.meta.glob({ as: 'raw' })`. |

### 3.8 Landing Redesign — widgets and sections
**Created:** 2026-05-07 | **Last updated:** 2026-05-08
**Status:** Production. 7+ polish rounds applied.
**Branch:** `vue-migration` (working branch; user handles git).

#### Composition tree
```
App.vue (single-column landing root)
├── HudNav (sticky, scroll-progress + IntersectionObserver active link, lang toggle, hamburger)
└── <main class="landing">
    ├── HeroSection            #hero    natural height, recruiter-grade intro, HUD ornaments
    ├── SkillsSection          #skills  3 categories (Frontend/Backend/DevOps), 22 tech cards
    ├── ExperienceSection      #experience  vertical HUD timeline, 5 cards, primary-tone first node
    └── NowProjectsSection     #projects  ≤6 main cards (live countdowns + STARTED IN for WORKING_ON) + ≤9 featured cards
└── SiteFooter                 #contact  brand+manifest, contact channels, socials grid
```

#### 3.8.1 `HudNav` (`@widgets/hud-nav.vue`)
**Role:** Sticky futuristic navigation rail. **Parent:** `App.vue`.
**Store:** None — internal `ref` state for scroll position, mobile menu, active section.
**Key methods:** `onScroll` (passive — also forces `active='hero'` when `scrollY < 80`), `IntersectionObserver` callback for section tracking (rootMargin `-45% 0px -45% 0px`).
**Layout:** Mobile bar `grid-template-columns: auto 1fr auto; gap: 0; padding: 0.6rem 1rem` (symmetric — gap was the asymmetry source). Brand `margin-left: 0.5rem`. Hamburger 44×44, `size="md"`, with own border. Lg+ bar restores `gap: 1.5rem; padding: 0.75rem 2rem`.
**Glyphs:** menu, close (Nerd Font). Brand glyph removed.

#### 3.8.2 `HeroSection` (`@sections/hero.vue`)
**Role:** Above-the-fold recruiter intro. **Parent:** `App.vue`.
**Imports:** `BrandIcon` (NEW for ORCID), `UiImage`, `UiLink`, `PROJECTS`, `TECHNOLOGIES`, both CV PDFs as `?url`.
**Key computed:** `cv_href` / `cv_filename` / `cv_label` (locale-aware), `active_projects` (count where `!featured`), `stack_count` (= 22 now), `years_suffix` (`YEARS` / `AÑOS` based on locale).
**Tag row (`__tag-row`):** flex container hosting two clickable pills:
  - `__tag` (CCS MEMBER): `<a href="https://github.com/ccs-devhub">` — neutral border + dark transparent bg, no visual hover, includes `.state-grid` loader + the i18n tag string with embedded `<span class="ccs-glyph">▣</span>`.
  - `__orcid`: `<a href="https://orcid.org/0009-0006-4459-5538">` — same height/padding/font as CCS, ORCID brand colors at ~55% opacity, no visual hover, BrandIcon orcid + "ORCID" label.
**Title:** `__name` (CRISTIAN D. MORENO) + `__alias` (A.K.A. KYONAX京 — bigger relative font on mobile via `font-size: 0.5em` at max-md).
**Stats grid (4):** EXPERIENCE OF (8 + small `YEARS`/`AÑOS` suffix), TECH SKILLS (`stack_count`), PROJECTS (active count), languages.
**Meta row:** LOCATION + value (UBICACION es), AVAILABLE FOR REMOTE WORK / DISPONIBLE PARA TRABAJO REMOTO. fs-100 mobile / fs-200 md+.
**Visual:** portrait `<UiImage>` aspect 3/4 desktop, overridden to 1/1 on max-md (`max-width: 320px`); HUD frame; scan-line via `__visual-frame-inner` (gradient 6%→28%→6%, 0.55 opacity, `animation: hero-scan 12s linear infinite` — slowed from 6s for less twitch). Visual-meta below: `FRAME // ▣-001` + `@KYONAX_ON_TECH`.
**CTAs:** primary CV download (clip-path angular cyber frame, no flare); secondary Contact (corner-grow-into-full-border animation).
**Decorations:** TR `// HANDSHAKE :: VERIFIED`, BL `// VECTOR :: KYO-001` (both pulled flush to edges on max-md). No watermark.
**Padding:** mobile `3rem 1.5rem 2.5rem`, md `10rem 2rem 3rem`. Image-first via `order: -1` on `.hero__visual` at max-md.
**Scroll-hint:** hidden on mobile, inline below content on desktop with `margin-top: 3.5rem`.

#### 3.8.3 `SkillsSection` (`@sections/skills.vue`)
**Role:** Categorized stack/tools showcase. **Parent:** `App.vue`.
**Imports:** `BrandIcon`, `TECHNOLOGIES`.
**Categories static map:** `frontend` (9 ids: html, css, scss, js, ts, react, next, vue, vite) / `backend` (8: node, express, nest, php, symfony, python, postgresql, mongodb) / `devops` (5: docker, aws, git, githubactions, jest).
**`BRAND_ICON_IDS` Set:** css, ts, next, vue, jest, node, express, symfony, vite, nest, postgresql, mongodb, githubactions (13 entries — orcid not here; consumed only by hero).
**Item rendering tiers:** BrandIcon → Nerd Font glyph → 2-letter abbreviation fallback.
**Layout:** 3-column category grid (md: 2-col; sm: 1-col); each category has 2-3 col tech grid.
**Flare:** 0.08 rest, 0.12 hover (substantially softened).
**Decorations:** TR `// SYNC :: 22 NODES`, BL `// デベロッパー`, watermark `開発者`.

#### 3.8.4 `ExperienceSection` (`@sections/experience.vue`)
**Role:** Vertical HUD timeline of professional roles. **Parent:** `App.vue`.
**Driver:** static `ENTRIES` array of 5 ids (`zeronet` is `tone: 'primary'`, others `neutral`).
**Layout:** rail (32-48px wide) with dot + line, then `<article class="card element-flare">` per entry. Primary-tone first card has glowing dot + gradient bg.
**Role title:** `font-size: var(--fs-500); font-weight: 700` (bumped from fs-400 so the title is unambiguously larger than the SpaceMono fs-200 specs row below it).
**Description:** Geomanist fs-300, line-height 1.65, letter-spacing 0.018em. Bold spans use `var(--clr-neutral-50)` (white) — was previously primary-yellow.
**Subtitle:** fs-400 + line-height 1.6 + letter/word spacing.
**Flare:** opacity 0.18.
**Decorations:** TR `// LOG :: VERIFIED`, BL `// 進化`, watermark `過去`.

#### 3.8.5 `NowProjectsSection` (`@sections/now-projects-section.vue`)
**Role:** Project cards driven by the state model.
**Imports:** `PROJECTS`, `PROJECT_STATUS`, `NOW_STATUS_PRIORITY`, `useProjectCountdowns`.
**State (script):** Reactive `_now_ms` ref + `setInterval(1000)` for the WORKING_ON count-up timers. `_format_deadline()` uses `Intl.DateTimeFormat` (timeZone 'America/Bogota', locale-aware, `.toUpperCase()`). `_format_elapsed_segments()` produces `[Xd, NNh, NNm, NNs]` from `_now_ms - started_ms`.
**Behavior:** sorts main_cards by `NOW_STATUS_PRIORITY` then deadline, slices NOW_MAX=6. Featured slice 0..9, no countdown.
**Card root:** `<component :is="card.has_link ? 'a' : 'div'">` — polymorphic. `is-static` class on URL-less branch.
**Card structure (NowShipping):**
  - Header: status badge (state-grid + label, colored via `--state-color`) + `#NN` index.
  - Name + version chip (uniform primary, NOT state color).
  - Milestone line: `// {{ description || deadline_label }}.toUpperCase()`.
  - Countdown / count-up block (3 rows: head with date, segments, timezone footnote).
  - Ended-state chip if expired.
  - Link footer (VIEW REPO + github + smaller external-link icon) OR `// ENDPOINT :: CLASSIFIED` for URL-less.
**Card structure (Featured):** Header (state-square + label, link-glyph or no-link tag) → name + version chip (state-color).
**Glyphs:** github, external link, check-circle (ended), star (featured label).
**Decorations:** TR `// PIPELINE :: OPEN`, BL `// 未来`, watermark `未来`.

#### 3.8.6 `SiteFooter` (`@sections/site-footer.vue`)
**Role:** Bottom-of-page contact + signature. **Parent:** `App.vue`.
**Script (NEW 2026-05-08):** Reactive refs `host`, `path`, `nav_language`, `viewport`. Hydrated in `onMounted` (window.location, navigator.language, viewport read; resize listener attached). Cleanup in `onBeforeUnmount`. `resolved_tz` evaluated at module load via Intl. `manifest` computed array of {key, label, value}.
**Imports:** `BrandIcon`, `UiIcon`, `UiLink`.
**Composition:** `__top` = grid (`1fr 1fr`, brand spans both columns via `grid-column: 1 / -1`) on every viewport; column gap widens at md+. Brand block stacks logo (full-width, max-width 480 desktop / none mobile) + signoff card (full-width inside brand block). Channels + socials side-by-side row 2.
**Signoff card:** dashed border + dark transparent bg. Hosts:
  - `SYS // SIGNATURE` tag (small primary).
  - Prose tagline (v-html, fs-200, opacity 0.75) ending `MADE WITH L♥VE.` (♥ in `.heart-glyph`).
  - `<dl class="__manifest">` 6-row grid (HOST/PATH/LOCALE/LANG/VIEWPORT/TZ), opacity 0.55, fs-100, auto-fit columns.
**Logo:** SVG recolored via filter chain → primary-yellow.
**Divider:** centered "END OF TRANSMISSION" tag on a primary gradient line; `margin-top: 14rem` (mobile) / `16rem` (md).
**Bottom row:** `margin-top: 3rem` mobile / `4rem` md from divider; copyright + DESIGNED BY split horizontally.
**Decorations:** TL `// BEACON :: ON`, TR `// CHANNEL :: CCS // KYONAX // ZERONET` (md+ only).
**Social-icon override:** `transform: translateY(0)` cancels global `.icon-glyph` lift inside the 44×44 grid cells.

#### 3.8.7 Legacy files — DELETED 2026-05-08 (Phase 8 batch 1)
All entries that previously occupied this slot have been removed from disk: `banner.vue`, `persistent-data.vue`, `content-data.vue`, `widgets/now-projects.vue`, `vimeo-video.vue`, `tech-stack.vue`, `_persistent-data.scss`, `_content-data.scss`, `_marquee.scss`, plus the empty `layout/` and `components/` SCSS directories. `src/views/components/sections/` now contains exactly the 5 active landing sections; `src/components/` contains only `blast-image.vue` + the 7 UI primitives in `ui/`; `src/scss/` contains only `abstracts/` + `base/`.

### 3.9 Composables (`src/composables/`)
| File | Role |
|---|---|
| `use-language.js` | Locale state + URL + localStorage + navigator fallback chain |
| `use-click-outside.js` | Click-outside detector |
| `use-seo-head.js` | Unhead meta tag binding (called once in `App.vue`) |
| `use-project-countdowns.js` | Wraps `now-project.worker` lifecycle + visibilitychange pause/resume |
| `use-scrolled-class.js` | IntersectionObserver replacement for inline scroll listeners |
| `use-image-manifest.js` | Build-time manifest helper |

### 3.10 Brand SVG library (`src/assets/brands/`) — 16 SVGs as of 2026-05-08
All `viewBox="0 0 24 24"` square, fills `currentColor`. Pulled verbatim from Simple Icons except the original 5 hand-authored marks (since refreshed).

| File | Subject | Source |
|---|---|---|
| `x.svg` | X (post-Twitter rebrand) | Hand-authored angular X |
| `next.svg` | Next.js | Simple Icons (refreshed 2026-05-08) |
| `vue.svg` | Vue.js | Hand-authored two-tone triangle |
| `jest.svg` | Jest | Simple Icons (refreshed 2026-05-08) |
| `tiktok.svg` | TikTok | Hand-authored |
| `css.svg` | CSS3 | Simple Icons |
| `node.svg` | Node.js | Simple Icons |
| `express.svg` | Express.js | Simple Icons (with `fill-rule="evenodd"` on path) |
| `symfony.svg` | Symfony | Simple Icons |
| `vite.svg` | Vite | Simple Icons (NEW 2026-05-08) |
| `nest.svg` | Nest.js | Simple Icons (NEW 2026-05-08) |
| `postgresql.svg` | PostgreSQL | Simple Icons (NEW 2026-05-08) |
| `mongodb.svg` | MongoDB | Simple Icons (NEW 2026-05-08) |
| `githubactions.svg` | GitHub Actions | Simple Icons (NEW 2026-05-08) |
| `ts.svg` | TypeScript | Simple Icons (NEW 2026-05-08, renamed from typescript.svg to match `'ts'` tech id) |
| `orcid.svg` | ORCID | Simple Icons (NEW 2026-05-08, hero badge only) |

### 3.11 Project state model (`src/data/projects.js`)
**`PROJECT_STATUS`** map — 9 entries, each `{ color, labelKey }`:
- NowShipping: WORKING_ON (accent), DONE (success), IN_PROGRESS (primary), ON_HOLD (warning), ON_TODO (secondary).
- Featured: LIVE (success), DEPRECATED (error), UPDATING (primary), RELEASE (secondary).
**`NOW_STATUS_PRIORITY`** map — sort priority 0..4, WORKING_ON first.
**Sample entries:**
- `agile-engine` (WORKING_ON): name AGILE ENGINE, description CLIENT MADISON REED, version REMOTE, started Nov 03 09:00:00 2024, no URL, featured: false.
- `sofia-married` (IN_PROGRESS): version v0.4.0, deadline Aug 30 2026.
- `veyra-organization` (ON_TODO): version v0.1.0, deadline Jul 20 2026.
- `zeronet-labs` (ON_HOLD): version v0.2.0, deadline Sep 15 2026.
- `zeronet-platform` (UPDATING, featured): version v0.4.0.
- `veyra-project` (RELEASE, featured): version v1.0.0.
- `cyber-code-syndicate` (LIVE, featured): version v0.3.0.

### 3.12 `kyo-web-online-old/` (reference repo)
**Status:** Read-only reference. Pre-migration mirror.
**Path:** `/home/kyonax/Documents/github-kyonax/kyo-web-online-old/`

### 3.13 `reckit/` (canonical pattern reference)
**Status:** Read-only reference.
**Path:** `/home/kyonax/Documents/github-kyonax/reckit/`

Read on first session resume: `eslint.config.mjs`, `vite.config.js`, `src/app/scss/abstracts/_variables.scss`, `src/app/scss/abstracts/_theme.scss`, `src/views/home.vue`, `src/shared/composables/use-recording-status.js`, `src/brands/kyonax-on-tech/sources/hud/cam-person.vue`. Roam-node companions: `~/.brain.d/roam-nodes/reckit/{2026-04-17-reckit_architecture,2026-04-20-reckit_naming_conventions}.org`.

---

## SECTION 4: FILE INDEX

> Every relevant path. Use this when you need to read or edit a file without searching.

### 4.1 Plan documents
| File | Association |
|---|---|
| `VUE_MIGRATION_PLAN.md` | top-level plan |
| `TRANSLATION_MIGRATION.md` | Phase 3 deep dive |
| `PERFORMANCE_MIGRATION.md` | perf deep dive |
| `SASS_THEMING_MIGRATION.md` | SCSS theming deep dive |
| `CODE_STANDARDS_MIGRATION.md` | lint + naming deep dive |
| `SCRIPTS_AUTOMATION.md` | scripts catalogue |

### 4.2 Scripts (`scripts/`) — slimmed 2026-05-08
| File | Purpose |
|---|---|
| `_lib.mjs` | shared helpers |
| `check-i18n.mjs` | locale parity gate |
| `check-i18n-keys.mjs` | template `t()` keys gate |
| `check-trans-attrs.mjs` | banned-reference gate |
| `check-color-usage.mjs` | 60/30/10 audit + literal ban (scans `<style>` AND comments) |
| `check-aliases.mjs` | Vite ↔ ESLint alias sync |
| `check-license-headers.mjs` | CCS preamble gate |
| `precheck.mjs` | composite gate |
| `convert-fonts.sh` | Phase 2 TTF→WOFF2 |
| `convert-images.mjs` | image WebP (q=90) + AVIF (q=75) generator (predev/prebuild) |

### 4.3 Source — config and root
| File | Role |
|---|---|
| `vite.config.js` | aliases + SCSS additionalData + `transformIndexHtml` LCP preload plugin |
| `eslint.config.mjs` | flat config (CCS + Vue rules) |
| `index.html` | root template |
| `src/main.js` | entrypoint |
| `src/App.vue` | landing root (HudNav + 4 sections + SiteFooter) |
| `src/config/features.js` | feature flags (`vimeo.enabled = false`) |
| `package.json` | predev/prebuild → `convert-images.mjs` |

### 4.4 Source — SCSS (post-cleanup 2026-05-08)
| File | Role |
|---|---|
| `src/scss/main.scss` | entry; only `@use` lines: `abstracts/variables`, `abstracts/mixins`, `abstracts/theme`, `base` |
| `src/scss/abstracts/_index.scss` | forwarded everywhere via `additionalData` (variables + mixins; theme not forwarded) |
| `src/scss/abstracts/_variables.scss` | colors (8 families incl. accent), breakpoints, typography map (small tier bumped) |
| `src/scss/abstracts/_mixins.scss` | parameterized `font-face($range)`, `cyberpunk-glow`, `min/max-media-query` |
| `src/scss/abstracts/_theme.scss` | `:root` token emit (incl. `--clr-orcid-bg/fg`) + shared `@keyframes` + `.element-flare` + `.icon-glyph` (translateY -0.18em) + `.ccs-glyph` (1.75em + -0.08em) + `.state-grid` (3×3 grid + state-grid-pulse keyframe) + `.hud-deco` (corners + watermark) + selection styles |
| `src/scss/base/_typography.scss` | font-face declarations + body baseline (`line-height: 1.6` unitless) |
| `src/scss/base/_global.scss` | global `:focus-visible` + `.sr-only` + `html { scroll-behavior: smooth; scroll-padding-top: 4.5rem }` + `prefers-reduced-motion` rule |

### 4.5 Source — Landing
| File | Role |
|---|---|
| `src/widgets/hud-nav.vue` | sticky nav with scroll-progress + active link (scaleX underline) + mobile drawer |
| `src/widgets/language-toggle.vue` | ADA-compliant locale dropdown (role=menu / menuitemradio); 44px height on max-md |
| `src/views/components/sections/hero.vue` | recruiter-grade hero, tag-row (CCS+ORCID), HUD ornaments, scan-line clipped to portrait |
| `src/views/components/sections/skills.vue` | categorized tech showcase with BrandIcon dispatch (13 brand IDs) |
| `src/views/components/sections/experience.vue` | vertical HUD timeline (5 entries); role title fs-500/700 |
| `src/views/components/sections/now-projects-section.vue` | state-model-driven cards; STARTED IN count-up; polymorphic root; date+TZ countdown |
| `src/views/components/sections/site-footer.vue` | brand + dynamic SYS // SIGNATURE manifest + heart-glyph signoff + contact + socials |

### 4.6 Source — UI primitives + composables (post-cleanup 2026-05-08)
| File | Role |
|---|---|
| `src/components/ui/{card,link,button,image,icon,section-heading,brand-icon}.vue` | UI primitives (7 total). `link` + `button` carry `cyber` + `cyber-outline` variants |
| `src/components/blast-image.vue` | image element (wrapped by `UiImage`) |
| `src/composables/{use-language,use-click-outside,use-seo-head,use-project-countdowns,use-scrolled-class,use-image-manifest}.js` | composables |
| `src/i18n/{index,messages,detect-locale,raw-html-keys}.js` | vue-i18n setup; `raw-html-keys.js` (32 lines, trimmed) covers about-me.description, 5×3 experience cards, `landing.{nav.logo, hero.tag, hero.summary, footer.signoff}` |
| `src/data/{projects,snippets,data,error}.js` | translation source (245-line `snippets.js`) + project list + state model + 22 TECHNOLOGIES |
| `src/workers/now-project.worker.js` | countdown worker (1 Hz tick) |
| `src/config/features.js` | feature flags |

> Vimeo facade (`vimeo-video.vue`), `tech-stack.vue`, and `now-projects.vue` were deleted 2026-05-08. The `vimeo.enabled` flag still exists for when a future video is recorded — at that point a fresh facade can be authored or pulled from the old repo.

### 4.7 Source — assets
| File | Role |
|---|---|
| `src/assets/app/kyonax_multiverse_characters{,-100,-300,-600,-900}.{jpg,webp,avif}` | Hero portrait variants (re-encoded 2026-05-08 from IMG_6550) |
| `src/assets/brands/{x,next,vue,jest,tiktok,css,node,express,symfony,vite,nest,postgresql,mongodb,githubactions,ts,orcid}.svg` | 16 brand SVGs (Simple Icons-derived) |

### 4.8 CI workflows (`.github/workflows/`)
| File | Role |
|---|---|
| `ci.yml` | 4 jobs (eslint / precheck / vitest / build) on PR + push to main/develop/vue-migration. `size-limit` + Lighthouse jobs still pending. |
| `deploy-to-build-dev.yml` | dev deploy |
| `deploy-to-build-main.yml` | main deploy |

### 4.9 Reference repositories + roam nodes
| Path | Role |
|---|---|
| `/home/kyonax/Documents/github-kyonax/kyo-web-online-old/` | pre-migration mirror (read-only) |
| `/home/kyonax/Documents/github-kyonax/reckit/` | canonical pattern reference (read-only) |
| `~/.brain.d/roam-nodes/2026-05-05-index_kyo_web_online.org` | this project's index dashboard |
| `~/.brain.d/roam-nodes/kyo_web_online/2026-05-05-vue_migration_plan.org` | this project's detailed migration node |
| `~/.brain.d/roam-nodes/reckit/{2026-04-17-reckit_architecture,2026-04-20-reckit_naming_conventions}.org` | reckit cross-references |

### 4.10 Auto-memory
| Path | Role |
|---|---|
| `/home/kyonax/.claude/projects/-home-kyonax-Documents-github-kyonax-kyo-web-online/memory/MEMORY.md` | memory index |
| `…/memory/feedback_no_git_commands.md` | hard rule: never run git commands |

### 4.11 This session file
*   `/home/kyonax/Documents/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/kyo-web-online.md`

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last (2026-05-08, Phase 8 dead-code sweep + CTA abstraction)

The conversation rolled through three ordered passes after the marathon-day Sass cleanup:

1. **Comment sweep across SCSS + Vue files.** Removed status / update / "what does this do" commentary that didn't carry hidden information. Preserved the CCS license preambles (gated by `check-license-headers.mjs`) and a small set of comments documenting non-obvious *why*. Two passes were needed — the first was too gentle, the second cleaned the rest after the user pointed out the surplus.

2. **CTA abstraction into UiLink + UiButton variants** (the user asked for an abstraction audit and called out the two hero CTAs by name as candidates):
   *   Added `cyber` variant to UiLink + UiButton — angular clip-path frame, SpaceMono uppercase, 2px lift on hover, no flare.
   *   Added `cyber-outline` variant to UiLink + UiButton — corner-grow-into-full-border animation via TL+BR pseudos animating `width` then `height` to `100%+2px`.
   *   Validators extended: UiLink `['primary','secondary','ghost','card','cyber','cyber-outline']`; UiButton same set minus `card`.
   *   Hero rewritten to `<UiLink variant="cyber" ...>` + `<UiLink variant="cyber-outline" ...>`. The bespoke `__cta-primary` / `__cta-secondary` SCSS blocks, the shared `.hero__cta` class, and the dead `&__watermark` rule were deleted from `hero.vue`.

3. **Phase 8 cleanup batch 1 — dead-code audit.** Two phases of deletions:
   *   **Phase A (SFCs + SCSS + scripts):** 6 dead SFCs (`banner.vue`, `persistent-data.vue`, `content-data.vue`, `widgets/now-projects.vue`, `vimeo-video.vue`, `tech-stack.vue`); 3 dead SCSS partials (`_persistent-data.scss`, `_content-data.scss`, `_marquee.scss`) + their parent `_index.scss` files + the now-empty `layout/` and `components/` SCSS directories; 4 one-shot migration scripts (`migrate-snippets-to-esm.mjs`, `migrate-trans-attrs.mjs`, `scaffold-sfc.mjs`, `audit-baseline.mjs`). `package.json` slimmed: dropped `migrate:*` / `scaffold:sfc` / `audit:baseline`; replaced the (no-longer-needed) `vite-imagetools` transitive dep with direct `sharp ^0.34.5`. `main.scss` simplified to 4 `@use` lines.
   *   **Phase B (i18n pruning to live keys only):** `src/data/snippets.js` 382 → 245 lines. `src/i18n/raw-html-keys.js` 49 → 32 lines. Only keys that are actually rendered by the live SFCs survive: `contact.{contact-me,wsp}`, `content-data.about-me.description`, `content-data.download.{cv-en,cv-es}`, `content-data.experience.<5-ids>.{role,specs,description,tools}`, `persistent-data.name`, `widget.trans-lang.{current,en,es}`, and the full `landing.*` block.

### Validation
*   `node scripts/precheck.mjs` — ✓ all 6 (i18n, i18n-keys, trans, color, aliases, licenses).
*   `npm run build` — ✓ 4.40s, 236.20 kB JS / 55.71 kB CSS. Clean output, no Sass deprecation warnings.
*   `npm test` — no test files (vitest exits 1 with "No test files found"; this is the existing baseline, not a regression).

### What was done in the prior round (2026-05-09 Sass mixed-decls cleanup, then 2026-05-08 marathon polish day)

The conversation that day was a 30+ round refinement marathon across the entire landing. Major outputs (newest-first):

*   **ORCID badge — final design.** Sibling pill to CCS MEMBER (in `.hero__tag-row`). Square corners, identical SpaceMono / fs-200 / 0.4rem 0.8rem padding to CCS. ORCID brand colors (text + 1px border) faded to `color-mix(...orcid-bg... 55%, transparent)` so the badge sits at the same visual weight as the neutral CCS pill beside it. Transparent dark bg (matches CCS). **No-visual-hover pattern** — every state pinned to resting; only `cursor: pointer` indicates clickability. Icon `transform: translateY(0.06em)` (slightly down). Label `transform: none` (was `translateY(-0.05em)`). Brand colors live in `_theme.scss` as `--clr-orcid-bg` / `--clr-orcid-fg`.
*   **CCS signature `▣` finalized + applied.** `kyo-web.landing.hero.tag` (en+es) and `FRAME // ▣-001` visual-meta both wrap `▣` in `<span class="ccs-glyph">`. Tag key added to RAW_HTML_KEYS. `.ccs-glyph` utility in `_theme.scss` (1.75em / -0.08em). The full 36-glyph option table that produced this decision is preserved in §2.5 of the previous reset for future-self reference (now archived in this section).
*   **CCS MEMBER tag is clickable.** `<a href="https://github.com/ccs-devhub" target="_blank">`. No visual hover changes — only cursor.
*   **Hero copy final form.** Lead key deleted; merged into summary. Summary opens "8 years of experience delivering **scalable, adaptable, and high-performance** web solutions for national and international clients..." Closes "...migrations that cut technical debt." (Drop-in Spanish equivalent.) **Stat labels rewritten:** EXPERIENCE OF / EXPERIENCIA DE; TECH SKILLS (both); PROJECTS / PROYECTOS. Years-value gets `<span class="hero__stat-suffix">YEARS|AÑOS</span>` smaller superscript-ish suffix. **Tilde scrub for SpaceMono Spanish strings**: MENU / UBICACION / BOGOTA / INICIO EN / CONTACTAME.
*   **Visual-meta line below image** changed `EXP. 8 YEARS | 2018 - 2026` → `@KYONAX_ON_TECH` (Twitter handle, not clickable; the years stat already shows the experience count).
*   **`A.K.A. KYONAX京` alias** scaled relative to title; bigger on mobile (`font-size: 0.5em` at max-md).
*   **Hero CTAs redesigned.** Primary CV download: angular cyber clip-path (top-right + bottom-left corner cuts), no flare/sweep. Secondary Contact: corner-grow-into-full-border animation (TL + BR pseudos, animate width then height with stagger, meet at center → full primary border). Hover fades text to primary on Contact.
*   **Yellow corners removed from hero.** All four `__corner` pseudo-elements deleted along with their SCSS block.
*   **Stronger then softer scan-line.** Final form: 5-stop gradient (transparent → 6% → 28% → 6% → transparent), opacity 0.55, `animation: hero-scan 12s linear infinite` (slowed from 6s).
*   **Image swap.** New IMG_6550.png portrait → `kyonax_multiverse_characters` JPG variants (5 sizes) regenerated. WEBP/AVIF re-encoded at higher quality (`WEBP_QUALITY 75 → 90`, `AVIF_QUALITY 50 → 75`). Source PNG removed from Downloads (note in §2.4: re-import IMG_6550.png if best quality is needed since the existing JPG was once-encoded q=82).
*   **Hero portrait aspect-ratio fix.** Mobile/tablet: `display: block` on `.hero__visual-frame` (was flex — `display: flex` made `.ui-image` collapse to min-content via circular percent-width); added `:deep(.ui-image) { width: 100% }`; `aspect-ratio: 1/1 !important` overrides UiImage's inline 3/4; max-width 320px.
*   **Tablet hero matches mobile.** `order: -1` on `.hero__visual` extended from max-sm to max-md so image sits above text on both mobile AND tablet single-column flow.
*   **Mobile/tablet typography overhaul.** Small tier bumped substantially: `--fs-100: 0.625 → 0.95rem`, `--fs-200: 0.875 → 1.05rem`, `--fs-300: 1 → 1.15rem`, `--fs-400: 1.125 → 1.25rem`, `--fs-500: 1.5 → 1.625rem`, `--fs-600: 1.875 → 2rem`, `--fs-700: 2.25 → 2.375rem`, `--fs-800: 3 → 3.125rem`. Body `line-height` switched to unitless `1.6` so it scales with descendants.
*   **Section subtitles bumped.** All three section subtitles → `var(--fs-400)`, line-height 1.6, letter/word-spacing.
*   **Mobile bar fix.** Final form: `gap: 0` (the asymmetry source), symmetric `padding: 0.6rem 1rem`, brand `margin-left: 0.5rem`. Hamburger 44×44 with own border on max-md.
*   **Mobile hamburger drawer.** Full-width with backdrop blur matching scrolled bar; rows trim right padding so they don't feel like they have extra space at the right edge.
*   **HOME active when at top.** `onScroll` forces `active = 'hero'` when `scrollY < 80`. IntersectionObserver `rootMargin` widened from `-40%/-55%` to `-45%/-45%` (10% trigger band, more reliable).
*   **Active-link underline.** `transform: scaleX(0/0.55/1)` on `::after`, GPU-composited. Left-anchored. Mobile drawer hides this and uses bg tint instead.
*   **Footer two-column on mobile + matching on desktop.** `__top` grid `1fr 1fr` always; brand block spans full width via `grid-column: 1 / -1`; channels + socials side-by-side row 2.
*   **Footer brand stack.** Logo full-width row (max-width 480px desktop, none mobile) → signoff card full-width row.
*   **Footer SYS // SIGNATURE manifest.** Replaced static prose with `<dl>`-rendered manifest of runtime browser data. 6 fields: HOST, PATH, LOCALE, LANG, VIEWPORT, TZ. Above the manifest: prose tagline ending `MADE WITH L♥VE.` (♥ in `.heart-glyph`, primary color, x-height aligned). Same English close-tag in both locales.
*   **Footer END OF TRANSMISSION + bottom row.** Divider `margin-top: 14rem` mobile / `16rem` md. Bottom row (© + DESIGNED BY) `margin-top: 3rem` mobile / `4rem` md from divider.
*   **Cyberpunk HUD decorations.** Global `.hud-deco` utility in `_theme.scss`. Per-section corner labels + giant kanji watermarks (NO watermark in hero per user request). Opacity 0.32 corners / 0.04 watermarks. Final corner content: hero HANDSHAKE/VECTOR; skills SYNC/デベロッパー (WM 開発者); experience LOG/進化 (WM 過去); projects PIPELINE/未来 (WM 未来); footer BEACON/CHANNEL.
*   **Project state model rewrite.** `PROJECT_STATUS` (9 states), `NOW_STATUS_PRIORITY` sort, NOW_MAX=6 / FEATURED_MAX=9, version chip on every card, polymorphic URL-less root with `// ENDPOINT :: CLASSIFIED` alt-text. State color narrowly scoped (status badge only on NowShipping; square+label+version on Featured). New `accent` magenta color family for WORKING_ON.
*   **STARTED IN count-up timer for WORKING_ON.** Same UI shape as countdown. Local 1Hz tick + elapsed segments. AGILE ENGINE entry: `description: 'CLIENT MADISON REED'` (overrides deadline label), `version: 'REMOTE'` (modality), started Nov 03 2024.
*   **Card numbers prefixed with `#` (NowShipping).** Card date in countdown uppercased (`MAR 3, 2025, 9:00 AM` style) and styled identical to label.
*   **External-link icon shrunk** (`--icon-glyph-size: 0.85em` in NowShipping link footer).
*   **TECHNOLOGIES expanded 17 → 22.** Added vite, nest, postgresql, mongodb, githubactions. Categories rebalanced (Frontend 9, Backend 8, DevOps 5).
*   **Brand SVGs refreshed via Simple Icons.** Re-fetched next, express, jest, symfony with `fill-rule="evenodd"` on path. Added typescript (renamed `ts.svg`), vite, nest, postgresql, mongodb, githubactions, orcid. Symfony moved into BRAND_ICON_IDS.
*   **Performance audit (carry-forward from 2026-05-07).** Global `prefers-reduced-motion` rule active. State-grid loaders animate `opacity` only. `content-visibility: auto` reverted everywhere (clipped hover lifts due to implicit `contain: paint`).
*   **All gates green.** `node scripts/precheck.mjs` ✓ all 6 (i18n, i18n-keys, trans, color, aliases, licenses). `npm run build` ✓.

### Pending / Not yet started
*   **Phase 7 polish:** confirm `dist/assets/` ships every WebP+AVIF variant (they're now q=90/q=75); verify the `transformIndexHtml` LCP preload tag lands in built `index.html`; sweep non-LCP `<img>` for `loading="lazy"` + `decoding="async"`.
*   **Phase 8 — only remaining items:** wire `size-limit` and Lighthouse CI jobs into `.github/workflows/ci.yml` (current 4 jobs: eslint, precheck, vitest, build). Everything else from Phase 8's original scope is **done** — webpack files / cheerio / dead SFCs / dead SCSS / migration scripts / orphaned i18n keys / license headers all swept by 2026-05-08.
*   **Re-import IMG_6550.png:** drop the original PNG back into Downloads and re-run `node scripts/convert-images.mjs --force` for lossless-source variants of the portrait.
*   **Architecture extraction:** if BrandIcon strategy + glyph-encoding rule + ADA dropdown pattern + state-grid opacity-animation pattern + polymorphic-card-root pattern + dynamic-manifest pattern + UI-primitive `cyber`/`cyber-outline` variant pattern survive next iteration without changes, extract into a persistent architecture memory file.
*   **Smoke test on real devices:** new landing on iPhone Safari + Chrome Android (especially the mobile typography bump + footer manifest + ORCID badge).
*   **Pending small task #16:** user once said *"change name."* with no antecedent. Never clarified. Probably stale.

### Where to resume

**If the user says "continue Phase 7" or "polish perf":**
1. `npm run build` — confirm `dist/assets/` has both `.webp` and `.avif` for every source image.
2. Open `dist/index.html` — confirm the `<link rel="preload" as="image" imagesrcset>` tag for the LCP image is present.
3. Grep SFCs for `<img` without `loading=` — add `loading="lazy"` + `decoding="async"` where missing.

**If the user says "finish Phase 8" or "ready to ship":**
1. The dead-code sweep is already done (2026-05-08). What's left: add `size-limit` job + Lighthouse CI job to `.github/workflows/ci.yml` (current 4 jobs: eslint / precheck / vitest / build).
2. Run `node scripts/precheck.mjs` and confirm all six gates pass; run `npm run build` for a final clean bundle.
3. **The user commits everything themselves — never run git commands.**

**If the user wants to add a new CTA / button style:**
1. Check whether an existing `UiLink` / `UiButton` variant fits (`primary`, `secondary`, `ghost`, `card` (link only), `cyber`, `cyber-outline`). If yes, consume it via `<UiLink variant="..." ...>` — don't reach for bespoke SCSS.
2. If a new variant is genuinely needed, add it to BOTH `UiLink` + `UiButton` in tandem (the validators must mirror, minus `card` for button). Implement in the SCSS at the bottom of each primitive.
3. Update §1.13, §1.29, §3.7 of this session file with the new variant.

**If the user reports a layout, alignment, or icon bug on the landing:**
1. Identify the element — section-level (`#hero`, `#skills`, `#experience`, `#projects`, `#contact`) → check the matching SFC under `@sections/`. UI primitive → check `@ui/<primitive>.vue`. Nav → `@widgets/hud-nav.vue`.
2. If a glyph renders as tofu / wrong logo: check whether it's a Nerd Font or BrandIcon case (§1.14). Verify codepoints via the Python audit in §1.15. If a tech ID is missing from skills, check `BRAND_ICON_IDS` Set.
3. If flare halos look too strong: tier down `--element-flare-opacity` further (§1.16). Default 0.45, primary cards 0.12, skills items 0.08/0.12.
4. If mobile typography looks too small: check the small tier in `_variables.scss` (§1.5).
5. If language picker keyboard nav is broken: re-check `language-toggle.vue` keydown handlers and focus return (§1.25).

**If the user wants to add a new tech to the skills grid:**
1. Add entry to `TECHNOLOGIES` in `src/data/data.js` with `id`, `name.{en,es}`, `iconGlyph` (Nerd Font codepoint or empty if SVG).
2. Add the id to the appropriate category's `ids` array in `skills.vue`'s `CATEGORIES`.
3. If using SVG: drop `<id>.svg` into `src/assets/brands/`, add `id` to `BRAND_ICON_IDS` Set in skills.vue. Filename must match the tech id.
4. Run `node scripts/precheck.mjs` to verify nothing broke.

**If the user wants to add a new project:**
1. Add entry to `PROJECTS` in `src/data/projects.js` with required fields: `name`, `url` (or empty for url-less polymorphic), `featured` (bool), `status` (one of PROJECT_STATUS keys), `version`, plus `deadlines: { '<label>': 'Mon DD HH:MM:SS YYYY' }` for countdown OR `started: '...'` for WORKING_ON count-up. Optional: `description` to override the deadline-derived label.
2. State color is automatic via the status enum.
3. Card slot is automatic via NOW_MAX=6 / FEATURED_MAX=9 + sort priority.

**If the user asks "where are we?":**
- Read this section first, then §2.2 for phase status.

**If the user asks about a specific topic** (translation, performance, SCSS, code standards, scripts):
- Open the matching deep-dive document (§4.1).

**If the user wants to update memory** (`/memory`, "remember X"):
- Save it under `/home/kyonax/.claude/projects/-home-kyonax-Documents-github-kyonax-kyo-web-online/memory/`; cross-link from §1 if it's a session-scoped guideline.

**If the user wants a new task:** check Section 2.4 (Pending Work).

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session. Newest row first. See `~/.claude/skills/session-reset/rules/activity-log.md` for the schema.

| Datetime         | Duration | Type            | Reference | Description |
|------------------|----------|-----------------|-----------|-------------|
| 2026-05-08 17:30 | 0.5h     | session-reset   | this      | Compacted Phase 8 dead-code sweep + CTA abstraction; updated §1.5/1.13/1.27/1.29, §2.2/2.3/2.4, §3.6/3.7/3.8.7, §4.2/4.4/4.6/4.8, §5 |
| 2026-05-08 16:30 | 0.5h     | implementation  | this      | i18n pruning to live keys: snippets.js 382→245 lines (dropped widget.discount/component.marquee/persistent-data.user.*/content-data.{intro-title,todo-pr,feature,now,about-me.title,download.title,experience.label,tools-label×10}/legacy footer.*/legacy now-projects.*); raw-html-keys.js 49→32 lines |
| 2026-05-08 15:30 | 1h       | implementation  | this      | Phase 8 batch 1 dead-code sweep: 6 dead SFCs + 3 dead SCSS partials + their _index.scss + empty layout/components dirs + 4 migration scripts deleted; package.json slimmed (migrate:*/scaffold:sfc/audit:baseline removed); vite-imagetools→sharp ^0.34.5; main.scss simplified to 4 @use lines |
| 2026-05-08 14:30 | 0.5h     | implementation  | this      | CTA abstraction: added cyber + cyber-outline variants on UiLink + UiButton (clip-path angular frame; corner-grow-into-full-border animation); hero rewritten to consume variants; deleted bespoke __cta-primary/__cta-secondary blocks + .hero__cta shared class + dead &__watermark rule |
| 2026-05-08 13:30 | 0.5h     | refinement      | this      | Comment sweep across SCSS + Vue files (two passes); preserved CCS license preambles; removed status/update/what-it-does commentary; kept only comments documenting non-obvious why |
| 2026-05-09 00:30 | 0.25h    | session-reset   | this      | Compacted Sass mixed-decls cleanup; pattern documented in §1.5; build + dev silent now |
| 2026-05-09 00:00 | 0.25h    | bug-fix         | this      | Sass mixed-decls warnings cleared in 4 blocks (.hero root + .hero__scroll-hint + .site-footer root + .site-footer__logo); plain declarations reordered before nested rules; build clean in 10.17s, no warnings |
| 2026-05-08 23:30 | 0.5h     | session-reset   | this      | Compacted 30+ round 2026-05-08 polish marathon: ORCID badge final, CCS signature wired, projects state model + count-up timer, footer dynamic manifest, HUD decorations across all sections, mobile typography overhaul, 5 new tech SVGs + TS, image swap |
| 2026-05-08 22:30 | 1h       | refinement      | this      | ORCID badge final form: sibling to CCS MEMBER in tag-row; faded brand colors at 55%; no-visual-hover pattern; icon translateY(0.06em) |
| 2026-05-08 21:00 | 1.5h     | refinement      | this      | Cyberpunk HUD decorations across all sections (.hud-deco utility + corner labels + kanji watermarks; opacity 0.32/0.04); rejected hero kanji watermark per user feedback |
| 2026-05-08 19:30 | 0.75h    | refinement      | this      | Footer dynamic SYS // SIGNATURE manifest (HOST/PATH/LOCALE/LANG/VIEWPORT/TZ via window+navigator+Intl); prose tagline ending MADE WITH L♥VE. with .heart-glyph |
| 2026-05-08 18:00 | 1.5h     | refinement      | this      | Footer two-column on mobile + matching desktop; brand block stack (logo full-width / signoff full-width); END OF TRANSMISSION margin bumps |
| 2026-05-08 16:30 | 1h       | refinement      | this      | Mobile/tablet polish: nav bar gap=0, hero image circular-width fix, image aspect 1/1 max-md, scroll-hint hidden mobile, typography small-tier bumped, body line-height unitless 1.6 |
| 2026-05-08 15:00 | 1h       | refinement      | this      | Stat labels rewritten (EXPERIENCE OF / TECH SKILLS / PROJECTS); years suffix YEARS/AÑOS; tilde scrub MENU/UBICACION/BOGOTA/INICIO EN/CONTACTAME |
| 2026-05-08 13:30 | 1.5h     | implementation  | this      | Project state model: PROJECT_STATUS (9 states) + NOW_STATUS_PRIORITY + caps + per-card --state-color narrowly scoped + AGILE ENGINE WORKING_ON sample; STARTED IN count-up timer; polymorphic root for url-less cards |
| 2026-05-08 12:00 | 0.75h    | implementation  | this      | Brand SVGs refreshed (Simple Icons): next/express/jest/symfony with fill-rule on path; added vite/nest/postgresql/mongodb/githubactions/ts/orcid (5 new techs + TypeScript + ORCID) |
| 2026-05-08 11:00 | 0.5h     | refinement      | this      | Hero CTAs: primary clip-path angular frame (no flare), secondary corner-grow-into-full-border animation; yellow corners removed |
| 2026-05-08 10:00 | 0.75h    | refinement      | this      | Hero copy final: lead merged into summary; "8 years of experience delivering scalable, adaptable, high-performance web solutions..."; alias A.K.A. KYONAX京 sized; visual-meta @KYONAX_ON_TECH |
| 2026-05-08 09:00 | 1h       | implementation  | this      | Image swap IMG_6550.png → kyonax_multiverse_characters variants regen; WEBP_QUALITY 75→90, AVIF_QUALITY 50→75; CCS signature ▣ wired everywhere; CCS MEMBER tag clickable; .ccs-glyph utility |
| 2026-05-07 20:30 | 0.25h    | refinement      | this      | CCS signature finalized → ▣ (option A). Added .ccs-glyph utility in _theme.scss (font-size 1.45em → later 1.75em). hero.tag added to raw-html-keys |
| 2026-05-07 19:45 | 0.25h    | planning        | this      | Authored CCS-signature glyph option set (36 glyphs across 6 metaphor groups) |
| 2026-05-07 17:30 | 0.5h     | session-reset   | this      | Compacted Landing Redesign + 4 polish rounds; introduced Section 6 |
| 2026-05-07 16:00 | 1.5h     | refinement      | this      | Polish round 4: BrandIcon SVG primitive + 5 brand SVGs, scan-line clip, flare 0.45→0.12, glyph translateY -0.08em, lang chevron CSS-triangle |
| 2026-05-07 13:00 | 3h       | refinement      | this      | Polish round 3: ADA LanguageToggle (role=menu/menuitemradio), flare reduction, future deadlines on all 6 projects, footer logo recolor filter, 14 Devicon codepoints |
| 2026-05-07 12:15 | 0.25h    | refinement      | this      | Em-dash sweep: replaced "—" with commas in 6 visible translation strings (en+es) |
| 2026-05-07 12:00 | 0.5h     | refinement      | this      | Hero copy round 3: "Skilled on" wording, removed redundant title from lead, widened summary container 60ch→80ch |
| 2026-05-07 11:30 | 0.5h     | refinement      | this      | Hero copy round 2: kept Senior Full Stack title, restructured summary to 4-beat (current role / leadership / strengths / working style) |
| 2026-05-07 11:00 | 0.5h     | refinement      | this      | Hero copy round 1: 8 years, AgileEngine for Madison Reed, Zerønet Founder & Lead, profession + visual-meta tags |
| 2026-05-07 10:00 | 1.5h     | implementation  | this      | Built landing redesign: HudNav, Hero, Skills, Experience, NowProjects, SiteFooter; rewrote App.vue; hid Vimeo |
| 2026-05-07 09:00 | 1h       | research        | this      | Loaded session file + reckit_naming_conventions roam node; surveyed UI primitives, data files, theme tokens |
| 2026-05-07 09:00 | —        | other           | this      | Activity Log added on this reset; prior history not back-filled |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
