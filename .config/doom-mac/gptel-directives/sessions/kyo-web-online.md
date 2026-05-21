<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the **kyo-web-online Vue 3 migration + Landing Redesign** session. Load it at the start of every conversation to gain full context without re-discovering anything. Read sections in order on first load — after that, reference them by number as needed. The migration started as a multi-phase rewrite of the user's portfolio (`kyo-web-online`) from a custom Web Components + Webpack stack onto Vue 3 + Vite. As of 2026-05-08, **Phases 0–6 are COMPLETE**, **Phase 7 (perf polish) is in flight**, **Phase 8 (CI/cleanup) is mostly done — only size-limit + Lighthouse CI remain**, and the **Landing Redesign** has settled into its production form (HudNav → Hero → Skills → Experience → NowProjects → SiteFooter) after 7+ polish rounds. The hero CTAs are now built from UiLink/UiButton `cyber` + `cyber-outline` variants (no more bespoke hero rules). All dead SFCs, dead SCSS partials, dead i18n keys, and one-shot migration scripts were swept this round. Vimeo is feature-flagged off; CCS signature is `▣`; ORCID badge sits beside the CCS MEMBER tag; cyberpunk HUD decorations live in every section; the footer signature card renders runtime browser data.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | CCS standards, reckit-alignment, color rule, scripts-first, CSS API surfaces (icon-glyph, brand-icon, ccs-glyph, heart-glyph, hud-deco, state-grid, element-flare, viewport units), Nerd Font + SVG icon strategy, image pipeline, accessibility floor, single-page landing patterns, performance rules. | Before any task. Mandatory constraints. |
| **2. Session Overview** | Project scope, 8-phase plan + landing-redesign track with status, key decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-deliverable detail: 6 plan docs + 14 scripts + UI primitives + sections + composables + reference repos + landing widgets/sections + new state model + 16 brand SVGs + dynamic footer manifest. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference path table for every plan doc, script, primitive, section, brand SVG, and source file. | When reading, editing, or locating files. |
| **5. Last Interaction** | What was just completed (2026-05-21 OKLCH contrast fix + ADA review + `size-limit` gate wired); pending work with **console-error resolution as TOP priority**; entry point for resuming. | At conversation start. |
| **6. Activity Log** | Datetime-stamped table of every meaningful event in this session. | When you need exact "what was done when". |

**Path variables.** The user works across multiple devices, so absolute repo paths vary per machine. This file refers to repo roots via variables — resolve them on the current device by `pwd` from inside the relevant checkout:

| Variable | Meaning | How to resolve |
|---|---|---|
| `$REPO` | local checkout of `Kyonax/kyo-web-online` (the active project) | `pwd` from inside the project — observed paths so far: `/home/kyonax/Documents/github-kyonax/kyo-web-online/` and `/run/media/kyonax/Da_ Disk/dev/github-kyonax/kyo-web-online/` |
| `$REPO_OLD` | local checkout of the pre-migration mirror | sibling directory: `<parent-of-$REPO>/kyo-web-online-old/` |
| `$REPO_RECKIT` | local checkout of the canonical pattern reference | sibling directory: `<parent-of-$REPO>/reckit/` |

Home-relative paths (`~/.brain.d/`, `~/.config/`, `~/.claude/`) are device-stable and are written without a variable.

**Operational Rule:** When the user references a plan document by name (e.g. "the perf doc"), open it directly from §4. When they ask "where are we?", read §5 first, then check §2.2 for phase status. When they ask to run something, check the script catalogue (§3.6) — never invent a command. **Hard rule: NEVER run any git command** — the user handles git themselves (memory `feedback_no_git_commands.md`).

**Key principle:** Data may appear in multiple sections with different framing. §1 frames knowledge as a *rule to follow*; §2 as *context to understand*; §3 as an *implementation to reference*. Each section answers a different question about the same knowledge.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `session-reset` (this file), `mr-roam-node`, `reckit-roam-node`, `code-review`, `skill-architect`, `pr-scribe`. This section stores session-scoped patterns. The six plan documents (§3.1–3.6) carry the deeper rationale.

### 1.1 Reference repositories — read-only context
*   `$REPO_OLD/` is the **pre-migration mirror**. NEVER write to it. Use it to verify legacy behavior. Ideas flow old → new.
*   `$REPO_RECKIT/` is the **canonical pattern reference** — Vue 3 + Vite + same SCSS palette + reckit naming conventions (Rules A–J). Mirror eslint config, alias map, 7-1 SCSS layout. NEVER write to it.
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

### 1.34a Contrast verification gate (added 2026-05-21)
*   **Every palette change requires a contrast pass before merge.** The OKLCH conversion that landed in PR #134 (2026-05-20) silently shipped `--clr-neutral-300` at `oklch(55.2% 0.016 285.938)` — Tailwind zinc-500 lightness — yielding ~3.2:1 on `--clr-neutral-500` (dark bg), failing WCAG AA 4.5:1. Lighthouse flagged it on `.skills__category-count` + `.skills__item-name`; 14 other call sites would have failed too.
*   **Floor for OKLCH lightness on neutral-500 bg (`Y ≈ 0.014`):** `L ≥ 64%` for AA pass (~4.5:1); `L ≥ 70%` for comfortable AA margin (~4.9:1+). Current safe defaults after 2026-05-21 fix: `neutral-50` L=76% (~5.8:1), `neutral-200` L=78% (~6.0:1), `neutral-300` L=70% (~4.9:1).
*   **Opacity stacks on semantic text are forbidden.** `opacity: 0.55` on `.site-footer__manifest` reduced effective contrast on its neutral-200 values and neutral-300 keys to ~2.2:1 / ~3.0:1 even with the old palette. Decorative pseudos (`::before`, `::after`, `aria-hidden` elements rendered via CSS `content:`) ARE exempt from WCAG 1.4.3 and may stack opacity freely (`.hud-deco`, `.element-flare`, hero scan-lines).
*   **Verification recipe:** compute `relative_luminance(fg)` and `relative_luminance(bg)`, then `contrast = (lighter+0.05)/(darker+0.05)`. For OKLCH `L%`, sRGB→linear→Y is the path. Quick eyeball table (neutral chroma, bg L=14.5%): L=55% → 3.2:1 (fail), L=60% → 3.8:1 (fail), L=65% → 4.4:1 (border), L=70% → 4.9:1 (pass), L=76% → 5.8:1 (safe), L=80% → 6.7:1 (safe).

### 1.34b `<html lang>` sync — locale watcher in App.vue (added 2026-05-21)
*   `App.vue` owns a `watch(locale, ...)` on the `vue-i18n` locale that updates `document.documentElement.lang` with `{ immediate: true }`. SSR-safe via `typeof document !== 'undefined'` guard.
*   **Why a watcher (not setLanguage):** vite-ssg per-route prerender emits `<html lang>` correctly at SSG time, BUT CSR navigation between EN↔ES routes (via `router.push` from the toggle, OR via direct URL hits to `/es/`, OR browser back/forward) does NOT update the live DOM `lang` attribute. The watcher catches every locale-change path with one source of truth — coupling the fix to `setLanguage` would miss URL-driven changes.
*   The `immediate: true` flag also corrects any SSR-vs-CSR hydration drift on first load.

### 1.34c `:aria-label="x || y || undefined"` — fallback pattern (added 2026-05-21)
*   `aria-label=""` (empty string) **suppresses** the natural ARIA name resolution chain (the element appears unnamed to assistive tech). `aria-label="undefined"` (Vue binds `null`/`undefined` as "omit the attribute") **falls through** to `aria-labelledby` / native role naming / inner text.
*   Pattern: `:aria-label="explicitLabel || derivedLabel || undefined"`. Use anywhere a fallback label might be blank (modals, custom controls, polymorphic components).
*   **Wrong:** `:aria-label="ariaLabel || title"` — if both blank, renders `aria-label=""` and breaks accessibility.

### 1.34d Vue `warnHandler` filter — only suppress what's noise (added 2026-05-21)
*   `app.config.warnHandler` in `main.js` filters ONE specific message: `<Suspense> is an experimental feature`. Vue 3 emits it every time `<Suspense>` mounts; Suspense has been functionally stable since 2020 — the warning is a doc nag, not actionable signal.
*   Filter is text-specific (`msg.includes('<Suspense>') && msg.includes('experimental')`) so future Vue warnings still surface.
*   **Never blanket-suppress.** If a new Vue warning appears, investigate first; only add it to the filter after confirming it's noise.

### 1.34e Bundle-size budget gate — `size-limit` (added 2026-05-21)
*   `.size-limit.json` enumerates 7 gzipped budgets across the prerendered output: main JS (160 KB), main CSS (12 KB), now-projects chunk (10 KB), modal (4 KB), image-viewer (3 KB), youtube-facade (5 KB), FAQ (3 KB). Headroom ~15-30% above current baseline so routine growth doesn't flake the gate.
*   `npm run check:size` runs locally; CI job `Size Limit` runs after `build` (reuses the `dist-${{ sha }}` artifact) and is wired into the `pre-check-label` aggregator.
*   **When a budget overruns:** first re-check whether the regression is a real perf issue or a measurement noise (tree-shaking can drift across vite versions). If real, either trim the source OR bump the budget WITH a justification in the PR description — never bump silently.
*   DevDeps: `size-limit` + `@size-limit/file` (5 packages total).

### 1.34 Footer composition (final 2026-05-08)
*   `.site-footer__top` is a 2-column grid (`1fr 1fr`) on every viewport; column gap widens from `1.25rem → 3rem` at `min-md`. The brand block (logo + signature card) always spans both columns via `& > :first-child { grid-column: 1 / -1 }`. Channels + socials sit side-by-side underneath.
*   Brand block stacks vertically: `.site-footer__logo` (full-width, `max-width: 480px` desktop, none mobile) → `.site-footer__signoff` (also full-width).
*   Logo SVG recoloring: filter chain transforms black source → primary-yellow.
*   `// END OF TRANSMISSION` divider has `margin-top: 14rem` (mobile) / `16rem` (md) for breathing room. The bottom row (© + DESIGNED BY) has `margin-top: 3rem` (mobile) / `4rem` (md) clear of the divider.

---

## SECTION 2: SESSION OVERVIEW

> Project context, scope, and current phase status.

### 2.1 Purpose
Migrate the user's portfolio site at `$REPO/` from native Web Components + vanilla JS + Webpack 5 to **Vue 3 (`<script setup>`, Composition API) + Vite**, then redesign the layout as a futuristic single-page recruiter-grade landing while preserving:

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
| `Phase 7` | head/perf | Unhead meta tags via `useSeoHead`; LCP `<link rel="preload">` via custom Vite plugin (verified landing in both `dist/index.html` + `dist/es/index.html` under vite-ssg, 2026-05-21); sharp-based image pipeline (now q=90 webp / q=75 avif); below-fold sections code-split via `defineAsyncComponent` + `Suspense`; lazy modals + warm-prefetch via `use-warm-modal` | **DONE** (2026-05-16, verified 2026-05-21) |
| `Phase 8` | cleanup | Delete `webpack.config.js`, `translation-webpack-plugin.js`, `cheerio` dep, dead SFCs, dead SCSS partials, migration scripts, orphaned i18n keys; mirror reckit's CI; size-limit + **Lighthouse CI** + auto PR comment; CCS license headers | **DONE** (2026-05-21). All 8 CI gates wired: eslint / precheck / vitest / vite-ssg build / size-limit / lighthouse-ci / security / pre-check-label aggregator. |
| `Landing Redesign` | layout | Single-page futuristic landing; HudNav + Hero + Skills + Experience + NowProjects + SiteFooter + FAQ; BrandIcon SVG primitive; ADA dropdown; YouTube facade replaces Vimeo | **DONE** (2026-05-07), polished 2026-05-08, FAQ added 2026-05-17 |
| `Projects state model` | feature | NowShipping (5 states + 6-card cap + sort) + Featured (4 states + 9-card cap), version chips, count-up STARTED IN timer for WORKING_ON, polymorphic URL-less cards | **DONE** (2026-05-07) |
| `Footer dynamic manifest` | feature | SYS // SIGNATURE replaced with browser-state readout (HOST/PATH/LOCALE/LANG/VIEWPORT/TZ); prose tagline ending `MADE WITH L♥VE.` | **DONE** (2026-05-08) |
| `HUD decorations` | feature | Global `.hud-deco` utility, then componentized to `<UiHudDeco>`; corner labels + giant kanji watermarks per section; opacity 0.32 corners / 0.04 watermarks | **DONE** (2026-05-08), componentized 2026-05-17 |
| `ORCID badge` | feature | Sibling of CCS MEMBER tag in hero; ORCID brand SVG; faded brand colors; no-visual-hover pattern | **DONE** (2026-05-08) |
| `OKLCH palette migration` | refactor | All 8 color families (primary/secondary/neutral/border/success/warning/error/accent) re-emitted in `oklch()` space; ORCID + new `--clr-youtube-red` token preserved | **DONE** (2026-05-20, PR #134) |
| `Hero visual split` | refactor | Extracted portrait + meta-frame from `hero.vue` into `hero-visual.vue`; wraps in `<button>` that emits `open` to image-viewer modal; warmed on hover/focus via `use-warm-modal` | **DONE** (2026-05-17 → 2026-05-20) |
| `FAQ section` | feature | 6-item accordion (location / availability / work / current-role / different / contact), `landing.faq.*` i18n keys, JSON-LD `FAQPage` graph, `use-in-viewport` gated flares | **DONE** (2026-05-17, PR #128) |
| `Modal system` | feature | Unified `<UiModal>` (sm/md/lg/full variants) with focus-trap, body-scroll lock (`ModalLockRegistry`), ARIA-compliant dialog; companion `<UiModalLoading>` placeholder | **DONE** (2026-05-16) |
| `Cookie consent + GA4` | feature | `cookie-consent.vue` global sibling to nav; `kyo:consent` localStorage gate; gtag bootstrap for GA4 `G-6M3P3M2HG5`; separate YouTube consent prompt | **DONE** (2026-05-16, PR #123) |
| `YouTube facade` | feature | Replaces Vimeo facade; static thumbnail + play button; consent-gated; `youtube-nocookie.com`; warmed via `use-youtube-warmup` | **DONE** (2026-05-16) |
| `vite-ssg prerender` | build | `vite build` → `vite-ssg build`; EN + ES routes prerendered; middleware: `stripTrailingSlash`, `resolveDirIndex` (`/es/`, `/privacy/` → `/index.html`); critical-CSS extraction disabled (incompatible with 100% prerender); `@unhead/vue@2.1.15` for head metadata | **DONE** (2026-05-16) |
| `SEO / AEO round` | feature | JSON-LD graphs (person, site, FAQPage, Profile, videos) via `use-structured-data`; OG/Twitter/hreflang/canonical for EN+ES via `use-seo-head`; sitemap.xml; `check-json-ld.mjs` + `seo-audit.mjs` gates | **DONE** (2026-05-17, PRs #128/#130) |
| `Error pages` | feature | Static prerendered pages at `public/error-pages/{400,401,403,404,500}.html`; served by host config | **DONE** (2026-05-17, PR #129) |
| `Font subset` | perf | Per-family unicode-range subsets via `convert:fonts:{subset,symbols,latin}` scripts; trims WOFF2 weight | **DONE** (2026-05-16, PR #122) |
| `ADA round` | accessibility | `use-prose-links` directive auto-hardens external `<a>` (rel + ARIA + "opens in new tab" hint); focus-trap in modals; aria-* across FAQ accordion | **DONE** (2026-05-16, PR #122) |
| `Security gates` | CI | Inline-grep job scans for `eval` / `innerHTML` / hardcoded secrets / insecure `http://`; Protected Files advisory; Pre-Check Label aggregator | **DONE** (2026-05-16, PRs #125/#126) |
| `Image viewer modal` | feature | `<UiImageViewer>` modal-driven portrait carousel + zoom; opened from `hero-visual.vue`; warmed via `use-warm-modal` | **DONE** (2026-05-17) |
| `State-grid + Hud-deco componentization` | refactor | Promoted from `_theme.scss` utility classes to real components (`<UiStateGrid>`, `<UiHudDeco>`) so usage carries props/variants; SCSS utilities still exist for legacy call sites | **DONE** (2026-05-20) |
| `OKLCH contrast fix` | accessibility | `--clr-neutral-300` L=55.2%→70% (~3.2:1 → ~4.9:1 AA), `--clr-neutral-200` L=70.5%→78% to keep tier separation, `--clr-neutral-50` L=70.8%→76% for margin; removed `opacity: 0.55` compounding on footer manifest | **DONE** (2026-05-21) |
| `ADA review round (8 findings)` | accessibility | App.vue locale watcher for `<html lang>` sync; `<sup lang="ja">京</sup>`; modal aria-label fallback `\|\| undefined`; youtube-facade aria-modal correction; use-project-countdowns `reactive`→`ref`; nested `<section>`→`<div role="region">`; Vue `<Suspense>` warning filter | **DONE** (2026-05-21) |
| `size-limit gate` | CI | `.size-limit.json` with 7 gzipped budgets; `check:size` npm script; `Size Limit` CI job after `build` (reuses `dist-${{ sha }}` artifact); wired into `pre-check-label` aggregator | **DONE** (2026-05-21) |
| `Lighthouse CI gate` | CI | `lighthouserc.json` (4 category gates + 4 metric gates); `@lhci/cli` devDep; `check:lighthouse` npm script; `Lighthouse CI` job after `build` (reuses `dist-${{ sha }}` artifact); `temporary-public-storage` upload for report URLs; wired into `pre-check-label` aggregator. Phase 8 final item. | **DONE** (2026-05-21) |
| `Lighthouse PR comment` | CI | `scripts/lighthouse-comment.mjs` reads `.lighthouseci/` output (median across 3 runs), emits markdown scorecard; CI step posts (`gh pr comment`) or updates in place (`gh api PATCH`) via marker `<!-- lighthouse-ci-comment -->`; runs `if: always() && github.event_name == 'pull_request'` so comment posts on assertion failure too; job re-fails after comment via separate step so pre-check-label still catches red runs. | **DONE** (2026-05-21) |
| `Console-error resolution` | docs | Confirmed `SES lockdown-install.js` + `index.js:1:1108 TypeError` are **MetaMask browser-extension content scripts** (Network tab shows `moz-extension://` source). Documented in `docs/development.md` with verification flow + per-profile filter recipes. Saved as `reference_console_noise.md` memory entry. | **DONE** (2026-05-21) |
| `Forced reflow fix (footer)` | perf | `site-footer.vue` onMounted now invokes existing `onResize()` (rAF-throttled) instead of reading `window.innerWidth/Height` synchronously. Lighthouse attributed 143ms cumulative forced-reflow to the inline read while hydration was still applying scoped styles to the footer subtree. Audit `forced-reflow-insight` now n/a. | **DONE** (2026-05-21) |
| `Render-blocking CSS defer` | perf | `scripts/defer-async-css.mjs` post-build pass scans `dist/**/*.html`, rewrites every non-`app-*.css` `<link rel="stylesheet">` to `media="print" onload="this.media='all'..."` + `<noscript>` fallback. Removes 150ms render-blocking savings flag. Entry CSS stays critical. Vite transformIndexHtml runs BEFORE vite-ssg injects async-component CSS, so post-build script is the right tool. | **DONE** (2026-05-21) |
| `Vimeo removal` | cleanup | Full rip-out: deleted `src/config/features.js` (orphan, only carried Vimeo flag), removed `src/config/` directory + `@config` Vite alias, stripped `VIMEO_ENABLED`/`VIMEO_PRECONNECT` consts + `<%- vimeoPreconnect %>` slot from `vite.config.js` + `index.html`, removed `loadEnv` import (no remaining VITE_* env reads), removed stale Vimeo comment from `src/App.vue`. Zero `vimeo` references in `src/`, `public/`, or `dist/`. | **DONE** (2026-05-21) |
| `Critical chain shortened` | perf | `SymbolsNerdFontMono-Regular` added to font-preload-injector's FONT_FAMILIES list (was 4 fonts → 5). Previously discovered late by CSS parser; now parallel-fetched with the other hero fonts. Critical path latency: **690ms → 105ms** (-84%). The remaining 105ms chain is `index.html → app.js` (entry bundle, can't be reduced). FCP improved ~485ms → ~444ms. | **DONE** (2026-05-21) |
| `Unused-JS reduction` | perf | `@intlify/unplugin-vue-i18n@11.2.3` devDep added with `runtimeOnly: false`, `compositionOnly: true`, `fullInstall: true`, `strictMessage: false`. Main bundle 142.84 → 139.84 KB gzipped (-3 KB). Unused JS 27.4 KiB → 24.6 KiB. Did NOT pursue the deeper `runtimeOnly: true` + JSON-migration path because the 2 SEO modules (`person.js`, `faq-page.js`) read leaf-string translations directly — would need refactor to use `i18n.global.t()`. | **DONE** (2026-05-21, partial — 3 KB shipped, ~8 KB further available behind JSON migration) |
| `Hydration mismatch fix` | bug-fix | **Root cause:** vite-ssg's `formatting: 'minify'` runs html-minifier-terser on prerendered HTML and strips/collapses whitespace inside elements. Vue's CSR render functions still emit the original whitespace → hydration compares VNode text vs minified DOM text → mismatch. Two concrete offenders: (1) `now-projects-section.vue` `<p class="...__milestone">\n  // {{ x }}\n</p>` compiled a leading space into VNode text; minifier stripped it from SSR; (2) `snippets.js` `landing.footer.rights` translation had `\n` chars; minifier collapsed to spaces in SSR HTML; CSR's t() kept them. Fix: collapse the milestone template to one line + replace `\n` with space in EN+ES rights strings. **Best Practices: 96 → 100.** All 4 Lighthouse categories now 100/100 on both locales. Saved as `reference_hydration_whitespace.md` memory. | **DONE** (2026-05-21) |
| `Footer mobile 2-row layout` | refinement | `site-footer.vue` SCSS — at max-md, `__top` grid switches to `1fr` (single column) with `row-gap: 3rem`; `__socials` gets `text-align: right` and `__socials-list` gets `justify-content: flex-end`. CONTACT_CHANNELS now stacks on its own row aligned left, SOCIAL_GRID on the next row aligned right. Desktop unchanged. | **DONE** (2026-05-21) |
| `Ended projects sort + style` | refinement | `now-projects-section.vue`: (a) new first comparator in `main_cards` sort — `if (a.ended !== b.ended) return a.ended ? 1 : -1` pushes ended cards to the bottom regardless of `NOW_STATUS_PRIORITY`; (b) new `&.is-ended` SCSS block at the end of `&__card` (wins specificity tie vs `:not(.has-modal):hover`) overriding `--element-flare-color` and hover/focus border to `var(--clr-warning-100)` so flare + hover border match the ended-state pill. | **DONE** (2026-05-21) |
| `Worker emits ended state` | bug-fix | `now-project.worker.js` previously `continue`-skipped projects whose deadlines were all past — never emitted them — so consumer saw `cd === undefined` and `card.ended = cd && !cd.countdown` was always `false`. Fix: track `last_past_label` per project; if no future deadline exists but past ones did, emit `{ key, label: last_past_label, utc_ts: null }`; `_tick` branches on `utc_ts === null` to emit `{ label, utc_ts: null, countdown: null }` (the ended signal). ENDED label now reliably renders for projects with no future deadlines. | **DONE** (2026-05-21) |

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
48. **(2026-05-21)** **Lighthouse CI threshold strategy:** Performance ≥ 0.85, Accessibility ≥ 0.95, Best Practices ≥ 0.95, SEO ≥ 0.95. Individual metrics: LCP < 2500ms (warn), CLS < 0.1 (error), TBT < 300ms (warn), FCP < 1800ms (warn). Categories use `error`, individual metrics use `warn` to avoid false-positives on CI hardware variance. BP threshold briefly lowered to 0.90 during the hydration-mismatch investigation (BP score capped at 96 because of the console-error audit) — restored to 0.95 once the mismatch was fixed and BP hit 100.
49. **(2026-05-21)** **PR-comment-on-every-run pattern.** `continue-on-error: true` on the lhci step + post-comment step with `if: always() && github.event_name == 'pull_request'` + separate "Fail job if Lighthouse assertions failed" step to re-surface the failure. Comment uses marker `<!-- lighthouse-ci-comment -->` for in-place update via `gh api PATCH` instead of stacking new comments per push. Same pattern reusable for any future PR-comment automation.
50. **(2026-05-21)** **Reusable pattern: console.warn shim inline in `<head>` to capture Vue production warnings.** Vue 3 production hydration warnings bypass `app.config.warnHandler` and call `console.warn` directly. To capture full element details (not chromium's `[object HTMLParagraphElement]`), inject a synchronous inline `<script>` BEFORE the module bundle loads that wraps `console.warn` and serializes `arguments[1]` via `outerHTML.slice(0, 1200)`. Implementation pattern stored as a reference in `reference_hydration_whitespace.md`. Gated by `VITE_HYDRATION_DEBUG=true` env var when needed.
51. **(2026-05-21)** **HTML-minifier whitespace is load-bearing.** vite-ssg's `formatting: 'minify'` collapses whitespace in SSR HTML in ways the CSR render function doesn't replicate. Two source-side rules now apply: (a) NEVER mix static text and `{{ interpolation }}` on multiple lines inside the same element — either collapse to one line or use a computed value; (b) NEVER put `\n` characters inside translation strings rendered via plain `{{ t(...) }}` — use spaces, or `v-html` with `<br>` if a real line break is required. Rule documented in `reference_hydration_whitespace.md`.
52. **(2026-05-21)** **Ended sort precedence.** In `now-projects-section.vue`'s `main_cards` computed, `card.ended` wins over `NOW_STATUS_PRIORITY`. An ended WORKING_ON card (theoretically — though WORKING_ON has no deadlines so this is hypothetical for that status) still sinks below an active IN_PROGRESS card. Sort comparator order: `ended ? 1 : -1` first, then status priority, then deadline ms. Hydration-safe because `ended = false` for all cards on SSR/initial CSR (worker hasn't reported); flips reactively after first tick.

### 2.4 Pending Work

#### Phase 8 — COMPLETE

All 8 CI gates wired (eslint / precheck / vitest / vite-ssg build / size-limit / lighthouse-ci / security / pre-check-label). All 4 Lighthouse categories at 100/100 on EN+ES. No outstanding Phase 8 items.

#### Minor follow-ups (no urgency)

*   **Pursue full vue-i18n runtime-only path** — current `@intlify/unplugin-vue-i18n` config (`runtimeOnly: false`) captured ~3 KB of the 27 KB unused-JS opportunity. Going `runtimeOnly: true` would unlock ~8-10 KB more but requires (a) migrating `src/data/snippets.js` → `src/data/snippets.json`, (b) refactoring `src/seo/json-ld/{person,faq-page}.js` to use `i18n.global.t(key, locale)` instead of direct `TRANSLATIONS[locale][...]` tree access (because pre-compilation turns leaf strings into render functions), (c) updating `scripts/_lib.mjs`'s `loadTranslations()` to load JSON instead of dynamic-importing the JS module. Scope: ~30-45 min. Was deferred — the user's call was "keep the 3 KB win and move on."
*   **Re-import IMG_6550.png for lossless portrait variants** — original PNG was removed from Downloads after the 2026-05-08 swap; current variants were re-encoded from a q=82 JPG. Drop IMG_6550.png back and run `node scripts/convert-images.mjs --force`.
*   **Architecture extraction.** Patterns now safe to lift into a persistent architecture-memory file: (1) BrandIcon SVG-glob + currentColor; (2) `\uXXXX`-only glyph encoding; (3) ADA dropdown (`role="menu"` + `menuitemradio`); (4) opacity-animated `.state-grid`; (5) polymorphic card root (`<component :is="...">`); (6) dynamic-manifest footer; (7) UI-primitive variant tandem (UiLink + UiButton mirror); (8) OKLCH palette tokenization; (9) vite-ssg prerender + middleware; (10) `<ClientOnly>` hydration guard; (11) JSON-LD via `use-structured-data`; (12) gtag consent gate via `kyo:consent`; (13) modal warm-prefetch via `use-warm-modal`; (14) `use-prose-links` external-link hardening; (15) `use-obfuscated-email` post-mount mailto; (16) **vite-ssg whitespace hydration trap** (NEW 2026-05-21 — also in `reference_hydration_whitespace.md`); (17) **PR-comment-on-every-run CI pattern** (NEW 2026-05-21); (18) **defer-async-css post-build pass** (NEW 2026-05-21); (19) **`unplugin-vue-i18n` partial config** (NEW 2026-05-21).
*   **Smoke test on real devices** — iPhone Safari + Chrome Android — focus on: footer mobile 2-row layout (channels left / socials right), FAQ accordion ARIA, cookie banner first-paint, YouTube facade activation, image-viewer modal, mobile typography, OKLCH parity on Safari <16.4.
*   **Stale task #16** — user once said *"change name."* with no antecedent. Probably ignore.
*   **Optional cleanups noticed during the 2026-05-20 audit:** verify whether legacy migration scripts that the 2026-05-08 sweep marked deleted have reappeared (the explore agent's report suggests "all legacy scripts remain" — reconcile against the §4.2 catalogue before next prune).

---

## SECTION 3: IMPLEMENTATIONS

> Per-deliverable detail. Each plan document is canonical for its phase; the scripts are the execution layer; the landing widgets/sections are the current page composition.

### 3.1 `VUE_MIGRATION_PLAN.md` (top-level plan)
**Created:** 2026-05-05 | **Last updated:** 2026-05-05
**Status:** Authored; phases 0–6 executed against it; Landing Redesign supersedes Phase 6's composition only.
**Path:** `$REPO/VUE_MIGRATION_PLAN.md`

Top-level plan. References every deep-dive companion. Architectural target tree (reckit kind-folder layout). Risk register. Feature-flag pattern. 8-phase plan. Testing checklist. Effort estimate ~6.5–7 days.

### 3.2 `TRANSLATION_MIGRATION.md` (i18n deep dive)
**Created:** 2026-05-05 | **Status:** Implemented (Phase 3 complete).
**Path:** `$REPO/TRANSLATION_MIGRATION.md`

Phase 3 detail. Eight sections: audit, 12 latent bugs, target design, improvements, step-by-step plan, file diff, testing checklist, decisions.

### 3.3 `PERFORMANCE_MIGRATION.md` (perf deep dive)
**Created:** 2026-05-05 | **Last updated:** 2026-05-08
**Status:** Mostly implemented (Phase 7 in progress); image-pipeline strategy revised; quality bumped 2026-05-08.
**Path:** `$REPO/PERFORMANCE_MIGRATION.md`

Workers + image strategy + fonts + Vimeo + cyberpunk-glow refactor + scroll/resize modernization + bundle compression + perf budgets. Plus the 2026-05-07 audit results (prefers-reduced-motion, opacity-based animations, content-visibility rejection).

### 3.4 `SASS_THEMING_MIGRATION.md` (SCSS theming deep dive)
**Created:** 2026-05-05 | **Last updated:** 2026-05-08
**Status:** Implemented (Phase 2 complete); palette extended (accent + orcid tokens); typography small tier rebumped.
**Path:** `$REPO/SASS_THEMING_MIGRATION.md`

Token verification, architecture mirroring reckit, `additionalData` injection, scope migration map, 60/30/10 rule, Phase 2 step-by-step, bug fixes.

### 3.5 `CODE_STANDARDS_MIGRATION.md` (lint + naming deep dive)
**Created:** 2026-05-05 | **Status:** Implemented (Phase 1 complete).
**Path:** `$REPO/CODE_STANDARDS_MIGRATION.md`

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
**Path:** `$REPO_OLD/`

### 3.13 `reckit/` (canonical pattern reference)
**Status:** Read-only reference.
**Path:** `$REPO_RECKIT/`

Read on first session resume: `eslint.config.mjs`, `vite.config.js`, `src/app/scss/abstracts/_variables.scss`, `src/app/scss/abstracts/_theme.scss`, `src/views/home.vue`, `src/shared/composables/use-recording-status.js`, `src/brands/kyonax-on-tech/sources/hud/cam-person.vue`. Roam-node companions: `~/.brain.d/roam-nodes/reckit/{2026-04-17-reckit_architecture,2026-04-20-reckit_naming_conventions}.org`.

### 3.14 OKLCH palette migration (2026-05-20, PR #134)
**Status:** Production. All 8 color families re-emitted in `oklch()` space.
**Path:** `src/scss/abstracts/_variables.scss`, `src/scss/abstracts/_theme.scss`.

Same family names + scale as the prior HSL palette (primary/secondary/neutral/border/success/warning/error/accent) with 5–7 shades each. Sample anchors: `primary-100 oklch(85.9% 0.1686 91.3)` (brand yellow `#f9cd26`), `secondary-100 oklch(54.7% 0.24 264.3)` (blue `#265ef9`), `neutral-500 oklch(14.5% 0 0)` (page bg). Border family preserves `/ 0.2` alpha. Off-palette tokens preserved: `--clr-orcid-bg`, `--clr-orcid-fg`. Added: `--clr-youtube-red` for the YouTube facade play button. Token consumption discipline (P1 `var(--clr-*)`) unchanged. **iOS Safari ≥16.4** required for native oklch — older versions need a graceful-degradation pass if telemetry shows non-trivial share.

### 3.15 Hero split — `hero-visual.vue` (2026-05-17 → 2026-05-20)
**Role:** Extracted portrait + visual-meta frame from `hero.vue`.
**Path:** `src/views/components/sections/hero-visual.vue`.
**Parent:** `hero.vue`.
**Imports:** `UiImage`, `use-warm-modal`.
**Composition:** wraps portrait in a `<button type="button">` that emits `open` to its parent → `<UiImageViewer>` modal. Warms image-viewer chunk + decoded bitmaps on `mouseenter`/`focus`. Visual-meta line stays inside this component (`FRAME // ▣-001`, `@KYONAX_ON_TECH`, aria-hidden spans).
**Why split:** the visual is the only modal-trigger surface in the hero — isolating it keeps `hero.vue` focused on copy + CTAs, and the warm-prefetch lifecycle stays scoped.

### 3.16 `FaqSection` (`@sections/faq.vue`) — 2026-05-17 (PR #128)
**Role:** Accordion FAQ above the footer. **Parent:** `App.vue` (lazy-loaded).
**Items (6):** `location`, `availability`, `work`, `current-role`, `different`, `contact` — keyed under `landing.faq.*` in `src/data/snippets.js`.
**Behavior:** toggle-driven accordion (one open at a time or independent — confirm in code on next visit). Uses `UiSectionHeader`, `UiHudDeco`, and `use-in-viewport` to gate flare animation until the section enters the viewport.
**JSON-LD:** Section copy + answers are also emitted as a `FAQPage` graph by `use-structured-data` so the markup helps AEO/Google AI surfaces.

### 3.17 Modal system — `<UiModal>` + `<UiModalLoading>` (2026-05-16)
**Role:** Unified modal primitive. **Paths:** `src/components/ui/modal.vue`, `src/components/ui/modal-loading.vue`.
**Props:** `size` (`sm | md | lg | full`), plus standard ARIA-dialog wiring (`role="dialog"`, `aria-modal`, `aria-labelledby`).
**Behavior:** focus-trap on open, body-scroll lock via a shared `ModalLockRegistry` (refcount so nested/concurrent modals don't double-toggle), close button + `Escape` + click-outside (click-outside is opt-in — verify per call site).
**Companion:** `<UiModalLoading>` is the placeholder rendered while an async-loaded modal's chunk is in flight.
**Consumers:** `image-viewer.vue` (hero portrait), any future media modal. Warm-prefetch via `use-warm-modal` on the trigger element.

### 3.18 Cookie consent + GA4 (`src/components/cookie-consent.vue`, 2026-05-16, PR #123)
**Role:** Global consent banner — mounted as sibling to `HudNav` in `App.vue`, not a `<section>`.
**Storage:** `localStorage['kyo:consent']` (accepted / declined / unset).
**GA4:** `G-6M3P3M2HG5` (public measurement ID, not a secret — flagged in the Security Scan exclusion list). Bootstraps `dataLayer` + `window.gtag` ONLY if user accepts. Declining wipes any pre-existing gtag state.
**YouTube consent:** separate gate for `youtube-nocookie.com` embeds — keeps the analytics decision and the third-party-iframe decision independent.
**Privacy link:** locale-aware (`/privacy` for EN, `/es/privacy` for ES).

### 3.19 YouTube facade — `youtube-facade.vue` (2026-05-16)
**Role:** Replaces the (deleted) Vimeo facade. **Path:** `src/components/ui/youtube-facade.vue`.
**Pattern:** static thumbnail + play-button overlay until the user clicks. On click checks the YouTube consent gate; if granted, swaps in the real iframe (`youtube-nocookie.com`). On hover/focus, `use-youtube-warmup` preloads the iframe chunk + thumbnail.
**Bytes shipped before activation:** thumbnail + a few KB of overlay CSS — zero `youtube.com` requests.

### 3.20 vite-ssg prerender + middleware (2026-05-16)
**Status:** Production. `npm run build` runs `NODE_ENV=production vite-ssg build`.
**Routes prerendered:** EN root + `/es/` + `/privacy` + `/es/privacy` (route list generated from `src/seo/routes.js`).
**`ssgOptions`:** `mode: 'production'`, `rootContainerId: 'root'`, `dirStyle: 'nested'`, `formatting: 'minify'`, `script: 'async'`.
**Middleware:** `stripTrailingSlash` returns 302 + no-cache; `resolveDirIndex` internally rewrites `/es/` and `/privacy/` to their `index.html` so they prerender cleanly.
**Hydration guard:** `<ClientOnly>` wraps SSR-unsafe content (footer manifest's runtime browser readout, anything reading `window` synchronously). Hydration mismatch is non-negotiable to avoid.
**Critical CSS:** extraction **disabled** — incompatible with 100% prerender (would inline duplicated critical blocks into every route).
**Head metadata:** `@unhead/vue@2.1.15` replaces the older Unhead binding. `use-seo-head` calls into `useHead()` with title/description/og/twitter/hreflang/canonical for each locale.

### 3.21 SEO / AEO / structured data (2026-05-17, PRs #128 + #130)
**Composables:** `use-seo-head.js` (OG/Twitter/hreflang/canonical), `use-structured-data.js` (JSON-LD injection).
**Graphs:** Person (Cristian Moreno Kyonax), Site (WebSite), FAQPage (from `faq.vue`), ProfilePage, Videos (when YouTube facade activates).
**Source folder:** `src/seo/json-ld/` (canonical graphs) + `src/seo/routes.js` (route list for vite-ssg).
**Sitemap:** `public/sitemap.xml` curated for both locales.
**Validation:** `node scripts/check-json-ld.mjs` validates every graph; `node scripts/seo-audit.mjs` + `node scripts/seo-analyzer-run.mjs` run post-build SEO quality checks.

### 3.22 Error pages — static prerendered (2026-05-17, PR #129)
**Path:** `public/error-pages/{400,401,403,404,500}.html`.
**Pattern:** plain HTML pages — NOT Vue-routed. Host config (Apache/nginx) serves them as `ErrorDocument` directives. Each page is brand-consistent (cyberpunk theme, OKLCH palette via inline `<style>`).

### 3.23 New composables (2026-05-16 → 2026-05-20)
| Composable | Path | Role |
|---|---|---|
| `use-clickable-card.js` | `src/composables/use-clickable-card.js` | `Enter` / `Space` key handler for keyboard card activation (works with `<component :is="...">` polymorphic root) |
| `use-image-ready.js` | `src/composables/use-image-ready.js` | Image-load detection hook + companion directive |
| `use-in-viewport.js` | `src/composables/use-in-viewport.js` | IntersectionObserver wrapper for viewport gating (flares, animations, FAQ-section reveal) |
| `use-obfuscated-email.js` | `src/composables/use-obfuscated-email.js` | Post-mount mailto: href assembly (SSR placeholder = `#`); defeats naïve scrapers |
| `use-prose-links.js` | `src/composables/use-prose-links.js` | `v-prose-links` directive — auto-hardens external `<a>` (`rel="noopener noreferrer"`, generated `aria-label`, "(opens in new tab)" hint) |
| `use-structured-data.js` | `src/composables/use-structured-data.js` | JSON-LD graph injection — site/person/FAQ/profile/videos |
| `use-warm-modal.js` | `src/composables/use-warm-modal.js` | Preloads modal chunk + decoded bitmaps on hover/focus; deduped; holds strong refs to decoded `ImageBitmap`s |
| `use-youtube-warmup.js` | `src/composables/use-youtube-warmup.js` | Preloads YouTube iframe chunk + thumbnail on hover/focus (no `youtube.com` traffic until activation) |

**Removed:** `use-scrolled-class.js` (no longer referenced).

### 3.24 UI primitive expansion (7 → 16)
**Added since 2026-05-08:**
| Primitive | Path | Role |
|---|---|---|
| `UiModal` | `src/components/ui/modal.vue` | Generic dialog (sm/md/lg/full); focus-trap; ModalLockRegistry-backed scroll lock |
| `UiModalLoading` | `src/components/ui/modal-loading.vue` | Placeholder rendered while async modal chunk loads |
| `UiSectionHeader` | `src/components/ui/section-header.vue` | Index-tag + h2/h3/h4 + optional subtitle. Separate from `section-heading.vue` (`UiSectionHeader` carries the index-tag + subtitle structure; `UiSectionHeading` is the minimal heading variant) |
| `UiStateGrid` | `src/components/ui/state-grid.vue` | Componentized 3×3 pulsing loader (was `.state-grid` SCSS utility); accepts `--state-color` via prop |
| `UiHudDeco` | `src/components/ui/hud-deco.vue` | Componentized HUD corner labels + watermarks (was `.hud-deco` SCSS utility); `position` (`tl/tr/bl/br/watermark`) variants |
| `UiYoutubeFacade` | `src/components/ui/youtube-facade.vue` | Static thumbnail + consent-gated activation |
| `UiClientOnly` | `src/components/ui/client-only.vue` | SSR hydration guard (renders children only post-mount); optional `<template #placeholder>` slot |
| `UiIconSprite` | `src/components/ui/icon-sprite.vue` | Inlined SVG sprite loader (alternate icon strategy for high-density grids) |
| `UiImageViewer` | `src/components/ui/image-viewer.vue` | Modal-driven portrait viewer + zoom; opened from `hero-visual.vue` |

### 3.25a OKLCH contrast fix + ADA round (2026-05-21)
**Status:** Production. Lighthouse contrast failures on `.skills__category-count` + `.skills__item-name` resolved.

**Palette adjustments (`src/scss/abstracts/_variables.scss`):**
*   `--clr-neutral-300`: `oklch(55.2% 0.016 285.938)` → `oklch(70% 0.016 285.938)`. Old value was Tailwind zinc-500 (~3.2:1 on bg, fails AA). New value ~4.9:1 (passes AA with margin). Affects 14 call sites.
*   `--clr-neutral-200`: `oklch(70.5% 0.015 286.375)` → `oklch(78% 0.014 286.375)`. Restores tier separation that the neutral-300 bump would have collapsed.
*   `--clr-neutral-50`: `oklch(70.8% 0 0)` → `oklch(76% 0 0)`. Was AA-marginal (~4.7:1, no buffer); bumped to ~5.8:1 for comfortable margin.

**Opacity stack fix (`src/views/components/sections/site-footer.vue`):**
*   Removed `opacity: 0.55` on `.site-footer__manifest`. Old value compounded with neutral-200/neutral-300 values to ~2.2:1 / ~3.0:1 effective contrast.

**ADA findings resolved (7 of 8, 1 skipped as already passing):**
| # | WCAG | File | Change |
|---|---|---|---|
| 1 | 3.1.1 | `App.vue` | `watch(locale, ...)` updates `document.documentElement.lang` on every locale change (immediate + SSR-safe) |
| 2 | 3.1.2 | `hero.vue` | `<sup>京</sup>` → `<sup lang="ja">京</sup>` |
| 3 | 4.1.2 | `modal.vue` | `:aria-label="ariaLabel \|\| title"` → `\|\| undefined` |
| 4 | 4.1.2 | `youtube-facade.vue` | `aria-modal="false"` → `"true"` (matches existing focus management) |
| 5 | 1.4.3 | `_variables.scss` | `neutral-50` L=70.8% → 76% |
| 6 | 1.4.3 | (skipped) | `warning-100` recompute ~8.5:1 — already passes AA |
| 7 | Vue 3 | `use-project-countdowns.js` + consumer | `reactive({})` + `Object.assign` → `ref({})` + atomic replacement |
| 8 | 1.3.1 | `now-projects-section.vue` | Nested `<section aria-labelledby>` → `<div role="region" aria-labelledby>` |

**Vue noise filter (`src/main.js`):**
*   `app.config.warnHandler` filters the `<Suspense> is an experimental feature` warning specifically. Other Vue warnings still surface.

### 3.25b `size-limit` gate (2026-05-21)
**Status:** Production. Wired as the 6th CI gate.
**Paths:** `.size-limit.json` (root), `npm run check:size` script, `Size Limit` job in `.github/workflows/ci.yml`.

**Budget table (gzipped, current baseline → limit):**
| Asset | Current | Limit | Headroom |
|---|---|---|---|
| main bundle (JS) `app-*.js` | 142.84 KB | 160 KB | 11% |
| main bundle (CSS) `app-*.css` | 10.17 KB | 12 KB | 15% |
| now-projects chunk (JS+CSS) | 8.64 KB | 10 KB | 14% |
| modal chunk (JS+CSS) | 2.6 KB | 4 KB | 35% |
| image-viewer chunk (JS+CSS) | 2.1 KB | 3 KB | 30% |
| youtube-facade chunk (JS+CSS) | 3.46 KB | 5 KB | 31% |
| FAQ chunk (JS+CSS) | 1.9 KB | 3 KB | 37% |

**CI integration:**
*   `Size Limit` job runs after `build`, downloads the `dist-${{ github.sha }}` artifact, runs `npm run check:size`.
*   Added to `pre-check-label` `needs:` list — overruns apply the "Pre-Check Failed" label.

**Side fix (same commit window):** `src/widgets/hud-nav.vue:48` `document.getElementById` → `document.querySelector('#'+id)` + curly braces on two single-line `if`s. These lint errors landed in commit `9097bb9` (2026-05-20 OKLCH PR) and would have blocked any future CI lint run.

### 3.25c Console errors — RESOLVED (extension-sourced, 2026-05-21)
**Status:** RESOLVED. Confirmed via Network-tab absence of `lockdown-install.js` (browser extension content scripts don't show up in page network) + `moz-extension://` source URL on the `index.js:1:1108` TypeError. Source: MetaMask (ships Agoric SES `lockdown-install.js` verbatim; minified `index.js` content-script entry). No project code change required.

**Deliverables:**
*   `docs/development.md` — documents expected console noise, two-step verification flow (Network tab + source URL prefix), per-profile filter recipes for Firefox + Chromium, and a verification checklist for future similar messages.
*   `reference_console_noise.md` memory entry — captures resolution so future sessions don't re-investigate.

### 3.26 Lighthouse CI + auto PR comment (2026-05-21)
**Status:** Production. Phase 8 final item.

**Files:**
*   `lighthouserc.json` — 4 category gates (perf ≥ 0.85, a11y ≥ 0.95, BP ≥ 0.95, SEO ≥ 0.95) + 4 metric gates (LCP < 2500ms warn, CLS < 0.1 error, TBT < 300ms warn, FCP < 1800ms warn). 3 runs per URL on `staticDistDir: './dist'`, `preset: 'desktop'`, `chromeFlags: '--no-sandbox --headless'`, audits `/` + `/es/`, uploads to `temporary-public-storage`.
*   `package.json` — added `@lhci/cli` devDep; `check:lighthouse` script.
*   `scripts/lighthouse-comment.mjs` — reads `.lighthouseci/lhr-*.json` + `links.json`, computes median scores per URL across runs, emits markdown scorecard with category table + metric table + report links. Stub-output path when `.lighthouseci/` is empty (e.g., lhci crashed). Marker: `<!-- lighthouse-ci-comment -->`.
*   `.github/workflows/ci.yml` — new `lighthouse-ci` job after `build` (reuses `dist-${{ sha }}` artifact). 3 steps: (1) run lhci with `continue-on-error: true` so the comment step always runs; (2) post-comment step `if: always() && github.event_name == 'pull_request'` — uses `gh api` to find existing comment by marker, PATCHes if found, posts fresh otherwise; (3) re-fail step `if: always() && steps.lhci.outcome == 'failure'` re-surfaces the assertion failure as the job outcome.
*   `.gitignore` — `.lighthouseci/` added (transient output).

**Wired into `pre-check-label` aggregator** so a failing Lighthouse run applies the Pre-Check Failed label.

**Reusable patterns:** the `continue-on-error + always-run comment + re-fail` triad is reusable for any future CI gate where you want comment-on-PR even on failure. Captured in §2.3 decision #49.

### 3.27 Forced reflow + render-blocking + critical chain (2026-05-21)
**Status:** Production. Three related performance fixes from a single Lighthouse audit pass.

**Forced reflow (143ms → 0ms):**
Source-mapped `app-*.js:133:1226` to `site-footer.vue`'s `onMounted` reading `window.innerWidth/Height` synchronously while hydration was still applying scoped styles + inlining brand SVGs to the footer subtree. Fix: replaced the inline read with `onResize()` (existing rAF-throttled helper). Layout settles inside the rAF tick. `forced-reflow-insight` audit now n/a.

**Render-blocking CSS (150ms savings → 0ms):**
`scripts/defer-async-css.mjs` post-build pass scans `dist/**/*.html`, rewrites every non-`app-*.css` `<link rel="stylesheet">` to use media-swap loading: `media="print" onload="this.media='all'..."` + `<noscript>` fallback. Skips `error-pages/` (standalone static pages). Wired in `postbuild`: `defer-async-css.mjs && seo-audit.mjs`. Entry CSS stays critical-blocking. Tried a `transformIndexHtml` plugin first but it runs BEFORE vite-ssg injects the async-component stylesheets — post-build script is the right tool.

**Critical chain (690ms → 105ms):**
The `SymbolsNerdFontMono-Regular.woff2` font was discovered late by the CSS parser (browser sees `@font-face`, only fetches when a PUA glyph in unicode-range appears). Added it to `vite.config.js`'s `font-preload-injector` `FONT_FAMILIES` list (now 5 fonts preloaded instead of 4). The font now parallel-fetches with the hero fonts. FCP improved ~485ms → ~444ms.

### 3.28 Vimeo removal (2026-05-21)
**Status:** Done.

**Files modified:**
*   `index.html` — removed `<%- vimeoPreconnect %>` slot + the comment reference to it.
*   `vite.config.js` — removed `VIMEO_ENABLED` / `VIMEO_PRECONNECT` consts, the `vimeoPreconnect` slot in `createHtmlPlugin`'s `inject.data`, the `VITE_VIMEO_*` define entries, the now-unused `loadEnv` import + call (only `process.env.npm_package_version` reads env now), the `@config` Vite alias.
*   `src/App.vue` — removed stale Vimeo comment from the license preamble block.

**Files deleted:**
*   `src/config/features.js` — only contained the Vimeo flag (orphan after no consumers remained).
*   `src/config/` directory — empty after deletion.

Zero `vimeo` references in `src/`, `public/`, or `dist/`. The unused `preconnect` to `player.vimeo.com` is gone (Lighthouse `uses-rel-preconnect` audit now 0ms / score 1).

### 3.29 Unused-JS reduction — `@intlify/unplugin-vue-i18n` partial wiring (2026-05-21)
**Status:** Partial — 3 KB shipped, ~8-10 KB more available behind a deeper migration.

**What landed:**
*   `@intlify/unplugin-vue-i18n@11.2.3` devDep added.
*   Plugin wired in `vite.config.js` plugins array (after `vue()`) with options: `runtimeOnly: false`, `compositionOnly: true`, `fullInstall: true`, `strictMessage: false`. The `compositionOnly` flag strips legacy Options-API code paths from vue-i18n; the other flags keep current behavior.
*   Main bundle: 142.84 → 139.84 KB gzipped (-3 KB).
*   Lighthouse `unused-javascript`: 27.4 KiB → 24.6 KiB.

**What didn't land (intentionally):**
*   `runtimeOnly: true` would unlock ~8-10 KB more by eliminating `@intlify/message-compiler` (~6.6 KiB transferred) + the compiler-side of `vue-i18n.mjs` (~6+ KiB). Requires pre-compilation of messages, which only works on JSON/YAML files matched by `include`.
*   Pre-compilation refactor scope: (a) migrate `src/data/snippets.js` → `src/data/snippets.json` (mechanical: strip license preamble, swap single→double quotes, kill trailing commas); (b) refactor `src/seo/json-ld/person.js` + `faq-page.js` from direct tree access (`TRANSLATIONS[locale]['kyo-web']...`) to `i18n.global.t(key, locale)` — pre-compilation turns leaf strings into functions, breaking direct reads; (c) update `scripts/_lib.mjs`'s `loadTranslations()` to load JSON via Node `with { type: 'json' }`.
*   User's call after weighing tradeoffs: "keep the 3 KB win and move on." Logged in §2.4 as optional follow-up.

### 3.30 Hydration mismatch — root cause + fix (BP 96 → 100, 2026-05-21)
**Status:** Production. All 4 Lighthouse categories now 100/100 on both locales.

**Root cause:** vite-ssg's `formatting: 'minify'` runs html-minifier-terser on prerendered HTML and strips/collapses whitespace inside elements. Vue 3's CSR render functions still emit the original whitespace. Hydration compares VNode text against minified DOM text and emits `[Vue warn]: Hydration text content mismatch` for each affected element.

**Two concrete offenders:**
1.  **`now-projects-section.vue` milestone `<p>` (6 mismatches, one per NowShipping card).** Template was:
    ```vue
    <p class="now-projects-section__milestone">
      // {{ card.label.toUpperCase() }}
    </p>
    ```
    The leading newline+indent compiled into VNode text as `" // " + label`. Minifier stripped the leading space from SSR HTML. Fix: collapse to one line — `<p>// {{ card.label.toUpperCase() }}</p>`.
2.  **`site-footer.vue` rights `<small>` (1 generic mismatch).** Source string in `snippets.js` had `\n` line breaks: `'Source code under GPL-2.0-only.\nDesign and original content © {year} ...\nAll rights reserved.'`. Runtime `t()` returned the literal `\n` chars. Minifier collapsed them to spaces in SSR HTML. Fix: replaced `\n` with space in EN + ES rights strings.

**Investigation tooling (used + reverted):**
1.  Set `__VUE_PROD_HYDRATION_MISMATCH_DETAILS__: 'true'` in `vite.config.js` `define` block to surface detailed warnings (Vue 3 strips them by default in production builds).
2.  Added a temporary `transformIndexHtml` plugin named `hydration-debug-capture` (gated by `VITE_HYDRATION_DEBUG=true` env var) that injects a synchronous inline `<script>` BEFORE the module bundle loads, wrapping `console.warn` to serialize `arguments[1]` (the element) via `outerHTML.slice(0, 1200)`. Vue 3 production hydration warnings bypass `app.config.warnHandler` and call `console.warn` directly — that's why a pre-bundle `console.warn` shim is the only reliable capture point.
3.  Built with debug flag, served `dist/` via Python http.server, hit with headless chromium, parsed `--enable-logging=stderr` output to extract the offending elements' outerHTML. Reverted all debug instrumentation after diagnosis.

**Cleanup:**
*   `lighthouserc.json` BP threshold restored to 0.95 (was lowered to 0.90 as workaround during investigation — not needed now that score is consistently 100).
*   Saved `reference_hydration_whitespace.md` memory entry capturing the trap, the two patterns to avoid going forward, and the debug-shim implementation reference.
*   `project_hydration_mismatch.md` memory entry deleted (was the "investigation in progress" marker; now resolved).

### 3.31 NowProjects refinements (2026-05-21)
**Status:** Production.

**Footer mobile/tablet 2-row layout (`site-footer.vue`):**
At max-md, `__top` grid switches from `1fr 1fr` to `1fr` (single column) with `row-gap: 3rem`. Channels stack on their own row (left-aligned, default), socials on the next row with `text-align: right` on `&__socials` + `justify-content: flex-end` on `&__socials-list`. Desktop layout unchanged (still 2-column with brand spanning row 1).

**Ended-projects sort + style (`now-projects-section.vue`):**
*   New first comparator in `main_cards` computed: `if (a.ended !== b.ended) return a.ended ? 1 : -1` — pushes ended cards to the bottom regardless of `NOW_STATUS_PRIORITY`.
*   New `&.is-ended` SCSS block at the END of `&__card` (placement matters — wins specificity tie vs `:not(.has-modal):hover`): overrides `--element-flare-color: var(--clr-warning-100)` and hover/focus `border-color: var(--clr-warning-100)` so the flare + hover border match the orange ended-state pill.

**ENDED label worker fix (`now-project.worker.js`):**
The worker was `continue`-skipping projects whose deadlines were all past — never emitting them — so consumer saw `cd === undefined` and `card.ended = cd && !cd.countdown` was always `false`. Fix:
*   In `_hydrate_cache`: track `last_past_label` per project; if no future deadline exists but past ones did, push `{ key, label: last_past_label, utc_ts: null }` to `cached_deadlines`.
*   In `_tick`: branch on `utc_ts === null` to emit `{ label, utc_ts: null, countdown: null }` (the ended signal).
*   Wire-protocol JSDoc updated: `outbound: Record<string, {label: string, countdown: string|null, utc_ts: number|null}>`.

The ENDED pill now reliably renders for all-deadlines-past projects.

### 3.25 New scripts (2026-05-16 → 2026-05-17)
| Script | Purpose | Wired? |
|---|---|---|
| `check-json-ld.mjs` | Validates every JSON-LD graph emitted by `use-structured-data` | yes — precheck composite + CI |
| `check-nerd-glyphs.mjs` | Verifies Nerd Font glyph usage (catches tofu drift) | yes — precheck composite |
| `seo-audit.mjs` | SEO quality checks (titles, meta, OG, canonicals) | post-build |
| `seo-analyzer-run.mjs` | Wrapper for the SEO analyzer tool run | post-build |
| `convert:fonts:subset` / `:symbols` / `:latin` | Per-family unicode-range WOFF2 subsets | manual (run when fonts change) |

**Reconcile next visit:** the 2026-05-08 Phase 8 sweep marked `migrate-snippets-to-esm.mjs`, `migrate-trans-attrs.mjs`, `scaffold-sfc.mjs`, `audit-baseline.mjs` deleted. The 2026-05-20 audit report suggests "all legacy scripts remain" — verify on disk and re-prune if they reappeared.

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

### 4.2 Scripts (`scripts/`) — refreshed 2026-05-21
| File | Purpose |
|---|---|
| `_lib.mjs` | shared helpers |
| `check-i18n.mjs` | locale parity gate |
| `check-i18n-keys.mjs` | template `t()` keys gate |
| `check-trans-attrs.mjs` | banned-reference gate |
| `check-color-usage.mjs` | 60/30/10 audit + literal ban (scans `<style>` AND comments) |
| `check-aliases.mjs` | Vite ↔ ESLint alias sync |
| `check-license-headers.mjs` | CCS preamble gate |
| `check-json-ld.mjs` | JSON-LD graph validation (added 2026-05-17) |
| `check-nerd-glyphs.mjs` | Nerd Font glyph usage / tofu drift (added 2026-05-17) |
| `precheck.mjs` | composite gate |
| `convert-fonts.sh` | Phase 2 TTF→WOFF2 |
| `convert-images.mjs` | image WebP (q=90) + AVIF (q=75) generator (predev/prebuild) |
| `seo-audit.mjs` | post-build SEO quality checks (added 2026-05-17) |
| `seo-analyzer-run.mjs` | wrapper for SEO analyzer tool (added 2026-05-17) |
| `convert:fonts:{subset,symbols,latin}` | per-family unicode-range WOFF2 subsetting (added 2026-05-16) |
| `defer-async-css.mjs` | post-build pass — converts non-`app-*.css` `<link rel="stylesheet">` tags to media-swap loading + `<noscript>` fallback (added 2026-05-21) |
| `lighthouse-comment.mjs` | reads `.lighthouseci/` output, emits PR-comment markdown scorecard with median scores + metrics + report links (added 2026-05-21) |

### 4.3 Source — config and root
| File | Role |
|---|---|
| `vite.config.js` | aliases + SCSS additionalData + `transformIndexHtml` LCP preload + font-preload (5 fonts incl. SymbolsNerdFontMono since 2026-05-21) + AD-10 redirect + `@intlify/unplugin-vue-i18n` plugin + vite-ssg `ssgOptions` + middleware (`stripTrailingSlash`, `resolveDirIndex`). Vimeo refs fully removed 2026-05-21; `@config` alias removed. |
| `eslint.config.mjs` | flat config (CCS + Vue rules) |
| `index.html` | root template; `<%- vimeoPreconnect %>` slot removed 2026-05-21 |
| `src/main.js` | entrypoint (vite-ssg entry); registers `app.config.warnHandler` to suppress the noisy `<Suspense> experimental` Vue warning (text-specific filter, 2026-05-21) |
| `src/App.vue` | landing root: HudNav + CookieConsent + 5 sections (Hero + Skills + Experience + lazy NowProjects + lazy FAQ) + SiteFooter. Owns `watch(locale, ...)` that syncs `document.documentElement.lang` (immediate, SSR-safe) for WCAG 3.1.1 across every locale-change path |
| `docs/development.md` | expected console noise (MetaMask SES + `index.js` TypeError) + verification flow + per-profile filter recipes (added 2026-05-21) |
| `.size-limit.json` | 7 gzipped bundle-size budgets (added 2026-05-21) — main JS 160 KB, main CSS 12 KB, async chunks 3-10 KB each |
| `lighthouserc.json` | Lighthouse CI config (added 2026-05-21): 4 category gates + 4 metric gates, `staticDistDir: './dist'`, 3 runs per URL, desktop preset, `temporary-public-storage` upload |
| `package.json` | scripts: predev/prebuild → `convert-images.mjs`; build → `vite-ssg build`; postbuild → `defer-async-css.mjs && seo-audit.mjs` (chained 2026-05-21); precheck composite; SEO + JSON-LD + nerd-glyph gates; `check:size`; `check:lighthouse` (added 2026-05-21); font-subset commands. DevDeps include `size-limit` + `@size-limit/file` + `@lhci/cli` + `@intlify/unplugin-vue-i18n@11.2.3`. `vimeo` deps + `@config` alias removed. |
| `.gitignore` | `.lighthouseci/` ignored (transient lhci output, added 2026-05-21) |

### 4.4 Source — SCSS (post-cleanup 2026-05-08)
| File | Role |
|---|---|
| `src/scss/main.scss` | entry; only `@use` lines: `abstracts/variables`, `abstracts/mixins`, `abstracts/theme`, `base` |
| `src/scss/abstracts/_index.scss` | forwarded everywhere via `additionalData` (variables + mixins; theme not forwarded) |
| `src/scss/abstracts/_variables.scss` | colors **(8 families re-emitted in `oklch()` 2026-05-20)** incl. accent + ORCID + `--clr-youtube-red`, breakpoints, typography map (small tier bumped) |
| `src/scss/abstracts/_mixins.scss` | parameterized `font-face($range)`, `cyberpunk-glow`, `min/max-media-query` |
| `src/scss/abstracts/_theme.scss` | `:root` token emit (incl. `--clr-orcid-bg/fg`) + shared `@keyframes` + `.element-flare` + `.icon-glyph` (translateY -0.18em) + `.ccs-glyph` (1.75em + -0.08em) + `.state-grid` (3×3 grid + state-grid-pulse keyframe) + `.hud-deco` (corners + watermark) + selection styles |
| `src/scss/base/_typography.scss` | font-face declarations + body baseline (`line-height: 1.6` unitless) |
| `src/scss/base/_global.scss` | global `:focus-visible` + `.sr-only` + `html { scroll-behavior: smooth; scroll-padding-top: 4.5rem }` + `prefers-reduced-motion` rule |

### 4.5 Source — Landing
| File | Role |
|---|---|
| `src/widgets/hud-nav.vue` | sticky nav with scroll-progress + active link (scaleX underline) + mobile drawer |
| `src/widgets/language-toggle.vue` | ADA-compliant locale dropdown (role=menu / menuitemradio); 44px height on max-md |
| `src/views/components/sections/hero.vue` | recruiter-grade hero copy + CTAs + tag-row (CCS+ORCID); visual delegated to `hero-visual.vue` |
| `src/views/components/sections/hero-visual.vue` | portrait + visual-meta frame; wraps in `<button>` that emits `open` to `UiImageViewer`; warmed via `use-warm-modal` |
| `src/views/components/sections/skills.vue` | categorized tech showcase with BrandIcon dispatch (13 brand IDs) |
| `src/views/components/sections/experience.vue` | vertical HUD timeline (5 entries); role title fs-500/700 |
| `src/views/components/sections/now-projects-section.vue` | state-model-driven cards; STARTED IN count-up; polymorphic root; date+TZ countdown (lazy-loaded) |
| `src/views/components/sections/faq.vue` | 6-item accordion (`landing.faq.*` keys); JSON-LD `FAQPage` graph; viewport-gated flares (lazy-loaded) |
| `src/views/components/sections/site-footer.vue` | brand + dynamic SYS // SIGNATURE manifest + heart-glyph signoff + contact + socials |
| `src/components/cookie-consent.vue` | Global consent banner (sibling to nav, not a section); GA4 `G-6M3P3M2HG5` + YouTube gate via `kyo:consent` localStorage |

### 4.6 Source — UI primitives + composables (refreshed 2026-05-20)
| File | Role |
|---|---|
| `src/components/ui/card.vue` | bordered container |
| `src/components/ui/link.vue` | `<a>` with `variant` (`primary`\|`secondary`\|`ghost`\|`card`\|`cyber`\|`cyber-outline`) |
| `src/components/ui/button.vue` | `<button>` mirroring `UiLink` API (minus `card`) |
| `src/components/ui/image.vue` | wraps `BlastImage` |
| `src/components/ui/icon.vue` | SVG icon wrapper |
| `src/components/ui/icon-sprite.vue` | inlined SVG sprite loader (alternate icon strategy) |
| `src/components/ui/section-heading.vue` | minimal accessible heading |
| `src/components/ui/section-header.vue` | index-tag + heading + subtitle composite |
| `src/components/ui/brand-icon.vue` | inline SVG brand logos via `import.meta.glob` |
| `src/components/ui/modal.vue` | dialog (sm/md/lg/full); focus-trap; ModalLockRegistry scroll lock |
| `src/components/ui/modal-loading.vue` | placeholder for async-loading modal chunk |
| `src/components/ui/state-grid.vue` | componentized 3×3 pulsing loader (was `.state-grid` utility) |
| `src/components/ui/hud-deco.vue` | componentized HUD corner labels + watermarks (was `.hud-deco` utility) |
| `src/components/ui/youtube-facade.vue` | static thumbnail + consent-gated YouTube activation |
| `src/components/ui/client-only.vue` | SSR hydration guard |
| `src/components/ui/image-viewer.vue` | modal-driven portrait viewer + zoom |
| `src/components/blast-image.vue` | image element (wrapped by `UiImage`) |
| `src/composables/use-language.js` | locale state |
| `src/composables/use-click-outside.js` | click-outside detector |
| `src/composables/use-clickable-card.js` | Enter/Space card activation |
| `src/composables/use-image-ready.js` | image-load detection hook + directive |
| `src/composables/use-in-viewport.js` | IntersectionObserver wrapper |
| `src/composables/use-image-manifest.js` | image metadata + dimensions lookup |
| `src/composables/use-obfuscated-email.js` | post-mount mailto: href assembly |
| `src/composables/use-project-countdowns.js` | worker lifecycle + visibility pause/resume |
| `src/composables/use-prose-links.js` | `v-prose-links` directive — hardens external `<a>` |
| `src/composables/use-seo-head.js` | OG/Twitter/hreflang/canonical via `@unhead/vue` |
| `src/composables/use-structured-data.js` | JSON-LD graph injection (person/site/FAQ/profile/videos) |
| `src/composables/use-warm-modal.js` | preloads modal chunk + decoded bitmaps on hover/focus |
| `src/composables/use-youtube-warmup.js` | preloads YouTube iframe chunk + thumbnail |
| `src/i18n/{index,messages,detect-locale,raw-html-keys}.js` | vue-i18n setup; `raw-html-keys.js` grew with `landing.faq.*` + cookie strings |
| `src/data/{projects,snippets,data,error}.js` | translation source (~513-line `snippets.js`) + project list + state model + 22 TECHNOLOGIES |
| `src/seo/{routes.js,json-ld/*.js}` | route list for vite-ssg + canonical JSON-LD graphs |
| `src/workers/now-project.worker.js` | countdown worker (1 Hz tick) |
| `src/config/features.js` | feature flags |

> Vimeo facade was replaced by `youtube-facade.vue` 2026-05-16. The `vimeo.enabled` flag may still exist in `features.js` — verify on next visit; it's now legacy.

### 4.7 Source — assets
| File | Role |
|---|---|
| `src/assets/app/kyonax_multiverse_characters{,-100,-300,-600,-900}.{jpg,webp,avif}` | Hero portrait variants (re-encoded 2026-05-08 from IMG_6550) |
| `src/assets/brands/{x,next,vue,jest,tiktok,css,node,express,symfony,vite,nest,postgresql,mongodb,githubactions,ts,orcid}.svg` | 16 brand SVGs (Simple Icons-derived) |

### 4.8 CI workflows (`.github/workflows/`)
| File | Role |
|---|---|
| `ci.yml` | 8 jobs (eslint / precheck / vitest / vite-ssg build / size-limit / **lighthouse-ci** / security scan / protected files / pre-check label aggregator) on PR + push to main/vue-migration. Lighthouse step uses `continue-on-error: true` + always-run post-comment step + separate re-fail step for assertions (2026-05-21). |
| `deploy-to-build-dev.yml` | dev deploy |
| `deploy-to-build-main.yml` | main deploy |

### 4.9 Reference repositories + roam nodes
| Path | Role |
|---|---|
| `$REPO_OLD/` | pre-migration mirror (read-only) |
| `$REPO_RECKIT/` | canonical pattern reference (read-only) |
| `~/.brain.d/roam-nodes/2026-05-05-index_kyo_web_online.org` | this project's index dashboard |
| `~/.brain.d/roam-nodes/kyo_web_online/2026-05-05-vue_migration_plan.org` | this project's detailed migration node |
| `~/.brain.d/roam-nodes/reckit/{2026-04-17-reckit_architecture,2026-04-20-reckit_naming_conventions}.org` | reckit cross-references |

### 4.10 Auto-memory
| Path | Role |
|---|---|
| `/home/kyonax/.claude/projects/-run-media-kyonax-Da--Disk-dev-github-kyonax-kyo-web-online/memory/MEMORY.md` | memory index (new path after repo move; legacy path under `-home-kyonax-Documents-` may still exist) |
| `…/memory/feedback_repo_path_variable.md` | repo root is `$REPO/` (user works across devices) |
| `…/memory/feedback_concise_comments.md` | comments must be one short line max (added 2026-05-21) |
| `…/memory/reference_console_noise.md` | console SES + index.js TypeError are MetaMask content scripts (added 2026-05-21) |
| `…/memory/reference_hydration_whitespace.md` | vite-ssg `formatting: 'minify'` strips whitespace that CSR keeps; never mix static text + interpolation on multiple lines, never `\n` in i18n strings under `{{ t(...) }}` (added 2026-05-21) |

### 4.11 This session file
*   `/home/kyonax/.config/doom/gptel-directives/sessions/kyo-web-online.md` (canonical local path; the dot-files mirror under `dot-files/.config/doom-mac/gptel-directives/sessions/` may still exist for sync purposes)

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last (2026-05-21 marathon — Phase 8 complete + perf hardening + hydration fix)

A full-day session of substantive code changes that closed Phase 8, hit perfect Lighthouse scores, and resolved a long-running hydration mismatch. Eight threads in roughly this order:

**Thread 1 — Console errors resolved.** Confirmed via Network-tab absence of `lockdown-install.js` (extension content scripts bypass page network) + `moz-extension://` source URL on the `index.js:1:1108` TypeError → MetaMask. Documented in `docs/development.md` + `reference_console_noise.md` memory. No project code changed.

**Thread 2 — Lighthouse CI wired (Phase 8 final).** `@lhci/cli` devDep, `lighthouserc.json` (4 category gates + 4 metric gates, desktop preset, 3 runs per URL, `temporary-public-storage`), `check:lighthouse` npm script, CI job after `build` reusing `dist-${{ sha }}` artifact, added to `pre-check-label` `needs:`. **Plus** auto PR-comment via `scripts/lighthouse-comment.mjs` + 3-step CI sequence (`continue-on-error: true` on lhci → always-run comment posts/updates via marker `<!-- lighthouse-ci-comment -->` → separate re-fail step preserves the assertion failure as job outcome).

**Thread 3 — Forced reflow fix (143ms → 0ms).** Source-mapped `app-*.js:133:1226` to `site-footer.vue`'s `onMounted` reading `window.innerWidth/Height` synchronously during hydration. Replaced with rAF-deferred `onResize()` call.

**Thread 4 — Render-blocking CSS deferred (150ms → 0ms).** `scripts/defer-async-css.mjs` post-build pass rewrites non-`app-*.css` stylesheet tags to media-swap loading. Chained into `postbuild`. Entry CSS stays critical.

**Thread 5 — Vimeo fully removed + critical chain shortened (690ms → 105ms).** Deleted `src/config/features.js` (orphan), `@config` alias, `vimeoPreconnect` slot, `VIMEO_ENABLED/PRECONNECT` consts, stale Vimeo comment in `App.vue`. Added `SymbolsNerdFontMono-Regular` to font-preload list (parallel-fetched with hero fonts).

**Thread 6 — Unused-JS reduction (partial).** `@intlify/unplugin-vue-i18n@11.2.3` with `compositionOnly: true` saved 3 KB gzipped (142.84 → 139.84). Full `runtimeOnly: true` path would unlock ~8 KB more but requires migrating `snippets.js` → JSON + refactoring 2 SEO modules to use `i18n.global.t()` — deferred per user.

**Thread 7 — Hydration mismatch root-cause + fix (BP 96 → 100).** Surfaced details via `__VUE_PROD_HYDRATION_MISMATCH_DETAILS__: 'true'` + a temporary inline `console.warn` shim plugin (Vue 3 production warnings bypass `app.config.warnHandler`). Found two patterns broken by `vite-ssg formatting: 'minify'`: (a) `<p>\n  // {{ x }}\n</p>` compiles a leading space into VNode text that the minifier strips from SSR; (b) `\n` chars in `landing.footer.rights` translation get collapsed to spaces in SSR but kept by CSR. Fix: collapse milestone template to one line + replace `\n` with space in EN+ES rights strings. **All 4 Lighthouse categories now 100/100 on both locales.** Saved `reference_hydration_whitespace.md` memory. Lighthouserc BP threshold restored to 0.95.

**Thread 8 — NowProjects + Footer refinements (user-driven).** Footer mobile/tablet 2-row layout (`__top` `grid-template-columns: 1fr` at max-md; channels left, socials right via `text-align: right` + `justify-content: flex-end`). Ended-projects sort: `card.ended` wins over `NOW_STATUS_PRIORITY`. Ended-cards hover/flare use `--clr-warning-100` (matches the orange ENDED pill). Worker bug fix: `now-project.worker.js` now emits an `{ utc_ts: null, countdown: null }` entry for projects with no future deadlines (was `continue`-skipping them) so `card.ended` evaluates true and the ENDED label reliably renders.

### State of the repo as of this reset (2026-05-21)

*   **Branch:** `main`. Working tree has uncommitted changes across ~15+ files. User handles git themselves.
*   **CI:** 8 jobs (eslint / precheck / vitest / vite-ssg build / size-limit / **lighthouse-ci (NEW)** / security scan / protected files / pre-check-label). Phase 8 COMPLETE.
*   **Lighthouse:** **100/100/100/100** on both EN+ES. LCP ~580ms, FCP ~444ms, TBT 0, CLS 0. Critical chain 105ms (just `index.html → app.js`).
*   **Bundle baseline (gzipped):** main JS 139.84 KB (down from 142.84) / main CSS 10.17 KB / async chunks 1.9-8.64 KB. All within `size-limit` budgets.
*   **Build clean:** zero Sass deprecation warnings, zero hydration mismatches, zero forced-reflow attributions.
*   **DevDeps added today:** `@lhci/cli`, `@intlify/unplugin-vue-i18n@11.2.3`.
*   **Memory entries new today:** `feedback_concise_comments.md`, `reference_console_noise.md`, `reference_hydration_whitespace.md`. Deleted: `project_console_errors_pending.md`, `project_hydration_mismatch.md` (both resolved).

### Pending / Not yet started — priority-ordered

All major items shipped today. Remaining minor follow-ups (no urgency):

1.  **Pursue full vue-i18n `runtimeOnly: true` path.** ~8-10 KB more savings available. See §3.29 and §2.4 for the migration scope.
2.  **Re-import IMG_6550.png for lossless portrait variants** (`node scripts/convert-images.mjs --force`).
3.  **Architecture extraction.** 19 patterns ready (see §2.4 list — extended with today's additions).
4.  **Smoke test on real devices** — iPhone Safari + Chrome Android — focus on new footer mobile 2-row layout, ENDED-pill visibility, FAQ accordion, cookie banner, YouTube facade, OKLCH on Safari <16.4.
5.  **Stale task #16** ("change name.") — probably ignore.
6.  **Legacy script reconcile** — verify whether `migrate-*` scripts reappeared after 2026-05-08 sweep.

### Where to resume

**If the user says "deeper unused-JS savings":**
1. Read §3.29 for the migration plan. Three changes: migrate `snippets.js` → `snippets.json`, refactor `src/seo/json-ld/{person,faq-page}.js` to use `i18n.global.t()`, update `_lib.mjs` `loadTranslations()`. Flip `runtimeOnly: true` in `vite.config.js`.

**If the user reports a new hydration mismatch:**
1. Read `reference_hydration_whitespace.md` first — the two source-side rules cover most cases.
2. To capture detailed warnings: set `__VUE_PROD_HYDRATION_MISMATCH_DETAILS__: 'true'` in `vite.config.js` `define`, then add a transformIndexHtml plugin that injects a synchronous inline `<script>` BEFORE the module bundle wrapping `console.warn` to serialize `arguments[1]` via `outerHTML.slice(0, 1200)`. Reference implementation pattern in the memory entry.

**If the user reports a Lighthouse regression in CI:**
1. The PR comment auto-posts on every run with the scorecard. Read it first.
2. If it's an unused-JS regression: check whether new dependencies were added with heavy tree-shake costs.
3. If it's a render-blocking regression: check whether a new lazy section's CSS landed in `<head>` without going through the `defer-async-css.mjs` post-build pass.
4. If it's a Best Practices regression on `errors-in-console`: enable hydration mismatch details (see above).

**If the user reports an OKLCH parity bug on iOS Safari <16.4:**
1. Add `@supports not (color: oklch(0% 0 0))` fallback in `_theme.scss` with hex equivalents for brand-critical tokens.

**If the user wants to add a new project / tech / CTA / SFC:**
1. Same patterns as documented in §1 and §3. The footer/now-projects/hero structure is settled.

**If the user asks "where are we?":**
1. Read this section; then §2.2 for phase status (Phase 8 = DONE); then §2.4 for the remaining minor follow-ups.

**If the user asks about a specific architectural concern:**
1. Open the matching §3.x subsection. 3.26-3.31 are today's additions; 3.14-3.25 are 2026-05-16 → 2026-05-20.

**If the user wants to update memory** (`/memory`, "remember X"):
1. Save it under `/home/kyonax/.claude/projects/-run-media-kyonax-Da--Disk-dev-github-kyonax-kyo-web-online/memory/`. Cross-link from §1 if it's a session-scoped guideline.

---

### Archive — What was done in the previous reset (2026-05-08, Phase 8 dead-code sweep + CTA abstraction)

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

> Archive ends here. Pending Work and Where-to-Resume blocks for the current state live above in §2.4 and at the top of §5.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session. Newest row first. See `~/.claude/skills/session-reset/rules/activity-log.md` for the schema.

| Datetime         | Duration | Type            | Reference | Description |
|------------------|----------|-----------------|-----------|-------------|
| 2026-05-21 10:30 | 0.5h     | session-reset   | this      | Full reset after Phase-8 completion day. Updated §2.2 (Phase 8 DONE; +11 rows for today's work — Lighthouse CI, PR-comment, console-errors, forced reflow, render-blocking, Vimeo removal, critical chain, unused-JS, hydration mismatch, footer mobile, ended-sort, worker ended-state). Added §2.3 +5 decisions (#48-52: Lighthouse thresholds, PR-comment-always pattern, console.warn shim for Vue prod warnings, HTML-minifier whitespace trap, ended sort precedence). Replaced §2.4 (Phase 8 COMPLETE — only minor follow-ups). Added §3.26-3.31 covering today's work + updated §3.25c to RESOLVED. Refreshed §4.2/4.3/4.8/4.10. Replaced §5 with today's 8-thread narrative + new where-to-resume blocks. Prepended §6 with 12+ new activity rows. |
| 2026-05-21 10:15 | 0.25h    | bug-fix         | this      | Worker emits ended-state for projects with no future deadlines. `now-project.worker.js` `_hydrate_cache` was `continue`-skipping all-past projects → consumer saw `cd === undefined` → `card.ended = false` forever. Fix: track `last_past_label` + emit `{key, label: last_past_label, utc_ts: null}`; `_tick` branches on `utc_ts === null` to emit `{label, utc_ts: null, countdown: null}`. ENDED label now reliably renders. |
| 2026-05-21 10:00 | 0.25h    | refinement      | this      | Ended-projects: (a) new sort comparator pushes `card.ended === true` to bottom regardless of `NOW_STATUS_PRIORITY`; (b) new `&.is-ended` SCSS block overrides `--element-flare-color` + hover/focus `border-color` to `var(--clr-warning-100)` matching the ENDED pill. Placed at the end of `&__card` to win specificity tie vs `:not(.has-modal):hover`. |
| 2026-05-21 09:45 | 0.25h    | refinement      | this      | Footer mobile/tablet 2-row layout. `__top` at max-md → `grid-template-columns: 1fr` + `row-gap: 3rem`. `&__socials { text-align: right }` + `&__socials-list { justify-content: flex-end }`. Channels stack on row 2 (left), socials on row 3 (right). Desktop unchanged. |
| 2026-05-21 08:45 | 1h       | bug-fix         | this      | Hydration mismatch root-cause + fix (BP 96 → 100). Used `__VUE_PROD_HYDRATION_MISMATCH_DETAILS__: 'true'` + temporary inline-script transformIndexHtml plugin to wrap console.warn and serialize element outerHTML (Vue 3 prod warnings bypass app.config.warnHandler). Found vite-ssg's `formatting: 'minify'` strips/collapses whitespace that CSR keeps. Two patterns broken: (a) milestone `<p>\n  // {{ x }}\n</p>` → leading space stripped; (b) `landing.footer.rights` `\n` chars collapsed to spaces. Fixed both at source. Saved `reference_hydration_whitespace.md`; deleted resolved `project_hydration_mismatch.md`; restored BP threshold to 0.95. **All 4 Lighthouse categories now 100/100 on EN+ES.** |
| 2026-05-21 08:15 | 0.5h     | implementation  | this      | `@intlify/unplugin-vue-i18n@11.2.3` wired with `runtimeOnly: false, compositionOnly: true, fullInstall: true, strictMessage: false`. Main bundle 142.84 → 139.84 KB gzipped (-3 KB). Unused-JS 27.4 → 24.6 KiB. Deeper `runtimeOnly: true` path (~8-10 KB more) deferred — would require JSON migration + 2 SEO module refactor. |
| 2026-05-21 07:30 | 0.75h    | implementation  | this      | Vimeo fully removed + critical chain shortened (690ms → 105ms). Deleted `src/config/features.js`, `src/config/`, `@config` Vite alias, `VIMEO_ENABLED/PRECONNECT` consts, `vimeoPreconnect` slot in `vite.config.js` + `index.html`, `loadEnv` import, stale Vimeo comment in `App.vue`. Added `SymbolsNerdFontMono-Regular` to font-preload-injector (5 fonts preloaded). Lighthouse `uses-rel-preconnect` 0ms, FCP ~485ms → ~444ms. |
| 2026-05-21 07:00 | 0.5h     | implementation  | this      | Render-blocking CSS deferred (150ms → 0ms). `scripts/defer-async-css.mjs` post-build pass rewrites non-`app-*.css` stylesheet tags to media-swap loading + `<noscript>` fallback. Chained into `postbuild`. transformIndexHtml plugin attempt failed (runs before vite-ssg injects async stylesheets), post-build script is the right tool. |
| 2026-05-21 06:45 | 0.25h    | documentation   | this      | Saved `feedback_concise_comments.md` — comments must be one short line max, never multi-line blocks. User flagged a 5-line `/* ... */` comment as too verbose. |
| 2026-05-21 06:30 | 0.25h    | bug-fix         | this      | Forced reflow fix (143ms → 0ms). Source-mapped `app-*.js:133:1226` to `site-footer.vue` onMounted reading `window.innerWidth/Height` synchronously during hydration. Replaced with existing rAF-throttled `onResize()` call. `forced-reflow-insight` audit now n/a. |
| 2026-05-21 06:00 | 0.5h     | implementation  | this      | Lighthouse PR-comment script (`scripts/lighthouse-comment.mjs`). Reads `.lighthouseci/lhr-*.json` + `links.json`, computes median scores across 3 runs, emits markdown scorecard. CI step: `gh api PATCH` to update existing comment by marker `<!-- lighthouse-ci-comment -->`, else `gh pr comment` fresh. Marker-based in-place update prevents stacking on every push. |
| 2026-05-21 04:30 | 1.5h     | implementation  | this      | Lighthouse CI wired (Phase 8 final). `lighthouserc.json` (4 category gates + 4 metric gates, desktop preset, 3 runs/URL, `temporary-public-storage`), `@lhci/cli` devDep, `check:lighthouse` npm script, `Lighthouse CI` job after `build` reusing `dist-${{ sha }}` artifact with `continue-on-error: true` + post-comment step `if: always() && github.event_name == 'pull_request'` + separate re-fail step preserving assertion failure as job outcome. Added to `pre-check-label` `needs:`. `.gitignore` includes `.lighthouseci/`. |
| 2026-05-21 04:00 | 0.5h     | documentation   | this      | Console errors resolved (extension-sourced). Confirmed via Network-tab absence of `lockdown-install.js` (extension content scripts bypass page network) + `moz-extension://` source URL on `index.js:1:1108` TypeError → MetaMask. Wrote `docs/development.md` (expected console noise, verification flow, per-profile filter recipes). Saved `reference_console_noise.md`; deleted resolved `project_console_errors_pending.md`. |
| 2026-05-21 03:30 | 0.5h     | session-reset   | this      | Proper detailed reset post-ADA-round + size-limit + OKLCH contrast fix. Added §1.34a-e (contrast floor table, `<html lang>` watcher, `aria-label` fallback pattern, Vue warnHandler discipline, size-limit conventions), §2.2 +4 rows (OKLCH contrast fix / ADA round / size-limit gate / LCP verified), §2.4 restructured with console-error resolution as #1 priority, §3.25a-c new subsections (OKLCH+ADA / size-limit / console-error plan), §4.3 refreshed with `.size-limit.json` + warnHandler note, §5 fully replaced with 2026-05-21 narrative + new where-to-resume blocks, §6 prepended. |
| 2026-05-21 02:00 | 0.5h     | implementation  | this      | `size-limit` wired as 6th CI gate: `.size-limit.json` (7 budgets, gzipped, ~85-90% headroom), `check:size` npm script, devDeps `size-limit` + `@size-limit/file`, CI job after `build` reusing `dist-${{ sha }}` artifact, added to `pre-check-label` `needs:`. Also fixed 3 pre-existing lint errors in `hud-nav.vue` (commit 9097bb9 introduced them — `getElementById` → `querySelector`, 2 missing curly braces). Verified LCP preload tag already lands in both `dist/index.html` + `dist/es/index.html` under vite-ssg. |
| 2026-05-21 01:00 | 1.5h     | code-review     | this      | ADA-prioritized code review (4 parallel sub-agents). Resolved: 7 findings implemented (App.vue locale watcher for `<html lang>` sync, `<sup lang="ja">京</sup>`, modal aria-label fallback with `\|\| undefined`, youtube-facade aria-modal="true" matching its focus management, neutral-50 oklch 70.8%→76% for AA margin, use-project-countdowns reactive→ref, nested `<section>`→`<div role="region">`). 1 skipped (warning-100 already passes ~8.5:1, audit miscalculated). Vue warnHandler in main.js suppresses noisy `<Suspense>` experimental warning. |
| 2026-05-21 00:30 | 0.5h     | bug-fix         | this      | OKLCH palette contrast fix: `--clr-neutral-300` oklch L=55.2% → 70% (~3.2:1 → ~4.9:1, AA pass); `--clr-neutral-200` L=70.5% → 78% to maintain tier separation; removed `opacity: 0.55` on `.site-footer__manifest` (compounded ~2.2:1 on neutral-300, ~3.0:1 on neutral-200). Fixed Lighthouse contrast failure on `.skills__category-count` / `.skills__item-name` and 12 other neutral-300 callers. |
| 2026-05-20 14:00 | 0.5h     | session-reset   | this      | Targeted patch after 12-day gap: refreshed paths (Documents → run/media; later abstracted to `$REPO` variable), §2.2 scope (+OKLCH, vite-ssg, FAQ, modals, cookie, YouTube, SEO, error pages, ADA, security, image-viewer, componentized state-grid/hud-deco), §2.4 pending, §3.14-3.25 new subsections, §4 file index, §5 last-interaction, §6 prepended |
| 2026-05-20 00:05 | —        | commit          | 9097bb9   | fix(ui): OKLCH palette, nav scroll-detection, hero layout, card polish (PR #134) |
| 2026-05-17 23:12 | —        | commit          | 381747f   | fix: Performance Implementations (PR #133) |
| 2026-05-17 22:00 | —        | commit          | 6d0daba   | fix: Performance Improvements (PR #132) |
| 2026-05-17 21:00 | —        | commit          | 0f4115b   | fix: New Performance Improvements (PR #131) |
| 2026-05-17 20:00 | —        | commit          | 68aa4f6   | feat: Updating Data & Error Pages (PR #130) |
| 2026-05-17 19:00 | —        | commit          | 3e9e797   | feat: Error Pages (static prerendered 400/401/403/404/500 at public/error-pages/) (PR #129) |
| 2026-05-17 18:00 | —        | commit          | 64c7748   | fix: Improving SEO/AEO — JSON-LD graphs + use-structured-data + sitemap (PR #128) |
| 2026-05-16 17:00 | —        | commit          | 200874e   | feat(data): FastAPI experience bullet, deadline refresh, webcam2ascii i18n fix (PR #127) |
| 2026-05-16 16:00 | —        | commit          | c9b27a2   | fix: Security Scans (PR #126) |
| 2026-05-16 15:00 | —        | commit          | a9cdacf   | fix: checks (PR #125) |
| 2026-05-16 14:00 | —        | commit          | db3d2cc   | feat(performance): font subset, consent gate, lazy modals, ADA round (PR #123) |
| 2026-05-16 13:00 | —        | commit          | 132dc91   | feat(performance): Fix Hydration & Improve Performance — vite-ssg, @unhead/vue, ClientOnly, modal system (PR #123) |
| 2026-05-16 12:00 | —        | commit          | 1315de3   | fix: litespeed loop (PR #121) |
| 2026-05-16 11:00 | —        | commit          | 9124273   | Merge PR #119: vue-migration → develop (then to main same day) — landing redesign + project state model lands on main |
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
