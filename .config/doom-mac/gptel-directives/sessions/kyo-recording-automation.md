<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the **RECKIT** project (repo: `Kyonax/reckit`, local: `kyo-recording-automation`). It tracks the design, implementation, and session state for building a capture-time OBS automation toolkit with cyberpunk HUD overlays — built to eliminate post-production work across every Kyonax content creation brand. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read the sections in order on first load — after that, reference them by number as needed.

| Section                        | Purpose                                                                                              | When to reference                                                                       |
|--------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| **1. Global Guidelines**       | Design constraints, visual language rules, technical, code, governance requirements for ALL work.    | Before creating or modifying any overlay, script, or automation component. Mandatory.   |
| **2. Session Overview**        | High-level context: project scope, goals, current status, key decisions, and pending work.           | When starting a new task — understand scope and status first.                           |
| **3. Implementations**         | Per-component detail: what was built, file structure, decisions made, current state.                 | When resuming work on a specific component or reviewing what happened.                  |
| **4. File Index**              | Quick-reference of all relevant file paths: overlays, scripts, assets, configs, docs.                | When locating files or assets without searching.                                        |
| **5. Last Interaction**        | Short-term memory: what was done last, what's pending, where to resume.                              | At the very start of a new conversation — entry point for continuing work.              |

**Operational Rule:** Throughout the conversation, the user will give new tasks. **Always look for the last request, identified by a markdown title starting with `###`.** Any `###` title means it is the newest request.

**Key principle:** Data may appear in multiple sections with different framing. Section 1 frames it as a rule to follow. Section 2 frames it as context to understand. Section 3 frames it as an implementation to reference. This is intentional — each section answers a different question about the same knowledge.

**Compaction sources:** Session 1 (2026-04-08 → 2026-04-13), Session 2 (2026-04-13 → 2026-04-14 governance), Session 3 (2026-04-14 → 2026-04-15 Vue app + first overlay + landing), Session 4 (2026-04-15 landing refinements, widget abstraction, v0.3 release preparation), Session 5 (2026-04-15 PR system authored, CI hardening, Vitest suite, Pre-Check Failed label, v0.3 PR #1 merged into dev, PR #2 `dev → master` open).

---

## SECTION 1: GLOBAL GUIDELINES & DESIGN CONSTRAINTS

> **Apply these rules to every task in this session.** No domain skills are loaded for OBS automation specifically — these guidelines stage session-specific design and tech rules that may eventually graduate into a dedicated skill. PR / commit / CHANGELOG formatting rules live in the user's memory system (`~/.claude/.../memory/feedback_*.md`) and are loaded automatically — see §1.12 for the catalogue. Do not restate them here; reference them by name.

### 1.1 Visual Language

*   **Color palette:** Black (`--clr-neutral-500` = #000), gold (`--clr-primary-100` = #FFD700, plus `--clr-primary-50` = #FFE564 light, `--clr-primary-200` = darker), white (`--clr-neutral-50` = #F2F2F2). Purple (`--clr-secondary-*`) reserved for license/version badges only. Red (`--clr-error-100`) for active recording state.
*   **Aesthetic:** Cyberpunk/sci-fi HUD — military-grade surveillance UI. Clean, grid-aligned, monospaced typography. Corner brackets and crosshairs (no rounded borders).
*   **HUD vocabulary:** SYS.LOG, REC FRAME, CAM ONLINE, REC MODE, NODE LINK, SES::SceneName::Txx, MODULE ACTIVE, CORE INPUT, DATA STREAM.
*   **Recording state:** When OBS is recording, the `REC` text and status indicator (the dot) turn red (`--clr-error-100`). MODE: text and timer stay white.

### 1.2 Technical Requirements

*   **OBS:** version 32.1.1 via `obs-studio-browser` AUR package (CEF browser source + WebSocket built-in). Canvas 1920×1080 @ 60fps. WebSocket on port 4455 with auth.
*   **OBS audio source:** `Mic/Aux` is the canonical input the audio visualizer monitors. Audio data comes from OBS WebSocket `InputVolumeMeters` event (NOT Web Audio API getUserMedia — that doesn't work reliably in CEF). Each input has channels with `[magnitude, peak, input_peak]` — use index 1 (peak).
*   **Browser sources:** Local URL via Vite dev server (`http://localhost:5173/<brand>/<overlay>`). 1920×1080, 60fps, clear default Custom CSS, layer above webcam.
*   **Edit-minimization principle:** Every automation choice must reduce post-production work. Single ship/no-ship gate.

### 1.3 Repo & File Organization

*   **Public repo:** `github.com/Kyonax/reckit` (renamed 2026-04-15 from `kyo-recording-automation`; the local directory name still reflects the old name for now). Default branch: `master`. Active branch model: `master ← dev ← feature branches`.
*   **Per-brand folders** at repo root: `@kyonax_on_tech/`, `@is.kyonax/` (planned), each with `assets/` etc. (the original org for non-app reference material).
*   **Vue app source** at `src/`:
    *   `src/views/` — top-level routes (home.vue landing page)
    *   `src/brands/<brand-slug>/` — brand-specific overlays (e.g. `kyonax-on-tech/cam-person.vue`)
    *   `src/shared/components/` — reusable UI primitives (still need consumer wiring)
    *   `src/shared/widgets/` — brand-agnostic drop-in widgets (self-contained; theme via CSS custom properties)
    *   `src/shared/composables/` — reactive composables (WebSocket, audio, scene, recording)
    *   `src/shared/data/` — static data registries (overlays.js)
    *   `src/shared/config.js` — runtime config from `.env`
    *   `src/shared/version.js` — runtime version pipeline (see §1.9)
    *   `src/app/scss/` — SCSS architecture (7-1 pattern, see §1.6)
    *   `src/app/fonts/` — font binaries (SpaceMono only currently)
*   **`.github/assets/`** — toolkit-wide brand assets (ASCII logo `logo.txt`).
*   **Test files** live next to their source using the `*.test.js` suffix (see §1.13).

### 1.4 GitHub Org-Mode README Rendering

*   ASCII art centering: `<table align="center">` inside `<div align="left">`. Never `<div align="center">` wrapping `<pre>` — cascades `text-align` and breaks ASCII column alignment.
*   shields.io badges: extension-less SVG URLs only (no `.png`). Custom logos: hardcode `fill="color"` in SVG before base64-encoding (the `logoColor` param is unreliable for data-URI SVGs).
*   Wrap non-clickable badges in `<a href="#">` to prevent GitHub auto-image-popup.
*   README style: minimal. Contributing info as Node.js-style prose with inline links.

### 1.5 Licensing & Governance

*   **Dual license:** MPL-2.0 (default) + Apache-2.0 (permissive). SPDX: `(MPL-2.0 OR Apache-2.0)`.
*   Per-file headers declare which license. No header = MPL-2.0 default.
*   Every Vue/JS/SCSS/HTML source file **must** start with the MPL header comment containing copyright + author. The CI `License Headers` job enforces this on `*.js`, `*.mjs`, `*.html`, `*.css`, `*.vue`, `*.scss` by grepping the first 5 lines for `Cristian D. Moreno`.
*   `NOTICE` mandatory in all redistributions.
*   LICENSE files: copyright line only before standard text (GitHub `licensee` breaks on custom preambles).
*   **CODEOWNERS:** `* @Kyonax`. Branch protection NOT yet configured on GitHub — planned post-v0.3 release.

### 1.6 SCSS Architecture (7-1 pattern, mirrors kyo-web-online)

```
src/app/scss/
  abstracts/
    _variables.scss   ← $colors, $breakpoints, $typo-scale (granular x25 steps)
    _mixins.scss      ← font-face, media queries, cyberpunk-glow, hud-label-base
    _theme.scss       ← :root CSS custom properties via @each loops
    _index.scss       ← @forward "theme"; @forward "mixins";
  base/
    _typography.scss  ← @font-face for SpaceMono (4 weights)
    _global.scss      ← reset, html font-size 12px (overlay base), body, scrollbar
    _index.scss
  layout/_index.scss  ← (placeholder)
  components/_index.scss ← (placeholder)
  main.scss           ← @use "abstracts"; @use "base"; @use "layout"; @use "components";
```

*   **Variables → CSS custom props:** SASS `$colors` map → `:root { --clr-<color>-<shade> }` via @each. SASS `$typo-scale` → `--fs-<size>` per breakpoint (small/medium/large media queries).
*   **Granular type scale:** `$typo-scale` defines x25 steps (100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400, 425, 450, 475, 500, 525, 550, 575, 600, 625, 650, 675, 700, 725, 750, 775, 800) for fine-grained sizing.
*   **DRY mixin:** `@mixin hud-label-base` (in `abstracts/_mixins.scss`) — every HUD label across overlay + landing imports and includes this for consistent typography (font, weight, transform, letter-spacing, base color). Custom variations added per-instance.
*   **Brand theming:** Override `--clr-primary-*` at a container level to skin the entire app per brand. All components consume CSS custom properties (never SASS variables directly), enabling runtime theme swaps.
*   **Canvas dimensions** as CSS custom props: `--canvas-width: 1920px`, `--canvas-height: 1080px`, `--hud-bar-offset: 24px`, `--hud-bar-gap: 24px`.

### 1.7 Code Style (ESLint — CCS standards)

*   Vue 3 **Composition API with `<script setup>`** — never Options API.
*   Naming: **camelCase functions**, **snake_case variables/parameters**, **UPPER_CASE constants**, **PascalCase classes**, **kebab-case filenames**.
*   Formatting: 2-space indent, single quotes, semicolons, 80 char max, trailing commas on multiline.
*   Imports sorted by `simple-import-sort`: external packages first (no blank line between vue/obs-websocket-js/etc.), blank line, then local relative imports.
*   `prefer-const`, `no-var`, `eqeqeq always`, `curly always`, `prefer-template`, `prefer-arrow-callback`, `object-shorthand`.
*   No magic numbers (ignore -1, 0, 1, 2). Always extract to UPPER_CASE constants.
*   Security: no-eval, no-implied-eval, no-new-func, no innerHTML assignment, no document.write.
*   The `security/detect-object-injection` warnings on numeric loop indexes (e.g. `arr[i]`, `arr[ch]`) are FALSE POSITIVES — ignore them.
*   Composables: clean up listeners and timers in `onUnmounted`. Singleton pattern for shared resources (WebSocket).
*   Globals: `__APP_VERSION__` registered as readonly (see §1.9). Test files (`*.test.{js,mjs}` / `*.spec.{js,mjs}` / `__tests__/**`) gain Vitest globals (`describe`, `it`, `test`, `expect`, `beforeEach`, `afterEach`, `beforeAll`, `afterAll`, `vi`) and override `no-magic-numbers`, `no-console`, `max-len` off.

### 1.8 Vue/Reactivity Rules

*   For arrays consumed by `v-for` in child components, use `ref` (not `shallowRef`) — Vue's reactivity needs to detect array assignment, and `shallowRef + triggerRef` does not trigger child re-renders reliably across the props boundary. Use new array assignment per frame.
*   Complex template expressions → extract to `computed`. Never inline `padStart`, `String()`, or multi-step formatting in template bindings.
*   `<style lang="scss" scoped>` everywhere. To use shared mixins: `@use "../../app/scss/abstracts/mixins" as *;` (path relative to component).

### 1.9 Version Management

*   **Semver `0.x[.y]`** — git tag authoritative. Doc-side version strings live in `README.org` (`#+VERSION:` line, logo bottom border, shields.io badge) and `CHANGELOG.org`. UI-side version comes from one source.
*   **Runtime version pipeline** (single source of truth):
    1.  Bump `package.json` → `"version": "0.3.0"`
    2.  `vite.config.js` does `createRequire` on `./package.json` and registers `define: { __APP_VERSION__: JSON.stringify(pkg.version) }`
    3.  `src/shared/version.js` exports `VERSION` (full semver) and `VERSION_TAG` (`v<major>.<minor>`, strips patch)
    4.  Any UI that displays a version imports from `src/shared/version.js` (`import { VERSION_TAG } from '.../shared/version.js'`). Never hardcode the string.
    5.  `__APP_VERSION__` is declared as a readonly global in `eslint.config.mjs`
*   **Doc-side bumps still manual** (by design — GitHub `release.yml` workflow gates PRs on `#+VERSION:` change): `README.org` `#+VERSION:`, logo footer line, shields.io badge URL + alt all need updating for each release.
*   **Release history:** v0.1 (brand bootstrap, DONE) → v0.3 (Vue app + CAM-LOG + landing, **cut 2026-04-15, PR #2 `dev → master` still open awaiting merge + tag**). v0.2 folded into v0.3. Next: v0.4 (ITEM-EXPLAIN + additional overlays) → v1.0 (packaging, OBS scene collection export).
*   **Full release runbook** lives in the PERMANENT section of `COMMIT.org` (local, gitignored) — 14 commands from `npm version` through `gh release create`. Never overwrite that section on regeneration.

### 1.10 Naming & Documentation

*   Org files preferred over Markdown (exceptions: `PULL_REQUEST_TEMPLATE.md`, `CODEOWNERS` — GitHub system files).
*   CHANGELOG entries: bare facts only. No explanations or rationale inside bullets — decisions go in the `** Decided` sub-section with bolded titles (see `feedback_changelog_format.md`).
*   Concise, self-explanatory names. Abbreviations where standard (PR not PULL_REQUEST).

### 1.11 Overlay Card Data Format (`overlays.js`)

Every entry in `src/shared/data/overlays.js` must follow this structure. Split WHAT (prose) from WHEN (keywords) from technical prereqs (requires):

| Field | Required | Format | Purpose |
|-------|----------|--------|---------|
| `id` | Yes | kebab-case string | Unique key for `v-for` |
| `brand` | Yes | `@handle` string | Groups overlays under brand tabs |
| `name` | Yes | UPPER-KEBAB-CASE, 2 tokens max (e.g. `CAM-LOG`, `ITEM-EXPLAIN`) | Card title. Generic + self-explanatory. Use tactical/surveillance vocabulary (LOG, FEED, NODE, FRAME). Avoid naming after specific personas or content. |
| `description` | Yes | Prose, 2-4 sentences with `**bold**` markers on key terms | **WHAT** the overlay is and visually shows. Functional + design vibe. No em dashes (`—`). Use commas or periods instead. `**word**` marks important terms — rendered via `parseEmphasis()` into `<strong class="emphasis">` segments (no `v-html`). |
| `use_cases` | Yes | Array of short lowercase keywords (e.g. `['vlogs', 'tutorials', 'code walkthroughs']`) | **WHEN** to use this overlay. Short searchable tags rendered as chip tags under a `WHEN TO USE` label. Included in the landing page search haystack. |
| `path` | Yes | `/@brand/overlay-id` | Route in the Vue app |
| `width`, `height`, `fps` | Yes | Numbers | Canvas spec |
| `requires` | Yes | Array of short strings | Each line = one prerequisite (WebSocket port, audio source name, layer order, etc.) |
| `triggers` | Yes | Array | Declared triggers with `id`, `label`, `description`, `payload`. Empty `[]` if none. |
| `status` | Yes | `'ready'` or `'planned'` | Gates all action buttons. Planned = everything disabled. |

**Writing the prose fields:**
- Never use em dashes. Prefer comma or period.
- `description` answers "what does this render and what does it track?" — bold the nouns/verbs a reader should anchor on.
- `use_cases` answers "in which scenes belongs this?" — short keyword fragments, no full sentences.
- If a sentence feels like a technical requirement, move it to `requires[]` instead.

**Rendering:** `overlay-card.vue` shows the description paragraph with emphasis segments highlighted brighter, followed by a `WHEN TO USE` label and a chip tag list of `use_cases[]`. Every keyword is searchable via the landing page filter bar.

### 1.12 PR / Commit / CHANGELOG Format Rules (loaded from memory system)

These rules are **global across the user's repos** and live in `~/.claude/projects/.../memory/`. They are auto-loaded into every session. Reference them by name; never restate their content inside PR bodies, commit messages, or the CHANGELOG itself.

| Memory file | What it enforces |
|---|---|
| `feedback_no_emojis.md` | No emojis in any generated file, commit, PR, or response. Only `✅ ❌ ⚠️` allowed in Testing Coverage status cells. |
| `feedback_no_redundancy_in_pr_commit.md` | Each fact lives in exactly one section across PR body, commit text, and template scaffolds. |
| `feedback_pr_template_design_validated.md` | The verbose self-documenting PR template (inline `<!--` comments under each section) is the validated design — do NOT strip the guide comments when "simplifying". |
| `feedback_commit_org_permanent_section.md` | `COMMIT.org` has a `═══ PERMANENT ═══` marker. Everything below (the Version Bump Runbook) survives every regeneration verbatim. Only overwrite the content above. |
| `feedback_changes_list_format.md` | Every Changes-list entry: `**[TAG]** \`path\` — description`. Closed 4-tag vocabulary: `[NEW] / [MOD] / [DEL] / [MOV]`. |
| `feedback_changes_subsections.md` | 5 themed `###` subsections inside "What does this PR do?" in fixed order: Implementation → Release → CI & Tooling → Dependencies → Docs. Omit empty. |
| `feedback_subsection_inner_format.md` | Each of the 5 subsections has its own inner layout (group labels, version header, job sub-bullets, 4-bullet deps block, flat doc list). Implementation opens with a tag legend blockquote verbatim. |
| `feedback_supporting_section_formats.md` | Technical Details (Chose / Over / Why / Trade-off bullets). How to test (flow tree + numbered groups with `***Expected:***` at 6-space indent, never checkboxes). Special Deployment (numbered list with `CRITICAL` / `REQUIRED` / `OPTIONAL` severity). Documentation (`### <MEDIA-TYPE> — <Target>` with closed vocab: DESKTOP / TABLET / MOBILE / VIDEO / DIAGRAM / SCREENSHOT). NO arrow characters (`→`, `⇒`, `=>`, `➜`) anywhere in the body. |
| `feedback_changelog_format.md` | CHANGELOG release entries: `*Summary:*` hook, `***` sub-grouped Added, Changed / Removed / Decided with bolded decision titles, verbatim code, no arrows, no private-file mentions. |
| `feedback_concise_changelog.md` | CHANGELOG bullets are bare facts — decisions carry rationale, everything else does not. |
| `feedback_shieldsio_logo_color.md` | shields.io custom logos: hardcode fill color in SVG before base64-encoding; `logoColor` param unreliable. |
| `reference_obs_credentials.md` | Runtime OBS WebSocket credentials live GPG-encrypted in the repo (see §3.12). |

**Private-file discipline:** the PR body, commit text, CHANGELOG, README, and the PR template **must never** reference these gitignored files: session notes, `COMMIT.org`, `PR.org`, `.rc.gpg`, `.rc`, `.env`, `.github/BRANCHES.org`. The session file itself IS one of these private surfaces and may reference them freely for continuity, but anything headed to the public repo scrubs them.

### 1.13 Testing Infrastructure

*   **Runner:** Vitest 4.1.x with `happy-dom` and `@vue/test-utils`. Reuses `vite.config.js` (one config for both Vite and Vitest) — no separate `vitest.config.js`.
*   **Test file naming:** `*.test.{js,mjs}` or `*.spec.{js,mjs}`, colocated next to the source they test (e.g. `src/shared/version.test.js` sits beside `src/shared/version.js`). `__tests__/**` also recognized.
*   **Vitest globals enabled** via `test.globals: true` — no import needed in test files. ESLint knows about them (see §1.7).
*   **Script surface:**
    *   `npm run test` — CI entrypoint; `vitest run` (no `--passWithNoTests` — failure if no tests found).
    *   `npm run test:watch` — local development.
    *   `npm run test:coverage` — adds v8 coverage with HTML + text reporters.
*   **Coverage excludes:** `src/main.js`, `src/App.vue`, `src/router.js`, and the test files themselves.
*   **Current coverage:** 17 tests across 2 files (`version.test.js` = 3, `overlays.test.js` = 14). See §3.15 for details; broader coverage is tracked as a follow-up in `CHANGELOG.org` TODO under `INFRASTRUCTURE > TESTING`.

### 1.14 Stale Dev-Server Discipline

Vite caches its config at startup. If a dev server stays running from a previous config snapshot, new imports (like `@vitejs/plugin-vue` or a newly-added plugin) will appear "missing" even though they're on disk. When an error claims a plugin is "not installed" and the plugin clearly IS installed:

1.  Check for stale processes: `pgrep -af vite` or `ss -tlnp | grep 5173`.
2.  Kill them: `fuser -k 5173/tcp` or kill by PID.
3.  Restart: `npm run dev`.

Never leave a `vite &` background process alive across sessions. If a tool call or script starts one, kill it before ending the session.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Build **RECKIT** (**R**ealtime · **E**dit-free · **C**apture · **K**yonax · **I**ntegrated · **T**oolkit) — a capture-time OBS automation system with cyberpunk HUD overlays. Scoped for every Kyonax content creation brand (`@kyonax_on_tech`, `@is.kyonax`, `CCS`, future channels). Primary metric: edit-minimization.

**Cross-reference:** `~/.brain.d/roam-nodes/content_creation/2025-02-10-content_creation_kyonax_on_tech.org`

### 2.2 Scope

| Phase                                  | Type       | Summary                                                              | Status          |
|----------------------------------------|------------|----------------------------------------------------------------------|-----------------|
| Phase 0: Inspiration & Design          | Design     | Reference images, visual language, layout planning                   | **DONE**        |
| Phase 0.5: Documentation & Governance  | Docs       | README.org, CHANGELOG, licensing, CI, branch protection              | **DONE** (branch protection still pending — see 2.4) |
| Phase 1: Static Overlay Design         | Design     | Base HUD overlay frames (HTML/CSS or SVG)                            | **MERGED INTO PHASE 3** |
| Phase 2: Dynamic Overlays              | Dev        | JS-driven dynamic elements (clock, status, audio vis)                | **MERGED INTO PHASE 3** |
| Phase 3: OBS Integration               | Dev        | Vue 3 app + cam-person overlay + landing + WebSocket integration     | **DONE v0.3 on `dev` (2026-04-15); PR #2 `dev → master` open awaiting merge + tag** |
| Phase 3.5: CI & Governance Hardening   | Dev / Docs | Self-documenting PR template, Vitest suite, Pre-Check Failed label   | **DONE** (landed on `dev` via same v0.3 PR) |
| Phase 4: Polish & Packaging            | Refinement | Final adjustments, documentation, OBS scene collection export        | NOT STARTED     |

### 2.3 Key Decisions (Session-Wide)

1.  **(2026-04-08)** Project initialized; 4 inspiration images.
2.  **(2026-04-13)** Edit-minimization scoped as primary success metric.
3.  **(2026-04-13)** README converted to org-mode.
4.  **(2026-04-13)** Project renamed to **RECKIT** (acronym defined).
5.  **(2026-04-13)** ASCII HUD logo redesigned with figlet **Bloody** font.
6.  **(2026-04-13)** Per-brand folder structure: `@handle/` at root.
7.  **(2026-04-13)** Dual license MPL-2.0 + Apache-2.0. NOTICE mandatory.
8.  **(2026-04-13)** CCS SVG logo at `ccs-devhub/.github/assets/logo.svg`.
9.  **(2026-04-13)** Semver 0.x[.y]. v0.1 = brand bootstrap.
10. **(2026-04-14)** GitHub CI: `ci.yml` + `release.yml`. Branch model master ← dev ← feature.
11. **(2026-04-14)** ESLint: CCS standards for browser JS.
12. **(2026-04-14)** shields.io `logoColor` unreliable for data-URI SVGs — hardcode fill.
13. **(2026-04-14)** Badge colors: gold (metadata + CCS), purple (licenses + version).
14. **(2026-04-14)** GitHub `licensee` requires clean LICENSE files.
15. **(2026-04-14)** OBS `obs-studio-browser` AUR installed — provides CEF browser source.
16. **(2026-04-14)** OBS WebSocket on 4455 confirmed; credentials encrypted at `.rc.gpg` and staged into `.env` (VITE_ prefix).
17. **(2026-04-14)** Branch `websocket-cam-person` for first overlay + Vue app.
18. **(2026-04-14)** App architecture: **single Vue 3 app, route-based per brand**, not multi-page or per-brand projects. Vite + Vue Router + Composition API + script setup.
19. **(2026-04-14)** App location: `src/` at repo root. Brand folders inside `src/brands/`.
20. **(2026-04-14)** Audio source: OBS WebSocket `InputVolumeMeters` event, NOT `getUserMedia`. Doesn't work in CEF + uses OBS's actual mixer levels.
21. **(2026-04-14)** Audio targeting: default to `Mic/Aux` input. Skip Desktop Audio (zeroes when nothing playing) and browser sources (empty levels array).
22. **(2026-04-14)** Audio peak extraction: use `inputLevelsMul[ch][1]` (peak), NOT magnitude (index 0) or input_peak (index 2).
23. **(2026-04-14)** Audio gain: 8x multiplier (mic peaks at 0.04-0.07, needs amplification to fill 0-255 visualizer scale).
24. **(2026-04-14)** SCSS: replicated kyo-web-online's 7-1 architecture exactly. Same `$colors` map (HSL gold/black/white). SpaceMono fonts copied from kyo-web-online.
25. **(2026-04-14)** Granular type scale: x25 steps from 100 to 800.
26. **(2026-04-14)** Vue reactivity: `shallowRef + triggerRef` does NOT trigger child component v-for re-renders across props boundary. Use `ref` with new array assignment per frame.
27. **(2026-04-14)** WebSocket composable: singleton pattern.
28. **(2026-04-14)** `EventSubscription.InputVolumeMeters` is high-volume — must be explicitly opted-in via `eventSubscriptions: All | InputVolumeMeters`.
29. **(2026-04-14)** Take counter (T01, T02, ...) increments on `RecordStateChanged` `OBS_WEBSOCKET_OUTPUT_STARTED`. Resets on page reload.
30. **(2026-04-14)** Scene name in HUD top-right: `SES::<sceneName>::T<NN>` format. Updates via `CurrentProgramSceneChanged`.
31. **(2026-04-15)** Landing page at `/` — index of all browser sources with copy-URL buttons, brand tabs, sticky meta header.
32. **(2026-04-15)** Logo loaded into landing via Vite `?raw` import from `.github/assets/logo.txt` — single source of truth, no duplication.
33. **(2026-04-15)** `_global.scss` body: NO `overflow: hidden`, NO fixed canvas dimensions. Black background, full-height. Overlay components set their own 1920×1080 dimensions.
34. **(2026-04-15)** Trigger system: each overlay declares a `triggers` array in `overlays.js`. Landing renders OPEN + trigger buttons that postMessage to the preview modal's iframe.
35. **(2026-04-15)** Production triggers come from OBS WebSocket events; postMessage is dev-only.
36. **(2026-04-15)** When overlay `status === 'planned'`, ALL action buttons are disabled with tooltip.
37. **(2026-04-15)** Preview opens in a MODAL (not popup). Iframe scale computed via direct CSS custom property mutation on `window.resize` — bypasses Vue reactivity to avoid feedback loops.
38. **(2026-04-15)** Landing container max-width 1440px. Overlay cards: `repeat(auto-fill, minmax(min(100%, 340px), 1fr))` → 3 cols desktop, 1 col mobile.
39. **(2026-04-15)** Filter bar: text input (name, size, cache, requirements, keyword tags) + status chips. Chip dot brand-gold only on active chip.
40. **(2026-04-15)** Overlay description supports `**bold**` markers. Parsed by `parseEmphasis()` via `String.prototype.matchAll` — rendered as `<strong class="emphasis">`. No `v-html`.
41. **(2026-04-15)** `use_case` prose replaced with `use_cases[]` short keyword tags. Chip-styled, lowercase, searchable.
42. **(2026-04-15)** Brand-agnostic widgets split into `src/shared/widgets/`. `src/shared/components/` keeps the primitives that still need wiring.
43. **(2026-04-15)** `audio-meter.vue` owns `useAudioAnalyzer` internally. API: `:obs` + `source_name` props, single `update:state` emit. Old `audio-visualizer.vue` deleted.
44. **(2026-04-15)** `live-readout.vue` accepts `text` + `refresh_ms`. Samples the prop at the interval when `refresh_ms > 0`.
45. **(2026-04-15)** Version centralization: `package.json` is the single source of truth; Vite `define: { __APP_VERSION__ }`; `src/shared/version.js`; UI imports `VERSION_TAG`.
46. **(2026-04-15)** v0.3 cut — Vue app + CAM-LOG + landing + widgets shipped under one version bump. v0.2 skipped.
47. **(2026-04-15)** GitHub repo renamed `kyo-recording-automation → reckit`. Default branch `master`. Local directory still uses the old name.
48. **(2026-04-15)** Security audit + comprehensive `.gitignore` rewrite: editor / IDE / OS junk, secret-file extension bans (`*.pem`, `*.key`, `id_rsa*`, etc.), `!.env.example` negation (template tracked), `package-lock.json` un-ignored (required by `npm ci`).
49. **(2026-04-15)** `COMMIT.org` + `PR.org` introduced as **local gitignored clipboard buffers** for the most recent commit / PR text. `COMMIT.org` embeds a PERMANENT Version Bump Runbook below a visible marker; that section survives every regeneration.
50. **(2026-04-15)** PR template rewritten against the Madison-Reed reference shape (`MadisonReed/mr#20512`): 13 enforced rules in a top `<!--` block, 5 Changes subsections (Implementation / Release / CI & Tooling / Dependencies / Docs), per-subsection inner formats, bold-italic colon labels, 6-space indent on `***Expected:***`, numbered lists in How-to-test + Special Deployment (never checkboxes outside top Checklist), no arrow characters anywhere. Rules catalogued in §1.12 and stored in the user's memory system.
51. **(2026-04-15)** Changes-list format standardized: `**[TAG]** \`path\` — description` with closed 4-tag vocabulary `[NEW] / [MOD] / [DEL] / [MOV]`. Tag legend blockquote sits under `### Implementation` (once per PR).
52. **(2026-04-15)** Tooling decision: Vitest over Jest (reuses Vite pipeline, zero-config Vue SFC handling). `happy-dom` over `jsdom` (faster; Vitest default). `vite.config.js` hosts both Vite and Vitest configs.
53. **(2026-04-15)** CI trigger widening: removed `branches: [master, dev]` filter so every `pull_request` runs CI regardless of target (a sub-PR into `feat-cam-person` is gated the same as one into `dev`). Added `push` trigger for `feat-*`, `feat/**`, `feature/**`, `fix-*`, `fix/**`. Concurrency group keyed on `head_ref || ref` with `cancel-in-progress: true` dedups push-vs-PR double runs.
54. **(2026-04-15)** New CI job `Pre-Check Label` — aggregates every prior gate's `needs:` result and toggles the GitHub label `Pre-Check Failed` via `gh pr edit`. `if: always()` so it runs even after a hard failure. Requires `pull-requests: write` + `issues: write` permissions (added to workflow).
55. **(2026-04-15)** Protected Files job expanded from 4 files to 6 tiered categories with contextual notes per category (Legal / Licensing, Governance, Supply Chain, CI / Security Config, Build / Config, Release Artifact). PR comment rendered with `###` per category; release PRs still warn about CHANGELOG/README (expected) but under the "Release Artifact" label.
56. **(2026-04-15)** License Headers CI job widened to `*.vue` and `*.scss`; emits `[MISSING HEADER] <path>` per hit and a summary block at the end.
57. **(2026-04-15)** Security Scan CI job: `--exclude=eslint.config.mjs` (config's own rule messages contain the banned-pattern strings literally), `--exclude-dir={node_modules,dist,.cache}` guards, `::error file=,line=::` annotations + `[category] file:line :: content` log lines.
58. **(2026-04-15)** Merge strategies standardized: feature → dev via `--rebase` (preserves the commit story linearly); dev → master via `--merge` (creates an unambiguous release marker on master's first-parent history).
59. **(2026-04-15)** Tag format: `vX.Y` (no patch segment during the 0.x era) — matches the runtime `VERSION_TAG` and the shields.io badge. `vX.Y.Z` reserved for true patch releases.
60. **(2026-04-15)** No-arrows rule: `→`, `⇒`, `=>`, `➜` banned in any generated content (PR body, commit, CHANGELOG, template). Only ASCII box-drawing chars (`├─ └─ │ ─`) allowed inside the optional How-to-test flow tree. Use italic labels, em dashes, or line breaks instead.
61. **(2026-04-15)** No-emojis rule: zero emojis in generated files, commits, PR text, workflow output, or assistant responses. The only exception is `✅ ❌ ⚠️` inside Testing Coverage / Quality Gates status cells where they are single-cell status glyphs.
62. **(2026-04-15)** Stale-Vite-server lesson: never leave a `vite &` process alive across tool calls. A config change doesn't propagate to a running instance; "plugin not installed" errors on a verified-installed plugin are the usual symptom.
63. **(2026-04-15)** CHANGELOG release-entry format: `*Summary:*` hook + `***` sub-grouped Added + Changed / Removed / Decided with bolded decision titles. Applied retroactively to the `[v0.3]` entry. Rules in `feedback_changelog_format.md`.
64. **(2026-04-15)** PR #1 (`websocket-cam-person → dev`) MERGED via rebase on 2026-04-15T18:19:46Z — landed the full v0.3 diff on `dev`.
65. **(2026-04-15)** PR #2 (`dev → master`, title `release: v0.3`) opened; 7/7 checks green at time of last reset. Awaits merge + tag.

### 2.4 Pending Work

**Immediate (finish v0.3 release):**
*   [ ] Merge PR #2 (`dev → master`) via `gh pr merge 2 --repo Kyonax/reckit --merge`.
*   [ ] Tag master with `v0.3` and push the tag.
*   [ ] Publish a GitHub Release at `v0.3` with the `[v0.3]` CHANGELOG section as notes.
*   [ ] Configure branch protection on `master` + `dev` (require status checks: all 7 CI + Release Checks; require PR; require up-to-date).

**Bugs (carry-over from CHANGELOG TODO):**
*   [ ] YouTube subscriber badge needs direct subscribe link.

**Landing page (carry-over from CHANGELOG TODO):**
*   [ ] **NEXT FOCUS after v0.3 tag:** Add a common / shared widgets section to the landing page (`home.vue`) alongside WEB SOURCES. List `audio-meter`, `live-readout`, and future drop-ins with descriptions, prop tables, usage examples. Reuse the `.overlays-grid` layout; add a lightweight `widget-card.vue` that mirrors `overlay-card.vue` minus URL/copy/preview.

**Next overlays (carry-over from CHANGELOG TODO):**
*   [ ] Build `item-explain.vue` at `src/brands/kyonax-on-tech/item-explain.vue`. Listen for `postMessage` with `action: 'show' | 'hide' | 'cycle'`. Flip `status: 'planned' → 'ready'` in `overlays.js` once done.

**Testing (carry-over from CHANGELOG TODO):**
*   [ ] Broaden Vitest coverage beyond the 17 baseline tests. Priority order: composables (`use-recording-status`, `use-audio-analyzer`, `use-scene-name`), widgets (`audio-meter`, `live-readout`), landing-page computed logic (search filter, brand tabs, preview modal scaling).

**Future overlays (open ideas):**
*   [ ] Scene transition overlays
*   [ ] Notification / popup overlay
*   [ ] Lower thirds / chyrons

**Phase 4:**
*   [ ] OBS scene collection export (final packaging)
*   [ ] Documentation polish

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 RECKIT Identity & Logo

**Created:** 2026-04-13 | **Last updated:** 2026-04-14
**Status:** Complete (v0.1)

Name + acronym + ASCII HUD logo (`.github/assets/logo.txt`, figlet **Bloody** font, 15 lines, version in bottom border, CRT-noise texture). CCS shields.io badge with base64-embedded SVG (`fill="#9B8200"` hardcoded), `flat-square`, links `github.com/ccs-devhub`. Version footer bumped to `░v0.3` at 2026-04-15.

### 3.2 README.org

**Created:** 2026-04-13 | **Last updated:** 2026-04-15
**Status:** Complete (v0.3 header).

org metadata → centered ASCII logo (`<table align="center">` in `<div align="left">`) → acronym tagline → badges → `* WHAT IS RECKIT?` → `** CONTRIBUTING`. `#+VERSION:`, ASCII logo footer, and shields.io version badge all synced to `v0.3` at 2026-04-15.

### 3.3 Licensing Stack

**Created:** 2026-04-14 | **Status:** Complete (v0.1)

`LICENSE` (MPL-2.0), `LICENSE-APACHE` (Apache-2.0), `LICENSING.org` (guide), `NOTICE` (attribution). `package.json`: `"(MPL-2.0 OR Apache-2.0)"`.

### 3.4 CI & Governance

**Created:** 2026-04-14 | **Last updated:** 2026-04-15
**Status:** Full gate suite on `dev`. Branch protection still not configured on GitHub.

**Workflow:** `.github/workflows/ci.yml` — 6 jobs triggered on `pull_request` (any target) + `push` on feat/fix branches, deduplicated by a concurrency group.

| Job | Role |
|---|---|
| `ESLint` | `npm ci && npm run lint` — CCS standards on all `*.{js,mjs,ts}` minus `node_modules` / `dist`. |
| `Security Scan` | greps for `eval(`, standalone `Function(`, `.innerHTML =`, `document.write(`, setTimeout/setInterval with string args, hardcoded secrets, `http://` URLs. Excludes `eslint.config.mjs` (config's own rule messages) + `node_modules` / `dist` / `.cache`. Emits `::error file=,line=::` annotations. |
| `License Headers` | greps first 5 lines of every `*.js`, `*.mjs`, `*.html`, `*.css`, `*.vue`, `*.scss` for `Cristian D. Moreno`. Echoes `[MISSING HEADER] <path>` per miss + end-of-run summary. |
| `Protected Files` | tiered warning for changes to Legal / Governance / Supply Chain / CI / Build / Release-artifact files. Posts an `###`-categorised PR comment with a contextual note per category. Advisory only (no block). |
| `Unit Tests` | `npm ci && npm run test` (Vitest). Currently 17 tests, all passing. |
| `Pre-Check Label` | Aggregator job at the end. `needs: [eslint, security-scan, license-headers, unit-tests]`, `if: always()`. Reads each `needs.*.result`; toggles the GitHub label `Pre-Check Failed` via `gh pr edit --add-label` / `--remove-label`. |

**Workflow permissions** (top-level): `contents: read`, `pull-requests: write`, `issues: write`.

**Release workflow:** `.github/workflows/release.yml` — runs on `dev → master` PRs only. Gates on `README.org` `#+VERSION:` actually changing vs `master` and `CHANGELOG.org` being updated. Passes for PR #2.

**Governance files:** `CODEOWNERS` (`* @Kyonax`), `SECURITY.org` (banned patterns reference), `PULL_REQUEST_TEMPLATE.md` (see §3.17), `BRANCHES.org` (local-only, gitignored).

**Branches on origin:** `master` (default, at v0.1 pre-PR#2), `dev` (has v0.3). Feature branch `websocket-cam-person` was merged + deleted on 2026-04-15.

### 3.5 ESLint Configuration

**Created:** 2026-04-14 | **Last updated:** 2026-04-15
**Status:** Complete, in active use.

`eslint.config.mjs` — CCS standards for browser JS. Plugins: `@eslint/js`, `import`, `jsdoc`, `simple-import-sort`, `unicorn`, `security`. All rules in §1.7. Globals: `...globals.browser` + `__APP_VERSION__: 'readonly'`. Test-file override block adds Vitest globals (§1.13) and loosens `no-magic-numbers` / `no-console` / `max-len`.

### 3.6 OBS Environment

**Created:** 2026-04-14 | **Status:** Ready for development.

`obs-studio-browser` 32.1.1 AUR (CEF + WebSocket). Canvas 1920×1080 @ 60fps. Audio inputs: `Desktop Audio` (pulse_output_capture) + `Mic/Aux` (pulse_input_capture, monitored). WebSocket on 4455; password GPG-encrypted (§3.12).

**Recommended OBS quality settings:** Lanczos downscale, 60fps, 1920×1080; MKV recording, NVENC HEVC, CQP @ CQ 18-20, keyframe interval 2, preset Quality, profile high; NV12, sRGB, **Full** range; Browser source 1920×1080, 60fps, custom CSS empty, **shutdown when not visible OFF**, **refresh on scene active OFF**; **Enable Browser Source Hardware Acceleration ON**.

### 3.7 Vue 3 App

**Created:** 2026-04-14 | **Last updated:** 2026-04-15
**Status:** Working. Dev server on 5173.

**Stack:** Vue 3 + Vite 6 + Vue Router 4 + obs-websocket-js 5 + sass + Vitest 4.1 + `@vue/test-utils` 2.4 + happy-dom 20.9.

**Entry chain:** `index.html` → `src/main.js` → `App.vue` (just `<router-view />` + global SCSS) → `src/router.js`.

**Routes:**
- `/` → `views/home.vue`
- `/@kyonax_on_tech/cam-person` → `brands/kyonax-on-tech/cam-person.vue`
- (planned) `/@kyonax_on_tech/item-explain` → `brands/kyonax-on-tech/item-explain.vue`

**Config from `.env`:** `VITE_OBS_WS_HOST`, `VITE_OBS_WS_PORT`, `VITE_OBS_WS_PASS`, `VITE_OBS_WS_LAN`. Loaded via `import.meta.env` in `src/shared/config.js`. `Object.freeze()` to prevent mutation.

#### Composables

| File | Role |
|------|------|
| `use-obs-websocket.js` | **Singleton** OBS WebSocket connection. Auto-reconnect. Subscribes to `EventSubscription.All | InputVolumeMeters`. Returns `{obs, connected, error, disconnect}` to all callers. |
| `use-recording-status.js` | Listens to `RecordStateChanged`. Exposes `is_recording`, `elapsed_time` (HH:MM:SS), `record_state`, `take_count`. |
| `use-audio-analyzer.js` | Listens to `InputVolumeMeters`. Defaults to `Mic/Aux`. GAIN=8, BAR_COUNT=16. Used internally by the `audio-meter` widget now — no direct consumer outside that. |
| `use-scene-name.js` | Listens to `CurrentProgramSceneChanged`. Returns `{scene_name}`. |

#### Shared Components (primitives — need wiring by consumer)

| File | Role |
|------|------|
| `corner-bracket.vue` | SVG corner (top-left/right, bottom-left/right). Props: `position`, `size`, `stroke_width`. `currentColor`. |
| `hud-frame.vue` | 4 corner brackets + 4 labels + slot. Border lines hidden. |
| `status-indicator.vue` | Blinking dot. Red when `active`, dimmed white otherwise. CSS `step-end`. |
| `recording-timer.vue` | `<status-indicator>` + `<rec>` + `<mode>` + `<time>`. Only REC + dot red when recording; MODE + time stay white. |
| `overlay-card.vue` | Landing card per overlay. Header (brand tag + name + status badge) in 2-col grid, description with `**bold**` parser, `WHEN TO USE` chip list from `use_cases[]`, spec grid (2-col), URL + COPY, REQUIRES, PREVIEW row. All buttons disabled when `status === 'planned'`. Overflow guards: `overflow: hidden`, `min-width: 0`, `word-break: break-word`. |
| `preview-modal.vue` | Teleport-to-body modal. Iframe scaled via CSS custom prop `--iframe-scale` computed on `window.resize`. RELOAD + trigger buttons in footer. ESC + click-outside close. |

#### Shared Widgets (self-contained, brand-agnostic, drop-in)

See §3.13.

### 3.8 cam-person.vue Overlay

**Created:** 2026-04-14 | **Last updated:** 2026-04-15
**Status:** Production-ready on `dev`. Will reach `master` when PR #2 merges.

**Path:** `src/brands/kyonax-on-tech/cam-person.vue`
**URL:** `http://localhost:5173/@kyonax_on_tech/cam-person`
**Canvas:** 1920×1080, transparent.

**Layout:**
- Top-left: `SYS.LOG` + `REC FRAME`
- Top-right: `SES::<sceneName>::T<NN>` + `CAM ONLINE`
- Bottom-left: identity — `Cristian D. Moreno` / `@KYONAX_ON_TECH`
- Bottom-right: `RECKIT {{ VERSION_TAG }}` (from `src/shared/version.js`)
- Center: crosshair
- Bottom center: `<recording-timer>` + `<audio-meter>` widget
- Above status bar: `<live-readout>` showing `WS:… | AUDIO:… | L0:…`, 200ms refresh

**Composables:** `useObsWebsocket`, `useRecordingStatus`, `useSceneName`. Audio is encapsulated inside `<audio-meter>` so `useAudioAnalyzer` is no longer imported directly.

**Widgets:** `<audio-meter :obs :source_name="Mic/Aux" @update:state="audio_state = $event" />`, `<live-readout :text="debug_text" :refresh_ms="200" />`.

#### Key Decisions (cam-person specific)

| Decision | Date | Rationale |
|---|---|---|
| Audio via InputVolumeMeters | 2026-04-14 | CEF can't use getUserMedia; mixer-accurate levels |
| Mic/Aux default | 2026-04-14 | Desktop Audio zeroes; browser sources empty |
| 8× gain | 2026-04-15 | Mic peaks ~0.07; needs amplification |
| `ref`, not `shallowRef` | 2026-04-15 | `shallowRef + triggerRef` does not propagate to child v-for across props boundary |
| Take counter resets on page reload | 2026-04-14 | Browser source refresh = new session |
| REC + dot red; MODE + time white | 2026-04-15 | Visual distinction between recording state and idle/highlight |
| Dimensions in `em` | 2026-04-14 | Scales with root font-size |

### 3.9 home.vue Landing Page

**Created:** 2026-04-15 | **Last updated:** 2026-04-15
**Status:** Production-ready on `dev`.

**Path:** `src/views/home.vue`
**URL:** `http://localhost:5173/`
**Layout:** sticky meta bar → ASCII logo + tagline → Quick Setup list → WEB SOURCES (brand tabs + filter bar + card grid) → footer. Container `max-width: 1440px`, dark background via `.home-bg` wrapper.

**Sticky meta bar (`.home-meta-bar`):** `sticky; top:0; z-index:50`, `background: rgba(0,0,0,0.85) + backdrop-filter: blur(10px)`, bleeds via `margin: 0 -5em`. Metrics: SOURCES, BRANDS, READY, CANVAS.

**ASCII logo:** Vite `?raw` import of `.github/assets/logo.txt`. Centered block with left-aligned text so columns align.

**Brand tabs:** auto-generated from `OVERLAYS` (computed `brands` Map).

**Filter bar:** text input (search haystack covers name, `width x height`, `CACHE DISABLE`, `requires[]`, `use_cases[]`) + status chips (ALL / READY / PLANNED). Chip dot brand-gold only on active chip. Empty state: "No web sources match the current filter."

**Overlay card grid:** `repeat(auto-fill, minmax(min(100%, 340px), 1fr))`, `gap: 2em`, `align-items: start`. 3 cols desktop, 1 col mobile.

#### Key Decisions (landing page)

| Decision | Date | Rationale |
|---|---|---|
| Single landing at `/` | 2026-04-15 | Index of all sources for copy-paste into OBS |
| Brand tabs auto-generated | 2026-04-15 | Adding a new brand to overlays.js creates the tab |
| ASCII logo via Vite ?raw | 2026-04-15 | Single source of truth at `.github/assets/logo.txt` |
| Sticky meta bar w/ backdrop blur | 2026-04-15 | Persistent reference while scrolling |
| Max-width 1440 (was 1080) | 2026-04-15 | Fits 3-column card grid |
| Preview via modal (not popup) | 2026-04-15 | Stays in landing flow; iframe scaled on `window.resize` |
| Search + status chips | 2026-04-15 | Name, size, cache, requires, tags all searchable |
| `**bold**` emphasis | 2026-04-15 | Anchor key terms; XSS-safe via `matchAll` |
| `use_cases[]` keyword tags | 2026-04-15 | Searchable; scan better than prose |
| Trigger buttons via postMessage | 2026-04-15 | Test animated overlays without real OBS events |

### 3.10 overlays.js Registry

**Created:** 2026-04-15 | **Status:** 2 overlays declared (1 ready, 1 planned). Schema in §1.11. Contract-tested by `overlays.test.js` (§3.15).

```js
export const OVERLAYS = [
  { id: 'cam-person', brand: '@kyonax_on_tech', name: 'CAM-LOG',
    description: 'Futuristic **HUD** layered over the **direct-to-camera feed** …',
    use_cases: ['vlogs', 'tutorials', 'code walkthroughs', 'reaction segments', 'talking head', 'person to camera'],
    path: '/@kyonax_on_tech/cam-person', width: 1920, height: 1080, fps: 60,
    requires: ['OBS WebSocket enabled (port 4455)', 'Audio source named Mic/Aux', 'Browser source layered above webcam'],
    triggers: [], status: 'ready' },
  { id: 'item-explain', brand: '@kyonax_on_tech', name: 'ITEM-EXPLAIN',
    description: '**Lower-third overlay** for introducing items …',
    use_cases: ['product demos', 'tool showcase', 'concept intro', 'citation callout', 'lower third', 'scene reveal'],
    path: '/@kyonax_on_tech/item-explain', width: 1920, height: 1080, fps: 60,
    requires: ['OBS WebSocket enabled (port 4455)'],
    triggers: [
      { id: 'show', label: 'SHOW', payload: { action: 'show', title: '…', description: '…', image: '' } },
      { id: 'hide', label: 'HIDE', payload: { action: 'hide' } },
      { id: 'cycle', label: 'CYCLE', payload: { action: 'cycle', duration_ms: 5000 } },
    ], status: 'planned' },
];
```

### 3.11 Overlay Trigger Protocol

**Created:** 2026-04-15 | **Status:** Modal + UI complete; no overlay listens yet (item-explain not built).

Modal-preview flow: `overlay-card.vue` sets `pending_trigger = trigger.payload` → opens `<preview-modal>` → iframe `load` fires → dispatches `target.contentWindow.postMessage(payload, origin)` after `READY_DELAY_MS = 300` → emits `consume_trigger` so parent clears `pending_trigger`. Modal footer has manual trigger buttons that `fire(trigger)` directly.

**Payload schema:** `{ action: 'show', title, description, image }` · `{ action: 'hide' }` · `{ action: 'cycle', duration_ms }`.

**Production note:** postMessage is dev-only. Production triggers come from OBS WebSocket events.

### 3.12 .env / Credentials Flow

**Created:** 2026-04-14 | **Status:** Active.

- `.rc.gpg` — GPG-encrypted source of truth (gitignored, `gpg --decrypt .rc.gpg`). Contains `OBS_WS_HOST`, `OBS_WS_PORT`, `OBS_WS_PASS`, `OBS_WS_LAN` plain values. GPG key: `DE30C73B784BB341`.
- `.env` — Vite-loaded runtime values, `VITE_` prefix. Gitignored.
- `.env.example` — committed template (the `!.env.example` negation in `.gitignore` makes this possible).
- `src/shared/config.js` — reads `import.meta.env.VITE_*` and exports frozen `OBS_CONFIG`.

### 3.13 Shared Widgets (`src/shared/widgets/`)

**Created:** 2026-04-15 | **Status:** 2 widgets in production.

Folder for **brand-agnostic, self-contained UI blocks**. Distinct from `src/shared/components/` (primitives). Both widgets color-theme via `--clr-primary-100`.

**`audio-meter.vue`** — encapsulates OBS audio visualization end-to-end.
- Props: `obs` (required), `source_name` (default empty = first active input), `bar_count` (default 16).
- Owns `useAudioAnalyzer` internally.
- Emits `update:state` with `{ active, source, peak }` on every analyzer frame.
- Replaced old `components/audio-visualizer.vue`.

**`live-readout.vue`** — generic HUD text readout with optional refresh throttle.
- Props: `text` (string), `refresh_ms` (number, default 0).
- `refresh_ms === 0` → render reactively. `refresh_ms > 0` → sample the `text` prop at that interval via `setInterval`; `clearInterval` on `onUnmounted`.
- No positioning / layout / font-size built in.

### 3.14 Version Centralization

**Created:** 2026-04-15 | **Status:** Active, used by home.vue + cam-person.vue.

Pipeline table (see §1.9 for rule; also covered in `COMMIT.org` PERMANENT runbook):

| Step | File | Role |
|---|---|---|
| 1 | `package.json` | Canonical semver. Bump to release. |
| 2 | `vite.config.js` | `createRequire` reads `package.json`; `define: { __APP_VERSION__: JSON.stringify(pkg.version) }`. |
| 3 | `eslint.config.mjs` | `__APP_VERSION__` as readonly global. |
| 4 | `src/shared/version.js` | `VERSION` (full semver) + `VERSION_TAG` (`v<major>.<minor>`). |
| 5 | UI | Imports `VERSION_TAG` (home.vue header, cam-person toolkit-id). |

Doc-side strings (manual, gated by `release.yml`): `README.org` `#+VERSION: vX.Y`, ASCII footer `░vX.Y`, shields.io badge URL + alt. `CHANGELOG.org` gets a new `[vX.Y]` entry.

### 3.15 Vitest Test Infrastructure

**Created:** 2026-04-15 | **Status:** 17 tests baseline, all passing. Follow-up coverage tracked in CHANGELOG TODO.

**Config:** in `vite.config.js`'s `test` block (see §1.13).

**Test inventory:**

| Path | Tests | Covers |
|---|---|---|
| `src/shared/version.test.js` | 3 | `VERSION` semver shape, `VERSION_TAG` derivation (`v<major>.<minor>`), leading `v`. |
| `src/shared/data/overlays.test.js` | 14 | registry non-emptiness, unique ids, required-field schema (12 fields), `status ∈ {ready, planned}`, `use_cases[]` is `string[]`, `path === /${brand}/${id}`, canvas dims are positive integers, no em dashes in description. |

**Import pattern in tests:**
```js
import { describe, expect, it } from 'vitest';
import { VERSION, VERSION_TAG } from './version.js';
```

Globals enabled via `test.globals: true` — `import` is still required for the explicitness CCS wants.

### 3.16 Git Workflow + Release Flow (v0.3)

**Created:** 2026-04-15 | **Status:** PR #1 MERGED, PR #2 OPEN (awaiting merge + tag).

**Branches:** `master` (default, at v0.1 pre-PR#2) ← `dev` (at v0.3) ← feature branches.

**Cut path for v0.3:**

```
websocket-cam-person
  (rebase, PR #1 MERGED 2026-04-15T18:19:46Z)
  dev
    (merge commit, PR #2 OPEN — this is the release PR)
    master
      git tag -a v0.3
      gh release create v0.3
```

**Merge strategies (standardized):**

| Transition | Strategy | Reason |
|---|---|---|
| feature → dev | `--rebase` | Linear history; preserve the commit story |
| dev → master | `--merge` | Unambiguous release marker on master's first-parent history |
| Patch → anywhere | author's call | Case-by-case |

**Tag format:** `vX.Y` (no patch in the 0.x era).

**PR #1 scope:** the 3 feature commits on `websocket-cam-person` (`feat: v0.3`, `fix(ci)`, `feat(ci)`) plus the merge commit.

**PR #2 scope:** identical to PR #1 (same diff) plus the `chore: improve CHANGELOG layout and expand Protected Files coverage` commit that landed on `dev` post-PR-#1-merge. Same 47 files.

### 3.17 PR Template + Clipboard-Buffer System

**Created:** 2026-04-15 | **Status:** In active use for PR #2. Validated design (`feedback_pr_template_design_validated.md`).

**`.github/PULL_REQUEST_TEMPLATE.md`** — 13-rule self-documenting template. Top `<!--` block enumerates every enforcement rule; each `##` / `###` section has its own inline `<!--` comment explaining its scope, inner format, and ordering rules. The rules travel with every PR populated from the template.

**Sections (in order):**
1. `## Checklist (check if it applies)` — 13 items
2. `## What does this PR do?` — summary + user story + **Design / Reference** link + 5 themed `###` subsections
    - `### Implementation` — tag legend blockquote + bold group labels
    - `### Release` — version header + canonical triad
    - `### CI & Tooling` — flat Changes list; nested `**Job:**` bullets on workflow files with multiple fixes
    - `### Dependencies` — 4 mandatory bullets (Runtime / Dev / Upgraded / Removed) + optional Lockfile line
    - `### Docs` — flat Changes list
    - `### Technical Details` — Chose / Over / Why / Trade-off 4-field bullets
    - `### Testing Coverage` — 2-table block (Automated tests + Quality gates)
3. `## How to test this PR` — optional ASCII flow tree + numbered-list feature groups with `***Expected:***` at 6-space indent
4. `## Special Deployment Requirements` — numbered list with `CRITICAL` / `REQUIRED` / `OPTIONAL` severity
5. `## Documentation` — `### <MEDIA-TYPE> — <Target>` subsections

**`COMMIT.org`** (local, gitignored clipboard buffer):
- Top half (regenerable): Subject + `#+BEGIN_SRC gitcommit` body + Notes.
- `═══ PERMANENT ═══ DO NOT OVERWRITE ═══` marker block.
- Bottom half (PERMANENT — 14-step Version Bump Runbook). Never touched on regeneration.

**`PR.org`** (local, gitignored clipboard buffer):
- Title `#+BEGIN_SRC text` block.
- Body `#+BEGIN_SRC markdown` block containing the full PR description.
- Regenerated completely on every `/pr` request.

**To push updates to an existing PR:**
```bash
awk '/^#\+BEGIN_SRC markdown$/{flag=1; next} /^#\+END_SRC$/{flag=0} flag' PR.org \
  | gh pr edit <PR#> --body-file -
```

### 3.18 CHANGELOG.org Format

**Created:** 2026-04-15 (format rewrite) | **Status:** `[v0.1]` and `[v0.3]` entries conform. `[v0.3]` on `dev`, will reach `master` when PR #2 merges.

**Per-release template** (rules in `feedback_changelog_format.md`):

```org
* [vX.Y] — YYYY-MM-DD :: <Short Title>

*Summary:* One sentence.

** Added
*** <Subsystem>
- item
*** <Another subsystem>
- item

** Changed
- =file= — before, after

** Removed
- =file= — reason

** Decided
- *<Decision title>:* rationale
```

**TODO block at top** (categorized):
- `** BUGs [0/N]`
- `** FEATUREs / *** LANDING / *** WEBSOCKETs`
- `** INFRASTRUCTURE / *** TESTING`

---

## SECTION 4: FILE INDEX

### Project Root

| Path | Association |
|---|---|
| `README.org` | 3.2 — header at v0.3 |
| `CHANGELOG.org` | 3.18 — `[v0.3]` entry on dev |
| `LICENSE` / `LICENSE-APACHE` / `LICENSING.org` / `NOTICE` | 3.3 — protected (Legal) |
| `eslint.config.mjs` | 3.5 — protected (CI / Security) |
| `package.json` | 3.7, 3.14 — canonical version; protected (Supply Chain) |
| `package-lock.json` | 3.14 — **tracked**; protected (Supply Chain). Required by `npm ci` |
| `vite.config.js` | 3.7, 3.14, 3.15 — `define: { __APP_VERSION__ }` + Vitest config |
| `index.html` | 3.7 — Vite entry |
| `.env` | 3.12 — local only, gitignored |
| `.env.example` | 3.12 — committed template (via `!.env.example` negation) |
| `.rc.gpg` | **CRITICAL — local only, gitignored.** GPG-encrypted runtime config (key `DE30C73B784BB341`) |
| `.gitattributes` | UTF-8 + LF on logo.txt |
| `.gitignore` | Comprehensive; categorized by concern |

### Vue App (`src/`)

| Path | Association |
|---|---|
| `src/main.js` | Vue bootstrap |
| `src/App.vue` | Root (router-view + global SCSS) |
| `src/router.js` | Routes |
| `src/views/home.vue` | 3.9 — landing page |
| `src/brands/kyonax-on-tech/cam-person.vue` | 3.8 — CAM-LOG overlay |
| `src/shared/components/corner-bracket.vue` | 3.7 |
| `src/shared/components/hud-frame.vue` | 3.7 |
| `src/shared/components/status-indicator.vue` | 3.7 |
| `src/shared/components/recording-timer.vue` | 3.7 |
| `src/shared/components/overlay-card.vue` | 3.7, 3.9 — landing card |
| `src/shared/components/preview-modal.vue` | 3.7, 3.11 — iframe modal |
| `src/shared/widgets/audio-meter.vue` | 3.13 |
| `src/shared/widgets/live-readout.vue` | 3.13 |
| `src/shared/composables/use-obs-websocket.js` | 3.7 — singleton |
| `src/shared/composables/use-recording-status.js` | 3.7 |
| `src/shared/composables/use-audio-analyzer.js` | 3.7 — used by audio-meter widget |
| `src/shared/composables/use-scene-name.js` | 3.7 |
| `src/shared/data/overlays.js` | 3.10 |
| `src/shared/data/overlays.test.js` | 3.15 — 14 tests |
| `src/shared/config.js` | 3.12 |
| `src/shared/version.js` | 3.14 |
| `src/shared/version.test.js` | 3.15 — 3 tests |

### SCSS (`src/app/scss/`)

| Path | Association |
|---|---|
| `abstracts/_variables.scss` | 1.6 |
| `abstracts/_mixins.scss` | 1.6 |
| `abstracts/_theme.scss` | 1.6 |
| `abstracts/_index.scss` | 1.6 |
| `base/_typography.scss` | 1.6 |
| `base/_global.scss` | 1.6 |
| `base/_index.scss` | 1.6 |
| `layout/_index.scss`, `components/_index.scss` | Placeholders |
| `main.scss` | Entry point |

### Fonts (`src/app/fonts/SpaceMono/`)

| Path | Association |
|---|---|
| `SpaceMonoNerdFont-Regular.ttf` | 400 normal |
| `SpaceMonoNerdFont-Bold.ttf` | 700 normal |
| `SpaceMonoNerdFont-Italic.ttf` | 400 italic |
| `SpaceMonoNerdFont-BoldItalic.ttf` | 700 italic |

### .github/

| Path | Association |
|---|---|
| `.github/assets/logo.txt` | 3.1 |
| `.github/workflows/ci.yml` | 3.4 — protected (CI / Security) |
| `.github/workflows/release.yml` | 3.4 — protected (CI / Security) |
| `.github/CODEOWNERS` | 3.4 — protected (Governance) |
| `.github/SECURITY.org` | 3.4 — protected (Governance) |
| `.github/PULL_REQUEST_TEMPLATE.md` | 3.4, 3.17 — protected (Governance) |
| `.github/BRANCHES.org` | 3.4 — **local only, gitignored** |

### Local Clipboard Buffers (gitignored)

| Path | Association |
|---|---|
| `COMMIT.org` | 3.17 — regenerable body + PERMANENT runbook below marker |
| `PR.org` | 3.17 — fully regenerated per `/pr` request |

### Brand Folders (legacy / non-app)

| Path | Association |
|---|---|
| `@kyonax_on_tech/assets/*.{jpg,png}` | 4 inspiration images |
| `@is.kyonax/` | Planned, not created |

### External

| Path | Association |
|---|---|
| `ccs-devhub/.github/assets/logo.svg` | 3.1 — CCS SVG logo (separate repo) |
| `~/.brain.d/roam-nodes/content_creation/2025-02-10-content_creation_kyonax_on_tech.org` | Brand context |
| `~/.claude/projects/.../memory/feedback_*.md` | §1.12 — PR / commit / CHANGELOG format rules |

---

## SECTION 5: LAST INTERACTION

### What was done last (2026-04-15 — v0.3 release in flight)

*   **PR template + CHANGELOG + governance rewritten end-to-end.** Authored `.github/PULL_REQUEST_TEMPLATE.md` against the Madison Reed reference (`MadisonReed/mr#20512`) — 13 embedded enforcement rules, 5 fixed Changes subsections (Implementation / Release / CI & Tooling / Dependencies / Docs), per-subsection inner formats (Chose / Over / Why / Trade-off bullets for Technical Details; numbered groups with `***Expected:***` at 6-space indent for How-to-test; numbered `CRITICAL` / `REQUIRED` / `OPTIONAL` list for Special Deployment; `### <MEDIA-TYPE> — <Target>` for Documentation). Catalogued all rules in the user's memory system so they propagate to future sessions. Validated design (`feedback_pr_template_design_validated.md`) — don't strip the inline `<!--` guide comments when "simplifying".
*   **CHANGELOG.org format modernized.** Rewrote the `[v0.3]` entry: `*Summary:*` hook, `***` sub-grouped Added (Foundation / Styles / Composables / Components / Widgets / Overlays / Landing Page / CI & Tests), Changed / Removed / Decided with bolded decision titles. Fixed a stray arrow in the `[v0.1]` entry. Scrubbed session-file and `.env` references (private).
*   **v0.3 feature branch shipped to `dev`.** Three commits on `websocket-cam-person`: `feat: v0.3` (Vue app + CAM-LOG + landing + widgets + version pipeline + `.gitignore` hardening), `fix(ci)` (unblock ESLint + Security Scan + License Headers; added MPL headers to `index.html` + `eslint.config.mjs` + 10 SCSS partials), `feat(ci)` (Vitest + Pre-Check Failed label + broader triggers + concurrency group). PR #1 MERGED via rebase on 2026-04-15T18:19:46Z.
*   **Vitest suite wired up.** Reused `vite.config.js` (no separate `vitest.config.js`). Added `happy-dom`, `@vue/test-utils`, `vitest` dev deps. 2 test files / 17 tests baseline — `src/shared/version.test.js` and `src/shared/data/overlays.test.js` (contract test for the registry schema). Teach ESLint Vitest globals on test files.
*   **CI suite expanded.** New `Unit Tests` job. New `Pre-Check Label` aggregator job toggles the GitHub label `Pre-Check Failed` via `gh pr edit` reading every prior job's `needs.*.result`. Widened triggers — `pull_request` fires on any target; `push` fires on `feat-*` / `feat/**` / `feature/**` / `fix-*` / `fix/**`. Concurrency group `${{ github.workflow }}-${{ github.head_ref || github.ref }}` with `cancel-in-progress: true`. `Protected Files` job expanded from 4 files to 6 tiered categories with contextual notes.
*   **PR #2 opened (`dev → master`, `release: v0.3`).** All 7 checks green; mergeable CLEAN. Body regenerated twice with the new format, linked to Kyonax/reckit#1 for the file-by-file scope. Awaits merge + tag.
*   **Follow-up `chore:` commit on `dev`** — CHANGELOG layout rewrite + Protected Files expansion. Pushed to `origin/dev`; PR #2 picked it up automatically. No-emoji rule enforced retroactively on the workflow's category headers.
*   **Stale Vite dev server bug diagnosed + fixed.** `npm run dev` was hitting a background Vite instance from an earlier tool-call session; the stale process had a cached config predating `plugin-vue`. Resolution: kill the stale PID, restart. Logged as `§1.14`.

### Pending / Not yet started

*   [ ] **IMMEDIATE:** Merge PR #2 via `gh pr merge 2 --repo Kyonax/reckit --merge`. Then tag `master` with `v0.3` and push. Then `gh release create v0.3` with the `[v0.3]` CHANGELOG section as notes.
*   [ ] **IMMEDIATE:** Configure branch protection on `master` + `dev` in GitHub Settings — require all 7 CI checks + Release Checks, require PR, require branches up-to-date.
*   [ ] **NEXT FEATURE FOCUS:** Add a **shared widgets section** to `home.vue` alongside WEB SOURCES — list widgets from `src/shared/widgets/` with descriptions, props, usage examples. New `widget-card.vue` mirroring `overlay-card.vue` minus URL/copy/preview.
*   [ ] Build `item-explain.vue` at `src/brands/kyonax-on-tech/item-explain.vue`. Listen for postMessage with `action: 'show' | 'hide' | 'cycle'`. Flip `status: 'planned' → 'ready'` in `overlays.js`.
*   [ ] Broaden Vitest coverage: composables first (`use-recording-status`, `use-audio-analyzer`, `use-scene-name`), then widgets, then landing computed logic.
*   [ ] Fix YouTube subscriber badge subscribe link.
*   [ ] Phase 4 (packaging, scene collection export).

### Where to resume

If the user asks to **merge PR #2 + tag + release**: follow the sequence in `COMMIT.org`'s PERMANENT Version Bump Runbook (steps 10-14 — merge, sync master, tag, push tag, publish release). Or the condensed flow: `gh pr merge 2 --repo Kyonax/reckit --merge` → `git checkout master && git pull` → `git tag -a v0.3 -m "RECKIT v0.3" && git push origin v0.3` → `gh release create v0.3 --title "RECKIT v0.3" --notes-file <[v0.3] awk extract from CHANGELOG.org>`. **Verify before merging:** `gh pr checks 2 --repo Kyonax/reckit --watch` — wait for all 7 to be green.

If the user asks to **configure branch protection**: GitHub Settings → Branches → Add rule for `master` and `dev`. Required status checks: `CI / ESLint`, `CI / Security Scan`, `CI / License Headers`, `CI / Protected Files`, `CI / Unit Tests`, `CI / Pre-Check Label`; on `master` also `Release Checks / …`. Require PR; require approvals; require Code Owners; require branches up-to-date.

If the user asks to **add the shared widgets section to the landing page**: edit `src/views/home.vue`. Add a new `<section>` below WEB SOURCES titled `SHARED WIDGETS`. Source a static array (inline or a new `src/shared/data/widgets.js`) describing each widget (name, path, one-line description, prop table, emits, usage snippet). Reuse `.overlays-grid` layout; create `src/shared/components/widget-card.vue` mirroring `overlay-card.vue` aesthetics but drop URL/copy/preview. Optionally add a `WIDGETS` metric to the sticky meta bar.

If the user asks to **build item-explain**: create `src/brands/kyonax-on-tech/item-explain.vue` following the cam-person pattern. Composition API + `<script setup>`. Listen for postMessage:
```js
function handleMessage(event) {
  if (event.origin !== window.location.origin) return;
  const { action, ...data } = event.data || {};
  if (action === 'show') visible.value = true;
  // ...
}
onMounted(() => window.addEventListener('message', handleMessage));
onUnmounted(() => window.removeEventListener('message', handleMessage));
```
Update `overlays.js`: `status: 'planned' → 'ready'`. Add a test to `overlays.test.js` if the schema grows.

If the user asks to **add a new overlay**: (1) create `.vue` in `src/brands/<brand>/`; (2) add a route in `src/router.js`; (3) add an entry in `src/shared/data/overlays.js` with all required fields (§1.11). Landing page index updates automatically. `overlays.test.js` will enforce the schema contract.

If the user asks to **add a new brand**: create entries in `overlays.js` with the new `brand` field. Brand tabs auto-generate. Create `src/brands/<new-brand-slug>/`.

If the user asks to **release a new version**: follow the PERMANENT runbook in `COMMIT.org` — 14 steps from `npm version` through `gh release create`.

If the user asks for a **commit text**: regenerate the top half of `COMMIT.org` (subject + body + notes), preserve the PERMANENT runbook verbatim. Apply the Changes-list format and the 5-subsection order (§1.12 memory files).

If the user asks for a **PR text**: regenerate `PR.org` body following the 13-rule template. Use absolute `https://github.com/Kyonax/reckit/blob/master/…` URLs (relative paths 404 in PR body context). No private-file mentions. No arrows. No emojis outside status cells.

If the user asks to **change colors/theming**: edit `src/app/scss/abstracts/_variables.scss` ($colors map). Components consume `var(--clr-*)` so changes propagate automatically. Scope an override class on the brand root for per-brand theming.

If the user asks to **modify cam-person**: see §3.8. Composables in `src/shared/composables/`. Audio + debug are now `<audio-meter>` + `<live-readout>` widgets (§3.13).

If the user hits **a plugin-not-installed error on `npm run dev`**: check for stale Vite processes (§1.14): `pgrep -af vite` → kill → restart.

If the user asks for a **new task**: check §2.4 (Pending Work) and the `[TODO]` block at the top of `CHANGELOG.org`.

<!-- DESCRIPTION AND USER CONTEXT END -->


