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

**Compaction sources:** Sessions 1-5 (2026-04-08 to 2026-04-15 — brand bootstrap through v0.3 PR system), Session 6 (2026-04-16 to 2026-04-17 — overlay card fixes, modal abstraction, cam-log rename, Tier 1 file headers, ESLint Vue + naming conventions, brand-driven architecture refactor, Vite aliases, CONTRIBUTING.org, SECURITY.org, Doom Emacs config for Vue development).

**CRITICAL:** NEVER run `git commit`, `git push`, `gh pr create`, or any git write command. The user handles all git operations manually. Write to `COMMIT.org` and `PR.org` only.

---

## SECTION 1: GLOBAL GUIDELINES & DESIGN CONSTRAINTS

> **Apply these rules to every task in this session.** No domain skills are loaded for OBS automation specifically — these guidelines stage session-specific design and tech rules. PR / commit / CHANGELOG formatting rules live in the user's memory system and are auto-loaded. The pr-scribe skill handles PR body generation (Kyonax brand).

### 1.1 Visual Language

*   **Color palette:** Black (`--clr-neutral-500` = #000), gold (`--clr-primary-100` = #FFD700), white (`--clr-neutral-50` = #F2F2F2). Purple for license/version badges only. Red (`--clr-error-100`) for active recording state.
*   **Aesthetic:** Cyberpunk/sci-fi HUD — military-grade surveillance UI. Corner brackets and crosshairs (no rounded borders).
*   **HUD vocabulary:** SYS.LOG, REC FRAME, CAM ONLINE, REC MODE, SES::SceneName::Txx.

### 1.2 Technical Requirements

*   **OBS:** 32.1.1 via `obs-studio-browser` AUR. Canvas 1920x1080 @ 60fps. WebSocket on port 4455.
*   **Audio source:** `Mic/Aux` via OBS WebSocket `InputVolumeMeters` (NOT getUserMedia).
*   **Browser sources:** `http://localhost:5173/<brand>/<source>`. 1920x1080, 60fps, clear Custom CSS.
*   **Edit-minimization principle:** Every automation choice must reduce post-production work.

### 1.3 Project Architecture (Brand-Driven)

**(2026-04-17)** Each `@brand/` folder at the project root is the single source of truth for that brand. The Vue app auto-discovers brands via `import.meta.glob`. Adding a new brand requires zero app code changes.

**Vite aliases (all imports use these, zero relative `../` paths):**

| Alias | Maps to | Purpose |
|-------|---------|---------|
| `@shared` | `src/shared/` | Composables, components, widgets, utils, config, version |
| `@views` | `src/views/` | Landing page + its components/utils |
| `@app` | `src/app/` | SCSS architecture, fonts |
| `@assets` | `.github/assets/` | Logo and brand assets |

**Brand folder structure:**
```
@kyonax_on_tech/
  brand.js              metadata: handle, name, identity, colors, links
  sources.js            web source card data (hud/animation/scene types)
  hud/cam-log.vue       HUD-type web source
  animation/            Animation web sources (triggered in/out)
  scene/                Scene web sources (future)
  assets/               brand images, logos
  styles/_theme.scss    brand CSS custom property overrides
```

**Web source types:** `hud` (persistent HUD over camera), `animation` (triggered in/out), `scene` (full-screen transitions).

**What stays in `src/shared/`:** components (corner-bracket, hud-frame, status-indicator, recording-timer), widgets (audio-meter, live-readout), composables (WebSocket, recording, audio, scene), brand-loader.js, config.js, version.js.

**What stays in `src/views/`:** home.vue + its private components (overlay-card, base-modal, preview-modal, detail-modal) + utils (parse-emphasis).

### 1.4 Licensing & Governance

*   **Dual license:** MPL-2.0 (default) + Apache-2.0. Per-file headers. CI enforces headers.
*   **CODEOWNERS:** `* @Kyonax`. Branch protection pending.

### 1.5 SCSS Architecture (7-1 pattern)

Global theme at `src/app/scss/`. Brand overrides via CSS custom properties in `@brand/styles/_theme.scss` + JS runtime injection in `App.vue` from `brand.js` colors. All components consume `var(--clr-*)`, never SASS variables directly.

### 1.6 Code Style (ESLint — CCS standards)

*   Vue 3 **Composition API with `<script setup>`** — never Options API.
*   **Naming (enforced by `@typescript-eslint/naming-convention`):** `snake_case` variables/parameters, `camelCase` functions/methods, `UPPER_CASE` or `snake_case` constants, `PascalCase` classes/Vue imports, `kebab-case` filenames.
*   **ESLint parses `.vue` SFCs** via `eslint-plugin-vue` + `vue-eslint-parser`. Processor understands `<script setup>` template bindings.
*   **Type-aware linting on plain JS** via `@typescript-eslint/parser` + `tsconfig.eslint.json`.
*   Formatting: 2-space indent, single quotes, semicolons, 80 char max, trailing commas.
*   All imports use `@shared/`, `@views/`, `@app/`, `@assets/` aliases. Zero relative `../` paths.

### 1.7 Vue/Reactivity Rules

*   `ref` for arrays consumed by child `v-for` (not `shallowRef`).
*   Complex template expressions to `computed`. `<style lang="scss" scoped>` everywhere.

### 1.8 Version Management

*   `package.json` is canonical. `vite.config.js` injects `__APP_VERSION__`. `src/shared/version.js` exports `VERSION` + `VERSION_TAG`. UI imports from version.js.
*   Release: v0.1 (DONE), v0.3 (on `dev`, PR #2 open). Next: v0.4.

### 1.9 Overlay Card Data Format (`sources.js`)

Each `@brand/sources.js` exports an array. Fields: `id`, `type` (hud/animation/scene), `brand`, `name`, `description` (**bold** markers), `use_cases[]`, `path`, `width`, `height`, `fps`, `requires[]`, `triggers[]`, `status` (ready/planned). The `id` + `brand` + `type` determine the component path: `/@brand/type/id.vue`.

### 1.10 Tier 1 File Header Pattern

All root config/setup/org files use figlet smslant ASCII art with cyberpunk place names. Layout: license (separate block), figlet art, filename + date, description, TOC, optional guidelines/requirements, author/contact. See memory file `feedback_config_file_headers.md` for full spec and place name registry.

### 1.11 Brand Metadata Schema (`brand.js`)

Each `@brand/brand.js` exports: `handle` (e.g. `@kyonax_on_tech`), `name`, `description`, `identity` (`author`, `display_handle`), `links` (x, youtube, github), `colors` (optional CSS custom property overrides, keys use underscores: `primary_100` becomes `--clr-primary-100`).

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Build **RECKIT** — a capture-time OBS automation system with cyberpunk HUD overlays. Brand-driven architecture: each `@brand/` folder is self-contained. Primary metric: edit-minimization.

### 2.2 Scope

| Phase | Status |
|-------|--------|
| Phase 0-0.5: Design + Governance | **DONE** |
| Phase 3: Vue app + CAM-LOG + landing | **DONE v0.3 on `dev`** |
| Phase 3.5: CI + Vitest + PR template | **DONE** |
| Phase 3.6: Fixes, refinement, architecture | **IN PROGRESS on `feat-fixes-and-refinement-v3`** |
| Phase 4: Polish + packaging | NOT STARTED |

### 2.3 Key Decisions

Decisions 1-78 from Sessions 1-6 remain (see prior compaction). Session 6 continued additions:

79. **(2026-04-17)** Brand-driven architecture: each `@brand/` folder at project root is single source of truth. `brand-loader.js` auto-discovers via `import.meta.glob`. Dynamic routing from discovered sources. Centralized `overlays.js` deleted, replaced by per-brand `sources.js`.
80. **(2026-04-17)** Web source types: `hud` (persistent HUD), `animation` (triggered in/out), `scene` (full-screen). Type determines subfolder: `@brand/hud/`, `@brand/animation/`, `@brand/scene/`.
81. **(2026-04-17)** `src/brands/` directory deleted. `cam-log.vue` moved to `@kyonax_on_tech/hud/cam-log.vue`. Imports rewritten to `@shared/` aliases.
82. **(2026-04-17)** Landing page components (overlay-card, base-modal, preview-modal, detail-modal, parse-emphasis) moved from `src/shared/` to `src/views/` — they are not shared across brands, only used by the landing page.
83. **(2026-04-17)** Vite aliases: `@shared`, `@views`, `@app`, `@assets`. All imports use aliases, zero relative `../` paths in the codebase.
84. **(2026-04-17)** `brand.js` schema expanded: `identity` (author + display_handle), `links`, `colors` (full palette). `cam-log.vue` pulls identity from `getBrand()` instead of hardcoded strings.
85. **(2026-04-17)** `App.vue` applies brand CSS class (`.brand-kyonax-on-tech`) + inline style vars from `brand.js` colors when a brand route is active.
86. **(2026-04-17)** `CONTRIBUTING.org` created (the dojo). Setup, conventions, workflow, CI pipeline, PR process.
87. **(2026-04-17)** `SECURITY.org` expanded: banned code patterns table with ESLint rule mapping, banned file patterns, enforcement layers, environment variables section, contributor checklist.
88. **(2026-04-17)** `PULL_REQUEST_TEMPLATE.md` and `SECURITY.org` given Tier 1 headers (the scroll, the shield).
89. **(2026-04-17)** `.gitignore` refined: removed internal comments, added `*.gpg`, `*.sqlite`, `*.db`, `*.sql`, `.npmrc`, `.yarnrc`, `*.token`, `*.secret`, `.ssh/`, `CLAUDE.md`.
90. **(2026-04-17)** Doom Emacs: `set-indent-vars!` bug fix (Doom's `:lang web` module calls with 5 args instead of list), `after! web-mode` forces `.html` registration over tree-sitter fallback.

### 2.4 Pending Work

*   [ ] **IMMEDIATE:** Regenerate COMMIT.org and PR.org for the full branch diff.
*   [ ] **IMMEDIATE:** Push branch, open PR into `dev`.
*   [ ] Merge PR #2 (`dev to master`), tag `v0.3`, publish GitHub Release, configure branch protection.
*   [ ] Build `item-explain.vue` at `@kyonax_on_tech/animation/item-explain.vue`.
*   [ ] Shared widgets section on landing page.
*   [ ] Broaden Vitest coverage (26 baseline tests currently).
*   [ ] Fix YouTube subscriber badge subscribe link.
*   [ ] **OPEN SOURCE:** Contribute to `wallyqs/org-ruby` for GitHub alert syntax in `.org` files.
*   [ ] Phase 4 (packaging, scene collection export).

---

## SECTION 3: IMPLEMENTATIONS

### 3.1-3.6 Foundation (compressed — Sessions 1-5)

Identity, README, licensing stack, CI (6 jobs), ESLint, OBS environment. All stable and documented in prior compaction. Tier 1 headers applied to all root files.

### 3.7 Vue 3 App

**Created:** 2026-04-14 | **Last updated:** 2026-04-17
**Status:** Working. Brand-driven architecture.

**Stack:** Vue 3 + Vite 6 + Vue Router 4 + obs-websocket-js 5 + sass + eslint-plugin-vue + @typescript-eslint + Vitest 4.1.

**Entry chain:** `index.html` to `src/main.js` to `App.vue` (router-view + brand theme injection) to `src/router.js` (dynamic routes from `brand-loader.js`).

**Routes:** Auto-generated from `SOURCES` via `resolveComponent()`. Each source with a matching `.vue` file gets a route at `source.path`. Home at `/`, catch-all blank.

**Brand discovery:** `brand-loader.js` uses `import.meta.glob('/@*/brand.js')` and `import.meta.glob('/@*/sources.js')` for eager metadata loading, `import.meta.glob('/@*/hud/*.vue')` etc. for lazy component loading. Exports `BRANDS`, `SOURCES`, `getBrand()`, `resolveComponent()`.

### 3.8 @kyonax_on_tech Brand

**Created:** 2026-04-13 | **Last updated:** 2026-04-17
**Status:** Active. 1 HUD source ready, 1 animation planned.

**`brand.js`:** handle `@kyonax_on_tech`, identity (Cristian D. Moreno), full gold/black cyberpunk color palette, social links.
**`sources.js`:** CAM-LOG (hud, ready) + ITEM-EXPLAIN (animation, planned).
**`hud/cam-log.vue`:** HUD overlay at `/@kyonax_on_tech/cam-log`. Uses `getBrand()` for identity data.
**`styles/_theme.scss`:** CSS class `.brand-kyonax-on-tech` with full color overrides.

### 3.9 Landing Page (`src/views/`)

**Created:** 2026-04-15 | **Last updated:** 2026-04-17
**Status:** Production-ready. Components separated from shared.

`home.vue` + private components (overlay-card, base-modal, preview-modal, detail-modal) + utils (parse-emphasis). All in `src/views/`. Imports `SOURCES` from `@shared/brand-loader.js`. Brand tabs auto-generated. Grid: 1col mobile, 2col sm, 3col lg.

### 3.10 Tier 1 File Header System

**Created:** 2026-04-17 | **Status:** Applied to 13 files.

Place names: the void (.gitignore), the forge (vite.config.js), the precinct (eslint.config.mjs), the gate (index.html), the lab (.gitattributes), the vault (.env.example), the watchtower (ci.yml), the checkpoint (release.yml), the logs (CHANGELOG.org), the bridge (README.org), the pact (LICENSING.org), the law (CLAUDE.md), the dojo (CONTRIBUTING.org), the shield (SECURITY.org), the scroll (PULL_REQUEST_TEMPLATE.md).

### 3.11 ESLint Vue + Naming Convention

**Created:** 2026-04-17 | **Status:** Enforced on all JS and Vue files.

`eslint-plugin-vue` with `flat/essential` processor + `flat/strongly-recommended`. `@typescript-eslint/naming-convention` via `@typescript-eslint/parser` + `tsconfig.eslint.json` (`allowJs: true`). 26 tests passing, 0 lint errors.

### 3.12 Brand-Driven Architecture

**Created:** 2026-04-17 | **Status:** Complete. Auto-discovery working.

Replaced centralized `src/shared/data/overlays.js` + `src/brands/` with per-brand `@brand/` folders at project root. `brand-loader.js` discovers brands and sources at build time. Routes generated dynamically. Brand theme injected via `App.vue` (CSS class + inline style vars).

---

## SECTION 4: FILE INDEX

### Project Root

| Path | Association |
|---|---|
| `README.org` | the bridge |
| `CHANGELOG.org` | the logs |
| `CONTRIBUTING.org` | the dojo |
| `LICENSING.org` | the pact |
| `LICENSE` / `LICENSE-APACHE` / `NOTICE` | Legal |
| `eslint.config.mjs` | the precinct |
| `vite.config.js` | the forge (aliases + build + test) |
| `tsconfig.eslint.json` | ESLint-only TS config |
| `package.json` / `package-lock.json` | Dependencies + version |
| `index.html` | the gate |
| `.env.example` | the vault |
| `.gitignore` | the void |
| `.gitattributes` | the lab |

### Brand: @kyonax_on_tech/

| Path | Association |
|---|---|
| `@kyonax_on_tech/brand.js` | Brand metadata + identity + colors |
| `@kyonax_on_tech/sources.js` | Web source registry |
| `@kyonax_on_tech/hud/cam-log.vue` | CAM-LOG HUD overlay |
| `@kyonax_on_tech/styles/_theme.scss` | Brand CSS overrides |
| `@kyonax_on_tech/assets/` | Brand images |

### Vue App (src/)

| Path | Association |
|---|---|
| `src/main.js` | Vue bootstrap |
| `src/App.vue` | Root + brand theme injection |
| `src/router.js` | Dynamic routes from brand-loader |

### Landing Page (src/views/) — @views alias

| Path | Association |
|---|---|
| `src/views/home.vue` | Landing page |
| `src/views/components/overlay-card.vue` | Source card |
| `src/views/components/base-modal.vue` | Modal shell |
| `src/views/components/preview-modal.vue` | Iframe preview |
| `src/views/components/detail-modal.vue` | Full source details |
| `src/views/utils/parse-emphasis.js` | Bold marker parser |

### Shared (src/shared/) — @shared alias

| Path | Association |
|---|---|
| `src/shared/brand-loader.js` | Auto-discovery engine |
| `src/shared/brand-loader.test.js` | 26 tests |
| `src/shared/config.js` | OBS_CONFIG from env |
| `src/shared/version.js` | VERSION + VERSION_TAG |
| `src/shared/version.test.js` | Version tests |
| `src/shared/components/corner-bracket.vue` | SVG corner |
| `src/shared/components/hud-frame.vue` | HUD frame + labels |
| `src/shared/components/status-indicator.vue` | Blinking dot |
| `src/shared/components/recording-timer.vue` | REC + timer |
| `src/shared/composables/use-obs-websocket.js` | Singleton WebSocket |
| `src/shared/composables/use-recording-status.js` | Recording state |
| `src/shared/composables/use-audio-analyzer.js` | Audio levels |
| `src/shared/composables/use-scene-name.js` | Current scene |
| `src/shared/widgets/audio-meter.vue` | Audio visualizer |
| `src/shared/widgets/live-readout.vue` | Text readout |

### .github/

| Path | Association |
|---|---|
| `.github/assets/logo.txt` | ASCII logo (@assets alias) |
| `.github/workflows/ci.yml` | the watchtower |
| `.github/workflows/release.yml` | the checkpoint |
| `.github/CODEOWNERS` | @Kyonax review |
| `.github/SECURITY.org` | the shield |
| `.github/PULL_REQUEST_TEMPLATE.md` | the scroll |

---

## SECTION 5: LAST INTERACTION

### What was done last (2026-04-17 — brand-driven architecture refactor)

*   **Brand-driven architecture implemented.** Each `@brand/` folder is the single source of truth. `brand-loader.js` auto-discovers brands + sources via `import.meta.glob`. Dynamic routing from discovered sources. `src/brands/` and `src/shared/data/overlays.js` deleted.
*   **Vite aliases configured:** `@shared`, `@views`, `@app`, `@assets`. All imports converted from relative to alias-based. Zero `../` paths in the codebase.
*   **Landing page components separated** from `src/shared/` to `src/views/` (overlay-card, modals, parse-emphasis).
*   **@kyonax_on_tech brand filled:** `brand.js` with identity, colors, links. `_theme.scss` with full CSS overrides. `cam-log.vue` reads identity from `getBrand()`.
*   **CONTRIBUTING.org created.** Setup, conventions, workflow, CI, PR process.
*   **SECURITY.org expanded.** Banned patterns, banned files, enforcement layers, env vars, contributor checklist.
*   **All Tier 1 headers applied** to 15 files including SECURITY.org (the shield) and PULL_REQUEST_TEMPLATE.md (the scroll).
*   **26 tests passing, 0 lint errors, clean build.**

### Pending / Not yet started

*   [ ] **IMMEDIATE:** Regenerate COMMIT.org and PR.org for the full branch diff.
*   [ ] **IMMEDIATE:** Push branch, open PR into `dev`.
*   [ ] Merge PR #2, tag `v0.3`, publish GitHub Release, branch protection.
*   [ ] Build `item-explain.vue` at `@kyonax_on_tech/animation/item-explain.vue`.
*   [ ] Shared widgets section on landing page.
*   [ ] Broaden Vitest coverage.
*   [ ] Fix YouTube subscriber badge.
*   [ ] **OPEN SOURCE:** org-ruby alert syntax support.
*   [ ] Phase 4 (packaging, scene collection export).

### Where to resume

If the user asks to **commit / prepare commit text**: write to `COMMIT.org` only. NEVER run any git command.

If the user asks to **prepare the PR**: write to `PR.org` using pr-scribe (Kyonax brand). NEVER run `gh pr create`.

If the user asks to **build item-explain**: create `@kyonax_on_tech/animation/item-explain.vue`, update `sources.js` status to `'ready'`. Route auto-discovers.

If the user asks to **add a new brand**: create `@new_brand/` with `brand.js` + `sources.js` + type folders. Zero app code changes needed.

If the user asks to **add a new web source to an existing brand**: add entry to `@brand/sources.js`, create `.vue` in the matching type folder. Route auto-discovers.

If the user asks to **add a new root config file**: follow §1.10 (Tier 1 header pattern).

If the user asks to **add an ESLint rule**: add to both JS and Vue config blocks in `eslint.config.mjs`.

If the user asks for a **new task**: check §2.4 and `CHANGELOG.org` TODO block.

<!-- DESCRIPTION AND USER CONTEXT END -->



