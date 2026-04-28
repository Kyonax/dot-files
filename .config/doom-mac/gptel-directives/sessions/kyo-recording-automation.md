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

**Compaction sources:** Sessions 1-5 (2026-04-08 to 2026-04-15 — brand bootstrap through v0.3 PR system), Session 6 (2026-04-16 to 2026-04-17 — overlay card fixes, modal abstraction, cam-log rename, Tier 1 file headers, ESLint Vue + naming conventions, brand-driven architecture refactor, Vite aliases, CONTRIBUTING.org, SECURITY.org, Doom Emacs config for Vue development), Session 7 Part A (2026-04-20 early — component architecture & naming convention §1.12: views restructured into sections/elements/modals, shared file renames, 7 new kind-aliases, four-layer naming pattern, Rules A–I codified), Session 7 Part B (2026-04-20 late — utils topic libraries §Rule J, three new ui/ primitives, widgets split by kind, ambiguous filename corrections, brand folder restructure into sources/, UiIcon multi-pool SVG discovery, SCSS single source of truth for brand theming, 27/27 tests), Session 8 prep (2026-04-21 — post PR #3 merge + `feat-brand_kot` branch creation; PR #4 opened for `dev` → `master` v0.4 release; release-PR composition pattern codified in §1.13; `release: v0.4 — Architectural Baseline` title chosen via title-body alignment rule; triad pending on `dev`), Session 9 (2026-04-21 continuation — first code on `feat-brand_kot`: cam-log dayjs UTC session-date, status-dot breathe animation, preview-modal iframe-scale sub-pixel fix via `getBoundingClientRect()`, dayjs runtime dep), Session 10 (2026-04-22 → 2026-04-23 — FPS-preservation performance pass on `feat-brand_kot`: cyberpunk-glow mixin removed, halo/glow reborn as opt-in :root tokens, singleton composables, event-driven audio analyzer with Float32Array + JITTER_TABLE, AudioMeter direct DOM writes via template refs, contain: layout paint everywhere, UiStatusDot layered halo+glow, brand.js gains host+region, §1.14 Performance Budget codified, 27→33 tests), Session 11 (2026-04-23 → 2026-04-26 — post-perf-pass deliverables: §1.14 exported into code-review skill as 135 atomic three-tier rules; COMMIT.org collapsed to hyper-concise 27-line shape; PR.org as full Pattern B Feature PR; §1.15 commit convention codified; feat-brand_kot deliverables ready), **Session 12 (2026-04-26 → 2026-04-27 — Plan #context-screen end-to-end on new `context-screen` branch: roam-node system bootstrapped (Index Reckit + Architecture + Naming Conventions split nodes + Plan #context-screen at ~/.brain.d/roam-nodes/), `reckit-roam-node` skill mirroring `mr-roam-node` with git/gh state extraction; full Plan executed across 6 phases — Geomanist font pulled from kyo-web-online, surface-bg + motion tokens, uniorg-parse runtime dep + custom AST renderer (UiOrgContent), useContextChannel singleton with HTTP polling cross-process bridge through Vite middleware (replaces unreliable import.meta.hot for OBS CEF), context-screen.vue with 4 surfaces (lower-third + marquee + sidebar + peek), ContextControlModal on landing page, third CONTROLS button on Card; multi-round visual refinement: 4.5px corner squares via per-corner pseudo-element mixins (corner-square-tl/tr) for overflow visibility, full-black surface backgrounds with white squares + var(--clr-border-100) borders, --clr-primary-100-{02..14} + --clr-neutral-50-{02..12} alpha tokens, alpha-tint/darken/lighten SCSS helpers, Shiki tokyo-night syntax highlighting via Vue-template token walk (no v-html), whitespace 2px-square markers per 1ch column with per-block --ws-marker, list-item flex-baseline fix, sidebar manually scrollable with 5s user-interaction pause, marquee 90s/scroll 16px/s slow pacing, headline `\|` accent replacing corner-bracket SVG; 79/79 tests, 0 lint errors).**

**CRITICAL:** NEVER run `git commit`, `git push`, `gh pr create`, or any git write command. The user handles all git operations manually. Write to `COMMIT.org` and `PR.org` only.

---

## SECTION 1: GLOBAL GUIDELINES & DESIGN CONSTRAINTS

> **Apply these rules to every task in this session.** No domain skills are loaded for OBS automation specifically — these guidelines stage session-specific design and tech rules. PR / commit / CHANGELOG formatting rules live in the user's memory system and are auto-loaded. The pr-scribe skill handles PR body generation (Kyonax brand).

### 1.1 Visual Language

*   **Color palette:** Black (`--clr-neutral-500` = #000), gold (`--clr-primary-100` = #FFD700), white (`--clr-neutral-50` = #F2F2F2). Purple for license/version badges only. Red (`--clr-error-100`) for active recording state.
*   **Aesthetic:** Cyberpunk/sci-fi HUD — military-grade surveillance UI. Corner brackets and crosshairs (no rounded borders).
*   **HUD vocabulary:** `brand.host` (top-left non-primary), session-date (`${brand.region} ∇ DD.MM.YYYY // DDD` — dayjs, UTC, uppercase; primary/gold), `[SESSION] ${scene_name}::T${take_count}` (top-right non-primary), CAM ONLINE (primary/gold), REC MODE. Status indicator (`<UiStatusDot>`) **breathes** when recording — smooth 2s ease-in-out opacity pulse (1 ↔ 0.35) on an inner `.glow` span; the dark halo stays on a separate static layer so the browser never re-rasterizes shadows per frame (decision #128). Never the hard `step-end` blink — soft breathing is the canonical recording-light idiom.

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

**Brand folder structure (mirrors `src/shared/` layout; web sources sandboxed under `sources/`):**
```
@kyonax_on_tech/
  brand.js                     metadata: handle, name, identity, colors, links
  sources.js                   web source registry (data)
  assets/                      brand images, logos
    svg/                       brand SVGs — auto-discovered by <UiIcon>
  styles/_theme.scss           brand CSS custom property overrides

  sources/                     WEB SOURCES — what the brand exposes to OBS
    hud/cam-log.vue            HUD-type web source
    animation/                 Animation web sources (triggered in/out)
    scene/                     Scene web sources (full-screen)

  components/                  [reserved] brand-private components
    hud/                         HUD-domain brand primitives
    ui/                          domain-agnostic brand primitives
  composables/                 [reserved] brand-private hooks
  widgets/                     [reserved] brand-private widgets
    hud/
    ui/
```

**Web source types:** `hud` (persistent HUD over camera), `animation` (triggered in/out), `scene` (full-screen transitions). Live under `<brand>/sources/<type>/<id>.vue`; `brand-loader.js` globs `/@*/sources/<type>/*.vue`.

**Brand-private vs shared:** the `components/`, `composables/`, `widgets/` folders inside a brand are OPTIONAL — create them only when 2+ files of that kind exist (Rule F). Use them when a primitive is specific to one brand (e.g. a `@kyonax_on_tech/components/hud/name-plate.vue` that renders Kyonax-specific bottom banners). Cross-brand primitives always live in `src/shared/`.

**What stays in `src/shared/`:** components (corner-bracket, hud-frame, status-indicator, recording-timer), widgets (audio-meter, live-readout), composables (WebSocket, recording, audio, scene), brand-loader.js, config.js, version.js.

**What stays in `src/views/`:** home.vue + its private components (overlay-card, base-modal, preview-modal, detail-modal) + utils (parse-emphasis).

### 1.4 Licensing & Governance

*   **Dual license:** MPL-2.0 (default) + Apache-2.0. Per-file headers. CI enforces headers.
*   **CODEOWNERS:** `* @Kyonax`. Branch protection pending.

### 1.5 SCSS Architecture (7-1 pattern)

Global theme at `src/app/scss/` — declares default CSS custom properties on `:root` via `@each` loops over the `$colors` + `$typo-scale` SCSS maps. Components consume `var(--clr-*)`, never SASS variables directly.

**Brand theming — SCSS is the single source of truth (2026-04-20):**

- Each `@<brand>/styles/_theme.scss` declares a `.brand-<handle>` class block that redeclares any subset of the CSS custom properties.
- `src/main.js` auto-loads every brand theme file via `import.meta.glob('/@*/styles/_theme.scss', { eager: true })` — Vite processes each file at build time and inlines the CSS into the global bundle.
- `src/App.vue` applies the brand class (`class="brand-<handle>"`) on the root div when the current route's `meta.brand` resolves to a brand. The CSS cascade does the rest — every descendant component's `var(--clr-*)` call resolves to the brand's override if one is declared, otherwise falls back to the `:root` default.
- `brand.js` **does NOT carry a `colors` field** — the SCSS file is the single source of truth. Adding colors to `brand.js` is forbidden (enforced by a brand-loader test that asserts `brand.colors === undefined`).
- A new brand copies `@<brand>/styles/_theme.scss`, renames the class selector to `.brand-<new-handle>`, and tunes whatever subset of variables it wants to override. Unlisted variables fall back to `:root` defaults.

**Why SCSS over JS-inline-vars:** full-palette override (not a hard-coded subset), single source of truth, zero JS runtime cost, CSS-cascade native, HMR-friendly.

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
*   Release history: v0.1 (DONE on `master`), v0.3 (DONE on `master`). Next: **v0.4 — Architectural Baseline** (PR #4 open: `dev` → `master`, triad pending).
*   **Release triad** (required for every `dev` → `master` PR — `release.yml` enforces): (1) `package.json` `"version"` bump via `npm version <X.Y.Z> --no-git-tag-version`; (2) `README.org` version markers at 4 spots (`#+VERSION:`, ASCII logo footer `░vX.Y`, shields.io badge `version-vX.Y`, `alt="vX.Y"`) + `#+LAST_UPDATE:` refreshed; (3) `CHANGELOG.org` new `[vX.Y] — <date> :: <Title>` block above the prior release with Added/Changed/Removed/Decided subsections. All three must land on `dev` before the release PR turns green. Partial triads are caught by the workflow gate.

### 1.9 Overlay Card Data Format (`sources.js`)

Each `@brand/sources.js` exports an array. Fields: `id`, `type` (hud/animation/scene), `brand`, `name`, `description` (**bold** markers), `use_cases[]`, `path`, `width`, `height`, `fps`, `requires[]`, `triggers[]`, `status` (ready/planned). The `id` + `brand` + `type` determine the component path: `/@brand/type/id.vue`.

### 1.10 Tier 1 File Header Pattern

All root config/setup/org files use figlet smslant ASCII art with cyberpunk place names. Layout: license (separate block), figlet art, filename + date, description, TOC, optional guidelines/requirements, author/contact. See memory file `feedback_config_file_headers.md` for full spec and place name registry.

### 1.11 Brand Metadata Schema (`brand.js`)

Each `@<brand>/brand.js` exports: `handle` (e.g. `@kyonax_on_tech`), `name`, `description`, `identity` (`author`, `display_handle`), `links` (x, youtube, github).

**Colors are NOT in `brand.js`** — they live exclusively in `@<brand>/styles/_theme.scss` under the `.brand-<handle>` selector (see §1.5 SCSS Architecture). Duplicating colors into `brand.js` is forbidden; the brand-loader test enforces `brand.colors === undefined`.

### 1.12 Component Architecture & Naming Rules

**(2026-04-20)** Project-wide classification, folder structure, alias layer, and naming convention for all `.vue` and `.js` files. Applies equally to `shared/` and `views/`. **Canonical reference for every new file.**

#### 1.12.1 Six component categories

| Category | Location | Ext | Vue-aware? | Side-effects? | Renders DOM? |
|---|---|---|---|---|---|
| **util** | `{views,shared}/utils/` | `.js` | No | No | No |
| **composable** | `shared/composables/` | `.js` | Yes | Yes | No |
| **ui component** | `shared/components/ui/` | `.vue` | Yes | No (props→DOM) | Yes |
| **domain component** | `shared/components/<domain>/` (e.g. `hud/`) | `.vue` | Yes | No (props→DOM) | Yes |
| **widget** | `shared/widgets/` | `.vue` | Yes | Yes (wraps composable) | Yes |
| **view component** | `views/components/<kind>/` | `.vue` | Yes | Mixed | Yes |

#### 1.12.2 Folder split by KIND

Both `shared/` and `views/` split by kind, not by page or region. Kind = answer to "what type of thing is this?"

**`views/components/`:**
- `sections/` — top-level page regions (used once per page)
- `elements/` — reusable pieces inside sections (v-for children, list items)
- `modals/` — overlay surfaces (shell + instances)

**`shared/components/`:**
- `ui/` — domain-agnostic primitives (icon, status dot)
- `hud/` — HUD-domain primitives (frame, timer)

**`shared/` top-level:**
- `composables/` — Vue-aware JS logic (hooks). One `useX` per file.
- `widgets/` — data-driven `.vue` **split by kind** (mirrors `components/` split):
  - `widgets/hud/` — HUD-domain widgets (OBS-coupled, HUD-themed). Example: `audio-meter.vue`.
  - `widgets/ui/` — domain-agnostic widgets (no project coupling). Example: `live-readout.vue`.
- `utils/` — **topic-based libraries** (e.g. `markup.js` for inline-markup parsers, `timecode.js` for time formatting, `dom.js` for DOM queries). Never one-function-per-file — see Rule J.
- `data/` — **topic-based static registries** (same library pattern as utils, but exports constants/data instead of functions)

**`views/utils/`:** same topic-library pattern as `shared/utils/`. Current: `markup.js` (inline-markup parsers — exports `parseEmphasis`). Reserved for future growth: a separate `typography.js` for CSS/rendering helpers (font sizes, tabular nums) if those ever appear — naming stays scoped to *what the file parses or styles*, never bundling unrelated concerns.

#### 1.12.3 Vite alias registry (kind-level)

**Top-level scopes** (unchanged since Session 6):
- `@shared`, `@views`, `@app`, `@assets`

**Kind folders** (added 2026-04-20):
- `@sections` → `src/views/components/sections/`
- `@elements` → `src/views/components/elements/`
- `@modals` → `src/views/components/modals/`
- `@ui` → `src/shared/components/ui/`
- `@hud` → `src/shared/components/hud/`
- `@widgets` → `src/shared/widgets/`
- `@composables` → `src/shared/composables/`

11 aliases total. Every kind-folder gets one. Add a new alias at the same moment a new kind-folder is created.

#### 1.12.4 Four-layer naming pattern

```
file              hero.vue              (short, kind-folder-contextual)
alias             @sections/hero.vue    (folder identifies kind)
import binding    HeroSection           (self-documenting at import site)
template tag      <HeroSection />       (matches import binding)
```

Vue `<script setup>` resolves tags from the **import binding**, not the filename — short filenames are fine. Filenames stay kebab-case (ESLint-enforced).

#### 1.12.5 Kind suffix/prefix registry

Import bindings (and template tags) follow English-natural positioning — prefix when the kind is a namespace-style noun, suffix when it's a structural noun, none when the file name is already self-descriptive.

| Folder | Position | Form | Examples |
|---|---|---|---|
| `@sections` | suffix | `XxxSection` | `HeroSection`, `MetaSection`, `SetupSection`, `SourcesSection`, `FooterSection` |
| `@modals` | suffix | `XxxModal` | `BaseModal`, `PreviewModal`, `DetailModal` |
| `@elements` | none | `Xxx` | `Card` |
| `@ui` | prefix | `UiXxx` | `UiIcon`, `UiStatus` |
| `@hud` | prefix | `HudXxx` | `HudFrame`, `HudTimer` |
| `@widgets` | none (descriptive filename) | as-is | `AudioMeter`, `LiveReadout` |

Rule: pick whichever position reads naturally in English. Nobody says "CardElement" — don't force it.

#### 1.12.6 Rules A-I (codified discipline)

- **Rule A (util vs composable):** `.js` that imports from `'vue'` is a composable; otherwise util.
- **Rule B (widget vs domain component):** `.vue` that calls a composable or owns side-effects (`setInterval`, `rAF`, event listeners) is a widget; otherwise a plain component.
- **Rule C (ui vs domain):** `.vue` using project vocabulary → `components/<domain>/`. Domain-agnostic → `components/ui/`.
- **Rule D (shared vs views):** if a brand overlay could consume it → `shared/`. If only a view uses it → `views/`.
- **Rule E (extraction discipline):** create a `.vue` only if (a) rendered in `v-for`, (b) composed by 2+ parents, (c) a named top-level page section, or (d) a generic shell intended for `shared/`. Otherwise inline in the parent. CSS class names are not section names.
- **Rule F (folder existence):** folders exist only when they group 2+ files of the same kind. Folder names are general categories (`sections`, `elements`, `modals`, `ui`, `hud`), not content labels (`cards`, `tabs`, `filters`).
- **Rule G (component naming — describe purpose, never repeat the kind):** filename is kebab-case. The filename conveys **what the component is or does** — it **must not repeat the alias/folder kind**. `@modals/base-modal.vue` is redundant (the alias already says "modal"); correct form is `@modals/base.vue`. Multi-word is justified only when the single word leaves an "X of what?" question unanswered: `metric.vue` fails because "metric of what?" has no answer from context alone — `data-point.vue` answers it directly. Single-word wins when the word fully conveys the thing (`hero.vue`, `frame.vue`, `icon.vue`, `chip.vue`, `badge.vue`, `card.vue`). Test: reading only the import line (`import X from '@kind/file.vue'`), can a developer understand the component's purpose? If not, the filename needs refining — but **refine by describing purpose, never by re-stating the kind**. Component identity comes from the import binding, which follows §1.12.5 and may legitimately repeat the kind at that layer (`<BaseModal>`, `<HeroSection>`, `<UiIcon>`) because the binding is the final readable identity.
- **Rule H (aliases):** every kind-folder gets a Vite alias + matching ESLint path. Add at folder-creation time.
- **Rule I (import bindings):** template tags use PascalCase to match the import binding. Snake_case props allowed (project-wide naming convention: `:is_open`, `:elapsed_time`).
- **Rule J (utils organization — topic libraries):** `utils/` and `data/` files are **topic-based libraries**, never one-function-per-file. Group related pure helpers in one file named by topic (`markup.js` for text helpers, `timecode.js` for time formatting, `dom.js` for DOM queries, etc.). Every function is a named export; consumers import only what they need: `import { parseEmphasis } from '@views/utils/markup.js'`. If a new helper doesn't fit an existing topic, create a new topic file — but only when you have at least one helper for it. If a topic file ends up with one function for too long, consider whether it belongs in an existing topic instead. Composables are exempt — each `useX.js` stays one hook per file because hooks own state/lifecycle and shouldn't share module scope with unrelated siblings.

#### 1.12.7 Decision tree (new file)

```
.js file
  ├── imports from 'vue'?     → shared/composables/  (useX function)
  └── pure?                   → {shared,views}/utils/  (or shared/data/ if constants)

.vue file
  ├── used only by one view?              → views/components/<kind>/
  │                                          (sections / elements / modals)
  ├── calls a composable / side-effects?  → shared/widgets/
  ├── uses HUD/domain vocabulary?         → shared/components/hud/
  └── domain-agnostic primitive?          → shared/components/ui/
```

**Extract:** top-level page section (Hero, Footer, feature block), v-for children (Card), generic shell (BaseModal), cross-parent reuse.

**Don't extract:** single-use sub-UI inside one section (brand tabs, filter bar, empty state, status chip, meta item, setup step) — stays inline.

### 1.13 PR Composition Pattern

**(2026-04-21)** Rules for producing PR bodies via the `pr-scribe` skill — apply consistently across feature PRs and release PRs. Augments the canonical `brand-kyonax` rule in the pr-scribe skill with project-specific conventions learned during PR #3 (feature) and PR #4 (release).

#### 1.13.1 Title patterns

| PR type | Format | Example |
|---|---|---|
| Feature branch → integration (`dev`) | `[vX.Y]: <Short Release Title>` | `[v0.3]: Vue App + CAM-LOG Overlay + Landing Index` |
| Feature branch (non-release) | `feat(<scope>): <lowercase summary>` | `feat(widgets): extract audio meter + readout` |
| Release integration (`dev` → `master`) | `release: vX.Y — <Semantic Name>` | `release: v0.4 — Architectural Baseline` |
| Hotfix / follow-up | `fix(<scope>): <lowercase summary>` | `fix(ci): unblock ESLint + Security Scan` |

**Title-body alignment rule:** the title should distill the body's strongest semantic framing, not list mechanical features. If a phrase appears in the summary paragraph + Technical Details + Documentation sections (3+ occurrences), that phrase is a title candidate. PR #4's `Architectural Baseline` came from exactly that pattern.

#### 1.13.2 Feature PR vs. Release PR body shape

Both use the Kyonax brand rules (Pattern B Changes, TD-4FIELD, TEST-TWO-TABLE, QA-HOW-TO-TEST, DEPLOY-SEVERITY, DOC-MEDIA-VOCAB) but differ in density:

| Aspect | Feature PR (e.g. PR #3) | Release PR (e.g. PR #4) |
|---|---|---|
| Body length | ~300 lines | ~225 lines |
| `### Implementation` | Full file inventory with `[NEW]/[MOD]/[DEL]/[MOV]` tags per entry | 5–10 theme-summary bullets, each ends `(see PR #N for detail)` |
| `### Release` subsection | Omitted | **Mandatory** — `**Version:** vX.Y (was vA.B)` + 3 `[MOD]` triad entries (flag as `(PENDING)` until they land) |
| `### Technical Details` | 10–15 TD-4FIELD decisions | 2–3 release-specific decisions (cut strategy, artifact flow, semantic framing) — never re-paste feature-level decisions |
| `### Testing Coverage` | Full Automated tests + Quality gates tables | Summary line + Quality gates table only; reference feature PR for per-file breakdown |
| `## How to test this PR` | Feature-verification groups | Release-verification groups (triad readiness, master baseline pipeline, UI version surfaces, post-tag artifact) |
| `## Special Deployment Requirements` | Often omitted | Always present for release PRs — triad steps as CRITICAL, tag as REQUIRED, GitHub Release + branch protection as OPTIONAL |
| Cross-references | Rare — this PR owns the detail | Heavy — release PR is a pointer, not a copy |

#### 1.13.3 Reference-rather-than-duplicate principle

When a follow-up PR ships content already detailed in an earlier merged PR (release PR → feature PR; hotfix PR → original feature PR):
- Link the prior PR in the summary paragraph AND under the first Changes subsection.
- Replace per-file bullets with theme-summary bullets ending `(see PR #N for detail)`.
- Technical Details keeps only decisions SPECIFIC to the new PR's context (e.g. for a release PR: release-cut strategy, release artifact flow, version-semantic framing).
- Testing Coverage keeps only gates relevant to the new PR's outcome; reference the prior PR for per-file test breakdowns.
- Never re-paste Implementation file inventories — maintenance burden doubles without adding signal.

#### 1.13.4 Release triad surfacing

Every release PR body must make the triad visible to reviewers:

1. **Checklist** — last two items (`CHANGELOG.org updated`, `Version bumped`) stay `[ ]` until triad lands; add a blockquote note right below the checklist pointing at the Special Deployment Requirements section.
2. **`### Release` subsection** — present with `**Version:** vX.Y (was vA.B)` header + 3 `[MOD]` entries (`package.json`, `README.<ext>`, `CHANGELOG.<ext>`). If any are not yet done, append `(PENDING — <exact-command>)` to the entry.
3. **Testing Coverage Quality gates table** — add a row for `Release triad (release.yml)` marked `⚠️ pending until triad lands on dev`.
4. **Special Deployment Requirements** — enumerate the triad steps as `CRITICAL` items with exact commands (`npm version X.Y.Z --no-git-tag-version`, the four `sed -i -E` patches on README, the `[vX.Y]` CHANGELOG block).
5. **How to test this PR** — first group should be `Pre-merge — triad readiness` with `grep` commands that confirm each triad file reads the new version.

#### 1.13.5 Release generation workflow

```
1. Feature PR merged to dev         → session file §5 + decision in §2.3
2. User creates release PR          → title pattern `release: vX.Y — <Name>`
3. Invoke pr-scribe with release context:
   - Reference predecessor feature PR(s)
   - Capture semantic framing from body patterns (title-body alignment)
   - Apply reference-rather-than-duplicate for Implementation/Testing/Deps/Docs
   - Add DEPLOY-SEVERITY with triad-as-CRITICAL
4. Complete triad on dev            → flip checklist + Release subsection PENDING flags
5. Merge release PR                 → tag vX.Y, push tag, optional GitHub Release
6. New branch from dev              → next feature cycle
```

### 1.14 Performance Budget for OBS Browser Sources

**(2026-04-23)** RECKIT HUDs run as OBS Browser Sources composited over live video — the source MUST hold full OBS canvas fps (typically 60) on low-core-processor machines. Rendering cost is a first-class constraint, not a polish concern. Every HUD-touching change must consciously weigh these rules before shipping. Reverse-engineered from Session 10's FPS-regression diagnosis (decision #116) and the rewrite that followed.

#### 1.14.1 The FPS budget rule

*   **Target:** the HUD must never drop OBS source fps on the user's hardware (multi-core CPUs with low per-core clock — streaming rigs, laptops, minis). If any rule below conflicts with a visual flourish, fps wins.
*   **Regression-first instinct:** when the HUD feels slow in OBS, profile before fixing. Common culprits in order of pain: broad CSS `filter` / `text-shadow` chains, per-frame JS allocations, animated properties that re-layout or re-rasterize, over-subscription to `InputVolumeMeters`.

#### 1.14.2 Don't apply expensive CSS broadly

*   **Never apply `filter: drop-shadow(...)` chains, multi-layer `box-shadow`, or `text-shadow` chains via a utility/mixin that hits every element of a color class.** The original `cyberpunk-glow` on every `--clr-primary-100` text was the FPS-killing incident — each gold label rasterized its own filter layer, compounded when animated.
*   **Halo/glow is OPT-IN per element**, declared via CSS custom properties (`--hud-halo`, `--hud-halo-text`, `--hud-glow`) that specific consumers reference. Never auto-apply by color.
*   **Group halo on a single container when possible.** `filter: drop-shadow()` rasterizes the subtree once; applying per-leaf multiplies the cost by N.

#### 1.14.3 Animate only cheap, composited properties

*   **Allowed per-frame:** `transform` (`translate`, `scale`, `rotate`), `opacity`.
*   **Avoid per-frame:** `width`, `height`, `top`/`left`/`right`/`bottom`, `box-shadow`, `filter`, `background`, anything that triggers layout or re-rasterization.
*   **Split static from animated.** If a decoration needs both a dark halo (expensive) and a pulsing glow (cheap), put the halo on one layer and animate `opacity` on a sibling — never animate an element whose `box-shadow`/`filter` is part of the keyframe. Example: `<UiStatusDot>` root carries static halo `box-shadow`; the inner `.glow` span is the only animated element (`opacity` only).
*   **Prefer `transform: scaleY()` over animated `height`** for bar-style visualizers. GPU-composited, zero layout cost.

#### 1.14.4 CSS containment on HUD sub-trees

*   Add `contain: layout paint` to any HUD group, widget root, or frequently-updating region (`.hud-group`, `.status-bar`, `.audio-meter`, `.recording-timer`, `.ui-data-point`, `.debug-info`, `.hud-frame`, `.cam-log-overlay`). Isolates paint/layout so a bar-height update in the audio meter cannot force a layout pass on unrelated HUD labels.
*   Don't use `contain: strict` or `contain: size` — they constrain layout too aggressively for brand-sized content.

#### 1.14.5 OBS WebSocket call budget

*   **One subscription per event per page.** Every OBS-WS-consuming composable is a **module-level singleton** (`useObsWebsocket`, `useRecordingStatus`, `useSceneName`, `useAudioAnalyzer`). N mounted consumers share one state object — never N subscriptions. First caller registers the handler; subsequent callers receive the same returned object.
*   **Singleton contract:** `const a = useX(); const b = useX(); assert(a === b)`. Enforced by `src/shared/composables/composables.test.js`.
*   **No `onUnmounted` cleanup on singletons** — they live for the page lifetime. Handler registration happens once at first-use.
*   **Event-driven over rAF.** If OBS already pushes data (e.g. `InputVolumeMeters` at ~50 Hz), drive the UI off that event, not a separate `requestAnimationFrame` loop. One clock, one update cadence.
*   **Throttle emits from hot paths.** A composable firing per OBS event should expose a `tick` counter for fine-grained watchers; Vue `$emit` calls that cross component boundaries should be throttled (~10 Hz is plenty for diagnostic readouts — use `performance.now()` delta gating).

#### 1.14.6 Zero-allocation hot path

*   **Preallocate typed arrays.** `new Float32Array(bar_count)` once at setup; mutate in place every tick. Never allocate new `Array(...)` or `{...}` per event.
*   **Precompute lookup tables.** Replace per-frame `Math.random()` with a seeded `JITTER_TABLE: Float32Array` (256-entry ring, cursor-advanced). Replace per-frame string concat with a quantized `SCALE_STRINGS` table (101-entry `transform: scaleY(0.00...1.00)` strings indexed by rounded scale).
*   **No per-event `Array.find` / `Array.map` / destructuring on large inputs.** Classic `for (let i = 0; i < len; i++)` in hot paths. Break out of the loop as soon as the target is found.
*   **Bypass Vue reactivity when appropriate.** For visualizers updating many siblings at 50 Hz, write directly to the DOM via template refs (`el.style.transform = ...`). Gate writes with a threshold (`Math.abs(next - last) < 0.01` → skip). Vue's reactivity graph is for *state*, not *per-frame paint*.

#### 1.14.7 Debounce event listeners on burst-prone inputs

*   `window.resize`, `scroll`, and similar fire in bursts. Debounce expensive handlers (`applyScale`, layout recomputation) by ~100 ms with `setTimeout`/`clearTimeout`; clear the timer on unmount.

#### 1.14.8 Review checklist before adding any HUD feature

1. Does this add a `filter`, `box-shadow`, `text-shadow` on a broad selector? → No. Opt-in via custom property.
2. Does this animate anything other than `transform` / `opacity`? → No. Refactor.
3. Does this add an OBS-WS subscription? → Inside a singleton composable, not inside the consumer.
4. Does this allocate per frame (new arrays, new objects, string concat)? → Preallocate or precompute.
5. Does this `$emit` on every tick? → Throttle.
6. Is this a frequently-updating sub-tree without `contain: layout paint`? → Add containment.
7. Does this own a burst-prone `window.*` event listener? → Debounce.

### 1.15 Commit Message Convention (Hyper-Concise)

**(2026-04-26)** RECKIT commit message bodies follow a hyper-concise shape — captured after the user collapsed a verbose 180-line draft into 27 lines for `feat(kot): brand refinement + OBS FPS perf pass`.

#### 1.15.1 Shape

```
<subject ≤ 60 chars>           ← Conventional Commits prefix + lowercase summary

<one-paragraph rationale>      ← 1–3 sentences max, present tense

- <verb-led bullet>            ← scannable; one concern per bullet
- <verb-led bullet>            ← group by subsystem when 3+ files share one
- ...                            (e.g. "useRecordingStatus / useSceneName /
                                  useAudioAnalyzer → module-level singletons")

Validation: <one line>         ← lint + tests + visual, single sentence
```

#### 1.15.2 Rules

- **Subject** uses Conventional Commits (`feat(scope):`, `fix(scope):`, `refactor(scope):`, `perf(scope):`, `chore(scope):`). Scope matches the branch suffix when applicable (`kot`, `ci`, `widgets`).
- **Rationale paragraph** explains *why* in 1–3 sentences, not *what* (the bullets do that).
- **Bullets are verb-led** (`remove`, `add`, `convert`, `bypass`, `debounce`, `harmonize`). No prose paragraphs inside the bullet list.
- **Group bullets by subsystem** when 3+ files share one concern (`useRecordingStatus / useSceneName / useAudioAnalyzer → module-level singletons`).
- **One validation line** at the end summarizing lint + tests + visual sanity. Not a separate `Validation` section.
- **No file inventory inside the commit body.** The exhaustive "Files in the diff" table lives in `COMMIT.org` BELOW the `#+END_SRC` gitcommit block as org-mode metadata — useful for staging review, not part of the commit message.
- **No "Scope note" / "Documentation sync" / "Trade-off" sub-sections.** Those belong in the PR body (which has TD-4FIELD blocks for trade-offs), not the commit.

#### 1.15.3 PR body shape stays unchanged

This convention applies to commit messages only. PR bodies keep the full Pattern B Kyonax Feature-PR shape (§1.13) — themed Changes subsections, TD-4FIELD Technical Details, TEST-TWO-TABLE Testing Coverage, QA-HOW-TO-TEST with ASCII flow tree, DOC-MEDIA-VOCAB. Commit and PR have different audiences: commits are scanned in `git log`, PRs are reviewed in GitHub.

### 1.16 Cross-Process State Sync (Landing Page ↔ OBS Browser Source)

**(2026-04-27)** When a feature needs to synchronize state between the user's regular browser tab AND the OBS Browser Source (a separate embedded Chromium / CEF process), `BroadcastChannel` is NOT enough — it works only within a single browser process. Vite HMR custom events (`import.meta.hot.send/on`) proved unreliable in OBS CEF (silent failure of the WebSocket handoff). The validated path is **HTTP polling + push through a Vite dev-server middleware**:

1.  **Vite plugin** in `vite.config.js` registers a middleware on a path (e.g. `/__context_state`) that handles `GET` (return current state) + `POST` (replace state). State held in a closure on the dev server.
2.  **Composable** polls every ~300 ms via `fetch('/__context_state')` and pushes via `fetch('/__context_state', { method: 'POST', body: JSON.stringify(snapshot) })` on every local action.
3.  **Echo suppression:** before pushing, the composable sets `last_pushed_hash = JSON.stringify(snapshot)`. The polling tick compares incoming JSON's hash; if it matches the last pushed hash, skip applying (prevents own-message echo loops).
4.  **`BroadcastChannel` is kept as a same-process accelerator** (sub-50 ms cross-tab sync within the regular browser); the HTTP path is the universal fallback that reaches OBS CEF.
5.  **Production-friendly:** the middleware-style endpoint can be hosted by any HTTP server in production. `import.meta.hot` is undefined in production, so any HMR-based bridge would break — HTTP doesn't.

**Debuggability:** `curl http://localhost:5173/__context_state` returns the current state JSON. Solving "state not propagating" is a one-liner check. This is the canonical pattern for any future RECKIT control plane crossing the regular-browser ↔ OBS boundary.

### 1.17 Color-Shade Tokens + SCSS Helpers

**(2026-04-27)** Inline `rgba(255, 215, 0, 0.0X)` and `rgba(255, 255, 255, 0.0X)` literals are forbidden — they bypass the brand theming pipeline. Every alpha-tinted color uses a token. The token convention extends the existing `--clr-primary-100-40` / `-80` pattern to lower opacities:

*   `--clr-primary-100-{02, 03, 04, 06, 10, 14, 40, 80}` declared at `:root` in `src/app/scss/abstracts/_theme.scss`. Each is `color-mix(in srgb, var(--clr-primary-100) X%, transparent)`.
*   `--clr-neutral-50-{02, 04, 12}` for white-on-dark tints.
*   For one-off opacities not covered by a named token, three SCSS helpers in `_mixins.scss`:
    *   `alpha-tint($color-var, $percent)` — `color-mix` with transparent (alpha)
    *   `darken($color-var, $by)` — `color-mix` with black
    *   `lighten($color-var, $by)` — `color-mix` with white
*   Helpers take a CSS-variable NAME (e.g. `--clr-primary-100`), not a value, because they emit `color-mix(... var(--clr-X) ..., target)` — the variable name is interpolated into the `var()` call.
*   Prefer named tokens over helpers. Helpers exist for ad-hoc one-offs (e.g. `lighten(--clr-neutral-500, 8%)` for a slightly-lifted-from-black banner background).

### 1.18 Corner-Square HUD Chrome

**(2026-04-26 → 2026-04-27)** Every "container with a background" surface in the HUD ships with two visual elements: a 1px solid border AND 4.5px squares at corner intersections. Two mixin variants in `src/app/scss/abstracts/_mixins.scss`:

1.  **`corner-dots($color, $size: 4.5px, $corners: tl tr bl br)`** — paints squares as `background-image` layers (one `linear-gradient` per requested corner). Uses `background-origin: border-box; background-clip: border-box` so position `0 0` / `100% 100%` reference the OUTER border edge. Square sits flush WITH the corner (not overflowing). Default size 4.5px. `$corners` accepts a list of any of `tl tr bl br`. Used for INTERIOR surfaces where overflow isn't needed (code blocks, quotes, tables, peek arrow).
2.  **`corner-square-tl($color, $size)` + `corner-square-tr($color, $size)`** — pseudo-element variants (::before / ::after) with negative offsets (`-$size * 0.5`) so the square sits CENTERED on the corner intersection (half outside, half inside). Used when overflow visibility matters (strip + sidebar at canvas-interior boundaries). **Critical:** the consumer must already be a positioned element (`position: relative | absolute | fixed | sticky`) — the mixin does NOT set `position: relative` so it doesn't clobber existing `position: absolute` consumers (the bug that broke the sidebar in mid-session). Consumer must NOT use `contain: paint` (which would clip the overflowing pseudo-element). Use `contain: layout` only.

**Per-surface corner spec rule:** corners that touch the canvas edge get NO square (since OBS canvases don't have a visible boundary there); corners at interior intersections DO get squares. For context-screen specifically:
*   `.context-strip`: `tl tr` (bottom is at canvas bottom) — pseudo-element variant
*   `.context-marquee`: NONE (decorative gold strip — no border, no squares)
*   `.context-sidebar`: `tl` only (top-right + bottom-right at canvas right; bottom-left dropped per design)
*   `.context-peek`: all 4 (small interior floating element) — `corner-dots` variant
*   Code blocks (src/results/example) + quote + table: all 4 — `corner-dots` variant
*   `.context-sidebar__header::after` standalone — square at LEFT end of separator line (right end at canvas edge)

**Color rule:** on full-black surfaces, squares are `var(--clr-neutral-50)` (bright white, pops against black); borders stay `var(--clr-border-100)` (white at 20% alpha — subtle line). The two are deliberately different: borders define structure quietly; squares are accent markers that demand attention.

**Z-index rule for overlapping squares:** when a corner square needs to render above an adjacent surface (strip's TR over the sidebar at the strip-right ↔ sidebar-left boundary), set `z-index: 110` on the OWNING container (e.g. `.context-lower`) so it stacks above the sidebar's `z-index: 100`. Drop `paint` from `contain` on that container so the pseudo-element isn't clipped.

**Shared-border rule between adjacent surfaces:** when two surfaces meet at an interior boundary (strip-right ↔ sidebar-left), DROP one side's border (e.g. `border-right: none` on the strip) so the pair shares a single 1px line instead of stacking into 2px.

### 1.19 Plan-Node Documentation Lifecycle (`reckit-roam-node` skill)

**(2026-04-26)** Project-scoped knowledge — plans, bugs, releases — lives as `.org` roam nodes parallel to the canonical session file. The system mirrors `mr-roam-node` (Madison Reed's JIRA-driven flow) but is git/gh-driven instead. New skill: `reckit-roam-node` at `dot-files/.config/doom-mac/gptel-directives/skills/reckit-roam-node/` (auto-loaded via the existing symlink to `~/.config/doom/gptel-directives/`).

**Three node types:**

| Variant | `#+TITLE:` | `#+FILETAGS:` | Body sections |
|---|---|---|---|
| Standard (Feature) | `Plan #<slug>` | `:RECKIT:PLAN:` | SCOPE & GOAL → DESIGN & ARCHITECTURE → DOCUMENTATION |
| Bug | `(BUG) Plan #<slug>` | `:RECKIT:BUG:` | SYMPTOM → REPRO → ROOT CAUSE → INVESTIGATION NOTEs |
| Release | `Release v<X.Y> — <Name>` | `:RECKIT:RELEASE:` | RELEASE SCOPE (with TRIAD STATUS) → RELEASE TASKs → RELEASE NOTES |

**Index dashboard:** `~/.brain.d/roam-nodes/2026-04-26-index_reckit.org` (root-level, alongside other index files like `2025-11-18-index_madison_reed.org`). Two-layer architecture: BACKLOG (org-roam `id:` links — source of truth) + PLAN BOARD (six lanes: IN PLANNING / IN DEVELOPMENT / IN REVIEW / IN TEST / ALL RELEASED / SHELVED). Lane assignment derives from git/gh state via `gh pr list --head <branch>`, `git log dev..<branch>`, and `git tag --contains`.

**Architecture nodes** (split from the original consolidated `reckit_project.org`):
*   `~/.brain.d/roam-nodes/reckit/2026-04-17-reckit_architecture.org` — ASCII project tree, auto-discovery flow, OBS data flow, brand theme flow, composition flow, import patterns, brand creation guide. UUID `ac49fa6e-4520-47b4-b01a-477d7f135add` (preserved from original).
*   `~/.brain.d/roam-nodes/reckit/2026-04-20-reckit_naming_conventions.org` — Six categories + kind folders + 11 alias registry + Rules A–J + decision tree + refinement log. UUID `680e3a77-9f0f-477b-b9ec-94ce7a87416b`.

**Plan node template (Standard):** `:PROPERTIES: :ID:` → metadata stack (`#+TITLE`, `#+SUBTITLE`, `#+EFFORT`, `#+TARGET_RELEASE`, `#+BRANCH`, `#+STATUS`) → SCOPE AND GOAL (REQUIREMENTS as GIVEN/WHEN/THEN, OUT OF SCOPE, OPEN QUESTIONS with Recommendations) → DESIGN AND ARCHITECTURE (DATA FLOW ASCII diagrams, DECISIONS as `*D<n>* (date)` blocks with `*Choice* / *Why* / *Alternatives* / *Cross-ref*`, TRADE-OFFS) → IMPLEMENTATION DISCIPLINE (file-header convention, naming convention table, ESLint rules, Vue patterns, composable singleton skeleton, SCSS pattern, test pattern, animation discipline, sharp-corner rule, pre-merge gate) → MEDIA → RELEVANT LINKs → TODO PLAN TASKs (phased, statistics-cookied) → DOCUMENTATION (STRUCTURE / DEPLOYMENT / TESTS / QA / FINDINGS — Phase 2 fill) → DELIVERABLEs → COMMENTs.

**Workflow:** during planning conversation, the skill drives REQUIREMENTS / DECISIONS / OPEN QUESTIONS authoring. As Phase 0 questions get answered, `Q<n> [ ]` flips to `[X]` and the corresponding `D<n> (deferred)` stub gets fully populated. As tasks complete, checkboxes flip + statistics cookies recalculate. Bug plans use `** UPDATE [<date>]` sub-headings under INVESTIGATION NOTEs as findings accumulate. Release plans gate the v<X.Y> triad (package.json + README.org + CHANGELOG.org).

**Cross-link convention:** every plan node's `RELEVANT LINKs` includes the branch URL, PR URL (once opened), Index Reckit, Reckit Architecture, Reckit Naming Conventions, and the canonical session file path with relevant section numbers. Plan UUIDs:
*   Index Reckit: `93c9b466-676d-48bf-9d1b-ec8b93816b5d`
*   Reckit Architecture: `ac49fa6e-4520-47b4-b01a-477d7f135add`
*   Reckit Naming Conventions: `680e3a77-9f0f-477b-b9ec-94ce7a87416b`
*   Plan #context-screen: `cabd1489-23cc-4ce3-9825-7b5a8eb065b9`

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
| Phase 3.6: Fixes, refinement, architecture (Session 7 A+B) | **DONE on `dev` — `feat-fixes-and-refinement-v3` merged (PR #3)** |
| Phase 3.7: @kyonax_on_tech brand expansion (Sessions 8–11, `feat-brand_kot`) | **READY TO COMMIT/PR** — perf pass + brand refinement; PR.org + COMMIT.org staged |
| Phase 3.8: Plan #context-screen — second web source (Session 12, `context-screen`) | **READY TO COMMIT/PR** — full implementation shipped, 79/79 tests, 0 lint errors |
| Phase 4: Polish + packaging | NOT STARTED |

**Current working branch:** `context-screen` (since 2026-04-26, branched from `dev`). Plan #context-screen fully implemented (Plan node at `~/.brain.d/roam-nodes/reckit/2026-04-26-150000-reckit_plan_context_screen.org`). All implementation conforms to §1.12 (Component Architecture) + §1.14 (Performance Budget) + §1.16 (Cross-Process State Sync) + §1.17 (Color Tokens + SCSS Helpers) + §1.18 (Corner-Square Chrome) + §1.19 (Plan-Node Lifecycle).

**Other open branches:** `feat-brand_kot` (Sessions 9–11) — still uncommitted, deliverables ready (`COMMIT.org` + `PR.org` regenerated for the perf pass). PR #4 (`dev` → `master`, v0.4 release) — open with triad pending on `dev`.

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
91. **(2026-04-20)** `views/components/` restructured into kind folders: `sections/` (top-level page regions), `elements/` (reusable pieces), `modals/` (overlay surfaces). Parallels `shared/components/ui|hud/` structure.
92. **(2026-04-20)** 5 sections extracted from `home.vue` (Meta, Hero, Setup, Sources, Footer). `home.vue` slimmed to thin composer (~60 lines). Filter state (search, status, brand tab) moved into `sources.vue`.
93. **(2026-04-20)** Shared file renames (Option C — short filenames, kind prefix/suffix at import binding): `hud/hud-frame.vue` → `hud/frame.vue`, `hud/recording-timer.vue` → `hud/timer.vue`, `ui/svg-icon.vue` → `ui/icon.vue`, `ui/status-indicator.vue` → `ui/status.vue`.
94. **(2026-04-20)** 7 new Vite kind-aliases: `@sections`, `@elements`, `@modals`, `@ui`, `@hud`, `@widgets`, `@composables`. Total alias count now 11. Every kind-folder has one.
95. **(2026-04-20)** Four-layer naming pattern adopted: file (kebab short) → alias (@kind) → import binding (PascalCase with suffix/prefix) → template tag (PascalCase match). Vue `<script setup>` resolves tags from import binding, not filename.
96. **(2026-04-20)** Kind suffix/prefix registry: `@sections` → `XxxSection`, `@modals` → `XxxModal`, `@elements` → `Xxx` (none), `@ui` → `UiXxx`, `@hud` → `HudXxx`, `@widgets` → as-is (descriptive filename). See §1.12.
97. **(2026-04-20)** Rules A–I codified in §1.12 covering util/composable/component/widget/view classification, extraction discipline (create a file only if rendered in v-for OR composed by 2+ parents OR a named page section OR a generic shell), folder existence (2+ files of same kind), and import binding convention.
98. **(2026-04-20)** Rule J added to §1.12: `utils/` and `data/` are topic-based libraries, never one-function-per-file. `views/utils/parse-emphasis.js` renamed to `views/utils/markup.js` (not `typography.js` — typography implies CSS/styling; this file *parses* markup like `** **`). `parseEmphasis` stays as a named export. Topic naming rule: a file is named after *what it parses or styles*, never bundling unrelated concerns. Composables exempt (one `useX` per file — hooks own state/lifecycle).
99. **(2026-04-20)** Three new `shared/components/ui/` primitives extracted after auditing all view components for Rule E duplication. `<UiMetric>` (label/value tile with `sm`/`lg` sizes — replaced `.meta-item` ×4 in meta.vue, `.spec` ×4 in card.vue, `.detail-spec` ×4 in detail.vue), `<UiChip>` (lowercase pill with `solid`/`overflow` — replaced `.use-case-tag` in card.vue and `.detail-tag` in detail.vue, both `v-for`), `<UiBadge>` (uppercase status pill with `active`/`dim` — replaced `.status-badge` in card.vue and `.detail-status` in detail.vue). ~40 lines of duplicated SCSS deleted from the three consumers. All three classify as `ui/` (pure props→DOM, no side-effects — Rule B not triggered). Names chosen per established web-UI vocabulary: **metric** (dashboard tile, Grafana/Datadog), **chip** (MD/PrimeVue/Quasar — distinct from HTML tags), **badge** (uppercase status pill) — avoiding ambiguous terms like "stat" or "tag".
100. **(2026-04-20)** Build-time gotcha captured: `defineProps()` in `<script setup>` is hoisted outside setup scope, so its `validator` arrow function cannot close over local `const` arrays. Inline validator arrays directly (`validator: (v) => ['sm', 'lg'].includes(v)`) — never extract to a `VALID_X` const in the same SFC.
101. **(2026-04-20)** Explicit widget-classification validation pass on all `shared/` files. `hud/timer.vue` confirmed as a component (not a widget): zero lifecycle hooks, zero composable calls, zero side-effects — purely props→DOM. The OBS subscription lives in the parent (`cam-log.vue` calls `useRecordingStatus`) and passes resolved values as props. Every other file also confirmed correctly placed. Only `audio-meter.vue` and `live-readout.vue` remain widgets — both own side-effects (composable subscription / `setInterval`) with `onUnmounted` cleanup. No rehoming needed.
102. **(2026-04-20)** `shared/widgets/` split by kind, mirroring `shared/components/hud|ui/` structure. `widgets/hud/` holds HUD-domain / OBS-coupled widgets (`audio-meter.vue`), `widgets/ui/` holds domain-agnostic widgets (`live-readout.vue`). The alias `@widgets` stays at the root; consumers use `@widgets/hud/audio-meter.vue` and `@widgets/ui/live-readout.vue`. Kind vocabulary (hud, ui) is consistent across both component and widget trees.
103. **(2026-04-20)** Rule G refined (then re-refined): filenames describe **purpose**, never repeat the alias/folder kind. `@modals/base-modal.vue` was redundant (alias already says "modal") — reverted to `@modals/base.vue`. Same for `preview.vue`, `detail.vue`. Multi-word is justified only when the single word leaves "X of what?" unanswered — `metric.vue` failed ("metric of what?") → `data-point.vue`, `status.vue` failed ("status shown how?") → `status-dot.vue`. Single-word wins when the word fully conveys the thing (`hero`, `frame`, `icon`, `chip`, `badge`, `card`). Import bindings unchanged (`<BaseModal>`, `<PreviewModal>`, `<DetailModal>`); UI renames updated bindings to `<UiDataPoint>` + `<UiStatusDot>`.
104. **(2026-04-20)** Brand folder restructured to mirror `src/shared/` layout. Web sources moved from `<brand>/<type>/` to `<brand>/sources/<type>/` — eliminates the vocabulary clash where `@kyonax_on_tech/hud/` (web sources) and `@hud/` (shared HUD primitives) both used the word "hud". Brand now has a consistent schema: `brand.js` + `sources.js` + `assets/` + `styles/` + `sources/{hud,animation,scene}/` + reserved `components/{hud,ui}/`, `composables/`, `widgets/{hud,ui}/`. `brand-loader.js` globs updated from `/@*/hud/*.vue` to `/@*/sources/hud/*.vue` (+ animation, scene); `resolveComponent()` key pattern updated to `/${brand}/sources/${type}/${id}.vue`.
105. **(2026-04-20)** `<UiIcon>` extended to auto-scan brand SVG assets. Previously globbed only `@shared/assets/svg/*.svg`; now also globs `/@*/assets/svg/*.svg`. Any brand can drop an SVG in `<brand>/assets/svg/<name>.svg` and reference it as `<UiIcon name="<name>" />`. Brand SVGs override shared SVGs on filename collision (useful for brand-flavored overrides of shared primitives). Cross-brand collisions are last-loaded-wins — document brand-specific SVGs with distinct names to avoid.
106. **(2026-04-20)** Brand theming fixed — moved to SCSS single source of truth. Previously: `@<brand>/styles/_theme.scss` was orphaned (never imported) while `brand.js` carried a `colors` object that `App.vue` injected as inline style vars per-route. Two problems: (a) brand SCSS file was dead code, (b) `brand.js` only held a subset of palette ranges so brands couldn't override border/warning/error shades. Fix: `src/main.js` now globs `/@*/styles/_theme.scss` eagerly so Vite bundles each brand's theme into global CSS; `brand.js` lost its `colors` field entirely; `App.vue` dropped the `brand_theme_vars` computed and only applies the brand class — cascade handles the rest. brand-loader test updated to assert `brand.colors === undefined` (enforcement). Test count 26 → 27.
107. **(2026-04-20)** PR #3 merged into `dev`. `feat-fixes-and-refinement-v3` shipped both Session 7 Part A (kind-folder architecture + 11 aliases + 5 sections + Tier 1 headers + ESLint Vue) and Part B (3 `ui/` primitives + widgets kind-split + filename discipline + topic-library utils + brand sources/ restructure + UiIcon multi-pool + SCSS theming single source of truth) in one coherent merge. New feature branch `feat-brand_kot` created from `dev` for @kyonax_on_tech brand expansion (Phase 3.7 / Session 8 scope). Release PR `dev` → `master` for v0.3 still pending — will accumulate Phase 3.7 work before cutting.
108. **(2026-04-21)** PR #4 opened on GitHub (`dev` → `master`, title "Release v0.4"). Scope: ship Sessions 6 + 7 A+B as v0.4 — the **Architectural Baseline** — without waiting for `feat-brand_kot` (Session 8) to land. Rationale per TD decision "release-cut strategy": Sessions 6+7 form a coherent architectural refactor already validated and merged; tagging now gives downstream brand work a named baseline. Session 8 will land in v0.4.1 or v0.5. Triad (package.json/README/CHANGELOG) still pending on `dev` at PR-creation time.
109. **(2026-04-21)** Release PR composition pattern codified in §1.13. Captures: title patterns (`[vX.Y]:` for feature branches vs. `release: vX.Y — <Name>` for `dev` → `master`), feature-vs-release body shape differences (release PRs are ~225 lines vs ~300, use reference-rather-than-duplicate), release triad surfacing (5 places in the body), title-body alignment rule (title should distill the body's strongest semantic framing), and the full release generation workflow. Built from the practical experience of PR #3 (feature) + PR #4 (release).
110. **(2026-04-21)** PR #4 title refined from generic "Release v0.4" to `release: v0.4 — Architectural Baseline`. Derivation: the phrase "architectural baseline" appears in the summary paragraph + TD decision #3 + Documentation DIAGRAM (3+ occurrences — passes the title-body alignment threshold). Format matches the canonical `release: vX.Y` Kyonax brand pattern for `dev` → `master` PRs. Length 40 chars (well under GitHub truncation). Tag-referenceable by downstream brand-work PRs ("built on the v0.4 architectural baseline").
111. **(2026-04-21)** Cam-log "REC FRAME" static label replaced with a live, brand-owned session-date. New format `UTC ∇ DD.MM.YYYY // DDD` (e.g. `UTC ∇ 21.04.2026 // TUE`) — picked from a 6-option matrix after the user chose option C (`UTC ∇ YY.MM.DD // MON`) then refined to full year + `DD.MM.YYYY`. Glyph `∇` (nabla, U+2207) is visually cyberpunk and renders in SpaceMono; `UTC` reads as a legit timestamp qualifier; `// DDD` tail keeps the segmented HUD style consistent with `SES::name::T00` on the right. Template class renamed from `.rec-frame` → `.session-date` (purpose-describing per Rule G).
112. **(2026-04-21)** `dayjs` (`^1.11.20`) added as the first project runtime dependency beyond `vue` / `vue-router` / `obs-websocket-js`. Chosen over native `Date` / `Intl.DateTimeFormat` after the user confirmed preference — tradeoff accepted: small bundle cost for cleaner format tokens and escape-bracket syntax (`[UTC ∇ ]DD.MM.YYYY[ // ]ddd`). UTC plugin (`dayjs/plugin/utc.js`) is extended at module scope via `dayjs.extend(utc)` before use. Calls chain `dayjs().utc().format(...).toUpperCase()`. Computed **once at setup** — date is stable for the browser-source lifetime; no midnight-crossing refresh (acceptable for a recording session's lifetime). If a future source needs rolling date, wrap in a composable.
113. **(2026-04-21)** `<UiStatusDot>` swapped from hard `step-end` blink (`opacity: 1 ↔ 0.2` flicker) to smooth `breathe` animation: `2s ease-in-out infinite`, `opacity: 1 ↔ 0.35`, `box-shadow: 0 0 4px ↔ 2px` of `var(--clr-error-100)`. Matches the canonical "recording red light" idiom (camera REC indicator, hardware record LEDs). Header comment updated: "blinks" → "breathes", prop doc notes the soft glow pulse. Ripples to every consumer of `<UiStatusDot>` (currently `<HudTimer>` in `cam-log.vue`; future overlays get it for free).
114. **(2026-04-21)** Preview-modal iframe-scale sub-pixel bug fixed in `src/views/components/modals/preview.vue:137`. Root cause: `element.clientWidth` returns **rounded integer** pixels; when the stage container renders at e.g. `1070.9999…px`, `clientWidth` returns `1071` → `scale = 1071/1920 = 0.5578125` → scaled iframe width `1920 × 0.5578125 = 1071.0px` **overflows** the real container by ~1px. User identified the workaround empirically (changing the scale from `0.5578125` → `0.5578124` fixed the visual offset). Proper fix: swap to `element.getBoundingClientRect().width` — returns sub-pixel float, scale lands correctly regardless of fractional container width. **Rule captured:** for scale-derived CSS custom properties, always read container width via `getBoundingClientRect()`, never `clientWidth` / `offsetWidth`.
115. **(2026-04-21)** Cam-log top-label sizing + positioning refined. `.session-date` and `.cam-online` both reduced from `hud-label-base`'s default `--fs-475` → `--fs-425` (−50 on the granular 25-step scale), then repositioned to `top: 5em` / `left,right: 4.7em` to hold visual anchor after font-size change (em units are relative to element font-size, so the original `4.5em`/`4em` offsets had to grow proportionally to compensate for the smaller type). `.toolkit-id` bottom-right badge bumped `--fs-300` → `--fs-350` for readability. Pattern captured: when changing a HUD label's `font-size`, always recompute its `em`-based position to preserve the intended anchor.
116. **(2026-04-22)** FPS regression post-mortem — the `cyberpunk-glow` mixin (previously in `src/app/scss/abstracts/_mixins.scss`) applied to every `--clr-primary-100` text element was diagnosed as the cause of OBS Browser Source fps drops on low-core-processor devices. Each gold label spawned its own animated `box-shadow` keyframe → the compositor paid a layer-rasterization cost per element per frame. Mixin + `.cyberpunk-glow` utility class + `sass:math` import removed. Blanket-glow-via-utility pattern **banned** (§1.14.2). Glow reborn as explicit opt-in token (`--hud-glow`). This single decision triggered the Session 10 cascade — #117 through #129 are the refactors that restore the cyberpunk aesthetic without the FPS cost.
117. **(2026-04-22)** Halo/glow lifted from SCSS mixin into CSS custom properties declared on `:root` in `src/app/scss/abstracts/_theme.scss`. Four tokens: `--hud-halo` (`filter: drop-shadow()` chain, 3 layers — dark outline + soft fall-off), `--hud-halo-text` (equivalent `text-shadow` chain for text-only elements), `--hud-glow` (brand-primary glow, bound to `--hud-glow-color` so consumers can re-point it to error/success colors without redefining the chain), `--hud-group-gap` (shared 0.6em vertical rhythm for grouped HUD labels). Plus `--clr-primary-100-80` / `--clr-primary-100-40` via `color-mix()` for bar-shadow tinting. Consumers compose: `filter: var(--hud-halo)` on containers, `text-shadow: var(--hud-halo-text), var(--hud-glow)` on leaves. **Explicit opt-in, not cascade-applied.** Legibility memory `project_hud_legibility_strategies.md` now stale on implementation pointers (mixin → tokens) — rationale about "why no blend modes" still valid.
118. **(2026-04-22)** `useRecordingStatus`, `useSceneName`, `useAudioAnalyzer` converted to **module-level singletons** following the existing `useObsWebsocket` pattern. Each now owns a `let shared_state = null;` at module scope; repeat calls return the same object. Callers dropped the `{ obs, connected }` parameters — the composables call `useObsWebsocket()` themselves. N mounted HUD leaves consuming recording state now share one event handler registration instead of N. `watch(connected, ..., { immediate: true })` replaces the old `onMounted` fetch to guarantee initial sync even if the composable is first called after the WS connects. No `onUnmounted` cleanup — singletons live for page lifetime, matching the OBS-WS contract. Singleton identity asserted by `composables.test.js` (`expect(a).toBe(b)`).
119. **(2026-04-22)** `useAudioAnalyzer` rewritten for zero-allocation event-driven operation. Changes: (a) levels/smoothed stored in preallocated `Float32Array(bar_count)` instead of `Array.from(...)`; (b) per-frame `Math.random()` replaced by a 256-entry `JITTER_TABLE: Float32Array` seeded once at module load, cursor-advanced per tick; (c) `requestAnimationFrame` render loop **deleted** — the composable now ticks off the OBS `InputVolumeMeters` event (~50 Hz); (d) reactivity surface reduced to a single `tick: ref(0)` counter + three low-churn refs (`active`, `source_name`); bar levels are NOT reactive — consumers watch `tick` and read `levels` synchronously; (e) target input hardcoded to `Mic/Aux` (was a runtime `source_name` option) — one known target eliminates a `find()` call per event. Result: one reactive write per event (the tick increment) instead of per-frame array allocation + 16-entry reactive update.
120. **(2026-04-22)** `<AudioMeter>` writes bar transforms directly to the DOM via template refs, bypassing Vue reactivity in the hot path. Architecture: (a) template renders `v-for` of static `<div ref="bar_els" class="bar" />` — no inline `:style` binding; (b) a `watch(tick, ...)` callback reads `levels[i]` synchronously and assigns `el.style.transform = SCALE_STRINGS[idx]`; (c) `SCALE_STRINGS` is a precomputed 101-entry `Array` of `scaleY(0.00)`...`scaleY(1.00)` strings (quantized to 2 decimals) — no per-frame string concat; (d) write-threshold skip: if `|scale − last_scale[i]| < 0.01`, skip the DOM write entirely; (e) `update:state` emit throttled to ~10 Hz via `performance.now()` delta. Bar CSS switched from animated `height` → `transform: scaleY(...)` with `transform-origin: bottom` so all animations are GPU-composited with no layout cost. `source_name` prop removed from `<AudioMeter>` — the analyzer owns the target, consumers just mount the meter.
121. **(2026-04-22)** `contain: layout paint` CSS containment applied to every frequently-updating HUD sub-tree: `.hud-group`, `.status-bar`, `.audio-meter`, `.recording-timer`, `.ui-data-point`, `.debug-info`, `.hud-frame`, `.cam-log-overlay`. Isolates paint and layout from siblings so a bar-height update in the audio meter cannot force a layout pass on unrelated HUD labels. Chose `layout paint` (not `strict` or `size`) so brand-sized content still reflows correctly when the HUD canvas resizes. Codified in §1.14.4.
122. **(2026-04-22)** `brand.js` schema extended with two new fields: `host` (short uppercase stream/workstation label, e.g. `"KYO-LABS"`) and `region` (uppercase country/geographic tag, e.g. `"COL"`). Consumed by `cam-log.vue`: `host` renders as the top-left non-primary label; `region` replaces the previously hardcoded `"UTC"` in the session-date format string, becoming `${region} ∇ DD.MM.YYYY // ddd`. Brand-theming rule unchanged: colors still live exclusively in `@<brand>/styles/_theme.scss`. These are identity/context fields only — no runtime side-effects. All future brand bootstraps include them.
123. **(2026-04-22)** `cam-log.vue` layout rewritten around a `.hud-group` structural system. Replaces ad-hoc absolute-positioned single-label classes (`.rec-frame`, `.cam-online`, `.identity-block`) with four positioned groups (`.group--top-left`, `.group--top-right`, `.group--bottom-left`, `.group--identity`) each containing vertically-stacked labels. Label markup unified under `.hud-text` + `.hud-text--primary` classes. Deprecates the `labels` prop on `<HudFrame>` for brand HUDs — structural grouping + explicit label markup is clearer than a labels map. `.dynamic-layer` extracted as a sibling of `<HudFrame>` to separate static HUD chrome from OBS-state-driven widgets (timer + audio meter + debug readout).
124. **(2026-04-22)** Preview-modal `window.resize` handler debounced to 100 ms (`RESIZE_DEBOUNCE_MS`). Before: every resize event triggered `applyScale()` → `getBoundingClientRect()` + CSS custom property write — during window drag this fires ~60 Hz and paints the iframe at every size. After: a trailing 100 ms `setTimeout` collapses the burst into one recompute when the user pauses. Timer cleared on `onUnmounted`. Codified in §1.14.7.
125. **(2026-04-22)** Vue emit event names normalized to kebab-case. `consume_trigger` → `consume-trigger` in both `<PreviewModal>` (`defineEmits`) and `<Card>` (`@consume-trigger` handler binding). Aligns with Vue's convention of declaring emit names in kebab-case and binding with `@kebab-case` on templates; snake_case remains the project convention for *props* (`:is_open`, `:elapsed_time`) per Rule I, but *events* follow the framework idiom.
126. **(2026-04-22)** Color harmonization — `var(--clr-neutral-200)` replaced with `var(--clr-neutral-100)` across view-layer text (footer, sources-section tabs / count badge / filter-input / placeholder / toolbar, card requires-overflow, preview modal meta-labels, badge `--dim` variant). Brand theme's `--clr-neutral-100` lightened from `hsl(0 0% 95%)` → `hsl(0 0% 85%)` to keep enough tonal separation from the neutral-50 (headline) tier. Net effect: secondary text gains legibility headroom against black backgrounds without changing the design-token count.
127. **(2026-04-22)** Unused SCSS utilities purged from `src/app/scss/abstracts/_mixins.scss`: `cyberpunk-glow` mixin (perf-fatal — #116), `max-media-query` mixin (unused anywhere), `@use "sass:math"` (only consumer was `cyberpunk-glow`). New `hud-text-base` mixin added as the minimal label-text core (font family, size, uppercase, letter-spacing, color); `hud-label-base` now composes it + adds `position: absolute` (positioned variant). Consumers that just want the typography tokens use `hud-text-base`; consumers that position themselves via the base use `hud-label-base`.
128. **(2026-04-22)** `<UiStatusDot>` composition refined for per-frame paint cost. Before: one `<span>` with animated `opacity` + `box-shadow` inside a single `breathe` keyframe — the browser re-rasterized the (expensive) shadow every frame. After: outer `<span>` owns the STATIC dark halo (`box-shadow: var(--hud-halo-text)`, never animated); inner `<span class="glow">` owns a red `box-shadow: var(--hud-glow)` with `opacity: 0` by default, animated to a `1 ↔ 0.35` pulse via a 2s ease-in-out `breathe` keyframe while `.active`. Only `opacity` changes per frame — the dark shadow stays cached. `--hud-glow-color` scoped inside `.glow` to `var(--clr-error-100)` so the halo-color-mix resolves to red without the root element changing color. Split-static-from-animated pattern codified in §1.14.3.
129. **(2026-04-22)** `composables.test.js` added at `src/shared/composables/composables.test.js` covering the three newly-singleton composables. Mocks `useObsWebsocket` at module scope (stub `obs` + a `connected` ref). For each of `useRecordingStatus`, `useSceneName`, `useAudioAnalyzer`: asserts documented initial state (`is_recording.value === false`, `scene_name.value === ''`, `levels instanceof Float32Array` of length 16, `tick.value === 0`) AND asserts the singleton identity contract (`expect(a).toBe(b)` with same sub-references). 6 new tests, test count 27 → 33. No assertions on event-driven mutation yet — singleton-identity + initial-state contract first; dynamic-behavior tests follow after the audio analyzer (#119) stabilizes.
130. **(2026-04-23 → 2026-04-26)** Performance discipline exported into the `code-review` skill at `/home/kyonax/.config/doom/gptel-directives/skills/code-review/`. Initial form (2026-04-23): added `rules/brand-detection.md` + `rules/brand-kyonax.md` mirroring pr-scribe's brand-aware rule-loading pattern — git-remote-driven detection, atomic Kyonax brand rule covering the seven §1.14 rule groups (CSS cost, OBS WS budget, zero-allocation hot path, reactivity boundary, event listener hygiene, pre-merge checklist, general conventions), MR detection left untouched. **Refactored form (2026-04-26):** the skill was externally rewritten to a more sophisticated three-tier detection (brand → project → tech-stack) with **135 atomic one-rule-per-file rules** across 15 directories, parallel worker dispatch (max 8 workers, min 5 rules per directory), 6-stage shell-script-driven flow (PR fetch → discovery → CI gate → triage → AI review → format → resolution), and an audit mode for PR review without code changes. Brand-Kyonax rules now live at `brand/kyonax/` as **21 atomic rules** instead of one monolithic file. Implication: reviewing any Kyonax repo auto-loads the FPS discipline; reviewing a HUD source loads `framework/vue3/` + `framework/vue3-composition/` + `brand/kyonax/` + `universal/` (~80 rules total), dispatched across parallel Sonnet workers with per-worker targeted prompts.
131. **(2026-04-23)** Lint-warnings acknowledgment — the 15 non-blocking warnings on hot-path files (8 in `use-audio-analyzer.js`, 5 in `audio-meter.vue`, 1 in `brand-loader.js`, 1 in `cam-log.vue`) are NOT defects. 14 are `security/detect-object-injection` static-analysis false positives on intentional typed-array indexing (`levels[i]`, `JITTER_TABLE[cursor]`, `els[i].style.transform = SCALE_STRINGS[idx]`) — patterns MANDATED by §1.14.6. The 1 `no-magic-numbers` is cosmetic on `.toFixed(3)` in cam-log debug-text formatting. Treat lint as **green at 0 errors**; warning count is informational. If future cleanup is desired: add file-level `/* eslint-disable security/detect-object-injection */` with an inline comment pointing at §1.14.6, OR extract `DEBUG_PEAK_PRECISION = 3` for the magic number. Either is optional — neither is required to merge.
132. **(2026-04-26)** Commit message hyper-concise convention codified in §1.15. Form: subject ≤ 60 chars (Conventional Commits) + 1-paragraph rationale + verb-led bullets (grouped by subsystem when 3+ files share a concern) + single validation line. Detailed file inventory lives in `COMMIT.org` BELOW the `#+END_SRC` gitcommit block as org-mode metadata, never inside the commit body. Captured after the user collapsed a verbose 180-line draft of `feat(kot): brand refinement + OBS FPS perf pass` into 27 lines. PR bodies keep their full Pattern B shape — different audiences (git log scanning vs GitHub review) get different densities.
133. **(2026-04-26)** RECKIT roam-node system bootstrapped at `~/.brain.d/roam-nodes/`. Index file at root (`2026-04-26-index_reckit.org`, UUID `93c9b466-676d-48bf-9d1b-ec8b93816b5d`); architecture + naming-conventions + plan nodes split into `~/.brain.d/roam-nodes/reckit/` subfolder. Old monolithic `content_creation/2026-04-17-162011-reckit_project.org` removed (architecture UUID preserved on the split file). New skill `reckit-roam-node` at `dot-files/.config/doom-mac/gptel-directives/skills/reckit-roam-node/` mirrors `mr-roam-node` shape but uses git+gh state instead of JIRA: 6 rule files (templates, gh-parsing, org-mode-reference, index-management, node-lifecycle, writing-standards). Three plan variants: Standard (Plan), Bug, Release. Two-layer index: BACKLOG (org-roam links) + PLAN BOARD (lane derived from `gh pr list` + `git log` + `git tag --contains`). Lifecycle codified in §1.19.
134. **(2026-04-26)** Plan #context-screen (UUID `cabd1489-23cc-4ce3-9825-7b5a8eb065b9`) — second `@kyonax_on_tech` web source, on new `context-screen` branch off `dev`. News-style HUD targeting a CONTEXT scene (separate from MAIN where `cam-log` lives): lower-third strip + marquee row + toggleable right sidebar that renders any `.org`-authored context (headlines, lists, checklists, tables, code blocks, `#+RESULTS:`, quotes, links) — content authored as small `.org` files at `@kyonax_on_tech/data/contexts/<slug>.org`, control surface on landing page (`<ContextControlModal>` triggered from a third "CONTROLS" button on the source card). Cross-page state propagation via the new bridge documented in §1.16. Plan node carries 7 R-items (R1, R2a-R2f, R3-R7), 15 D-decisions (D1-D15), 7 phases / 46 tasks. All phases shipped this session (44/46 — 2 deferred to user: OBS smoke test + git commit/push).
135. **(2026-04-26)** D1: BroadcastChannel over OBS WebSocket relay for landing→HUD control plane — keeps OBS WS budget free per §1.14.5. D2: `.org` as authoring format (Emacs symmetry with project's `.org` governance docs). D3: `uniorg-parse` AST + custom Vue renderer (battle-tested parser at ~25 KB gzipped beats a 400+ LoC custom regex parser as soon as scope exceeds title+description+code; AST cached at glob-time, walked by `<UiOrgContent>` template branches). D4: library + active-selector model via `<ContextControlModal>` (multiple `.org` files coexist, user picks active one). D5: hybrid 3-surface layout (lower-third + marquee + sidebar with dynamic split — strip width transitions 100% ↔ 62% on toggle). D6: code blocks render in sidebar (consequence of D5 — keeps strip minimal). D7: `.org` v1 schema lock (required `#+TITLE` + `#+DESCRIPTION`; optional `#+SUBTITLE`, `#+TAGS`, `#+begin_marquee` block; body = anything supported by uniorg). All in Plan #context-screen DECISIONS section.
136. **(2026-04-26)** D8: `--surface-bg` gradient token at `:root` (`linear-gradient(135deg, hsl(0 0% 2%) 0%, hsl(0 0% 7%) 50%, hsl(0 0% 14%) 100%)`). D9: Geomanist titles + SpaceMono body (Geomanist .ttf pulled from sibling `kyo-web-online` — `--font-display` token alongside existing `--font-mono`; NOTICE updated with Atipo Foundry credit). D10: per-node styling for `<UiOrgContent>` (Geomanist headlines, SpaceMono prose, custom checkbox glyphs `▢ ▣ ▨`, table borders, src-block lang label, `#+RESULTS:` OUTPUT label, etc.).
137. **(2026-04-26)** D11: recursive Vue template walk inside `<UiOrgContent>`, NEVER `v-html` — project's `eslint.config.mjs` `no-restricted-syntax` rule bans `innerHTML` assignment, and `v-html` compiles to `innerHTML` under the hood. Token-based render: each Shiki token → `<span :style="{ color }">{{ tok.content }}</span>`. Hard rule for any future render of arbitrary user content.
138. **(2026-04-26)** D12: reuse existing `@ui/` primitives over inventing new ones. Sidebar tag chips → `<UiChip variant="solid" shape="square">`; active-slug indicator → `<UiBadge variant="active">`; modal shell → `<BaseModal>`; iframe scaling → `getBoundingClientRect()` (decision #114 pattern). Only new primitive this plan introduced: `<UiOrgContent>` at `@ui/org-content.vue`. `<UiChip>` extended with `shape` prop (`pill | square`, both currently `border-radius: 0`; pill reserved for a future redesign that may introduce radius).
139. **(2026-04-26)** D13: animation discipline — single easing curve `--motion-strip-ease: cubic-bezier(0.22, 1, 0.36, 1)` across all motion; shared duration `--motion-sidebar-ms: 280ms` for sidebar slide + lower-third reflow + peek-arrow fade (sync'd off the `.context-sidebar-open` class flip); `--motion-peek-pulse-s: 2.5s` for closed-state peek breathe; `--motion-marquee-s: 90s` for marquee scroll. All animations are `transform`/`opacity` only EXCEPT the one acknowledged exception: `.context-strip` `width` transition on sidebar toggle (~280 ms × 2 toggles per session — amortized layout cost). D14: closed-sidebar peek indicator with layered halo + animated opacity (mirrors `<UiStatusDot>` decision #128). D15: sharp corners across all context-screen surfaces (`border-radius: 0` mandatory).
140. **(2026-04-27)** D11 enforcement strengthened — when integrating `shiki@^4.0.2` for syntax highlighting (theme `tokyo-night`), used `codeToTokens` API (returns 2-D `tokens[line][token]` array with `content + color`) and rendered tokens through Vue's template engine as `<span :style="{ color: tok.color }">`. Skipped `uniorg-rehype` and `shiki/codeToHtml` entirely — both produce HTML strings that would require `v-html`. Bundle: ~25 KB gzipped for shiki core + onig wasm; per-language grammars lazy-loaded as separate chunks. localhost-only delivery — no network cost concern.
141. **(2026-04-27)** Cross-process bridge revised. Initial implementation used Vite HMR custom events via `import.meta.hot.send/on` with a `server.ws.on/send` plugin. This proved unreliable in OBS browser source (CEF process) — the WebSocket handoff or import.meta.hot init silently failed in CEF, leaving the OBS source on the "NO CONTEXT ACTIVE" placeholder. Replaced with HTTP polling + push through a Vite middleware on `/__context_state` (GET returns current state, POST replaces it; state held in a closure on the dev server). Composable polls every 300 ms via `fetch`, pushes via POST on every local action, with `last_pushed_hash` echo suppression. Universal across browser processes; debuggable with `curl`. Pattern codified in §1.16.
142. **(2026-04-27)** Color shade tokens added at `:root`: `--clr-primary-100-{02, 03, 04, 06, 10, 14}` (low-alpha gold tints) and `--clr-neutral-50-{02, 04, 12}` (low-alpha white tints) — replaces all inline `rgba(255, 215, 0, 0.0X)` and `rgba(255, 255, 255, 0.0X)` literals across the codebase (16+ occurrences in card.vue, context-control.vue, chip.vue, preview.vue, setup.vue, sources.vue). For one-off opacities, three SCSS helpers in `_mixins.scss`: `alpha-tint($color-var, $percent)`, `darken($color-var, $by)`, `lighten($color-var, $by)` — each takes a CSS variable NAME (e.g. `--clr-primary-100`) and emits a runtime `color-mix()` expression. Pattern codified in §1.17.
143. **(2026-04-26 → 2026-04-27)** Corner-square HUD chrome treatment — every "container with bg" gets a 1px solid border AND 4.5px squares at corner intersections. Two mixin variants in `_mixins.scss`: `corner-dots($color, $size, $corners)` for interior surfaces (background-image-based, sits flush with corner via `background-origin: border-box`); `corner-square-tl($color, $size)` + `corner-square-tr($color, $size)` for surfaces where the square should overflow the border (pseudo-element-based with negative offsets `-$size * 0.5`, requires consumer to be a positioned element + NOT use `contain: paint`). Per-corner exclusion based on canvas-edge proximity (corners at canvas edge get NO square). Color rule: white squares (`--clr-neutral-50`) on full-black surfaces; subtle border (`--clr-border-100`) for border lines. Pattern codified in §1.18.
144. **(2026-04-27)** Strip + sidebar share a single 1px border at their interior boundary (when sidebar is open). Strip has `border: 1px solid var(--clr-border-100); border-right: none` so the sidebar's left border is the only visible line at the strip-right ↔ sidebar-left intersection — prevents the 2px-stacking artifact. When sidebar is closed, strip's right edge sits at canvas right edge (no missing visual since canvas has no boundary). Strip's TR corner square overflows ABOVE the sidebar via `z-index: 110` on `.context-lower` (sidebar at z=100) plus `contain: layout` (no `paint` — paint would clip the overflowing pseudo-element).
145. **(2026-04-27)** Sidebar manually scrollable. Body switched from `overflow: hidden` + translateY trick to native `overflow-y: auto`. Auto-scroll now drives `body.scrollTop` programmatically via the rAF tick. User interaction (`wheel` / `touchstart`) sets `last_user_interaction_at = performance.now()`; auto-scroll pauses for `USER_INTERACTION_PAUSE_MS = 5000` after any input. Native scrollbar hidden via `scrollbar-width: none` + `::-webkit-scrollbar { width: 0 }`. Custom lateral indicator was tried then removed (per user — scroll progress UI not needed; the auto-scroll behavior speaks for itself).
146. **(2026-04-27)** Whitespace markers in code blocks. `splitWhitespaceSegments(content)` helper splits each Shiki token's content into alternating runs (whitespace vs non-whitespace). Whitespace runs render as `<span class="org-src-block__ws">{{ raw_spaces }}</span>` with a CSS overlay: `linear-gradient(to right, transparent calc((1ch - 2px)/2), var(--ws-marker), transparent calc((1ch + 2px)/2))` sized `1ch × 2px`, `repeat-x` — paints a 2px square at the center of every `1ch` column. Per-block `--ws-marker` CSS variable: src/example use `--clr-neutral-300`, OUTPUT uses `--clr-primary-300` (darker gold).
147. **(2026-04-27)** List/checklist line-wrap fix. `<li>` originally contained a `<p>` paragraph (block-level → broke to new line under bullet/checkbox). Fix: unordered list-items use `display: flex; align-items: baseline; gap: 0.45em` so bullet/checkbox glyph + paragraph sit on the same line. Ordered list-items keep block flow so the browser-rendered `list-style: decimal` numbering still works. Headline marker also revised: was a 0.4em corner-bracket (border-left + border-top L-shape); now a `3px` vertical bar (border-left only) — reads as `| TITLE` cleanly.

### 2.4 Pending Work

**Recently completed:**
*   [x] ~~Commit staged Part B files + update PR #3 body via `gh pr edit 3 --body-file -`.~~ (2026-04-20) Done manually by user.
*   [x] ~~Merge PR #3 into `dev`.~~ (2026-04-20) Done — `feat-fixes-and-refinement-v3` merged; Sessions 7 A+B shipped together.
*   [x] ~~Open release PR `dev` → `master` for v0.4.~~ (2026-04-21) PR #4 open with refined body via pr-scribe + title `release: v0.4 — Architectural Baseline` in PR.org. Ready to paste via `gh pr edit 4 --title "..." --body-file -`.

**Immediate — release triad (gates PR #4 merge):**
*   [ ] **CRITICAL:** `package.json` bump to `0.4.0` on `dev` — `npm version 0.4.0 --no-git-tag-version`.
*   [ ] **CRITICAL:** `README.org` version markers on `dev` — four `sed -i -E` patches per the PERMANENT runbook in `COMMIT.org`.
*   [ ] **CRITICAL:** `CHANGELOG.org` on `dev` — new `[v0.4] — 2026-04-21 :: Architectural Baseline` block with Added/Changed/Removed/Decided subsections above `[v0.3]`.
*   [ ] Post-merge: tag `v0.4` on `master` — `git tag -a v0.4 -m "RECKIT v0.4" && git push origin v0.4`.
*   [ ] Post-tag (optional): `gh release create v0.4 --title "RECKIT v0.4" --notes-file <changelog-slice>`.
*   [ ] Post-release (optional, one-time): enable branch protection on `master` (require PR review + status checks).

**On current branch `feat-brand_kot` (Phase 3.7 — Session 8/9/10 scope):**
*   [x] ~~Cam-log: replace static "REC FRAME" label with dayjs UTC session-date (`UTC ∇ DD.MM.YYYY // DDD`); retune `.session-date` + `.cam-online` to `--fs-425` with `top: 5em` / `left,right: 4.7em`; bump `.toolkit-id` to `--fs-350`.~~ (2026-04-21)
*   [x] ~~`<UiStatusDot>`: swap hard blink for smooth `breathe` animation (2s ease-in-out, opacity + box-shadow pulse) — the canonical recording-light idiom.~~ (2026-04-21)
*   [x] ~~`<PreviewModal>`: fix iframe sub-pixel scale offset by reading container width via `getBoundingClientRect().width` instead of `clientWidth`.~~ (2026-04-21)
*   [x] ~~Add `dayjs` (`^1.11.20`) as runtime dep + enable `utc` plugin.~~ (2026-04-21)
*   [x] ~~**FPS regression** — diagnose + remove `cyberpunk-glow` mixin; replace with opt-in `--hud-halo` / `--hud-halo-text` / `--hud-glow` CSS custom properties on `:root`.~~ (2026-04-22)
*   [x] ~~Convert `useRecordingStatus`, `useSceneName`, `useAudioAnalyzer` to module-level singletons (one WS subscription per event per page).~~ (2026-04-22)
*   [x] ~~Rewrite audio analyzer event-driven: Float32Array + 256-entry JITTER_TABLE + no rAF; drive ticks off `InputVolumeMeters`.~~ (2026-04-22)
*   [x] ~~`<AudioMeter>` direct DOM writes via template refs; quantized SCALE_STRINGS (101 entries); write-threshold skip; emit throttling (~10 Hz); `transform: scaleY()` over animated `height`.~~ (2026-04-22)
*   [x] ~~`contain: layout paint` on every frequently-updating HUD sub-tree.~~ (2026-04-22)
*   [x] ~~`<UiStatusDot>`: split into static halo shell + animated-opacity `.glow` layer.~~ (2026-04-22)
*   [x] ~~`cam-log.vue` layout rewrite around `.hud-group` system (4 positioned groups).~~ (2026-04-22)
*   [x] ~~`brand.js` schema: add `host` + `region` fields; `region` drives the session-date prefix.~~ (2026-04-22)
*   [x] ~~Preview-modal resize debounced 100 ms.~~ (2026-04-22)
*   [x] ~~Emit event names kebab-cased (`consume_trigger` → `consume-trigger`).~~ (2026-04-22)
*   [x] ~~Color harmonization `neutral-200` → `neutral-100` across views.~~ (2026-04-22)
*   [x] ~~Purge `max-media-query` + `cyberpunk-glow` mixins + `sass:math` import from `_mixins.scss`; add `hud-text-base` mixin.~~ (2026-04-22)
*   [x] ~~New `composables.test.js` — 6 tests covering initial state + singleton identity contract. Test count 27 → 33.~~ (2026-04-22)
*   [x] ~~Codify §1.14 Performance Budget — reverse-engineered discipline from the perf pass.~~ (2026-04-23)
*   [x] ~~Run lint + tests on the staged perf-pass diff. 0 errors, 15 non-blocking warnings (typed-array indexing per §1.14.6 + 1 cosmetic), 33/33 tests passing in 371ms.~~ (2026-04-23)
*   [x] ~~Export §1.14 discipline into the `code-review` skill — initially as `rules/brand-detection.md` + `rules/brand-kyonax.md` (pr-scribe-style brand loading); subsequently refactored externally into three-tier detection with 135 atomic rules across 15 directories.~~ (2026-04-23 → 2026-04-26)
*   [x] ~~Regenerate `COMMIT.org` (hyper-concise — see §1.15) + `PR.org` (full Pattern B Kyonax Feature PR) for the perf pass + brand refinement.~~ (2026-04-23, refined 2026-04-26)
*   [x] ~~Codify §1.15 hyper-concise commit-message convention.~~ (2026-04-26)
*   [ ] User stages remaining unstaged perf iterations (10 files) + new `composables.test.js`, runs commit using `COMMIT.org` body, opens PR using `PR.org`. NEVER run git write commands on user's behalf.
*   [ ] Build `item-explain.vue` at `@kyonax_on_tech/sources/animation/item-explain.vue` (follows the new kind-alias conventions — imports from `@hud/*`, `@widgets/hud/*`, `@widgets/ui/*`, `@composables/*`; template tags in PascalCase). Update `sources.js` status to `'ready'` once the file lands. **Must conform to §1.14 from day 1.**
*   [ ] Brand-private primitives as needed at `@kyonax_on_tech/components/{hud,ui}/`, `@kyonax_on_tech/composables/`, `@kyonax_on_tech/widgets/{hud,ui}/` — create each folder only when 2+ files of that kind exist (Rule F). Cross-brand primitives stay in `src/shared/`.
*   [ ] Brand SVGs at `@kyonax_on_tech/assets/svg/` — any SVG dropped there is auto-picked up by `<UiIcon name="<file>" />` via the multi-pool glob; brand SVGs win filename collisions with shared SVGs.
*   [ ] Brand theme tuning in `@kyonax_on_tech/styles/_theme.scss` — single source of truth for the brand palette (never re-add `colors` to `brand.js`; enforced by brand-loader test).
*   [ ] Regenerate `COMMIT.org` + `PR.org` (via pr-scribe) for Session 9 work (see §5) when ready to PR `feat-brand_kot` back into `dev`.

**Cross-branch / ongoing:**
*   [ ] Broaden Vitest coverage beyond the 27 baseline tests — especially the 5 section components, the 4 renamed shared primitives, the 3 new `ui/` primitives, and the brand theming chain (integration test that `.brand-<handle>` resolves to expected CSS vars).
*   [ ] Shared widgets showcase on landing page (would be a new `@sections/widgets.vue` or a dedicated route).
*   [ ] Fix YouTube subscriber badge subscribe link.
*   [ ] **OPEN SOURCE:** Contribute to `wallyqs/org-ruby` for GitHub alert syntax in `.org` files.
*   [ ] Phase 4 (packaging, scene collection export).

---

## SECTION 3: IMPLEMENTATIONS

### 3.1-3.6 Foundation (compressed — Sessions 1-5)

Identity, README, licensing stack, CI (6 jobs), ESLint, OBS environment. All stable and documented in prior compaction. Tier 1 headers applied to all root files.

### 3.7 Vue 3 App

**Created:** 2026-04-14 | **Last updated:** 2026-04-20
**Status:** Working. Brand-driven architecture + SCSS-cascade theming.

**Stack:** Vue 3 + Vite 6 + Vue Router 4 + obs-websocket-js 5 + sass + eslint-plugin-vue + @typescript-eslint + Vitest 4.1.

**Entry chain:** `index.html` → `src/main.js` (eager-globs all brand theme SCSS) → `App.vue` (applies `.brand-<handle>` class) → `src/router.js` (dynamic routes from `brand-loader.js`).

**Routes:** Auto-generated from `SOURCES` via `resolveComponent()`. Each source with a matching `.vue` file gets a route at `source.path`. Home at `/`, catch-all blank.

**Brand discovery (brand-loader.js):**
- `import.meta.glob('/@*/brand.js', { eager: true })` → `BRANDS`
- `import.meta.glob('/@*/sources.js', { eager: true })` → `SOURCES`
- `import.meta.glob('/@*/sources/hud/*.vue')` — lazy HUD source loaders
- `import.meta.glob('/@*/sources/animation/*.vue')` — lazy animation source loaders
- `import.meta.glob('/@*/sources/scene/*.vue')` — lazy scene source loaders
- `resolveComponent(source)` resolves the key `/${source.brand}/sources/${source.type}/${source.id}.vue`.

**Other eager globs at bootstrap:**
- `src/main.js` — `/@*/styles/_theme.scss` (brand themes inlined into global CSS)
- `src/shared/components/ui/icon.vue` — `@shared/assets/svg/*.svg` + `/@*/assets/svg/*.svg` (two-pool SVG discovery for `<UiIcon>`)

### 3.8 @kyonax_on_tech Brand

**Created:** 2026-04-13 | **Last updated:** 2026-04-22
**Status:** Active. 1 HUD source ready (rewritten in Session 10), 1 animation planned.

**`brand.js`:** handle `@kyonax_on_tech`, identity (Cristian D. Moreno), social links. **NEW fields (2026-04-22):** `host: 'KYO-LABS'`, `region: 'COL'` — consumed by `cam-log.vue` labels. NO colors — those live exclusively in `styles/_theme.scss`.
**`sources.js`:** CAM-LOG (hud, ready) + ITEM-EXPLAIN (animation, planned).
**`sources/hud/cam-log.vue`:** HUD overlay at `/@kyonax_on_tech/cam-log`. Uses `getBrand()` for identity/host/region data. Imports from `@hud/*`, `@widgets/hud/*`, `@widgets/ui/*`, `@composables/*`, plus `dayjs` + `dayjs/plugin/utc.js` for the session-date.

Structural layout (Session 10 rewrite — see §1.12 for kind rules, §1.14 for perf rules):

```
.cam-log-overlay  (contain: layout paint)
├── <HudFrame width=1920 height=1080>   — brackets + crosshair
│   ├── .hud-group .group--top-left    — { brand.host, .session-date (primary) }
│   ├── .hud-group .group--top-right   — { [SESSION] id, CAM ONLINE (primary) }
│   ├── .hud-group .group--bottom-left — { brand.identity.author, display_handle (primary) }
│   ├── .toolkit-id                    — RECKIT {VERSION_TAG}
│   └── .offline (v-if !connected)
└── .dynamic-layer  (sibling; separates OBS-state widgets from HUD chrome)
    ├── .status-bar — <HudTimer> + <AudioMeter>
    └── .debug-info — <LiveReadout text="WS:... AUDIO:... L0:...">
```

  - **Top-left primary label (`.session-date`):** live dayjs UTC date formatted `${brand.region} ∇ DD.MM.YYYY // DDD` (e.g. `COL ∇ 22.04.2026 // WED`) — computed once at setup via `dayjs().utc().format('[${region} ∇ ]DD.MM.YYYY[ // ]ddd').toUpperCase()`. `--fs-425`, inherits halo/glow via `.hud-text--primary`.
  - **Top-right primary label (`.cam-online`):** mirrors `.session-date` — `CAM ONLINE`. Secondary row above it: `[SESSION] ${scene_name}::T${take_count}` with `.bracket` spans translateY(-0.12em).
  - **Bottom-right (`.toolkit-id`):** `RECKIT {VERSION_TAG}`, `--fs-350`.
  - **Status bar:** `<HudTimer>` + `<AudioMeter>`. `<HudTimer>` composes `<UiStatusDot active="{is_recording}" />` which **breathes** red when recording (decision #113 + #128 for the static-halo / animated-glow split). `<AudioMeter>` writes bar `transform: scaleY()` directly to the DOM via template refs, driven by the `tick` counter from the singleton `useAudioAnalyzer` (decisions #119 + #120).
  - **Halo/glow:** `.hud-text--primary` composes `text-shadow: var(--hud-halo-text), var(--hud-glow)` (opt-in per element). `<HudFrame>` root carries `filter: var(--hud-halo)` so the bracket SVGs inherit the dark outline automatically.
**`styles/_theme.scss`:** CSS class `.brand-kyonax-on-tech` with full color overrides — auto-loaded by `src/main.js` (single source of truth for the brand's palette, see §1.5). `--clr-neutral-100` lightened to `hsl(0 0% 85%)` (Session 10) for secondary-text legibility headroom.
**`assets/`:** currently holds development reference images (`temp_obs_inspo_*`). The reserved `assets/svg/` subfolder is auto-scanned by `<UiIcon>` if/when brand SVGs are added.

### 3.9 Landing Page (`src/views/`)

**Created:** 2026-04-15 | **Last updated:** 2026-04-20
**Status:** Production-ready. Kind-folder split. Thin `home.vue` composer.

`home.vue` (~80 lines) composes 5 sections: `<StatsSection>`, `<HeroSection>`, `<SetupSection>`, `<SourcesSection>`, `<FooterSection>`. Sources section owns filter state (search / status / brand tabs). `<Card>` (v-for child) triggers `<PreviewModal>` + `<DetailModal>` — both compose `<BaseModal>`. Sections consume `<UiBadge>`, `<UiChip>`, `<UiDataPoint>` primitives from shared/ui. Utils: `markup.js` (topic library, exports `parseEmphasis`). Grid: 1col mobile, 2col sm, 3col lg. All imports via kind aliases (`@sections`, `@elements`, `@modals`, `@ui`, `@shared`).

### 3.10 Tier 1 File Header System

**Created:** 2026-04-17 | **Status:** Applied to 13 files.

Place names: the void (.gitignore), the forge (vite.config.js), the precinct (eslint.config.mjs), the gate (index.html), the lab (.gitattributes), the vault (.env.example), the watchtower (ci.yml), the checkpoint (release.yml), the logs (CHANGELOG.org), the bridge (README.org), the pact (LICENSING.org), the law (CLAUDE.md), the dojo (CONTRIBUTING.org), the shield (SECURITY.org), the scroll (PULL_REQUEST_TEMPLATE.md).

### 3.11 ESLint Vue + Naming Convention

**Created:** 2026-04-17 | **Status:** Enforced on all JS and Vue files.

`eslint-plugin-vue` with `flat/essential` processor + `flat/strongly-recommended`. `@typescript-eslint/naming-convention` via `@typescript-eslint/parser` + `tsconfig.eslint.json` (`allowJs: true`). 27 tests passing, 0 lint errors (7 pre-existing false-positive warnings in `use-audio-analyzer.js` array indexing).

### 3.12 Brand-Driven Architecture

**Created:** 2026-04-17 | **Status:** Complete. Auto-discovery working.

Replaced centralized `src/shared/data/overlays.js` + `src/brands/` with per-brand `@brand/` folders at project root. `brand-loader.js` discovers brands and sources at build time. Routes generated dynamically. Brand theme injected via `App.vue` (CSS class + inline style vars).

### 3.13 Component Kind-Folder Architecture

**Created:** 2026-04-20 | **Last updated:** 2026-04-20
**Status:** Complete. Applied project-wide. Canonical reference: §1.12.

Split `views/` and `shared/` component layers by **kind** (what type of thing it is), not by page or region. Mirror symmetry — kind folders in both trees.

**Views layer** (`src/views/components/`):
- `sections/` — top-level page regions. Five extracted from `home.vue`: `stats.vue` (`<StatsSection>`, renamed from `meta.vue`), `hero.vue` (`<HeroSection>`), `setup.vue` (`<SetupSection>`), `sources.vue` (`<SourcesSection>`), `footer.vue` (`<FooterSection>`).
- `elements/` — reusable list items (v-for children). Currently: `card.vue` (`<Card>`), renamed from `overlay-card.vue`.
- `modals/` — overlay surfaces (shell + instances). Three files: `base.vue` (`<BaseModal>`), `preview.vue` (`<PreviewModal>`), `detail.vue` (`<DetailModal>`). Filenames stay single-word per Rule G — alias `@modals/` supplies "modal".

**Shared layer** (`src/shared/`):
- `components/hud/` — HUD-domain primitives: `frame.vue` (`<HudFrame>`), `timer.vue` (`<HudTimer>`). Renamed from `hud-frame.vue` / `recording-timer.vue`.
- `components/ui/` — domain-agnostic primitives: `icon.vue` (`<UiIcon>`, generic SVG loader via `import.meta.glob('@shared/assets/svg/*.svg', { eager: true, query: '?raw' })`), `status-dot.vue` (`<UiStatusDot>`, 6×6 square with a **layered composition** (decision #128) — outer `<span>` owns the STATIC dark halo `box-shadow: var(--hud-halo-text)` (never animated); inner `<span class="glow">` owns a red `box-shadow: var(--hud-glow)` at `opacity: 0` by default, animated to a 1 ↔ 0.35 pulse via `breathe 2s ease-in-out infinite` while `.active`. Only `opacity` changes per frame — dark shadows stay cached. Multi-word answers "status shown how? as a dot"), `data-point.vue` (`<UiDataPoint>`, label/value tile with `sm`/`lg` size variants + opt-in `text-shadow: var(--hud-halo-text), var(--hud-glow)` — multi-word answers "metric of what? a data point, i.e. a label paired with a value"), `chip.vue` (`<UiChip>`, lowercase pill with `solid`/`overflow` variants — "chip" is a visual primitive per Material Design / PrimeVue / Quasar), `badge.vue` (`<UiBadge>`, uppercase HUD status pill with `active`/`dim` variants).
- `widgets/hud/` — HUD-domain widgets. `audio-meter.vue` (`<AudioMeter>`) — calls `useAudioAnalyzer({ obs })`, OBS-coupled.
- `widgets/ui/` — domain-agnostic widgets. `live-readout.vue` (`<LiveReadout>`) — owns `setInterval` for throttled text display, no project coupling.
- `composables/` — Vue-aware JS hooks (unchanged): 4 `use-*.js` files.
- `assets/svg/corner-bracket.svg` — raw SVG asset replacing the deleted `components/corner-bracket.vue`.

**`home.vue` slim-down:** ~655 lines → ~80. Composes 5 section tags. Filter state (`search_query`, `status_filter`, `selected_brand`) moved from `home.vue` into `sources.vue` where it's actually consumed.

**Seven new Vite kind-aliases** in `vite.config.js`: `@sections`, `@elements`, `@modals`, `@ui`, `@hud`, `@widgets`, `@composables`. Alias count grew from 4 to 11 — every kind-folder has one (Rule H).

**Four-layer naming pattern** (see §1.12.4): file (kebab-case short) → alias (`@<kind>/`) → import binding (PascalCase with kind suffix/prefix) → template tag (matches binding). Vue `<script setup>` resolves tags from the import binding, not the filename, so short filenames work without triggering `vue/multi-word-component-names`.

**Kind suffix/prefix registry** (see §1.12.5): `@sections` → `XxxSection`, `@modals` → `XxxModal`, `@elements` → `Xxx` (no suffix), `@ui` → `UiXxx`, `@hud` → `HudXxx`, `@widgets` → descriptive-as-is.

**Rules A–I** codified in §1.12.6: util/composable boundary (imports from `'vue'`?), widget/component boundary (calls composable or owns side-effects?), ui/domain boundary (project vocabulary?), shared/views boundary (could a brand overlay consume it?), extraction discipline (v-for OR 2+ parents OR named page section OR generic shell — otherwise inline), folder existence (2+ files of same kind), component naming (filename = kebab short, identity = import binding), alias discipline (kind folder = alias), import binding convention (PascalCase tags matching binding).

**Files touched in the refactor (Session 7 Part A — 18 staged pre-refinement):** `vite.config.js` (aliases), `@kyonax_on_tech/hud/cam-log.vue` (imports + tags), 4 shared moves with content edits, 4 view moves, 5 new section files, 1 new `ui/icon.vue`, 1 new `assets/svg/corner-bracket.svg`, 1 deleted `shared/components/corner-bracket.vue`, `src/views/home.vue` (thinned).

**Session 7 Part B (post-reset refinements):** 3 new `ui/` primitives (`UiDataPoint`, `UiChip`, `UiBadge` + consumer migrations), widgets split by kind (`widgets/hud/`, `widgets/ui/`), 5 filename renames (meta→stats, metric→data-point, status→status-dot + modal reverts), `views/utils/parse-emphasis.js` → `views/utils/markup.js`, brand folder restructured (`<brand>/sources/<type>/`), `<UiIcon>` multi-pool (shared + brand SVGs), brand theming SCSS single source of truth.

**Validation (final):** 27/27 tests pass, 0 lint errors (7 pre-existing warnings in `use-audio-analyzer.js`), production build clean in 1.25s.

### 3.14 Brand Theming Architecture

**Created:** 2026-04-20 | **Last updated:** 2026-04-20
**Status:** Complete. SCSS is the single source of truth for brand palettes.

Three-layer flow:

```
BUILD TIME
  src/app/scss/abstracts/_theme.scss        @<brand>/styles/_theme.scss
  ┌──────────────────────────┐              ┌──────────────────────┐
  │ :root {                  │              │ .brand-<handle> {    │
  │   --clr-*, --fs-*,       │              │   --clr-*: override  │
  │   --font-*, --hud-*, ... │              │ }                    │
  │ }                        │              └─────────┬────────────┘
  └─────────┬────────────────┘                        │ eager glob in
            │ @use in main.scss                       │ src/main.js
            ▼                                         ▼
            └───── bundled into dist/assets/index-*.css ─────┘

RUN TIME
  Route change → router.js attaches meta.brand on the route
    → App.vue sets <div class="brand-<handle>">
      → CSS cascade resolves var(--clr-*) to brand override
        or :root default fallback
```

**Key architecture rules:**

- `brand.js` does NOT carry colors. It holds `handle`, `name`, `description`, `identity`, `links` only. Enforcement: `brand-loader.test.js` asserts `brand.colors === undefined`.
- `@<brand>/styles/_theme.scss` is the single source of truth for the brand palette. Scoped inside `.brand-<handle> { ... }`. Auto-loaded at app boot by `src/main.js` via `import.meta.glob('/@*/styles/_theme.scss', { eager: true })`.
- `src/App.vue` only announces the brand context by applying the class (`class="brand-<handle>"`) on the root `<div>`. Zero inline style vars, zero JS theme computation.
- Unlisted variables fall back to `:root` defaults from `src/app/scss/abstracts/_theme.scss`.

**Why SCSS-single-source over JS-inline-vars:** full-palette override (not a hard-coded subset), zero JS runtime cost, CSS-cascade native, HMR-friendly, single source of truth.

**SCSS-comment gotcha documented:** `*/` inside a block-comment glob pattern closes the comment prematurely. Use line comments (`// ...`) in SCSS file headers whenever referencing glob paths.

**Verified:** `grep brand-kyonax-on-tech dist/assets/*.css` matches after `npm run build` — brand theme is truly in the bundle.

**HUD halo/glow tokens (Session 10 addition):** `src/app/scss/abstracts/_theme.scss` declares four design tokens on `:root` — `--hud-halo` (filter chain), `--hud-halo-text` (text-shadow chain), `--hud-glow` (brand-primary glow, rebindable via `--hud-glow-color`), `--hud-group-gap` — plus `--clr-primary-100-80` / `--clr-primary-100-40` via `color-mix()`. These are **globally-scoped utilities**, not brand-scoped overrides: brands CAN redefine them inside `.brand-<handle>` if they want a different glow color, but the baseline chain lives at `:root` so `src/shared/` components can consume `var(--hud-halo)` without caring which brand is active. Opt-in per element — never applied via a catch-all utility class (decision #116 lesson).

### 3.15 Performance Pass (Session 10)

**Created:** 2026-04-22 | **Last updated:** 2026-04-23
**Status:** Implementation complete on `feat-brand_kot`; pending commit + PR.

Session-wide FPS-preservation refactor triggered by the `cyberpunk-glow` mixin diagnosed as an OBS Browser Source FPS killer. Rationale: RECKIT HUDs are composited live; the source MUST hold OBS canvas fps on low-core-processor hardware. Every change here trades a small visual/ergonomic cost for measurable frame-time budget.

**Techniques applied** (reverse-engineered and codified in §1.14):

| # | Technique | Decision | File(s) |
|---|---|---|---|
| 1 | Remove broad CSS filter utility; replace with opt-in design tokens | #116, #117 | `_mixins.scss`, `_theme.scss` |
| 2 | Module-level singleton composables | #118 | `use-recording-status.js`, `use-scene-name.js`, `use-audio-analyzer.js` |
| 3 | Event-driven (no rAF) + preallocated typed arrays + precomputed jitter table | #119 | `use-audio-analyzer.js` |
| 4 | Direct DOM writes via template refs (bypass Vue reactivity); quantized string-lookup table; write-threshold skip; emit throttling; GPU-composited `transform: scaleY()` | #120 | `audio-meter.vue` |
| 5 | `contain: layout paint` on HUD sub-trees | #121 | ~8 components |
| 6 | Split static/animated layers (static halo + animated-opacity glow) | #128 | `status-dot.vue`, `timer.vue` |
| 7 | Debounce burst-prone `window.*` listeners | #124 | `preview.vue` |

**Measurement note:** no formal profiling data captured in this session — regression diagnosis was perceptual (HUD "felt slow in OBS") + elimination-based (removing `cyberpunk-glow` restored fps). Future sessions should capture OBS `Stats` numbers (rendering lag, FPS, encoder overload) before and after any HUD change for objective evidence.

**Tests added:** `src/shared/composables/composables.test.js` — 6 tests covering initial state + singleton identity for the three newly-singleton composables. Mocks `useObsWebsocket` at module scope. Test count 27 → 33.

**Pending validation:** user will run `npm test` + `npm run lint` + browser-source smoke test in OBS before committing.

### 3.16 Context-Screen Web Source (Session 12)

**Created:** 2026-04-26 | **Last updated:** 2026-04-27
**Status:** Implementation complete on `context-screen` branch; pending commit + PR + OBS smoke test.

Second `@kyonax_on_tech` web source. News-style HUD targeting a CONTEXT scene — surfaces per-video context (title + description + tags + talking-point marquee + full deep-dive sidebar) authored as a small `.org` file per video at `@kyonax_on_tech/data/contexts/<slug>.org`. State synchronizes from the landing page (`<ContextControlModal>` triggered from a third "CONTROLS" button on the source card) to every mounted browser source via the cross-process bridge (BroadcastChannel for same-process speed + HTTP polling on `/__context_state` Vite middleware for OBS CEF — see §1.16).

#### File structure (Session 12 net-new)

```
src/
├── app/
│   ├── fonts/Geomanist/                 NEW — pulled from kyo-web-online
│   │   ├── GeomanistRegular.ttf
│   │   ├── GeomanistBold.ttf
│   │   └── GeomanistItalic.ttf
│   └── scss/
│       ├── abstracts/
│       │   ├── _theme.scss              MOD — +--font-display, +--surface-bg,
│       │   │                                  +--motion-{sidebar-ms,strip-ease,
│       │   │                                              peek-pulse-s,marquee-s},
│       │   │                                  +--clr-primary-100-{02,03,04,06,10,14},
│       │   │                                  +--clr-neutral-50-{02,04,12}
│       │   └── _mixins.scss             MOD — +alpha-tint/darken/lighten functions,
│       │                                       +corner-dots, +corner-square-tl/tr
│       └── base/_typography.scss        MOD — +3 Geomanist @font-face declarations
├── shared/
│   ├── brand-loader.js                  MOD — +CONTEXTS glob, +getContexts(),
│   │                                          +buildContextsMap()
│   ├── brand-loader.test.js             MOD — +7 CONTEXTS shape/discovery tests
│   ├── components/ui/
│   │   ├── chip.vue                     MOD — +shape prop (pill | square)
│   │   ├── org-content.vue              NEW — recursive Vue AST renderer (D11);
│   │   │                                       per-node styling per D10; whitespace
│   │   │                                       markers via splitWhitespaceSegments
│   │   └── org-content.test.js          NEW — 11 mount tests
│   ├── composables/
│   │   ├── use-context-channel.js       NEW — singleton + BroadcastChannel +
│   │   │                                       HTTP poll/push + localStorage debounce
│   │   │                                       + .context-sidebar-open class toggle
│   │   └── composables.test.js          MOD — +5 useContextChannel tests
│   └── utils/
│       ├── org.js                       NEW — Rule J topic library wrapping
│       │                                       uniorg-parse; OrgSchemaError;
│       │                                       parseOrg/extractMetaKey/extractFiletags/
│       │                                       extractMarqueeBlock/collectBodyNodes
│       ├── org.test.js                  NEW — 14 tests covering happy + error paths
│       └── highlight.js                 NEW — Shiki async wrapper, theme=tokyo-night,
│                                              codeToTokens API, 18-language allowlist,
│                                              memo cache keyed by lang::code
├── views/components/
│   ├── elements/card.vue                MOD — +CONTROLS button (conditional on
│   │                                          source.id === 'context-screen'),
│   │                                          +<ContextControlModal> mount,
│   │                                          +.card-secondary-actions flex row
│   └── modals/context-control.vue       NEW — slug list + sidebar toggle + live
│                                              preview iframe (reuses BaseModal +
│                                              getBoundingClientRect scaling)
└── ...

@kyonax_on_tech/
├── data/contexts/                       NEW
│   ├── obs-browser-sources.org          NEW — fixture-rich (every D10 node type)
│   └── quick-note.org                   NEW — minimal no-marquee fixture
├── sources.js                           MOD — +context-screen registry entry
└── sources/hud/context-screen.vue       NEW — full HUD source (~370 LoC)

vite.config.js                            MOD — +context_relay_plugin (HTTP middleware)
package.json + package-lock.json          MOD — +uniorg-parse@^3.2.1, +shiki@^4.0.2
NOTICE                                    MOD — +Third-Party Fonts section (Geomanist
                                                + SpaceMono credits)
```

#### Component layout (the four HUD surfaces)

```
.context-screen-overlay        position: fixed; inset: 0
├── .context-lower             absolute bottom-0 width=100%/62% (dynamic split);
│   │                          z-index: 110 (overlays sidebar at boundary);
│   │                          contain: layout (NOT paint — TR square overflows).
│   ├── .context-strip         border 1px (border-right: none — sidebar owns
│   │                          the shared boundary); corner-square-tl + tr in
│   │                          --clr-neutral-50 (white squares overflow corners
│   │                          with negative offsets via pseudo-elements).
│   │                          Bg: --clr-neutral-500 (full black).
│   │   ├── __title            font-display Geomanist, --fs-700, color:
│   │   │                       --clr-neutral-200 (no full white per user)
│   │   ├── __subtitle         font-display, --fs-400 (optional)
│   │   └── __description      font-mono SpaceMono, --fs-300
│   └── .context-marquee       Bg: --clr-primary-100 (gold); color: --clr-neutral-500;
│                              NO border, NO corner squares (per user — clean strip);
│                              translateX(0% → -50%) infinite at --motion-marquee-s
│                              (90s); items repeated 3× per half then doubled for
│                              seamless loop; 3px-square ::after separator between
│                              items.
├── .context-sidebar           absolute top-0 right-0 width=38% height=100%;
│   │                          z-index: 100; bg: --clr-neutral-500; border 1px;
│   │                          corner-square-tl only (TR + BR at canvas edge,
│   │                          BL dropped per design); slide-in via translateX
│   │                          + opacity over --motion-sidebar-ms (280ms).
│   ├── __header               border-bottom 1px; ::after = white square at
│   │                          left end of separator (right end at canvas edge).
│   │                          Renders parsed.tags as <UiChip variant="solid"
│   │                          shape="square">.
│   ├── __body                 overflow-y: auto (native scroll); scrollbar
│   │                          hidden via scrollbar-width: none. Auto-scrolls
│   │                          body.scrollTop after 3s open delay at 16 px/s
│   │                          via rAF; pauses 5s on user wheel/touch input;
│   │                          bottom-hold 3s + reset to top + loop.
│   │                          Mounts <UiOrgContent :ast="parsed.body_ast" />.
│   └── (no lateral indicator — removed per user)
└── .context-peek              absolute right-edge, vertically centered;
                               z-index: 99; bg: --clr-neutral-500; border 1px;
                               corner-dots all 4 corners (small floating element);
                               glyph "❮"; layered halo (static box-shadow on
                               outer span) + animated opacity (.pulse inner span,
                               1 ↔ 0.55 over --motion-peek-pulse-s ease-in-out
                               infinite); fades to opacity 0 when sidebar opens.
```

#### Org rendering pipeline (build-time + runtime)

```
USER EDITS @kyonax_on_tech/data/contexts/<slug>.org
  ↓
BUILD TIME (Vite glob, eager + ?raw)
  brand-loader.js: import.meta.glob('/@*/data/contexts/*.org', { eager: true, query: '?raw' })
  ↓
  org.js parseOrg(raw_string) — uniorg-parse → unified AST → partition:
    keyword nodes  → metadata { title, subtitle, description, tags }
    marquee block  → marquee_items[] (split by \n, trimmed, empties dropped)
    everything else → body_ast (Array<OrgNode>)
  Required key violation throws OrgSchemaError.
  ↓
  CONTEXTS map exposed: { brand: { slug: { raw, parsed, parse_error } } }
  ↓
RUNTIME (HUD source mounts)
  parsed = computed(() => CONTEXTS[BRAND_HANDLE]?.[active_slug]?.parsed)
  ↓
  <UiOrgContent :ast="parsed.body_ast" /> walks AST recursively:
    - headline → <h2|h3|h4> with corner-bracket replaced by 3px vertical bar (D10 + user 2026-04-27)
    - paragraph → <p>
    - bold/italic/verbatim/code/strike → semantic inline elements
    - plain-list (unordered) → <ul> with <li> in flex baseline (paragraph stays
      on same line as bullet/checkbox per user 2026-04-27)
    - plain-list (ordered) → <ol> with default decimal numbering
    - list-item → <li> with custom checkbox glyphs (▢ ▣ ▨) + color per state
    - table → <table> with thin borders + header row distinct + alt-row stripes
              + corner-dots all 4 corners
    - src-block → <pre> + lang label + Shiki tokenized output (theme tokyo-night,
                  codeToTokens async); each token's content split into whitespace
                  vs non-whitespace runs; WS runs render with --ws-marker overlay
                  showing 2px square per 1ch column
    - fixed-width with affiliated.RESULTS → OUTPUT block (gold-tint bg + gold
      border + corner-dots; OUTPUT label has lifted-black bg + pure-black border
      + neutral-200 text per user 2026-04-27)
    - quote-block → <blockquote> with gold left-border + gold-tint bg
    - link → <a target="_blank" rel="noopener noreferrer">
    - horizontal-rule, etc.
```

#### State propagation

```
USER ACTION on landing page
  ↓
useContextChannel.setActiveSlug(slug) / toggleSidebar() / hideSidebar()
  ↓
broadcastSnapshot({ active_slug, sidebar_open }):
  - channel.postMessage(snapshot)              ← BroadcastChannel: same-process speed
  - pushState(snapshot) → fetch POST           ← HTTP: cross-process universality
                                                  (last_pushed_hash echo guard set)
  ↓
DELIVERED TO ALL CONSUMERS
  - same-tab + iframe in same browser: BroadcastChannel sub-50ms
  - OBS browser source (separate CEF process): HTTP poll picks up within 300ms
  ↓
applyRemote(snapshot):
  active_slug.value = snapshot.active_slug
  sidebar_open.value = snapshot.sidebar_open
  ↓
WATCHERS:
  - watch([active_slug, sidebar_open]) → schedulePersist (localStorage debounce 100ms)
  - watch(sidebar_open) → applyDocumentClass — toggle .context-sidebar-open on <html>
  - watch(channel.sidebar_open) inside context-screen.vue → start/stop auto-scroll
  - .context-sidebar-open class flips → CSS transitions fire (sidebar slide,
    strip width reflow, peek-arrow fade — all sync'd over --motion-sidebar-ms)
```

#### Key Decisions Table (recap, full detail in plan node)

| ID  | Date         | Decision                                                                        |
|-----|--------------|---------------------------------------------------------------------------------|
| D1  | 2026-04-26   | BroadcastChannel over OBS WS for same-process control plane                     |
| D2  | 2026-04-26   | `.org` as authoring format (not `.json`/`.yaml`)                                 |
| D3  | 2026-04-26   | `uniorg-parse` AST + custom Vue renderer                                         |
| D4  | 2026-04-26   | Library + active-selector via `<ContextControlModal>`                           |
| D5  | 2026-04-26   | Hybrid 3-surface layout (lower-third + marquee + sidebar)                       |
| D6  | 2026-04-26   | Code blocks render in sidebar                                                   |
| D7  | 2026-04-26   | `.org` v1 schema lock                                                           |
| D8  | 2026-04-26   | `--surface-bg` gradient token (later replaced by solid `--clr-neutral-500` per user) |
| D9  | 2026-04-26   | Geomanist (titles) + SpaceMono (body)                                           |
| D10 | 2026-04-26   | Per-node styling for `<UiOrgContent>` (D11 + Shiki)                             |
| D11 | 2026-04-26   | Recursive Vue template walk; never `v-html`                                     |
| D12 | 2026-04-26   | Reuse existing `@ui/` primitives                                                |
| D13 | 2026-04-26   | Animation discipline (single curve + sync'd duration)                           |
| D14 | 2026-04-26   | Closed-sidebar peek indicator (layered halo + opacity)                          |
| D15 | 2026-04-26   | Sharp corners across all context-screen surfaces                                |
| —   | 2026-04-27   | Cross-process bridge → HTTP polling (replaces unreliable Vite HMR for OBS CEF)  |
| —   | 2026-04-27   | 4.5px corner squares via `corner-square-tl/tr` mixins (overflow visible)        |
| —   | 2026-04-27   | Strip drops `border-right` → single shared 1px line with sidebar               |
| —   | 2026-04-27   | Strip's TR square overflows above sidebar (`z-index: 110` on `.context-lower`) |
| —   | 2026-04-27   | Whitespace markers (2px squares per 1ch) in code blocks                         |
| —   | 2026-04-27   | List-item flex-baseline fix (bullet + paragraph on same line)                   |
| —   | 2026-04-27   | Shiki tokyo-night theme; tokens rendered via Vue templates (no v-html)          |
| —   | 2026-04-27   | OUTPUT label: lifted-black bg + pure-black border + neutral-200 text            |

#### Validation

`npm run lint` → 0 errors / 38 expected warnings (intentional typed-array indexing per §1.14.6 + decision #131 — false-positive `security/detect-object-injection` on indexed access).
`npm test` → 79/79 passing in ~900 ms (33 baseline + 14 org parser + 7 brand-loader CONTEXTS + 11 UiOrgContent + 5 useContextChannel + 9 derived from per-source `it.each` assertions added by the new `context-screen` registry entry).
`npm run build` → ~2.1 s. Bundle: index ~67 KB gzipped (unchanged), context-screen route chunk ~70 KB gzipped (Shiki core + onig wasm). Per-language grammar chunks lazy-loaded as separate files.

#### Pre-merge gate (passed)

`grep -rE 'innerHTML\|v-html' src/ @kyonax_on_tech/` → 0 hits (D11 enforcement).
`grep -rE 'document\.write' src/ @kyonax_on_tech/` → 0 hits.
`grep -rE 'border-radius:\s*[1-9]'` on context-screen-related files → 0 hits (D15).
`grep -rE 'rgba\(255'` on src + brand → 0 hits (token discipline §1.17).
Every new file has the MPL-2.0 header. Every new filename is `kebab-case` (only `App.vue` exempt).
§1.14.8 perf-checklist 7/7 pass (no broad CSS filters; only `transform` + `opacity` for continuous animation; one acknowledged `width` exception on strip toggle; no OBS-WS subscriptions; zero allocations per rAF tick; `contain` on every updating sub-tree; resize-debounced 100 ms in `<ContextControlModal>`).

#### Deliverables

`COMMIT.org`: regenerated for `context-screen` branch — subject `feat(context): news-style lower-third + sidebar HUD` (51 chars), 11 verb-led bullets grouped by subsystem, validation line. PERMANENT runbook untouched.
`PR.org`: full Pattern B Kyonax Feature PR via `pr-scribe` skill — title `feat(context): news-style lower-third + sidebar HUD`, 13-item checklist, themed Implementation subsections (Brand assets / Org parser + AST renderer / BroadcastChannel composable / HUD source / Landing-page control surface), Dependencies section, 6 TD-4FIELD decisions, TEST-TWO-TABLE Testing Coverage, QA-HOW-TO-TEST with ASCII flow tree + 7 feature groups, DOC-MEDIA-VOCAB Documentation. Global writing rules satisfied (no emojis except `✅` in test status cells, no arrows, no private-file refs, absolute GitHub URLs).

User runs `git commit -F <(awk ... COMMIT.org)` + `gh pr create --base dev --head context-screen --title "..." --body-file <(awk ... PR.org)`. Per absolute prohibition in repo + global `CLAUDE.md`, this skill never runs git write commands.

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
| `package.json` / `package-lock.json` | Dependencies (`vue`, `vue-router`, `obs-websocket-js`, `dayjs ^1.11.20`) + version |
| `index.html` | the gate |
| `.env.example` | the vault |
| `.gitignore` | the void |
| `.gitattributes` | the lab |

### Roam-node Documentation (Session 12)

| Path | Association |
|---|---|
| `~/.brain.d/roam-nodes/2026-04-26-index_reckit.org` | Index Reckit (root dashboard) |
| `~/.brain.d/roam-nodes/reckit/2026-04-17-reckit_architecture.org` | Architecture (project tree, flows) |
| `~/.brain.d/roam-nodes/reckit/2026-04-20-reckit_naming_conventions.org` | Naming Conventions (Rules A–J) |
| `~/.brain.d/roam-nodes/reckit/2026-04-26-150000-reckit_plan_context_screen.org` | Plan #context-screen |
| `dot-files/.../skills/reckit-roam-node/SKILL.md` | reckit-roam-node skill — entry point |
| `dot-files/.../skills/reckit-roam-node/AGENTS.md` | reckit-roam-node skill — philosophy |
| `dot-files/.../skills/reckit-roam-node/rules/templates.md` | Plan/Bug/Release node templates |
| `dot-files/.../skills/reckit-roam-node/rules/gh-parsing.md` | git+gh state extraction (replaces JIRA polling) |
| `dot-files/.../skills/reckit-roam-node/rules/index-management.md` | 8-step index update flow |
| `dot-files/.../skills/reckit-roam-node/rules/node-lifecycle.md` | 7-step node lifecycle |
| `dot-files/.../skills/reckit-roam-node/rules/org-mode-reference.md` | Org-mode syntax reference |
| `dot-files/.../skills/reckit-roam-node/rules/writing-standards.md` | Writing standards per section |

### Brand: @kyonax_on_tech/

| Path | Association |
|---|---|
| `@kyonax_on_tech/brand.js` | Brand metadata + identity + `host: "KYO-LABS"` + `region: "COL"` (NO colors — those live in `styles/_theme.scss`) |
| `@kyonax_on_tech/sources.js` | Web source registry (data file) |
| `@kyonax_on_tech/sources/hud/cam-log.vue` | CAM-LOG HUD overlay (session-date via dayjs, breathing status dot) |
| `@kyonax_on_tech/sources/hud/context-screen.vue` | CONTEXT-SCREEN HUD overlay (Session 12 — strip + marquee + sidebar + peek) |
| `@kyonax_on_tech/data/contexts/obs-browser-sources.org` | Fixture-rich `.org` context (every D10 node type) |
| `@kyonax_on_tech/data/contexts/quick-note.org` | Minimal no-marquee `.org` fixture |
| `@kyonax_on_tech/styles/_theme.scss` | Brand CSS overrides |
| `@kyonax_on_tech/assets/` | Brand images; `assets/svg/` auto-scanned by `<UiIcon>` |
| `@kyonax_on_tech/components/` | [reserved] brand-private components |
| `@kyonax_on_tech/composables/` | [reserved] brand-private hooks |
| `@kyonax_on_tech/widgets/` | [reserved] brand-private widgets |

### Vue App (src/)

| Path | Association |
|---|---|
| `src/main.js` | Vue bootstrap |
| `src/App.vue` | Root + brand theme injection |
| `src/router.js` | Dynamic routes from brand-loader |

### Landing Page (src/views/) — @views + kind aliases

| Path | Alias | Tag |
|---|---|---|
| `src/views/home.vue` | — | (page root) |
| `src/views/components/sections/stats.vue` | `@sections` | `<StatsSection>` (renamed from `meta.vue` for clarity) |
| `src/views/components/sections/hero.vue` | `@sections` | `<HeroSection>` |
| `src/views/components/sections/setup.vue` | `@sections` | `<SetupSection>` |
| `src/views/components/sections/sources.vue` | `@sections` | `<SourcesSection>` |
| `src/views/components/sections/footer.vue` | `@sections` | `<FooterSection>` |
| `src/views/components/elements/card.vue` | `@elements` | `<Card>` |
| `src/views/components/modals/base.vue` | `@modals` | `<BaseModal>` (alias supplies "modal") |
| `src/views/components/modals/preview.vue` | `@modals` | `<PreviewModal>` |
| `src/views/components/modals/detail.vue` | `@modals` | `<DetailModal>` |
| `src/views/utils/markup.js` | — | `parseEmphasis()` (topic library — Rule J) |

### Shared (src/shared/) — @shared + kind aliases

| Path | Alias | Tag / Export |
|---|---|---|
| `src/shared/brand-loader.js` | `@shared` | `BRANDS`, `SOURCES`, `getBrand`, `resolveComponent` |
| `src/shared/brand-loader.test.js` | — | 27 tests (brands have `links`, do NOT carry `colors`) |
| `src/shared/config.js` | `@shared` | `OBS_CONFIG` |
| `src/shared/version.js` | `@shared` | `VERSION`, `VERSION_TAG` |
| `src/shared/version.test.js` | — | Version tests |
| `src/shared/assets/svg/corner-bracket.svg` | `@shared/assets` | raw SVG |
| `src/shared/components/hud/frame.vue` | `@hud` | `<HudFrame>` |
| `src/shared/components/hud/timer.vue` | `@hud` | `<HudTimer>` |
| `src/shared/components/ui/icon.vue` | `@ui` | `<UiIcon>` |
| `src/shared/components/ui/status-dot.vue` | `@ui` | `<UiStatusDot>` (breathing red dot — 2s ease-in-out opacity + box-shadow pulse when `active`) |
| `src/shared/components/ui/data-point.vue` | `@ui` | `<UiDataPoint>` (label/value tile, `size` prop) |
| `src/shared/components/ui/chip.vue` | `@ui` | `<UiChip>` (lowercase pill, `variant` prop) |
| `src/shared/components/ui/badge.vue` | `@ui` | `<UiBadge>` (uppercase status pill, `variant` prop) |
| `src/shared/components/ui/org-content.vue` | `@ui` | `<UiOrgContent>` (Session 12 — recursive AST renderer, no v-html) |
| `src/shared/components/ui/org-content.test.js` | — | 11 mount tests (Session 12) |
| `src/shared/utils/org.js` | `@shared/utils` | parser topic library (Session 12 — uniorg-parse wrap) |
| `src/shared/utils/org.test.js` | — | 14 parser tests (Session 12) |
| `src/shared/utils/highlight.js` | `@shared/utils` | Shiki async wrapper (Session 12 — tokyo-night) |
| `src/shared/composables/use-context-channel.js` | `@composables` | `useContextChannel()` singleton (Session 12) |
| `src/views/components/modals/context-control.vue` | `@modals` | `<ContextControlModal>` (Session 12 — slug list + sidebar toggle + iframe preview) |
| `src/app/fonts/Geomanist/{Regular,Bold,Italic}.ttf` | — | Geomanist font assets (Session 12, pulled from kyo-web-online) |
| `vite.config.js` | — | MOD: `+context_relay_plugin` HTTP middleware (Session 12) |
| `src/shared/composables/use-obs-websocket.js` | `@composables` | `useObsWebsocket()` (singleton) |
| `src/shared/composables/use-recording-status.js` | `@composables` | `useRecordingStatus()` (singleton — Session 10) |
| `src/shared/composables/use-audio-analyzer.js` | `@composables` | `useAudioAnalyzer()` (singleton, event-driven, Float32Array — Session 10) |
| `src/shared/composables/use-scene-name.js` | `@composables` | `useSceneName()` (singleton — Session 10) |
| `src/shared/composables/composables.test.js` | — | 6 tests — singleton identity + initial state (Session 10) |
| `src/shared/widgets/hud/audio-meter.vue` | `@widgets` (path `hud/`) | `<AudioMeter>` (HUD-domain, OBS-coupled) |
| `src/shared/widgets/ui/live-readout.vue` | `@widgets` (path `ui/`) | `<LiveReadout>` (domain-agnostic) |
| `src/shared/data/` | — | reserved (empty) |
| `src/shared/utils/` | — | reserved (empty) |

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

### What was done last (2026-04-28 — `PR.org` regenerated for `context-screen`, ground-truth refresh)

Single-task follow-up to Session 12: regenerated `PR.org` body via the `pr-scribe` skill (Kyonax brand, Pattern B Feature PR), this time grounding every claim against the actual working tree rather than the original Plan node text. Title preserved (`feat(context): news-style lower-third + sidebar HUD`). Scope of the refresh:

1. **Ground-truth pass against working tree.** Read `git status`, the diff vs `dev`, and the contents of `vite.config.js`, `_mixins.scss`, `use-context-channel.js`, `highlight.js`, `context-screen.vue` to anchor every PR claim. Found that several plan-time values had drifted during refinement: auto-scroll constants `OPEN_DELAY_MS = 3000` (plan said 2000), `SCROLL_SPEED_PX_PER_SEC = 16` (plan said 25), plus a NEW `USER_INTERACTION_PAUSE_MS = 5000` constant added during the manual-scroll iteration.
2. **Implementation realities NOT in the original plan or prior PR.org.** Surfaced four items that landed during Session 12 refinement but never made it into the prior PR body:
    *   **Shiki `tokyo-night` syntax highlighting** — D3 plan explicitly said "no syntax highlighting in v1", but the user later asked for it. Shipped via `src/shared/utils/highlight.js` (async `codeToTokens` + per-`<lang>:<code>` `Map` cache) plus the dedicated `shiki@^4.0.2` runtime dep. Whitespace markers via `splitWhitespaceSegments` + 1ch×2px background-image gradient with per-block `--ws-marker` (neutral-300 for src, primary-300 for OUTPUT).
    *   **HTTP polling cross-process relay** — `context_relay_plugin` Vite plugin mounting middleware on `/__context_state`; `useContextChannel` polls + pushes via `fetch`, echo-suppressed via `last_pushed_hash`. Required because `BroadcastChannel` cannot cross OBS's CEF process boundary (the `import.meta.hot.send` first attempt failed silently). Replaces — not augments — the broadcast-only design from the original plan.
    *   **Corner-square pseudo-element SCSS mixins** — `corner-square-tl($color, $size: 4.5px)` / `corner-square-tr` added to `_mixins.scss` for overflow-visible corner markers; `corner-dots` + `alpha-tint` / `darken` / `lighten` SCSS functions also surfaced. Mixin documented to NOT set `position: relative` itself (consumer handles it) — this was the regression that broke the sidebar's `position: absolute` anchor mid-Session-12.
    *   **Alpha-tint token migration sweep** — `setup.vue`, `sources.vue`, `preview.vue` `rgba(255, 215, 0, 0.04)` literals replaced with `var(--clr-primary-100-04)` shade tokens. Surfaced as polish entries in the Implementation list.
3. **Single shared border / TR overflow** — Strip drops `border-right`; sidebar's left border is the single 1px line; strip's TR corner-square overflows above sidebar via `z-index: 110` + `contain: layout` (no `paint`). Captured in both Implementation prose and the DIAGRAM blockquote.
4. **Test counts + perf gates updated to actual.** 79/79 tests in 826 ms (was 33 / 357 ms baseline); lint 0 errors / 33 expected warnings; build 1.37 s. Runtime deps: `uniorg-parse@^3.2.1`, `shiki@^4.0.2`. PR.org Quality-gates table reflects real numbers.
5. **TD-4FIELD section expanded to 8 decisions.** Added the Shiki tokenization decision and the Corner-square HUD-chrome decision alongside the original 6 (BroadcastChannel+HTTP relay, uniorg-parse, recursive Vue template, hybrid 3-surface layout, sidebar auto-scroll, layered peek indicator, primitive reuse).
6. **QA "How to test" rewritten** — 8-group ASCII flow tree (Setup, Lower-third+marquee, Sidebar toggle, Org rendering coverage with Shiki + per-node styling, Sidebar auto-scroll + interaction pause, Closed-state peek indicator, Cross-process bridge with `curl http://localhost:5173/__context_state`, OBS smoke test deferred). Each step uses `***Expected:***` 6-space-indent bold-italic per Kyonax variant.
7. **Global writing rules sweep.** No emojis outside `✅` in Testing-Coverage status cells; `↔` arrow chars replaced with "to" (`opacity 1 to 0.55 over 2.5 s`); zero private-file references; every repo link absolute to `Kyonax/reckit/blob/master/`.

Final `PR.org`: 307 lines, body inside the standard `#+BEGIN_SRC markdown` block, runbook header preserved.

### State of `context-screen` at reset

*   Branch: `context-screen` (off `dev`, since 2026-04-26). 11 untracked + 16 modified files in working tree. NO commits yet.
*   Validation: `npm run lint` → 0 errors / 33 expected warnings; `npm test` → 79/79 in 826 ms; `npm run build` → 1.37 s.
*   Deliverables ready: `COMMIT.org` (hyper-concise per §1.15) + `PR.org` (Pattern B Kyonax Feature PR, 307 lines, refreshed 2026-04-28). User runs `git commit` + `gh pr create` themselves.
*   OBS smoke test: deferred to user — see the dedicated "OBS smoke test" QA group inside `PR.org`.

### Pending / Not yet started

**Immediate — context-screen ship (this branch):**
*   [ ] User stages all working-tree changes + runs `git commit -F <(awk '/^#\+BEGIN_SRC gitcommit$/{f=1; next} /^#\+END_SRC$/{f=0} f' COMMIT.org)`.
*   [ ] User runs `git push -u origin context-screen` + `gh pr create --base dev --head context-screen --title "$(awk '/^#\+BEGIN_SRC text$/{f=1; next} /^#\+END_SRC$/{f=0} f' PR.org)" --body-file <(awk '/^#\+BEGIN_SRC markdown$/{f=1; next} /^#\+END_SRC$/{f=0} f' PR.org)`.
*   [ ] OBS smoke test at `http://localhost:5173/@kyonax_on_tech/context-screen` — verify cross-process state sync, fps holds, all four surfaces render correctly.
*   [ ] After PR open, `reckit-roam-node` skill's `gh-parsing.md` lane derivation auto-moves Plan #context-screen from `IN PLANNING` → `IN DEVELOPMENT` (first commit) → `IN REVIEW` (PR open) on next index sync.

**Immediate — v0.4 release (UNCHANGED; still gates PR #4 merge):**
*   [ ] **CRITICAL:** `package.json` bump to `0.4.0` on `dev` — `npm version 0.4.0 --no-git-tag-version`.
*   [ ] **CRITICAL:** `README.org` version markers on `dev` — four `sed -i -E` patches per `COMMIT.org` PERMANENT runbook step 3.
*   [ ] **CRITICAL:** `CHANGELOG.org` on `dev` — new `[v0.4] — <date> :: Architectural Baseline` block above `[v0.3]`.
*   [ ] Push refined PR.org body + title to PR #4.
*   [ ] Post-merge: tag `v0.4` on `master`.

**Immediate — Sessions 9 + 10 commit/PR (on `feat-brand_kot`, DELIVERABLES still ready):**
*   [ ] User stages remaining unstaged perf iterations on `feat-brand_kot` + runs `git commit` using that branch's `COMMIT.org`. Title pre-set: `feat(kot): brand refinement + OBS FPS perf pass`.
*   [ ] User opens PR `feat-brand_kot` → `dev` using that branch's `PR.org`.

**On `context-screen` branch (Plan Phase 0 → 6 finished — minor follow-ups):**
*   [ ] `<ContextControlModal>` could grow per-context-block toggles (currently just slug-select + sidebar-on/off). User explicitly de-scoped this in Phase 0 — sidebar is all-or-nothing, lower-third always visible (R2a + D6).
*   [ ] Consider `prefers-reduced-motion: reduce` gate for `breathe` + `pulse` continuous animations + `marquee` scroll (carryover from Session 11 cross-branch list).
*   [ ] Optional cleanup: file-level `/* eslint-disable security/detect-object-injection */` on hot-path files with comment pointing at §1.14.6. Drops the warning count to 0. Not required to merge.

**On `feat-brand_kot` (Phase 3.7 — remaining scope):**
*   [ ] Build `item-explain.vue` at `@kyonax_on_tech/sources/animation/item-explain.vue`. Conform to §1.14 from day 1.
*   [ ] Brand-private primitives under `@kyonax_on_tech/components|composables|widgets/` as needed (Rule F — 2+ files before folder).
*   [ ] Brand SVGs at `@kyonax_on_tech/assets/svg/` — multi-pool glob auto-discovers.

**Cross-branch / ongoing:**
*   [ ] Broaden Vitest coverage — section components, brand-theming integration, AudioMeter DOM-write correctness, tick-driven event propagation.
*   [ ] Shared widgets showcase on landing page.
*   [ ] Fix YouTube subscriber badge subscribe link.
*   [ ] **OPEN SOURCE:** Contribute to `wallyqs/org-ruby` for GitHub alert syntax in `.org` files.
*   [ ] Phase 4 (packaging, scene collection export).

### Where to resume

**Branch & PR context at resume:**
*   Current branch: `context-screen`. Working tree: ~18 files changed/added. No commits yet.
*   Open PRs: **PR #4** (`dev` → `master`, v0.4 release, triad pending).
*   Other open branch: `feat-brand_kot` — uncommitted (deliverables on disk).
*   Today's date at reset: 2026-04-28.

**Resume paths (by task):**

If the user asks to **commit/push the context-screen PR**: do NOT run git write commands yourself (absolute prohibition in repo + global `CLAUDE.md`). Walk them through the awk one-liners in `COMMIT.org` PERMANENT runbook step 9–10, OR simply remind them where the title + body live.

If the user asks to **debug the OBS bridge** (controls not propagating to OBS source): start with `curl http://localhost:5173/__context_state` — should return current state JSON. If empty, the Vite middleware isn't registered (check `vite.config.js` for `context_relay_plugin`). If state updates on landing-page actions but OBS source doesn't reflect: check OBS Studio dev tools (right-click source → Interact → F12) for fetch errors. Pattern fully documented in §1.16.

If the user asks to **regenerate `COMMIT.org`** for context-screen: follow §1.15 (hyper-concise) — subject ≤ 60 chars, 1-paragraph rationale, verb-led bullets grouped by subsystem, one validation line. Files-in-the-diff table BELOW `#+END_SRC`. PERMANENT runbook untouched.

If the user asks to **regenerate `PR.org`** for context-screen: invoke the `pr-scribe` skill (Kyonax brand, Pattern B Feature PR). 8 TD-4FIELD decisions are settled (BroadcastChannel + HTTP relay over OBS-WS, uniorg-parse + custom Vue renderer, recursive Vue template + no v-html, Shiki `tokyo-night` async tokenization, hybrid 3-surface layout, sidebar auto-scroll via `body.scrollTop` + 5 s user-interaction pause, layered peek with halo + animated opacity, corner-square pseudo-element mixins). Always ground claims against the working tree (constants in `context-screen.vue`, deps in `package.json`, plugin in `vite.config.js`) — the plan node text drifted during refinement. 79/79 tests in 826 ms, lint 0 errors / 33 warnings, build 1.37 s. Reference §1.14, §1.16, §1.17, §1.18 inline.

If the user asks to **add a new context `.org`**: drop file at `@kyonax_on_tech/data/contexts/<slug>.org`. Required: `#+TITLE` + `#+DESCRIPTION`. Optional: `#+SUBTITLE`, `#+TAGS: :tag1:tag2:`, `#+begin_marquee … #+end_marquee` block (one item per line). Body: any uniorg-supported construct (headlines, lists, checklists, tables, `#+BEGIN_SRC` + `#+RESULTS:`, quotes, links). Vite picks up via `import.meta.glob`; `<ContextControlModal>` lists it; HMR refreshes both pages on save.

If the user asks to **change the Shiki theme**: edit `SHIKI_THEME` constant in `src/shared/utils/highlight.js`. Available bundled themes include `tokyo-night` (current), `vitesse-dark`, `monokai`, `one-dark-pro`, `dracula`, `github-dark`, `nord`, `synthwave-84`, `catppuccin-mocha`, `rose-pine`, `aurora-x`, `night-owl`. The cache key is `lang::code` (theme not included), so Vite HMR reload auto-rerenders all blocks with the new theme.

If the user asks to **modify the sidebar auto-scroll behavior**: constants in `context-screen.vue` — `OPEN_DELAY_MS = 3000`, `BOTTOM_HOLD_MS = 3000`, `SCROLL_SPEED_PX_PER_SEC = 16`, `USER_INTERACTION_PAUSE_MS = 5000`, `MS_PER_SEC = 1000`. The rAF tick reads paused state via `last_user_interaction_at` and skips increments while paused; `wheel`/`touchstart` listeners on the body bump this timestamp.

If the user asks to **add a new corner-decorated surface**: use `corner-dots($color, $size, $corners)` for interior surfaces (background-image, sits flush). Use `corner-square-tl($color, $size)` + `corner-square-tr($color, $size)` for surfaces where the square should overflow (pseudo-element with negative offsets). Consumer must be a positioned element; consumer must NOT use `contain: paint`. Corners at canvas edge → no square. White squares (`--clr-neutral-50`) on full-black surfaces; matching-hue squares for tinted surfaces (gold for OUTPUT, neutral-300 for src-block, etc.).

If the user asks to **build a new RECKIT plan node**: invoke the `reckit-roam-node` skill. Provide the slug. Skill walks the planning conversation (REQUIREMENTS as GIVEN/WHEN/THEN, OPEN QUESTIONS with Recommendations, DECISIONS as `*D<n>* (date)` blocks, IMPLEMENTATION DISCIPLINE checklist, phased TODO PLAN TASKs). Plan node lives at `~/.brain.d/roam-nodes/reckit/YYYY-MM-DD-HHMMSS-reckit_<slug>.org`. Update Index Reckit BACKLOG + PLAN BOARD on every operation (8-step flow from `index-management.md`). Plan UUIDs are stable references — see §1.19.

If the user asks to **review another HUD feature's performance**: trigger the `code-review` skill — auto-loads `brand/kyonax/` (21 atomic rules) on Kyonax repos. Walk the change against §1.14.8 checklist (broad filter/shadow? non-transform/opacity animation? new OBS-WS subscription? per-frame allocation? per-tick `$emit`? missing `contain: layout paint`? un-debounced burst listener?).

If the user asks to **build a new component** (any kind): follow §1.12 for placement + naming; §1.14 for performance; §1.18 for chrome (corner squares + 1px border) when it's a HUD container. Kebab short filename; kind folder; `@<kind>` alias; PascalCase binding per §1.12.5. Don't extract unless Rule E is met.

If the user asks to **add a new OBS-WS-driven composable**: follow the singleton pattern (§1.14.5): module-level `shared_state`, early-return on repeat calls, `watch(connected, fn, { immediate: true })` for initial fetch, no `onUnmounted`. Add a test to `composables.test.js` asserting initial state + `expect(a).toBe(b)` identity.

If the user asks to **add a new cross-process control plane** (any feature spanning landing page + OBS browser source): follow §1.16. Vite middleware on a path (`/__<feature>_state`); composable polls via `fetch` + pushes via POST; echo-suppress with `last_pushed_hash`. Keep `BroadcastChannel` as same-process accelerator. Confirm with `curl` before testing in OBS.

If the user asks to **add a new cross-brand color shade**: declare at `:root` in `src/app/scss/abstracts/_theme.scss` following the `--clr-<family>-<shade>-<alpha>` naming. Use `color-mix(in srgb, var(--clr-<family>-<shade>) X%, transparent)`. For one-offs, the SCSS helpers `alpha-tint` / `darken` / `lighten` in `_mixins.scss` (§1.17).

If the user asks to **complete the v0.4 release triad**: see §2.4 and the `feat-brand_kot` branch's `COMMIT.org` PERMANENT runbook. Three CRITICAL commands on `dev`, then `gh pr edit 4` and merge. Session 9+10 PR rebases on top of `dev` after that merge; context-screen rebases in turn.

If the user asks for a **new task**: check §2.4 (Pending Work) and `CHANGELOG.org` TODO block.

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
