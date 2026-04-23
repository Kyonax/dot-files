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

**Compaction sources:** Sessions 1-5 (2026-04-08 to 2026-04-15 — brand bootstrap through v0.3 PR system), Session 6 (2026-04-16 to 2026-04-17 — overlay card fixes, modal abstraction, cam-log rename, Tier 1 file headers, ESLint Vue + naming conventions, brand-driven architecture refactor, Vite aliases, CONTRIBUTING.org, SECURITY.org, Doom Emacs config for Vue development), Session 7 Part A (2026-04-20 early — component architecture & naming convention §1.12: views restructured into sections/elements/modals, shared file renames, 7 new kind-aliases, four-layer naming pattern, Rules A–I codified), Session 7 Part B (2026-04-20 late — utils topic libraries §Rule J, three new ui/ primitives, widgets split by kind, ambiguous filename corrections, brand folder restructure into sources/, UiIcon multi-pool SVG discovery, SCSS single source of truth for brand theming, 27/27 tests), Session 8 prep (2026-04-21 — post PR #3 merge + `feat-brand_kot` branch creation; PR #4 opened for `dev` → `master` v0.4 release; release-PR composition pattern codified in §1.13; `release: v0.4 — Architectural Baseline` title chosen via title-body alignment rule; triad pending on `dev`), Session 9 (2026-04-21 continuation — first code on `feat-brand_kot`: cam-log "REC FRAME" replaced with dynamic dayjs UTC session-date `UTC ∇ DD.MM.YYYY // DDD`, status-dot switched from hard blink to smooth breathe animation, preview modal iframe-scale sub-pixel fix via `getBoundingClientRect()`, dayjs added as runtime dep), Session 10 (2026-04-22 → 2026-04-23 — FPS-preservation performance pass on `feat-brand_kot`: blanket `cyberpunk-glow` mixin diagnosed as OBS FPS killer and removed; halo/glow reborn as opt-in design tokens on `:root` (`--hud-halo`, `--hud-halo-text`, `--hud-glow`); `useRecordingStatus`, `useSceneName`, `useAudioAnalyzer` collapsed to module-level singletons; audio analyzer rewritten event-driven off `InputVolumeMeters` with `Float32Array` + precomputed JITTER_TABLE + no rAF; `<AudioMeter>` writes bar transforms directly to the DOM via template refs (quantized SCALE_STRINGS, write-threshold skip, emit throttled ~10 Hz); `transform: scaleY()` replaces animated `height`; `contain: layout paint` applied to every HUD sub-tree; `<UiStatusDot>` split into static halo shell + animated-opacity `.glow` layer; cam-log layout restructured around `.hud-group` system; `brand.js` gains `host` + `region`; preview resize debounced; emit names kebab-cased; neutral-200 → neutral-100 harmonization; new `composables.test.js` (27 → 33 tests); §1.14 Performance Budget codified).

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
| Phase 3.7: @kyonax_on_tech brand expansion (Session 8) | **IN PROGRESS on `feat-brand_kot`** (branched from `dev` 2026-04-20) |
| Phase 4: Polish + packaging | NOT STARTED |

**Current working branch:** `feat-brand_kot` ("feature brand Kyonax-on-Tech") — branched from `dev` after PR #3 merge. Scope: brand-specific work for `@kyonax_on_tech`, likely including `sources/animation/item-explain.vue`, brand-private primitives under `@kyonax_on_tech/components|composables|widgets/`, and/or brand SVGs at `@kyonax_on_tech/assets/svg/`. All conventions from §1.12 (Component Architecture & Naming Rules) + §1.5 (SCSS theming) + §1.11 (Brand Metadata Schema) apply.

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

### Brand: @kyonax_on_tech/

| Path | Association |
|---|---|
| `@kyonax_on_tech/brand.js` | Brand metadata + identity + `host: "KYO-LABS"` + `region: "COL"` (NO colors — those live in `styles/_theme.scss`) |
| `@kyonax_on_tech/sources.js` | Web source registry (data file) |
| `@kyonax_on_tech/sources/hud/cam-log.vue` | CAM-LOG HUD overlay (session-date via dayjs, breathing status dot) |
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

### What was done last (2026-04-22 → 2026-04-23 — Session 10: performance pass on `feat-brand_kot`)

Root-cause FPS incident + reverse-engineered performance discipline. All work on `feat-brand_kot`; no new architectural shifts to §1.12 conventions. One new test file. Net −73 LoC across ~30 files — mostly cleanup on top of a deep hot-path rewrite.

1. **FPS regression diagnosed and mixin removed.** The user had previously applied the `cyberpunk-glow` mixin to every element inheriting `var(--clr-primary-100)` (gold labels, audio meter bars, timer, status dots) to get the signature cyberpunk aura. Inside OBS Browser Source, the HUD felt slow on low-core-processor hardware. Profiling-by-elimination traced it to the mixin: each gold element spawned its own animated `box-shadow` keyframe → the compositor rasterized a new layer per element per frame. Removal path: `cyberpunk-glow` mixin + `.cyberpunk-glow` utility class + `sass:math` import deleted from `_mixins.scss` + `_theme.scss`. Cost principle captured in new §1.14: **never apply `filter` / `box-shadow` / `text-shadow` chains via a utility that hits every element of a color class.**

2. **Halo/glow re-expressed as opt-in design tokens.** Replaced the mixin with four CSS custom properties on `:root` in `src/app/scss/abstracts/_theme.scss` — `--hud-halo` (filter chain), `--hud-halo-text` (text-shadow chain), `--hud-glow` (brand-primary glow, rebindable via `--hud-glow-color`), `--hud-group-gap`. Plus `--clr-primary-100-80/40` via `color-mix()`. Consumers now opt in per element (`filter: var(--hud-halo)` on a container; `text-shadow: var(--hud-halo-text), var(--hud-glow)` on a leaf). Decision #117.

3. **All OBS-WS composables converted to module-level singletons.** `useRecordingStatus`, `useSceneName`, `useAudioAnalyzer` now follow the `useObsWebsocket` pattern: `let shared_state = null;` at module scope; first call wires the WS handler and populates state; subsequent calls return the same object. Callers dropped `{ obs, connected }` params. Rationale: many HUD leaves on one page subscribing to the same event used to register N handlers per event → singleton collapses to 1. `onUnmounted` cleanup dropped (singletons live for page lifetime). Decision #118.

4. **Audio analyzer rewritten event-driven + zero-allocation.** `use-audio-analyzer.js` was the biggest hot path (OBS fires `InputVolumeMeters` ~50 Hz). Changes: preallocated `Float32Array(bar_count)` for levels/smoothed; 256-entry `JITTER_TABLE: Float32Array` seeded once at module load, cursor-advanced per tick (replaces per-frame `Math.random()`); `requestAnimationFrame` loop **deleted** (OBS already gives us a clock); reactivity surface collapsed to a single `tick: ref(0)` counter plus `active` / `source_name` refs — bar levels are NOT reactive, consumers watch the tick and read `levels` synchronously; target input hardcoded to `Mic/Aux` (eliminates per-event `Array.find`). Decision #119.

5. **AudioMeter bypasses Vue reactivity in the hot path.** `audio-meter.vue` no longer uses `:style` binding on bars. Architecture: static `v-for` of `<div ref="bar_els" class="bar" />`; `watch(tick, ...)` callback reads `levels[i]` and assigns `el.style.transform = SCALE_STRINGS[idx]`. `SCALE_STRINGS` is a precomputed 101-entry string table (`scaleY(0.00)`...`scaleY(1.00)`). Write-threshold skip: `|scale − last_scale[i]| < 0.01` → skip DOM write. `update:state` emit throttled to ~10 Hz via `performance.now()`. CSS animation switched from `height` → `transform: scaleY()` with `transform-origin: bottom` — GPU-composited, zero layout cost. `source_name` prop removed (analyzer owns target). Decision #120.

6. **CSS containment + GPU-friendly transforms applied across the HUD.** `contain: layout paint` added to `.hud-group`, `.status-bar`, `.audio-meter`, `.recording-timer`, `.ui-data-point`, `.debug-info`, `.hud-frame`, `.cam-log-overlay`. `<UiStatusDot>` split into static outer halo shell + animated-opacity `.glow` inner span so the dark shadow never re-rasterizes per frame — only `opacity` changes. Decision #121 + #128.

7. **`cam-log.vue` layout restructured around a `.hud-group` system.** Four positioned groups (`top-left`, `top-right`, `bottom-left`, `identity`) replace ad-hoc absolute-positioned labels. Unified `.hud-text` + `.hud-text--primary` classes. `.dynamic-layer` sibling extracted for OBS-state widgets (timer / audio meter / debug readout). Deprecates `labels` prop on `<HudFrame>` for brand HUDs. Decision #123.

8. **`brand.js` gains `host` + `region`.** `@kyonax_on_tech/brand.js` adds `host: 'KYO-LABS'` (top-left non-primary label) and `region: 'COL'` (replaces hardcoded `"UTC"` in the session-date format → `${region} ∇ DD.MM.YYYY // ddd` → `COL ∇ 22.04.2026 // WED`). Colors-in-SCSS rule unchanged. Decision #122.

9. **Smaller polish.** Preview-modal resize handler debounced 100 ms (decision #124). Vue emit event names kebab-cased: `consume_trigger` → `consume-trigger` on `<PreviewModal>` + `<Card>` (decision #125). Secondary text upgraded `--clr-neutral-200` → `--clr-neutral-100` across ~10 view surfaces; brand theme's `--clr-neutral-100` lightened `95%` → `85%` to preserve tonal separation (decision #126). Frame.vue dead `.border-*` divs removed. `LiveReadout` skips no-op writes. Comment stripping across config files matching the "default no comments" CLAUDE.md guidance.

10. **Tests.** New `src/shared/composables/composables.test.js` — 6 tests covering initial state + singleton identity contract (`expect(a).toBe(b)`) for `useRecordingStatus`, `useSceneName`, `useAudioAnalyzer`. Mocks `useObsWebsocket` at module scope. Dynamic-behavior tests deferred. Decision #129.

11. **This reset** — added §1.14 Performance Budget for OBS Browser Sources (the codified discipline, reverse-engineered from this session's work); added decisions #116–#129 to §2.3; updated §3.8 cam-log with the new layout system + brand.host/region consumption; refined §3.13 UiStatusDot composition note; added halo-token line to §3.14; new §3.15 Performance Pass implementation entry; §4 file index adds `composables.test.js`; §1.1 HUD vocabulary updated to note `brand.region` drives the session-date prefix; intro compaction-sources extended.

**Staged + unstaged at reset (~32 files modified + 1 new test file):**

Staged (baseline of the perf pass): `@kyonax_on_tech/brand.js`, `@kyonax_on_tech/sources/hud/cam-log.vue`, `@kyonax_on_tech/styles/_theme.scss`, `eslint.config.mjs`, `src/App.vue`, `src/app/scss/abstracts/_mixins.scss`, `src/app/scss/abstracts/_theme.scss`, `src/app/scss/base/_global.scss`, `src/app/scss/components/_index.scss`, `src/app/scss/layout/_index.scss`, `src/main.js`, `src/shared/brand-loader.js`, `src/shared/components/hud/frame.vue`, `src/shared/components/hud/timer.vue`, `src/shared/components/ui/badge.vue`, `src/shared/components/ui/data-point.vue`, `src/shared/components/ui/status-dot.vue`, `src/shared/composables/use-audio-analyzer.js`, `src/shared/composables/use-obs-websocket.js`, `src/shared/composables/use-recording-status.js`, `src/shared/composables/use-scene-name.js`, `src/shared/version.js`, `src/shared/widgets/hud/audio-meter.vue`, `src/shared/widgets/ui/live-readout.vue`, `src/views/components/elements/card.vue`, `src/views/components/modals/preview.vue`, `src/views/components/sections/footer.vue`, `src/views/components/sections/sources.vue`, `src/views/utils/markup.js`, `vite.config.js`, `package.json`, `package-lock.json`.

Unstaged (in-progress iteration on 10 of the above): `@kyonax_on_tech/sources/hud/cam-log.vue`, `src/app/scss/abstracts/_mixins.scss`, `src/shared/components/hud/frame.vue`, `src/shared/components/ui/data-point.vue`, `src/shared/components/ui/status-dot.vue`, `src/shared/composables/use-audio-analyzer.js`, `src/shared/composables/use-recording-status.js`, `src/shared/widgets/hud/audio-meter.vue`, `src/views/components/elements/card.vue`, `src/views/components/modals/preview.vue`.

Untracked: `src/shared/composables/composables.test.js`.

**No validation run yet** — user will run `npm test` + lint before committing (pre-commit hooks handle linting).

**Documentation sync:** this reset writes the perf discipline into §1.14 (new) + decisions #116–#129 + §3.15 (new). The user also asked for a reverse-engineered rules table at the bottom of the chat response — that table is the conversational counterpart to §1.14's structured form. COMMIT.org + PR.org for Session 10 are pending.

### Pending / Not yet started

**Immediate — v0.4 release (UNCHANGED; still gates PR #4 merge):**
*   [ ] **CRITICAL:** `package.json` bump to `0.4.0` on `dev` — `npm version 0.4.0 --no-git-tag-version`. *Note: Sessions 9 + 10 added deps / tests / code on `feat-brand_kot` — those rides v0.5+, NOT v0.4. Triad lives on `dev` only.*
*   [ ] **CRITICAL:** `README.org` version markers on `dev` — four `sed -i -E` patches (see §1.8 + `COMMIT.org` PERMANENT runbook step 3).
*   [ ] **CRITICAL:** `CHANGELOG.org` on `dev` — new `[v0.4] — <date> :: Architectural Baseline` block above `[v0.3]` with Added / Changed / Removed / Decided subsections.
*   [ ] Push refined PR.org body + title to PR #4 — `gh pr edit 4 --title "release: v0.4 — Architectural Baseline" --body-file <(awk ...)`.
*   [ ] Post-merge: tag `v0.4` on `master`, optional GitHub Release + branch protection.

**Immediate — Session 9 + 10 commit/PR (on `feat-brand_kot`):**
*   [ ] User stages + commits remaining unstaged perf iterations (10 files listed above) + the new `composables.test.js`.
*   [ ] User generates `COMMIT.org` — likely two logical commits: `feat(kot)` for Session 9 HUD polish (if not yet committed) + `perf` for Session 10 (audio pipeline, singletons, halo tokens, containment). Alternatively one combined commit since both are on the same feature branch.
*   [ ] User opens PR `feat-brand_kot` → `dev` via pr-scribe (Kyonax brand, **Feature PR shape**). Title candidate: `feat(kot): live session-date + performance pass (halo tokens, singleton composables, GPU-composited audio meter)`.
*   [ ] Expected next release including Session 9 + 10: **v0.5** — performance discipline is a named baseline feature, not a patch-level fix.

**On current branch `feat-brand_kot` (Phase 3.7 — remaining scope):**
*   [ ] Build `item-explain.vue` at `@kyonax_on_tech/sources/animation/item-explain.vue`. Kind-alias imports; PascalCase tags. Flip `sources.js` to `'ready'` on land. **Must conform to §1.14 from day 1** — no blanket filter/shadow utilities, `transform`/`opacity`-only animations, `contain: layout paint` on the root.
*   [ ] Brand-private primitives under `@kyonax_on_tech/components|composables|widgets/` as needed (Rule F — 2+ files before folder).
*   [ ] Brand SVGs at `@kyonax_on_tech/assets/svg/` — multi-pool glob auto-discovers.
*   [ ] Brand theme tuning at `@kyonax_on_tech/styles/_theme.scss` — single source of truth (never re-add `colors` to `brand.js`).

**Cross-branch / ongoing:**
*   [ ] Broaden Vitest coverage beyond the now-33 baseline — 5 sections, 4 renamed shared primitives, 3 new `ui/` primitives, brand theming integration, `<AudioMeter>` DOM-write correctness (render levels → assert `style.transform`), `tick`-driven event propagation.
*   [ ] Shared widgets showcase on landing page (new `@sections/widgets.vue` or dedicated route).
*   [ ] Fix YouTube subscriber badge subscribe link.
*   [ ] **OPEN SOURCE:** Contribute to `wallyqs/org-ruby` for GitHub alert syntax in `.org` files.
*   [ ] Phase 4 (packaging, scene collection export).
*   [ ] Consider `prefers-reduced-motion` gates on `breathe` and any future continuous pulsations.

### Post-Session-10 status

`feat-brand_kot` now carries: (a) Session 9 HUD polish (live session-date, breathing status, iframe sub-pixel fix, dayjs); (b) Session 10 performance pass (cyberpunk-glow-mixin removal, halo/glow design tokens, 3 singleton composables, event-driven audio analyzer, DOM-direct AudioMeter, CSS containment, GPU-composited transforms, resize debounce, UiStatusDot layered composition, color harmonization, 6 new singleton-contract tests). **§1.14 Performance Budget** now codifies the discipline for all future HUD work. PR #4 (v0.4 Architectural Baseline — `dev` → `master`) still open, triad still pending on `dev`.

### Where to resume

**Branch & PR context at resume:**
- Current branch: `feat-brand_kot`. Staged (Session 10 baseline) + 10 unstaged iterations + 1 new untracked test file (`composables.test.js`).
- Open PRs: **PR #4** (`dev` → `master`, `release: v0.4 — Architectural Baseline`, triad pending on `dev`).
- Merged: PR #3 (`feat-fixes-and-refinement-v3` → `dev`).
- Expected next PR: `feat-brand_kot` → `dev` (post-v0.4 merge). Title candidate: `feat(kot): live session-date + performance pass`.

**Resume paths (by task):**

If the user asks to **commit Session 10 work**: do NOT run `git commit`. Regenerate `COMMIT.org` with a subject under 72 chars summarizing the perf pass (halo tokens, singleton composables, audio-meter GPU path). Consider splitting Session 9 (HUD polish) and Session 10 (perf) into two commits for clean PR-readability, OR one combined commit — user's call. Keep all commits on `feat-brand_kot`; triad stays on `dev`.

If the user asks to **generate the PR for Session 9 + 10**: invoke the `pr-scribe` skill with Kyonax brand, **Feature PR shape** (not Release). Changes block: Pattern B flat `**Changes:**` with `[MOD]` / `[NEW]` tags. Technical Details must cover at minimum: (a) the `cyberpunk-glow` FPS regression + halo-token mitigation; (b) singleton composable pattern; (c) event-driven audio analyzer + Float32Array / precomputed tables; (d) DOM-direct AudioMeter rationale; (e) GPU-composited `scaleY()` over animated `height`; (f) CSS containment adoption. Testing Coverage: 33 tests (27 baseline + 6 new singleton contract). Reference §1.14 inline for the codified discipline. Reference §1.13 for feature-PR conventions.

If the user asks to **complete the release triad on `dev`**: see §2.4 — check out `dev`, run the three CRITICAL commands, push, then `gh pr edit 4 --title ... --body-file ...`. After CI green, merge PR #4. Session 9 + 10 PR rebases on top of `dev` after the merge.

If the user asks to **generate commit / PR text**: write to `COMMIT.org` or `PR.org` only (NEVER run git write commands). Follow Kyonax Feature-PR or §1.13 release-PR conventions.

If the user asks to **review another HUD feature's performance**: walk it against §1.14.8 checklist: broad filter/shadow? animates non-transform/opacity? new OBS-WS subscription outside a singleton? per-frame allocation? per-tick `$emit`? missing `contain: layout paint`? un-debounced burst listener? Any "yes" → refactor before merging.

If the user asks to **add a new OBS-WS-driven composable**: follow the singleton pattern (§1.14.5): module-level `shared_state`, early-return on repeat calls, `watch(connected, fn, { immediate: true })` for initial fetch, no `onUnmounted`. Add a test to `composables.test.js` asserting initial state + `expect(a).toBe(b)` identity.

If the user asks to **add a new HUD widget or primitive**: default to `contain: layout paint` on the root. Use `transform`/`opacity` for any animation. Reference halo/glow via `var(--hud-halo)` / `var(--hud-halo-text)` / `var(--hud-glow)` — never reintroduce the deleted `cyberpunk-glow` mixin or any `filter`-chain utility.

If the user asks to **change the cam-log session-date format**: modify the `dayjs().utc().format(...)` token string in `@kyonax_on_tech/sources/hud/cam-log.vue`. Escape literal segments with `[...]`. Keep `.toUpperCase()`. `brand.region` drives the prefix — changing the region value on `brand.js` is the lowest-cost way to re-label the source. Update §1.1 HUD vocabulary line + §3.8 label spec when the format changes.

If the user asks for **another HUD animation** (scanline, glitch, boot sequence): animate `transform`/`opacity` only; apply halo via `var(--hud-halo)` on a static layer; no `box-shadow` or `filter` inside `@keyframes`. If broadly reusable, create a new `@shared/utils/animations.scss` partial or `@ui/` primitive; if brand-specific, keep inside the brand source `.vue`. Add `prefers-reduced-motion: reduce` media-query gate for any continuous pulsation (not yet applied on `breathe` — open TODO).

If the user asks to **tune status-dot timing**: edit `src/shared/components/ui/status-dot.vue` — the `2s ease-in-out` duration + `0.35` low-opacity point are the two tuning knobs on `.glow`'s `breathe` keyframe. The dark halo layer on the outer span stays static — do not move it into the keyframe.

If the user asks to **build `item-explain`**: create `@kyonax_on_tech/sources/animation/item-explain.vue` + flip `sources.js` to `'ready'`. Route auto-discovers. Kind-alias imports; PascalCase tags. Conform to §1.14 from day 1.

If the user asks to **build a new component** (any kind): follow §1.12 for placement + naming; follow §1.14 for performance. Kebab short filename; kind folder; `@<kind>` alias; binding per §1.12.5. Don't extract unless Rule E is met.

If the user asks to **add a new brand**: create `@<brand>/` with `brand.js` (`handle`, `name`, `description`, **`host`**, **`region`**, `identity`, `links`) + `sources.js` + `assets/` + `styles/_theme.scss` + `sources/{hud,animation,scene}/`. Zero app code changes needed.

If the user asks to **add a web source to an existing brand**: add entry to `@<brand>/sources.js`, create `.vue` in the matching `sources/<type>/` folder. Route auto-discovers.

If the user asks to **add a brand-private SVG**: drop at `@<brand>/assets/svg/<name>.svg`, reference `<UiIcon name="<name>" />`. Multi-pool glob auto-discovers.

If the user asks to **customize a brand's palette / tokens**: edit `@<brand>/styles/_theme.scss` only — inside `.brand-<handle>` class. NEVER add `colors` to `brand.js` (forbidden by test).

If the user asks to **add a section / element / modal**: follow §1.12.5 kind registry.

If the user asks to **add a new kind folder**: Rule F (2+ files first) + add Vite alias (Rule H) + register suffix/prefix in §1.12.5.

If the user asks to **add a helper / util function**: find the matching topic file (Rule J). Never one-function-per-file.

If the user asks to **add a new root config file**: follow §1.10 (Tier 1 figlet header + cyberpunk place name).

If the user asks to **cut a future release**: follow §1.13.5 workflow.

If the user asks for a **new task**: check §2.4 and `CHANGELOG.org` TODO block.

<!-- DESCRIPTION AND USER CONTEXT END -->



