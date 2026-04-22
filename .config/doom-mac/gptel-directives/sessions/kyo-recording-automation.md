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

**Compaction sources:** Sessions 1-5 (2026-04-08 to 2026-04-15 — brand bootstrap through v0.3 PR system), Session 6 (2026-04-16 to 2026-04-17 — overlay card fixes, modal abstraction, cam-log rename, Tier 1 file headers, ESLint Vue + naming conventions, brand-driven architecture refactor, Vite aliases, CONTRIBUTING.org, SECURITY.org, Doom Emacs config for Vue development), Session 7 Part A (2026-04-20 early — component architecture & naming convention §1.12: views restructured into sections/elements/modals, shared file renames, 7 new kind-aliases, four-layer naming pattern, Rules A–I codified), Session 7 Part B (2026-04-20 late — utils topic libraries §Rule J, three new ui/ primitives, widgets split by kind, ambiguous filename corrections, brand folder restructure into sources/, UiIcon multi-pool SVG discovery, SCSS single source of truth for brand theming, 27/27 tests), Session 8 prep (2026-04-21 — post PR #3 merge + `feat-brand_kot` branch creation; PR #4 opened for `dev` → `master` v0.4 release; release-PR composition pattern codified in §1.13; `release: v0.4 — Architectural Baseline` title chosen via title-body alignment rule; triad pending on `dev`), Session 9 (2026-04-21 continuation — first code on `feat-brand_kot`: cam-log "REC FRAME" replaced with dynamic dayjs UTC session-date `UTC ∇ DD.MM.YYYY // DDD`, status-dot switched from hard blink to smooth breathe animation, preview modal iframe-scale sub-pixel fix via `getBoundingClientRect()`, dayjs added as runtime dep).

**CRITICAL:** NEVER run `git commit`, `git push`, `gh pr create`, or any git write command. The user handles all git operations manually. Write to `COMMIT.org` and `PR.org` only.

---

## SECTION 1: GLOBAL GUIDELINES & DESIGN CONSTRAINTS

> **Apply these rules to every task in this session.** No domain skills are loaded for OBS automation specifically — these guidelines stage session-specific design and tech rules. PR / commit / CHANGELOG formatting rules live in the user's memory system and are auto-loaded. The pr-scribe skill handles PR body generation (Kyonax brand).

### 1.1 Visual Language

*   **Color palette:** Black (`--clr-neutral-500` = #000), gold (`--clr-primary-100` = #FFD700), white (`--clr-neutral-50` = #F2F2F2). Purple for license/version badges only. Red (`--clr-error-100`) for active recording state.
*   **Aesthetic:** Cyberpunk/sci-fi HUD — military-grade surveillance UI. Corner brackets and crosshairs (no rounded borders).
*   **HUD vocabulary:** SYS.LOG, session-date (`UTC ∇ DD.MM.YYYY // DDD` — dayjs, UTC, uppercase), CAM ONLINE, REC MODE, SES::SceneName::Txx. Status indicator (`<UiStatusDot>`) **breathes** when recording — smooth 2s ease-in-out opacity pulse (1 ↔ 0.35) + red `box-shadow` glow inhale/exhale. Never the hard `step-end` blink — soft breathing is the canonical recording-light idiom.

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

**On current branch `feat-brand_kot` (Phase 3.7 — Session 8/9 scope):**
*   [x] ~~Cam-log: replace static "REC FRAME" label with dayjs UTC session-date (`UTC ∇ DD.MM.YYYY // DDD`); retune `.session-date` + `.cam-online` to `--fs-425` with `top: 5em` / `left,right: 4.7em`; bump `.toolkit-id` to `--fs-350`.~~ (2026-04-21)
*   [x] ~~`<UiStatusDot>`: swap hard blink for smooth `breathe` animation (2s ease-in-out, opacity + box-shadow pulse) — the canonical recording-light idiom.~~ (2026-04-21)
*   [x] ~~`<PreviewModal>`: fix iframe sub-pixel scale offset by reading container width via `getBoundingClientRect().width` instead of `clientWidth`.~~ (2026-04-21)
*   [x] ~~Add `dayjs` (`^1.11.20`) as runtime dep + enable `utc` plugin.~~ (2026-04-21)
*   [ ] Build `item-explain.vue` at `@kyonax_on_tech/sources/animation/item-explain.vue` (follows the new kind-alias conventions — imports from `@hud/*`, `@widgets/hud/*`, `@widgets/ui/*`, `@composables/*`; template tags in PascalCase). Update `sources.js` status to `'ready'` once the file lands.
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

**Created:** 2026-04-13 | **Last updated:** 2026-04-21
**Status:** Active. 1 HUD source ready (refined in Session 9), 1 animation planned.

**`brand.js`:** handle `@kyonax_on_tech`, identity (Cristian D. Moreno), social links. NO colors — those live exclusively in `styles/_theme.scss`.
**`sources.js`:** CAM-LOG (hud, ready) + ITEM-EXPLAIN (animation, planned).
**`sources/hud/cam-log.vue`:** HUD overlay at `/@kyonax_on_tech/cam-log`. Uses `getBrand()` for identity data. Imports from `@hud/*`, `@widgets/hud/*`, `@widgets/ui/*`, `@composables/*`, plus `dayjs` + `dayjs/plugin/utc.js` for the session-date.
  - **Top-left label (`.session-date`):** live dayjs UTC date `UTC ∇ DD.MM.YYYY // DDD`, computed once at setup. `--fs-425`, `top: 5em`, `left: 4.7em`.
  - **Top-right label (`.cam-online`):** mirrors `.session-date` — `CAM ONLINE`, `--fs-425`, `top: 5em`, `right: 4.7em`.
  - **Bottom-right (`.toolkit-id`):** `RECKIT {VERSION_TAG}`, `--fs-350`.
  - **Bottom status bar:** `<HudTimer>` + `<AudioMeter>` (widgets). `<HudTimer>` composes `<UiStatusDot active="{is_recording}" />` which now **breathes** red when recording.
**`styles/_theme.scss`:** CSS class `.brand-kyonax-on-tech` with full color overrides — auto-loaded by `src/main.js` (single source of truth for the brand's palette, see §1.5).
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
- `components/ui/` — domain-agnostic primitives: `icon.vue` (`<UiIcon>`, generic SVG loader via `import.meta.glob('@shared/assets/svg/*.svg', { eager: true, query: '?raw' })`), `status-dot.vue` (`<UiStatusDot>`, 6×6 square that **breathes** red when active — 2s ease-in-out opacity + red box-shadow pulse; multi-word answers "status shown how? as a dot"), `data-point.vue` (`<UiDataPoint>`, label/value tile with `sm`/`lg` size variants — multi-word answers "metric of what? a data point, i.e. a label paired with a value"), `chip.vue` (`<UiChip>`, lowercase pill with `solid`/`overflow` variants — "chip" is a visual primitive per Material Design / PrimeVue / Quasar), `badge.vue` (`<UiBadge>`, uppercase HUD status pill with `active`/`dim` variants).
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
| `@kyonax_on_tech/brand.js` | Brand metadata + identity + colors |
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
| `src/shared/composables/use-obs-websocket.js` | `@composables` | `useObsWebsocket()` |
| `src/shared/composables/use-recording-status.js` | `@composables` | `useRecordingStatus()` |
| `src/shared/composables/use-audio-analyzer.js` | `@composables` | `useAudioAnalyzer()` |
| `src/shared/composables/use-scene-name.js` | `@composables` | `useSceneName()` |
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

### What was done last (2026-04-21 — Session 9: first code on `feat-brand_kot`)

First feature pass on the new brand branch. Four staged changes, all scoped to HUD polish / UX refinement — no new files, no architectural shifts.

1. **Cam-log top-left label turned from static to live.** User asked to replace the hardcoded `REC FRAME` label with today's date in a "cyberpunk sci-fi" format. Presented a 6-option format matrix (cycle day-of-year, UTC dotted, glyph + DDD, etc.). User picked **option C** (`UTC ∇ YY.MM.DD // MON`), then refined to full year + `DD.MM.YYYY`. Final format: `UTC ∇ DD.MM.YYYY // DDD` (e.g. `UTC ∇ 21.04.2026 // TUE`). Implemented via `dayjs().utc().format('[UTC ∇ ]DD.MM.YYYY[ // ]ddd').toUpperCase()`, computed once at setup. Template class renamed `.rec-frame` → `.session-date` (Rule G — purpose-describing, not repeating the kind). HUD vocabulary in §1.1 updated accordingly.

2. **Font-size + position retune on the top sub-labels.** User iterated: first asked for "100 less on fs" (→ `--fs-375`), then "up 50" (→ `--fs-425`), then gave explicit final values `top: 5em` / `left: 4.7em` for `.session-date`. Applied to both `.session-date` (left) and `.cam-online` (right — mirrored to `right: 4.7em`) so they stay symmetrical. Rationale in decision #115: when HUD label `font-size` changes, its `em`-based anchor must be recomputed proportionally to preserve the intended visual position. Also noted: `.toolkit-id` was separately bumped `--fs-300` → `--fs-350` in the user's manual edit.

3. **Status-dot: hard blink → smooth "respirando" (breathing).** User asked for the recording indicator to breathe like a normal recording red button. Swapped `animation: blink 0.8s step-end infinite` (`opacity: 1 ↔ 0.2` flicker) for `animation: breathe 2s ease-in-out infinite` — `opacity: 1 ↔ 0.35` with a soft red `box-shadow: 0 0 4px ↔ 2px of var(--clr-error-100)` inhale/exhale. This is the canonical camera REC / hardware record-LED idiom. Updated the file header comment ("blinks" → "breathes", prop doc gains "soft glow pulse"). All `<UiStatusDot>` consumers inherit the change for free.

4. **Preview modal iframe-scale sub-pixel offset fixed.** User reported a ~1px visual offset solved empirically by nudging `--iframe-scale` from `0.5578125` → `0.5578124`. Diagnosed: `element.clientWidth` (at `preview.vue:137`) returns a **rounded integer**; when the real stage width renders at e.g. `1070.9999…px`, `clientWidth` returns `1071` → `scale = 1071/1920 = 0.5578125` → scaled iframe width `1920 × 0.5578125 = 1071.0px` overshoots the real container by ~1px. Proper fix: switched to `element.getBoundingClientRect().width` for sub-pixel float precision. Decision #114 codifies a general rule: scale-derived CSS custom properties must read container width via `getBoundingClientRect()`, never `clientWidth` / `offsetWidth`.

5. **Dependency added: `dayjs ^1.11.20`.** User confirmed dayjs preference over native `Date` / `Intl.DateTimeFormat`. First runtime dep added beyond the core trio (`vue`, `vue-router`, `obs-websocket-js`). UTC plugin loaded at module scope (`dayjs.extend(utc)`). `package.json` + `package-lock.json` updated.

6. **This reset** — added decisions #111–#115 to §2.3; updated §1.1 HUD vocabulary (session-date + breathing status idiom); extended §3.8 with cam-log's label spec; flagged `.status-dot` as breathing in §3.13 + §4 file index; noted dayjs dep in §4; marked Session 9 completions in §2.4; compaction sources header extended with Session 9.

**Staged at reset (5 files, all modifications, no new files):**
- `@kyonax_on_tech/sources/hud/cam-log.vue` — session-date + dayjs import + label style retune + `.toolkit-id` fs bump.
- `src/shared/components/ui/status-dot.vue` — blink → breathe animation + header comment.
- `src/views/components/modals/preview.vue` — `clientWidth` → `getBoundingClientRect().width`.
- `package.json` — `+"dayjs": "^1.11.20"`.
- `package-lock.json` — dayjs lockfile entry.

**No validation run yet** — user will run tests / lint before/after commit (pre-commit hooks handle linting regardless).

**Documentation sync:** changes flow into this session file only. No roam node update. COMMIT.org + PR.org are the downstream deliverables of this reset (see "Where to resume").

### Pending / Not yet started

**Immediate — v0.4 release (UNCHANGED from Session 8 prep; still gates PR #4 merge):**
*   [ ] **CRITICAL:** `package.json` bump to `0.4.0` on `dev` — `npm version 0.4.0 --no-git-tag-version`. *Note: Session 9 bumped dep list on `feat-brand_kot` — do NOT let the version bump land on the feature branch; only on `dev`.*
*   [ ] **CRITICAL:** `README.org` version markers on `dev` — four `sed -i -E` patches (see §1.8 + `COMMIT.org` PERMANENT runbook step 3).
*   [ ] **CRITICAL:** `CHANGELOG.org` on `dev` — new `[v0.4] — 2026-04-21 :: Architectural Baseline` block above `[v0.3]` with Added / Changed / Removed / Decided subsections.
*   [ ] Push refined PR.org body + title to PR #4 — `gh pr edit 4 --title "release: v0.4 — Architectural Baseline" --body-file <(awk ...)`.
*   [ ] Post-merge: tag `v0.4` on `master`, optional GitHub Release + branch protection.

**Immediate — Session 9 commit/PR (on `feat-brand_kot`):**
*   [ ] User stages / commits the 5 modified files using the new `COMMIT.org` generated this reset.
*   [ ] User opens PR `feat-brand_kot` → `dev` using the `PR.org` body generated by pr-scribe (Kyonax brand, Feature PR shape — not release).
*   [ ] Expected next release including Session 9: **v0.4.1 or v0.5** (decided at release-cut time; v0.4 ships Session 6+7 baseline first).

**On current branch `feat-brand_kot` (Phase 3.7 — remaining Session 8/9+ code):**
*   [ ] Build `item-explain.vue` at `@kyonax_on_tech/sources/animation/item-explain.vue`. Kind-alias imports; PascalCase tags. Flip `sources.js` to `'ready'` on land.
*   [ ] Brand-private primitives under `@kyonax_on_tech/components|composables|widgets/` as needed (Rule F — 2+ files before folder).
*   [ ] Brand SVGs at `@kyonax_on_tech/assets/svg/` — multi-pool glob auto-discovers.
*   [ ] Brand theme tuning at `@kyonax_on_tech/styles/_theme.scss` — single source of truth (never re-add `colors` to `brand.js`).

**Cross-branch / ongoing:**
*   [ ] Broaden Vitest coverage beyond 27 baseline — 5 sections, 4 renamed shared primitives, 3 new `ui/` primitives, brand theming integration. Consider adding a `status-dot` animation test (state-based class check).
*   [ ] Shared widgets showcase on landing page (new `@sections/widgets.vue` or dedicated route).
*   [ ] Fix YouTube subscriber badge subscribe link.
*   [ ] **OPEN SOURCE:** Contribute to `wallyqs/org-ruby` for GitHub alert syntax in `.org` files.
*   [ ] Phase 4 (packaging, scene collection export).

### Post-Session-9 status

`feat-brand_kot` holds 5 staged modifications covering: (a) @kyonax_on_tech brand cam-log label refinements (session-date + label sizing/position), (b) shared `<UiStatusDot>` recording-idiom upgrade (breathe), (c) shared `<PreviewModal>` iframe sub-pixel scale bug fix, (d) runtime dep addition (`dayjs`). No new files, no architectural changes, no test additions. PR #4 (v0.4 Architectural Baseline — `dev` → `master`) remains open with triad pending.

### Where to resume

**Branch & PR context at resume:**
- Current branch: `feat-brand_kot` (5 files staged — Session 9 work).
- Open PRs: **PR #4** (`dev` → `master`, `release: v0.4 — Architectural Baseline`, triad pending on `dev`).
- Merged: PR #3 (`feat-fixes-and-refinement-v3` → `dev`).
- Expected next Session 9 PR: `feat-brand_kot` → `dev` (post-v0.4 merge, so Session 9 can ride the new baseline). Title candidate: `feat(kot): live session-date + breathing status + iframe sub-pixel fix`.

**Resume paths (by task):**

If the user asks to **commit Session 9 work**: do NOT run `git commit`. Regenerate `COMMIT.org` with a subject under 72 chars summarizing the 5-file staged diff (cam-log live date, breathing status, preview sub-pixel fix, dayjs). Keep the Session 9 commit limited to `feat-brand_kot` — triad / release work stays on `dev`.

If the user asks to **generate the PR for Session 9**: invoke the `pr-scribe` skill with Kyonax brand, **Feature PR shape** (not Release). Changes block uses Pattern B (flat `**Changes:**` list with `[MOD]` tags — no release subsection). Technical Details should include at minimum: TD on the dayjs-vs-native tradeoff; TD on `clientWidth` → `getBoundingClientRect()` sub-pixel rule; TD on blink → breathe animation rationale. Testing Coverage stays light — 27/27 baseline still passes (no new tests added this session). Reference §1.13 feature-PR conventions.

If the user asks to **complete the release triad on `dev`**: see §2.4 — check out `dev`, run the three CRITICAL commands, push, then `gh pr edit 4 --title ... --body-file ...`. After CI green, merge PR #4. Session 9 PR rebases on top of `dev` after the merge.

If the user asks to **generate commit / PR text**: write to `COMMIT.org` or `PR.org` only (NEVER run git write commands). For feature PRs use Kyonax Feature-PR shape; for release PRs follow §1.13 composition.

If the user asks to **change the cam-log session-date format**: modify the `dayjs().utc().format(...)` token string in `@kyonax_on_tech/sources/hud/cam-log.vue`. Escape literal segments with `[...]`. Keep `.toUpperCase()`. Update §1.1 HUD vocabulary line + §3.8 label spec when the format changes.

If the user asks for **another HUD animation** (scanline, glitch, boot sequence): if broadly reusable, create a new `@shared/utils/animations.scss` partial or `@ui/` primitive; if brand-specific, keep inside the brand's source `.vue`. Keep animations `prefers-reduced-motion`-aware if they pulse continuously (future enhancement — not yet applied on `breathe`).

If the user asks to **tune status-dot timing**: edit `src/shared/components/ui/status-dot.vue` — the `2s ease-in-out` duration + `0.35` low-opacity point are the two tuning knobs. Keep `box-shadow` color bound to `var(--clr-error-100)` so brand theming cascade still works.

If the user asks to **build `item-explain`**: create `@kyonax_on_tech/sources/animation/item-explain.vue` + flip `sources.js` to `'ready'`. Route auto-discovers. Kind-alias imports; PascalCase tags.

If the user asks to **build a new component** (any kind): follow §1.12. Kebab short filename; kind folder; `@<kind>` alias; binding per §1.12.5. Don't extract unless Rule E is met.

If the user asks to **add a new brand**: create `@<brand>/` with `brand.js` + `sources.js` + `assets/` + `styles/_theme.scss` + `sources/{hud,animation,scene}/`. Zero app code changes needed.

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



