<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the **org-2-html** session — the standalone npm package `@kyonax/org2html` (v1.0.2, GPL-3.0-only) that transforms Org-mode (`.org`) files into static HTML and Vue 3 SFCs. It is intentionally positioned as **the engine layer beneath the upcoming `kyo-blog`** project, which will compose its output using the visual language already proven in [session: kyo-web-online.md](kyo-web-online.md). The cross-repo conventions that shape this project's root files, CI gates, and license posture are referenced from `[session: kyonax-conventions-architecture.md]`. Load this file at the start of every conversation about org-2-html to gain full context without re-discovering anything. Read sections in order on first load — afterwards, reference them by number.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | TS/CLI conventions, AGENTS.md three-exception comment rule, sanitizer-first defaults, plugin API contract, template variable contract, the kyo-web-online design contract that the rendered HTML must remain compatible with, and the Kyonax-wide root-file conventions referenced from the architecture memory. | Before any task. Mandatory constraints. |
| **2. Session Overview** | Project scope, package surface, current vs. README-aspirational file tree, status, key decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-deliverable detail: parser, renderer, CLI, vue generator, templates, plugins, scripts, governance files, dependencies. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference path table for every source file, template asset, governance file, script, and reference repo. | When reading, editing, or locating files. |
| **5. Last Interaction** | What was just completed (2026-05-21 convention-alignment pass: Tier 1 headers + 7-job CI suite + scripts/ + license-preamble sweep across 17 src/**/*.ts + .gga retirement + README.org with kyo-web-online org2html ASCII art + concise-header refinement). | At conversation start. |
| **6. Activity Log** | Datetime-stamped table of every meaningful event in this session. | When you need exact "what was done when". |

**Path variables.** The user works across multiple devices, so absolute repo paths vary per machine. This file refers to repo roots via variables — resolve them on the current device by `pwd` from inside the relevant checkout:

| Variable | Meaning | How to resolve |
|---|---|---|
| `$REPO` | local checkout of `Kyonax/org2html` (this project) | `pwd` from inside — observed path: `/run/media/kyonax/Da_ Disk/dev/github-kyonax/org-2-html/` |
| `$REPO_KWO` | local checkout of `Kyonax/kyo-web-online` (design-system reference) | sibling: `<parent>/kyo-web-online/` |
| `$REPO_RECKIT` | local checkout of `Kyonax/reckit` (convention reference) | sibling: `<parent>/kyo-recording-automation/` |
| `$REPO_BLOG` | local checkout of `Kyonax/kyo-blog` (downstream consumer, not yet started) | sibling: `<parent>/kyo-blog/` (TBD) |

Home-relative paths (`~/.brain.d/`, `~/.config/`, `~/.claude/`) are device-stable and are written without a variable.

**Operational Rule:** When the user references "the engine," they mean this project. When they reference "the blog," they mean the downstream `kyo-blog` consumer. When they say "the design system" or "the look and feel," they mean the patterns abstracted in §1.10 (which references the kyo-web-online session). When they say "the conventions," they mean the Kyonax-wide patterns referenced from `[session: kyonax-conventions-architecture.md]`. Never run any git or `gh` write command — the user manages all git operations manually (global CLAUDE.md rule).

**Key principle:** This project is a *library/CLI*, not a website. The "look and feel" lives in the templates the engine emits (`templates/default.html`, `templates/styles.css`) plus the Vue components it can generate. The visual contract is owned by §1.10 (references `kyo-web-online.md`); the repo-shape contract is owned by §1.19–§1.21 (references `kyonax-conventions-architecture.md`).

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `session-reset` (this file), `session-memory` (cross-session references), `reckit-roam-node` (roam node template, adapted for Kyonax git/gh-driven projects). This section stores session-scoped patterns. The kyo-web-online design contract (§1.10) is the *outward* contract for emitted HTML; the Kyonax convention contract (§1.19–§1.21) governs *this repo's own shape*; everything else is *inward* engine-implementation rules.

### 1.1 Package identity
*   Name: `@kyonax/org2html`. Binary: `org2html` (`dist/cli/index.mjs`). Main: `dist/index.js`. Types: `dist/index.d.ts`. License GPL-3.0-only. Engines `node >=18`. Author: `Cristian D. Moreno - Kyonax`.
*   Public surface (library): `parse`, `renderToHtml`, `applyTemplate`, default async `org2html(orgContent, options)` returning `{ html, metadata, assets? }` — `src/index.ts`.
*   Public surface (CLI): `build`, `watch`, `test`, `help [command]` — `src/cli/index.ts`. Commander v11.
*   `files` whitelist in `package.json` ships `dist` + `templates` — nothing else lands in the tarball.
*   Funding: `https://github.com/sponsors/kyonax`.

### 1.2 Comment policy (AGENTS.md — three-exception rule)
*   `AGENTS.md`: comments forbidden in TypeScript source EXCEPT (1) the GPL-3.0-only license preamble, (2) the filename + description block immediately following the preamble, (3) JSDoc on exported helpers / type definitions / surprising algorithms documenting non-obvious WHY.
*   Description-of-WHAT in JSDoc is still discouraged — well-named identifiers carry that load.
*   Enforcement: `scripts/check-license-headers.mjs` (CI gate) verifies preamble presence on every src/ + scripts/ TS/JS file. The "no other comments" portion is enforced by code review (CODEOWNERS).
*   Pattern is captured cross-repo as `[session: kyonax-conventions-architecture.md > architecture-decisions > ad-004]`.

### 1.3 Build & toolchain
*   Bundler: `tsup` (`tsup.config.ts`). Scripts: `build`, `dev` (watch), `test` (vitest), `test:coverage`, `lint` (eslint flat), `prepublishOnly`.
*   ESLint: flat config (`eslint.config.mts`) using `@eslint/js` + `typescript-eslint` + `globals`.
*   TS target / module: see `tsconfig.json`; `bin` ships `.mjs`.
*   Dependency surface (runtime): `chalk`, `chokidar`, `commander`, `dompurify`, `fast-glob`, `jsdom`, `probe-image-size`, `shiki`, `slugify`. Optional: `sharp` (graceful absence).
*   Dev: `@eslint/js`, `@types/dompurify`, `@types/jsdom`, `@types/node`, `eslint`, `globals`, `jiti`, `sharp`, `tsup`, `typescript`, `typescript-eslint`, `vitest`.

### 1.4 README ↔ actual file tree (DIVERGENCE log — verified 2026-05-21)
*   **README.org** (newly converted from `.md` on 2026-05-21) now accurately reflects the present tree.
*   **Actually present today** (under `src/`):
    - `cli/` — `index.ts`, `utils.ts`, `vue-generator.ts`, `commands/{build,watch,test}.ts`
    - `parser/` — `lexer.ts`, `parser.ts`, `ast.ts`, `metadata.ts`
    - `renderer/` — `html-renderer.ts`, `template.ts`, `sanitizer.ts`
    - `plugins/` — `code-highlight.ts`, `toc.ts`
    - `index.ts`, `types.ts`
*   **Still MISSING (aspirational, tracked in §2.4 + CHANGELOG.org §TODO):** `src/plugins/plugin-api.ts`, `src/plugins/shortcode.ts`, `src/plugins/asset-fetcher.ts`, `src/assets/asset-handler.ts`, `client/hydrate.ts`, top-level `tests/` directory.
*   **Templates folder** (now documented in README.org): `default.html`, `styles.css`, `manifest.json`, `favicon.svg`, `robots.txt`. Ships in npm tarball.
*   **Working-tree status (end of session):** many new files (CHANGELOG.org / CONTRIBUTING.org / LICENSING.org / NOTICE / README.org / .editorconfig / .gitattributes / .github/* / scripts/*) untracked; 17 src/**/*.ts files modified (license-preamble sweep); `.gga` deleted; `README.md` deleted (replaced by README.org); `dist/*` deletions remain pending the next `npm run build`.

### 1.5 Render pipeline contract
1. `parse(orgContent)` → `OrgAst` (`src/parser/parser.ts`). Extracts metadata via `extractMetadata(lines)` (`src/parser/metadata.ts`), computes `readingTime`, `wordCount`, `excerpt`.
2. `renderToHtml(ast, options)` → `{ html, metadata, assets? }` (`src/renderer/html-renderer.ts`). TOC auto-generated when `metadata.options.toc !== false` (depth defaults to 3) and there are headings.
3. `applyTemplate(html, metadata, template?, templateDir?)` → final HTML (`src/renderer/template.ts`). When the caller does not specify a template, the default at `templates/default.html` is used. **`applyTemplate` is always called** by the top-level sugar.
4. `org2html(...)` is the sugar that chains the three steps.

### 1.6 RenderOptions contract (`src/types.ts`)
*   `template?: string` — template file path.
*   `templateDir?: string` — base dir for templates + assets.
*   `sanitize?: boolean` — default sanitize (DOMPurify via `src/renderer/sanitizer.ts`); `--no-sanitize` on CLI disables.
*   `allowRawHtml?: boolean` — paired with sanitize.
*   `codeHighlight?: boolean` — Shiki-driven (`src/plugins/code-highlight.ts`). `--no-highlight` on CLI disables.
*   `fetchRemoteAssets?: 'none' | 'metadata' | 'full'` — image metadata fetching (`probe-image-size`, optional `sharp`).
*   `maxAssetSize?: number`, `baseUrl?: string`, `componentMap?: Record<string,string>`.

### 1.7 OrgMetadata contract (`src/types.ts`)
*   Core: `title`, `author`, `date`, `email`, `description`, `keywords[]`, `language`, `category`, `tags[]`, `options`, `properties`, `slug`, `coverImage`, `canonical`, `readingTime`, `wordCount`, `excerpt`.
*   SEO/social: `ogTitle`, `ogDescription`, `ogImage`, `ogType`, `twitterCard`, `twitterSite`, `twitterCreator`, `themeColor`, `robots`.
*   Extension: `[key: string]: any` — plugins can attach fields without type changes. Use this contract when designing `kyo-blog` post frontmatter.

### 1.8 OrgOptions (`src/types.ts`)
*   `toc?: boolean | number`, `num?: boolean`, `date?: boolean`, `H?: number`, `author?`, `email?`, `title?`. Map 1:1 to Org-mode `#+OPTIONS:` semantics.

### 1.9 Default template contract (`templates/default.html`)
*   Mustache-style braces: `{{title}}`, `{{description}}`, `{{keywords}}`, `{{author}}`, `{{language}}`, `{{canonical}}`, `{{coverImage}}`, `{{date}}`, `{{tags}}`, `{{structuredData}}`, `{{styles}}`, `{{content}}`.
*   Block helper present: `{{#if canonical}} ... {{/if}}`.
*   Ships full SEO surface: Primary Meta + Open Graph (article) + Twitter Card (summary_large_image) + JSON-LD slot + favicon + apple-touch-icon + manifest + RSS alternate + inlined `<style>{{styles}}</style>` + `<article>{{content}}</article>` body.
*   `templates/styles.css` ships a CSS-custom-property design system (8 tokens — text / background / primary / secondary / border / code-bg / link / link-hover) + a `prefers-color-scheme: dark` override. `--max-width: 800px`. **Engine default — `kyo-blog` replaces wholesale via `--template-dir`. `prefers-reduced-motion` rule still pending** (§2.4).

### 1.10 kyo-web-online design contract (REFERENCE — do not duplicate)
> **All look-and-feel patterns the engine must remain compatible with live in the kyo-web-online session.** Do NOT inline-copy them here. Reference syntax: `[session: kyo-web-online.md > §<n.n> > <topic>]`.

*   **Tokens & color usage:** `[session: kyo-web-online.md > §1.5 > SCSS theming]` + `[§1.6 > 60/30/10 color usage]`. 8 OKLCH families + brand tokens (orcid, youtube-red). Hardcoded hex/hsl literals forbidden in SFC `<style>` blocks (including comments). **Engine default `styles.css` does not yet honor this.**
*   **Typography:** `[session: kyo-web-online.md > §1.5 > Typography]`. 8 sizes × 3 breakpoint tiers, body baseline `var(--fs-300)` line-height 1.6 unitless.
*   **Cyberpunk HUD ornament primitives:** `.hud-deco` `[§1.22]`, `.element-flare` `[§1.16]`, `.state-grid` `[§1.21]`, `.icon-glyph` `[§1.18]`, `.ccs-glyph` `[§1.19]`, `.heart-glyph` `[§1.20]`.
*   **Brand SVG strategy (16 brands, currentColor-based):** `[§1.14]`. Filename = tech ID. Simple Icons paths with `fill-rule="evenodd"` on `<path>`.
*   **UI primitive composition (7 primitives + tandem variants):** `[§1.13]` + `[§1.29]` (cyber + cyber-outline). When `vue-generator.ts` emits SFCs for `kyo-blog`, bindings must match `<UiCard>`, `<UiLink>`, `<UiButton>`, `<UiImage>`, `<UiIcon>`, `<UiSectionHeading>`, `<BrandIcon>`.
*   **Layout grid:** `[§1.24]`. Section max-width 1280px, padding 5rem 1.5rem (md: 6rem 2rem).
*   **Accessibility floor:** `[§1.28]`. Every emitted region needs `tabindex="0"` + `role="region"` + `aria-label`. Real `<button>`/`<a>`, never `<div role="...">`.
*   **Image pipeline:** `[§1.23]`. Sharp-based AVIF + WebP + JPG fallback; LCP preload + `fetchpriority="high"` + `decoding="async"`; below-fold `loading="lazy"`.
*   **Glyph encoding:** `[§1.15]`. `'\uXXXX'` JS escapes only.
*   **Translation contract (downstream):** `[§1.7]`. `RAW_HTML_KEYS` allowlist governs which keys carry inline HTML.
*   **Performance rules:** `[§1.9]`. `prefers-reduced-motion` global rule; opacity-only animations on hot paths; `content-visibility: auto` *rejected*.
*   **Polymorphic root pattern (Vue output):** `[§1.26]`. `<component :is="card.has_link ? 'a' : 'div'">`.

### 1.11 Sanitizer-first default
*   `dompurify` + `jsdom` (server-side) — `src/renderer/sanitizer.ts`. Default sanitizes. Opt out via `--no-sanitize` on CLI or `sanitize: false` programmatically.
*   Org `raw HTML` blocks → `rawHtml` AST node type. Renderer must respect sanitize flag before emitting.
*   **Rule:** never disable sanitize on user-supplied content. Disable only when the org corpus is fully trusted.

### 1.12 Plugin/postprocess contract (`OrgPlugin` in `src/types.ts`)
*   `blockHandlers?` — keyed by block name (`#+BEGIN_<KEY>`).
*   `inlineHandlers?` — keyed by inline marker.
*   `metadataProcessor?(metadata)` — transform before render.
*   `postProcessor?(html, metadata) => Promise<string>` — final HTML transform (async).
*   **Plugin registry / loader (`plugin-api.ts`) is NOT yet implemented** despite README listing it.

### 1.13 Vue generation (`src/cli/vue-generator.ts`)
*   `safeEncodeForSfc(value)` — encodes JSON for safe embedding inside `.vue` files.
*   `stripDocumentWrapper(html)` — drops `<!doctype>`, `<head>`, `<style>`, `<script>`, outer `<html>` + `<body>` tags.
*   Shortcode placeholders `<div data-component="CompName" attr="val">` → Vue imports + props declarations + replaced HTML.
*   **Path the kyo-blog migration will exercise:** emit `.vue` SFCs consuming the 7 UI primitives from `[session: kyo-web-online.md > §1.13]`, with imports resolved against the consumer's Vite alias map `[§1.4]`.

### 1.14 CLI surface (`src/cli/index.ts`)
*   `org2html build <input> [-o dist] [-t template] [--template-dir dir] [--no-sanitize] [--no-highlight]`
*   `org2html watch <input> [-o dist] [-t template] [--template-dir dir]` — chokidar-driven.
*   `org2html test <file>` — single-file preview.
*   `org2html help [command]` — bespoke help text + per-command help.
*   `org2html -V` reports `1.0.0` from the Commander setup, while `package.json` is `1.0.2` — **version drift still pending** (§2.4).

### 1.15 Test posture
*   `vitest ^4.0.8` is wired as devDependency + `npm test` script.
*   **No `tests/` directory exists.** The CI vitest job has a "no test files found" tolerance branch so the gate stays green until tests land.
*   Intended layout when populated: `tests/<unit>.test.ts` + `tests/fixtures/<topic>/`.

### 1.16 License-header gate (replaces the retired `.gga` tool)
*   Every committed `.ts`, `.tsx`, `.js`, `.mjs` file under `src/` and `scripts/` must carry the GPL-3.0-only preamble in its first 8 lines.
*   Enforced by `scripts/check-license-headers.mjs`, invoked by `scripts/precheck.mjs`, invoked by the `precheck` job in `.github/workflows/ci.yml`.
*   The header convention itself is captured cross-repo as `[session: kyonax-conventions-architecture.md > architecture-decisions > ad-002]` (single-license posture) + `[ad-004]` (three-exception comment rule).

### 1.17 Distribution & install
*   Published as `@kyonax/org2html` (scoped) — `npm install @kyonax/org2html` (drop `-g` flag for project-local).
*   README's older `npm install -g org2html` claim was unscoped — keep an eye on that if the README is regenerated.

### 1.18 Working-directory contract
*   `dist/` is `.gitignore`d (build output — regenerated by `npm run build`).
*   `nodes/` and `images/` are author-local test sandboxes — gitignored.
*   `package-lock.json` and `templates/` are COMMITTED (CI + npm tarball).

### 1.19 Kyonax Tier 1 file-header convention (REFERENCE)
*   Every root-level config / governance / release-tracking file in this repo carries the Tier 1 header — GPL preamble + figlet smslant banner + concise WHAT/WHY block + optional Guidelines.
*   The full convention spec lives at `[session: kyonax-conventions-architecture.md > architecture-decisions > ad-001]` and the figlet/naming constraint at `[constraints > cl-001]`.
*   This repo's per-file banner registry is in `LICENSING.org` §Tier 1 (the canonical local copy).
*   Scripts under `scripts/` do NOT carry figlet banners — only the simpler source-file preamble — per `[architecture-decisions > ad-003]`.

### 1.20 CI gate suite shape (REFERENCE)
*   `.github/workflows/ci.yml` runs the 7-job pattern: ESLint → Precheck → Vitest → Production build → Security Scan → Protected Files → Pre-Check Label aggregator.
*   Full pattern documented at `[session: kyonax-conventions-architecture.md > design-patterns > dp-001]`.
*   `scripts/precheck.mjs` is the composite local gate — `[dp-002]`.
*   Protected-files job is advisory only — `[dp-003]`.
*   Pre-Check Label aggregator pattern — `[dp-004]`.
*   `.github/workflows/publish.yml` is the tag-driven npm publish (`v*.*.*` semver tags only); runs lint+test first, then build + publish via `NPM_TOKEN`.

### 1.21 Three-tier composition (REFERENCE)
*   org-2-html (engine) + kyo-web-online (design contract) + kyo-blog (consumer) is the Kyonax three-tier pattern.
*   Full pattern + cross-tier rules + anti-patterns documented at `[session: kyonax-conventions-architecture.md > design-patterns > dp-005]`.
*   The reference-over-copy discipline for cross-session design references is `[dp-006]`.

### 1.22 .gitignore — AI-agent paths intentionally omitted
*   The Kyonax `.gitignore` template does NOT include `.claude/`, `.aider*`, `.cursor/`, `.continue/`.
*   Decision rationale at `[session: kyonax-conventions-architecture.md > architecture-decisions > ad-005]`.
*   When generating or refreshing the `.gitignore`, do NOT auto-include any AI-coding-agent paths. Editor swap files (`.swp`, `.swo`, `*~`, `\#*\#`, `.\#*`) stay.

---

## SECTION 2: SESSION OVERVIEW

> Project context, scope, and current phase status.

### 2.1 Purpose
Stand up **`@kyonax/org2html` as the engine layer** for the future `kyo-blog` site (three-tier composition per `[session: kyonax-conventions-architecture.md > dp-005]`). The engine reads `.org` files and emits static HTML (via `templates/default.html`) or Vue 3 SFCs (via `src/cli/vue-generator.ts`). The blog will compose those outputs inside a Vue 3 + Vite + SCSS 7-1 shell mirroring the kyo-web-online visual contract.

### 2.2 Scope
| Item | Type | Summary | Status |
|---|---|---|---|
| Core parser | feature | `lexer.ts` + `parser.ts` + `ast.ts` + `metadata.ts`; AstNode + OrgAst types complete | **DONE** |
| Core renderer | feature | `html-renderer.ts` + `template.ts` + `sanitizer.ts`; mustache subs + dompurify | **DONE** |
| CLI | feature | `build`, `watch`, `test`, `help`; commander v11 + chokidar watch | **DONE** |
| Default template | asset | Full SEO surface + inline styles slot | **DONE** |
| Default styles | asset | 8 CSS custom properties + dark-mode `@media` | **DONE** (engine-default; consumer overrides) |
| Code-highlight plugin | feature | Shiki integration | **DONE** |
| TOC plugin | feature | Heading collector + render | **DONE** |
| Vue SFC generator | feature | `stripDocumentWrapper` + shortcode + `safeEncodeForSfc` | **PARTIAL** (helpers exist; full SFC pipeline not exercised E2E) |
| Convention alignment | governance | Tier 1 headers + 7-job CI suite + scripts/ composite gate + governance files + license-preamble sweep across src/**/*.ts | **DONE** (2026-05-21) |
| Architecture memory extract | docs | Kyonax cross-project conventions extracted to `kyonax-conventions-architecture.md` (5 ad + 6 dp + 1 cl) | **DONE** (2026-05-21) |
| README.org | docs | Converted from README.md + adopted kyo-web-online org2html.txt ASCII art + reckit-style structure | **DONE** (2026-05-21) |
| `.gga` retirement | governance | External code-review hook removed; CI license-header gate replaces it | **DONE** (2026-05-21) |
| `.gitignore` rewrite | governance | Tier 1 header + Kyonax sections; AI-agent paths intentionally omitted | **DONE** (2026-05-21) |
| Source-file license-preamble sweep | cleanup | GPL-3.0-only preamble + filename block on 17 src/**/*.ts files; JSDoc preserved per ad-004 | **DONE** (2026-05-21) |
| Concise-header refinement | docs | Tightened Tier 1 header bodies (2-3 line WHAT/WHY + 1-3 line Guidelines); removed figlet from scripts/ | **DONE** (2026-05-21) |
| Plugin API | feature | `OrgPlugin` interface in `types.ts`; no registry/loader yet | **DESIGN ONLY** |
| Shortcode plugin | feature | README-listed `plugins/shortcode.ts` | **MISSING** (functionality partly in `vue-generator.ts`) |
| Asset fetcher / handler | feature | README-listed `plugins/asset-fetcher.ts` + `assets/asset-handler.ts` | **MISSING** (deps installed, no code) |
| Client hydration | feature | README-listed `client/hydrate.ts` | **MISSING** |
| Test suite | quality | Vitest installed; `tests/` empty | **MISSING** |
| CLI version sync | bug-fix | `program.version("1.0.0")` vs package.json 1.0.2 | **PENDING** |
| `prefers-reduced-motion` in default styles | a11y | Inherit kyo-web-online floor in engine-default `templates/styles.css` | **PENDING** |
| `package.json` `maintainers` field | docs | Add ORCID maintainer block per kyo-web-online shape | **PENDING** |
| kyo-blog scaffold | downstream | Sibling repo that consumes engine + kyo-web-online design system | **NOT STARTED** |

### 2.3 Key Decisions (Session-Wide)

1. **(2026-05-21)** **org-2-html is the engine, kyo-blog is the consumer.** Two-repo split decided. — see Activity Log row at 00:00.
2. **(2026-05-21)** **Default template stays in this repo; OKLCH palette is NOT vendored.** Consumer wires via `--template-dir`.
3. **(2026-05-21)** **Cross-session references for design rules.** Look-and-feel rules live in `[session: kyo-web-online.md > §1.x]` and are *never* copied inline — pattern is `[session: kyonax-conventions-architecture.md > dp-006]`.
4. **(2026-05-21)** **Sanitize by default.** Authors opt out with `--no-sanitize` only on trusted corpora.
5. **(2026-05-21)** **CLI version drift logged but not yet fixed** — needs to be read from `package.json` at build time.
6. **(2026-05-21)** **README treated as roadmap, not ground truth — until 2026-05-21.** README.org now in sync with the actual tree.
7. **(2026-05-21)** **Reckit-style plan node + index file convention adopted** for org-2-html roam knowledge.
8. **(2026-05-21)** **Convention alignment with reckit + kyo-web-online.** Tier 1 root files carry GPL header + figlet banner + concise body (`[ad-001]`); 7-job CI gate suite (`[dp-001]`); `scripts/precheck.mjs` composite gate (`[dp-002]`); protected-files advisory (`[dp-003]`); Pre-Check Label aggregator (`[dp-004]`); engine + design-contract + consumer three-tier (`[dp-005]`); cross-session reference-over-copy (`[dp-006]`); figlet smslant ≤12-char banners (`[cl-001]`). — see Activity Log row at 01:00.
9. **(2026-05-21)** **`.gga` external code-review hook retired.** CI license-header gate (`scripts/check-license-headers.mjs`) is the canonical enforcement; the "no other comments" portion falls to CODEOWNERS review. — see Activity Log row at 02:30.
10. **(2026-05-21)** **AGENTS.md refactored to three explicit exceptions:** GPL preamble + filename block + JSDoc on exported helpers documenting non-obvious WHY (`[ad-004]`). — see Activity Log row at 02:00.
11. **(2026-05-21)** **README.md → README.org with the kyo-web-online org2html.txt ASCII art.** Mirrors the reckit-style structure (centered ASCII + acronym tagline + shields.io badge row + sections). The art lives canonically at `$REPO_KWO/src/assets/ascii/org2html.txt`. — see Activity Log row at 03:00.
12. **(2026-05-21)** **`.gitignore` rewritten with Tier 1 header; AI-agent paths intentionally excluded** (`[ad-005]`). The "Editors / IDEs / AI agents" section from kyo-web-online template is NOT carried over.
13. **(2026-05-21)** **`scripts/` excluded from Tier 1 figlet treatment** (`[ad-003]`). Scripts get the simpler source-file header (preamble + 2-3 line description in one `/* */` block).
14. **(2026-05-21)** **Concise pattern for Tier 1 header body.** Two-line tagline+date, 1-3 line WHAT/WHY (≤70 chars wrap), optional Guidelines with single-line rules. Section lists and verbose context paragraphs are out. — see Activity Log row at 03:30.
15. **(2026-05-21)** **Architecture memory file `kyonax-conventions-architecture.md` created.** 5 architecture decisions + 6 design patterns + 1 constraint extracted from this session. — see Activity Log row at 04:00.

### 2.4 Pending Work
*   **CLI version sync:** `program.version(packageJson.version)` (read at build time via tsup `define`).
*   **Plugin API: implement `src/plugins/plugin-api.ts`** — registry + loader for `OrgPlugin` interface. Currently `code-highlight` and `toc` are direct imports inside the renderer.
*   **Shortcode plugin:** lift shortcode processing from `vue-generator.ts` into `src/plugins/shortcode.ts` so it works for HTML output too.
*   **Asset fetcher / handler:** implement `src/plugins/asset-fetcher.ts` + `src/assets/asset-handler.ts`. Hook `fetchRemoteAssets` (`'none'|'metadata'|'full'`) + `probe-image-size` + optional `sharp`.
*   **Client hydration:** implement `client/hydrate.ts` for Vue 3 hydration of emitted SFCs.
*   **Test suite:** populate `tests/` (vitest wired). Minimum: parser fixtures, sanitizer escape tests, template variable subs, plugin pipeline.
*   **`templates/styles.css` evolution:** add `@media (prefers-reduced-motion: reduce)` rule per `[session: kyo-web-online.md > §1.9]` so any consumer inherits the floor.
*   **`package.json` `maintainers` field:** add the ORCID-linked maintainer block mirroring kyo-web-online's shape (currently `author` is set, `maintainers` is not).
*   **Working-tree drift confirmation:** many new files untracked, 17 src files modified, `.gga` deleted, `README.md` deleted, `dist/` deletions pending rebuild. User commits these themselves — no git writes from this side.
*   **`kyo-blog` scaffold (not started).** Decision points to surface when work begins:
    - Repo layout: monorepo (`apps/blog/` + `packages/engine/`) vs sibling repos (sibling matches the current Kyonax pattern per `[dp-005]`).
    - Static-site shell: vite-ssg (matches kyo-web-online) vs Nuxt.
    - Post storage: `.org` files under `content/`, watched via `org2html watch`.
    - i18n: per-post `#+LANGUAGE:` mapping to `<html lang>` + RAW_HTML_KEYS gating.
    - Routing: vite-ssg crawler + sitemap.xml + JSON-LD per post via `use-structured-data`.

---

## SECTION 3: IMPLEMENTATIONS

> Per-deliverable detail.

### 3.1 `src/index.ts` — library entry point
*   **Path:** `src/index.ts` | **Last updated:** 2026-05-21 (preamble sweep)
*   **Role:** Public library API. Re-exports `parse`, `renderToHtml`, `applyTemplate`, all types from `types.js`. Exposes default `org2html(orgContent, options)` chaining all three.
*   **Status:** Carries the GPL-3.0-only preamble + filename block. The 4 `// Comments and More Comments` placeholder lines that violated AGENTS.md were removed in the 2026-05-21 sweep.

### 3.2 `src/types.ts` — type system
*   **Path:** `src/types.ts` | **Last updated:** 2026-05-21 (preamble sweep)
*   **Role:** All public types. RenderOptions (§1.6), OrgMetadata (§1.7), OrgOptions (§1.8), OrgPlugin (§1.12). Also: `AssetMetadata`, `RenderResult`, `NodeType` (28 node kinds), `AstNode`, `OrgAst`, `BuildConfig`.

### 3.3 `src/parser/*` — Org → AST
*   **Last updated:** 2026-05-21 (preamble sweep)
*   `lexer.ts` — tokenization (token types: `HEADING`, `CODE_BLOCK_START`, `BLOCK_START`, `TABLE_ROW`, etc.).
*   `parser.ts` — token stream → `OrgAst`. Computes `readingTime`, `wordCount`, `excerpt`.
*   `ast.ts` — `createNode`, `createTextNode`, `createDocument` factories.
*   `metadata.ts` — `extractMetadata(lines)` returns `{ metadata, contentStartLine }`. Also `calculateReadingTime`, `extractExcerpt`.

### 3.4 `src/renderer/*` — AST → HTML
*   **Last updated:** 2026-05-21 (preamble sweep)
*   `html-renderer.ts` — walks `ast.children`, renders each node, generates TOC when enabled, applies highlight/sanitize as configured.
*   `template.ts` — mustache substitution against `OrgMetadata` + `{{content}}` + `{{styles}}` + `{{structuredData}}`.
*   `sanitizer.ts` — DOMPurify + jsdom wrapper.

### 3.5 `src/plugins/*` — render-time enhancements
*   **Last updated:** 2026-05-21 (preamble sweep)
*   `code-highlight.ts` — Shiki (`^0.14.5`). Wired when `codeHighlight` is true (default).
*   `toc.ts` — `generateToc(headings, depth)` invoked by the renderer.

### 3.6 `src/cli/*` — Commander surface
*   **Last updated:** 2026-05-21 (preamble sweep)
*   `index.ts` — Commander setup (§1.14). Hardcoded `version("1.0.0")` is the drift point (§2.4).
*   `commands/build.ts` — fast-glob + lib pipeline; emits HTML + Vue SFC + metadata.json + og-metadata.json + structured-data.json + sitemap.json + feed.json + routes.js.
*   `commands/watch.ts` — chokidar-driven dev loop.
*   `commands/test.ts` — single-file preview.
*   `utils.ts` — slug + path helpers.
*   `vue-generator.ts` — see §1.13.

### 3.7 `templates/*` — engine-default ship-in-tarball
*   `default.html` — SEO-complete HTML5 doc + Mustache braces.
*   `styles.css` — 8 CSS custom properties + dark-mode `@media` override; 800px reading width.
*   `manifest.json`, `favicon.svg`, `robots.txt` — PWA / SEO defaults.

### 3.8 Reference repository — `kyo-web-online` design surface
*   **Path:** `$REPO_KWO/` (resolve per device).
*   **Role:** Canonical design contract. Sections under `[session: kyo-web-online.md > §1.5 / §1.6 / §1.13 / §1.14 / §1.22 / §1.23 / §1.24 / §1.28 / §1.29]`.
*   **Companion roam nodes:** `[[id:1277be3c-89cb-41d3-aac6-5d04a5404cb7][Index Kyo Web Online]]`, `[[id:0a059967-e5a3-43e6-b79d-ebef96171690][Kyo Web Online — Vue 3 Migration Plan]]`.

### 3.9 Convention alignment (2026-05-21 — multi-pass)
*   **Created:** 2026-05-21 | **Last updated:** 2026-05-21
*   **Status:** **DONE.** All four phases shipped: (1) Tier 1 config + governance files, (2) scripts/ composite gate, (3) docs (CHANGELOG.org / CONTRIBUTING.org / README.org), (4) license-preamble sweep across 17 src/**/*.ts. Plus three refinement rounds: .gga retirement + .gitignore rewrite, README.org rewrite with kyo-web-online org2html.txt ASCII art, concise-header refinement.
*   **Architecture references:**
    - Tier 1 header pattern → `[session: kyonax-conventions-architecture.md > ad-001]`
    - Single-license posture → `[ad-002]`
    - scripts/ ≠ Tier 1 → `[ad-003]`
    - AGENTS.md three-exception rule → `[ad-004]`
    - .gitignore omits AI agents → `[ad-005]`
    - 7-job CI gate suite → `[dp-001]`
    - precheck composite gate → `[dp-002]`
    - Protected files advisory → `[dp-003]`
    - Pre-Check Label aggregator → `[dp-004]`
    - Engine/design-contract/consumer → `[dp-005]`
    - Reference-over-copy → `[dp-006]`
    - smslant ≤12-char banners → `[cl-001]`
*   **New files (15):** see §4.6 for the full list.
*   **Modified files (4):** `.github/workflows/ci.yml` (replaced 26-line stub with 7-job suite), `.github/workflows/publish.yml` (rewritten), `AGENTS.md` (three-exception clause), `.gitignore` (rewritten with no AI-agents section).
*   **Deleted files (2):** `.gga` (retired per §1.16 + ad-002), `README.md` (replaced by README.org).
*   **Source-file sweep (17 files):** GPL-3.0-only preamble + filename description block added to every `src/**/*.ts` file. JSDoc preserved everywhere per `[ad-004]`. Legacy `// src/foo.ts` markers and `// Comments and More Comments` placeholders removed.
*   **Concise-header pass:** body-below-figlet on every Tier 1 file tightened to the new pattern (2-line tagline+date, 1-3 line WHAT/WHY, 1-3 single-line Guidelines). Section lists and verbose paragraphs cut. Scripts lost their figlet banners (kyo-web-online actual pattern).
*   **Validation:** `node scripts/precheck.mjs` → PASS on 20 files (17 src + 3 scripts). Build verification pending the user running `npm ci && npm run build`.

### 3.10 Architecture memory file — `kyonax-conventions-architecture.md`
*   **Created:** 2026-05-21 | **Last updated:** 2026-05-21
*   **Status:** **DONE.**
*   **Path:** `~/.config/doom/gptel-directives/sessions/kyonax-conventions-architecture.md`
*   **Domain:** Cross-repository conventions shared across all Kyonax projects.
*   **Source sessions:** `org-2-html.md` (this file), `kyo-web-online.md`, `kyo-recording-automation.md` (reckit — recorded as a license-posture exception).
*   **Knowledge categories:** 5 architecture decisions (`ad-001..005`), 6 design patterns (`dp-001..006`), 1 constraint (`cl-001`). Shared State & Data Flow and Reusable References sections present but empty.
*   **Consumers:** referenced from §1.2, §1.16, §1.19–§1.22 of this session, and (going forward) any new Kyonax-repo session file that adopts these conventions.

---

## SECTION 4: FILE INDEX

> Quick-reference path table. All paths are relative to `$REPO/` (`/run/media/kyonax/Da_ Disk/dev/github-kyonax/org-2-html/`) unless noted.

### 4.1 Source — library
| File | Purpose |
|---|---|
| `src/index.ts` | Public library API + `org2html()` default export |
| `src/types.ts` | All public types |
| `src/parser/lexer.ts` | Tokenizer |
| `src/parser/parser.ts` | Token stream → OrgAst |
| `src/parser/ast.ts` | Node factories |
| `src/parser/metadata.ts` | `#+TITLE:` / `#+OPTIONS:` extraction + reading-time/excerpt |
| `src/renderer/html-renderer.ts` | AST walker → HTML |
| `src/renderer/template.ts` | Mustache template engine |
| `src/renderer/sanitizer.ts` | DOMPurify + jsdom wrapper |
| `src/plugins/code-highlight.ts` | Shiki integration |
| `src/plugins/toc.ts` | TOC builder |

### 4.2 Source — CLI
| File | Purpose |
|---|---|
| `src/cli/index.ts` | Commander setup |
| `src/cli/commands/build.ts` | Build pipeline |
| `src/cli/commands/watch.ts` | Chokidar watcher |
| `src/cli/commands/test.ts` | Single-file preview |
| `src/cli/utils.ts` | CLI shared helpers |
| `src/cli/vue-generator.ts` | Vue SFC generation helpers |

### 4.3 Templates — ship in tarball
| File | Purpose |
|---|---|
| `templates/default.html` | Default HTML wrapper (Mustache + full SEO surface) |
| `templates/styles.css` | Default CSS (engine-default; consumer overrides) |
| `templates/manifest.json` | PWA manifest |
| `templates/favicon.svg` | Default favicon |
| `templates/robots.txt` | Default robots |

### 4.4 Project config
| File | Purpose |
|---|---|
| `package.json` | `@kyonax/org2html` v1.0.2, deps, scripts |
| `tsconfig.json` | TS compiler config |
| `tsup.config.ts` | Bundler config |
| `eslint.config.mts` | ESLint flat config |
| `AGENTS.md` | Code-review rules (three-exception clause per `[ad-004]`) |
| `LICENSE` / `COPYING` | GPL-3.0-only |
| `.nvmrc` | Node version pin |
| `.gitignore` | Build artifacts + secrets + OS junk + author sandboxes |
| `.editorconfig` | Editor-agnostic formatting (`[ad-001]` Tier 1 header) |
| `.gitattributes` | Per-path encoding + EOL discipline |

### 4.5 Governance + docs
| File | Purpose |
|---|---|
| `README.org` | GitHub landing page; kyo-web-online org2html ASCII art + reckit-style structure |
| `CHANGELOG.org` | Release history + TODO backlog |
| `CONTRIBUTING.org` | Contributor onboarding |
| `LICENSING.org` | License guide + per-extension header templates + Tier 1 registry |
| `NOTICE` | Attribution to author |
| `.github/CODEOWNERS` | `* @Kyonax` catch-all review router |
| `.github/SECURITY.org` | Banned patterns + enforcement + private reporting flow |
| `.github/PULL_REQUEST_TEMPLATE.md` | Default PR body |
| `.github/workflows/ci.yml` | 7-job gate suite per `[dp-001]` |
| `.github/workflows/publish.yml` | Tag-driven npm publish |

### 4.6 Scripts
| File | Purpose |
|---|---|
| `scripts/_lib.mjs` | Shared helpers (REPO_ROOT, walk, hasCcsHeader, ok/fail/head, exitWith) |
| `scripts/precheck.mjs` | Composite gate per `[dp-002]` |
| `scripts/check-license-headers.mjs` | License-preamble validator |

### 4.7 Reference — read-only design + convention surfaces
| Path | Role |
|---|---|
| `$REPO_KWO/` | Visual-language reference (`Kyonax/kyo-web-online`). Do not write. |
| `$REPO_KWO/src/assets/ascii/org2html.txt` | Canonical org2html ASCII art (used in this repo's README.org) |
| `$REPO_RECKIT/` | Convention reference (`Kyonax/reckit`). Do not write. |
| `~/.config/doom/gptel-directives/sessions/kyo-web-online.md` | Canonical kyo-web-online session — referenced from §1.10 |
| `~/.config/doom/gptel-directives/sessions/kyo-recording-automation.md` | Canonical reckit session |
| `~/.config/doom/gptel-directives/sessions/kyonax-conventions-architecture.md` | Cross-repo conventions architecture memory — referenced from §1.2, §1.16, §1.19–§1.22 |
| `~/.brain.d/roam-nodes/org_2_html/2026-05-21-org_2_html_engine_plan.org` | This project's roam plan node |
| `~/.brain.d/roam-nodes/2026-05-21-index_org_2_html.org` | This project's roam index |
| `~/.brain.d/roam-nodes/kyo_web_online/2026-05-05-vue_migration_plan.org` | kyo-web-online plan roam node (template reference) |
| `~/.brain.d/roam-nodes/2026-05-05-index_kyo_web_online.org` | kyo-web-online index (dashboard pattern reference) |

---

## SECTION 5: LAST INTERACTION

### 5.1 What was just completed (2026-05-21)
1. **Convention-alignment Phase 1-2** — created Tier 1 root files (`CHANGELOG.org`, `CONTRIBUTING.org`, `LICENSING.org`, `NOTICE`, `.editorconfig`, `.gitattributes`), governance files (`.github/CODEOWNERS`, `SECURITY.org`, `PULL_REQUEST_TEMPLATE.md`), `scripts/` composite gate (`_lib.mjs`, `precheck.mjs`, `check-license-headers.mjs`), and rewrote `.github/workflows/{ci,publish}.yml` from a 26-line stub to the 7-job Kyonax gate suite.
2. **Phase 3 — docs** — `README.md` → `README.org` (initial pass with custom pixel-text logo).
3. **Phase 4 — source-file license-preamble sweep** — added GPL-3.0-only preamble + filename description block to all 17 `src/**/*.ts` files. AGENTS.md refactored to the three-exception clause (`[ad-004]`). All existing JSDoc preserved per user direction. `// Comments and More Comments` placeholder garbage removed from `src/index.ts`.
4. **`.gga` retirement + `.gitignore` rewrite** — deleted `.gga`, scrubbed references from `ci.yml` / `SECURITY.org` / `CONTRIBUTING.org`. Rewrote `.gitignore` with the Tier 1 header format. AI-agent paths intentionally NOT carried over (`[ad-005]`).
5. **README.org rewrite (second pass)** — replaced the custom pixel-text logo with the canonical `kyo-web-online/src/assets/ascii/org2html.txt` scribe figure (+ `v1.0.2` tag). Added acronym tagline (`O.R.G.2.H.T.M.L = Org · Renderer · Generator · 2 · Hypertext · Templated · Modular · Library`) and the reckit-style shields.io badge row.
6. **Concise-header refinement** — tightened body-below-figlet on all 12 Tier 1 files to the new pattern (2-line tagline+date, 1-3 line WHAT/WHY, 1-3 single-line Guidelines). Section lists and verbose paragraphs cut. Removed figlet banners from `scripts/_lib.mjs`, `precheck.mjs`, `check-license-headers.mjs` (per `[ad-003]`). `LICENSING.org` Tier 1 table updated to exclude scripts and codify the concise-body pattern.
7. **Architecture memory extracted** — created `kyonax-conventions-architecture.md` with 5 architecture decisions, 6 design patterns, 1 constraint. Source-of-truth for cross-repo conventions referenced from §1.2 + §1.16 + §1.19–§1.22 of this session.
8. **Session-reset (this row)** — compacted the above into this session file.

### 5.2 Pending Work (carry-forward from §2.4 — quick view)
*   Fix CLI version drift (`1.0.0` → `package.json.version` via tsup `define`).
*   Implement `plugin-api.ts`, `shortcode.ts`, `asset-fetcher.ts`, `asset-handler.ts`, `client/hydrate.ts`.
*   Populate `tests/` (vitest wired; CI is tolerant of empty `tests/`).
*   Add `prefers-reduced-motion` rule to engine-default `templates/styles.css`.
*   Add `maintainers` field to `package.json` mirroring kyo-web-online's ORCID-linked shape.
*   Decide kyo-blog repo layout (monorepo vs sibling).
*   User commits the working-tree state (many untracked files, 17 modified src files, two deletions).

### 5.3 Where to resume

**If the user says "fix the CLI version drift":**
1. Read package.json version at build time via tsup `define`. Pattern: `tsup.config.ts` exports `{ define: { __PACKAGE_VERSION__: JSON.stringify(pkg.version) } }`, then `src/cli/index.ts` uses `.version(__PACKAGE_VERSION__)` (declared global in `src/types.ts` or via `tsconfig.json` `compilerOptions.types`).
2. Verify `org2html -V` matches `package.json` after `npm run build`.

**If the user says "start kyo-blog":**
1. Default layout: sibling repo at `<parent>/kyo-blog/` (matches `[dp-005]`).
2. Scaffold with vite-ssg + vue-i18n + the 16-alias map mirrored from kyo-web-online `[session: kyo-web-online.md > §1.4]`.
3. Install `@kyonax/org2html`. Wire `org2html watch content/ -o public/posts --template-dir ./blog-templates`.
4. Replace the engine-default `styles.css` with the kyo-web-online OKLCH palette via `--template-dir`.
5. Mirror the 7-job CI gate suite from this repo per `[dp-001]`.

**If the user wants to implement the plugin API:**
1. Start from `OrgPlugin` (`src/types.ts`). Build `src/plugins/plugin-api.ts` as a registry: `registerPlugin`, `runMetadataProcessors`, `runBlockHandler`, `runInlineHandler`, `runPostProcessors`. Wire into `html-renderer.ts`.
2. Refactor `code-highlight` and `toc` to register through the new API instead of direct import.
3. Add unit tests under `tests/` (this is also the moment to populate the tests/ directory).

**If the user wants to add tests:**
1. Create `tests/parser.test.ts`, `tests/renderer.test.ts`, `tests/sanitizer.test.ts`, `tests/template.test.ts`, `tests/cli.test.ts`, `tests/vue-generator.test.ts`. Fixtures under `tests/fixtures/`. vitest already wired.
2. The CI vitest job will switch from "no test files found, passing" to "running test suite" automatically.

**If the user asks "what does org-2-html actually have vs README":**
1. README.org is now in sync with the actual tree (as of 2026-05-21). The §1.4 divergence note now only tracks the *aspirational* missing modules (plugin-api, shortcode, asset-fetcher, asset-handler, client/hydrate, tests/), not file-tree drift.

**If the user asks about look-and-feel for the blog:**
1. Open §1.10 — every bullet links to the canonical rule inside `[session: kyo-web-online.md]`. Never inline-copy; load on demand.

**If the user asks about the conventions in this repo:**
1. Cross-repo origin lives at `[session: kyonax-conventions-architecture.md]`.
2. Local registry of Tier 1 banners is in `LICENSING.org` §Tier 1.
3. Per-file enforcement is `scripts/check-license-headers.mjs` (invoked by `scripts/precheck.mjs`, invoked by the `precheck` job in `ci.yml`).

**If the user wants to update memory** (`/memory`, "remember X"):
1. Save under `/home/kyonax/.claude/projects/-run-media-kyonax-Da--Disk-dev-github-kyonax-org-2-html/memory/`.

**If the user asks for a new task:** check §2.4 (Pending Work).

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session. Newest row first. See `~/.claude/skills/session-reset/rules/activity-log.md` for the schema.

| Datetime         | Duration | Type                  | Reference | Description |
|------------------|----------|-----------------------|-----------|-------------|
| 2026-05-21 10:22 | —        | session-reset         | this      | Re-invoked /session-reset via the Skill tool; no new substantive work since 04:00 — mandatory `session-reset` row prepended to satisfy the per-reset audit rule |
| 2026-05-21 04:00 | 0.5h     | session-reset         | this      | Compacted convention-alignment day: Tier 1 + CI suite + scripts + license-preamble sweep + .gga retirement + README rewrite + concise-header pass; architecture memory extracted |
| 2026-05-21 04:00 | —        | architecture-extract  | this      | Created `kyonax-conventions-architecture.md` — 5 ad (Tier 1 header / single-license / scripts-NOT-Tier-1 / AGENTS three-exception / no-AI-in-gitignore) + 6 dp (CI suite / precheck / protected-files / label aggregator / three-tier / reference-over-copy) + 1 cl (smslant ≤12-char) |
| 2026-05-21 03:30 | 0.5h     | documentation         | this      | Concise-header refinement pass across all 12 Tier 1 files (2-line tagline+date, 1-3 line WHAT/WHY, 1-3 single-line Guidelines); removed figlet banners from scripts/{_lib,precheck,check-license-headers}.mjs per [ad-003]; LICENSING.org Tier 1 table updated |
| 2026-05-21 03:00 | 0.5h     | documentation         | this      | README.org rewrite (second pass): adopted kyo-web-online/src/assets/ascii/org2html.txt scribe figure with v1.0.2 tag; added O.R.G.2.H.T.M.L acronym tagline + shields.io badge row matching reckit/kyo-web-online style |
| 2026-05-21 02:30 | 0.5h     | configuration         | this      | Retired `.gga` (deleted file + scrubbed references in ci.yml/SECURITY.org/CONTRIBUTING.org); rewrote `.gitignore` with Tier 1 header + Kyonax sections; AI-agent paths intentionally omitted per [ad-005] |
| 2026-05-21 02:00 | 0.5h     | implementation        | this      | License-preamble sweep across 17 src/**/*.ts files (GPL-3.0-only header + filename block on every file; JSDoc preserved per [ad-004]); AGENTS.md refactored to three-exception clause; placeholder `// Comments and More Comments` debt removed |
| 2026-05-21 01:30 | 0.5h     | documentation         | this      | Phase 3 docs: created CHANGELOG.org + CONTRIBUTING.org + first-pass README.org (converted from README.md, deleted README.md) |
| 2026-05-21 01:00 | 1h       | implementation        | this      | Convention alignment Phase 1-2: created 12 Tier 1 root + governance files with figlet smslant banners (LICENSING.org / NOTICE / .editorconfig / .gitattributes / CODEOWNERS / SECURITY.org / PULL_REQUEST_TEMPLATE.md / ci.yml + publish.yml); created scripts/{_lib,precheck,check-license-headers}.mjs composite gate per [dp-002] |
| 2026-05-21 00:00 | 1h       | session-reset         | this      | Bootstrap: project audited cold; README vs actual tree divergence catalogued (§1.4); kyo-web-online design contract referenced by §1.10 pointers (no inline copy); roam plan node + index seeded under `~/.brain.d/roam-nodes/org_2_html/`; skill symlinks fixed (session-memory / reckit-roam-node / tophat-tools added to ~/.claude/skills/) |
| 2026-05-21 00:00 | —        | documentation         | this      | session-memory skill applied for the design-system contract — patterns referenced from `[session: kyo-web-online.md > §1.x]` instead of duplicated |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
