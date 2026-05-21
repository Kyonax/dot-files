<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the DOTCOMPB-7929 (FAQPage JSON-LD on non-PDP templates) + DOTCOMPB-7945 (BUG: Missing BreadcrumbList JSON-LD on Shade Shop PLPs) session. Both tickets are co-tracked because they form one unified structured-data pass on non-PDP routes and ship together via the unified `additionalScripts` pipeline. Loaded at the start of every conversation to give the AI full context without re-discovering anything. Read sections in order on first load — after that, reference them by number.

| Section                  | Purpose                                                           | When to reference                            |
|--------------------------|-------------------------------------------------------------------|----------------------------------------------|
| **1. Global Guidelines** | Rules, patterns, conventions for ALL work in this session.        | Before any code task. Mandatory constraints. |
| **2. Session Overview**  | Scope, ticket status, key decisions, pending work.                | When starting a new task.                    |
| **3. Implementations**   | Per-ticket detail: AC, decisions, files, commands.                | When resuming or referencing existing work.  |
| **4. File Index**        | Quick-reference file path table — every file with line numbers.   | When reading, editing, or locating files.    |
| **5. Last Interaction**  | Short-term memory: last work, pending, resume points.             | At conversation start — entry point.         |
| **6. Activity Log**      | Datetime-stamped table of every meaningful event in this session. | When you need exact "what was done when".    |

**Operational Rule:** Always look for the last request identified by `###` title. Load relevant skills (see Section 1) and apply Section 1 rules.

**Key principle:** Data may appear in multiple sections with different framing — Section 1 frames it as a rule, Section 2 frames it as scope context, Section 3 frames it as ticket implementation. Each section answers a different question about the same knowledge.

**Roam node (single source of truth for plan):**
- File: `~/.brain.d/roam-nodes/madison_reed/2026-04-29-121311-dotcompb_7929.org`
- UUID: `bd9f004e-1c13-442a-b999-b5bdf73037c6`
- Co-tracks BOTH tickets. Updated to v14 (2026-05-01 — Tophat-first restored, supersedes v10).

**Roam index (Madison Reed master):**
- File: `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org`
- BACKLOG anchors `<<ticket-7929>>`, `<<bug-7945>>` both pointing to UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`. Sprint Board: IN CODE REVIEW lane.

**Architecture memory (this session's domain):**
- File: `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/mr-seo-structured-data-architecture.md`
- v3 (2026-04-30): 6 architecture decisions, 5 design patterns, 3 constraints, 5 reusable references.
- Cross-reference syntax: `[session: mr-seo-structured-data-architecture > <section> > <entry-id>]`.

**Compaction sources (chronological — newest first):**
- 2026-05-18 18:30 (this reset, v9): *Audit-accuracy pass.* After v8, audit still showed 8 MISSING URLs (7 `/shopping/*` + `/shop-all`) and a dedup artifact (13 non-shade `/shop/<category>` URLs reading PASS via inheritance from `/shop/brown`'s leader but not actually emitting per-slug). Investigation confirmed: **all 21 non-shade URLs have `breadcrumbs: null` in their Vuex `__INITIAL_STATE__` — no visible breadcrumb component renders on those pages**. Emitting BreadcrumbList JSON-LD there would violate Google's content-match rule. Reclassified the 21 URLs to `expected: []` in the audit list (phase `P3.9-no-breadcrumb`); the 4 DOTCOMPB-7945 AC URLs (`/shop/{brown,blonde,red,black}`) keep `expected: ['BreadcrumbList']`. Modifications: (a) `patternFor` in `.tasks/DOTCOMPB-7929/improve-site-urls.mjs` keys `/shop/<color>` against a `SHADE_SHOP_FAMILIES` set; (b) commented out 8 v2-followup lines in `.tasks/seo-jsonld-check/urls.txt` that were re-overriding the pattern (kept as comments documenting deferred scope); (c) added `🚧 Out-of-scope follow-ups` section to `template-faq-mutations.org` listing the 21 deferred URLs grouped by render mechanism + the 2-phase follow-up path (add visible breadcrumb UI to `ShopProductByCategoryV2.vue` first, then JSON-LD). Audit re-ran with 239 URLs and produced the cleanest run of the session: **52 PASS / 1 PARTIAL (cid 2903 var B, architectural) / 0 MISSING / 0 duplicates**.
- 2026-05-18 17:30 (v8): *Late-afternoon continuation of the v7 session.* (a) BreadcrumbList implementation for DOTCOMPB-7945 settled: initial Vue-component approach in `ShadeShopPage.vue` was reverted in favor of additionalScripts Pug on cid 2686 var A — same mechanism as the FAQPage mutations (consistency win, see `[session: mr-seo-structured-data-architecture > design-patterns > dp-003]`). Pug body authored, written + mirrored to production_content + stage_content, verified end-to-end. (b) "Unexpected 'i'" save-blocker traced to a real Tophat-controller divergence: `ContentVersionCtl.js:203-210` special-cases `ld+json + forceInterpolation → Pug editor mode`, but `TemplateEditCtl.js` had no such override — template scripts always opened in JSON mode → Ace JSON validator flagged Pug's leading `if` → `disableTheForm` blocked Save. Fixed `TemplateEditCtl.js` to mirror the content controller's 3-line guard (staged, single file change). (c) Tophat-tools helpers patched again — both `add-jsonld-script.mjs` and `add-jsonld-to-template.mjs` now write `_editorOptions.mode` + `_editorHasErrors` natively (mode picked by `forceInterpolation` flag); 4 pre-patch entries retrofitted in-place; all 3 ADD mutations rerun cleanly via patched helpers. (d) QA-handoff doc rebuilt: top-of-file COPY-PASTE REPLAY CHECKLIST with 4 self-contained surface blocks (URL + Pug + checkboxes + Verify curl per surface) so anyone can click-paste through Local-then-Dotcom in minutes; deep reference content preserved below. (e) `code-review` pass on the staged Tophat fix flagged a 4-line comment as overlong per the project's "2-3 lines max for *why* comments" rule — condensed to 2 lines. (f) DOTCOMPB-7945 ticket pulled via Atlassian MCP: ACs are 4 URLs (`/shop/{brown,blonde,red,black}`) with specific `itemListElement` triad — out-of-scope `/shop/<category>` (13 URLs) and `/shopping/*` + `/shop-all` (8 URLs) untouched. (g) Roam node `2026-04-29-121311-dotcompb_7929.org` now carries `* COMMIT MSG` and `* PR DESCRIPTION` sections per MR brand (`pr-scribe brand-madison-reed`), drafted for the staged Tophat controller fix.
- 2026-05-18 16:00 (v7): partial-template-level JSON-LD pattern proven (`[session: mr-seo-structured-data-architecture > design-patterns > dp-006]` + `> architecture-decisions > ad-007`). Three mutations applied via tophat-tools: cid 2350 var A R1 removed, template 1211 (`video-chat-faqs`) + 1375 (`faqs-with-icons-pro`) configured with partial-level Pug — single config per template covers all bound content docs. Audit re-run shows 27 PASS / 1 architecturally-correct PARTIAL / 0 duplicates / 25 MISSING (all BreadcrumbList, DOTCOMPB-7945 scope). Tophat-tools enhanced: `add-jsonld-script.mjs` `body→text` bug patched, `add-jsonld-to-template.mjs` + `remove-jsonld-script.mjs` written. Audit harness refined: 3 top-of-report action lists, progressive multi-line TUI with bare Tophat URLs, enriched member detail. QA-handoff doc `.tasks/DOTCOMPB-7929/template-faq-mutations.org` written.
- 2026-05-04 17:45 (v6): hotfix-google branch (DOTCOMPB-8174) work captured in parallel with SEO track. Final approach: drop custom MrBtn, use `gai.renderButton()`. PR #20716 open. Hotfix implementation lives in `dotcompb-7942-google-sso-booking.md`; this file logs the events.
- 2026-05-01 12:55 (v5): harness refinement pass — Tier 1 dedup (`dedup.mjs`, group by content_id / manual `group=<label>`), CSV ingest (`csv-ingest.mjs`, AEO Roadmap Sheet2), FAQ-content detector (`faq-detector.mjs`) + `NOT_A_CANDIDATE` status, fail-fast on FETCH_ERROR + dev-SSR-aware throttle defaults, JSON-LD blocks embedded in org+JSON reports, cwd path-resolution fix, csv-ingest hardened against tab-separated input. New MR-specific tooling: `.tasks/DOTCOMPB-7929/improve-site-urls.mjs` overlays curated metadata + pattern rules onto the 499-URL CSV, dropping 260 non-SEO routes → 239 audit-worthy URLs collapsing to ~50 leader audits. Site-wide audit attempted twice; both halted by Vue dev SSR OOM (heap exhaustion under sustained PDP rendering). Audit not yet successfully completed — needs dev restart.
- 2026-05-01 04:30 (v4): v14 plan (Tophat-first restored — supersedes v10); harness rewritten — experiment-aware, raw-HTML-only verification, single-URL mode, `--show-raw` JSON-LD dump, AC5 duplicate detection, optional Tophat-link enrichment via single Mongo query at startup; harness migrated to `seo-analyzer/jsonld-check/` with comprehensive README.org; PB.1 *temporarily completed* via direct MongoDB writes — pending validation/redo via proper Tophat editor flow.
- 2026-05-01 00:30: v3 reset. v8-v13 plan refinements + Pug-interpolation accuracy work + harness CMS-state inspection.
- 2026-04-30 17:00: v2 reset. v3-v7 plan refinement (DB ground-truth audit, AskUserQuestion gates, AEO Roadmap KPI, execution discipline, Tophat configuration discovery).
- 2026-04-29 19:00: First reset. Architecture memory v1 extracted (ad-001 through ad-004, dp-001, cl-001, rr-001, rr-002).

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `mr-dotcom-dev`, `mr-roam-node`, `seo-web-quality`, `code-review`, `mr-style`, `session-reset`. This section stores session-scoped patterns not yet captured in those skills — staging for guidelines that may eventually be promoted there.

### 1.1 Scope Boundary — Tophat-first, code-as-residue (v14, supersedes v10 unified mechanism)

*   **Default fix path:** open Tophat for the URL's content, configure the JSON-LD script in *Advanced Config → Scripts → +*, save, re-run audit. Repeat per URL.
*   **Code path is invoked only when Tophat is exhausted:** routes truly outside CMS (PDPs, `/shop-all`), or routes where *every* Tophat configuration alternative fails to take effect.
*   **Audit is the gate.** The harness is the source of truth for whether a URL's JSON-LD is reaching the SEO crawler. Tophat is the configuration surface; the audit decides whether the configuration worked.
*   **Direct DB writes are NOT a shipping mechanism.** They may be used for diagnosis (proving a Pug script compiles + emits) but are wiped by the next Tophat publish. Never ship via DB writes — go through Tophat editor or Phase C code.
*   **One reference implementation:** `/colorbar/locations` directory (content_id 2349) emits FAQPage via Tophat auto-gen — regression sentinel. Slugged children of `/colorbar/locations/` (content_id 2350) need additional R2 Pug FAQPage configured **per variation** (see 1.10 / 1.11 below).

### 1.2 JSON-LD Pipeline — short reference (full reference in roam node)

> **Canonical deep-dive lives in the roam node** at section `* JSON-LD MECHANISM REFERENCE — full Tophat-to-HTML flow` (`#json-ld-mechanism-reference`). 12 subsections: pipeline overview, 2 storage levels (template + content), Tophat UI source pointers, full script descriptor field reference, request-time pipeline diagram, interpolation context, 3 verified production examples (R1/R2/R3), generic recipe + decision tree for ANY Schema.org `@type`, gotchas table, file:line references, Tophat checkbox cheat-sheet. **Read it before designing any JSON-LD task.**

```
[SAVE TIME — Tophat]                    [REQUEST TIME — render]
content.renderOptions.additionalScripts ── htmlRenderer.js:198-214
                                              ├─ addScriptsDefsInto({content.renderOptions, ...})
                                              └─ addScriptsDefsInto({template, ...})
                                                  └─ pug.compile(_pugScript, locals)  ← SILENT FAILURE here on error
                                                      → locals.header.scripts.push(...)
                                                          → vue-layout-ssr.pug:24-26 each scriptDefinition
                                                              → raw HTML <head>
```

**Three writers** produce `additionalScripts[]` entries:
- **Path R1 — Tophat auto-gen** (dp-002): `addFaqMetadata: true` flag on a `componentList` component triggers `ContentEditCtl.getAdditionalScriptForFAQs` at save time. Sets `generatedAutomatically: true`.
- **Path R2 — Tophat hand-authored Pug** (dp-003): `forceInterpolation: true` + Pug source for runtime interpolation (`#{...}` / `!{...}` / `each`). Used by `/colorbar/locations/` for HairSalon.
- **Path R3 — Route-handler push** (ad-006): code calls `pushJsonLdToContent(content, schema, metadata)` after CMS content load. Reserved for routes whose schemas can't be authored in Tophat.

**Tophat checkbox cheat-sheet for JSON-LD scripts:** type=`ld+json`, isUrl=`false`, inHeader=`true`, forceInterpolation=`false` (static JSON) or `true` (Pug-driven), addBodyLoadScript=`false`. UI source: `tophat/src/views/ngpartials/cms/content/edit.pug:~600` + controller `tophat/src/ngscripts/cms/ContentEditCtl.js`.

### 1.3 JSON-LD Output Rules

*   **Server-rendered.** JSON-LD must appear in raw HTML (verify via the audit harness). Client-side-only injection is **not acceptable** (AC4).
*   **Inject in `<head>`.** All `additionalScripts` entries must use `inHeader: true`.
*   **One block per `@type` per page (AC5).** The harness flags duplicates per variation (e.g., R1 auto-gen + R2 Pug both emitting FAQPage).
*   **Empty content → no block.** Schema builders return `null` when input is empty. AC6.
*   **Sanitize answer HTML in code-built schemas only** (Path R3): allowlist `[a, br, ol, ul, li, p, strong, em]`. AC2 sanitizer applies ONLY to Path R3 builders.
*   **`Question.name` is plain text.** No HTML.
*   **Pug-interpolated answer text:** use `!{JSON.stringify(faq.answer)}` (unescaped) so JSON.stringify handles quote/backslash escaping while Pug doesn't HTML-escape `<` to `&lt;`. Plain `#{...}` corrupts JSON in script tags.

### 1.4 Canonical Schema Shapes

- FAQPage canonical JSON template + worked example: roam node TICKET CONTEXT section (anchor `#ticket-context`, immediately after the `Schema Structure (FAQPage):` block). Two `#+begin_src json` code blocks: a fillable template and the actual rendered output for `/colorbar/locations/<slug>`.
- BreadcrumbList: roam node TICKET CONTEXT section, immediately after FAQPage. Plus reusable references rr-001 / rr-002 in architecture memory.
- additionalScripts entry: rr-003.

### 1.5 Shared Utility Discipline (relevant only when Phase C code residue lands)

*   **Pure functions only.** Schema builders take plain data, return JSON objects (or `null`). No `req`, no globals, no side effects.
*   **Sanitizer is its own module.** `sanitizeFaqAnswerHtml(html)` exported from `mr_modules/cms/lib/jsonLd/sanitize.js`.
*   **Builders live at `mr_modules/cms/lib/jsonLd/`** (G1.1 default).
*   **No webservices-layer logic.** Schema work goes in `mr_modules/cms/lib/`, never `mr_modules/webservices/lib/`.
*   **Backward-compatible exports.** Existing `buildFaqPageJsonLd({ product, productUrl })` callers must keep working.
*   **Push helper invariant.** `pushJsonLdToContent` from route handlers MUST NOT set `generatedAutomatically: true` — that flag is reserved for Tophat auto-gen and triggers wipe on next editor save.

### 1.6 SSR Safety

*   `mr_modules/` and SSR-executed Vue paths use `require('Log')` (no `console.*`).
*   No `window`, `document`, `localStorage` unless guarded by `typeof window !== 'undefined'` or `import.meta.env.SSR`.

### 1.7 QA Verification — the audit harness is the only verification path

**Verification = raw HTML only. Never Mongo, never Tophat, never the dev DB. The harness fetches the URL and reads what an SEO crawler reads.**

The canonical harness lives at `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/` (with a sibling copy at `/Volumes/dev-partition/github-madison-reed/the-code/.tasks/seo-jsonld-check/`). Behavior:

1. **Fetch** the URL (native `fetch`, no JS execution, no cookies).
2. **Detect experiment** from rendered HTML markers (`experimentId`, `experimentName`, `variationKey`).
3. **Probe each variation** by re-fetching `?v=<key>&xid=<experimentId>` for A/B/C/D… (the same hooks `mr_modules/cms/lib/loaders.js#getContentVariation` exposes for QA).
4. **Per variation:** extract every `<script type="application/ld+json">` block, parse, validate shape (FAQPage / BreadcrumbList rules per AC1/AC2/AC3/AC9), check JSON validity (trailing commas, multi-object concatenation, parse errors), enforce AC5 (single block per `@type` per page), count Question entities.
5. **Status:** `PASS` only when *every* variation emits the expected schema(s), valid and unique. `PARTIAL` when some have it; `MISSING` when none; `INVALID` when malformed.

**Per-task verify (every Phase B Tophat attempt):** run the harness against the touched URL(s); confirm the variation breakdown shows ✓ for every variation. The harness's `--show-raw` flag prints every JSON-LD block pretty-printed for inspection ("what does the SEO actually see?"). The harness's optional Tophat-link feature (one Mongo query at startup) surfaces the `http://localhost:4000/#/cms/content/edit/<content_id>` URL per audited URL — for navigation only, never for verification.

**Site-wide audit (added 2026-05-01):** the harness now supports a 1-command site-scale run via `--csv <path>` (Sheet2 from the AEO Roadmap) and Tier 1 dedup by CMS `content_id` (default — collapses 100s of URLs into ~10s of leader audits). The FAQ-content detector classifies pages with no FAQ markup as `NOT_A_CANDIDATE` so the actionable list isn't drowned by genuinely irrelevant URLs. The org report's "URLs needing a Tophat tweak" section is the canonical hot-list — pages with FAQ content + missing/invalid FAQPage JSON-LD, grouped by template, sorted by group size. Reports embed every parsed JSON-LD block per leader/variation. Fail-fast on `FETCH_ERROR` (default on) protects an OOM-prone Vue dev SSR; URLs not yet reached are written as `SKIPPED`. See `seo-analyzer/README.org` "Site-wide audit from CSV" + "Deduplication (Tier 1)" sections.

**Pre-merge sign-off (Phase 6 / P6.4):** Google Rich Results Test on QA URLs (https://dotcom.mdsnrd.com/...). Zero errors, zero warnings.

### 1.8 Code Review / Style

*   `mr-dotcom-dev` rules apply for any Vue/Pug/Vuex changes.
*   `mr-style` rules apply for any template/style edits.
*   JSDoc block description mandatory on every public function.
*   Lint before commit: `bash /Volumes/dev-partition/github-madison-reed/the-code/.tasks/lint-changed.sh`.
*   No `console.*` — use `require('Log')` server-side.
*   Brackets for if/else — no one-line conditionals.

### 1.9 Tracking & Observability

*   **No new Segment events.** SEO work is invisible to user.
*   **Sentry watch post-deploy:** `JSON.stringify` errors and sanitizer throws — both should degrade to "no schema block", not crash.

### 1.10 Validated Architectural Facts (DO NOT re-derive)

*   **CMS database name is `cms`** (NOT `madisonreed`). Use `docker exec mr-mongo mongosh cms`.
*   **JSON-LD storage path:** `cms.contentVersion {content_id, version}.renderOptions.additionalScripts[]`. Live rendering also reads `production_content` / `stage_content` `.variations.{platform}[].renderOptions.additionalScripts[]` denormalized copies.
*   **Multi-variation reality (CORRECTS F23 misdiagnosis):** each `db.contentVersion` lookup for `(content_id, version)` returns multiple records — one per A/B/C variation. Plus `production_content[content_id].variations.desktop[]` is the renderable copy with the same A/B/C variations. The CMS picks the variation served per request based on cookie/experiment state. **Tophat edits target ONE variation at a time.** Per-variation `componentList` entries and `mixin_key` may differ — content_id 2350 variation A uses `hcb-location-page` (V1 mixin) while B/C use `hcb-location-page-v2` (V2 mixin). `/colorbar/locations/<slug>` requests typically hit variation B → no auto-gen FAQPage (R1 was only configured on variation A) → looked like a "pipeline drop" but was actually a "missing config in B/C." F23 in the roam node is superseded by this finding; the harness now correctly probes all variations and reports per-variation status.
*   **`how-to-question-accordion-vue` is THE canonical FAQ component** — `hcb-location-page` / `hcb-location-page-v2` are HCB-specific FAQ-bearing components.
*   **Tophat auto-gen runs at SAVE only.** Pre-existing content docs need editor re-save before the auto-gen FAQ script appears.
*   **Pipeline silent-failure mode:** `scriptsUtils.js:addScriptsDefsInto()` lines 134-142 wraps `pug.compile()` in try/catch. On error: `log.error(err)` then `_htmlScript = ''`. Easy to miss without enabling `DEBUG=cms.scriptsUtils`.
*   **Dev server in-process content cache:** the local dev server caches content per request handler — direct DB writes to `contentVersion` / `production_content` are not reliably picked up until either the dev server restarts or Tophat's publish event fires. This invalidates raw DB writes as a verification mechanism for "did Tophat config land?" — always go via the actual Tophat UI flow.

### 1.11 Tophat editor surface — A/B/C variations are separate edits

Tophat's "Advanced Config / Scripts" panel is per-content-doc but the saved scripts go into the *currently selected variation* (A is the default panel). To configure JSON-LD for an A/B-tested URL, the editor must:

1. Open the content doc (e.g., content_id 2350).
2. Switch the variation selector (typically a dropdown / tab labeled by `variationKey`).
3. Add the script in *Advanced Config → Scripts* for variation A.
4. Repeat for B, C, etc.
5. Save each variation separately.

**This is the most common reason a "Tophat save" doesn't reflect on the live URL** — the editor configured variation A but the request hits B. The harness's per-variation probing (`?v=X&xid=Y`) makes this obvious immediately.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Add `FAQPage` JSON-LD to non-PDP templates and `BreadcrumbList` JSON-LD to the 4 Shade Shop PLPs. The injection mechanism is the existing unified `additionalScripts → header.scripts → vue-layout-ssr.pug:24-26` pipeline (validated in production for 14/15 JSON-LD-bearing URIs).

**SEO outcome:** Every CMS-FAQ-bearing page becomes Rich Results-eligible (FAQPage); Shade Shop PLPs gain BreadcrumbList eligibility; PDPs gain crawler-visible FAQPage.

### 2.2 Scope

| Ticket          | Type             | Status                                                                              | Anchor            |
|-----------------|------------------|-------------------------------------------------------------------------------------|-------------------|
| `DOTCOMPB-7929` | Story            | **In Code Review** (PB.1 done via DB-write workaround; awaiting Tophat-proper redo) | `<<ticket-7929>>` |
| `DOTCOMPB-7945` | Bug — co-tracked | **In Code Review** (PB.2 pending)                                                   | `<<bug-7945>>`    |

JIRA: https://madison-reed.atlassian.net/browse/DOTCOMPB-7929 / https://madison-reed.atlassian.net/browse/DOTCOMPB-7945
Related: DOTCOMPB-7466 (parent of bug), DOTCOMPB-7230 (predecessor SPIKE), PR #20512 (where breadcrumb gap originated).

### 2.3 Key Decisions

1.  **(2026-04-29)** Co-track 7929 + 7945 on a single roam node (UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`). Both ship together.
2.  **(2026-04-29)** Refined ACs to 10 strict + 10 DEV-AC. AC4 mandates raw-HTML presence. AC10 mandates automated checker harness.
3.  **(2026-04-29)** Verification harness location: originally `.tasks/seo-jsonld-check/`. Stack: native fetch + regex (no axios/cheerio after Node 18 undici/`File` issue).
4.  **(2026-04-30 morning)** Tophat is auth-walled; CMS data inspection done via direct Mongo (`docker exec mr-mongo mongosh cms`). Discovered unified pipeline.
5.  **(2026-04-30 mid)** AEO Roadmap CSV imported as companion roam node. KPI: FAQ Questions Indexed ~0 → 200+.
6.  **(2026-04-30 mid)** v6 plan refinement (execution discipline) — temporal framing dropped; Phase 5 reordered before Phase 3; Routes affected + Per-task verify lines on every code-touching task.
7.  **(2026-04-30 evening)** v8 audit-first restructure — built harness ahead of Phase 5 schedule; Phase A (audit + classify) and Phase B (manual Tophat validation) inserted before Phase 1.
8.  **(2026-04-30 evening)** v10 unified mechanism — collapsed R1/R2/R3 into a single route-handler push. **Superseded by v14.**
9.  **(2026-04-30 evening)** v11-v12 documentation deep-dive — wrote canonical 12-section JSON-LD MECHANISM REFERENCE in roam node + Pug-interpolation accuracy refinement.
10. **(2026-04-30 evening, 23:30)** v13 harness upgrade — `cms-check.mjs` for CMS state inspection; BROKEN-IN-CMS classification; strict JSON validity. **Removed in this session's harness rewrite.**
11. **(2026-05-01 — this session)** **v14 plan refinement (Tophat-first restored — supersedes v10).** Audit drives Tophat config attempts URL-by-URL; code is the residue. Phase B re-instated as canonical. Phase C invoked only for routes outside CMS or where Tophat can't take effect.
12. **(2026-05-01 — this session)** **Harness rewritten for Tophat-first discipline.** Verification = raw HTML only. Experiment-aware: probes A/B/C variations via `?v=&xid=` overrides. New flags: `--url <single>`, `--types <a,b>`, `--show-raw` (pretty-prints every JSON-LD block — what the SEO crawler actually sees), `--tophat-base`, `--no-cms-link`. AC5 duplicate detection added. Question count semantics changed from sum-across-variations to max-per-URL summed-across-URLs (a crawler hits one variation per crawl).
13. **(2026-05-01 — this session)** **Optional Tophat link enrichment** — single Mongo round-trip at startup resolves URI → content_id (with `takesUrlParameters` parent fallback). Surfaces a clickable `http://localhost:4000/#/cms/content/edit/<id>` per URL. Decorative only; verification still raw-HTML. Disabled with `--no-cms-link` when Mongo unavailable.
14. **(2026-05-01 — this session)** **Harness migrated to seo-analyzer.** Canonical home: `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/`. Comprehensive `README.org` (~19 KB, 12 sections: overview, install, CLI flags, npm scripts, URL list format, status taxonomy, output formats, experiment detection, Tophat link resolver, examples, validators, architecture, troubleshooting, programmatic API). MR copy at `.tasks/seo-jsonld-check/` retained as a sibling but the seo-analyzer copy is now the canonical one for cross-project reuse.
15. **(2026-05-01 — this session)** **F23 misdiagnosis corrected.** Earlier sessions thought slugged children "dropped" the auto-gen FAQPage (pipeline bug). Actually: each contentVersion has multiple records per (content_id, version) — one per A/B/C variation — and the auto-gen R1 was only configured on variation A. Slug requests typically hit B/C, which never had the script. Not a pipeline bug; a per-variation config gap.
16. **(2026-05-01 — this session)** **PB.1 *temporarily completed* via direct MongoDB writes** to all 3 contentVersion variations + `production_content` + `stage_content` for content_id 2350. Made `/colorbar/locations/hillsboro` flip from BROKEN-IN-CMS → PASS (then PARTIAL once AC5 duplicate detection landed — variation A now emits 2 FAQPage blocks: pre-existing R1 auto-gen + our R2 Pug). **NOT the Tophat-first solution.** DB writes will be wiped by next Tophat publish event. Pending: validate why this approach was used and decide whether to redo via proper Tophat editor flow per variation.
17. **(2026-05-01 afternoon)** **Tier 1 deduplication formalized.** New `seo-analyzer/jsonld-check/dedup.mjs` groups input rows by CMS `content_id` (auto, via cms-link.mjs) or manual `group=<label>` token in urls.txt. Only the leader of each group is fetched + audited; members inherit the verdict and the leader's question count (so the AC11 KPI sums leader_count × group_size automatically). Disable with `--no-dedup`. 239 URLs collapse to ~50 leader audits. Caveats: (a) member-specific expected types ignored (leader's are used), (b) per-content-doc DB drift undetected — re-run with `--no-dedup` to verify.
18. **(2026-05-01 afternoon)** **Fail-fast halt + dev-SSR-aware pacing.** Default `--throttle 1500ms` between leaders, `--variation-throttle 400ms` between A/B/C re-fetches, retries-on-AbortError in fetchers.mjs (2 retries with backoff). `--stop-on-fetch-error` (default on) halts the run on the first FETCH_ERROR — protects an OOM-prone Vue dev SSR from continued hammering and writes a partial report with `SKIPPED` rows for unreached URLs. Disable for resilient long runs (e.g. against a stable QA host).
19. **(2026-05-01 afternoon)** **Site-wide audit pipeline.** CSV ingest (`csv-ingest.mjs` reads "Page URL" column, strips host, dedupes) + FAQ-content detector (`faq-detector.mjs` scans raw HTML for FAQ Vue components / hcb-faqs class / "Frequently Asked Questions" heading / already-emitted FAQPage JSON-LD) + `NOT_A_CANDIDATE` status when expected-FAQPage but no FAQ content + "URLs needing a Tophat tweak" section in the org report (filtered, grouped by template, sorted by group size). Curated urls.txt overrides applied via `.tasks/DOTCOMPB-7929/improve-site-urls.mjs` (overlays scope/phase tags + applies pattern rules: PDPs → `group=PDP`, blog → `group=blog-post`, /shop/* → BreadcrumbList, drops booking/auth/dashboard). 499 raw URLs → 239 audit-worthy.
20. **(2026-05-01 afternoon)** **Reports embed parsed JSON-LD.** Every `<script type="application/ld+json">` block found in raw HTML is embedded in the org report under each leader (pretty-printed inside `#+begin_src json`) and the JSON report's per-variation `blocks` array. Disable with `--no-embed-jsonld`.

### 2.4 Pending Work (v14 plan)

**🚨 NEXT ACTION — VALIDATION:** *Validate why PB.1 was completed via direct MongoDB write rather than via the Tophat UI editor flow.* This was a workaround taken by the AI mid-session when the audit kept showing BROKEN-IN-CMS despite the user's statement that Tophat was configured. The actual problem was that the user's Tophat save likely targeted variation A only, while the slug request hits variation B. The AI bypassed Tophat entirely by writing directly to MongoDB. This is **not aligned with the Tophat-first principle** in 1.1. Options to evaluate:

- **(a)** Keep DB-write approach for now. Acknowledge it as development-only verification; will be wiped on next Tophat publish and need redoing.
- **(b)** Redo properly via Tophat editor. Open content_id 2350; switch variation selector to A, save R2 Pug script in Advanced Config → Scripts; repeat for B and C. Re-run audit; confirm all three variations PASS. Document the per-variation editor workflow as the canonical process. *Recommended.*
- **(c)** Document DB-write as a forensic tool only. Add a section in the roam node clarifying that DB writes are valid for *diagnosis* (e.g., proving the Pug script compiles + emits) but **never** for shipping. Production fix path = Tophat editor only (or Phase C code residue when CMS isn't usable). *Recommended in addition to (b).*

**Other Phase B tasks (in order):**
- **PB.2** — `/shop/*` BreadcrumbList. All 8 `/shop/*` URLs share content_id 2686. One Tophat R2 hand-authored Pug entry there covers `/shop/brown` (v1) + 7 v2 followups. Requires populating `req.metaData.shadeFamilyName` / `colorSlug` in route handler (small code change in `views.js`). Recipe in §3.2.
- **PB.3** — `/schedule-video-chat` FAQPage. content_id 2536, FAQ in page=YES. Try R1 auto-gen on `video-chat-faqs` component first; fall back to R2 if shape mismatches.
- **PB.4** — Audit re-run + categorize residue. Embed updated `audit.org` into roam node between BEGIN-AUDIT/END-AUDIT.

**Phase C — code residue (only if PB.1–PB.3 leave anything failing):**
- **PC.1** — Phase 1+2 utilities (sanitizer, builders, push helper, extractFaqsFromContent). Only what residue requires.
- **PC.2** — PDP `/product/:slug` route-handler push (always required — PDPs are not in CMS).
- **PC.3** — Any other residue URL.
- **PC.4** — Convert `productJsonLdUtils.updateFaqPageJsonLdScript` to no-op + dedup (after PC.2).

**Phase 5/6/7 — sign-off & post-deploy:**
- **P5.5** — Playwright spec at `.tasks/qa-automation/DOTCOMPB-7929/specs/dotcompb-7929-jsonld.spec.ts`.
- **P6** — Lint clean, tests clean, `npm run jsonld-verify` exits 0, Rich Results Test on QA, push + PR + merge.
- **P7.1** — Capture `baselines/t0.json` post-deploy.

**v2 follow-up ticket:** P3.5–P3.8 (Tophat editor work for `/help`, `/blog`, `/`), SHOP_CATEGORIES expansion, P3.10 PDP catalog chase, P7.2/P7.3 KPI checkpoints.

### 2.5 Decision Gates (canonical list mirrors roam node)

| Gate | Status | Scope | Default selection | Blocks | What it decides |
|---|---|---|---|---|---|
| G0.1 | answered | v1 | "Local only" | P0.1 [DONE] | Branch state |
| G0.3 | pending | v1 | "Paste URI list" | P0.3, Phase 5 seed | URL Scope source |
| G0.5 | pending | mixed | q1 *No*, q2 *Keep both*, q3 *4 shades only* | P3.9 scope | F10.1 + F10.2 + F10.10 |
| G1.1 | conditional | v1 | "New `jsonLd/` subdir" — only fires if Phase C lands | All Phase C | Module location |
| G3.5_7 | pending | v2 | "None of the above" | P3.5–P3.7 | Catch-all FAQPage scope |
| G3.9 | pending | v1 | "4 shades only" | P3.9 | SHADE_SHOP_FAMILIES list |
| G6.4 | pending | v1 | "QA only" | P6.4 | Rich Results Test scope |

Each gate has a fully-formed `⚙️ GATE` block in the roam node TASKS section specifying the exact `AskUserQuestion` field shape.

---

## SECTION 3: FEATURE / TICKET IMPLEMENTATIONS

### 3.1 DOTCOMPB-7929 — FAQPage JSON-LD on Non-PDP Templates

**Created:** 2026-04-29 | **Last updated:** 2026-05-01 (v14)
**Status:** In Code Review — branch `DOTCOMPB-7929` clean, no commits yet (PB.1 work was DB-only — no source changes; awaiting Tophat-proper redo)
**Branch:** `DOTCOMPB-7929` (local; remote/PR state via gate G0.1)
**JIRA:** https://madison-reed.atlassian.net/browse/DOTCOMPB-7929
**Roam node:** `~/.brain.d/roam-nodes/madison_reed/2026-04-29-121311-dotcompb_7929.org` (UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`)

#### Acceptance Criteria — 10 strict (full GIVEN/WHEN/THEN in roam node TICKET CONTEXT)

| AC | Title | Verification |
|---|---|---|
| AC1 | Shared FAQ schema builder, exported and pure | Unit tests for `buildFaqPageJsonLd({ faqs, pageUrl, about })` |
| AC2 | Sanitizer with allowlist on Answer.text | Unit test allowlist enforcement |
| AC3 | Question.name is plain text | Unit test: HTML in question → 0 tags in output |
| AC4 | JSON-LD present in raw HTML | `npm run jsonld-check -- --url <url>` shows ≥1 ld+json with the expected `@type` per variation |
| AC5 | Single FAQPage per page; multi-source merge | Harness flags duplicate emissions per variation |
| AC6 | Empty-content guard | Harness shows 0 ld+json on a known empty-FAQ page |
| AC7 | CMS-driven propagation without code deploy | Tophat edit → reload → schema reflects |
| AC8 | Per-template wire-up enumerated | URL Scope spreadsheet exhaustively wired (gate G0.3) |
| AC9 | BreadcrumbList JSON-LD on 4 Shade Shop PLPs | Section 3.2 |
| AC10 | Automated verification harness committed | `seo-analyzer/jsonld-check/` (canonical) + MR sibling at `.tasks/seo-jsonld-check/` |
| AC11 | KPI alignment with AEO Roadmap target | `faqQuestionCountTotal: 200+` in audit summary |

#### Per-Route Wiring Table

| Route | Path | Action |
|---|---|---|
| `/colorbar/locations` | R1 (Tophat auto-gen) | DONE (reference) |
| `/colorbar/locations/<slug>` | R2 (Tophat hand-authored Pug — per variation) | **PB.1: DB-write workaround applied; Tophat-proper redo PENDING** |
| PDP `/product/:slug` | R3 (route-handler push — code) | **PC.2 — always required (not in CMS)** |
| Bundle `/bundle/perfect-pair/:slug` | n/a (no FAQ content) | VERIFY-ONLY post-PC.2 |
| `/help` | R1 (Tophat — gated by G3.5_7) | v2 |
| `/blog` landing | R1 | v2 |
| `/` | R1 | v2 (deprioritized) |
| `/schedule-video-chat` | R1 (Tophat — try first) | PB.3 (v2 scope) |
| `/shop/{brown,blonde,red,black}` | R2 (Tophat — shared content_id 2686) | **PB.2** |
| `/shop/{all-hair-color, …}` | R2 (same content_id, shared config) | v2 (gate G3.9) |
| 16 PDPs with `pdp-tabs-reviews-faqs` | R3 (resolved by PC.2) | verify post-PC.2 |

#### PB.1 worked example — the FAQPage Pug we landed (currently in DB only, NOT in Tophat)

R2 Pug script written into content_id 2350 contentVersion + production_content + stage_content for variations A/B/C:

```pug
if metaData && metaData.code && settings && settings.componentList
  - var hcbComp = settings.componentList.find(function(c) { return c.settings && c.settings.faqs && c.settings.faqs.length > 0; })
  if hcbComp
    | {
    |  "@context": "https://schema.org",
    |  "@type": "FAQPage",
    |  "mainEntity": [
    each faq, idx in hcbComp.settings.faqs
      if idx > 0
        | ,
      | {
      |  "@type": "Question",
      |  "name": !{JSON.stringify(faq.question && faq.question.mainQuestion ? faq.question.mainQuestion : faq.question)},
      |  "acceptedAnswer": {
      |    "@type": "Answer",
      |    "text": !{JSON.stringify(faq.answer)}
      |  }
      | }
    | ]
    | }
```

Tophat checkbox state: type=Ld-Json, isUrl=☐, inHeader=☑, forceInterpolation=☑, addBodyLoadScript=☐.

The find function uses `c.settings.faqs.length > 0` (not a specific mixin name) so it handles both V1 (`hcb-location-page`) and V2 (`hcb-location-page-v2`) variations.

`metaData.code` gate ensures the script only emits on slug pages (where `req.metaData = locationSummary` is populated by `views.js:1593-1607`), not on the directory page (which already passes via R1 auto-gen on variation A).

#### Test Coverage

Per AC10/AC11: harness `npm run jsonld-audit` produces `reports/audit.org` + `audit.json`. Question count rolled up across URLs.

---

### 3.2 DOTCOMPB-7945 — BreadcrumbList JSON-LD on Shade Shop PLPs

**Created:** 2026-04-29 | **Last updated:** 2026-05-01 (v14)
**Status:** In Code Review — co-tracked with 7929; PB.2 pending
**JIRA:** https://madison-reed.atlassian.net/browse/DOTCOMPB-7945

#### Bug Reproduction

Live audit: every `/shop/*` URL in the audit set shows `MISSING — no experiment` for BreadcrumbList. All 8 URLs share content_id 2686 (resolved via parent `/shop/`).

#### Affected Pages

| Color | Dev | QA | Production |
|---|---|---|---|
| Brown | localhost:3000/shop/brown | dotcom.mdsnrd.com/shop/brown | www.madison-reed.com/shop/brown |
| Blonde | …/shop/blonde | …/shop/blonde | …/shop/blonde |
| Red | …/shop/red | …/shop/red | …/shop/red |
| Black | …/shop/black | …/shop/black | …/shop/black |

#### Implementation Detail (PB.2 plan)

`/shop/*` is a CMS-driven route under content_id 2686 (template `specific-shop-product-config`). additionalScripts is empty at both content and template levels.

**Tophat attempt (R2 hand-authored Pug):**

1. Open content_id 2686 in Tophat.
2. For *each* desktop variation (probe with the harness first to discover how many — likely just default since the harness reports "no experiment" today).
3. Add a hand-authored Pug BreadcrumbList script in *Advanced Config → Scripts*:
   - Type: `Ld-Json`
   - isUrl: ☐
   - inHeader: ☑
   - forceInterpolation: ☑
   - addBodyLoadScript: ☐
4. Pug source uses `req.metaData.shadeFamilyName` populated by route handler. *Code change required:* extend the `/shop/:colorFamily` Express handler in `views.js`:
   ```js
   const SHADE_SHOP_FAMILIES = Object.freeze({
     brown: 'Brown Shades', blonde: 'Blonde Shades',
     red: 'Red Shades', black: 'Black Shades',
   });
   app.get('/shop/:color', (req, res, next) => {
     const family = SHADE_SHOP_FAMILIES[req.params.color];
     if (family) {
       req.metaData = req.metaData || {};
       req.metaData.shadeFamilyName = family;
       req.metaData.colorSlug = req.params.color;
     }
     next();
   });
   ```
5. Pug source:
   ```pug
   if metaData.shadeFamilyName
   | {
   |   "@context": "https://schema.org",
   |   "@type": "BreadcrumbList",
   |   "itemListElement": [
   |     { "@type": "ListItem", "position": 1, "name": "Home", "item": "https://www.madison-reed.com/" },
   |     { "@type": "ListItem", "position": 2, "name": "Shop All Products", "item": "https://www.madison-reed.com/shop-all" },
   |     { "@type": "ListItem", "position": 3, "name": !{JSON.stringify(metaData.shadeFamilyName)}, "item": !{JSON.stringify('https://www.madison-reed.com/shop/' + metaData.colorSlug)} }
   |   ]
   | }
   ```
6. Save in Tophat. Verify with `node jsonld-check/index.mjs --url http://localhost:3000/shop/brown --types BreadcrumbList`.

If R2 doesn't take effect (e.g., `req.metaData` not being merged into the Pug locals for this route), fall back to PC.3 (route-handler push code).

---

## SECTION 4: FILE INDEX

### Documentation (external — roam, sessions, spreadsheets)

| File | Purpose |
|---|---|
| `~/.brain.d/roam-nodes/madison_reed/2026-04-29-121311-dotcompb_7929.org` | **Roam node** — co-tracks 7929+7945. v14 plan: Tophat-first restored, AC5 enforcement, FAQPage canonical JSON template added to TICKET CONTEXT. UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`. |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-30-aeo_roadmap_executive_summary.org` | **AEO Roadmap wrapper roam node** — UUID `0bca0fb0-73dd-439e-949c-d594d3ad8806`. |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-30-aeo_roadmap_executive_summary.csv` | AEO Roadmap CSV (JIRA attachment). |
| `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` | Roam index — IN CODE REVIEW lane. |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dotcompb-7929-json-ld-non-pdp.md` | **This session file** (you are here). |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/mr-seo-structured-data-architecture.md` | Architecture memory v3 (788 lines). |
| URL Scope spreadsheet | https://docs.google.com/spreadsheets/d/1aSSIPzu89i6ZHD2xIPmGLfZThec7klfzrGrHkJnThbc/edit |
| Tophat CMS admin (auth required) | http://localhost:4000/#/cms/content |
| Google Rich Results Test | https://search.google.com/test/rich-results |

### JSON-LD audit harness — CANONICAL home (seo-analyzer)

The harness was migrated this session from MR's `.tasks/seo-jsonld-check/` to a project-agnostic location. Both copies stay in sync via copy-paste; the seo-analyzer copy is the canonical one for cross-project reuse.

| File | Purpose |
|---|---|
| `/Volumes/dev-partition/local-projects/seo-analyzer/README.org` | **Comprehensive usage docs** (~19 KB, 12 sections). Read this for any harness usage question. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/index.mjs` | CLI entry point + reporting. Resolves --urls / --report-* against process.cwd(). |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/fetchers.mjs` | Native `fetch` + `AbortController` timeout. 2 retries with backoff on AbortError / fetch failure. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/extractors.mjs` | Regex JSON-LD extraction + JSON validity issues. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/validators.mjs` | FAQPage / BreadcrumbList shape rules. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/experiments.mjs` | `detectExperiment()` + `probeAllVariations()` via `?v=&xid=` overrides. Now takes `variationThrottle` ms between A/B/C re-fetches. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/cms-link.mjs` | Optional URI → content_id resolver via `docker exec mr-mongo mongosh`. Tophat URL builder. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/dedup.mjs` | **(NEW 2026-05-01)** Tier 1 grouping. `buildGroups(rows, cmsLinks, {enabled})` returns `{groups, rowToKey}`. Group key precedence: manual `group=<label>` → `cid:<contentId>` → `solo:<idx>:<url>`. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/csv-ingest.mjs` | **(NEW 2026-05-01)** `loadUrlsFromCsv(path, opts)` reads a CSV (Page URL column), strips host, dedupes, returns audit rows. Refuses tab-separated urls.txt files as input. Auto-adds BreadcrumbList expectation for /shop/*. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/faq-detector.mjs` | **(NEW 2026-05-01)** `detectFaqContent(html)` → `{hasFaq, signals}`. Looks for FAQ Vue components, hcb-faqs class, "Frequently Asked Questions" heading, already-emitted FAQPage JSON-LD. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/urls.txt` | URL list template (replace examples with project URLs). |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/urls.site.txt` | **(NEW 2026-05-01)** Site-wide audit list — 239 URLs from the AEO Roadmap Sheet2 CSV with curated overrides + pattern rules applied. Regenerate via `node .tasks/DOTCOMPB-7929/improve-site-urls.mjs`. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/subsets/dedup-smoke.txt` | **(NEW 2026-05-01)** 11-URL fixture covering all 4 group flavors (cid auto, manual, solo, regression sentinel). |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/reports/audit.org` + `audit.json` | Generated by `npm run jsonld-audit` (small curated `urls.txt` set). |
| `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/reports/site-audit.org` + `site-audit.json` | **Generated by site-wide audit** (239-URL `urls.site.txt`). Org report's "URLs needing a Tophat tweak" section is the actionable hot-list. JSON report includes the parsed JSON-LD per variation when `--embed-jsonld` is on (default). |
| `/Volumes/dev-partition/local-projects/seo-analyzer/package.json` | Now exposes `npm run jsonld-check`, `npm run jsonld-audit`, `npm run jsonld-verify`, `npm run audit-site` (CSV-driven). yargs pinned to `^17.7.2` for Node 18 compat. |
| `/Volumes/dev-partition/local-projects/seo-analyzer/README.org` | Updated 2026-05-01: new sections "Site-wide audit from CSV" + "Deduplication (Tier 1)"; CLI flags table covers `--csv`, `--csv-host`, `--csv-out`, `--no-dedup`, `--no-detect-faq`, `--throttle`, `--variation-throttle`, `--no-stop-on-fetch-error`, `--no-embed-jsonld`. Status taxonomy adds `NOT_A_CANDIDATE` and `SKIPPED`. |

### JSON-LD harness — MR sibling copy (kept in sync)

| File | Purpose |
|---|---|
| `/Volumes/dev-partition/github-madison-reed/the-code/.tasks/seo-jsonld-check/{*.mjs, urls.txt, reports/}` | Sibling copy of the harness. MR-specific `urls.txt` lives here (16 URLs covering v1 + v2 scope). Use this copy when running audits against the MR repo. |

### MR-specific tooling (DOTCOMPB-7929)

| File | Purpose |
|---|---|
| `/Volumes/dev-partition/github-madison-reed/the-code/.tasks/DOTCOMPB-7929/improve-site-urls.mjs` | **(NEW 2026-05-01)** One-shot transformer: reads the AEO Roadmap CSV + the curated `.tasks/seo-jsonld-check/urls.txt`, applies pattern rules (`/product/*` → `group=PDP`, `/blog/*` → `group=blog-post`, `/shop/*` → BreadcrumbList, drops booking/auth/dashboard/API), writes the improved 239-URL list to `seo-analyzer/jsonld-check/urls.site.txt`. Idempotent — re-runs from CSV every time. |
| `/Users/col-ae-052/Downloads/Madison_Reed_AEO_Roadmap - Sheet2.csv` | Source SEO export (499 URLs with traffic + bounce metadata). Drives the site-wide audit. |

### Source — MR codebase (`/Volumes/dev-partition/github-madison-reed/the-code/`)

#### Files to MODIFY (Phase B / Phase C only — no edits yet)

| File | Change | Phase |
|---|---|---|
| `website/src/routing/views.js` | Add `req.metaData.shadeFamilyName` / `colorSlug` population for `/shop/:color` (sample in §3.2). | PB.2 |
| `mr_modules/cms/lib/router.js` | Line 462: replace `content.faqPageJsonLd = …` with `pushJsonLdToContent(content, faqPageJsonLd, {type:'faq', source:'productRouter'})`. | PC.2 |
| `mr_modules/cms/lib/productJsonLd.js` | Generalize `buildFaqPageJsonLd` to `({faqs, pageUrl, about})` with backward-compat overload. | PC.1 |
| `website/src/vuescripts/utilities/productJsonLdUtils.js` | Convert `updateFaqPageJsonLdScript()` to no-op + dedup. | PC.4 |

#### Files to CREATE (Phase C only — only when needed)

| File | Purpose | Phase |
|---|---|---|
| `mr_modules/cms/lib/jsonLd/index.js` | Barrel + `pushJsonLdToContent` helper | PC.1 |
| `mr_modules/cms/lib/jsonLd/sanitize.js` + `.test.js` | Allowlist sanitizer | PC.1 |
| `mr_modules/cms/lib/jsonLd/faqPage.js` + `.test.js` | Generalized FAQPage builder | PC.1 |
| `mr_modules/cms/lib/jsonLd/breadcrumbList.js` + `.test.js` | BreadcrumbList builder | PC.1 |
| `mr_modules/cms/lib/jsonLd/extractFaqs.js` | Server-side port of `getFAQsFromQuestionsAnswersPair` | PC.1 (only if HCB residue) |
| `mr_modules/cms/lib/jsonLd/routeScoped.js` | `applyRouteScopedJsonLd` for shade shop residue | PC.3 (only if PB.2 fails) |
| `.tasks/qa-automation/DOTCOMPB-7929/specs/dotcompb-7929-jsonld.spec.ts` | Playwright spec | P5.5 |

#### Files referenced (read-only)

| File | Why |
|---|---|
| `mr_modules/cms/lib/scriptsUtils.js:30-79, 134-142` | Compiles `additionalScripts[]` into `header.scripts[]`. Silent-failure mode at lines 134-142. |
| `mr_modules/cms/lib/htmlRenderer.js:165-214, 520-588` | `renderContent` + `getLocals`. Locals shape: `content`, `settings` (= `templateData`), `metaData` (shallow merge), `params`, `query`, `parsedUrl`. |
| `mr_modules/cms/lib/loaders.js:32-99, 414-486` | Dev/live loader, variation selection (`getContentVariation`), `?v=&xid=` override hooks (basis for the harness's experiment probing). |
| `mr_modules/dataAccess/cms/CMSPackage.js:89-242` | `getCurrentContent` + `getCurrentContentForRender`. Multi-variation reality is sourced here. |
| `website/src/routing/views.js:1593-1607` | `/colorbar/locations/:urlKey` populates `req.metaData = locationSummary`. |
| `website/src/views/desktop/vue-layout-ssr.pug:24-26` | The `each scriptDefinition in header.scripts` iterator — emits all `additionalScripts[]` entries to raw HTML. |

### Tooling / external reference

| Path | Purpose |
|---|---|
| `.tasks/qa-automation/parse-acs.mjs` | Roam-node-to-Playwright parser (sister 7942 reference). |
| `.tasks/qa-automation/playwright.config.ts` | Reused as-is. |
| `.tasks/lint-changed.sh` | Local lint matching PilkoLint CI. |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-23-150000-dotcompb_7942.org` | Sister roam — Playwright pattern reference. |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.** 2026-05-18 single-day session, three phases: **v7 morning** (FAQPage scope resolved), **v8 afternoon** (BreadcrumbList shipped on cid 2686, Tophat controller parity bug found + patched in staging, COPY-PASTE checklist rebuilt), **v9 evening** (audit accuracy pass — 21 non-shade `/shop/*` + `/shopping/*` + `/shop-all` URLs reclassified to `expected: []` in the audit because they don't render visible breadcrumbs; emitting JSON-LD there would violate Google's content-match rule). End state in local dev: **the cleanest audit of the session — 52 PASS / 1 PARTIAL (cid 2903 var B, architecturally correct) / 0 MISSING / 0 duplicates**. Both DOTCOMPB-7929 (FAQPage) and DOTCOMPB-7945 (BreadcrumbList AC scope = 4 shade pages) are AC-compliant. Outstanding human-driven work: ship the staged Tophat controller patch through Release, dotcom Tophat replay (4 surfaces — see COPY-PASTE checklist in `.tasks/DOTCOMPB-7929/template-faq-mutations.org`), local + dotcom `addFaqMetadata` uncheck on cid 2350 var A.

### Final audit (post-v9 reclassification)

| Status | Count | What it represents |
|---|---:|---|
| PASS | **52** | 27 FAQPage + 4 BreadcrumbList (shade pages) + 21 reclassified-empty (`expected: []` after v9) |
| PARTIAL | 1 | cid 2903 var B — architecturally correct (no FAQ component in var B's componentList → Pug correctly emits nothing) |
| MISSING | **0** | All expected schemas emit. No outstanding ticket-scope failures. |
| NOT_A_CANDIDATE | 186 | Pages with no FAQ section, FAQPage was the default expectation but irrelevant |
| Duplicates (ACTION LIST 3) | 0 | cid 2350 var A's R1 was eliminated by `tophat-tools remove-jsonld-script.mjs --auto-gen` |
| FAQ Questions Indexed (KPI) | 117 | +16 from session start (~101) |

ACTION LIST 1 has exactly one entry — cid 2903 var B's architectural PARTIAL. No other URLs need implementation.

### Main decisions this session (with implications)

1. **(2026-05-18 v7) Partial-template-level Pug pattern adopted for FAQPage.** *Implication:* a single Pug script on a reusable partial template covers every content doc that mounts it. Reduces the FAQPage surface area from N-per-content to 1-per-partial. Validated on templates 1211 (3 docs), 1375 (3 docs). See `[session: mr-seo-structured-data-architecture > architecture-decisions > ad-007]`.

2. **(2026-05-18 v8) BreadcrumbList shipped via the SAME `additionalScripts` mechanism, not a Vue component.** Started with `ShadeShopPage.vue` augmentation but reverted — the Vue path was a one-off pattern not used anywhere else in MR. *Implication:* consistency across both tickets (FAQPage + BreadcrumbList) → one mechanism, one mental model, one mutation tool (tophat-tools), one debugging surface. The 4-shade ACs are met by a single 16-line Pug on cid 2686 var A with a slug→display-name map. The map extends to cover the other 13 `/shop/<category>` URLs trivially when product opens scope.

3. **(2026-05-18 v8) Discovered Tophat-controller parity bug — patched.** `ContentVersionCtl.js:203-210` had the `ld+json + forceInterpolation → Pug editor mode` override; `TemplateEditCtl.js` did not. Template-level Pug scripts always opened in JSON mode → Ace's JSON validator flagged the leading `if` → `disableTheForm()` disabled Save. *Implication:* without this patch, **the dotcom Tophat-UI replay path is broken** for the 2 template-level scripts (template 1211, 1375) we wrote. With the patch, future `ld+json + forceInterpolation` scripts created via Tophat UI on dotcom work natively — no DevTools hacks, no manual editor-mode flip. See `[session: mr-seo-structured-data-architecture > constraints > cl-005]`.

4. **(2026-05-18 v8) Tophat-tools helpers now write `_editorOptions` natively.** Previously the helpers omitted `_editorOptions`, causing the Tophat editor to fall through to its type-mapped default — JSON for `ld+json`. *Implication:* future writes through `add-jsonld-script.mjs` / `add-jsonld-to-template.mjs` will produce entries that open in Pug mode in the local Tophat UI without retrofit. Already-written entries were retrofitted in this session, then re-ran cleanly via patched helpers so they're written natively now.

5. **(2026-05-18 v8) Single QA-handoff doc with COPY-PASTE checklist replaces multi-source instructions.** `.tasks/DOTCOMPB-7929/template-faq-mutations.org` now opens with 4 self-contained "Surface" blocks — each carrying URL (Local + Dotcom) + Pug body + checkbox table + Verify curl in one screen. *Implication:* Carley or any human replaying in dotcom can click-paste through all 4 surfaces sequentially without context-switching to a second doc. The original detailed sections are preserved below for deep reference.

6. **(2026-05-18 v8) `code-review` enforced the project's 2-3 line comment rule on the staged Tophat fix.** *Implication:* code style is consistent with `mr-dotcom-dev` and `.claude/rules/coding-standards.md`. The PR body in the roam node carries the architectural rationale (where prose is appropriate) — the inline comment is just enough to send the next reader to the matching code in `ContentVersionCtl.js`.

7. **(2026-05-18 v8) Drafted commit message + PR description via `pr-scribe brand-madison-reed`** appended to the roam node as `* COMMIT MSG` and `* PR DESCRIPTION`. *Implication:* zero context-switching when the user is ready to commit + push — copy from the org-mode `#+begin_src` blocks. PR follows the MR Pattern A flat `**Changes:**` shape with TD-FREEFORM, QA-INSTRUCTIONS, bug-shape blockquote.

8. **(2026-05-18 v9) Audit-accuracy reclassification — 21 non-shade URLs dropped the `BreadcrumbList` expectation.** Investigation revealed all 21 have `breadcrumbs: null` in their Vuex `__INITIAL_STATE__` — no visible breadcrumb component renders. *Implication:* the audit's MISSING count goes 8 → 0 (and ACTION LIST 3 stays at 0 duplicates) because the 21 URLs no longer expect a schema they couldn't honestly emit. The 4 DOTCOMPB-7945 AC URLs (`/shop/{brown,blonde,red,black}`) keep `expected: ['BreadcrumbList']` — they're the only `/shop/*` pages that render visible breadcrumbs. The deferred 21 URLs are documented in `template-faq-mutations.org → 🚧 Out-of-scope follow-ups` with the 2-phase path to enable them (Vue component dispatches `setBreadcrumbs(...)` first, then the families map gets extended). Two source edits: `improve-site-urls.mjs` keys `/shop/<color>` against a `SHADE_SHOP_FAMILIES` set; 8 v2-followup lines in `.tasks/seo-jsonld-check/urls.txt` commented out to stop curated-overlay overrides. The audit is now an *honest* signal of what's intentionally schema-emitting vs. what's intentionally deferred.

### What was done last (full inventory through 2026-05-18 v8)

**Architectural breakthrough — partial-template-level JSON-LD pattern proven.** See `[session: mr-seo-structured-data-architecture > design-patterns > dp-006]` + `[session: mr-seo-structured-data-architecture > architecture-decisions > ad-007]`. Single Pug script on a partial's `templateVersion.additionalScripts[]` covers every content doc that mounts the partial — zero per-content edits. Replaces the v14 per-content R2 plan for reusable partials.

**Three mutations applied in local dev Mongo (backups in `the-code/cms-backups/jsonld/`):**

| # | Target | Operation | Source/Tool |
|---|---|---|---|
| 1 | content_id 2350 var A | Removed R1 auto-gen FAQPage entry (`generatedAutomatically:true`, `metadata.type:'faq'`). Mirrored removal to `production_content` + `stage_content` variations.desktop[A]. | tophat-tools `remove-jsonld-script.mjs 2350 --variation A --auto-gen --confirm` (NEW helper) |
| 2 | templateVersion 4708 (template_id 1211, `video-chat-faqs`) | Added partial-level FAQPage Pug to `templateVersion.additionalScripts[]`. | tophat-tools `add-jsonld-to-template.mjs 1211 --src .tasks/DOTCOMPB-7929/faqpage-partial.pug --header --force-interpolation --confirm` (NEW helper) |
| 3 | templateVersion 5049 (template_id 1375, `faqs-with-icons-pro`) | Same partial-level Pug. | Same NEW helper |

**Tophat-tools helpers — NEW/PATCHED in this session (live in `~/.claude/skills/tophat-tools/scripts/`):**
- `add-jsonld-script.mjs` — **PATCHED**: was writing `body` field but `scriptsUtils.js:53` reads `text`. Anything pushed via this helper silently never rendered. Fixed 2026-05-18.
- `add-jsonld-to-template.mjs` — **NEW**: template-level companion. Targets `templateVersion.additionalScripts[]` (direct, NOT under `renderOptions` — schema differs from content). Standard dry-run + `--confirm` + backup conventions.
- `remove-jsonld-script.mjs` — **NEW**: general-purpose removal. Filters: `--auto-gen` (matches R1 entries), `--index N`, `--where field=value` (dotted paths). Mirrors writes to `production_content` + `stage_content` (override with `--no-mirror`).
- See `[session: mr-seo-structured-data-architecture > reusable-references > rr-006]` for the canonical workflow.

**Audit harness refinements** (`/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/index.mjs`):
- Per-leader TUI now multi-line: `[N/total] cid=<id> → <Tophat URL>` first, then leader URL + status + FAQ signals. Tophat URL printed BEFORE the probe so user can click while audit runs.
- Three top-of-report ACTION LISTs (in addition to existing detail sections):
  - *ACTION LIST 1 — URLs Needing FAQPage Implementation* (FAQ content + MISSING/PARTIAL/INVALID FAQPage)
  - *ACTION LIST 2 — URLs Already Emitting FAQPage* (PASS, for manual sanity check)
  - *ACTION LIST 3 — URLs with Duplicate FAQPage Objects* (AC5 violation detection)
- All Tophat URLs in the org report rendered as **bare clickable links** (was `[[url][open in Tophat]]` — hid the URL). Applies to action lists, Per-URL detail, and Members covered sub-lists.
- Members-covered block enriched with inherited verdict + FAQ-section indicator + FAQPage-emitted indicator.

**Post-mutation audit re-run (2026-05-18 ~13:00) — verification:**

| Status | Count | Change vs previous audit | Notes |
|---|---:|---|---|
| PASS | 27 | +24 | cid 2350 (21 slug URLs) + cid 2536 + cid 2936 + cid 2992 newly passing |
| PARTIAL | 1 | −20 | Only cid 2903 — var A ✓ (4 questions), var B ✗ (no FAQ component in var B's componentList — architecturally correct) |
| MISSING | 25 | −2 | **ALL 25 are BreadcrumbList expectations, ZERO are FAQPage** — DOTCOMPB-7945 scope, not this story |
| NOT_A_CANDIDATE | 186 | −2 | Pages with no FAQ section, correctly skipped |
| Duplicates (ACTION LIST 3) | 0 | −21 | cid 2350 var A duplicate eliminated |
| FAQ Questions Indexed (KPI) | 117 | +16 | Approaching AC11 target (200+) |

**Bonus URLs (not tracked in urls.site.txt but verified PASS via single-URL audit):**
- `/schedule-video-chat-application` (cid 2568) — covered by template 1211
- `/help/contact-us` (cid 3176) — covered by template 1211

**QA replay documentation written:** `.tasks/DOTCOMPB-7929/template-faq-mutations.org` — per-template Tophat URLs (local + dotcom prod side-by-side), Pug source for copy-paste, checkbox-state cheat-sheet, manual UI follow-up for cid 2350, verification plan, backups index. **This is the QA-handoff document.**

### Outstanding work (in priority order)

1. **Ship the staged Tophat controller fix (gateway for everything else).** `tophat/src/ngscripts/cms/TemplateEditCtl.js` is staged with the 3-line `ld+json + forceInterpolation → Pug` override mirroring `ContentVersionCtl.js:203-210`. Roam node carries the COMMIT MSG and PR DESCRIPTION sections drafted per MR brand (`pr-scribe brand-madison-reed`). User commits + opens PR + ships through Release. Once merged to dotcom Tophat, item 2 below works through the UI without DevTools hacks.

2. **Dotcom production replay — 4 Tophat surfaces.** Step-by-step COPY-PASTE checklist at the top of `.tasks/DOTCOMPB-7929/template-faq-mutations.org`. Each surface has Local + Dotcom URLs, Pug body inline, checkbox table, and Verify curl:
   - **① Template 1211** (`video-chat-faqs`) — paste FAQPage Pug → covers `/schedule-video-chat`, `/schedule-video-chat-application`, `/help/contact-us` (3 docs).
   - **② Template 1375** (`faqs-with-icons-pro`) — same FAQPage Pug → covers `/haircolorbar/memberships/limitless-pro-plus`, `/haircolorbar/memberships`, `/home-memberships` (3 docs).
   - **③ Content 2686 var A** (`/shop/`) — paste BreadcrumbList Pug → covers `/shop/{brown,blonde,red,black}` (DOTCOMPB-7945 AC scope).
   - **④ Content 2350 var A** (`/colorbar/locations/`) — cleanup ONLY: (a) delete the static FAQPage entry (`generatedAutomatically: true`, "What is a Hair Color Bar?"), (b) uncheck `addFaqMetadata` on `componentList[0]` (`hcb-location-page`). Per `[session: mr-seo-structured-data-architecture > constraints > cl-004]` — without (b), Tophat regenerates R1 on next save.

3. **Local-dev `addFaqMetadata` uncheck on cid 2350 var A** — same as step ④ above, but in `http://localhost:4000/#/cms/content/edit/2350`. Local DB write already removed R1; this uncheck prevents the next local Tophat save from regenerating it.

4. **DOTCOMPB-7945 scope decision — DONE in v9.** The 21 non-AC URLs (13 non-shade `/shop/<category>`, 7 `/shopping/*`, 1 `/shop-all`) are reclassified to `expected: []` in the audit because none render visible breadcrumbs. They show as PASS by `expected.length === 0` (vacuous truth in `classifyUrl`'s `allOk` check). Documented as out-of-scope follow-ups in `template-faq-mutations.org → 🚧 Out-of-scope follow-ups`. The 2-phase path to enable any of them is captured there (Vue component dispatches `setBreadcrumbs(...)` first, then JSON-LD). Promoting any slug into BreadcrumbList scope requires a real product/design decision on the visible breadcrumb shape.

5. **Harness content-validator (Phase 4 — planned, not started):** new module `content-validator.mjs` that extracts FAQ Q&A from rendered HTML's FAQ-component markup, normalizes both sides (strip HTML, collapse whitespace, lowercase), compares to FAQPage JSON-LD `mainEntity[]` entities, flags per-pair `match`/`mismatch-question`/`mismatch-answer`/`only-in-jsonld`/`only-on-page`. Adds a 4th ACTION LIST to the report. CLI flag `--validate-content` (default on).

6. **Sign-off (not in this pass):** P5.5 Playwright spec, P6.1–P6.5 lint/test/Rich Results Test on QA, P7.1 baseline capture post-deploy.

### Where things live (canonical paths)

**SEO/JSON-LD (DOTCOMPB-7929):**
- Audit harness: `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/`
- Harness command: `cd /Volumes/dev-partition/local-projects/seo-analyzer && node jsonld-check/index.mjs --urls jsonld-check/urls.site.txt --report-org jsonld-check/reports/site-audit.org --report-json jsonld-check/reports/site-audit.json`
- Latest audit report: `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/reports/site-audit.{org,json}`
- urls.site.txt regenerator: `node /Volumes/dev-partition/github-madison-reed/the-code/.tasks/DOTCOMPB-7929/improve-site-urls.mjs`
- **QA replay doc**: `.tasks/DOTCOMPB-7929/template-faq-mutations.org` (template URLs, Pug body, verification steps)
- Pug source: `.tasks/DOTCOMPB-7929/faqpage-partial.pug` (partial-level — single canonical script used for templates 1211 + 1375)
- DB backups: `the-code/cms-backups/jsonld/2350/`, `the-code/cms-backups/jsonld/template-1211/`, `the-code/cms-backups/jsonld/template-1375/` — restore via tophat-tools `restore-content.mjs` or hand-replay
- Roam node: `~/.brain.d/roam-nodes/madison_reed/2026-04-29-121311-dotcompb_7929.org` (UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`) — needs update to reflect partial-template approach
- Branch: `DOTCOMPB-7929` (local — no commits yet; all work has been DB-side + harness-side)

**Tophat-tools helpers (skill source — dot-files repo):**
- `~/.claude/skills/tophat-tools/scripts/` symlinked from `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/skills/tophat-tools/scripts/`
- Three relevant helpers: `add-jsonld-script.mjs` (patched), `add-jsonld-to-template.mjs` (new), `remove-jsonld-script.mjs` (new)
- Architecture memory entry: `[session: mr-seo-structured-data-architecture > reusable-references > rr-006]`

### QA Verification Plan (post-replay)

After dotcom Tophat replay lands (item 1 above), QA verification:

**Step 1 — Restart dev SSR locally** (needed because dev caches per-handler):
```sh
# In dev server terminal, kill + restart Vite/SSR
curl -s -o /dev/null -w "HTTP %{http_code} in %{time_total}s\n" http://localhost:3000/
```

**Step 2 — Re-run site-wide audit:**
```sh
cd /Volumes/dev-partition/local-projects/seo-analyzer
node jsonld-check/index.mjs --urls jsonld-check/urls.site.txt --report-org jsonld-check/reports/site-audit.org --report-json jsonld-check/reports/site-audit.json
```

**Step 3 — Spot-check raw HTML for the 3 mutation targets:**
```sh
node jsonld-check/index.mjs --url http://localhost:3000/colorbar/locations/hillsboro --show-raw  # single FAQPage block
node jsonld-check/index.mjs --url http://localhost:3000/schedule-video-chat --show-raw           # FAQPage from template 1211
node jsonld-check/index.mjs --url http://localhost:3000/haircolorbar/memberships/limitless-pro-plus --show-raw  # FAQPage var A from template 1375; var B no FAQPage (expected)
```

**Expected counts after replay:**
- ACTION LIST 1 (FAQPage needs): 1 entry (cid 2903 — var B architectural case) OR 0 if reclassified
- ACTION LIST 2 (FAQPage PASS): 7 templates, 27 URLs (or more if BreadcrumbList action also lands)
- ACTION LIST 3 (duplicates): 0
- MISSING: 25 BreadcrumbList (will drop to ~0 after DOTCOMPB-7945 ships)

**Step 4 — Google Rich Results Test** (pre-merge sign-off, future):
- Run https://search.google.com/test/rich-results on QA URLs (`https://dotcom.mdsnrd.com/colorbar/locations/hillsboro`, `https://dotcom.mdsnrd.com/schedule-video-chat`, etc.) — expect zero errors, zero warnings.

### Where to resume

**If the user says "continue DOTCOMPB-7945" / "let's do the BreadcrumbList":** they've already started this thread. We've identified 17 URLs on cid 2686 (`takesUrlParameters: true`) need a single content-level Pug edit. The Pug needs a slug→display-name map. User wants to investigate existing BreadcrumbList implementations on the site first to pick the most accurate pattern — that's the active next step (paused mid-investigation when this reset was triggered).

**If the user says "replay in dotcom" / "ship to production":** open `.tasks/DOTCOMPB-7929/template-faq-mutations.org` — it has the exact UI steps + URLs + Pug body to paste. Three Tophat surfaces to touch (templates 1211, 1375, content 2350 var A).

**If the user says "re-run the audit":** verify dev SSR is up, then the command in step 2 above. ~8-10 min for 50 leader audits with throttle.

**If the user says "the audit shows something strange":** see `[session: mr-seo-structured-data-architecture > constraints > cl-004]` — duplicates may resurface if anyone has saved cid 2350 var A in Tophat after our DB write. Also check that the MISSING count is BreadcrumbList-only (DOTCOMPB-7945 scope) and not FAQPage regressions.

**If the user says "add the content validator":** start by reading the FAQ component HTML markup for each FAQ-bearing partial — `faqs-with-icons-pro` uses `.faqs-with-icons` > `.question-answer` > `.question-text` + `.answer p`; `hcb-location-page-v2` uses similar pattern with different classes; `video-chat-faqs` similar. Build one extractor per known component family, normalize text, compare to parsed JSON-LD mainEntity[].

**If the user asks for harness reference:** `/Volumes/dev-partition/local-projects/seo-analyzer/README.org` — sections "Site-wide audit from CSV", "Deduplication (Tier 1)", "CLI flags".

**If the user asks for plan / mechanism deep-dive:** SEO roam node — anchors `#ticket-context`, `#json-ld-mechanism-reference`, `#audit-report`, `#ticket-tasks`. Architecture memory has the canonical patterns (dp-002 R1 auto-gen, dp-003 Pug-interpolated, dp-006 partial-template-level, ad-005 unified pipeline, ad-006 push helper, ad-007 prefer-template-level, cl-004 addFaqMetadata-regenerates).

**If the user asks for status snapshot:** 2026-05-18 audit shows FAQPage scope **100% resolved in local dev** — 27 PASS, 1 architecturally-correct PARTIAL (cid 2903 var B), 0 duplicates. 25 BreadcrumbList still MISSING (DOTCOMPB-7945, separate ticket). Dotcom production replay PENDING — DB writes are local-only per the Tophat-first discipline.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table. Newest first. See `~/.claude/skills/session-reset/rules/activity-log.md`.

| Datetime         | Duration | Type                  | Reference        | Description |
|------------------|----------|-----------------------|------------------|-------------|
| 2026-05-18 18:30 | 0.5h     | session-reset         | this             | v9 reset. End-state audit captured: 52 PASS / 1 architecturally-correct PARTIAL / 0 MISSING / 0 duplicates / KPI 117. Reclassified 21 no-breadcrumb URLs in audit list (improve-site-urls.mjs SHADE_SHOP_FAMILIES set; seo-jsonld-check/urls.txt v2 BreadcrumbList block commented out). Audit now an honest signal: only URLs with a visible breadcrumb backing the schema get a `BreadcrumbList` expectation. Deferred 21 URLs documented in `template-faq-mutations.org → 🚧 Out-of-scope follow-ups` with the 2-phase path (visible breadcrumb UI first, then JSON-LD). Both ticket-scope FAQPage + BreadcrumbList ACs met locally; remaining outstanding work is human-driven (ship the staged Tophat patch + dotcom replay + cid 2350 `addFaqMetadata` uncheck). |
| 2026-05-18 18:15 | 0.5h     | refinement            | DOTCOMPB-7945    | Reclassification of 21 non-shade BreadcrumbList URLs. Investigation: curl'd each of the 13 non-shade `/shop/<category>` URLs + 7 `/shopping/*` + `/shop-all` — `breadcrumbs: null` in the Vuex __INITIAL_STATE__ on every one, no visible breadcrumb component renders. Per Google's content-match rule, emitting BreadcrumbList JSON-LD without a visible breadcrumb is deceptive markup. Two source edits: (a) `patternFor` in `.tasks/DOTCOMPB-7929/improve-site-urls.mjs` now keys `/shop/<color>` against a `SHADE_SHOP_FAMILIES = {brown, blonde, red, black}` set — only those 4 get `expected: ['BreadcrumbList']`; everything else `/shop/*` + `/shopping/*` + `/shop-all` gets `expected: []` with phase `P3.9-no-breadcrumb`. (b) `.tasks/seo-jsonld-check/urls.txt` v2-followup block (8 hand-curated BreadcrumbList overrides) commented out to stop the curated overlay re-overriding the pattern; comment header preserves the deferred scope as documentation. Regenerated urls.site.txt: 239 rows, `P3.9-no-breadcrumb = 21`. New `🚧 Out-of-scope follow-ups` section added to `template-faq-mutations.org` (between surface ④ and ⏱ Order matters?). |
| 2026-05-18 18:00 | 0.3h     | research              | DOTCOMPB-7945    | Per-URL audit-accuracy investigation: confirmed dedup artifact (13 non-shade `/shop/<category>` URLs showed PASS via Tier 1 dedup inheritance from `/shop/brown` leader despite individually emitting nothing — the families map only covers 4 slugs and the `if familyName` guard blocks non-matches). Confirmed 8 actual MISSING + 13 dedup-hidden are the same architectural class: no visible breadcrumb on the page. Confirmed for `/shop/all-hair-color` via curl + `__INITIAL_STATE__` parse: `breadcrumbs: null`, no `<nav breadcrumb>` markup, no `itemtype="https://schema.org/BreadcrumbList"` markup. Same pattern verified across other categories + `/shopping/*` + `/shop-all`. |
| 2026-05-18 17:30 | 0.5h     | session-reset         | this             | v8 reset. Late-afternoon continuation of v7 session. Captures: (1) BreadcrumbList shipped via `additionalScripts` Pug on cid 2686 (matched FAQPage mechanism instead of separate Vue-component approach — reverted that mid-session); (2) Tophat controller divergence found and fixed — `TemplateEditCtl.js` patched to mirror `ContentVersionCtl.js:203-210`'s `ld+json + forceInterpolation → Pug` override (eliminates "Unexpected 'i'" save-blocker on template-level Pug scripts); (3) tophat-tools helpers patched to write `_editorOptions` natively + 4 entries retrofitted + 3 ADD mutations rerun; (4) QA-handoff doc rebuilt with COPY-PASTE REPLAY CHECKLIST (4 self-contained surface blocks at top of file); (5) code-review enforced project's 2-3 line comment rule on staged fix; (6) DOTCOMPB-7945 ticket pulled (4-URL AC scope confirmed); (7) roam node now carries `* COMMIT MSG` and `* PR DESCRIPTION` per MR brand for the staged Tophat fix. Architecture memory v4 → v5 (added cl-005 Tophat-controller-parity, rr-007 editor-mode-script-descriptor). |
| 2026-05-18 17:30 | —        | architecture-extract  | this             | Architecture memory v4 → v5. Added: cl-005 (Tophat editor-mode parity bug — TemplateEditCtl.js lacks the ld+json+forceInterpolation→Pug override that ContentVersionCtl.js has; patch staged under DOTCOMPB-7929), rr-007 (editor-mode-script-descriptor pattern — _editorOptions.mode preserves across opens via the !_editorOptions filter check; tophat-tools helpers now write it natively). |
| 2026-05-18 17:15 | 0.5h     | documentation         | DOTCOMPB-7929    | Roam node `2026-04-29-121311-dotcompb_7929.org` updated with `* COMMIT MSG` and `* PR DESCRIPTION` sections drafted via `pr-scribe brand-madison-reed`. Commit: conventional-commit fix: title + 5-line body referencing ContentVersionCtl.js:203-210. PR: MR Pattern A flat **Changes:** list, bug-shape blockquote (SUMMARY/IMPACT), TD-FREEFORM Technical Details paragraph explaining Ace lookup + parity restoration, QA-INSTRUCTIONS with 5 steps (templates 1211/1375 + forceInterpolation toggle + ContentVersionCtl sanity check). Labels reminder: DOTCOM TEAM, Pending QA Review. Sections inserted before `* COMMENTs` at file end. |
| 2026-05-18 17:00 | 0.25h    | code-review           | DOTCOMPB-7929    | `/code-review` pass on the staged `TemplateEditCtl.js` fix. 1 finding (LOW, rule-u-cs-002 + project coding-standards.md): 4-line comment block exceeded project's 2-3 line max for *why* comments. Condensed to 2 lines: `// ld+json + forceInterpolation carries Pug source — use Pug editor mode so the JSON / // validator doesn't block saves. Mirrors ContentVersionCtl.js:203-210.` Kept the cross-reference, dropped redundant detail. |
| 2026-05-18 16:45 | 1h       | bug-fix               | DOTCOMPB-7929    | Patched `tophat/src/ngscripts/cms/TemplateEditCtl.js` to mirror `ContentVersionCtl.js:203-210`'s `ld+json + forceInterpolation → $scope.editorOpts.pug` override inside `setEditorOnScriptDescriptor`. Also captured `originalEditor` once and reused it inside the `onLoad` callback (instead of re-deriving from `$scope.editorOpts[scriptDescriptor.type]`). Bug history: TemplateEditCtl.js was missing the parity override; template-level Pug-JSON-LD scripts opened in JSON mode → Ace JSON validator flagged the leading `if` → `_editorHasErrors = true` → `disableTheForm` blocked Save. Without this fix, dotcom Tophat UI replay is impossible. |
| 2026-05-18 16:30 | 0.5h     | research              | tophat-cms       | Traced the "Unexpected 'i'" complaint through Tophat controllers: confirmed `_editorOptions` field controls Ace mode and persists across opens via `!scriptDescriptor._editorOptions` filter checks in load paths; identified the divergence between `ContentVersionCtl.js` (had ld+json+forceInterp override at lines 203-210) and `TemplateEditCtl.js` (didn't). Also clarified that the script `type` dropdown (Ld-Json/Javascript/CSS/Image/Font) is unrelated to editor mode — there's no "Pug" type option and there shouldn't be. |
| 2026-05-18 16:15 | 0.5h     | implementation        | tophat-tools     | Patched both helpers in `~/.claude/skills/tophat-tools/scripts/` to write `_editorOptions: { mode: <forceInterp ? 'pug' : 'json'>, theme: 'monokai', maxLines: 300 }` + `_editorHasErrors: false` on every new entry. Mirrors what the Tophat UI persists. Retrofitted 4 pre-patch entries (template 1211, template 1375, cid 2686 var A, cid 2350 var A index 1) + their denormalized copies in production_content + stage_content. Re-ran all 3 ADD mutations via patched helpers for clean native writes (new backups in cms-backups/jsonld/). |
| 2026-05-18 15:30 | 0.5h     | implementation        | DOTCOMPB-7945    | BreadcrumbList implementation completed via `additionalScripts` Pug on cid 2686 var A. Wrote `.tasks/DOTCOMPB-7929/breadcrumblist-shade-shop.pug` — 16-line Pug with families map (brown/blonde/red/black) + emit guard via `if familyName`. Applied via `add-jsonld-script.mjs --confirm` + mirrored to production_content + stage_content. Reverted the earlier Vue-component approach in `ShadeShopPage.vue` for consistency with the FAQPage mechanism (single source of truth, single mutation tool, single debugging surface). The Vue revert: removed `<component :is="'script'">` block from template + dropped `breadcrumbJsonLd` computed property. |
| 2026-05-18 15:15 | 0.3h     | research              | DOTCOMPB-7945    | Atlassian MCP fetch of DOTCOMPB-7945. Ticket scope: 4 URLs (`/shop/{brown,blonde,red,black}`), `itemListElement` triad (Home / Shop All Products / `<Color> Shades`). Status: Tareas por hacer (moved back from In Test on 2026-05-13 — Bre retested, no fix had landed, no PR found). Reporter Debrena McEwen. Sprint D467 Indiana Jones. |
| 2026-05-18 15:00 | 0.5h     | research              | DOTCOMPB-7945    | BreadcrumbList implementation investigation: traced `/shop/brown` rendering to `ShadeShopPage.vue` (Vue Router-mounted via cid 2686's template 1327). Component dispatches breadcrumbs to `global/setBreadcrumbs` Vuex mutation in `created()`; `Breadcrumbs.vue` emits Schema.org microdata (not JSON-LD) via `<ol itemscope itemtype="https://schema.org/BreadcrumbList">`; mounted from `SsrApp.vue` with `v-if="breadcrumbs && isAppMounted"` — `isAppMounted` is false during SSR → visible breadcrumb is client-only post-hydration. Confirmed zero existing BreadcrumbList JSON-LD anywhere on the site (contentVersion + templateVersion + production_content + stage_content all empty). |
| 2026-05-18 14:30 | 0.5h     | documentation         | DOTCOMPB-7929    | Rebuilt the top of `.tasks/DOTCOMPB-7929/template-faq-mutations.org` with a `⚡ COPY-PASTE REPLAY CHECKLIST` section: 4 self-contained per-surface blocks (Tophat URL Local + Dotcom side-by-side, Pug body inline, checkbox table, Verify curl one-liner). Pug A (FAQPage) duplicated across surfaces ① and ② for click-paste speed. Surface ④ (cid 2350 cleanup) carries the sub-step table for entry deletion + addFaqMetadata uncheck. Editor-mode gotcha note added to header quote. Order-matters? + production-env-replacement notes at the bottom. Detail-reference content preserved below the checklist. |
| 2026-05-18 16:00 | —        | architecture-extract  | this             | Architecture memory v3 → v4 (788 → ~1100 lines). Added: dp-006 (partial-template-level JSON-LD via `templateVersion.additionalScripts[]`, reads `settings.faqs[]` directly), ad-007 (prefer-template-level for reusable partials — 1 config covers N content docs), cl-004 (addFaqMetadata flag regenerates R1 on every Tophat save — DB removal is reversible), rr-006 (tophat-tools jsonld helpers: add-jsonld-script, add-jsonld-to-template, remove-jsonld-script with dry-run + backup + mirror conventions). |
| 2026-05-18 15:00 | 0.5h     | research              | DOTCOMPB-7945    | BreadcrumbList investigation: cid 2686 (/shop/) has `takesUrlParameters: true`, covers 17 /shop/<color> URLs which have visible breadcrumb in raw HTML (from Vuex siteNav.js + Shop.vue). The 8 /shopping/* + /shop-all URLs are NOT in CMS (Express routes / catch-all) and have no visible breadcrumb — should be reclassified as NOT_A_CANDIDATE rather than implementing schema for non-existent breadcrumbs. User asked to first check existing breadcrumb implementations on other pages before picking the approach. |
| 2026-05-18 13:30 | 0.3h     | documentation         | DOTCOMPB-7929    | Audit accuracy cross-check: the 25 MISSING URLs are 100% BreadcrumbList expectations (not FAQPage); the 1 PARTIAL is cid 2903 var B (architectural — var B's componentList has no FAQ component, so Pug correctly emits nothing). All FAQPage targets resolved. Net delta vs prior audit: +24 PASS, −20 PARTIAL, −2 MISSING, −2 NOT_A_CANDIDATE, −21 duplicates, +16 FAQ Questions Indexed (KPI now 117). |
| 2026-05-18 13:00 | 0.3h     | documentation         | DOTCOMPB-7929    | `.tasks/DOTCOMPB-7929/template-faq-mutations.org` written — QA-handoff doc with per-template Tophat URLs (local + dotcom side-by-side), Pug body for copy-paste, checkbox-state cheat-sheet, cid 2350 manual UI follow-up (uncheck addFaqMetadata), verification plan (restart SSR + re-run audit + spot-check URLs), backups index. Tophat URL rendering refined to bare clickable links (was `[[url][text]]` hiding URL). |
| 2026-05-18 12:30 | 0.5h     | implementation        | DOTCOMPB-7929    | Three mutations applied via tophat-tools `--confirm`: (1) `remove-jsonld-script.mjs 2350 --variation A --auto-gen` removed R1 entry (3 → 2 scripts) + mirrored to production_content + stage_content. (2) `add-jsonld-to-template.mjs 1211 --src faqpage-partial.pug --header --force-interpolation` added partial-level Pug to template 1211 (video-chat-faqs). (3) Same for template 1375 (faqs-with-icons-pro). Backups in `the-code/cms-backups/jsonld/`. Post-mutation audit: 27 PASS, 1 PARTIAL, 0 duplicates. |
| 2026-05-18 12:00 | 0.5h     | implementation        | DOTCOMPB-7929    | Wrote `.tasks/DOTCOMPB-7929/faqpage-partial.pug` — canonical 19-line partial-level FAQPage Pug. Reads `settings.faqs[]` directly (no componentList.find — at partial render context, settings IS the FAQ component's settings). Reusable across all FAQ-bearing partial templates. |
| 2026-05-18 11:30 | 1.5h     | implementation        | tophat-tools     | Three helpers created/patched in `~/.claude/skills/tophat-tools/scripts/`: (1) `add-jsonld-script.mjs` PATCHED: was writing `body` field but scriptsUtils.js:53 reads `text` — silent render failure. Bug introduced when helper was first written; never end-to-end tested. (2) `add-jsonld-to-template.mjs` NEW: template-level companion. Writes to `templateVersion.additionalScripts[]` (direct, not under renderOptions). (3) `remove-jsonld-script.mjs` NEW: general remove with `--auto-gen`, `--index N`, `--where field=value` filters; mirrors to production_content + stage_content (override with `--no-mirror`). Standard dry-run + `--confirm` + backup conventions. |
| 2026-05-18 10:30 | 1h       | research              | DOTCOMPB-7929    | Partial-template pattern discovered: `htmlRenderer.js:208` calls `addScriptsDefsInto({originalObject: template, ...})` AFTER content's renderOptions, so template-level scripts flow into the parent's `<head>` via the same pipeline. `templateVersion.additionalScripts[]` is direct (NOT under renderOptions — different from content schema). 4 FAQ-bearing partials inventoried: how-to-question-accordion-vue (137 bindings/10 docs), hcb-location-page (7/1), hcb-location-page-v2 (3/1), video-chat-faqs (47/3), faqs-with-icons-pro (43/3). Strategic shift: instead of v14's per-content R2 Pug, configure partial templates once → covers all content docs that mount them. Plan revised. |
| 2026-05-18 09:30 | 1h       | research              | DOTCOMPB-7929    | tophat-tools deep-investigation of cid 2350, 2536, 2903 current state. Findings: cid 2350 var A has R1 auto-gen FAQPage (`generatedAutomatically:true`, hardcoded "What is a Hair Color Bar?") PLUS R2 Pug — confirmed duplicate source. var A `componentList[0]=hcb-location-page` has `addFaqMetadata:true` triggering R1 regeneration on every save. cid 2536 has video-chat-faqs at componentList index 7 (var A) / 1 (var B) with editor-authored faqs[]. cid 2903 var A has faqs-with-icons-pro at index 3 (CMS partial template_id 1375) with editor-authored faqs[]; var B has NO faq-bearing component (architectural). |
| 2026-05-18 09:00 | 0.3h     | research              | DOTCOMPB-7929    | Site-wide audit re-run completed (10 min, 50 leader audits). Action list breakdown reveals scope: 3 templates need FAQPage (cid 2350 PARTIAL/dup, cid 2536 MISSING, cid 2903 MISSING) — these are this session's targets. 25 BreadcrumbList MISSING flagged for DOTCOMPB-7945 (separate ticket). Currently-passing FAQPage emitters use R1 auto-gen on content level (cid 2349, 2081, 2083). |
| 2026-05-18 08:00 | 1h       | implementation        | seo-analyzer     | Audit harness refinements: progressive per-leader TUI showing `[N/total] cid=<id> → <Tophat URL>` BEFORE the probe (so user can click while audit runs), then leader URL + post-probe status + FAQ signals. Three top-of-report ACTION LIST sections added: needs-implementation, already-emitting, duplicates. Bare Tophat URLs throughout report (org-mode auto-linked, viewer-agnostic). Per-member detail enriched with inherited verdict + FAQ-section + FAQPage-emitted indicators. |
| 2026-05-04 17:30 | 0.3h     | refinement            | DOTCOMPB-8174    | PR DESCRIPTION section in hotfix roam node updated to reflect SignInMixin.js consistency fix: new Changes bullet + "FedCM mediation consistency across all sign-in surfaces" Technical Details subsection (with before/after table showing the flag now landing on both surfaces). |
| 2026-05-04 17:15 | 0.5h     | bug-fix               | DOTCOMPB-8174    | Sentry AI race-condition response: added `use_fedcm_for_button: true` to `SignInMixin.js`'s `gai.initialize()` so every Google sign-in entry point on the site (booking flow, sign-in modal, sign-up, login/signup combined) opts into FedCM mediation regardless of mount order. One-line change. Reply posted at PR #20716#discussion_r3175405177 (reply id 3175416600). |
| 2026-05-04 16:45 | 0.5h     | code-review           | DOTCOMPB-8174    | /code-review pass on the 4 modified + 1 deleted hotfix files. 2 findings: (1) MEDIUM — imports interleaved with const declaration in `useGoogleSignIn.test.js` (moved `ERROR_MESSAGES` below all imports); (2) LOW — stale "overlay button" wording in test description (renamed to "Google button"). Both implemented. Lint 0 errors, tests 60/60 passing. |
| 2026-05-04 16:00 | 1h       | documentation         | DOTCOMPB-8174    | Roam node refined to final state: WHY CUSTOM BUTTONs CANNOT WORK (FedCM/user-trusted-activation explanation), APPROACHEs TRIED (5 subsections — 1 per failed approach with what-it-did + blocker), FINAL APPROACH (drop custom MrBtn + use `gai.renderButton()`), COMMIT MSG, PR DESCRIPTION (full MR brand-formatted PR body). Deleted obsolete sections (FEDCM REQUIREMENTs AUDIT, WHAT CANNOT BE SOLVED, PLAN H — CLICK-PROXY, HISTORICAL Phase 2 OAUTH CODE). JIRA description rewritten to be issue-only (no solution prescription). |
| 2026-05-04 15:00 | 0.3h     | bug-fix               | DOTCOMPB-8174    | Mobile full-width regression: container had `v-show="googleReady"` so `offsetWidth` was 0 at init time, falling back to `GSI_MAX_BUTTON_WIDTH = 400` and overflowing mobile viewports. Fixed by always rendering `.google-btn-container` (skeleton uses `v-if`, container is permanent in DOM). Container CSS: `margin 0 auto; max-width 320px (mobile) / 400px (mq-tablet-plus); text-align center`. |
| 2026-05-04 14:30 | 0.5h     | refinement            | DOTCOMPB-8174    | Composable cleaned to minimal-touch: dropped Sentry import, MAPPED_AUTH_ERRORS, categorizeAuthError extraction + export, ERROR_MESSAGES export, redundant `type: 'standard'`. Restored inline if/else error mapping (master pattern). Final composable diff vs master is exactly the FedCM flip + width cap + dead-code removal (no decoration). |
| 2026-05-04 13:00 | 1h       | implementation        | DOTCOMPB-8174    | FINAL APPROACH lands: drop custom MrBtn for Google sign-in. SignInOptions.vue replaces `MrBtn` with `.google-btn-container` (always-rendered `gai.renderButton()` target). Composable simplified — `triggerGoogleSignIn` removed, render-button-only. Delete orphan `google-g.svg` (only consumed by removed MrBtn). Tests reduced 67 → 60 (dropped overlay/proxy + categorizeAuthError describe blocks). Lint 0 errors. |
| 2026-05-02 (multi-hour iteration) | ~6h | refinement+implementation | DOTCOMPB-8174 | Iteration log: Plan v1 (FedCM lifecycle wiring) → Plan v2 (OAuth Code popup) → Plan v3 (OAuth Code redirect) → Plan H (click-proxy on hidden Google button) → Plan O (invisible overlay) → FINAL (default rendered button). Each approach implemented + tested before moving on. Failure modes documented in roam node APPROACHEs TRIED. v3 backend code (`googleAuth.js`, `googleLoginCode` controller + webservice + config + service) added then fully reverted when redirect mode hit `redirect_uri_mismatch` 400 in dev. |
| 2026-05-01 evening | 0.3h    | documentation         | DOTCOMPB-8174    | JIRA ticket DOTCOMPB-8174 created via Atlassian MCP — sprint D465 Home Alone 04/29, status En curso, linked as child of parent feature DOTCOMPB-7942. Initial roam node also created (UUID `0a4fa07b-15ab-4287-b448-3cb63e9498f9`) with bug-template structure. |
| 2026-05-01 12:55 | 0.5h     | session-reset         | this             | v5 reset. Documents harness refinement pass: Tier 1 dedup, CSV ingest, FAQ-content detector + NOT_A_CANDIDATE, fail-fast halt + dev-SSR pacing defaults, JSON-LD blocks embedded in reports, cwd path fix, urls.site.txt improver. |
| 2026-05-01 12:30 | 0.25h    | bug-fix               | seo-analyzer     | Path resolution: --urls / --report-org / --report-json now resolve against process.cwd() instead of __dirname. Was producing "jsonld-check/jsonld-check/urls.site.txt" when run from seo-analyzer dir. |
| 2026-05-01 12:10 | 0.5h     | bug-fix               | seo-analyzer     | csv-ingest hardened: refuses tab-separated urls.txt as CSV input (was producing /color-advisor%09FAQPage URLs after a self-CSV ingest corrupted urls.site.txt). Also skips `#` comment lines in data rows. |
| 2026-05-01 11:45 | 0.5h     | implementation        | DOTCOMPB-7929    | improve-site-urls.mjs (.tasks/DOTCOMPB-7929/) — overlays curated urls.txt metadata + URL-pattern rules + drops booking widgets/auth/dashboard. 499 → 239 URLs (260 excluded), 50 leaders after dedup, manual groups: PDP=109, blog-post=46. |
| 2026-05-01 11:25 | 0.5h     | implementation        | seo-analyzer     | Stop-on-fetch-error (default on) + variation pacing. --throttle 1500ms, --variation-throttle 400ms, --no-stop-on-fetch-error opt-out. Halt info surfaces in summary + org/JSON reports; un-reached URLs marked SKIPPED. |
| 2026-05-01 11:10 | 0.75h    | implementation        | seo-analyzer     | Pacing + retry-on-AbortError. fetchers.mjs: 2 retries with backoff. index.mjs: --throttle (between leaders) + dev-SSR notice when --base-url is localhost. JSON-LD blocks embedded per leader/variation in org+JSON reports (--no-embed-jsonld disables). |
| 2026-05-01 10:35 | 1h       | implementation        | seo-analyzer     | FAQ-content detector + CSV ingest + NOT_A_CANDIDATE classification. faq-detector.mjs (Vue components, hcb-faqs class, FAQ heading, existing FAQPage); csv-ingest.mjs (Page URL column → audit rows); --csv / --csv-out flags; "needs Tophat tweak" section in org report. |
| 2026-05-01 10:00 | 1h       | implementation        | seo-analyzer     | Tier 1 dedup: dedup.mjs (groupBy content_id default, manual group=<label> override, --no-dedup escape hatch). index.mjs leader-audit + member-inherit; org/JSON/terminal reporters dedup-aware; question-count extrapolated via member inheritance. |
| 2026-05-01 04:30 | 1.5h     | session-reset         | this             | v4 reset. Documents v14 Tophat-first restoration; harness rewrite (experiment-aware, raw-HTML-only, single-URL, --show-raw, AC5 duplicate detection, Tophat-link enrichment); migration to seo-analyzer with comprehensive README.org; F23 misdiagnosis correction (multi-variation reality); PB.1 DB-write workaround flagged for validation. |
| 2026-05-01 03:30 | 1h       | implementation        | DOTCOMPB-7929    | Migrated harness to /Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/. Added cms-link.mjs (Tophat URL display via single Mongo round-trip; takesUrlParameters parent-fallback). Pinned yargs to ^17.7.2 for Node 18 compat. Wrote ~19 KB README.org (12 sections: overview, install, CLI flags, npm scripts, URL list format, status taxonomy, output formats, experiment detection, Tophat link resolver, examples, validators, architecture, troubleshooting, programmatic API). |
| 2026-05-01 02:30 | 1.5h     | implementation        | DOTCOMPB-7929    | Rewrote harness. Removed cms-check.mjs / cms-state-cache.json (no Tophat/Mongo for verification). Added experiments.mjs (detectExperiment + probeAllVariations via ?v=&xid= overrides). Added --url single-URL mode + --types + --show-raw (pretty-printed JSON-LD blocks). Added AC5 duplicate detection. Changed question count semantics: max-per-URL summed across URLs (was sum-across-variations — over-inflated). |
| 2026-05-01 01:30 | 1h       | bug-fix               | DOTCOMPB-7929    | PB.1 investigation: audit kept showing BROKEN-IN-CMS for /colorbar/locations/hillsboro despite user's "Tophat is configured" assertion. Discovered multi-variation reality — db.contentVersion has 3 records per (content_id=2350, version=42) for variations A/B/C. Variation A = hcb-location-page V1 mixin, B/C = hcb-location-page-v2 V2 mixin. Slug request hits variation B. F23 was a misdiagnosis: the auto-gen R1 was only on variation A; B/C never had it. Direct DB writes to all 3 variations + production_content + stage_content made the slug emit FAQPage. Confirmed PASS, then PARTIAL once AC5 duplicate detection landed. DB-write approach flagged as workaround pending validation. |
| 2026-05-01 01:00 | 0.5h     | refinement            | DOTCOMPB-7929    | v14 plan refinement (Tophat-first restored — supersedes v10). Roam node Execution Constraints rewritten: "audit-driven, Tophat-first iterative loop, with code as last resort." Phase B restored from "REMOVED in v10" to canonical 4-task iterative loop (PB.1 HCB FAQPage, PB.2 /shop/* BreadcrumbList, PB.3 video-chat FAQPage, PB.4 audit re-run). Tasks #17-28 created in TaskList. |
| 2026-05-01 00:45 | 0.5h     | documentation         | DOTCOMPB-7929    | Added FAQPage canonical JSON template + worked example + field rules to roam node TICKET CONTEXT (after the existing Schema Structure block). Two #+begin_src json code blocks for copy-paste. |
| 2026-05-01 00:30 | 1h       | session-reset         | this             | v3 reset (detailed). Compacted v8-v13 plan refinements + Pug-interpolation accuracy work + harness upgrades. Replaced Section 5 with concise current state. Added Section 2.3 decisions 15-20 covering v8-v13. Updated Section 1.1 to reflect v10 unified mechanism. |
| 2026-05-01 00:30 | —        | architecture-extract  | this             | Architecture memory v2 → v3 (575 → 788 lines). Added: dp-004 (idempotent route-handler push), dp-005 (server-side port of CMS auto-gen logic via `extractFaqsFromContent`), cl-003 (slugged-child static-script gap, formalizing F23), rr-005 (BROKEN-IN-CMS detection methodology). |
| 2026-04-30 23:30 | 1h       | implementation+audit  | DOTCOMPB-7929    | v13 harness upgrade — CMS state inspection + strict JSON validity + BROKEN-IN-CMS classification. Built `cms-check.mjs`. (Note: this work was reversed on 2026-05-01 02:30 in the harness rewrite — cms-check.mjs deleted, BROKEN-IN-CMS classification removed, since verification reverted to raw-HTML-only.) |
| 2026-04-30 23:00 | 0.5h     | research+documentation | DOTCOMPB-7929 | v12 Pug-interpolation accuracy refinement. Section 6 (Interpolation context) rewritten with verified merge logic. New Section 12 added (5 subsections, ~150 lines). |
| 2026-04-30 22:30 | 1h       | research+documentation | DOTCOMPB-7929 | v11 JSON-LD mechanism deep-dive documentation. Wrote new top-level section in roam node `* JSON-LD MECHANISM REFERENCE` (~250 lines, 11 subsections). |
| 2026-04-30 22:00 | 0.5h     | refinement            | DOTCOMPB-7929    | v10 plan refinement (unified mechanism). Dropped R1/R2/R3 path distinction. (SUPERSEDED by v14 on 2026-05-01 01:00.) |
| 2026-04-30 21:30 | 0.5h     | refinement            | DOTCOMPB-7929    | v9 plan refinement (pattern reduction + FAQ-content gating). urls.txt: 25 → 16 URLs. |
| 2026-04-30 21:00 | 1.5h     | implementation+refinement | DOTCOMPB-7929 | v8 plan refinement (audit-first execution). Built and ran the verification harness AHEAD of original Phase 5 schedule. urls.txt seeded with 25 URLs. First audit run captured. |
| 2026-04-30 20:00 | 1h       | research+refinement   | DOTCOMPB-7929    | v7 plan refinement (Tophat configuration + slugged-child gap). F23 added. (Note: F23 corrected on 2026-05-01 01:30 — gap was actually a per-variation config issue, not a pipeline bug.) |
| 2026-04-30 19:00 | 0.5h     | refinement            | DOTCOMPB-7929    | v6 plan refinement (execution discipline). Dropped temporal framing. Reordered execution so Phase 5 lands BEFORE Phase 3. |
| 2026-04-30 18:30 | 0.5h     | refinement            | DOTCOMPB-7929    | v5 plan refinement (AEO Roadmap-driven). Added AC11 (FAQ Questions Indexed KPI ≥ 200). |
| 2026-04-30 18:00 | 0.3h     | documentation         | DOTCOMPB-7929    | AEO Roadmap CSV (JIRA attachment) imported. Created wrapper roam node `2026-04-30-aeo_roadmap_executive_summary.org`. |
| 2026-04-30 17:00 | 1.5h     | session-reset         | this             | v2 reset; merged v3+v4 roam refinement; arch memory v2 (ad-001 superseded; ad-005, ad-006, dp-002, dp-003, cl-002, rr-003, rr-004 added). |
| 2026-04-30 17:00 | —        | architecture-extract  | this             | Extracted 7 new entries into mr-seo-structured-data-architecture.md (273→575 lines). |
| 2026-04-30 16:00 | 1h       | refinement            | DOTCOMPB-7929    | v4 plan refinement: added 11 inline ⚙️ GATE blocks for AskUserQuestion + top-level Decision Gates index. |
| 2026-04-30 14:30 | 1.5h     | refinement            | DOTCOMPB-7929    | v3 plan refinement: F18-F22 findings; DEV-AC3/4 rewritten; Phase 2 collapsed; Phase 3 reframed. |
| 2026-04-30 13:30 | 1h       | research              | DOTCOMPB-7929    | Live MongoDB audit; verified unified additionalScripts pipeline; documented Tophat auto-gen contract. |
| 2026-04-30 11:30 | 1.5h     | research              | DOTCOMPB-7929    | 4 parallel Explore agents — Tophat CMS UI, CMS data models, SSR plumbing, per-route audit. |
| 2026-04-30 10:30 | 0.5h     | documentation         | this             | Loaded existing roam node + session file; oriented on prior work. |
| 2026-04-29 19:00 | 1h       | session-reset         | this             | First reset; extracted architecture memory v1. |
| 2026-04-29 17:00 | 2h       | refinement            | DOTCOMPB-7929    | Per-phase compliance briefing; added * COMPLIANCE & GUIDELINE CHECKs section to roam node. |
| 2026-04-29 17:00 | 1.5h     | research              | DOTCOMPB-7929    | Validation Round 2: enumerated 11 routes.js; F11–F17 added. |
| 2026-04-29 14:00 | 3h       | refinement            | DOTCOMPB-7929    | Refined ACs to 10 strict + 10 DEV-AC; live validation Round 1; Phase 0 validation report F1–F10. |
| 2026-04-29 12:00 | 1h       | documentation         | DOTCOMPB-7929    | Created roam node + index entries + session file scaffold (UUID bd9f004e-1c13-442a-b999-b5bdf73037c6). |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
