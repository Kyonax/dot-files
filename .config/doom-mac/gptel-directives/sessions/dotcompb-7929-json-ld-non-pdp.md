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
- 2026-05-01 12:55 (this reset, v5): harness refinement pass — Tier 1 dedup (`dedup.mjs`, group by content_id / manual `group=<label>`), CSV ingest (`csv-ingest.mjs`, AEO Roadmap Sheet2), FAQ-content detector (`faq-detector.mjs`) + `NOT_A_CANDIDATE` status, fail-fast on FETCH_ERROR + dev-SSR-aware throttle defaults, JSON-LD blocks embedded in org+JSON reports, cwd path-resolution fix, csv-ingest hardened against tab-separated input. New MR-specific tooling: `.tasks/DOTCOMPB-7929/improve-site-urls.mjs` overlays curated metadata + pattern rules onto the 499-URL CSV, dropping 260 non-SEO routes → 239 audit-worthy URLs collapsing to ~50 leader audits. Site-wide audit attempted twice; both halted by Vue dev SSR OOM (heap exhaustion under sustained PDP rendering). Audit not yet successfully completed — needs dev restart.
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

> **Start here when resuming.** This conversation ran two parallel tracks: the SEO/JSON-LD work (paused — site-wide audit blocked on Vue dev SSR OOM) and a hotfix-google branch (DOTCOMPB-8174, fully implemented + PR #20716 open). Both are awaiting external action: SEO needs a clean dev SSR restart; the hotfix needs CI/review/merge.

### What was done last (2026-05-04 evening session reset)

**Track 2 — hotfix-google (DOTCOMPB-8174) [PR #20716 open]:** Production booking-flow Google sign-in was broken on iPhone, Safari, mobile Chrome under FedCM. After five failed approaches (FedCM lifecycle wiring, OAuth Code popup, OAuth Code redirect, click-proxy on hidden Google button, invisible overlay), the **final approach drops the custom MrBtn entirely** and uses Google's default rendered button via `gai.renderButton()` — same pattern as the rest of the site (`SignInMixin`, `LoginSignUp`, `SignIn`, `SignUp`). Sentry AI flagged a `window.googleInitialized` race between `useGoogleSignIn` and `SignInMixin`; resolved by adding `use_fedcm_for_button: true` to `SignInMixin.js`'s `gai.initialize()` so both surfaces opt into FedCM mediation regardless of mount order. Final unstaged set: 5 modified + 1 deleted file. Lint 0 errors, tests 60/60 passing. Roam node `~/.brain.d/roam-nodes/madison_reed/2026-05-01-132350-hotfix_google.org` heavily refined with WHY CUSTOM BUTTONs CANNOT WORK + APPROACHEs TRIED (5 approaches with blockers) + FINAL APPROACH + COMMIT MSG + PR DESCRIPTION sections. Implementation home is the DOTCOMPB-7942 session file (parent ticket); this session file logs the events and cross-references it.

**Track 1 — seo-analyzer harness (DOTCOMPB-7929) [paused at full-site audit]:** Earlier in the same conversation: Tier 1 dedup, CSV ingest, FAQ-content detector + `NOT_A_CANDIDATE`, fail-fast halt + dev-SSR pacing defaults, JSON-LD blocks embedded in reports, cwd path fix, urls.site.txt improver. Two attempts at full-site audit triggered Vue dev SSR OOM (`FATAL ERROR: Ineffective mark-compacts near heap limit`). **Site-wide audit not yet completed** — needs dev process restarted.

### Outstanding work

**Track 2 — hotfix-google PR #20716 (this branch):**
- [ ] Human-driven: stage + commit (commit msg in roam node `* COMMIT MSG`), push, address CI feedback, request review, merge to Release. *AI runs no git commands.*
- [ ] Pre-merge: verify GCP OAuth client *Authorized JavaScript origins* includes `www.madison-reed.com`, `dotcom.mdsnrd.com`, all QA hosts, `localhost:3000`. No `Authorized redirect URIs` needed.

**Track 1 — SEO/JSON-LD (DOTCOMPB-7929 / DOTCOMPB-7945):**
- 🚨 **PB.1 validation** — DB-write workaround for `/colorbar/locations/<slug>` needs Tophat-editor redo (per-variation R2 Pug at content_id 2350 A/B/C). Will be wiped on next Tophat publish.
- **PB.2** — `/shop/*` BreadcrumbList (content_id 2686, `views.js` change + Tophat R2 Pug per variation).
- **PB.3** — `/schedule-video-chat` FAQPage (content_id 2536, R1 auto-gen first).
- **PB.4** — Site-wide audit re-run on a healthy dev SSR; categorize residue.
- **Phase C** if PB.1–PB.3 leave failures (PC.0–PC.4 plan in roam node).
- **Sign-off** — P5.5 Playwright + P6.1–P6.5 + P7.1.

### Where things live (canonical paths)

**Hotfix-google:**
- Roam node: `~/.brain.d/roam-nodes/madison_reed/2026-05-01-132350-hotfix_google.org` (UUID `0a4fa07b-15ab-4287-b448-3cb63e9498f9`).
- Plan archive: `/Volumes/dev-partition/github-madison-reed/the-code/.tasks/hotfix-google/plan.md` (Plan H header on top + historical v1/v2/v3 retained).
- PR: https://github.com/MadisonReed/mr/pull/20716 (Sentry AI thread answered at #discussion_r3175405177; reply id 3175416600).
- JIRA: https://madison-reed.atlassian.net/browse/DOTCOMPB-8174 (description rewritten to be issue-only; no solution prescription).
- Implementation session file: `dotcompb-7942-google-sso-booking.md` (parent ticket — natural home for the hotfix narrative).

**SEO/JSON-LD (unchanged from prior reset):**
- Harness: `cd /Volumes/dev-partition/local-projects/seo-analyzer` then `node jsonld-check/index.mjs --urls jsonld-check/urls.site.txt --report-org jsonld-check/reports/site-audit.org --report-json jsonld-check/reports/site-audit.json`.
- urls.site.txt regenerator: `node /Volumes/dev-partition/github-madison-reed/the-code/.tasks/DOTCOMPB-7929/improve-site-urls.mjs`.
- Roam node: `~/.brain.d/roam-nodes/madison_reed/2026-04-29-121311-dotcompb_7929.org` (UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`).

### Where to resume

**If the user says "continue with the hotfix" / "open the PR" / "ship 8174":** the work is already done. Direct them to the roam node `* COMMIT MSG` + `* PR DESCRIPTION` sections — copy verbatim into `git commit` and `gh pr create` (or update the existing PR #20716). Verify GCP OAuth origins as the only pre-merge step. AI runs no git commands.

**If the user says "address PR feedback" / "respond to a reviewer":** read the latest comments via `gh pr view 20716 --comments` or `gh api /repos/MadisonReed/mr/pulls/20716/comments` (review-thread comments). Sentry AI thread is already answered.

**If the user says "the hotfix is broken" / "google sign-in broke":** check the unstaged changes are still present (`useGoogleSignIn.js` has `use_fedcm_for_button: true`, `SignInOptions.vue` has `.google-btn-container` not the MrBtn, `SignInMixin.js` has `use_fedcm_for_button: true` too). The fix relies on `gai.renderButton()` being the click target. If reverted, the FedCM bug returns.

**If the user says "re-run the SEO audit" / "let's see the report":**
```sh
cd /Volumes/dev-partition/local-projects/seo-analyzer
node jsonld-check/index.mjs \
  --urls jsonld-check/urls.site.txt \
  --report-org jsonld-check/reports/site-audit.org \
  --report-json jsonld-check/reports/site-audit.json
```
*Make sure the dev SSR is up first.* With dedup + throttle defaults, ~50 leader audits × ~3 variations × ~2-5s = 10-15 min on a healthy dev. Reports land at the paths above.

**If the user says "regenerate site list" / "the CSV changed":**
```sh
node /Volumes/dev-partition/github-madison-reed/the-code/.tasks/DOTCOMPB-7929/improve-site-urls.mjs
```

**If the user says "validate the DB write" / "redo PB.1 via Tophat":** Open Tophat at content_id 2350 (`http://localhost:4000/#/cms/content/edit/2350`), switch variation selector A → B → C and add the R2 Pug script (text in §3.1) per variation, then re-run the harness.

**If the user says "start PB.2" / "fix shade shop":** Confirm variations on content_id 2686 via harness against `/shop/brown`, add `req.metaData.shadeFamilyName` + `colorSlug` in `views.js` (Recipe §3.2 step 4), Tophat R2 BreadcrumbList per variation.

**If the user asks for harness reference:** Read `/Volumes/dev-partition/local-projects/seo-analyzer/README.org` — sections "Site-wide audit from CSV", "Deduplication (Tier 1)", "CLI flags".

**If the user asks for plan / mechanism deep-dive (SEO):** Read the SEO roam node — anchors `#ticket-context`, `#json-ld-mechanism-reference`, `#audit-report`, `#ticket-tasks`.

**If the user asks for a status snapshot:** SEO last successful audit (2026-05-01 morning): 1 PASS + 1 PARTIAL (DB-write workaround) + 14 MISSING out of 16 curated URLs; site-wide audit not yet run successfully. Hotfix-google: code complete, PR #20716 open, awaiting CI/review/merge. Both tracks are externally blocked.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table. Newest first. See `~/.claude/skills/session-reset/rules/activity-log.md`.

| Datetime         | Duration | Type                  | Reference        | Description |
|------------------|----------|-----------------------|------------------|-------------|
| 2026-05-04 17:45 | 0.5h     | session-reset         | this             | v6 reset. Captures the hotfix-google branch (DOTCOMPB-8174) work that ran in parallel to the SEO track. Replaces Section 5 with a dual-track state (SEO paused on dev SSR OOM; hotfix complete + PR #20716 open). Implementation home for the hotfix narrative is the DOTCOMPB-7942 session file — this file logs the events. |
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
