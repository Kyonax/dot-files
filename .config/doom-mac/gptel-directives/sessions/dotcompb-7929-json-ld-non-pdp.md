<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the DOTCOMPB-7929 (FAQPage JSON-LD on non-PDP templates) + DOTCOMPB-7945 (BUG: Missing BreadcrumbList JSON-LD on Shade Shop PLPs) session. Both tickets are co-tracked because they form one unified structured-data pass on non-PDP routes and ship together via the unified `additionalScripts` pipeline (see [session: mr-seo-structured-data-architecture > architecture-decisions > ad-005]). Loaded at the start of every conversation to give the AI full context without re-discovering anything. Read sections in order on first load — after that, reference them by number.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, patterns, conventions for ALL work in this session. | Before any code task. Mandatory constraints. |
| **2. Session Overview** | Scope, ticket status, key decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-ticket detail: AC, decisions, files, commands. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference file path table — every file with line numbers. | When reading, editing, or locating files. |
| **5. Last Interaction** | Short-term memory: last work, pending, resume points. | At conversation start — entry point. |
| **6. Activity Log** | Datetime-stamped table of every meaningful event in this session. | When you need exact "what was done when". |

**Operational Rule:** Always look for the last request identified by `###` title. Load relevant skills (see Section 1) and apply Section 1 rules.

**Key principle:** Data may appear in multiple sections with different framing — Section 1 frames it as a rule, Section 2 frames it as scope context, Section 3 frames it as ticket implementation. Each section answers a different question about the same knowledge.

**Roam node (single source of truth for plan):**
- File: `~/.brain.d/roam-nodes/madison_reed/2026-04-29-121311-dotcompb_7929.org`
- UUID: `bd9f004e-1c13-442a-b999-b5bdf73037c6`
- Co-tracks BOTH tickets. Updated to v4 (2026-04-30) — includes inline `⚙️ GATE` blocks for AskUserQuestion-driven dynamic execution.

**Roam index (Madison Reed master):**
- File: `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org`
- BACKLOG anchors `<<ticket-7929>>`, `<<bug-7945>>` both pointing to UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`. Sprint Board: IN CODE REVIEW lane.

**Architecture memory (this session's domain):**
- File: `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/mr-seo-structured-data-architecture.md`
- v3 (2026-04-30 evening): 6 architecture decisions (ad-001 superseded), 5 design patterns, 3 constraints, 5 reusable references. 788 lines.
- Cross-reference syntax: `[session: mr-seo-structured-data-architecture > <section> > <entry-id>]`.

**Compaction sources (chronological):**
- 2026-04-29 afternoon: roam node + index + session file created. UUID assigned. 4 parallel research subagents. ACs refined to 10 strict + 10 DEV-AC. F1–F10 written.
- 2026-04-29 evening: per-phase compliance briefing mapped against `mr-dotcom-dev` + `code-review` skill rule trees. Validation Round 2 — F11–F17 added. Architecture memory v1 extracted.
- 2026-04-30 morning: 4 parallel deep-research agents on Tophat CMS UI, CMS data models, SSR plumbing, per-route audit.
- 2026-04-30 afternoon: live MongoDB audit revealed unified pipeline; F18–F22 added; ad-001 superseded; v3-v4 plan refinement (Phase 2 collapsed, Path R1/R2/R3 designations, Phase 4 merged into P3.9, 11 inline `⚙️ GATE` blocks added).
- 2026-04-30 17:00: v2 session reset. AEO Roadmap CSV imported; v5-v6 plan refinements (AC11 KPI, Phase 7, scope tiers v1/v2, temporal framing dropped, Phase 5 reordered before Phase 3, Routes-affected + Per-task-verify lines).
- 2026-04-30 evening: v7 (Tophat slugged-child gap F23 discovered) → v8 (audit-first restructure, harness scaffolded ahead of schedule) → v9 (URL set 25→16, FAQ-content gating) → v10 (R1/R2/R3 collapsed to single route-handler push mechanism, Phase B removed, P1.8 added) → v11-v12 (JSON-LD MECHANISM REFERENCE 12-section deep-dive doc in roam node) → v13 (harness upgrade with `cms-check.mjs`, BROKEN-IN-CMS auto-classification, strict JSON validity).
- 2026-04-30 23:30 onward: this session reset (v3 architecture memory adds dp-004, dp-005, cl-003, rr-005).

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `mr-dotcom-dev`, `mr-roam-node`, `seo-web-quality`, `code-review`, `mr-style`. This section stores session-scoped patterns not yet captured in those skills — staging for guidelines that may eventually be promoted there.

### 1.1 Scope Boundary — Templates AND URL-derived data, not URLs themselves

*   **v10 unified mechanism:** every URL needing JSON-LD in this PR uses ONE pattern — route-handler push via `pushJsonLdToContent`. Code reads from Magento product, CMS `componentList` (via `extractFaqsFromContent` — dp-005), or URL params; builds the schema; pushes; existing pipeline emits. R1/R2/R3 path classification is documentation-only (see roam node `* JSON-LD MECHANISM REFERENCE`).
*   **No code deploy on CMS edits.** The Tophat auto-gen pipeline (dp-002) bakes JSON-LD at save time; an editor adding/removing a FAQ item must reflect on next page render without a rebuild. Idempotent push (dp-004) protects existing R1 emissions.
*   **One reference implementation:** `/colorbar/locations` directory (content_id 2349) emits FAQPage via Tophat auto-gen — regression sentinel. Slugged children of `/colorbar/locations/` (content_id 2350) are the broken case fixed by P3.4 via dp-005.

### 1.2 JSON-LD Pipeline — short reference (full reference in roam node)

> **Canonical deep-dive lives in the roam node** at section `* JSON-LD MECHANISM REFERENCE — full Tophat-to-HTML flow` (`#json-ld-mechanism-reference`). 11 subsections covering: pipeline overview, 2 storage levels (template + content), Tophat UI source pointers, full script descriptor field reference, request-time pipeline diagram, interpolation context, 3 verified production examples, generic recipe + decision tree for ANY Schema.org `@type`, gotchas table, file:line references, Tophat checkbox cheat-sheet. **Read it before designing any JSON-LD task.**

This section is the short reference (full traceability as of v7+, simplified mechanism choice in v10):


```
[SAVE TIME — Tophat]                    [REQUEST TIME — render]
content.renderOptions.additionalScripts ── htmlRenderer.js:198-214
                                              ├─ addScriptsDefsInto({content.renderOptions, ...}) [scriptsUtils.js:16-156]
                                              └─ addScriptsDefsInto({template, ...})
                                                  └─ pug.compile(_pugScript, locals)  ← SILENT FAILURE here on error: log.error + _htmlScript=''
                                                      → locals.header.scripts.push(...)
                                                          → vue-layout-ssr.pug:24-26 each scriptDefinition
                                                              → raw HTML <head>
```

**Three writers** produce `additionalScripts[]` entries — see [session: mr-seo-structured-data-architecture > architecture-decisions > ad-005]:
- **Path R1 — Tophat auto-gen** (dp-002): editor adds a component with `addFaqMetadata: true` + `questions[]/answers[]` (or `faqs[]` HCB shape, or custom field names via `faqMetadataFields`). `ContentEditCtl.js:getAdditionalScriptForFAQs` (lines 1465-1537) runs at save time only; merges all flagged components into ONE FAQPage entry. Sets `generatedAutomatically: true`.
- **Path R2 — Pug-interpolated** (dp-003): hand-authored entry via Tophat Advanced Config / Scripts UI. `forceInterpolation: true` + Pug source for runtime data (`#{metaData.x}`). Used by `/colorbar/locations/` for HairSalon/Review per location.
- **Path R3 — Route-handler push** (ad-006): code calls `pushJsonLdToContent(content, schema, metadata)` after CMS content load and before `htmlRenderer.renderContent`. Used by PDP FAQs (Magento data) and Shade Shop / Shop Category breadcrumbs (URL params). **Critical:** must NOT set `generatedAutomatically: true` — that flag is reserved for Path R1 and triggers wipe on next editor save.

**Tophat Advanced Config / Scripts checkbox semantics for FAQPage:** type=`ld+json`, isUrl=`false`, inHeader=**`true`**, forceInterpolation=`false` (static FAQPage) or `true` (Pug-driven, e.g., HairSalon), addBodyLoadScript=`false`. UI source: `tophat/src/views/ngpartials/cms/content/edit.pug:~600` + controller `tophat/src/ngscripts/cms/ContentEditCtl.js`. Full reference in roam node finding F23.

### 1.3 JSON-LD Output Rules (apply regardless of writer path)

*   **Server-rendered.** JSON-LD must appear in raw HTML (verify: `curl -sS <URL> | grep -c 'application/ld+json'` ≥ 1). Client-side-only injection is **not acceptable** (AC4).
*   **Inject in `<head>`.** All `additionalScripts` entries must use `inHeader: true`.
*   **One block per `@type` per page.** If a page has multiple FAQ-bearing components, Tophat auto-gen merges Q&A into a single `FAQPage` `mainEntity` array. Don't emit two `FAQPage` blocks.
*   **Empty content → no block.** Schema builders return `null` when input is empty; `pushJsonLdToContent` early-returns on null. AC6.
*   **Sanitize answer HTML** (when builder is in OUR control — Path R3): allowlist `[a, br, ol, ul, li, p, strong, em]`. See [session: mr-seo-structured-data-architecture > architecture-decisions > ad-003].
*   **`Question.name` is plain text.** Full strip — no HTML in question names.
*   **Note on Tophat-authored answers:** the production `/colorbar/locations` data preserves raw `<a href="...">` and unicode quotes verbatim — the auto-gen pipeline does NOT sanitize. AC2's sanitizer applies ONLY to Path R3 builders we write; do not insert sanitization into the auto-gen pipeline (out of scope and risky).

### 1.4 Canonical Schema Shapes

- FAQPage: [session: mr-seo-structured-data-architecture > reusable-references > rr-001]
- BreadcrumbList: [session: mr-seo-structured-data-architecture > reusable-references > rr-002]
- additionalScripts entry: [session: mr-seo-structured-data-architecture > reusable-references > rr-003]

### 1.5 Shared Utility Discipline

*   **Pure functions only.** Schema builders take plain data, return JSON objects (or `null`). No `req`, no globals, no side effects. See [session: mr-seo-structured-data-architecture > design-patterns > dp-001].
*   **Sanitizer is its own module.** `sanitizeFaqAnswerHtml(html)` exported from `mr_modules/cms/lib/jsonLd/sanitize.js` — single source of truth for the allowlist.
*   **Builders live at `mr_modules/cms/lib/jsonLd/`** — see [session: mr-seo-structured-data-architecture > architecture-decisions > ad-004].
*   **No webservices-layer logic.** Schema work goes in `mr_modules/cms/lib/`, never `mr_modules/webservices/lib/` (per `the-code/.claude/rules/shared-modules.md` — webservices are thin pass-throughs only).
*   **Backward-compatible exports.** Existing `buildFaqPageJsonLd({ product, productUrl })` callers must keep working (DEV-AC9). Use overload or thin adapter.
*   **Push helper invariant.** When using `pushJsonLdToContent` from route handlers, NEVER set `generatedAutomatically: true` — that flag is reserved for Tophat's auto-gen and triggers wipe on next editor save. See [session: mr-seo-structured-data-architecture > architecture-decisions > ad-006].

### 1.6 SSR Safety

*   `mr_modules/` and SSR-executed Vue paths use `require('Log')` (no `console.*`).
*   No `window`, `document`, `localStorage` unless guarded by `typeof window !== 'undefined'` or `import.meta.env.SSR` (per `the-code/.claude/rules/ssr-safety.md`).
*   `document.head.appendChild` stays client-only — kept in `productJsonLdUtils.js` ONLY as defensive dedup helper, no longer canonical injection.

### 1.7 QA Verification (v6 — harness-first discipline)

**Per-task verify (every Phase 3 / Phase 6 task):**
1. **Automated checker (AC10 + AC11):** `node .tasks/seo-jsonld-check/check.mjs --urls <subset|urls.txt> --types <expected> --fail-on-missing` exits 0. Each Phase 3 task uses its own `subsets/p3.X-<task>.txt` (just-the-routes-it-touched). Phase 6 P6.3 runs the full sweep with `--kpi-report`.
2. **Empty-content guard** (per template): pick one page of the same template with no FAQ items → harness reports `BreadcrumbList` count = 0 (or whatever the expected schema is) for that URL.
3. **PDP regression spot-check:** harness `--types Product,FAQPage` on `/product/calabria-dark-brown` confirms BOTH still emit post-P3.1.
4. **CMS edit propagation (Path R1, v2 work):** Tophat edit → reload → harness reflects change without restart.

**Pre-merge sign-off (Phase 6 only):**
5. **Google Rich Results Test** on QA URL (https://dotcom.mdsnrd.com/...) per G6.4 default — zero errors, zero warnings.
6. **Diagnostic mongo one-liner:** see [session: mr-seo-structured-data-architecture > reusable-references > rr-004]. Inspect `cms.contentVersion.renderOptions.additionalScripts[]` directly when something looks off.

**Pre-harness fallback (P0.2 baseline only — used ONCE before P5.4 lands):**
7. `curl -sS http://localhost:3000/<path> | grep -c 'application/ld+json'` ≥ 1 — only acceptable for the pre-change baseline confirmation in P0.2. Once P5.4 is in place, every subsequent verification goes through the harness, NOT raw curl + grep.

### 1.8 Code Review / Style

*   `mr-dotcom-dev` rules apply for any Vue/Pug/Vuex changes.
*   `mr-style` rules apply for any template/style edits.
*   JSDoc block description mandatory on every public function (PilkoLint enforces). Use `Function` (capital F) for type, never `function`.
*   Lint before commit: `bash /Volumes/dev-partition/github-madison-reed/the-code/.tasks/lint-changed.sh` from repo root.
*   No `console.*` — use `require('Log')` server-side.
*   Brackets for if/else — no one-line conditionals.
*   Test commands:
    ```sh
    cd /Volumes/dev-partition/github-madison-reed/the-code/website && npm run test:vue
    cd /Volumes/dev-partition/github-madison-reed/the-code && npm run unit-tests
    ```

### 1.9 Tracking & Observability

*   **No new Segment events.** SEO work is invisible to user. No `trackMREvent` calls.
*   **Sentry watch post-deploy:** `JSON.stringify` errors (cyclic CMS refs) and sanitizer throws on malformed HTML — both should degrade to "no schema block", not crash.

### 1.10 Validated Architectural Facts (DO NOT re-derive)

*   **CMS database name is `cms`** (NOT `madisonreed`). Use `docker exec mr-mongo mongosh cms`.
*   **JSON-LD storage path:** `cms.contentVersion {content_id, version}.renderOptions.additionalScripts[]`. Top-level on contentVersion, NOT nested under `templateData`.
*   **15 production URIs have JSON-LD today** (verified 2026-04-30): `/colorbar/locations`, `/colorbar/locations/`, `/hair-color-chart`, `/how-to-color-your-hair`, `/how-to-highlight`, 11 `/blog/*` posts. **Zero coverage:** PDP, `/schedule-video-chat`, all `/shop/*`, `/help`, `/blog` (landing), `/`, `/bundle/perfect-pair/*`.
*   **`how-to-question-accordion-vue` is THE canonical FAQ component** — 10/10 simple-case production usage. `hcb-location-page` is the only other FAQ-flagged component (1/1).
*   **`pdp-tabs-reviews-faqs` (16 PDPs) does NOT carry `addFaqMetadata` flag** — major gap. After P3.1 ships (Path R3 from product catalog), these should pick up automatically IF catalog has `productTypeFaqs/productFaqs` data.
*   **`specific-shop-product-config` template has NO componentList** — Path R1 not viable for `/shop/*`. See [session: mr-seo-structured-data-architecture > constraints > cl-002].
*   **HCB Location V1 is dead code** — `cms.contentVersion` for `/colorbar/locations/` only references `hcb-location-page` (V2). Confirmed 2026-04-30. Skip V1 entirely.
*   **`/colorbar/locations` directory uses Path R1** (auto-gen, not hand-authored Pug). componentList[3] = `how-to-question-accordion-vue` with `addFaqMetadata: true`. Resolves F10.7.
*   **Tophat auto-gen runs at SAVE only.** Pre-existing content docs need editor re-save before FAQ script appears. No retroactive generation server-side.
*   **Slugged-child gap (F23, verified 2026-04-30 evening):** every slugged child of `/colorbar/locations/` (`/colorbar/locations/<slug>` for ~85+ HCB locations) emits HairSalon (Pug-interpolated, `forceInterpolation: true`) but DROPS the static auto-gen FAQPage (`forceInterpolation: false`, `generatedAutomatically: true`) despite both being present in `content.renderOptions.additionalScripts` for content_id 2350. Cause unknown — three candidates: silent Pug compile error (escaped HTML in answer text), cache staleness, or render-path bug specific to `takesUrlParameters: true`. P3.4 reopened with investigation tree.
*   **Pipeline silent-failure mode:** `scriptsUtils.js:addScriptsDefsInto()` lines 134-142 wraps `pug.compile()` in try/catch. On error: `log.error(err)` then `_htmlScript = ''`. The script object stays in `header.scripts[]` but emits empty in HTML. Easy to miss without enabling `DEBUG=cms.scriptsUtils`.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Add `FAQPage` JSON-LD to non-PDP templates and `BreadcrumbList` JSON-LD to the 4 Shade Shop PLPs. The injection mechanism is the existing unified `additionalScripts → header.scripts → vue-layout-ssr.pug:24-26` pipeline (validated in production for 14/15 JSON-LD-bearing URIs). The PDP path is fixed as a side effect by switching from the legacy client-side `document.head.appendChild` injector to a route-handler push into the same pipeline.

**SEO outcome:** Every CMS-FAQ-bearing page becomes Rich Results-eligible (FAQPage); Shade Shop PLPs gain BreadcrumbList eligibility; PDPs gain crawler-visible FAQPage.

### 2.2 Scope

| Ticket | Type | Status | Anchor |
|---|---|---|---|
| `DOTCOMPB-7929` | Story | **In Code Review** (branch new, no implementation yet) | `<<ticket-7929>>` |
| `DOTCOMPB-7945` | Bug — co-tracked | **In Code Review** (merged into P3.9 of unified plan) | `<<bug-7945>>` |

JIRA: https://madison-reed.atlassian.net/browse/DOTCOMPB-7929 / https://madison-reed.atlassian.net/browse/DOTCOMPB-7945
Related: DOTCOMPB-7466 (parent of bug), DOTCOMPB-7230 (predecessor SPIKE), PR #20512 (where breadcrumb gap originated).

### 2.3 Key Decisions

1.  **(2026-04-29)** Co-track 7929 + 7945 on a single roam node (UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`). Both ship together.
2.  **(2026-04-29)** Refined ACs to 10 strict + 10 DEV-AC. AC4 mandates raw-HTML presence. AC10 mandates automated checker harness.
3.  **(2026-04-29)** Verification harness location: `.tasks/seo-jsonld-check/`. Stack: axios + cheerio (adapted from `/Volumes/dev-partition/local-projects/seo-analyzer`). No Puppeteer (raw HTML only). Plus Playwright spec at `.tasks/qa-automation/DOTCOMPB-7929/specs/dotcompb-7929-jsonld.spec.ts`.
4.  **(2026-04-29 evening)** Per-phase compliance mapping completed. Roam node `* COMPLIANCE & GUIDELINE CHECKs` section has R/P/X bullets per task + Risk Register + Quick Reference table.
5.  **(2026-04-30)** **Mechanism unified** — see [session: mr-seo-structured-data-architecture > architecture-decisions > ad-005]. SUPERSEDES the 2026-04-29 decision to add new Pug `if` blocks (ad-001). Now: route handlers push into `content.renderOptions.additionalScripts[]`; existing `each scriptDefinition` iterator emits.
6.  **(2026-04-30)** **PDP fix is a 3-line change** in `mr_modules/cms/lib/router.js:462` (replace `content.faqPageJsonLd = …` with `pushJsonLdToContent(content, faqPageJsonLd, {type:'faq', source:'productRouter'})`). No Pug edits required. See [session: mr-seo-structured-data-architecture > architecture-decisions > ad-006].
7.  **(2026-04-30)** **Phase 4 merged into P3.9** of refined plan. Shade Shop and Shop Category breadcrumbs use one helper (`applyRouteScopedJsonLd` in `mr_modules/cms/lib/jsonLd/routeScoped.js`) with two constant maps (`SHADE_SHOP_FAMILIES`, `SHOP_CATEGORIES`).
8.  **(2026-04-30)** **Plan made dynamic** (v4) — 11 inline `⚙️ GATE` blocks in roam node specify exact `AskUserQuestion` invocations for every decision point. Top-level Decision Gates index lets future-AI track pending vs answered.
9.  **(2026-04-30)** Architecture memory v2 — ad-001 superseded; ad-005, ad-006, dp-002, dp-003, cl-002, rr-003, rr-004 added. Architecture file: 273 → 575 lines.
10. **(2026-04-30 afternoon)** **AEO Roadmap CSV imported** as companion roam doc node (UUID `0bca0fb0-73dd-439e-949c-d594d3ad8806`). Establishes that this ticket is the engineering execution of *Action #1 — FAQ Schema Implementation* of the AEO roadmap. Surfaces the success KPI: *FAQ Questions Indexed: ~0 → 200+*.
11. **(2026-04-30 evening)** **v5 plan refinement** — added AC11 (KPI alignment), Phase 7 (post-deploy KPI tracking, 3 tasks), v1/v2 scope tiers (this PR vs follow-up). Phase 5 harness extended to count Question entities and emit `kpi.json`. Cross-team coordination note added (Content team's Answer-First reformatting overlaps Tophat assets touched by P3.5–P3.8). Counter 4/41 → 4/44.
12. **(2026-04-30 late evening)** **v6 plan refinement (execution discipline)** — three changes per user request: (a) all temporal framing dropped (no day targets, no 14-day window, no Day-7 re-eval, no t+14/30/90 — Phase 7 cadence is now "per PM"); (b) Phase 5 (verification harness) reordered to land BEFORE Phase 3 — every per-route change is harness-verified against just-the-routes-it-touched as part of the task itself; (c) every code-touching task now declares explicit `Routes affected:` (URL list) and `Per-task verify:` (exact harness or test command). v1/v2 retained as scope labels for "this PR / follow-up PR" without time bound. Each Phase 3 task uses a `subsets/p3.X-<task>.txt` URL file scoped to its own routes.
13. **(2026-04-30 late evening)** **v6 audit completion pass** — fixed lingering `[SCOPE: v2 DEFER]` → `[SCOPE: v2]` on P3.5; added Routes affected + Per-task verify to the previously-missed Phase 0 unchecked tasks (P0.1, P0.3) and Phase 4 lifecycle items (F10.2, F10.10); updated TODO TASKs source comment to record v6. Final tally: 42 `Routes affected:` + 42 `Per-task verify:` lines, zero stale temporal markers in the plan body.
14. **(2026-04-30 evening)** **v7 plan refinement (Tophat configuration + slugged-child gap F23).** User-directed re-investigation: content_id 2350 (`/colorbar/locations/`, `takesUrlParameters: true`) has 2 `additionalScripts` but slugged children emit only [0] HairSalon Pug, dropping [1] FAQPage static auto-gen. Roam node F23 added: full Tophat checkbox reference + Path R1 auto-gen mechanism trace + request-time pipeline silent-failure mode. P3.4 reopened. Now formalized as architecture memory cl-003.

15. **(2026-04-30 evening)** **v8 plan refinement (audit-first execution).** Built verification harness AHEAD of original Phase 5 schedule. `.tasks/seo-jsonld-check/` committed: native `fetch` (no axios — Node 18 undici/`File` issue) + regex JSON-LD extraction (no cheerio). Phase A (audit + classify) and Phase B (manual Tophat validation) inserted between Phase 0 and Phase 1. v8 sequence: Phase 0 → A → B → 1 → 2 → 5 (re-runs) → 3 → 6 → 7. Code is the LAST RESORT.

16. **(2026-04-30 evening)** **v9 plan refinement (pattern reduction + FAQ-content gating).** Per-URL grep verified question-field presence. Reduced urls.txt 25→16. v1 active testing = 3 representative URLs: `/product/calabria-dark-brown` (all PDPs), `/colorbar/locations/hillsboro` (~85+ HCB slugs), `/shop/brown` (4 shade families). Plus reference `/colorbar/locations`. Bundles dropped (no FAQ content). P3.2 → VERIFY-ONLY.

17. **(2026-04-30 evening)** **v10 plan refinement (unified mechanism).** Dropped R1/R2/R3 path distinction. Every URL fixed via route-handler push (`pushJsonLdToContent`). Phase B REMOVED. New helper P1.8: `extractFaqsFromContent(content)` ports `ContentEditCtl.getAdditionalScriptForFAQs` server-side (formalized as dp-005). Idempotent push (formalized as dp-004) protects existing R1 emissions. Three v1 fix tasks now: P3.1 PDP, P3.4 HCB-detail, P3.9 Shade Shop — all the same shape. Counter 10/49 → 10/48.

18. **(2026-04-30 evening)** **v11-v12 documentation deep-dive.** Wrote canonical 12-section JSON-LD MECHANISM REFERENCE in roam node — pipeline overview, 2 storage levels, Tophat UI sources, full script descriptor field schema, ASCII pipeline diagram with silent-failure mode, interpolation context with verified `locals.metaData = {...content.metaData, ...req.metaData}` shallow merge, 3 verified examples (R1/R2/R3), decision tree for ANY Schema.org @type, gotchas table, 10 file:line refs, Tophat checkbox cheat-sheet. Verified `/shop/**` template: both content + template `additionalScripts` empty arrays — editor CAN add scripts. Section 12 added: Pug-interpolated scripts deep-dive with content_id 2350 dissection (3 known JSON-validity bugs flagged out-of-scope). Session 1.2 reduced to short ref + pointer to roam doc.

19. **(2026-04-30 evening)** **v13 harness upgrade — CMS state inspection + strict JSON validity + BROKEN-IN-CMS classification.** Built `cms-check.mjs` (queries MongoDB via `docker exec mr-mongo mongosh --eval`; parent-fallback walks path components — formalized as rr-005). Added `detectJsonValidityIssues` (multi-object concatenation, trailing commas, parse errors). New `BROKEN-IN-CMS` status auto-classifies F23 case. Re-ran: **1 PASS, 11 MISSING, 1 BROKEN-IN-CMS automatically classified (`/colorbar/locations/hillsboro`), 3 NOT-A-CANDIDATE**. faqQuestionCountTotal = 4 vs AEO target 200+. PDPs confirmed not-in-CMS (code-only path). Plan top now references `[[#audit-report]]` with current-state summary table.

20. **(2026-04-30 evening)** **Architecture memory v3 extracted.** dp-004 idempotent route-handler push, dp-005 server-side port of CMS auto-gen logic (extractFaqsFromContent), cl-003 slugged-child static-script gap (F23 formalized), rr-005 BROKEN-IN-CMS detection methodology. Architecture file: 575 → 788 lines.

### 2.4 Pending Work (v6 plan)

Total: **4/44 done (9%)**. Phase 0: 4/6 done. Roam node `* TODO TICKET TASKs` is canonical — this is a summary mirror.

**Execution sequence (no time-axis; each step blocks the next):**
Phase 0 → Phase 1 → Phase 2 → Phase 5 (harness, BEFORE Phase 3) → Phase 3 → Phase 6 → Phase 7.

**Phase 0 — Discovery [4/6]**
- ⬜ P0.1: Pull PR diff (gate G0.1; v6 default = "Local only" since `git status` is clean)
- ✅ P0.2: PDP SSR-gap verified
- ⬜ P0.3: URL Scope spreadsheet → urls.txt (gate G0.3; v6 default = "Paste URI list")
- ✅ P0.4: ld+json injection paths mapped
- ✅ P0.5: Stakeholder questions PARTIAL — F10.6/F10.7 resolved by F18; F10.1/F10.2/F10.10 still need PM/SEO (gate G0.5; v6 defaults: q1 *No*, q2 *Keep both*, q3 *4 shades only*)
- ✅ P0.6: CMS data inspection done via direct DB read (replaces auth-walled Tophat manual)

**Phase 1 — Builders & Sanitizer [0/7]** — independent of all gates EXCEPT G1.1. Pure utilities; routes affected = indirect (consumed by Phase 3).
**Phase 2 — Push helper + audit + client cleanup [0/3]** — P2.1 used by every Path R3 task; P2.3 affects PDP + Bundle (do AFTER P3.1 to avoid leaving PDP without injection mid-stream).
**Phase 5 — Verification harness [0/5]** — *MUST land before Phase 3.* Adds Question entity counter (AC11), `--kpi-report` flag emitting `kpi.json`, scaffolds `subsets/`, `baselines/`, `reports/` dirs. First useful run is the pre-change baseline.
**Phase 3 — Per-route wiring [0/11]** — each task uses harness against `subsets/p3.X-<task>.txt` as per-task verify. v1 routes: P3.1 (PDP), P3.2 (Bundle), P3.9 (Shade Shop 4 only), P3.11 (audit). v2 routes: P3.5–P3.8 (Tophat coordination), SHOP_CATEGORIES expansion, P3.10 chase.
**Phase 4 — MERGED into P3.9.** Lifecycle items F10.2 + F10.10 still open as stakeholder gates.
**Phase 6 — QA, PR, Sign-off [0/5]** — full harness sweep with `--kpi-report` (P6.3), Rich Results Test on QA (G6.4 default).
**Phase 7 — Post-deploy KPI tracking [0/3]** — P7.1 captures t=0 baseline (v1); P7.2 (cadence per PM) + P7.3 (Search Console correlation) are v2.

### 2.5 Decision Gates (canonical list mirrors roam node — v6)

| Gate | Status | Scope | v6 default selection (override on contradicting input) | Blocks | What it decides |
|---|---|---|---|---|---|
| G0.1 | pending | v1 | "Local only" (`git status` clean) | P0.1 | Branch state — PR exists / pushed-no-PR / local-only |
| G0.3 | pending | v1 | "Paste URI list" | P0.3, P3.11, Phase 5 seed | URL Scope source — paste / CSV / use existing 26 / defer |
| G0.5 | pending | mixed | q1 *No*, q2 *Keep both*, q3 *4 shades only* | P3.9 scope, P6.4 | F10.1 (`/shop-all`) + F10.2 (microdata) + F10.10 (categories) — 3 questions in one call |
| G1.1 | pending | v1 | "New `jsonLd/` subdir (Recommended)" | All Phase 1 + downstream | Module location — uses preview field with code mockup |
| G2.2 | conditional | v2 | "Document as out of scope" | P2.2 | Mobile Pug coverage — only fires if grep finds variants without iteration |
| G3.5_7 | pending | v2 | "None of the above" | P3.5, P3.6, P3.7 | Which CMS-driven pages need FAQPage (multi-select) |
| G3.8a | pending | v2 | (skip — P3.8 is v2-scope) | P3.8 | After Tophat inspection: `video-chat-faqs` shape |
| G3.8b | pending | v2 | (skip) | P3.8 | Conditional follow-up if shape mismatched |
| G3.9 | pending | v1 | "4 shades only" (matches G0.5 q3) | P3.9 | Confirm SHADE_SHOP_FAMILIES + leave SHOP_CATEGORIES empty for v2 |
| G3.10 | conditional | v1+v2 | "Defer to catalog ticket" | P3.10 | Only fires if post-P3.1 verify shows missing PDP FAQs |
| G6.4 | pending | v1 | "QA only" (pre-merge sanity) | P6.4 | Rich Results Test — QA / production / both |

**Each gate has a fully-formed `⚙️ GATE` block in the roam node TASKS section** specifying the exact `AskUserQuestion` field shape (label, description, multiSelect, optional preview). When executing the plan, plug straight into `AskUserQuestion`. v6 default selections let future-AI execute the v1 path without reprompting unless PM/SEO pushes back.

---

## SECTION 3: FEATURE / TICKET IMPLEMENTATIONS

### 3.1 DOTCOMPB-7929 — FAQPage JSON-LD on Non-PDP Templates

**Created:** 2026-04-29 | **Last updated:** 2026-04-30 (v4 plan)
**Status:** In Code Review — branch `DOTCOMPB-7929` clean, no implementation yet (P0.1 pending)
**Branch:** `DOTCOMPB-7929` (local; remote/PR state via gate G0.1)
**JIRA:** https://madison-reed.atlassian.net/browse/DOTCOMPB-7929
**Roam node:** `~/.brain.d/roam-nodes/madison_reed/2026-04-29-121311-dotcompb_7929.org` (UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`)

#### Acceptance Criteria — 10 strict (full GIVEN/WHEN/THEN in roam node TICKET CONTEXT)

| AC | Title | Verification |
|---|---|---|
| AC1 | Shared FAQ schema builder, exported and pure | Unit tests for `buildFaqPageJsonLd({ faqs, pageUrl, about })` |
| AC2 | Sanitizer with allowlist on Answer.text | Unit test: each allowlisted tag preserved (incl. `href` on `a`); each disallowed tag stripped |
| AC3 | Question.name is plain text | Unit test: HTML in `question` → 0 tags in output |
| AC4 | JSON-LD present in raw HTML | `curl -sS <URL> \| grep -c 'application/ld+json'` ≥ 1 |
| AC5 | Single FAQPage per page; multi-source merge | Tophat auto-gen handles this naturally — multiple flagged components collapse into one entry |
| AC6 | Empty-content guard | `curl -sS <empty> \| grep -c 'FAQPage'` returns 0 |
| AC7 | CMS-driven propagation without code deploy | Tophat edit → reload → schema reflects |
| AC8 | Per-template wire-up enumerated | URL Scope spreadsheet exhaustively wired (gate G0.3) |
| AC9 | BreadcrumbList JSON-LD on 4 Shade Shop PLPs | Section 3.2 |
| AC10 | Automated verification harness committed | `.tasks/seo-jsonld-check/check.mjs --fail-on-missing` exits 0 |

#### DEV-AC summary

DEV-AC1 single source of truth for builder; DEV-AC2 sanitizer is its own pure module; **DEV-AC3 unified `additionalScripts` pipeline** ([session: mr-seo-structured-data-architecture > architecture-decisions > ad-005] — supersedes prior ad-001); **DEV-AC4 PDP fix via `pushJsonLdToContent` in `router.js:462`** ([session: mr-seo-structured-data-architecture > architecture-decisions > ad-006]); DEV-AC5 Shade Shop reads route param; DEV-AC6 no webservices logic; DEV-AC7 SSR-safe code; DEV-AC8 no new event tracking; DEV-AC9 backward-compatible exports; DEV-AC10 lint + tests + ≥80% Codecov.

Full text in roam node `** DEVELOPMENT AC` section (DEV-AC3/4 rewritten 2026-04-30).

#### Implementation Detail (post-v4 plan)

Three writer paths into `content.renderOptions.additionalScripts[]`:
- **Path R1 (Tophat auto-gen)** for CMS-driven pages with `componentList`: `/help`, `/blog` landing, `/`, `/schedule-video-chat`. Editor adds `how-to-question-accordion-vue` component with `addFaqMetadata: true` + `questions[]/answers[]`. Zero code.
- **Path R2 (Pug-interpolated)** already in use by `/colorbar/locations/` for HairSalon/Review. Reference example only — no new R2 work in this ticket.
- **Path R3 (route-handler push)** for routes where data isn't editor-authorable: PDP `/product/:slug` (Magento product FAQs), `/bundle/perfect-pair/:slug` (same as PDP), `/shop/{brown,blonde,red,black}` + shop categories (URL-derived breadcrumbs).

#### Per-Route Wiring Table

| Route | Path | Action |
|---|---|---|
| `/colorbar/locations` | R1 | DONE (reference) |
| `/colorbar/locations/:urlKey` | R2 | DONE (reference) |
| PDP `/product/:slug` | R3 | Edit `mr_modules/cms/lib/router.js:462` — 3 lines |
| Bundle `/bundle/perfect-pair/:slug` | R3 | Confirm route handler in P3.2; usually shares with productRouter |
| `/help` | R1 | Tophat editor adds component (gated by G3.5_7) |
| `/blog` landing | R1 | Tophat editor adds component (gated by G3.5_7) |
| `/` | R1 | Tophat editor adds component (deprioritized, gated by G3.5_7) |
| `/schedule-video-chat` | R1 | Tophat: tick `addFaqMetadata` on `video-chat-faqs` (gated by G3.8a/b — depends on field shape) |
| `/shop/{brown,blonde,red,black}` | R3 | New `applyRouteScopedJsonLd` helper (P3.9) |
| Shop categories `/shop/{all-hair-color, …}` | R3 | Same helper (P3.9), depends on G3.9 confirming category list |
| 16 PDPs with `pdp-tabs-reviews-faqs` | R3 (auto) | Resolved by P3.1 — verify post-deploy (G3.10 conditional) |

#### Test Coverage

Per DEV-AC10: ≥ 80% Codecov patch coverage on new files; sanitizer + builder unit tests; per-route integration assertions.

---

### 3.2 DOTCOMPB-7945 — BreadcrumbList JSON-LD on Shade Shop PLPs

**Created:** 2026-04-29 | **Last updated:** 2026-04-30 (merged into P3.9 of unified plan)
**Status:** In Code Review — co-tracked with 7929
**Branch:** same as 7929 (`DOTCOMPB-7929` per branch naming convention)
**JIRA:** https://madison-reed.atlassian.net/browse/DOTCOMPB-7945

#### Bug Reproduction (verified 2026-04-29 + still confirmed 2026-04-30)

```sh
for c in brown blonde red black; do
  echo -n "/shop/$c: "; curl -sS http://localhost:3000/shop/$c | grep -c 'application/ld+json'
done   # → 0 0 0 0
```

#### Affected Pages

| Color | Dev | QA | Production |
|---|---|---|---|
| Brown | localhost:3000/shop/brown | dotcom.mdsnrd.com/shop/brown | www.madison-reed.com/shop/brown |
| Blonde | …/shop/blonde | …/shop/blonde | …/shop/blonde |
| Red | …/shop/red | …/shop/red | …/shop/red |
| Black | …/shop/black | …/shop/black | …/shop/black |

#### Implementation Detail (post-v4 plan)

Cannot use Path R1: the `/shop/` content's template `specific-shop-product-config` (template_id 1327) has NO `componentList` field. See [session: mr-seo-structured-data-architecture > constraints > cl-002].

Path R3 only:
1. New `mr_modules/cms/lib/jsonLd/routeScoped.js` exports `applyRouteScopedJsonLd(req, content)`:
   - Match `req.path` against `/shop/{slug}/?$`
   - Look up slug in `SHADE_SHOP_FAMILIES = Object.freeze({brown: 'Brown Shades', blonde: 'Blonde Shades', red: 'Red Shades', black: 'Black Shades'})` OR `SHOP_CATEGORIES` (gate G3.9 confirms list).
   - Build 3-item `items` array: `[Home, Shop All Products, {Label}]` with absolute https URLs (origin from `config.website.protocol + '://' + config.website.host`).
   - Call `buildBreadcrumbListJsonLd({items})`, then `pushJsonLdToContent(content, schema, {type: 'breadcrumb', source: 'routeScoped'})`.
2. Hook into `mr_modules/cms/lib/router.js pageRouter()` after content load, before `htmlRenderer.renderContent()`.
3. Verify 4 shade URLs return ≥ 1 `application/ld+json`, with parsed schema `@type === 'BreadcrumbList'`.

Existing `Breadcrumbs.vue` microdata (`itemtype="https://schema.org/BreadcrumbList"`) coexists with new JSON-LD — keep both per gate G0.5 question 2 (recommended) until SEO confirms.

---

## SECTION 4: FILE INDEX

### Documentation (external — roam, sessions, spreadsheets)

| File | Purpose |
|---|---|
| `~/.brain.d/roam-nodes/madison_reed/2026-04-29-121311-dotcompb_7929.org` | **Roam node** — co-tracks 7929+7945. v6 plan: harness-first ordering, Routes affected + Per-task verify on every code-touching task, no time-axis. UUID `bd9f004e-1c13-442a-b999-b5bdf73037c6`. |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-30-aeo_roadmap_executive_summary.org` | **AEO Roadmap wrapper roam node** — UUID `0bca0fb0-73dd-439e-949c-d594d3ad8806`. Embeds CSV as 4 org tables (top 7 actions, risks, projected impact, engineering mapping). Strategic input that drives this ticket's KPI alignment (AC11). |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-30-aeo_roadmap_executive_summary.csv` | AEO Roadmap CSV (JIRA attachment from DOTCOMPB-7929) — authoritative source companion to wrapper node above. |
| `~/.brain.d/roam-nodes/madison_reed/2025-11-16-docs_madison.org` | Doc index for MR; new APRIL 2026 entry pointing at AEO Roadmap wrapper node. |
| `~/.brain.d/roam-nodes/2025-11-18-index_madison_reed.org` | Roam index — IN CODE REVIEW lane. |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dotcompb-7929-json-ld-non-pdp.md` | **This session file** (you are here). |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/mr-seo-structured-data-architecture.md` | Architecture memory v2 (575 lines). |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/dotcompb-7942-google-sso-booking.md` | Sister session — Playwright integration pattern source. |
| URL Scope spreadsheet | https://docs.google.com/spreadsheets/d/1aSSIPzu89i6ZHD2xIPmGLfZThec7klfzrGrHkJnThbc/edit |
| Tophat CMS admin (auth required) | http://localhost:4000/#/cms/content |
| Google Rich Results Test | https://search.google.com/test/rich-results |

### Source — MR codebase (`/Volumes/dev-partition/github-madison-reed/the-code/`)

#### Files to MODIFY

| File | Change | Phase |
|---|---|---|
| `mr_modules/cms/lib/router.js` | Line 462: replace `content.faqPageJsonLd = …` with `pushJsonLdToContent(content, faqPageJsonLd, {type:'faq',source:'productRouter'})`. Add hook for `applyRouteScopedJsonLd` after content load. | P3.1, P3.9 |
| `mr_modules/cms/lib/productJsonLd.js` | Generalize `buildFaqPageJsonLd` to `({faqs, pageUrl, about})` with backward-compat overload. Replace blanket `stripTags()` on `Answer.text` with `sanitizeFaqAnswerHtml()`. Re-export from new `jsonLd/`. | P1.4, P1.5 |
| `mr_modules/controllers/lib/productCatalog.js:1995` | Verify backward-compat overload preserves behavior; no change otherwise. | P1.4 |
| `website/src/vuescripts/utilities/productJsonLdUtils.js` | Convert `updateFaqPageJsonLdScript()` to no-op + dedup (removes duplicate `#pdp-faq-json-ld` if present). | P2.3 |
| `website/src/vuescripts/utilities/productJsonLdUtils.test.js` | Update assertions to no-op behavior. | P2.3 |

#### Files to CREATE

| File | Purpose | Phase |
|---|---|---|
| `mr_modules/cms/lib/jsonLd/index.js` | Barrel re-exports + `pushJsonLdToContent` helper. | P1.1, P2.1 |
| `mr_modules/cms/lib/jsonLd/sanitize.js` | `sanitizeFaqAnswerHtml(html)` — allowlist `[a, br, ol, ul, li, p, strong, em]`. | P1.2 |
| `mr_modules/cms/lib/jsonLd/sanitize.test.js` | Sanitizer unit tests. | P1.3 |
| `mr_modules/cms/lib/jsonLd/faqPage.js` | `buildFaqPageJsonLd({faqs, pageUrl, about})` — generalized. | P1.4 |
| `mr_modules/cms/lib/jsonLd/faqPage.test.js` | Builder unit tests. | P1.6 |
| `mr_modules/cms/lib/jsonLd/breadcrumbList.js` | `buildBreadcrumbListJsonLd({items})`. | P1.7 |
| `mr_modules/cms/lib/jsonLd/breadcrumbList.test.js` | Breadcrumb builder unit tests. | P1.7 |
| `mr_modules/cms/lib/jsonLd/routeScoped.js` | `applyRouteScopedJsonLd(req, content)` — Shade Shop + categories. | P3.9 |
| `.tasks/seo-jsonld-check/{package.json, fetchers.js, extractors.js, validators.js, check.mjs, urls.txt}` | Verification CLI. v6: validators.js exports `countQuestionEntities()`; check.mjs adds `--kpi-report` flag. | P5.1–P5.4 |
| `.tasks/seo-jsonld-check/subsets/p3.X-<task>.txt` | Per-task URL subsets (P3.1 PDP, P3.2 Bundle, P3.3 locations, P3.4 locations-detail, P3.5 help, P3.6 blog, P3.7 home, P3.8 video-chat, P3.9 shade-shop, P3.10 pdp-16). Each Phase 3 task seeds + uses its own subset. | P5.1, then per Phase 3 task |
| `.tasks/seo-jsonld-check/baselines/t0.json` (and t<N>.json for v2) | KPI baselines committed alongside harness — P7.1 captures t=0 immediately after deploy. | P7.1 (v1), P7.2 (v2) |
| `.tasks/seo-jsonld-check/reports/*.json` | Gitignored harness output (per-run reports). Used by P3.10 + P3.11 + P6.3. | P5.1 |
| `.tasks/qa-automation/DOTCOMPB-7929/specs/dotcompb-7929-jsonld.spec.ts` | Playwright spec. | P5.5 |

#### Files referenced (read-only)

| File | Why |
|---|---|
| `mr_modules/cms/lib/scriptsUtils.js:30-79` | The compiler that turns `additionalScripts[]` into `header.scripts[]`. |
| `mr_modules/cms/lib/htmlRenderer.js` | Calls `addScriptsDefsInto`. |
| `mr_modules/cms/lib/vueSsr.js` | SSR plumbing — verify `content.*` reaches Pug context. |
| `mr_modules/cms/lib/productJsonLd.test.js` | Existing tests must continue to pass. |
| `website/src/views/desktop/vue-layout-ssr.pug:24-26` | The `each scriptDefinition` iterator. **DO NOT EDIT** — it already does the right thing. |
| `website/src/views/desktop/vue-layout.pug:24-26` | Mirror for non-SSR template. |
| `website/src/views/desktop/vue-layout-no-header-footer.pug:24-26` | Mirror for header-less layout. |
| `website/src/vuescripts/components/Faqs/Faqs.vue` | PDP FAQ component (async). |
| `website/src/vuescripts/components/HairColorBar/HcbIndividual/HairColorBarLocationFAQs/` | HCB Location V2 FAQ component (already covered). |
| `website/src/vuescripts/components/VideoChat/ScheduleVideoChat/VideoChatFaqs/` | VideoChat FAQ component (gate G3.8a inspection target). |
| `website/src/vuescripts/components/Shop/routes.js:21-37` | Vue Router for `/shop/:colorFamily(brown\|blonde\|red\|black)` (client-side). |
| `website/src/vuescripts/components/Breadcrumbs/Breadcrumbs.vue` | Existing microdata (NOT JSON-LD) — gate G0.5 question 2 decides fate. |
| `tophat/src/ngscripts/cms/ContentEditCtl.js:1377-1582` | Tophat auto-gen at save. Reference for understanding dp-002. |

### Tooling / external reference

| Path | Purpose |
|---|---|
| `/Volumes/dev-partition/local-projects/seo-analyzer/seo-analyzer.js` | Reference for `getRawHtml` (line 15-23) and JSON-LD cheerio extraction (line 113-116). |
| `.tasks/qa-automation/parse-acs.mjs` | Roam-node-to-Playwright parser (Option B in 7942 — custom spec preferred). |
| `.tasks/qa-automation/playwright.config.ts` | Reused as-is. |
| `.tasks/lint-changed.sh` | Local lint matching PilkoLint CI. |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-23-150000-dotcompb_7942.org` | Sister roam — Playwright pattern reference. |
| `~/.brain.d/roam-nodes/madison_reed/2026-04-27-150000-dotcompb_7942_e2e_test_report.org` | Sister E2E report — format reference for P6.4. |

### URL list — seed for `.tasks/seo-jsonld-check/urls.txt`

**Round 1 (initial 13 URLs, 2026-04-29):**
| URL (dev) | Expected schema | Currently in HTML | Phase |
|---|---|---|---|
| /product/calabria-dark-brown | FAQPage | NO (only Product) | P3.1 |
| /product/glassa | FAQPage | NO (only Product) | P3.1 |
| /colorbar/locations/towson | FAQPage | DONE (auto-gen + HairSalon) | P3.4 |
| /colorbar/locations/hillsboro | FAQPage | DONE | P3.4 |
| /schedule-video-chat | FAQPage | NO (none) | P3.8 |
| /shop/brown | BreadcrumbList | NO (none) | P3.9 |
| /shop/blonde | BreadcrumbList | NO (none) | P3.9 |
| /shop/red | BreadcrumbList | NO (none) | P3.9 |
| /shop/black | BreadcrumbList | NO (none) | P3.9 |
| /shop-all | BreadcrumbList? | NO (none) | F10.1 — gate G0.5 q1 |
| / | FAQPage? | NO (none) | gate G3.5_7 |
| /help | FAQPage? | NO (none) | gate G3.5_7 |
| /blog | FAQPage? | NO (none) | gate G3.5_7 |

**Round 2 (validated 2026-04-29 evening + 2026-04-30):**
| URL (dev) | Expected | Currently | Body | Phase |
|---|---|---|---|---|
| /bundle/perfect-pair/glassa | Product+FAQPage | NO | TBD | P3.2 |
| /bundle/perfect-pair/calabria-dark-brown | Product+FAQPage | NO | TBD | P3.2 |
| /colorbar/locations | FAQPage | YES (auto-gen) | 171 KB | DONE |
| /shop/all-hair-color | BreadcrumbList(+FAQPage?) | NO | 1.5 MB | P3.9 |
| /shop/permanent-color | BreadcrumbList(+FAQPage?) | NO | 10.7 MB | P3.9 |
| /shop/men | BreadcrumbList(+FAQPage?) | NO | 10.7 MB | P3.9 |
| /shop/color-reviving-gloss | BreadcrumbList(+FAQPage?) | NO | 10.7 MB | P3.9 |
| /shop/gloss | BreadcrumbList(+FAQPage?) | NO | 1.1 MB | P3.9 |
| /shop/hair-care | BreadcrumbList(+FAQPage?) | NO | 2.5 MB | P3.9 |
| /shop/root-touch-up-kit | BreadcrumbList(+FAQPage?) | NO | 0.8 MB | P3.9 |
| /madison-reed-reviews/glassa | none? | NO | 46 KB | OUT OF SCOPE (F10.9) |
| /product-review/clear-tone | none? | NO | 30 KB | OUT OF SCOPE |
| /product-review/glassa | none? | NO | TBD | OUT OF SCOPE |

**Total: 26 URLs.** For QA: swap base to https://dotcom.mdsnrd.com. For prod: https://www.madison-reed.com.

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### Current state (after v3 session reset, 2026-04-30 evening)

**Plan:** v13 finalized. Counter 10/48 done (21%). Roam node `* TODO TICKET TASKs` is canonical.

**Audit:** fresh report embedded at roam node `* AUDIT REPORT` (anchor `#audit-report`) between `# BEGIN-AUDIT` / `# END-AUDIT` markers. Refresh: `cd .tasks/seo-jsonld-check && npm run audit-full`.

| Status | Count | Notes |
|---|---|---|
| PASS | 1 | `/colorbar/locations` — regression sentinel |
| MISSING | 11 | 1 v1 PDP + 1 v1 Shade Shop + 9 v2 deferred |
| BROKEN-IN-CMS | 1 | `/colorbar/locations/hillsboro` — auto-detected F23 (P3.4) |
| NOT-A-CANDIDATE | 3 | `/help`, `/blog`, `/` — no FAQ section, skip per Schema.org |
| faqQuestionCountTotal | 4 | vs AEO Roadmap target 200+ |

**v1 PR scope (confirmed by audit) — 3 code tasks:**
- **P3.1** — `/product/:slug` (Tophat = not in CMS) → 3-line edit at `mr_modules/cms/lib/router.js:462`
- **P3.4** — `/colorbar/locations/:urlKey` (Tophat = YES, pipeline drops it = BROKEN-IN-CMS) → Code-bypass via P1.8 `extractFaqsFromContent` + push from HCB route handler
- **P3.9** — `/shop/{brown,blonde,red,black}` (Tophat = NO, empty additionalScripts) → new `applyRouteScopedJsonLd` helper

### Most recent session activity (top 5; full trail in Section 6)

1. **2026-04-30 23:30 — v13 harness upgrade.** Built `cms-check.mjs` (MongoDB via docker exec, parent-fallback for slugged URIs). Added `detectJsonValidityIssues` (multi-object concatenation, trailing commas, parse errors). New `BROKEN-IN-CMS` status auto-classifies F23 case. New `Tophat?` column in tables. Per-URL details show CMS scripts breakdown. Re-ran: 1 PASS, 11 MISSING, 1 BROKEN-IN-CMS, 3 NOT-A-CANDIDATE. v1 fix plan reduced to 3 code tasks.
2. **2026-04-30 23:00 — v12 Pug-interpolation accuracy refinement.** Verified `req.metaData` population path for `/colorbar/locations/<slug>`: `views.js:1593` → `htmlRenderer.js:179` shallow merge → `scriptsUtils.js:71-78` Pug source construction → `pug.compile()` line 134 silent failure on error. Added Section 11 to roam node JSON-LD MECHANISM REFERENCE (Pug-interpolated scripts deep-dive, 5 subsections, R2-vs-R3 decision matrix).
3. **2026-04-30 22:30 — v11 JSON-LD mechanism deep-dive doc.** Wrote canonical 12-section reference in roam node (anchor `#json-ld-mechanism-reference`) — pipeline overview, 2 storage levels, Tophat UI sources, full script descriptor schema, ASCII pipeline diagram, interpolation context, 3 verified examples, decision tree, gotchas, file:line refs, Tophat checkbox cheat-sheet.
4. **2026-04-30 22:00 — v10 unified mechanism.** Dropped R1/R2/R3 path classification. Every URL fixed via route-handler push. Phase B removed. P1.8 added (`extractFaqsFromContent`). Idempotent push protects existing R1 emissions.
5. **2026-04-30 21:30 — v9 pattern reduction + FAQ-content gating.** urls.txt 25→16. v1 active testing = 3 representative URLs. Bundles dropped (no FAQ content). P3.2 → VERIFY-ONLY.

### Pending — next action (v13 audit-confirmed)

**Fire G1.1** (module location). v6 default = *"New `jsonLd/` subdir"*. Then scaffold Phase 1 (P1.1–P1.8: builders + sanitizer + `extractFaqsFromContent` + unit tests).

**Then sequence:**
1. Phase 1 (P1.1–P1.8) — pure builders, sanitizer, FAQ extractor, unit tests.
2. Phase 2 (P2.1, P2.3) — push helper + client-side PDP cleanup (P2.3 AFTER P3.1).
3. Phase 3 — three route-handler push tasks (P3.1, P3.4, P3.9) using harness against `subsets/p3.X.txt` per task.
4. Phase 6 — lint, tests, harness `--fail-on-missing`, Rich Results Test on QA, PR push, merge.
5. P7.1 (post-deploy) — capture `baselines/t0.json`.

**v2 follow-up ticket:** P3.5–P3.8 (Tophat editor work), SHOP_CATEGORIES expansion (7 categories), P3.10 chase (if surfaced), P7.2/P7.3 (KPI checkpoints).

### Where to resume

If user says **"start Phase 1" / "begin builders"**: fire G1.1 → scaffold `mr_modules/cms/lib/jsonLd/` per their answer (subdir per default).

If user says **"start P3.1" / "fix PDP"**: confirm Phase 1+2 done → 3-line edit at `mr_modules/cms/lib/router.js:462` → run harness against `subsets/p3.1-pdp.txt`.

If user says **"start P3.4" / "fix HCB locations"**: confirm Phase 1+2 done → write `extractFaqsFromContent` (P1.8) → push from HCB route handler → run harness against `subsets/p3.4-locations-detail.txt`.

If user says **"start P3.9" / "fix shade shop"**: invoke G3.9 to confirm SHADE_SHOP_FAMILIES → write `mr_modules/cms/lib/jsonLd/routeScoped.js` → hook into `pageRouter()` → run harness against `subsets/p3.9-shade-shop.txt`.

If user says **"validate current state" / "re-run audit"**:
```sh
cd /Volumes/dev-partition/github-madison-reed/the-code/.tasks/seo-jsonld-check
npm run audit-full   # CMS check + HTML audit
```

If user asks to **inspect any URI's CMS state**: see [session: mr-seo-structured-data-architecture > reusable-references > rr-004]. For automated, use `cms-check.mjs` (rr-005).

If user asks **"what's the mechanism"**: see roam node `* JSON-LD MECHANISM REFERENCE` (anchor `#json-ld-mechanism-reference`) — 12 sections covering everything.

If user asks for **status summary**: 10/48 done (21%). Audit confirms 3 v1 code tasks: P3.1 + P3.4 + P3.9. Critical path: G1.1 → Phase 1 → Phase 2 → P3.1/P3.4/P3.9 → Phase 6 → P7.1.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table. Newest first. See `~/.claude/skills/session-reset/rules/activity-log.md`.

| Datetime         | Duration | Type                  | Reference        | Description |
|------------------|----------|-----------------------|------------------|-------------|
| 2026-05-01 00:30 | 1h       | session-reset         | this             | v3 reset (detailed). Compacted v8-v13 plan refinements + Pug-interpolation accuracy work + harness upgrades. Replaced Section 5 with concise current state (1 PASS / 11 MISSING / 1 BROKEN-IN-CMS / 3 NOT-A-CANDIDATE; 3 v1 code tasks confirmed). Added Section 2.3 decisions 15-20 covering v8-v13. Updated Section 1.1 to reflect v10 unified mechanism (R1/R2/R3 collapsed). Updated compaction sources timeline. |
| 2026-05-01 00:30 | —        | architecture-extract  | this             | Architecture memory v2 → v3 (575 → 788 lines). Added: dp-004 (idempotent route-handler push), dp-005 (server-side port of CMS auto-gen logic via `extractFaqsFromContent`), cl-003 (slugged-child static-script gap, formalizing F23), rr-005 (BROKEN-IN-CMS detection methodology via `cms-check.mjs`). |
| 2026-04-30 23:30 | 1h       | implementation+audit  | DOTCOMPB-7929    | **v13 harness upgrade — CMS state inspection + strict JSON validity + BROKEN-IN-CMS classification**. Built `cms-check.mjs` (queries MongoDB via `docker exec mr-mongo mongosh --eval`; parent-fallback walks path components so `/colorbar/locations/hillsboro` resolves to content_id 2350 via parent `/colorbar/locations/`). Cache file `cms-state-cache.json` resolves 14/16 URIs. Updated `extractors.mjs` with `detectJsonValidityIssues` (regex-checks multi-object concatenation `}\s*{`, trailing commas before `}` or `]`, JSON parse errors). Updated `check.mjs` with new `BROKEN-IN-CMS` status (Tophat HAS expected schema but raw HTML lacks it = F23 case auto-detected) + new `Tophat?` column showing script counts + per-URL details with CMS scripts breakdown (level/flags/types/bytes). New npm scripts: `cms-check`, `audit`, `audit-full`. Re-ran full audit: 1 PASS, 11 MISSING, **1 BROKEN-IN-CMS automatically classified** (`/colorbar/locations/hillsboro` — confirmed F23 without manual investigation), 3 NOT-A-CANDIDATE. Confirmed PDP routes `/product/:slug` are not-in-CMS (code-only). Embedded report regenerated in roam node. |
| 2026-04-30 23:00 | 0.5h     | research+documentation | DOTCOMPB-7929 | **v12 Pug-interpolation accuracy refinement**. User provided full content of content_id 2350 hand-authored Pug script (HairSalon/Review). Investigated monorepo to verify exact mechanism. Trace: route handler `views.js:1593-1607` populates `req.metaData = (region.locationSummaries).find(...)` for `/colorbar/locations/:urlKey` → `htmlRenderer.js:179` shallow-merges `locals.metaData = {...content.metaData, ...req.metaData}` → `scriptsUtils.js:71-78` wraps editor text in `script(type="application/ld+json")` and indents lines by 2 spaces → `pug.compile(pugSource, locals)` at line 134 (silent failure on error → `_htmlScript=''`). Section 6 (Interpolation context) rewritten with the verified merge logic + table of routes populating `req.metaData` vs not. New Section 12 added (5 subsections, ~150 lines): exact Pug source construction, Pug syntax cheat-sheet for ld+json scripts (`if`/`unless`/`for`/`|`/`#{...}`/`-`/`//-`), production reference dissected with known JSON-validity bugs flagged out-of-scope, worked recipe for `/shop/<color>` BreadcrumbList R2 alternative, R2-vs-R3 decision matrix (7 dimensions). P3.4 task now references this section. Sections renumbered: old 12 → new 13. |
| 2026-04-30 22:30 | 1h       | research+documentation | DOTCOMPB-7929 | **v11 JSON-LD mechanism deep-dive documentation**. User direction: deeply understand and document the additionalScripts mechanism — formats, examples, recipe for adding ANY JSON-LD. Verified `/shop/**` template state via DB: template `specific-shop-product-config` (template_id 1327 v11) has `additionalScripts: []` (empty); content `/shop/` (content_id 2686 v12) has `renderOptions.additionalScripts: []` (empty). Both levels EXIST and ACCEPT new entries — editor CAN add scripts via Tophat for /shop/ today. Wrote new top-level section in roam node `* JSON-LD MECHANISM REFERENCE` (~250 lines, 11 subsections) covering: 3 patterns (R1/R2/R3) with file:line refs, 2 storage levels, Tophat UI source files, full script descriptor field schema, request-time pipeline ASCII diagram with silent-failure mode call-out, interpolation context (what's in `locals` per route type), 3 verified production examples (R1 auto-gen FAQPage on /colorbar/locations, R2 hand-authored Pug HairSalon on /colorbar/locations/, R3 code-push for PDP per P3.1), decision tree for adding ANY Schema.org @type, common gotchas table, all file:line refs, Tophat checkbox cheat-sheet. Plan now references this doc from Execution Constraints. Session 1.2 reduced to short reference + pointer to roam doc. |
| 2026-04-30 22:00 | 0.5h     | refinement            | DOTCOMPB-7929    | **v10 plan refinement (unified mechanism)**. User feedback: R1/R2/R3 path distinction was confusing without execution benefit. Dropped the path classification entirely. New rule: every URL needing JSON-LD in this PR is fixed via route-handler push (`pushJsonLdToContent`). Code reads data from Magento product / CMS componentList / URL params, builds via Phase 1 builders, pushes; pipeline emits. Phase B (manual Tophat validation) REMOVED — no longer required. New helper P1.8: `extractFaqsFromContent(content)` ports `ContentEditCtl.getAdditionalScriptForFAQs` server-side; P3.4 uses it to bypass the slugged-child gap entirely (no investigation needed). Idempotent push protects existing R1 emissions. Counter 10/49 → 10/48 (Phase B removed = -2 tasks; P1.8 added = +1). |
| 2026-04-30 21:30 | 0.5h     | refinement            | DOTCOMPB-7929    | **v9 plan refinement (pattern reduction + FAQ-content gating)**. Per-URL grep on raw HTML to count FAQ question fields → drop URLs without FAQ content from FAQPage candidacy. Pattern reduction: one representative per template pattern (PDP, HCB-detail, Shade Shop). urls.txt: 25 → 16 URLs (v1 active = 3 + reference = 1; v2 deferred = 12). Dropped: /product/glassa (PDP duplicate), bundles (no FAQ content), /colorbar/locations/{pembrokepines, towson, bethesda} (HCB duplicates), /shop/{blonde, red, black} (shade-family duplicates). P3.2 downgraded to VERIFY-ONLY. Phase 6 final QA can expand back to full set; this audit set is for development decision-making. Saved 36% URL volume in active testing while preserving pattern coverage. |
| 2026-04-30 21:00 | 1.5h     | implementation+refinement | DOTCOMPB-7929 | **v8 plan refinement (audit-first execution)**. Built and ran the verification harness AHEAD of original Phase 5 schedule (P5.1–P5.4 marked DONE). Final dep set: yargs + chalk only — dropped axios (Node 18 + new undici lacks `File` global) and cheerio (also pulls undici transitively); replaced with native `fetch` + AbortController + regex extractor for `<script type="application/ld+json">` blocks. Created `.tasks/seo-jsonld-check/{package.json, fetchers.mjs, extractors.mjs, validators.mjs, check.mjs, urls.txt, .gitignore}` plus subdirs `subsets/, reports/, baselines/, fixtures/`. urls.txt seeded with 25 URLs tagged scope/phase/path. First audit run captured `reports/audit.org` (1 PASS, 24 MISSING, faqQuestionCountTotal=4 vs AEO target 200+) and embedded into roam node between `# BEGIN-AUDIT` / `# END-AUDIT` markers. Plan restructured: Phase A (audit + classify) and Phase B (user Tophat manual validation) inserted between Phase 0 and Phase 1. v8 sequence: Phase 0 → A → B → 1 → 2 → 5 (re-runs) → 3 (code only when Tophat is exhausted) → 6 → 7. Code is now the LAST RESORT, not the default. |
| 2026-04-30 20:00 | 1h       | research+refinement   | DOTCOMPB-7929    | **v7 plan refinement (Tophat configuration + slugged-child gap)**. Live verification: `/colorbar/locations/pembrokepines` + `/towson` both emit only HairSalon, never FAQPage, despite content_id 2350 having both scripts in `additionalScripts`. Root-cause unknown — investigation deferred to P3.4 with 4-step diagnostic tree. F23 added: full Tophat checkbox reference (type=ld+json, isUrl=false, inHeader=true, forceInterpolation=false-for-static/true-for-Pug, addBodyLoadScript=false), full Path R1 auto-gen mechanism trace through `ContentEditCtl.js:getAdditionalScriptForFAQs` lines 1380-1537 (componentList walk + `addFaqMetadata` flag + questions/answers/faqs/faqMetadataFields field-shape options + merge-into-single-FAQPage + `generatedAutomatically: true` semantics), full request-time pipeline trace through `scriptsUtils.js:addScriptsDefsInto` (Pug compile with silent-failure on error → empty `_htmlScript`). P3.4 reopened (was DONE-incorrectly) with 4-option fix tree + risk-gate downgrade-to-v2 path. P3.5–P3.8 recipes upgraded with concrete checkbox state. Tophat UI source files identified in monorepo: `tophat/src/views/ngpartials/cms/content/edit.pug:~600` + `tophat/src/ngscripts/cms/ContentEditCtl.js`. |
| 2026-04-30 19:00 | 0.5h     | refinement            | DOTCOMPB-7929    | **v6 plan refinement (execution discipline)**. Dropped temporal framing entirely (no day targets, no 14-day window, no Day-7 re-eval gate). Reordered execution so Phase 5 (verification harness) lands BEFORE Phase 3 (per-route wiring); per-task verify on every Phase 3 task is now `node .tasks/seo-jsonld-check/check.mjs --urls subsets/<task>.txt --fail-on-missing` against just the routes that task touched. Added explicit `Routes affected:` and `Per-task verify:` lines to every code-touching task in Phases 1, 2, 3, 5, 6, 7. Reframed v1/v2 as scope labels for "this PR / follow-up PR" without time framing. Decision Gates table: tier column = "Scope" not "Tier"; default-selection column drops "Day 1 standup"-style temporal language. Phase 7 KPI checkpoints (P7.2/P7.3) reframed as "cadence per PM" rather than t+14/t+30/t+90. |
| 2026-04-30 18:30 | 0.5h     | refinement            | DOTCOMPB-7929    | **v5 plan refinement (AEO Roadmap-driven)**. Added AC11 (FAQ Questions Indexed KPI ≥ 200). Inserted AEO Roadmap Constraints subsection at top of TODO TICKET TASKs (14-day window, KPI alignment, cross-team coordination with Content's Answer-First reformatting). Added v1 SHIP / v2 DEFER scope tiers per task. Decision Gates table now shows v5 default per gate. Phase 5 P5.3/P5.4 extended to count Question entities + emit `kpi.json`. Phase 7 added (3 tasks: deploy-day baseline, t+14/30/90 checkpoints, Search Console correlation). Task counter 4/41 → 4/44. |
| 2026-04-30 18:00 | 0.3h     | documentation         | DOTCOMPB-7929    | AEO Roadmap CSV (JIRA attachment) imported. Moved CSV → `~/.brain.d/roam-nodes/madison_reed/2026-04-30-aeo_roadmap_executive_summary.csv`. Created wrapper roam node `2026-04-30-aeo_roadmap_executive_summary.org` (UUID `0bca0fb0-73dd-439e-949c-d594d3ad8806`) with 4 org tables (top 7 actions, risks, projected impact, engineering mapping). Registered in `docs_madison.org` April 2026 section. Cross-linked from 7929 roam node TICKET CONTEXT (strategic alignment para) + RELEVANT LINKs. KPI surfaced: FAQ Questions Indexed ~0 → 200+ over 90 days. |
| 2026-04-30 17:00 | 1.5h     | session-reset         | this             | v2 reset; merged v3+v4 roam refinement; arch memory v2 (ad-001 superseded; ad-005, ad-006, dp-002, dp-003, cl-002, rr-003, rr-004 added) |
| 2026-04-30 17:00 | —        | architecture-extract  | this             | Extracted 7 new entries (1 supersession, 2 ad, 2 dp, 1 cl, 2 rr) into mr-seo-structured-data-architecture.md (273→575 lines) |
| 2026-04-30 16:00 | 1h       | refinement            | DOTCOMPB-7929    | v4 plan refinement: added 11 inline ⚙️ GATE blocks for AskUserQuestion + top-level Decision Gates index |
| 2026-04-30 14:30 | 1.5h     | refinement            | DOTCOMPB-7929    | v3 plan refinement: F18-F22 findings; DEV-AC3/4 rewritten; Phase 2 collapsed (5→3 tasks); Phase 3 reframed (11 tasks, Path R1/R2/R3 designations); Phase 4 merged into P3.9 |
| 2026-04-30 13:30 | 1h       | research              | DOTCOMPB-7929    | Live MongoDB audit; verified unified additionalScripts pipeline; documented Tophat auto-gen contract; resolved F10.6/F10.7/F16 |
| 2026-04-30 11:30 | 1.5h     | research              | DOTCOMPB-7929    | 4 parallel Explore agents — Tophat CMS UI, CMS data models, SSR plumbing, per-route audit |
| 2026-04-30 10:30 | 0.5h     | documentation         | this             | Loaded existing roam node + session file; oriented on prior work |
| 2026-04-29 19:00 | 1h       | session-reset         | this             | First reset; extracted architecture memory v1 (ad-001 through ad-004, dp-001, cl-001, rr-001, rr-002) |
| 2026-04-29 17:00 | 2h       | refinement            | DOTCOMPB-7929    | Per-phase compliance briefing against mr-dotcom-dev + code-review skill rules; added * COMPLIANCE & GUIDELINE CHECKs section to roam node |
| 2026-04-29 17:00 | 1.5h     | research              | DOTCOMPB-7929    | Validation Round 2: enumerated 11 routes.js; F11–F17 added; 5 new Phase 3 tasks (P3.10–P3.14) |
| 2026-04-29 14:00 | 3h       | refinement            | DOTCOMPB-7929    | Refined ACs to 10 strict + 10 DEV-AC; live validation Round 1; Phase 0 validation report F1–F10 |
| 2026-04-29 12:00 | 1h       | documentation         | DOTCOMPB-7929    | Created roam node + index entries + session file scaffold (UUID bd9f004e-1c13-442a-b999-b5bdf73037c6) |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
