# Tophat Tools — Architectural Guide

This skill packages the operational toolkit for working with Madison Reed's Tophat CMS — the Mongo-backed content/template/experiment system that drives most of the consumer site's URLs. It is the second skill in the JSON-LD/CMS family, complementing `seo-web-quality` (which focuses on SEO standards) by providing the *executable* surface for inspecting, auditing, and (with care) mutating CMS state.

## Why this skill exists

Every CMS investigation in this codebase historically followed the same painful path:

1. The user asks "what does this URL render?" or "what's bound to that experiment?"
2. The AI writes a one-off `docker exec mr-mongo mongosh cms --eval '…'` query.
3. The AI parses the (BSON-flavoured, pretty-printed) output by eye and pastes a follow-up query.
4. Repeat 4-6 times to assemble the picture.
5. None of the queries are reusable; the next investigation starts from scratch.

That tax repeats in every CMS-adjacent ticket: DOTCOMPB-7929 (JSON-LD), DOTCOMPB-7942 (Booking SSO), DOTCOMPB-8120 (Site Revolution Marketing LP). The fix is a stable, scripted toolkit:

* **One shared mongosh wrapper** — every script talks to the DB the same way (`lib/mongo.mjs`).
* **One shared CLI convention** — every script takes `--container`, `--db`, `--version`, `--variation`, dry-run/`--confirm`.
* **Pure-read scripts that print structured JSON** — `jq`-pipeable; no need to re-parse free-form mongosh output.
* **Mutation scripts that always back up + dry-run first** — see `rules/safety-and-conventions.md`.

The skill is built around the **two-agent model** the rest of the user's skill library follows:

* `SKILL.md` is keyword-rich and tiny — the Skill Analyzer scans it to decide which rule files to load for the Worker.
* `rules/*.md` are the comprehensive encyclopedia entries — the Worker reads them when actually doing work.
* `scripts/` holds executable Node scripts that the Worker shells out to.

## What the scripts do (in 30 seconds)

### Inspect (read-only)

`inspect-content`, `inspect-content-by-uri`, `inspect-template`, `inspect-experiment`, `get-component-list`, `get-template-jade`, `find-template-usage`, `get-cms-additional-scripts`, `inspect-jsonld`, `find-cms-component-code`, `find-route` — eleven scripts that, together, answer almost every "what does the CMS think?" question in a single command.

### Mutate (writes — always with `--confirm`)

`set-experiment-status`, `set-variant-weight`, `add-jsonld-script`, `migrate-content-experiment`, `backup-content`, `restore-content` — six scripts that handle the most common write operations. All five non-backup writers default to dry-run, snapshot before mutating, and emit verification output after.

The *one* canonical inline-migration example is `.tasks/DOTCOMPB-8120/cms-migrate.mjs` in the MR repo. The skill's `migrate-content-experiment.mjs` is the generalised version of that script — config-file driven, idempotent, with the same backup convention.

## How the pieces relate

```
                    ┌──────────────────────────────┐
                    │   lib/mongo.mjs              │  shared mongosh wrapper
                    │   (parseArgs, mongoJson,     │
                    │    findContentById, etc.)    │
                    └───────────────┬──────────────┘
                                    │
        ┌───────────────────────────┼───────────────────────────┐
        │                           │                           │
        ▼                           ▼                           ▼
┌──────────────────┐      ┌──────────────────┐      ┌──────────────────────┐
│  Inspect scripts │      │  Mutate scripts  │      │  Code locator        │
│  (read-only)     │      │  (--confirm)     │      │  scripts (FS only)   │
│                  │      │                  │      │                      │
│  • inspect-*     │      │  • set-*-status  │      │  • find-cms-…-code   │
│  • get-*         │      │  • set-…-weight  │      │  • find-route        │
│  • find-template-│      │  • add-jsonld-*  │      │                      │
│    usage         │      │  • migrate-*     │      │  No Mongo touch.     │
│                  │      │  • backup/restore│      │  Pure grep + find.   │
└──────────────────┘      └──────────────────┘      └──────────────────────┘
```

The third column (code locator scripts) is intentionally *not* tied to Mongo. Renaming a mixin_key, finding the Vue file behind a CMS tag, or locating an Express route handler all happen at the filesystem level. Keeping them mongo-free makes them runnable in any clone of the codebase, without Docker.

## Token economics

The skill is designed to cut tokens across two dimensions:

1. **Script over query.** A scripted inspection returns a single JSON tree the AI can read once. A mongosh-by-mongosh investigation returns 4-6 paginated outputs the AI must summarise into prose.
2. **Rule files load on demand.** The Skill Analyzer's keyword match keeps the Worker's context lean. If a session asks "show me the experiments", only `rules/experiment-management.md` loads — not the whole skill.

The rule-file boundaries reflect this:

| Rule file                | Loaded when the Worker is asked about…                                           |
|--------------------------|----------------------------------------------------------------------------------|
| `cms-data-model.md`      | Schemas, collections, the runtime-vs-doc-id trap, denormalisation drift.         |
| `inspection-scripts.md`  | "Show me the data" tasks — read-only investigations.                             |
| `experiment-management.md`| A/B status changes, weight tuning, retiring experiments.                        |
| `json-ld-management.md`  | JSON-LD authoring, R1/R2/R3 paths, audit + drift detection.                       |
| `content-migration.md`   | Re-binding a content_id between experiments — the DOTCOMPB-8120 pattern.        |
| `code-locator-scripts.md`| Translating between CMS metadata (mixin_key, URI) and the codebase.              |
| `safety-and-conventions.md`| Any mutation — what dry-run looks like, where backups live, idempotency rules.|

## Extension points

Things the skill does NOT yet ship — add them when needed, following the same conventions:

* **`create-subtemplate.mjs`** — currently inlined in the canonical `cms-migrate.mjs`. Generalise into a standalone script when the next migration hits.
* **`set-audience-target.mjs`** — audience-targeted variations (region, customer-state). Tophat-only for now.
* **`schema-validate-jsonld.mjs`** — pull `validators.mjs` from `seo-analyzer/jsonld-check/` for richer schema validation than `inspect-jsonld.mjs`'s syntactic checks.
* **`replicate-to-tophat.mjs`** — automate the Tophat-publish step Carley does manually. Probably out of scope (requires Tophat auth flow + UI automation).

## What this skill is NOT

* **Not a Tophat replacement.** Production changes go through Tophat. These scripts are local diagnostic / verification tools.
* **Not an SEO auditor.** For full per-URL audit + per-variation probing + reporting, the JSON-LD harness at `/Volumes/dev-partition/local-projects/seo-analyzer/jsonld-check/` is canonical. This skill borrows its primitives (`extractors`, `experiments`, `cms-link`) but doesn't try to replace the harness.
* **Not a content authoring tool.** The componentList and settings of a contentVersion are best edited in Tophat. This skill changes them programmatically *only* when reproducing a specific migration locally.

## When to invoke this skill

Trigger keywords (load `SKILL.md` here):

* "tophat", "cms", "mongo cms", "contentVersion", "mixin_key", "experiment", "variation", "weight", "additionalScripts", "JSON-LD"
* "content_id", "templateVersion", "production_content", "stage_content"
* "what's bound to this experiment?", "where is this CMS template?", "how is /shop/blonde rendered?"
* "migrate content", "rebind experiment", "retire experiment"
* "what does the SEO crawler actually see?"

If the question is purely about Tophat *the editor UI* (button positions, workflow), this skill is the wrong tool — refer the user to the Tophat docs / a screenshare. If the question is about the underlying data store and how it maps to the rendered page, this skill is the answer.
