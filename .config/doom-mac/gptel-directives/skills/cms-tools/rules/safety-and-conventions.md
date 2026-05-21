---
title: Safety, Dry-Run, Backup & Idempotency Conventions
impact: HIGH
impactDescription: The non-negotiable safety contract every mutation script in this skill must obey. Without a dry-run gate every script becomes a foot-gun; without backups, every mistake is irrecoverable; without idempotency, every re-run risks duplicate writes.
tags: safety, dry-run, confirm, backup, restore, idempotent, atomic, rollback, audit, no-shipping, db-write-disclaimer, dev-server-cache, prod-replication
---

# Safety, Dry-Run, Backup & Idempotency Conventions

Every mutation script in this skill must obey **all five** of the rules below. Read scripts use only rules 1, 4, 5.

## 1. Dry-run by default; `--confirm` to apply

Every script that writes performs a dry-run when run without `--confirm`:

* Print the inputs (resolved IDs, target collection, intended diff).
* Print the operations that *would* run.
* Exit with code `0` and no DB writes.

`--confirm` is the only flag that flips writes on. Never default-on writes. Never auto-write based on a heuristic. The user must be explicit every time.

```sh
node set-experiment-status.mjs 504 Paused           # dry-run, prints plan
node set-experiment-status.mjs 504 Paused --confirm # actually writes
```

## 2. Backup before write

Before any destructive write, snapshot the affected docs to disk under `./cms-backups/<scope>/<id>/<ISO-stamp>.json`. Scopes:

| Scope         | Path                                         | What's backed up                                                  |
|---------------|----------------------------------------------|-------------------------------------------------------------------|
| `content`     | `./cms-backups/<contentId>/<stamp>/snapshot.json` | content + every contentVersion + production_content + stage_content + relevant experiments + counters |
| `experiment`  | `./cms-backups/experiment/<expDocId>/<stamp>.json`   | The experiment doc                                              |
| `jsonld`      | `./cms-backups/jsonld/<contentId>/<stamp>-<variation>.json` | The contentVersion record before the JSON-LD push           |

Backups are JSON. Use `restore-content.mjs` (for content scope) or `mongoEval` directly (for experiment scope) to roll back.

## 3. Idempotent writes

Re-running a script with the same inputs must converge to the same final state — never duplicate, never drift. Concrete patterns used here:

* **Migration:** `db.contentVersion.deleteMany({content_id, version: toVersion})` before re-inserting. A re-run with the same target version drops the previous attempt and replays cleanly.
* **Status / weight changes:** check the current value before writing; exit early if already in desired state.
* **JSON-LD push:** dedupe by content + body + variation if you re-run. (The current `add-jsonld-script.mjs` does *not* dedupe — it always appends. This is intentional for diagnostic use; for production-safe pushes, extend it to check for an existing entry first.)

## 4. Direct DB writes are NOT a shipping mechanism

Any write made by these scripts is **local-only**. Production fix path = Tophat editor (or Phase C code residue when CMS isn't usable). Concretely:

* The dev server in-process content cache may not pick up raw writes until restart or a Tophat publish event.
* Tophat's publish flow re-derives `production_content` / `stage_content` from `contentVersion`. A direct write to `production_content` will be wiped on the next publish.
* Tophat auto-gen (R1 path) wipes any `additionalScripts` entry with `generatedAutomatically: true` on every save. Never set that flag from a route-handler push or a script.

Use these scripts for:

* Local diagnosis (proving a Pug template compiles + emits).
* Validating a planned Tophat change before Carley applies it on staging/prod.
* Reproducing a production issue locally to debug.

Do **not** use them to:

* Ship production-bound JSON-LD.
* "Patch around" a Tophat editor save that won't take.
* Modify production data in any environment.

## 5. Container + DB defaults are explicit

Every script accepts `--container <name>` and `--db <name>`. Defaults are `mr-mongo` + `cms`. Don't hardcode the cms DB name in script bodies — call `setMongoTarget()` from `lib/mongo.mjs` or rely on the parsed flags.

This makes the skill portable to environments where the container is named differently (e.g., a CI / staging copy where you've spun up a parallel mongo) without code changes.

## Audit fields on writes

When inserting or pushing entries, set:

* `created_by` / `updated_by`: `"tophat-tools/<script-name>"`
* `created_at` / `updated_at`: `new Date()`
* For pushed `additionalScripts` entries, optionally include `source: "tophat-tools/add-jsonld-script.mjs"` so a future audit can identify scripted additions.

`generatedAutomatically: false` for any non-R1 entry — never `true` from a script.

## Verification rule

After any write that affects rendered HTML, verify against the page itself, not Mongo:

```sh
# What changed on disk vs what the SEO / HTML renderer actually serves?
node inspect-jsonld.mjs <url> --variation <key>
curl -sS <url> | grep -E "data-cms-tid|data-cms-mixin-key" | head -20
```

If the rendered HTML disagrees with Mongo, restart Vite (`npm run dev`) and re-probe before assuming the write didn't land.
