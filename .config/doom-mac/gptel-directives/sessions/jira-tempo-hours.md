# jira-tempo-hours — Session Context

<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the `jira-tempo-hours` tool — Kyo's local Node CLI that automates filling Tempo worklogs on AgileEngine's Jira (`agileenginecloud.atlassian.net`) for billing Madison Reed work. It is loaded at the start of every conversation about the tool to give the AI full context without re-discovering anything.

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Rules, conventions, and hard constraints for the tool. | Before any task. Mandatory constraints. |
| **2. Session Overview** | Scope, decisions, pending work. | When starting a new task. |
| **3. Implementations** | Per-command detail: what was built, where, decisions. | When resuming or referencing existing work. |
| **4. File Index** | Quick-reference file path table. | When reading, editing, or locating files. |
| **5. Last Interaction** | Short-term memory: last work, pending, resume points. | At conversation start — entry point. |
| **6. Activity Log** | Datetime-stamped, append-only audit table. | When you need exact "what was done when". |

**Operational Rule:** The latest user request is at the very bottom of the conversation. Address only that request unless explicitly asked to revisit prior steps.

**Authoritative companion file:** `/Volumes/dev-partition/github-madison-reed/jira-tempo-hours/SESSION.org` is the project-local, exhaustive (~40KB) org-mode version maintained inside the tool's repo. This central markdown file mirrors the same structure for cross-session indexing and quick reference; the org file remains the source of truth for in-project work.

**Cross-session references** use `[session: filename > section-N.M]` syntax — see `~/.claude/skills/session-memory/rules/reference-syntax.md`.

**Compaction sources:**
- 2026-04-29 — Phase 1 (scaffold, Tempo client, `whoami`/`history`/`export`/`push`).
- 2026-04-30 — Phase 2 (calendar integration, scheduler).
- 2026-04-30 (late) — Phase 3 (theme classifier, render-org.js, MR-4 single-issue resolution, Activity Log infrastructure).
- 2026-05-15 — Central session file created from local `SESSION.org` v3 (Phase 3 complete; first real push for May 1–15).
- 2026-05-29 — Updated after second biweekly push (May 16–31 cycle confirmed done).

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> Apply these rules to every task in this session. No external skill is loaded specifically for this project — these are the project's authoritative rules.

### 1.1 Architecture

*   **Tempo REST API v4 only.** Endpoint base: `https://api.tempo.io/4`. Never use Playwright/Puppeteer/DOM scraping — the Atlassian iframe is fragile.
*   **Two-stage pipeline:** `plan` generates `week.json`, the user tweaks it by hand, `push` posts to Tempo. The two stages are always separate scripts.
*   **Never submit the timesheet from code.** The scripts create worklogs only; the user clicks **Submit** in the browser. Hard product line — not a TODO.

### 1.2 Runtime & Tooling

*   Node ≥ 18, ESM (`type: "module"` in `package.json`).
*   Use **native `fetch`** — no `axios` or `node-fetch` dependency.
*   CLI is built with `commander`. Env vars loaded with `dotenv`.
*   Keep the dependency list minimal. Add a dep only if a feature genuinely needs it. Current deps: `commander ^12.1.0`, `dotenv ^16.4.5`, `node-ical ^0.20.1`.

### 1.3 Auth & Secrets

*   Three secrets in `.env`: `TEMPO_API_TOKEN`, `ATLASSIAN_API_TOKEN`, `ATLASSIAN_ACCOUNT_ID`. Plus `GOOGLE_CALENDAR_ICS_URL` for the calendar.
*   **Always wrap token values in single quotes** in `.env`. Single quotes are literal in dotenv — no `$VAR` expansion, no `#` comment parsing, no escaping. Tokens contain `$`, `#`, `/`, `+`, `&` regularly. Documented in `.env.example` and `README.org`.
*   Tempo uses `Authorization: Bearer <token>`. Jira uses HTTP Basic (`email:token` base64).
*   `ATLASSIAN_ACCOUNT_ID` is fetched once via `npm run whoami` and pasted into `.env` by the user.

### 1.4 Tempo API Specifics

*   Worklog creation requires **numeric `issueId`** (not `issueKey`). Look up via `GET /rest/api/3/issue/{key}` and cache per run. Plan stamps every entry with both fields; push prefers `issueId` directly (no Jira lookup needed).
*   Worklog payload: `authorAccountId`, `issueId`, `timeSpentSeconds`, `billableSeconds`, `startDate`, `startTime`, `description`, `attributes`.
*   Listing supports paging via `metadata.next`. Always follow pagination.

### 1.5 CLI Conventions

*   Every destructive command must support `--dry-run`.
*   Date flags use `YYYY-MM-DD` format.
*   Default `--from` for `history` is 60 days ago; `--to` defaults to today.
*   `plan` and `export` require explicit `--from`/`--to` (no defaults — they produce files).
*   Errors bubble through a top-level `try/catch` in `src/index.js` that prints `Error: <message>` and sets `process.exitCode = 1`.

### 1.6 Documentation Format

*   **All documentation in this project uses org-mode (`.org`)**. The user is a Doom Emacs user — markdown was explicitly removed and replaced with org.
*   Code blocks: `#+begin_src bash` ... `#+end_src`.
*   Tables: org pipe tables.
*   Inline code and identifiers: `=verbatim=` (equals signs) in the project's own docs, backticks in this central markdown index.
*   **Exception:** This central session file is markdown because the central sessions folder follows markdown convention.

### 1.7 Sources for the `plan` Command

*   **Roam nodes** live at `~/.brain.d/roam-nodes/madison_reed/*.org`. Parser reads `#+TITLE`, `#+SUBTITLE`, `#+LAST_UPDATE`, `#+STORY_POINTS`. Ticket key is extracted from the title via `/[A-Z]+-\d+/`.
*   **Sessions** live at `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions`. Parser reads `SESSION.md` inside each subdirectory and `*.md` at root.
*   Both parsers filter by `mtime` falling inside the requested date range.

### 1.8 Time-Slot Scheduler

*   `src/scheduler.js` places fixed events on a per-day timeline, then fills remaining intervals with dev work.
*   Day envelope: `workHours.earliestStart` → `workHours.preferredEnd` (default `08:00`–`17:00`). Spills into `latestEnd` (`18:00`) only when meetings push total dev time past `preferredEnd`.
*   Lunch (`12:30`–`13:00`) is carved out by default (`skipByDefault: true`) unless a meeting overlaps it.
*   Meetings (from `meetings.json` or live ICS) are placed first at their actual times.
*   Per-day budget = `hoursPerDay` × 60 minutes. Meetings count against the budget; dev items fill the remainder.
*   Chunk targets: 240 min (4h) preferred, minimum 90 min (1.5h), max 240 min per allocation. Matches historical pattern (67% of dev chunks ≥ 1.5h, 42% ≥ 3h).

### 1.9 Working-Hours Pattern (derived 2026-04-30 from 33-weekday history)

| Slot | Occupancy | Verdict |
|---|---|---|
| Before `08:00` | 0/33 | NEVER fill |
| `08:00`–`12:00` | 33/33 | ALWAYS filled (morning dev block) |
| `12:00`–`12:30` | 28/33 | Standup window |
| `12:30`–`13:00` | 6/33 | LUNCH (skipped 82% of days) |
| `13:00`–`16:00` | ~25/33 | Afternoon work |
| `16:00`–`17:00` | 9–18/33 | Tail-off |
| `17:00`–`18:00` | 1–2/33 | RARE overrun |
| After `18:00` | 0/33 | NEVER fill |
| Weekends | 0 | NEVER fill |

These constraints are encoded in `config.json → workHours` and enforced by the scheduler.

### 1.10 Single-Issue Logging (Tempo)

*   All worklogs go to **one** AgileEngine issue: **`MR-4`** (numeric id `96270`, summary `MR-1 [2026] Development`, project `MR — Madison Reed`, type `Task`).
*   Configured in `config.json → tempo`: `issueKey: "MR-4"`, `issueId: 96270`, `issueSummary: "MR-1 [2026] Development"`, `alwaysSingleIssue: true`.
*   Confirmed from 137 historical entries spanning Mar 1 – Apr 15 2026 — every single one used `issueId 96270`.

### 1.11 Calendar Integration

*   Source: Google Calendar private `.ics` URL in `GOOGLE_CALENDAR_ICS_URL` (treat as secret — anyone with the URL can read the full calendar).
*   Parser: `node-ical@^0.20.1` (0.26+ uses regex `v` flag requiring Node 22+; this project pins to a Node-18-compatible version).
*   Filtering: `skipAllDay`, `skipDeclined` (against `config.calendar.ownerEmail`), `skipFreeBusy: 'free'`. Recurring events (RRULE) expanded across the date range; `EXDATE` and overrides honored.
*   **DST anchor:** `rrule.between` doesn't track DST. Each occurrence is re-anchored to the original DTSTART time-of-day (`hours/minutes/seconds`) before being emitted — see `src/calendar.js` `expandOccurrences`.
*   **Meeting merge rules** (`config.calendar.mergeMeetings`): same-day events matching a `from` summary are absorbed into the corresponding `into` summary with summed duration and spanned start/end. Default rule: `DotCom/Mobile Team Scrum` → `Site Revolution - Booking Flow Scrum`.
*   Plan calls the calendar fetcher **internally** — no need to pre-stage with the standalone `calendar` command. `--no-calendar` skips fetch; `--meetings <file>` overrides with a pre-fetched JSON.
*   Owner email used for declined-detection: `cristian.moreno@madison-reed.com` (config). Distinct from the Atlassian login `cristian.moreno@agileengine.com` used by Tempo/Jira — both are correct for their respective systems.

### 1.12 Theme Classification (Smart Filter)

*   Roam nodes contribute themes only when their `ticketKey` starts with one of `REAL_TICKET_PREFIXES` (`DOTCOMPB`, `COREPB`, `MR`). Code-review labels (`LESSON-`, `NIT-`, `BUG-`, `SENTRY-`, `CONCERN-`, `BLOCKER-`, `SUGGESTION-`, `LINT-`, `MPL-`, `TD-`) embedded in session content are never treated as tickets.
*   Session files contribute **exactly one theme each** in priority order:
    1. Filename-prefixed ticket (`dotcompb-7929-...`) → that ticket;
    2. Filename-prefixed PR (`pr-20652-...`) → that PR;
    3. Single dominant PR mention in body → that PR;
    4. MR feature label match (Site Revolution / Shade Shop / Booking Flow / Google Sign-On / Location Specific Page / Navigation Redesign / Structured Data (JSON-LD) / SEO / Dash Hudson / X-Sell Carousel / Quiz / Playwright AC) → feature theme;
    5. Otherwise drop the session.
*   Claude/local meta sessions are excluded by `SESSION_EXCLUDE_PATTERNS`: `skill`, `doom`, `kyo-`, `omarchy`, `brain-d`, `knowledge-management`, `memory-`, `config-analysis`, `recording-automation`, `session-reset`, `-master$`. Positive override: `dotcompb-`, `pr-`, `ticket-` prefixes are always kept.
*   Theme weight: roam `Math.max(2, storyPoints || recencyBoost)`; PR/feature themes start at 3. Activity Log signals bump weight further.

### 1.13 Activity Log (Section 6 of session files)

*   Per the `session-reset` skill v4.1, every session file carries a Section 6 **Activity Log** — append-only chronological table of events.
*   Schema: `| Datetime | Duration | Type | Reference | Description |` where `Datetime` is `YYYY-MM-DD HH:MM` (host-local), `Duration` is `Nh / N.5h / Nm / —`, `Type` is from the controlled vocabulary (`session-reset`, `pr-open`, `pr-update`, `pr-feedback`, `pr-review`, `pr-merge`, `commit`, `refinement`, `implementation`, `documentation`, `testing`, `qa`, `bug-fix`, `debugging`, `research`, `planning`, `meeting`, `configuration`, `migration`, `architecture-extract`, `other`).
*   `src/sources/activity-log.js` parses both markdown (`## SECTION 6`) and org-mode (`* SECTION 6`) formats. Sessions without a Section 6 fall back to mtime — no regression.
*   `src/themes.js` `applyActivityLogSignals()` bumps theme weight by logged hours and pins `hintDate` to the most recent logged-work date. Verified: 5.5h logged on DOTCOMPB-7929 raised weight `5 → 11` and budget `18.5h → 22h`.
*   Activity Log is **never compressed** — it grows monotonically; rows are small.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Automate filling Tempo worklogs on AgileEngine's Jira (`agileenginecloud.atlassian.net`) for Kyo's `cristian.moreno@agileengine.com` account. The user logs hours for AgileEngine billing them for Madison Reed work. Source of truth for what was worked on: org-roam nodes (`~/.brain.d/roam-nodes/madison_reed/`) and Claude Code session files (`gptel-directives/sessions/`).

Final approval and submission stay manual in the browser. The scripts only populate the entries.

### 2.2 How to Use It (Daily Flow)

**The whole flow in one command:**

```bash
cd /Volumes/dev-partition/github-madison-reed/jira-tempo-hours
npm run plan -- --from 2026-05-16 --to 2026-05-31
```

This single command:
1. Fetches Google Calendar meetings for `--from`…`--to`.
2. Reads roam nodes in `~/.brain.d/roam-nodes/madison_reed/` touched in the range.
3. Reads session `*.md` files in `gptel-directives/sessions/` touched in the range.
4. Filters Claude/local meta sessions out. Keeps only sessions that name a real DOTCOMPB ticket, a PR number, or an MR feature.
5. Builds **themes** — one per real ticket/PR/feature with smart names like `DOTCOMPB-7929 — Bug Fixes & Implementation (Structured Data (JSON-LD))`.
6. Allocates each theme a per-week minute budget proportional to its weight (story points, recency, Activity Log signal).
7. Schedules each day: meetings first at their actual times → lunch carved out → morning + afternoon free intervals filled with theme chunks of 3–4h preferred, ≥1.5h whenever possible.
8. Stamps every entry with `issueKey: "MR-4"` and `issueId: 96270`.
9. Writes `week.json` (source of truth — what `push` sends to Tempo).
10. Writes `week.org` alongside it — human-readable view with overview, themes, and per-day breakdown.

**Then review and push:**

```bash
npm run push -- week.json --dry-run    # Sanity check — no API calls
npm run push -- week.json              # Actually create the worklogs
```

After `push` succeeds, open Tempo in your browser and click **Submit**.

### 2.3 Available Commands

| Command | Purpose | Status |
|---|---|---|
| `npm run whoami` | Verify Atlassian creds and print `accountId` | Phase 1 ✓ |
| `npm run history -- --from <date> --to <date>` | Fetch and summarize past worklogs | Phase 1 ✓ |
| `npm run export -- --from <date> --to <date> [--format csv]` | Dump worklogs in a date range to JSON or CSV | Phase 1 ✓ |
| `npm run calendar -- --from <date> --to <date> [--print]` | Pull Google Calendar meetings to JSON (debug only) | Phase 2 ✓ |
| `npm run plan -- --from <date> --to <date>` | One command: calendar + roam + sessions + Activity Log → `week.json` + `week.org` | Phase 3 ✓ |
| `npm run push -- week.json [--dry-run]` | POST `week.json` entries to Tempo | Phase 1 ✓ |

### 2.4 One-Time Setup

1. `cd /Volumes/dev-partition/github-madison-reed/jira-tempo-hours && npm install`
2. `cp .env.example .env && cp config.example.json config.json`
3. Get an Atlassian API token at `id.atlassian.com → Security → API tokens`, paste into `.env` as `ATLASSIAN_API_TOKEN='...'`.
4. Get a Tempo API token at Jira → Apps → Tempo → Settings → API Integration. Paste as `TEMPO_API_TOKEN='...'`.
5. Get Google Calendar secret ICS URL (Calendar settings → Integrate calendar → Secret address in iCal format). Paste as `GOOGLE_CALENDAR_ICS_URL='...'`. **Treat as secret** — anyone with it can read the full calendar.
6. `npm run whoami` → paste printed `ATLASSIAN_ACCOUNT_ID=...` into `.env`.
7. Tune `config.json` if needed (defaults derived from real history rarely need changing).

### 2.5 Key Decisions (Session-Wide)

1. **(2026-04-29)** Tempo REST API v4 over browser automation — more reliable, faster, no DOM-breakage risk.
2. **(2026-04-29)** Two-phase build. Phase 1 = scaffold + Tempo client + basic commands. Phase 2 = tune `plan.js` after seeing real submission patterns via `history`.
3. **(2026-04-29)** Submission is never automated. Scripts create worklogs; user clicks **Submit** in the browser. Hard product line.
4. **(2026-04-29)** Single-quote all token values in `.env`. Tokens contain `$`, `#`, `+`, `/`. dotenv treats single quotes as literal — safest format.
5. **(2026-04-29)** Org-mode for all documentation in this project. User is a Doom Emacs user. `README.md` was removed and replaced with `README.org`.
6. **(2026-04-29)** Native `fetch` over `axios`. Node 18+ has fetch built in.
7. **(2026-04-29)** Sources for `plan` are roam nodes + sessions filtered by `mtime` within the range.
8. **(2026-04-30)** Single-issue logging confirmed: `MR-4` (numeric `96270`). All 137 historical entries used `issueId 96270`.
9. **(2026-04-30)** Working-hours envelope from data: `08:00`–`18:00` weekdays, prefer `17:00` end. Lunch `12:30`–`13:00` skipped by default.
10. **(2026-04-30)** Google Calendar = mandatory meeting source. Meetings go in first; dev work fills around them.
11. **(2026-04-30)** `node-ical@^0.20.1` — 0.26+ requires Node 22+; pinned to 0.20 for Node 18 compatibility.
12. **(2026-04-30)** Round-robin distributor replaced by time-slot scheduler (`src/scheduler.js`).
13. **(2026-04-30 14:00)** DST-safe recurrence expansion: each occurrence re-anchored to original DTSTART local time-of-day.
14. **(2026-04-30 14:30)** Single command for the whole flow. `plan` calls the calendar fetcher internally.
15. **(2026-04-30 15:00)** Smart theme classifier: one theme per session, real ticket prefixes whitelisted, Claude/local meta excluded. Reduced theme count from 79 noisy entries to 5 clean ones.
16. **(2026-04-30 15:00)** Chunk-aware scheduler: target 3–4h dev blocks, minimum 1.5h preferred, fragments only when meetings force it.
17. **(2026-04-30 16:00)** DotCom Scrum merged into Site Revolution Scrum via `config.calendar.mergeMeetings`.
18. **(2026-04-30 17:00)** Auto-emit `week.org` alongside `week.json`. Plan writes both; `push` reads only the JSON.
19. **(2026-04-30 18:00)** Activity Log infrastructure: parser added, theme weight bumped by logged hours, `week.org` shows per-theme evidence. Aligned with `session-reset` skill v4.1 and `session-memory` v1.1.

### 2.6 Biweekly Push History

| Cycle | Workdays | Hours | Worklogs | Status |
|---|---|---|---|---|
| 2026-05-01 → 2026-05-15 | 11 | 88.0h | 57 (IDs 3472938–3472994) | **Done 2026-05-15** — click Submit in Tempo UI |
| 2026-05-16 → 2026-05-31 | 10 | 80.0h | — | **Done 2026-05-29** — click Submit in Tempo UI |

### 2.7 Pending Work

*   **Backfill Activity Logs in existing sessions** — none of the existing `gptel-directives/sessions/*.md` files have a Section 6 yet (except this one). On their next resets, add bootstrap rows per the v4.1 skill rule.
*   **Optional:** pull MR ticket activity from JIRA directly so dev descriptions reflect actual ticket activity.
*   **Optional:** detect PTO/holidays automatically.
*   **Optional:** per-theme description templates so dev blocks read like the historical pattern.
*   **Optional:** extract architecture memory once patterns validate across 2–3 future sessions.

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 CLI Scaffold
**Created:** 2026-04-29 | **Status:** Done.

`package.json` — type=module, bin=tempo-hours, scripts: `start`, `whoami`, `history`, `export`, `calendar`, `plan`, `push`. `src/index.js` — Commander entry. Wraps every action handler in `run(fn)` helper that catches errors and sets `process.exitCode`.

### 3.2 Tempo Client — `src/tempo.js`
**Created:** 2026-04-29 | **Status:** Done.

`tempoFetch(path, options)` wraps `fetch`, adds Bearer auth, throws on non-2xx. `listWorklogs({ from, to, accountId, limit })` handles pagination via `metadata.next`. `createWorklog(entry)` POSTs to `/worklogs` with v4 shape (numeric `issueId`).

### 3.3 Jira Client — `src/jira.js`
**Created:** 2026-04-29 | **Status:** Done.

HTTP Basic auth via `ATLASSIAN_EMAIL:ATLASSIAN_API_TOKEN` base64. `getMyself()` for accountId, `getIssue(keyOrId)`, `getIssueId(key)` convenience.

### 3.4 `whoami` — `src/whoami.js`
**Created:** 2026-04-29 | **Status:** Done.

Calls `getMyself()`, prints displayName, email, accountId, timeZone. Tells the user to paste `ATLASSIAN_ACCOUNT_ID=<id>` into `.env`.

### 3.5 `history` — `src/history.js`
**Created:** 2026-04-29 | **Status:** Done.

Defaults: `--from` 60 days ago, `--to` today. Computes: total hours, hours per weekday, top 10 issues, top 10 descriptions, last 5 sample entries. `--json` flag dumps raw worklogs.

### 3.6 `export` — `src/export.js`
**Created:** 2026-04-29 | **Status:** Done.

Fetches worklogs in `--from`..`--to`, writes to file. `--format json` (default) or `csv`. Default output: `submissions-<from>-to-<to>.<format>`.

### 3.7 `push` — `src/push.js`
**Created:** 2026-04-29 | **Status:** Done.

Reads plan file (accepts array or `.entries` shape). `validateEntries` rejects bad date format, missing `issueKey`/`issueId`, non-positive hours. Looks up `issueId` via `getIssueId(issueKey)` (cached per run), builds Tempo payload, POSTs. `--dry-run` prints payloads without sending.

### 3.8 `plan` — `src/plan.js` + sources/* + scheduler.js
**Created:** 2026-04-29 | **Last updated:** 2026-04-30 | **Status:** Phase 3 (chunk-aware scheduler + Activity Log signal mixing).

`loadConfig()` reads `config.json` if present, else `config.example.json`. Flow:
1. Load config + optional `meetings.json` (`--meetings` flag).
2. List workdays in range.
3. Read roam + sessions, filter by mtime in range → dev items with `minutesPerDay` proportional to weight.
4. For each day: build queue, call `scheduleDay` with workHours, target minutes, day's meetings, queue.
5. `scheduler.js` places lunch + meetings on a timeline, fills free intervals with dev items, returns sorted entries.
6. Write `week.json` with `issueId`, `workHours`, source counts, flat entries array.
7. Write `week.org` via `render-org.js`.

### 3.9 `calendar` — `src/calendar.js`
**Created:** 2026-04-30 | **Last updated:** 2026-04-30 (DST + merge) | **Status:** Done.

`fetchAndParse(url, from, to, calCfg)` uses `node-ical` `async.fromURL`, walks `VEVENT` records, expands `RRULE` via `rrule.between(from, to, true)`, applies `EXDATE` and recurrence overrides. **DST anchor** re-sets each occurrence's hours/minutes to `evStart` time-of-day. Filters per config. `applyMergeRules(events, rules)` same-day post-processing absorbs `from` summaries into `into` summaries (default: DotCom Scrum → Site Revolution Scrum).

### 3.10 `scheduler.js` — Time-Slot Allocator
**Created:** 2026-04-30 | **Last updated:** 2026-04-30 (chunk-aware) | **Status:** Done.

`scheduleDay({ date, workHours, targetMinutes, meetings, themes, remaining, issueKey, issueId })` builds blocked intervals (lunch + meetings), computes free intervals, splits into `preferred` (≤ `preferredEnd`) and `overflow` (after). `fillChunkAware(intervals, themes, remaining, ...)` places themes with target 240 min (4h), minimum 90 min (1.5h), max 240 min per allocation. `allocateBudgets(themes, totalDevMinutes)` rounds budgets to 30-min steps proportional to weight. Theme picking prefers same-day `hintDate` match; otherwise picks the theme with largest remaining budget. Every entry stamped with both `issueKey` and `issueId`. Each entry has `meta.source`: `calendar`, `dev`, or `dev-overflow`.

### 3.11 `themes.js` — Smart Theme Classifier
**Created:** 2026-04-30 | **Status:** Done.

`buildThemes({ roamNodes, sessions, range })` → deduplicated array sorted by weight desc. `REAL_TICKET_PREFIXES = ['DOTCOMPB', 'COREPB', 'MR']`. `SESSION_EXCLUDE_PATTERNS` drops Claude/local meta. `FEATURE_KEYWORDS` matches 14 MR feature labels. `ACTIVITY_RULES` regex chain → 7 activity buckets. `harvestSessionThemes(session, activityInRange)` returns exactly one theme per session: filename ticket > filename PR > body PR > feature label > drop. `applyActivityLogSignals(themesMap, sessions, range)` bumps weight by logged hours and tightens `hintDate`.

### 3.12 `render-org.js` — Human-Readable Plan View
**Created:** 2026-04-30 | **Status:** Done.

`renderPlanOrg(plan)` → org-mode document. Auto-emitted by `plan` alongside `week.json`. Sections: title + Overview table + Themes table + Theme sources description + Activity Log evidence (when present) + Daily breakdown per-day with `~start–end~ [duration] =TYPE= description` rows + Legend. Type tags: `MTG` (calendar), `DEV` (dev), `OVR` (dev-overflow). `week.org` is regenerated each run; never edit directly.

### 3.13 `sources/activity-log.js` — Section 6 Parser
**Created:** 2026-04-30 | **Status:** Done.

`parseActivityLog(content)` → array of `{ datetime, date, time, durationMinutes, type, reference, description, raw }`. Header regex matches both markdown (`## SECTION 6: ACTIVITY LOG`) and org-mode (`* SECTION 6: ACTIVITY LOG`). `VALID_TYPES` mirrors the controlled vocabulary in the `session-reset` skill. `parseDuration` accepts `Nh`, `N.5h`, `Nm`, `—`. `parseReference` classifies into `ticket`, `pr`, `commit`, `self`, `none`, `other`. `sources/sessions.js` now exposes the parsed log as `session.activityLog`.

---

## SECTION 4: FILE INDEX

Project root: `/Volumes/dev-partition/github-madison-reed/jira-tempo-hours/`

| File | Role |
|---|---|
| `package.json` | npm scripts, deps, ESM type |
| `.env` / `.env.example` | env (single-quoted token slots) |
| `.gitignore` | excludes `node_modules`, `.env`, `config.json`, logs |
| `config.json` / `config.example.json` | sources, schedule, workHours, calendar, tempo, defaults |
| `README.org` | full setup + usage docs (org-mode) |
| `SESSION.org` | project-local exhaustive session context (org-mode) |
| `src/index.js` | CLI entry (commander) |
| `src/tempo.js` | Tempo API v4 client |
| `src/jira.js` | Jira API client (issue/myself) |
| `src/whoami.js` | `whoami` command |
| `src/history.js` | `history` command |
| `src/export.js` | `export` command |
| `src/plan.js` | `plan` command — end-to-end pipeline |
| `src/push.js` | `push` command |
| `src/distribute.js` | workday list + (legacy) round-robin distributor |
| `src/scheduler.js` | time-slot allocator (meetings + lunch + dev fill) |
| `src/calendar.js` | `calendar` command + .ics fetch/parse/filter/merge |
| `src/themes.js` | smart theme classifier + Activity Log signal mixer |
| `src/render-org.js` | renders `week.json` → `week.org` |
| `src/sources/roam.js` | `~/.brain.d/roam-nodes/madison_reed/*.org` parser |
| `src/sources/sessions.js` | `gptel-directives/sessions/**` parser (+ activity-log) |
| `src/sources/activity-log.js` | Section 6 Activity Log parser (markdown + org) |
| `diagnose.js` | per-day plan diagnostic |
| `analyze.js` | one-off pattern analyzer |
| `backup-march.json` | exported worklogs Mar 1–31 (pattern reference) |
| `backup-april-15days.json` | exported worklogs Apr 1–15 (pattern reference) |
| `week.json` | latest plan (last: May 16–31, 80h, single `MR-4`/id `96270`) |
| `week.org` | human-readable view of `week.json` (auto-generated) |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What Was Done Last (2026-05-29)

**Second biweekly Tempo push — confirmed done.** User ran the full flow for the 2026-05-16 → 2026-05-31 cycle:
*   Loaded session context to identify the correct command sequence for May 16 → end of month (May 31, Saturday → last workday May 30).
*   Confirmed the cycle covers **10 workdays** (May 16, 19–23, 26–30), **80.0h total** on `MR-4`/96270.
*   New roam nodes picked up by `plan` for this cycle: `DOTCOMPB-8206` (`dotcompb-8206-booking-calendar-availability`), `DOTCOMPB-xxxx` (DY quiz buttons dynamic). Roam `eng-onboarding-skills` filtered (no real ticket prefix).
*   New sessions picked up: `dotcompb-8206-booking-calendar-availability`, `dy-question-email-capture-epic`, `mr-seo-structured-data-architecture`. Filtered: `eng-onboarding-skills`, `kyo-web-online`.
*   User confirmed: "I already sent everything" — plan generated, push executed, worklogs created. Click **Submit** in Tempo browser UI to finalize.

### Pending / Not Yet Started

*   **Next biweekly cycle** (2026-06-01 → 2026-06-15): run when ready.
*   **Backfill Activity Logs** — existing `gptel-directives/sessions/*.md` files still lack Section 6.
*   **Optional:** JIRA ticket activity import, PTO detection, per-theme description templates, architecture-memory extraction.

### Where to Resume

*   **To plan + push the next biweekly cycle** (Jun 1 → Jun 15):
    ```bash
    cd /Volumes/dev-partition/github-madison-reed/jira-tempo-hours
    npm run plan -- --from 2026-06-01 --to 2026-06-15
    # review week.org, then:
    npm run push -- week.json --dry-run
    npm run push -- week.json
    ```
    After API confirms, click **Submit** in Tempo's browser UI.
*   **To verify daily balance before push:** `node -e "const p=JSON.parse(require('fs').readFileSync('week.json')); const b={}; for(const e of p.entries) b[e.date]=(b[e.date]||0)+e.hours; for(const d of Object.keys(b).sort()) console.log(d, b[d].toFixed(2)+'h');"` — every day must equal `hoursPerDay` (default 8.0).
*   **If a session file gains a Section 6 Activity Log:** re-run `plan` to pick up the new evidence.
*   **If a calendar event lands at a wrong wall-clock time:** first check the DST anchor in `src/calendar.js` `expandOccurrences`.
*   **If themes look wrong:** check `src/themes.js` `SESSION_EXCLUDE_PATTERNS` and `REAL_TICKET_PREFIXES`.
*   **If a command errors:** verify `.env` quoting (single quotes around tokens), check `ATLASSIAN_ACCOUNT_ID`, re-run `whoami`. For calendar errors, verify `GOOGLE_CALENDAR_ICS_URL` and Node version (≥18, `node-ical@0.20`).
*   **If asked to commit / push to git:** confirm before any destructive git action; the project has no remote set up as of this writing.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session. Newest row first. See `session-reset/rules/activity-log.md` for the full spec.

| Datetime | Duration | Type | Reference | Description |
|---|---|---|---|---|
| 2026-05-29 11:30 | 0.25h | session-reset | this | Reset — updated Last Interaction with May 16-31 push confirmation, converted pending to history table, next cycle Jun 1 |
| 2026-05-29 11:00 | — | other | MR-4 | Ran plan + push for 2026-05-16 → 2026-05-31 (10 workdays, 80h, confirmed by user) |
| 2026-05-15 12:45 | 0.25h | session-reset | this | Session reset after first real push — updated Last Interaction with end-to-end run results, marked first-push pending item Done, added daily-balance verification recipe |
| 2026-05-15 12:35 | 0.1h | other | MR-4 | Pushed 57 worklogs to Tempo for 2026-05-01 → 2026-05-15 (88.00h on MR-4/96270, worklog IDs 3472938–3472994, 0 failed) |
| 2026-05-15 12:30 | 0.1h | other | this | Generated plan for 2026-05-01 → 2026-05-15 (11 workdays, 88h, 22 meetings, 10 DOTCOMPB themes); verified all days balance to 8.0h |
| 2026-05-15 12:00 | 0.5h | documentation | this | Created central `jira-tempo-hours.md` session file from project-local `SESSION.org`; added pointer in `MEMORY.md` |
| 2026-04-30 19:00 | 0.75h | session-reset | this | Reset run after v4.1 skill update — Section 6 promoted, theme classifier + render-org + Activity Log infra merged into context block |
| 2026-04-30 18:00 | 1.5h | session-reset | this | Added Section 6 + Activity Log spec to session-reset/session-memory skills; wired parser into jira-tempo-hours |
| 2026-04-30 17:00 | 1h | implementation | this | Built `src/sources/activity-log.js` parser + integrated into `themes.js` weight-bumping |
| 2026-04-30 16:00 | 0.5h | configuration | this | Added `mergeMeetings` rule to config; collapses DotCom/Mobile Team Scrum into Site Revolution Scrum |
| 2026-04-30 15:30 | 0.25h | research | MR-4 | Resolved `issueId 96270` → JIRA key `MR-4` (`MR-1 [2026] Development`) |
| 2026-04-30 15:00 | 1h | implementation | this | Added `src/render-org.js` → `week.org` human-readable plan view |
| 2026-04-30 14:00 | 2h | implementation | this | Built calendar fetcher, scheduler chunk-aware allocator, `plan` single-command pipeline |
| 2026-04-30 12:00 | 1h | bug-fix | this | Fixed DST drift in `node-ical` recurrence expansion (anchor occurrences to DTSTART time-of-day) |
| 2026-04-30 11:00 | 1h | research | this | Analyzed Mar–Apr 15 Tempo history (137 entries) to derive workHours envelope and theme patterns |
| 2026-04-29 16:00 | 1h | session-reset | this | Initial session compaction after Phase 1 (CLI, Tempo client, history, export, push) |
| 2026-04-29 14:00 | 2h | implementation | this | Built `whoami`, `history`, `export`, `push` commands + Tempo v4 + Jira clients |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
