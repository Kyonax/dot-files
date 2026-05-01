---
title: Session Activity Log — Datetime-Stamped Event Tracking
impact: HIGH
impactDescription: Defines the mandatory Activity Log table that every session file must carry. The Activity Log records each session reset, PR action, commit, refinement, and other relevant events with absolute datetimes and durations — making session files programmatically auditable, accurate inputs to time-tracking tools, and reliable historical records of what was done when.
tags: activity-log, datetime, time-tracking, audit, pr, session-reset, table, timestamp, duration, controlled-vocabulary, jira-tempo
---

This rule defines the Activity Log — a mandatory chronological table inside every session file that records each meaningful event with a datetime, duration, type, reference, and description. The Activity Log replaces vague "last updated" notes with a full audit trail of work, and serves as the authoritative input to downstream time-tracking and timesheet automation (e.g., `jira-tempo-hours`).

## What Is the Activity Log?

A single org-table-compatible markdown table at the bottom of every session file (Section 6) that captures every notable event during the session. Every entry has an absolute datetime, a duration (or em-dash for instant events), a controlled-vocabulary type, a reference (ticket / PR / commit / "this"), and a one-line description.

**Why it exists:**

1. **Datetime accuracy** — Section 2.3 (Decisions) and Section 3 (Implementations) carry dates only. The Activity Log adds *time-of-day*, which is what timesheet automation needs to map work to specific calendar slots.
2. **Audit trail** — A future-you (or another AI) can answer "when did I do X?" by scanning one table instead of grepping conversation transcripts.
3. **Tempo accuracy** — The `jira-tempo-hours` `plan` command reads the Activity Log to know exactly how long each ticket was worked on and on which date — replacing the previous mtime-only heuristic. Sessions without an Activity Log fall back to mtime, which is less accurate.
4. **Cross-session consistency** — All sessions follow the same schema, so tooling can rely on a stable format.

## Mandatory Placement

The Activity Log lives in **Section 6** of the context block, after Section 5 (Last Interaction). It is *not* compressed under normal conditions — it grows monotonically and is only ever appended to.

```
SECTION 1: Global Guidelines
SECTION 2: Session Overview
SECTION 3: Implementations
SECTION 4: File Index
SECTION 5: Last Interaction
SECTION 6: Activity Log     ← this rule
```

## Table Schema

```markdown
## SECTION 6: ACTIVITY LOG

| Datetime         | Duration | Type             | Reference        | Description |
|------------------+----------+------------------+------------------+-------------|
| YYYY-MM-DD HH:MM | Nh / Nm  | <type>           | <ref>            | One-line text |
```

### Field rules

| Field         | Format                                  | Required | Notes |
|---------------+-----------------------------------------+----------+-------|
| `Datetime`    | `YYYY-MM-DD HH:MM` (24-hour, local TZ)  | Yes      | When the event *started*. No seconds. No timezone (host-local). |
| `Duration`    | `Nh`, `N.5h`, `Nm`, or `—`              | Yes      | `—` for instant events (PR open, commit, push). Use `h` for hours, `m` for minutes; halves allowed (`1.5h`). |
| `Type`        | One of the controlled vocabulary below  | Yes      | Lowercase, kebab-case. |
| `Reference`   | Ticket / PR / commit / `this`           | Yes      | `DOTCOMPB-####`, `PR #####`, short SHA, `this` (= the current session file), `n/a` if truly none. |
| `Description` | One-line free text (≤ 100 chars)        | Yes      | Concrete, past-tense. Not "working on X" — say what you *did*. |

### Sort order

**Newest first** at the top of the table. New entries are *prepended*, not appended. This way the most recent activity is always visible without scrolling.

## Controlled Vocabulary (Type column)

Pick the most specific type. Do not invent new types without updating this rule.

| Type                     | Use when                                                                  |
|--------------------------+---------------------------------------------------------------------------|
| `session-reset`          | A session-reset compaction operation                                       |
| `architecture-extract`   | Extracting architectural memory into a memory file                         |
| `pr-open`                | Opening a new PR                                                           |
| `pr-update`              | Pushing new commits to an existing PR                                      |
| `pr-review`              | Reviewing someone else's PR (giving feedback)                              |
| `pr-feedback`            | Addressing review comments on your own PR                                  |
| `pr-merge`               | Merging / approving / shipping a PR                                        |
| `commit`                 | Direct commit to a branch (no PR)                                          |
| `refinement`             | Refining a ticket — ACs, scope, technical plan, validation                 |
| `implementation`         | Writing/editing production code                                            |
| `documentation`          | Writing or updating docs (README, JSDoc, internal docs, roam nodes)        |
| `testing`                | Writing or running tests                                                   |
| `qa`                     | Manual QA verification (browser testing, screenshot evidence, etc.)        |
| `bug-fix`                | Investigating + fixing a bug                                               |
| `debugging`              | Investigating without (yet) a fix                                          |
| `research`               | Codebase exploration, library research, prior-art analysis                 |
| `planning`               | Sprint planning, estimation, scoping, kickoff                              |
| `meeting`                | Meeting attendance (only if not already in a calendar feed)                |
| `configuration`          | Updating config, env, dependencies, tooling                                |
| `migration`              | Data or schema migration work                                              |

If none fit, use `other` and add the actual activity to the `Description`. If `other` keeps appearing, propose a new vocabulary entry.

## Reference Conventions

| Reference shape         | Means                                                          |
|-------------------------+----------------------------------------------------------------|
| `DOTCOMPB-7929`         | A real Madison Reed JIRA ticket                                |
| `MR-4`                  | An AgileEngine billing issue                                   |
| `PR #20652`             | A pull request — number only, no repo prefix                   |
| `commit a1b2c3d`        | Git commit short SHA                                           |
| `this`                  | The current session file (e.g., for `session-reset` rows)      |
| `n/a`                   | No external reference (rare — prefer a more specific shape)    |

When an activity touches multiple references, list the primary one. Mention the others in the description: `Description: "DOTCOMPB-7929 — also touched DOTCOMPB-7945"`.

## When to Add an Entry

Add an Activity Log entry *immediately* after each of these events:

- ✅ **Session reset is performed.** Type `session-reset`, reference `this`, description summarizes what was compacted.
- ✅ **A PR is opened, updated, reviewed, addressed, or merged.** One entry per action.
- ✅ **A commit is pushed without a PR.** Type `commit`, reference is the short SHA.
- ✅ **A ticket is refined or scoped.** Type `refinement`, reference is the ticket key.
- ✅ **Implementation work is done.** Type `implementation`, reference is the ticket key.
- ✅ **A bug is investigated or fixed.** Type `debugging` or `bug-fix`.
- ✅ **Significant research or planning happens** (>30 min).

Do **not** add entries for:
- ❌ Trivial conversational turns ("explained X to user")
- ❌ Reading existing files without producing an artifact
- ❌ Failed/aborted attempts that left no trace

## Example Activity Log

```markdown
## SECTION 6: ACTIVITY LOG

| Datetime         | Duration | Type            | Reference        | Description |
|------------------+----------+-----------------+------------------+-------------|
| 2026-04-30 14:30 | 1.5h     | session-reset   | this             | Compacted Phase 2 — calendar + scheduler + theme classifier |
| 2026-04-30 12:00 | —        | pr-open         | PR #20652        | Site Revolution Refinement — opened for review |
| 2026-04-29 18:00 | 0.5h     | bug-fix         | calendar.js      | Anchored RRULE occurrences to DTSTART time-of-day to fix DST drift |
| 2026-04-29 14:00 | 2h       | refinement      | DOTCOMPB-7929    | Refined ACs, added Phase 0 validation report |
| 2026-04-29 11:00 | 1h       | research        | DOTCOMPB-7929    | Codebase research on existing `productJsonLd.js` builder |
| 2026-04-28 16:30 | 0.75h    | testing         | DOTCOMPB-7942    | Wrote unit tests for Booking Flow Google SSO reducer |
| 2026-04-28 09:00 | 3h       | implementation  | DOTCOMPB-7942    | Initial Google SSO integration in BookingFlow component |
```

## Interaction with Other Sections

| Section         | Relationship to Activity Log |
|-----------------+------------------------------|
| Section 2.3 (Decisions) | Decisions reference back: "(2026-04-29) — see Activity Log entry at 14:00" |
| Section 3 (Implementations) | Per-item "Last updated" date should match the most recent Activity Log entry for that ticket |
| Section 5 (Last Interaction) | "What was done last" should be the top 3-5 entries of the Activity Log, abbreviated |
| Architecture Memory | When Step 2.5 extracts knowledge, the source-session reference includes the Activity Log entry datetime so consumers know exactly when the decision was made |

## Backwards Compatibility

For sessions that exist *without* an Activity Log:

1. **Do not retroactively fabricate datetimes.** If you don't know the time of an old event, omit the entry — partial truth is better than fake precision.
2. On the next session reset, **add a single bootstrap row** at the top:
   ```
   | YYYY-MM-DD HH:MM | — | other | this | Activity Log added on this reset; prior history not back-filled |
   ```
3. From that reset forward, all new events get full entries.

## Validation Checklist

Before finishing a session reset, verify the Activity Log:

- [ ] Section 6 exists and is named exactly `## SECTION 6: ACTIVITY LOG`
- [ ] Table header matches the schema exactly (5 columns, in order)
- [ ] Every datetime is in `YYYY-MM-DD HH:MM` format (no seconds, no timezone)
- [ ] Every duration uses `Nh`, `N.5h`, `Nm`, or `—`
- [ ] Every type is from the controlled vocabulary (no invented types)
- [ ] References are concrete (no "various tickets" — split into multiple rows instead)
- [ ] Descriptions are past-tense and concrete
- [ ] Newest row is at the top
- [ ] At least one new entry was added by this reset itself (the `session-reset` row)
