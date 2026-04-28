---
title: Plan Node Templates — Three Variants (Standard / Bug / Release), Metadata, and Formatting
impact: CRITICAL
impactDescription: Templates define the exact org-mode structure of every generated plan node. Without this rule, plans have inconsistent metadata, wrong section layouts, missing fields, or incorrect formatting — making them unusable for validation, cross-referencing, and the index dashboard.
tags: template, standard, bug, release, plan, base, metadata, formatting, structure, title, subtitle, author, filetags, source-url, date, effort, target-release, branch, status, file-naming, uuid, section-headers, screaming-case, checkbox, requirements-format, given-when-then, org-roam, properties
---

This rule defines the templates used to generate org-roam plan nodes for RECKIT. Every plan node must follow one of three template variants (Standard, Bug, Release), all built on a shared base structure. Deviating produces nodes that break cross-referencing (wrong `:ID:` placement), fail validation (missing sections), or render incorrectly in Emacs.

## Base Template Structure

```
:PROPERTIES: → :ID: → :END:
#+TITLE → #+SUBTITLE → #+AUTHOR → #+EMAIL → #+DATE → #+FILETAGS →
#+SOURCE_URL → #+LAST_UPDATE → #+EFFORT → #+TARGET_RELEASE → #+BRANCH → #+STATUS → #+OPTIONS
[Description paragraph]
* TABLE OF CONTENTs :toc:
  (nested links for all sections and sub-sections)
* SCOPE AND GOAL          (Standard / Release)
  ** REQUIREMENTS
  ** OUT OF SCOPE
  ** OPEN QUESTIONS
* DESIGN AND ARCHITECTURE (Standard / Release)
  ** DATA FLOW
  ** DECISIONS
  ** TRADE-OFFS
* MEDIA
* RELEVANT LINKs
* TODO PLAN TASKs
  ** Phase 0 — ...
  ** Phase 1 — ...
* DOCUMENTATION
  ** STRUCTURE AND FUNCTIONALITY
  ** DEPLOYMENT NOTEs
  ** UNIT TEST COVERAGE
  ** QA INSTRUCTIONs
  ** FINDINGS
* DELIVERABLEs
* COMMENTs
```

The `:PROPERTIES:` `:ID:` drawer **must come first** in the file, before any `#+` keywords (org-roam convention).

---

## Standard Plan Template (Feature)

```org
:PROPERTIES:
:ID:       [generated-uuid-v4]
:END:
#+TITLE: Plan #[slug]
#+SUBTITLE: [one-line goal]
#+AUTHOR: [[https://orcid.org/0009-0006-4459-5538][Cristian D. Moreno - Kyonax]]
#+EMAIL: kyonax.corp@gmail.com
#+DATE: [Mon DD, YYYY]
#+FILETAGS: :RECKIT:PLAN:
#+SOURCE_URL: https://github.com/Kyonax/reckit/tree/[branch]
#+LAST_UPDATE: [Mon DD, YYYY]
#+EFFORT: [S|M|L]
#+TARGET_RELEASE: v[X.Y]
#+BRANCH: [branch-name]
#+STATUS: [PLANNING|IN_DEV|IN_REVIEW|IN_TEST|RELEASED|SHELVED]
#+OPTIONS: toc:nil num:nil H:5

[Summary paragraph — what is being built, why now, scope boundaries. Cross-link
to architecture / naming-conventions / session file as relevant.]

* TABLE OF CONTENTs :toc:
- [[#scope-and-goal][SCOPE AND GOAL]]
  - [[#requirements][REQUIREMENTS]]
  - [[#out-of-scope][OUT OF SCOPE]]
  - [[#open-questions][OPEN QUESTIONS]]
- [[#design-and-architecture][DESIGN AND ARCHITECTURE]]
  - [[#data-flow][DATA FLOW]]
  - [[#decisions][DECISIONS]]
  - [[#trade-offs][TRADE-OFFS]]
- [[#media][MEDIA]]
- [[#relevant-links][RELEVANT LINKs]]
- [[#plan-tasks][PLAN TASKs]]
- [[#documentation][DOCUMENTATION]]
  - [[#structure-and-functionality][STRUCTURE AND FUNCTIONALITY]]
  - [[#deployment-notes][DEPLOYMENT NOTEs]]
  - [[#unit-test-coverage][UNIT TEST COVERAGE]]
  - [[#qa-instructions][QA INSTRUCTIONs]]
  - [[#findings][FINDINGS]]
- [[#deliverables][DELIVERABLEs]]
- [[#comments][COMMENTs]]

* SCOPE AND GOAL
# SOURCE: Frozen at first commit. Source of truth for what THIS plan ships.
** REQUIREMENTS
- *R1 — [short title]*
  - *GIVEN* [precondition],
  - *WHEN* [action],
  - *THEN* [expected outcome],
  - *AND* [additional clauses as needed].

** OUT OF SCOPE
# What this plan explicitly does NOT cover. Prevents scope creep.
- ...

** OPEN QUESTIONS
# Resolved during planning; each gets a Decision in the next section.
- [ ] *Q1 — [question]*
  - *Recommendation:* [your lean]
- [X] *Q2 — [question]* → resolved by D[n]

* DESIGN AND ARCHITECTURE
** DATA FLOW
# ASCII tree / diagram. Mirror session-file §3.x style.

** DECISIONS
- *D1 — [title]* ([date])
  - *Choice:* [what was picked]
  - *Why:* [rationale]
  - *Alternatives:* [what was rejected and why]
  - *Cross-ref:* session-file decision #[n] if global

** TRADE-OFFS
# Costs paid by the chosen design. Distinct from Out of Scope (which is deferred work).

* MEDIA
# Mockups, screenshots, OBS captures, before/after.

* RELEVANT LINKs
- [[https://github.com/Kyonax/reckit/tree/[branch]][Branch [branch-name]]]
- [[https://github.com/Kyonax/reckit/pull/[N]][PR #[N]]]  (added when opened)
- [[id:93c9b466-676d-48bf-9d1b-ec8b93816b5d][Index Reckit]]
- [[id:ac49fa6e-4520-47b4-b01a-477d7f135add][Reckit Architecture]]
- [[id:680e3a77-9f0f-477b-b9ec-94ce7a87416b][Reckit Naming Conventions]]
- *Session file:* `dot-files/.config/doom-mac/gptel-directives/sessions/kyo-recording-automation.md`
  - §X.Y [relevant sections]

* TODO PLAN TASKs [0/N] [0%]
# SOURCE: Phased committable units. Each phase ends with lint+test green.
** Phase 0 — Lock Decisions (pre-implementation)
- [ ] Resolve Q1 → update D[n]
** Phase 1 — [name]
- [ ] [task]
** Phase 2 — [name]
- [ ] [task]

* DOCUMENTATION
# SOURCE: Filled during/after implementation.
** STRUCTURE AND FUNCTIONALITY
** DEPLOYMENT NOTEs
** UNIT TEST COVERAGE
** QA INSTRUCTIONs
** FINDINGS

* DELIVERABLEs
- *Branch:* [name]
- *Commits:* [hash subj], ...
- *PR:* #[N] — merged [date]
- *Files touched:* [count]
- *Tests added:* [count]
- *Session file refs:* §[X.Y], decisions #[n]–#[m]
- *Tag:* v[X.Y] (if part of a release)

* COMMENTs
```

---

## Bug Plan Template

Same metadata block; `#+FILETAGS: :RECKIT:BUG:`. Body changes:

```org
:PROPERTIES:
:ID:       [generated-uuid-v4]
:END:
#+TITLE: (BUG) Plan #[slug]
#+SUBTITLE: [bug description]
#+AUTHOR: [[https://orcid.org/0009-0006-4459-5538][Cristian D. Moreno - Kyonax]]
#+EMAIL: kyonax.corp@gmail.com
#+DATE: [Mon DD, YYYY]
#+FILETAGS: :RECKIT:BUG:
#+SOURCE_URL: https://github.com/Kyonax/reckit/tree/[branch]
#+LAST_UPDATE: [Mon DD, YYYY]
#+EFFORT: [S|M|L]
#+TARGET_RELEASE: v[X.Y]
#+BRANCH: [branch-name]
#+STATUS: [PLANNING|IN_DEV|IN_REVIEW|IN_TEST|RELEASED|SHELVED]
#+OPTIONS: toc:nil num:nil H:5

[Bug context paragraph — what's broken, who reported, severity.]

* TABLE OF CONTENTs :toc:
- [[#symptom-and-impact][SYMPTOM AND IMPACT]]
- [[#steps-to-reproduce][STEPs TO REPRODUCE]]
- [[#expected-result][EXPECTED RESULT]]
- [[#actual-result][ACTUAL RESULT]]
- [[#root-cause][ROOT CAUSE]]
- [[#relevant-links][RELEVANT LINKs]]
- [[#plan-tasks][PLAN TASKs]]
- [[#investigation-notes][INVESTIGATION NOTEs]]
- [[#deliverables][DELIVERABLEs]]
- [[#comments][COMMENTs]]

* SYMPTOM AND IMPACT
# Who is affected, in what scenario, how often.

* STEPs TO REPRODUCE
# Numbered list — minimal repro path.
1. ...
2. ...
3. ...

* EXPECTED RESULT
# What should happen.

* ACTUAL RESULT
# What actually happens — the bug behavior.

* ROOT CAUSE
# Filled during investigation. May reference INVESTIGATION NOTEs.

* RELEVANT LINKs
- [[https://github.com/Kyonax/reckit/tree/[branch]][Branch [branch-name]]]
- [[id:93c9b466-676d-48bf-9d1b-ec8b93816b5d][Index Reckit]]
- [[id:[uuid-of-related-plan]][Plan #[related-slug]]] (parent feature, if any)

* TODO PLAN TASKs [0/N] [0%]
- [ ] Investigate root cause
- [ ] Implement fix
- [ ] Add regression test
- [ ] Verify fix in OBS smoke test

* INVESTIGATION NOTEs
# Debugging diary. Use ** UPDATE sub-headings for findings discovered after initial fix.

* DELIVERABLEs
- *Branch:* [name]
- *Commits:* ...
- *PR:* #[N]
- *Tests added:* regression test for the bug

* COMMENTs
```

---

## Release Plan Template

`#+FILETAGS: :RECKIT:RELEASE:`. Body:

```org
:PROPERTIES:
:ID:       [generated-uuid-v4]
:END:
#+TITLE: Release v[X.Y] — [Semantic Name]
#+SUBTITLE: [one-line release theme]
#+AUTHOR: [[https://orcid.org/0009-0006-4459-5538][Cristian D. Moreno - Kyonax]]
#+EMAIL: kyonax.corp@gmail.com
#+DATE: [Mon DD, YYYY]
#+FILETAGS: :RECKIT:RELEASE:
#+SOURCE_URL: https://github.com/Kyonax/reckit/pull/[N]
#+LAST_UPDATE: [Mon DD, YYYY]
#+EFFORT: [S|M|L]
#+TARGET_RELEASE: v[X.Y]
#+BRANCH: dev
#+STATUS: [PLANNING|IN_REVIEW|IN_TEST|RELEASED]
#+OPTIONS: toc:nil num:nil H:5

[Release summary — what theme, which plans bundle, why now.]

* TABLE OF CONTENTs :toc:
- [[#release-scope][RELEASE SCOPE]]
  - [[#whats-included][WHAT'S INCLUDED]]
  - [[#triad-status][TRIAD STATUS]]
  - [[#release-type][RELEASE TYPE]]
- [[#media][MEDIA]]
- [[#relevant-links][RELEVANT LINKs]]
- [[#release-tasks][RELEASE TASKs]]
  - [[#triad-readiness][Triad readiness (gates merge)]]
  - [[#tag--push][Tag + Push]]
  - [[#post-release][Post-release]]
- [[#documentation][DOCUMENTATION]]
  - [[#release-notes][RELEASE NOTES]]
  - [[#upgrade-notes][UPGRADE NOTES]]
- [[#deliverables][DELIVERABLEs]]
- [[#comments][COMMENTs]]

* RELEASE SCOPE
** WHAT'S INCLUDED
- [[id:[uuid]][Plan #[slug]]] :: ...
- [[id:[uuid]][Plan #[slug]]] :: ...

** TRIAD STATUS
- [ ] =package.json= bumped to [X.Y.0]
- [ ] =README.org= version markers refreshed (4 spots: =#+VERSION=, footer ASCII, shields.io badge, =alt="vX.Y"=)
- [ ] =CHANGELOG.org= new =[v[X.Y]] — [date] :: [Title]= block

** RELEASE TYPE
[Architectural Baseline | Feature | Patch | Hotfix]

* MEDIA

* RELEVANT LINKs
- [[https://github.com/Kyonax/reckit/pull/[N]][Release PR #[N]]]
- [[https://github.com/Kyonax/reckit/releases/tag/v[X.Y]][Tag v[X.Y]]] (post-release)
- [[id:93c9b466-676d-48bf-9d1b-ec8b93816b5d][Index Reckit]]
- *Session file:* §1.13 PR Composition Pattern, §1.8 Version Management

* TODO RELEASE TASKs [0/N] [0%]
** Triad readiness (gates merge)
- [ ] =package.json= bump (=npm version X.Y.0 --no-git-tag-version=)
- [ ] =README.org= version-marker patch (4 sed -i -E commands)
- [ ] =CHANGELOG.org= new release block above prior

** Tag + Push
- [ ] =git tag -a v[X.Y] -m "RECKIT v[X.Y]"=
- [ ] =git push origin v[X.Y]=

** Post-release
- [ ] (optional) =gh release create v[X.Y] --title "RECKIT v[X.Y]" --notes-file [...]=
- [ ] (optional) Branch protection on =master= toggle review

* DOCUMENTATION
** RELEASE NOTES
** UPGRADE NOTES

* DELIVERABLEs
- *PR:* #[N] — merged [date]
- *Tag:* v[X.Y]
- *Plans bundled:* [list of plan UUIDs]
- *Session file refs:* §X.Y release narrative

* COMMENTs
```

---

## Template Variant Selection

| Plan kind                              | Template     | Title Format                              | Key Sections                                          |
|----------------------------------------|--------------|-------------------------------------------|-------------------------------------------------------|
| Feature, refactor, new web source      | **Standard** | `Plan #<slug>`                            | SCOPE / DESIGN / DECISIONS / PHASES / DOCUMENTATION   |
| Bug fix                                | **Bug**      | `(BUG) Plan #<slug>`                      | SYMPTOM / REPRO / EXPECTED / ACTUAL / ROOT CAUSE / NOTEs |
| Release (`dev` → `master` PR)          | **Release**  | `Release v<X.Y> — <Semantic Name>`         | SCOPE (incl. triad) / RELEASE TASKs / NOTES           |

When unsure, ask. Most plans are Standard.

---

## Metadata Header Rules

| Header              | Value / Format                                                                  | Notes                                                                                                          |
|---------------------|---------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------|
| `#+TITLE:`          | `Plan #<slug>` / `(BUG) Plan #<slug>` / `Release v<X.Y> — <Name>`               | Slug is kebab-case-ish but uses underscores in filenames                                                       |
| `#+SUBTITLE:`       | One-line goal / bug description / release theme                                 |                                                                                                                |
| `#+AUTHOR:`         | `[[https://orcid.org/0009-0006-4459-5538][Cristian D. Moreno - Kyonax]]`        | Always ORCID-linked format                                                                                     |
| `#+EMAIL:`          | `kyonax.corp@gmail.com`                                                         | Fixed value                                                                                                    |
| `#+DATE:`           | `Mon DD, YYYY` (e.g., `Apr 26, 2026`)                                           | Creation date                                                                                                  |
| `#+LAST_UPDATE:`    | `Mon DD, YYYY`                                                                  | Updated on every node modification                                                                             |
| `#+FILETAGS:`       | `:RECKIT:PLAN:` / `:RECKIT:BUG:` / `:RECKIT:RELEASE:` / `:RECKIT:INDEX:`        | Choose by variant                                                                                              |
| `#+SOURCE_URL:`     | `https://github.com/Kyonax/reckit/tree/<branch>` (or `/pull/<N>` for releases)  | Updated to PR URL once PR opens                                                                                |
| `#+EFFORT:`         | `S` / `M` / `L`                                                                 | Coarse self-estimate (S < 1 day, M = 1–3 days, L = a week+)                                                    |
| `#+TARGET_RELEASE:` | `v<X.Y>` (e.g., `v0.5`)                                                         | Which release this plan ships in                                                                               |
| `#+BRANCH:`         | Local feature branch name                                                       | `dev` for release plans                                                                                        |
| `#+STATUS:`         | `PLANNING` / `IN_DEV` / `IN_REVIEW` / `IN_TEST` / `RELEASED` / `SHELVED`        | Mirrors current Plan Board lane                                                                                |
| `#+OPTIONS:`        | `toc:nil num:nil H:5`                                                           | Fixed value                                                                                                    |

**No `#+SETUPFILE:` / `#+STORY_POINTS:`** (per user's confirmed setup).

## Section Formatting Rules

1. **TOC links** use `[[#kebab-case-id][DISPLAY NAME]]` format.
2. **Section headers** use SCREAMING CASE with trailing `s` on plural words (e.g., `COMMENTs`, `RELEVANT LINKs`, `PLAN TASKs`, `DELIVERABLEs`, `TICKET TASKs`).
3. **Requirement format** uses org-mode bold `*GIVEN*`, `*WHEN*`, `*THEN*` — NOT `=GIVEN=`.
4. **TODO tasks** use `* TODO PLAN TASKs [0/N] [0%]` with `- [ ]` / `- [X]` checkboxes; phases as `**` sub-headings.
5. **Decisions** use `*D[n] — [title]* ([date])` with bold field labels (`*Choice:*`, `*Why:*`, `*Alternatives:*`, `*Cross-ref:*`).
6. **Cross-references** to other nodes use org-roam ID links: `[[id:UUID][Display Text]]`.
7. **Cross-references** to session-file sections: backtick-quoted file path + section number, e.g. ``§1.14`` (no link — file lives outside org-roam, in the dot-files repo).
8. **Code blocks** use `#+begin_src LANG` / `#+end_src` for code, `#+begin_quote` for quotes.

## File Naming

- Pattern: `YYYY-MM-DD-HHMMSS-reckit_<slug>.org`.
- Bug plans: prefix slug with `bug_` → `reckit_bug_<slug>` (e.g., `reckit_bug_iframe_subpixel`).
- Release plans: prefix slug with `release_` → `reckit_release_v0_5` (use underscores in version, not dots).
- Slug = kebab-cased description with underscores: `context_screen`, `brand_kot_perf_pass`, `audio_meter_rewrite`.
- All lowercase, underscores everywhere, no hyphens in the slug portion.

## UUID Generation

- Generate a standard v4 UUID for the `:ID:` property (use `uuidgen` on Linux/macOS).
- Each node must have a unique ID for org-roam cross-referencing.
- The `:PROPERTIES:` drawer with `:ID:` **must come first** in the file, before any `#+` keywords (org-roam convention).

## Common Project UUIDs

These are stable references — use them in `RELEVANT LINKs` of every plan node:

| Node                            | UUID                                     |
|---------------------------------|------------------------------------------|
| Index Reckit                    | `93c9b466-676d-48bf-9d1b-ec8b93816b5d`   |
| Reckit Architecture             | `ac49fa6e-4520-47b4-b01a-477d7f135add`   |
| Reckit Naming Conventions       | `680e3a77-9f0f-477b-b9ec-94ce7a87416b`   |
| Plan #context-screen            | `cabd1489-23cc-4ce3-9825-7b5a8eb065b9`   |
