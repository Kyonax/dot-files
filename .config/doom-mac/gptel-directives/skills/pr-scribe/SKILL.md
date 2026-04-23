---
name: pr-scribe
description: >-
  Draft, regenerate, refine, and validate pull request bodies with
  brand-aware structure AND brand-aware data sourcing. Auto-detects the
  target brand from the git remote origin URL (MadisonReed, Kyonax, or
  a generic fallback) and applies both the matching format conventions
  (checklist items, Changes layout — flat `**Changes:**` with ticket
  references OR themed `###` subsections with [NEW]/[MOD]/[DEL]/[MOV]
  tags, Technical Details shape, Testing Coverage layout, QA heading
  wording, Special Deployment severity vocabulary, Documentation
  media-type vocabulary) AND the matching data-source conventions
  (roam org-node extraction with field-to-slot mapping for MadisonReed,
  git-state + user-narrative + repo-state for Kyonax, conservative
  fallbacks otherwise), plus cross-cutting discipline (no emojis, no
  arrows, no private-file references, absolute URLs, no redundancy,
  bold-italic inline colon labels). Use when drafting a PR body from
  scratch, regenerating a single section (Changes block, Technical
  Details, How-to-test, Special Deployment, Documentation), maintaining
  a repo's `.github/PULL_REQUEST_TEMPLATE.md`, validating existing PR
  text against a brand's conventions, or onboarding a new brand by
  writing a new `brand-<name>.md` rule.
metadata:
  author: Cristian D. Moreno — Kyonax
  version: "2.0.0"
---

# pr-scribe

Structured authoring for **pull request bodies** with brand-aware format selection. The skill detects the target brand from the repo's git remote (or honors an explicit user override), loads the matching brand rule, and applies that brand's full PR convention — from checklist items through Documentation heading vocabulary — while enforcing a set of cross-cutting writing disciplines that apply regardless of brand.

This skill does **one thing**: produce PR description text. It does not draft commit messages, generate CHANGELOG entries, or handle release flow. Its scope is the contents of the PR body.

Each brand rule specifies BOTH the format the output takes AND the data sources the Worker reads to fill that format. For example, `brand-madison-reed.md` says "read a roam org-node with `#+TITLE:` / `#+SOURCE_URL:` / `** QA INSTRUCTIONs` etc. and map those fields onto PR slots", while `brand-kyonax.md` says "read the git state of the feature branch plus the user's conversational description and the repo's existing CHANGELOG / README". Picking the wrong brand doesn't just break formatting — it also makes the Worker look in the wrong place for the content.

## Core Principles

1. **Brand-aware, not one-size-fits-all.** Different GitHub orgs follow different PR conventions. The skill auto-detects the target brand from the repo remote and loads the matching `brand-<name>.md`. Fallback is a conservative generic format that works for any repo.
2. **Closed vocabularies.** Every enumerated element (Changes tags, severity labels, media types) is drawn from a closed list defined in the relevant rule. The skill never invents new vocabulary.
3. **Self-documenting templates.** When maintaining `.github/PULL_REQUEST_TEMPLATE.md`, the enforcement rules live as inline `<!--` comments inside the template so every populated PR carries the rules with it.
4. **One fact, one section.** The Changes list is the authoritative file inventory. Summary / rationale / test steps / deploy preconditions cross-reference by path, never re-list.
5. **Public-facing discipline.** PR bodies never reference private / gitignored files. All links are absolute GitHub URLs. No emojis outside Testing status cells. No arrow characters anywhere.

## When to Apply

Reference these guidelines when:

*   Drafting a pull request body from scratch on any repo.
*   Regenerating only one PR subsection (Changes block, Technical Details, Testing Coverage, How-to-test / QA, Special Deployment, Documentation).
*   Maintaining or creating `.github/PULL_REQUEST_TEMPLATE.md` for a repo.
*   Validating existing PR text against the brand's conventions or the cross-cutting rules.
*   Onboarding a new brand — writing a new `rules/brand-<name>.md` when the existing brand rules don't cover a target org's conventions.

## When to Read Which Rules

Every task starts with brand detection. Then load the matching brand rule plus the specific-section rule(s) the task needs.

| If working on... | Read these rules |
|---|---|
| Any PR task — always start here | `rules/brand-detection.md` (to pick the brand), then load the matching brand rule below |
| Drafting a full PR body (Madison Reed repo) | `rules/brand-madison-reed.md` + `rules/pr-body-structure.md` + `rules/changes-list.md` + `rules/supporting-sections.md` + `rules/content-richness.md` + `rules/global-writing-rules.md` |
| Drafting a full PR body (Kyonax repo) | `rules/brand-kyonax.md` + `rules/pr-body-structure.md` + `rules/changes-list.md` + `rules/supporting-sections.md` + `rules/content-richness.md` + `rules/global-writing-rules.md` |
| Drafting a full PR body (any other repo — generic fallback) | `rules/brand-detection.md` (read the fallback defaults) + `rules/pr-body-structure.md` + `rules/changes-list.md` + `rules/supporting-sections.md` + `rules/content-richness.md` + `rules/global-writing-rules.md` |
| Writing only the Changes block | `rules/brand-detection.md` + the matching brand rule + `rules/changes-list.md` + `rules/content-richness.md` + `rules/global-writing-rules.md` |
| Writing only Technical Details / Testing / QA / Deployment / Documentation | `rules/brand-detection.md` + the matching brand rule + `rules/supporting-sections.md` + `rules/content-richness.md` + `rules/global-writing-rules.md` |
| Maintaining `.github/PULL_REQUEST_TEMPLATE.md` | `rules/brand-detection.md` + the matching brand rule + `rules/pr-body-structure.md` + `rules/global-writing-rules.md` |
| Validating existing PR text for rule violations | `rules/global-writing-rules.md` + the matching brand rule + the specific section rule(s) for the part being validated |
| Onboarding a new brand (writing `rules/brand-<name>.md`) | `rules/brand-detection.md` (see "How to adopt a new brand") + `rules/pr-body-structure.md` + `rules/changes-list.md` + `rules/supporting-sections.md` (to understand which variants are available to reference) |

## Quick Reference

| Rule | Description |
|---|---|
| `brand-detection` | Decision tree for picking the correct brand rule from repo context — git remote URL inspection (`github.com/MadisonReed/*` ⇒ Madison Reed; `github.com/Kyonax/*` ⇒ Kyonax; anything else ⇒ generic fallback), explicit user override support, repo-local indicator files as tiebreaker, fallback defaults for unrecognized repos, and the adoption procedure for adding a new brand rule. Always loaded first. |
| `pr-body-structure` | Universal top-level PR body skeleton — fixed vertical ordering of sections (Checklist, What does this PR do? with Changes subsections + Technical Details + Testing Coverage, How to test, Special Deployment, Documentation, optional reference footer), the universal-vs-brand-configurable slot matrix, per-slot omission rules, and the invariants for maintaining a self-documenting `.github/PULL_REQUEST_TEMPLATE.md` (top-of-file rule block, per-section inline guide comments). |
| `content-richness` | Universal content quality standards independent of brand layout — defines the detail level that fills each section structure. Two levels: RICH (default, status tags on Changes entries, one test case per row, detailed expected labels, thorough Technical Details) and MINIMAL (grouped summaries, no tags in Pattern A, brief labels). Brand rules declare which level applies. Separates "how detailed is each entry" from "what shape does the section take." Always loaded alongside the brand rule and section-specific rules. |
| `changes-list` | The two Changes-block formats — Pattern A (flat `**Changes:**` bullet list with optional ticket references, status tags controlled by content richness level) and Pattern B (five themed `###` subsections — Implementation, Release, CI & Tooling, Dependencies, Docs — with the closed `[NEW] / [MOD] / [DEL] / [MOV]` tag vocabulary, mandatory tag legend blockquote under Implementation, canonical release triad under Release, four mandatory bullets under Dependencies). The brand rule picks one pattern; `content-richness.md` controls the detail level within it. |
| `supporting-sections` | Variant catalog for the five supporting sections — Technical Details (`TD-FREEFORM` custom subheadings vs `TD-4FIELD` Chose/Over/Why/Trade-off), Testing Coverage (`TEST-SINGLE` one table vs `TEST-TWO-TABLE` Automated tests + Quality gates), QA heading (`QA-INSTRUCTIONS` with `- **Expected:**` inline bullet vs `QA-HOW-TO-TEST` with `***Expected:***` at 6-space indent), Special Deployment (`DEPLOY-FREEFORM` prose with config blocks vs `DEPLOY-SEVERITY` numbered list with CRITICAL/REQUIRED/OPTIONAL), Documentation (`DOC-OPEN` free-form vs `DOC-MEDIA-VOCAB` with closed DESKTOP/TABLET/MOBILE/VIDEO/DIAGRAM/SCREENSHOT vocabulary), plus reference-link footer on/off. Brand rules pick one variant per section. |
| `global-writing-rules` | Seven cross-cutting disciplines applied to every PR body regardless of brand — no emojis (except `✅ ❌ ⚠️` in Testing Coverage status cells), no arrow characters (`→ ⇒ => ➜`), no references to private / gitignored files, absolute GitHub URLs only in PR body context, no fact restated across sections, inline colon labels are bold-italic (`***Expected:***`) while group headers stay bold-only (`**Prereqs:**`), checkboxes restricted to the top `## Checklist` block. Includes the pre-return sweep checklist. |
| `brand-madison-reed` | Madison Reed Dotcom team spec — **Data source:** roam org-node (`.org` file with `#+TITLE:` / `#+SUBTITLE:` / `#+SOURCE_URL:` / `* RELEVANT LINKs` / `** STRUCTURE AND FUNCTIONALITY` / `** Key Decisions` / `** UNIT TEST COVERAGE` / `** QA INSTRUCTIONs` / `** DEPLOYMENT NOTEs` / `* COMMIT MSG` fields) with extraction table mapping each field onto a PR slot, org-to-Markdown conversion rules, checklist-conditional-logic based on org content, bug-vs-feature detection from the `(BUG)` prefix, no-fabrication rule. **Format:** `[DOTCOMPB-XXXX]: <Subtitle>` title, bare JIRA URL first line, 6-item checklist, `**Design:** [Figma](url)` line, Pattern A flat `**Changes:**` list with `[NEW]/[MOD]/[DEL]/[MOV]` status tags (RICH content), `TD-FREEFORM` Technical Details, `TEST-SINGLE` Unit Testing Coverage table (one row per test case), `QA-INSTRUCTIONS` heading with detailed `- **Expected:**` inline bullets, `DEPLOY-FREEFORM` prose + config blocks, `DOC-OPEN` Documentation, reference-link footer, labels reminder (`DOTCOM TEAM`, `Pending QA Review`). |
| `brand-kyonax` | Kyonax organization spec — **Data source:** git state of the feature branch (branch name, commits ahead of base, diff) + user-provided conversational narrative + repo cross-references (`CHANGELOG.org` existing entries, `README.org` identity, `package.json` version, `.github/workflows/ci.yml` job names); explicitly forbids referencing private local files (session notes, clipboard buffers, `.rc.gpg`, `.env`, `.github/BRANCHES.org`); no external tickets, no Figma; release-detection via the three-file triad test. **Format:** `[vX.Y]: <Title>` / `release: vX.Y` / `feat(scope):` title patterns, no top-of-body URL, 13-item checklist with absolute-URL references and a `Pre-Check Failed` label condition, `**Design / Reference:**` link label, Pattern B themed Changes subsections with `[NEW] / [MOD] / [DEL] / [MOV]` tag vocabulary, `TD-4FIELD` Technical Details (Chose / Over / Why / Trade-off), `TEST-TWO-TABLE` Testing Coverage with Automated tests + Quality gates, `QA-HOW-TO-TEST` heading with 6-space-indented bold-italic `***Expected:***` + optional ASCII flow tree, `DEPLOY-SEVERITY` numbered list with CRITICAL / REQUIRED / OPTIONAL, `DOC-MEDIA-VOCAB` with closed DESKTOP / TABLET / MOBILE / VIDEO / DIAGRAM / SCREENSHOT vocabulary, no reference footer, no label reminder. |
