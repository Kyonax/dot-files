# Kyonax Project Conventions Architecture Memory

## 1. Summary

**Domain:** Cross-repository conventions shared across all Kyonax projects (root file organization, file headers, CI gate suite, license posture, scripts layout).

**Project:** Kyonax umbrella (reckit, kyo-web-online, org-2-html, future kyo-blog and others).

**Last updated:** 2026-05-21

**Source sessions:**
- `org-2-html.md` (2026-05-21) — extracted Tier 1 header pattern, 7-job CI gate suite, scripts/ composite gate, three-tier composition, single-license posture, AI-section exclusion, scripts-NOT-Tier-1 distinction, AGENTS.md three-exception clause, reference-over-copy for design rules, figlet/guardhouse naming theme, README ASCII-art shared between repos.
- `kyo-web-online.md` (2026-05-05 → 2026-05-20) — reference implementation of every pattern listed above.
- `kyo-recording-automation.md` (2026-04-08 → 2026-04-26) — reckit reference implementation; predates the GPL-3.0-only convention (reckit uses MPL-2.0 OR Apache-2.0 dual-license — recorded as an exception in ad-002).

**Knowledge categories:**
- Architecture Decisions: 5 entries (`ad-001` … `ad-005`)
- Design Patterns: 6 entries (`dp-001` … `dp-006`)
- Constraints & Limitations: 1 entry (`cl-001`)

---

## 2. Architecture Decisions

### ad-001: tier-1-figlet-header-convention
**Date:** 2026-05-21 | **Status:** active
**Source:** `org-2-html.md` — Activity Log 2026-05-21 01:00, mirroring `kyo-web-online.md` §1.x and `kyo-recording-automation.md`

**Context:** Every Kyonax repo has root-level files (config, governance, release-tracking) that benefit from a uniform "first-line scan" signal so a visitor or future maintainer knows the file's intent without reading its body.

**Decision:** Every Tier 1 file carries an extended header block in its first ~12 lines:
1. Two-line GPL preamble (copyright + license URL/marker).
2. Blank comment line.
3. Figlet-rendered ASCII banner (font: `smslant`) showing an evocative "THE _____" label.
4. Blank comment line.
5. `<filename> — <one-line tagline>`.
6. `<YYYY-MM-DD>` on its own line.
7. Blank comment line.
8. 1–3 lines of WHAT + WHY, hard-wrapped at ≤70 chars.
9. Blank comment line.
10. `Guidelines:` followed by 1–3 single-line rules.

**Alternatives considered:**
- Plain copyright preamble only (no figlet) — rejected: loses the visual scan signal that ties root files together across repos.
- Section list inline in every header — rejected as verbose: the file's own section delimiters are self-documenting.
- Long multi-paragraph context — rejected: the user's directive is *concise is better*, and verbose context becomes maintenance debt.

**Consequences:**
- Figlet (`figlet -f smslant "THE <NOUN>"`) is now a documented prerequisite for editing or creating Tier 1 files.
- The body-below-figlet pattern is bounded (≤10 lines), making headers diff-friendly.
- The naming theme is captured in `cl-001` (guardhouse vocabulary, ≤12 chars).

### ad-002: single-license-posture-per-repo
**Date:** 2026-05-21 | **Status:** active
**Source:** `org-2-html.md` — Activity Log 2026-05-21 01:00

**Context:** Multiple license postures across Kyonax repos make derivative-work obligations ambiguous and dilute the brand's IP stance.

**Decision:** Each new Kyonax repo picks **one** GPL family member at inception, declares it via SPDX in `package.json` + `LICENSE` + `LICENSING.org`, and never adds a permissive fallback. CI gates the presence of the license preamble on every committed source file via `scripts/check-license-headers.mjs`.

**Per-repo registry (as of 2026-05-21):**
- `kyo-web-online` → GPL-2.0-only.
- `org2html` (`@kyonax/org2html`) → GPL-3.0-only.
- `reckit` → MPL-2.0 OR Apache-2.0 dual-license (predates this convention; recorded as an exception, not a precedent).

**Alternatives considered:**
- Permissive (MIT / Apache-only) default — rejected: weakens the copyleft posture the user wants to maintain.
- Per-repo decision with no guidance — rejected: led to historical drift; new repos now inherit the convention by default.
- Dual-licensing everywhere — rejected as the *new* default for the reckit-style reason (permissive escape hatch).

**Consequences:**
- New Kyonax repos default to GPL-3.0-only unless the user explicitly says otherwise.
- The `LICENSING.org` file in each repo carries the per-extension header template specific to that repo's license variant.
- The reckit exception stays explicit in `kyo-recording-automation.md` so future maintainers don't read it as a counter-pattern.

### ad-003: scripts-folder-not-tier-1
**Date:** 2026-05-21 | **Status:** active
**Source:** `org-2-html.md` — Activity Log 2026-05-21 03:30 (refinement pass)

**Context:** Initial draft put `scripts/_lib.mjs`, `precheck.mjs`, `check-license-headers.mjs` in the Tier 1 list with figlet banners. The user pushed back: figlet is for *visitor-facing* infrastructure (root config + governance), not internal tooling.

**Decision:** `scripts/` carries the simpler **source-file** header (GPL preamble + 2-3 line description in a single `/* */` block). No figlet banner. Source files in `src/` follow the same rule (preamble + filename block + JSDoc per `ad-004`).

**Alternatives considered:**
- Keep figlet on every file — rejected: visual noise without information value on internal tooling.
- Remove ALL headers from scripts — rejected: license preamble is required for CI gate + IP discipline.

**Consequences:**
- `LICENSING.org` Tier 1 table lists ONLY root + `.github/` files (no scripts row).
- New scripts ship with the lean header. Future tooling that surfaces visitor-facing artifacts (e.g., a CLI binary directly invoked by end users) can earn a figlet on a case-by-case basis.

### ad-004: agents-md-three-exception-clause
**Date:** 2026-05-21 | **Status:** active
**Source:** `org-2-html.md` — Activity Log 2026-05-21 02:00

**Context:** `AGENTS.md` rule "no comments in TypeScript" originally read as absolute, but the CI license-header gate *requires* a comment (the preamble). Strict reading of the rule created a contradiction.

**Decision:** Comments are forbidden in TypeScript source EXCEPT:
1. **License preamble** — the GPL header at the top (mandatory; gated).
2. **Filename + description block** immediately following the preamble.
3. **JSDoc on exported helpers / type definitions / surprising algorithms** that documents non-obvious WHY (constraints, invariants, workarounds). Description-of-WHAT is still discouraged — well-named identifiers beat narration.

Placeholder, status, and "what this does" inline comments must still be removed.

**Alternatives considered:**
- Strict zero-comments rule + skip the license-header gate — rejected: license posture is non-negotiable.
- Allow free-form JSDoc — rejected: WHAT-descriptions are noise; only non-obvious WHY earns the budget.

**Consequences:**
- `scripts/check-license-headers.mjs` is the *one* enforcement point.
- Code review (CODEOWNERS) is the human enforcement for the JSDoc-quality boundary.
- New repos copy this clause verbatim into their `AGENTS.md`.

### ad-005: gitignore-omits-ai-agent-paths
**Date:** 2026-05-21 | **Status:** active
**Source:** `org-2-html.md` — Activity Log 2026-05-21 02:30

**Context:** The kyo-web-online `.gitignore` template includes an "Editors / IDEs / AI agents" section that ignores `.claude/`, `.aider*`, `.cursor/`, `.continue/`. When mirroring that template into a new repo, the user explicitly asked to NOT carry those paths over.

**Decision:** Kyonax `.gitignore` templates do NOT include any AI-coding-agent paths. Editor swap-file ignores (`.swp`, `.swo`, `*~`, `\#*\#`, `.\#*`) stay; the AI agents block is omitted.

**Alternatives considered:**
- Inherit the kyo-web-online template verbatim — rejected on user direction.
- Explicitly track AI-agent state — rejected: still off the table; the omission is editorial, not architectural permission to commit those paths.

**Consequences:**
- When generating a new Kyonax repo `.gitignore`, the "Editors / IDEs / AI agents" section is intentionally absent.
- Future contributors are expected to manage agent-tool state via their own global gitignore (`~/.config/git/ignore`), not via the repo.
- This decision is recorded so a future automated template doesn't silently re-introduce the section.

---

## 3. Design Patterns

### dp-001: seven-job-ci-gate-suite
**Date:** 2026-05-21 | **Status:** active
**Source:** `kyo-web-online/.github/workflows/ci.yml` (canonical), mirrored verbatim into `org-2-html/.github/workflows/ci.yml`

**When to use:** First CI workflow in any new Kyonax repo, or when an existing repo upgrades from a stub workflow.

**Structure:**
```
ESLint
↓
Precheck (composite gate — scripts/precheck.mjs)
↓
Vitest (with "no test files yet" tolerance branch)
↓
Production build (uploads dist/ as 7-day artifact)
↓
Security Scan (inline grep, GitHub annotations)
↓
Protected Files (categorized advisory PR comment — never blocks merge)
↓
Pre-Check Label aggregator
  (needs: [all-above]; if: always() && pull_request; toggles `Pre-Check Failed` label)
```

**Mandatory fields per job:** `runs-on: ubuntu-latest`, `actions/checkout@v4`, `actions/setup-node@v4` with `node-version: '20'` + `cache: 'npm'`, then `npm ci` before the gate command.

**Trigger pattern:**
```yaml
on:
  pull_request:
    branches: [master, main]   # (or develop, vue-migration as the repo dictates)
  push:
    branches: [master, main]

concurrency:
  group: ${{ github.workflow }}-${{ github.head_ref || github.ref }}
  cancel-in-progress: true

permissions:
  contents: read
  pull-requests: write
  issues: write
```

**Anti-patterns:**
- Adding a job WITHOUT updating the aggregator's `needs:` list — the gate becomes invisible to the label automation.
- Putting `Protected Files` in blocking position — it must stay advisory, by design.
- Letting `Security Scan` scan its own rule strings — exclude `eslint.config.*`, `scripts/check-*.mjs`.

### dp-002: precheck-composite-gate
**Date:** 2026-05-21 | **Status:** active
**Source:** `kyo-web-online/scripts/precheck.mjs` + `_lib.mjs`

**When to use:** When a repo accumulates 2+ validation scripts under `scripts/check-*.mjs`. The composite gate sequences them, prints a PASS/FAIL summary, and exits non-zero if any check failed.

**Structure:**
- `scripts/precheck.mjs` — the orchestrator. CHECKS array lists `{ id, script, label }` triples. Supports `--skip=id1,id2`. Skips checks whose script doesn't exist yet (graceful bootstrap).
- `scripts/_lib.mjs` — shared helpers: `REPO_ROOT`, `walk(dir, {ext, ignore})`, `read`, `rel`, `hasCcsHeader`, `exitWith`, color/log primitives (`ok`, `warn`, `fail`, `head`, `line`, `c`). Pure Node built-ins; no deps.
- `scripts/check-<topic>.mjs` — individual gates. Each imports from `_lib.mjs`, builds a `failures[]` array, and ends with `exitWith({ failures, name })`.

**Invocation:**
- Local dev: `node scripts/precheck.mjs`
- CI: a `precheck` job in `ci.yml` runs `node scripts/precheck.mjs`.
- `precheck` is wired as `prebuild` in `package.json` for repos that build (vite-ssg, tsup, etc.).

**Anti-patterns:**
- Calling validation scripts directly in CI without going through precheck — divergence between local and CI execution.
- Putting business logic in `_lib.mjs` — keep it generic; per-gate logic lives in `check-*.mjs`.

### dp-003: protected-files-categorized-advisory
**Date:** 2026-05-21 | **Status:** active
**Source:** `kyo-web-online/.github/workflows/ci.yml` (`protected-files` job)

**When to use:** Soft governance layer in CI that flags edits to sensitive files (legal, governance, supply chain, CI/security, build/config, release artifacts) without blocking merge.

**Structure:**
- Six file-list variables (`LEGAL_FILES`, `GOVERNANCE_FILES`, `SUPPLY_CHAIN_FILES`, `CI_SECURITY_FILES`, `BUILD_CONFIG_FILES`, `RELEASE_ARTIFACT_FILES`).
- `git diff --name-only origin/${{ github.base_ref }}...HEAD` produces the changed-file list.
- A `check_category()` helper produces per-category hit lists.
- An `add_section()` helper assembles a multi-section markdown comment.
- If any category has hits, the workflow `gh pr comment`s the assembled body. `::warning::` annotation surfaces in the Checks UI.
- Never sets exit code 1. Always succeeds.

**Anti-patterns:**
- Letting this job block merge — defeats the "advisory" nature.
- Listing high-churn files (e.g., individual `scripts/check-*.mjs`) — drowns the signal.
- Forgetting to `set -e` discipline in bash here — the helper is intentionally permissive (uses `|| true`).

### dp-004: pre-check-label-aggregator
**Date:** 2026-05-21 | **Status:** active
**Source:** `kyo-web-online/.github/workflows/ci.yml` (`pre-check-label` job)

**When to use:** Always — every Kyonax CI workflow ends with this aggregator.

**Structure:**
```yaml
pre-check-label:
  name: Pre-Check Label
  runs-on: ubuntu-latest
  needs: [eslint, precheck, tests, build, security-scan]
  if: always() && github.event_name == 'pull_request'
  permissions:
    contents: read
    pull-requests: write
    issues: write
  steps:
    - name: Sync "Pre-Check Failed" label
      env:
        GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        PR_NUMBER: ${{ github.event.pull_request.number }}
        REPO: ${{ github.repository }}
        # one ${{ needs.<job>.result }} env per job above
      run: |
        # if any RESULT != "success", add label; else remove
        gh pr edit "$PR_NUMBER" --repo "$REPO" --add-label "Pre-Check Failed" || true
```

**Anti-patterns:**
- Forgetting `if: always()` — the aggregator gets skipped when any upstream job fails (the opposite of what it should do).
- Not using `|| true` on the `gh` calls — the label sync can fail silently for forks; that's acceptable.
- Adding a new gate job and forgetting to add it to `needs:` AND the env block.

### dp-005: engine-design-contract-consumer-three-tier
**Date:** 2026-05-21 | **Status:** active
**Source:** `org-2-html.md` §2.1 (decision recorded 2026-05-21)

**When to use:** Kyonax projects with a UI surface that ship a reusable rendering engine separately from the visual brand.

**Structure:**
1. **Engine** — framework-agnostic library. Owns parsing, transformation, sanitization, plugin contract. *Has no opinion about visual identity.* Ships a minimal default theme so it's runnable out-of-the-box, but the default is *expected to be overridden* by consumers. Example: `@kyonax/org2html` (TypeScript engine, GPL-3.0).
2. **Design-contract source-of-truth** — the visual brand. Owns OKLCH palette, typography, UI primitive composition, accessibility floor, image pipeline, performance rules. Documented in its own session file with stable section numbering so other sessions can reference `[session: <file>.md > §X.Y]` instead of copying. Example: `kyo-web-online`.
3. **Consumer** — the deployable site. Composes engine output INSIDE the design-contract's visual shell. Wires the engine via npm dep + `--template-dir`. Owns routing, i18n surface, SEO graphs, deploy pipeline. Example: future `kyo-blog`.

**Cross-tier rules:**
- The engine repo MUST NOT vendor the design system — vendoring couples release cadence and violates the "framework-agnostic" claim.
- The design-contract repo MUST NOT depend on the engine — it's the spec, not a consumer.
- The consumer composes both via plain dependency arrows: `npm install <engine>` + read the design-contract session file when extending visuals.
- Sibling repos (not monorepo) is the current default. Monorepo (`apps/blog/` + `packages/engine/`) remains an option if release cadence tightly couples — re-evaluate per project.

**Anti-patterns:**
- "Just include the engine code in the consumer for now" — kills the engine's reusability claim.
- "Add a kyo-web-online-compatible template variant inside the engine repo" — pulls design into the framework-agnostic layer.

### dp-006: cross-session-reference-over-copy
**Date:** 2026-05-21 | **Status:** active
**Source:** `org-2-html.md` §1.10 (applied 2026-05-21), pattern grounds in `session-memory` skill rules

**When to use:** When a new session file needs to constrain its work by rules that live in another, already-canonical, session file (e.g., design rules, naming conventions, infrastructure decisions).

**Structure:**
- The new session declares a referenced-session section (e.g., `§1.10` in `org-2-html.md` references `kyo-web-online.md`).
- Each bullet in the referenced section is a *pointer with intent*: a brief 1-line summary of what's relevant, followed by `[session: <file>.md > §X.Y > <topic>]` resolving to the canonical rule.
- Body of the new session NEVER inline-copies the rule text. Updates to the rule propagate automatically because consumers re-read the reference target on demand.

**Reference syntax:**
- File level: `[session: kyo-web-online.md]` → loads file Summary.
- Section level: `[session: kyo-web-online.md > §1.5]` → loads one numbered section.
- Topic level: `[session: kyo-web-online.md > §1.5 > SCSS theming]` → loads the topic bullet.

**Density rule:** ≤20 references per context block. If a guideline can be stated in 1-2 lines, keep it inline. Use references when the full canonical explanation is 5+ lines.

**Anti-patterns:**
- Copying the referenced rule "for convenience" — duplication invariably diverges over time.
- Referencing a section that doesn't have a stable numeric id — section numbers MUST be stable in the canonical source.
- Over-referencing — if more than 20 references per block, the new session is probably under-scoped or should fold into the referenced session.

---

## 4. Shared State & Data Flow

*(No entries yet. Add when cross-repo data contracts or shared state flows are formalized.)*

---

## 5. Constraints & Limitations

### cl-001: figlet-smslant-12-char-banner
**Date:** 2026-05-21 | **Status:** active
**Source:** `org-2-html.md` Activity Log 2026-05-21 01:00 (figlet banner generation pass)

**Constraint:** Tier 1 file headers (`ad-001`) use the figlet `smslant` font. To fit the 4-line banner under 80 columns, the banner subject MUST be ≤12 visible characters (including the leading "THE ").

**Workaround when a longer name is needed:** Drop "THE " prefix; banners with no prefix can run up to ~16 chars before hitting the 80-col limit. The "THE " prefix is the default for the Tier 1 convention; the prefix-less variant is a documented escape hatch, not the norm.

**Naming theme:** Guardhouse / watchpost vocabulary. Current Kyonax-wide registry:

| Banner          | Used for                                            |
|-----------------|-----------------------------------------------------|
| THE WATCHTOWER  | `.github/workflows/ci.yml`                          |
| THE FOUNDRY     | `.github/workflows/publish.yml` / `release.yml`     |
| THE SEAL        | `.github/CODEOWNERS`                                |
| THE SHIELD      | `.github/SECURITY.{org,md}`                         |
| THE BLUEPRINT   | `.github/PULL_REQUEST_TEMPLATE.md`                  |
| THE PACT        | `LICENSING.org`                                     |
| THE LOGS        | `CHANGELOG.org`                                     |
| THE GATE        | `CONTRIBUTING.org`                                  |
| THE BEACON      | `README.org` (alongside the project's ASCII art)    |
| THE DESK        | `.editorconfig`                                     |
| THE GRAIN       | `.gitattributes`                                    |
| THE VOID        | `.gitignore`                                        |

New Tier 1 files should pick a single noun in the same register before generating their banner.

**Generation command:** `figlet -f smslant "THE <NOUN>"`. Verify the output is ≤4 lines and ≤80 cols before committing.

---

## 6. Reusable References

*(No entries yet. Add when reusable schemas, API contracts, or data shapes are formalized across Kyonax repos.)*
