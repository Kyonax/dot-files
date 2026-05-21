<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the **CV Bullet Refinement** session. Load it at the start of every conversation to gain full context without re-discovering anything. Read sections in order on first load — after that, reference them by number as needed. The session refined the user's personal CV (`cristian_d_moreno_jsfr.org`) by abstracting and applying a reusable **5-layer achievement bullet pattern** to four work experiences (Madison Reed / Agile Engine, Zerønet Labs, Softtek, Cabeza Rota — Digital Marketing & Web Developer), rewrote the top description, fixed the org-mode → LaTeX → PDF export pipeline (font / fontspec / Unicode fallback), and expanded the Skills & Tools table with the Madison Reed stack + AI tooling. As of 2026-05-09 the CV's three primary professional experiences (MR, Zerønet, Softtek) and one of the three Cabeza Rota roles (Digital Marketing & Web Developer) are fully refined; the **two remaining Cabeza Rota roles are out of scope** per user (Senior Frontend Web Developer 2021–2023 and Growth Marketer & Web Developer 2020–2021 stay as-is unless reopened).

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | The 5-layer achievement bullet pattern, conciseness rules, what-not-to-do list, hard user preferences (em-dash ban, "Led" honesty rule, no soft skills in tools table, no internal-tool naming, generic "documentation" instead of roam/Org), context-questions-before-refining protocol. | Before refining any future CV bullet, drafting a new bullet, or auditing existing copy. |
| **2. Session Overview** | Project scope (single CV file + LaTeX setup), 4 refined experiences + 1 description rewrite + LaTeX pipeline fix, pending Cabeza Rota roles (out of scope), key decisions (skills-table contents, AI placement, em-dash ban). | When starting a new task in this session. |
| **3. Implementations** | Per-experience refinement detail (bullets pre/post + outcome map), LaTeX export pipeline fix (font install + setup-file cleanup), Skills & Tools table additions, description rewrite. | When resuming a specific refinement or referencing the exact bullet shapes. |
| **4. File Index** | Path table for the CV org file, the LaTeX setup file, the user's Doom Emacs config, the brain.d roam-nodes root. | When reading, editing, or locating any file touched in this session. |
| **5. Last Interaction** | What was just completed (Cabeza Rota Digital Marketing & Web Developer refinement, 2026-05-09); pending work; entry point for resuming. | At conversation start. |
| **6. Activity Log** | Datetime-stamped table of every meaningful event in this session. | When you need exact "what was done when". |

**Operational Rule:** When the user says "refine the next experience", apply the 5-layer pattern from §1.1 and **always ask context questions per bullet first** (§1.4) — never guess showcase intent. When the user says "audit", run a conciseness pass against the rules in §1.2–1.3. **Hard rule: NEVER use the em-dash `—` character** anywhere in user-facing copy (memory of repeated user correction). When uncertain about a leadership claim ("Led", "Owned"), ask before writing — the user has corrected unfounded leadership claims twice in this session.

**Key principle:** Data may appear in multiple sections with different framing. §1 frames knowledge as a *rule to follow*; §2 as *context to understand*; §3 as an *implementation to reference*. Each section answers a different question about the same knowledge.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `session-reset` (this file). The 5-layer achievement bullet pattern (§1.1) is the canonical reusable contribution of this session and should be applied to *any* future CV refinement work.

### 1.1 The 5-Layer Achievement Bullet Pattern (canonical)

Every CV bullet is a single sentence with up to five layers, in this order:

| # | Layer | Purpose | Examples |
|---|---|---|---|
| 1 | **Action verb** (decisive, past tense) | Lead with ownership; never duty-list. | `Architected, Designed, Drafted, Captured, Integrated, Made, Led, Owned, Built, Migrated, Engineered, Refined, Partnered, Containerized, Delivered`. **Never:** *Worked on / Helped with / Was responsible for / Collaborated* (alone, without a concrete artifact). |
| 2 | **Concrete artifact / practice** | What you actually produced. | architecture decisions, ACs, TDD tests, e2e suites, reusable components, regression baseline, SDK wrappers, perf baselines, sprint planning system, CI pipelines |
| 3 | **Tools embedded inline** | Tech as the *medium* of the action, not a trailing list. | `wrote *TDD* unit tests with *Vitest* upfront` — never `Used *Vitest, Playwright, ESLint*…` |
| 4 | **Mechanism (optional, only when the *how* is the cleverness)** | A `by …` / `through …` / participle clause that explains the approach. | `by abstracting repetitive patterns`, `capturing baselines from the legacy page`, `wrapping each in CMS-driven abstraction layers` |
| 5 | **Distinct outcome** | Each ending must be a *different shape* of value. | speed, confidence, removed bottleneck, measurable lift, no eng tickets, no PR-per-change, AI/crawler citations, reduced rework |

**Skeleton:** `[Action verb] [concrete artifact] [tools embedded inline], [optional: by/through HOW], [so/distinct outcome].`

### 1.2 Hard "do not" rules (compiled from user corrections)

1. **Never use em-dash `—`** anywhere. Replace with a comma, full stop, or `,` + clause. *(User corrected this 3 times.)*
2. **Don't claim leadership unless it actually happened.** "Led" / "Owned" require explicit confirmation from the user. The Madison Reed bullets specifically dropped "Led" because the user was a senior IC, not a lead. Softtek B5 (onboarding) and Softtek B6 (PR reviews) use "Led" / "Owned" because the user confirmed those.
3. **No buzzwords / corporate beautification.** Banned: *world-class, hand-in-hand, drove (in lead sense), strong ownership, consistent results, robust, beautiful, stunning, beauty industry client, fully functional* (when implied).
4. **No internal tool / skill / script names.** When describing a custom AI skill or script, describe the *logic implemented*, not the proprietary product name. "Custom AI skills with three-tier detection" — yes. Naming the skill — no.
5. **No private vocabulary.** Generalize "roam nodes / org-roam / org-mode planning files" to **"planning documentation"** or **"architecture documentation"**. Recruiters won't know what those are.
6. **No soft skills in the Skills & Tools table.** *Leadership, Mentoring, PR Reviews* were removed. *Documentation, Agile/SCRUM, TDD* are kept (methodologies/practices, like TDD). Rule of thumb: only tools and methodologies, no people-skills.
7. **Don't restate the same outcome shape across two bullets.** If two bullets both end on "improving performance" or "reducing time", the second one is wasted. Each ending must be context-specific.
8. **Don't list `tools like X`.** Just say `*X*`. The phrase "tools like" is filler.

### 1.3 Conciseness rules

- **Drop trailing scope-specifiers when context already implies them.** *(Pattern abstracted from user's example: "for every requirement" can go because AC-context already implies it.)* Other examples cut: *"on every new build"* after "recurring", *"during the switch"* after "cutover", *"on each page"* when SEO context is given, *"from the development cycle"* when "repetitive patterns" already implies it.
- **Drop redundant adjectives** when an earlier word already implies them. `clean architectural patterns` → `clean patterns` if `Architected` is in the same sentence. `solid component-composition patterns` → `component-composition patterns`. `fully functional e-commerce site` → `e-commerce site`.
- **Cut prepositional fluff.** "on implementations for a redesign" → "for a redesign" if the redesign already implies multi-implementation scope.
- **Abbreviate where consistent.** `acceptance criteria` → `ACs`, `end-to-end` → `e2e`, `operating system` → `OS`. Once an abbreviation appears, reuse it.
- **Keep load-bearing words.** *"before merging"* (gate condition), *"accurately"* (user's priority on AI work), *"(Emacs)"* after GPTel (recruiter disambiguation), full names like *Core Web Vitals* (ATS keyword matching, not "CWV").
- **Avoid "the work" / vague nouns** unless the surrounding clause grounds them.

### 1.4 Context-questions-before-refining protocol

Before rewriting any new experience block, ask the user one short question per bullet:

```
Bullet B<n>: what is the *showcase intent* — what do you want this bullet to prove?
```

The user will answer in 1–2 lines. **Do not guess.** Two exceptions:
1. The user explicitly says "do it without asking" (e.g., the Cabeza Rota Digital Marketing pass on 2026-05-09).
2. The bullet is being trimmed for conciseness only (no semantic change).

If you guess, the user will reject — there's a documented pattern of corrections in this session ("I never said I Led", "again it started horrible", "the focus is awful explained", etc.).

### 1.5 Pre-write sanity checklist (per bullet)

Before committing a bullet:

1. Does the lead verb describe **me doing** something, or just narrate?
2. Is the achievement specific enough that a stranger to the project gets the **outcome**?
3. Does the ending differ from the previous bullet's ending in **type** of value?
4. Are all named tools/SDKs ones the user **actually touched**?
5. If I stripped to **{action + artifact + tech + outcome}** — does it still hold? If not, the rest is filler.

### 1.6 Description / summary rules

The CV summary at the top follows the same conciseness rules. Closing lines like *"Consistently delivers results in remote environments with strong ownership"* are buzz and were replaced with concrete framing (`Efficient on remote work environments.`). AI craft should appear *seamlessly inside* a workflow sentence, not as a headline closer.

---

## SECTION 2: SESSION OVERVIEW

> Overall context, purpose, and scope of the CV refinement session.

### 2.1 Purpose

Refine the user's personal CV to match a single canonical bullet structure (5-layer achievement pattern), strip buzzwords and filler, embed the Madison Reed (Agile Engine) experience accurately, and ensure the org-mode → LaTeX → PDF export pipeline works end-to-end with all Unicode characters used.

### 2.2 Scope

| Item | Type | Summary | Status |
|---|---|---|---|
| `mr-experience` | new entry | Madison Reed / Agile Engine 2025–Present, 7 bullets, full Vue 3 + AI + SEO/AEO + CMS + SDK integration scope. | DONE (2026-05-09) |
| `zeronet-refine` | rewrite | Zerønet Labs 2017–Present, 6 bullets, freelance/agency tech-adaptability theme. | DONE (2026-05-09) |
| `softtek-refine` | rewrite | Softtek 2023–2025, 8 bullets, Shopware migration + DevOps + onboarding leadership. | DONE (2026-05-09) |
| `cabeza-dm-refine` | rewrite | Cabeza Rota — Digital Marketing & Web Developer 2021–2021, 3 bullets, analytics-driven design + agency-stack adaptability + reusable components. | DONE (2026-05-09) |
| `description-rewrite` | rewrite | Top summary paragraph: AI tooling woven into workflow sentence, "Efficient on remote work environments" closer, technical-debt re-anchored to refactors (not migrations), migrations reframed as "preserve full functionality". | DONE (2026-05-09) |
| `skills-table-expand` | additions | New rows: Vue 3, Vuex, Vue Router, Pug, Stylus, Vitest; Playwright, E2E Testing, TDD, Express, MongoDB, MySQL; JSON-LD, SEO, AEO, Lighthouse, Core Web Vitals, ADA Accessibility; Birdeye, Dynamic Yield, Dash Hudson, Segment, Sentry, FedCM; Claude Code, GPTel (Emacs), LiteLLM, GPT, Gemini, Grok; AI Skills, AI Workflows, Prompt Engineering, LLM Orchestration, Storybook, Prettier; Vue Test Utils, Swiper, Dayjs, Figma, Documentation, Agile/SCRUM. Removed: Leadership, Mentoring, PR Reviews. | DONE (2026-05-09) |
| `latex-pipeline-fix` | infra fix | Cleaned `cv-latex-export.org` (removed pdflatex-only packages mixed with fontspec), installed Carlito font (`brew install --cask font-carlito`), added Apple Symbols Unicode fallback for `→`. | DONE (2026-05-09) |
| `cabeza-fe-senior` | (out of scope) | Cabeza Rota Senior Frontend Web Developer 2021–2023. User explicitly declined to refine ("Im not going to refine the other experiences") — for the *English Frontend* CV only. | SKIPPED (English Frontend) |
| `cabeza-growth` | (out of scope) | Cabeza Rota Growth Marketer & Web Developer 2020–2021. User implicit decline for the *English Frontend* CV. | SKIPPED (English Frontend) |
| `spanish-fullstack-cv` | new | Spanish Full Stack CV (`2025-06-24-crstian_david_moreno_js-full-es.org`) refined in full: 6 experiences (MR, Zerønet, Softtek, Cabeza Rota Senior FS, Cabeza Rota DM, Cabeza Rota Growth) all rewritten to 5-layer pattern in Spanish, description rewritten, skills table expanded with new MR + AI rows + Spanish-translated entries (19 rows total), SETUPFILE path fixed (`./latex/...` → `../../latex/...`), location updated Villavicencio → Bogotá. | DONE (2026-05-09) |

### 2.3 Key Decisions (Session-Wide)

1. **5-layer pattern is canonical** (2026-05-09). Action → artifact → tech inline → optional mechanism → distinct outcome. Each ending must be context-specific. *(See §1.1.)*
2. **Em-dash `—` is permanently banned** in CV copy (2026-05-09, user corrected at least 3×). Use commas. Apple Symbols fallback in LaTeX setup is for `→` only — em-dashes shouldn't reappear in source.
3. **AI placement** in description: woven into workflow sentence (`integrating AI tooling to automate repetitive work and keep delivery fast`), not the closing line (2026-05-09). The closer is `Efficient on remote work environments.` — short, concrete, no buzz.
4. **"Reduce technical debt" stays anchored to refactors, not migrations** (2026-05-09). Migrations get a separate framing: "platform migrations that preserve full functionality" (matches actual Softtek delivery).
5. **Skills table is technical-only** (2026-05-09). Soft skills (Leadership, Mentoring) and process activities (PR Reviews) were removed. Methodologies (TDD, Agile/SCRUM, Documentation) stay.
6. **Madison Reed bullets describe a senior IC, not a lead** (2026-05-09). "Led" was rejected by the user for that experience. "Drafted, Designed, Built, Architected, Integrated, Made, Captured, Improved" carry the work.
7. **Custom AI skills are described by their logic, not by name** (2026-05-09). The bullet says "custom skills with three-tier detection (brand → project → tech-stack)" — never names the skill (`code-review`, `mr-dotcom-dev`, etc.).
8. **Org-mode / org-roam vocabulary is generalized** to "planning documentation" / "architecture documentation" (2026-05-09). Recruiters don't know those tools.

### 2.4 Pending Work

Nothing tracked in this session is pending. Two CV experiences remain in their original (pre-refinement) state but were explicitly declined by the user:

- **Cabeza Rota Senior Frontend Web Developer (2021–2023)** — 4 bullets, mix of duty-listing + buzz, would need a 4-question context pass before refining. User said "Im not going to refine the other experiences" on 2026-05-09.
- **Cabeza Rota Growth Marketer & Web Developer (2020–2021)** — 4 bullets covering social media growth + brand websites. Same user decision applies.

If reopened: follow §1.4 (ask context questions per bullet first), distinguish from earlier-period agency work in Cabeza Rota DM (B2 there is `agency stack-matching`), distinguish growth-marketing bullet from frontend-dev bullets.

---

## SECTION 3: IMPLEMENTATIONS

> Per-experience refinement detail. Each subsection captures the final bullet shape and the distinct outcome each bullet anchors on.

### 3.1 Madison Reed (Agile Engine) — 2025–Present
**Created:** 2026-05-09 | **Last updated:** 2026-05-09
**Status:** Final form. 7 bullets.

Outcome map (each ending is a different *kind* of value):

| # | Action | Artifact | Tech | Distinct outcome |
|---|---|---|---|---|
| B1 | Drafted | architecture decisions + ACs + tests | Vue 3, Vitest, ESLint, Playwright | PR reviews focused, no AC/code-quality rework |
| B2 | Captured | legacy regression baseline | Playwright | Confidence to ship redesign across every flow |
| B3 | Designed | AI dev workflow | Claude Code, GPTel, GPT, Gemini, Grok, LiteLLM | Engineering time on design, not scaffolding |
| B4 | Improved | data-driven structured markup | JSON-LD, SSR, ADA, h1→h2→h3 | Rich-result eligibility, AI-engine citations, conversion |
| B5 | Architected | reusable components | Vue 3, Pug, Stylus, Express SSR, Mongo CMS, Vite, Webpack 4, Storybook | Content updates without engineering tickets, blocks scale to new pages |
| B6 | Integrated | SDK abstraction layers | Birdeye, Dynamic Yield, Dash Hudson, FedCM, Segment | A/B + rollouts via config, no PR per change |
| B7 | Made | perf as redesign objective | Lighthouse, CWV | Measurable gains over previous design |

Key decisions specific to this entry:
- **Title**: `*Agile Engine - Madison Reed*` (user added the dash to the company line on 2026-05-09).
- **Senior IC framing, not lead**: "Led" and "Collaborated" were both rejected for B1. The bullets describe individual contributions to a shared frontend team.
- **AI workflow description (B3)**: must show *understanding and selection* of multiple LLMs by task strengths, not just "combining them". The user-correct framing is "abstracting repetitive patterns into custom skills, scripts, and tooling that automate the work accurately, picking [LLMs] per task by strengths and efficiency".
- **SEO/AEO bullet (B4)**: *don't* list specific JSON-LD schema names (FAQPage, HairSalon, BreadcrumbList, Product). The achievement is the *data identification* per page, not the schema list. ADA is integrated into the same sentence (semantic structure) — not a separate bullet.
- **CMS architecture bullet (B5)**: emphasizes business teams configuring without engineering tickets, and components scaling to new pages. *Don't* claim "marketing teams own end-to-end" — that's overstated; new layouts still need engineering.
- **SDKs bullet (B6)**: "vendor swaps" was rejected as overstating — actual achievement is A/B experiments + rollouts + integration tuning via config (not vendor replacement).
- **Performance bullet (B7)**: every redesign must beat the legacy baseline before merge. Bottlenecks fixed *in the same ticket*, not as follow-up.

### 3.2 Zerønet Labs — 2017–Present
**Created:** 2026-05-09 | **Last updated:** 2026-05-09
**Status:** Final form. 6 bullets.

| # | Action | Artifact | Tech | Distinct outcome |
|---|---|---|---|---|
| B1 | Delivered | frontend solutions | React, Next, Vue, Webpack, Vite, Gulp, Grunt, Vanilla JS | Adaptability — match each client's existing stack |
| B2 | Built | reusable npm packages + UI libraries | React, Vue, Next.js, semantic-release, GitHub Actions | Velocity + visual consistency + zero component duplication |
| B3 | Built | custom perf libraries | (frontend perf bottlenecks) | Cut load times, never re-solve the same problem |
| B4 | Designed | modular project architectures + SASS/SCSS systems | SASS/SCSS, design patterns | Long-term maintainability, refactors don't tear out foundations |
| B5 | Containerized | dev environments + CI pipelines | Docker, docker-compose | One-command onboarding, identical envs across OSes |
| B6 | Built | automated bots + backend services | Discord, Python, TypeScript, MySQL, MariaDB | Operational automation, manual flows become self-serve |

Key decisions specific to this entry:
- **B1 vs Cabeza Rota DM B2**: both are about tech-adaptability. Zerønet's framing is *direct freelance with each client's team and constraints* (B1 ends "instead of forcing a single template"); Cabeza Rota DM's framing is *agency context, marketing websites and landing pages* (different scope/output).
- **B2 vs MR B5**: both are about reusable components. Zerønet's is *portable npm packages with semantic-release/CI for cross-project reuse*; MR's is *in-project Vue 3 components driven by CMS configuration*.
- **B6 (Discord bots)**: achievement framing is "self-serve automation" + "freeing operators from constant manual attention" — not specific to Discord-only mechanics.

### 3.3 Softtek — 2023–2025
**Created:** 2026-05-09 | **Last updated:** 2026-05-09
**Status:** Final form. 8 bullets.

| # | Action | Artifact | Tech | Distinct outcome |
|---|---|---|---|---|
| B1 | Migrated | admin platform → e-commerce site | Shopware (Symfony + Vue.js) | Customer purchase flow fully automated, no business operation lost |
| B2 | Designed | SASS/SCSS pattern library + modular architecture | SASS/SCSS, PostCSS | Design changes propagate without manual rework, scales with storefront |
| B3 | Engineered | custom frontend solutions inside Shopware OOB | Shopware | Full design fidelity instead of cutting to platform defaults |
| B4 | Refined | sprint planning + work distribution | Azure DevOps, Agile | Steady velocity even when scope shifts mid-sprint |
| B5 | Led | onboarding (walkthroughs + documentation) | (frontend stack from earlier) | Joiners ramp up without constant 1:1 time |
| B6 | Owned | daily PR reviews | Unit Testing | Common issues caught before reaching production |
| B7 | Containerized | development + CI pipelines (prod/dev/staging) | Dockware, docker-compose, Azure DevOps | One-command Shopware envs + clean merge gate |
| B8 | Partnered | API contracts (REST + GraphQL) | REST, GraphQL | Integration sprints don't stall on contract mismatches |

Key decisions specific to this entry:
- **"Led onboarding" (B5) and "Owned PR reviews" (B6)** are explicitly user-confirmed leadership claims for this role. Other bullets use individual-contributor verbs.
- **B2 vs Zerønet B4**: both are about modular architecture. Softtek's adds *cross-browser via PostCSS* + *e-commerce platform context* (storefront scale); Zerønet's emphasizes *long-term refactor resilience* (no foundational rip-out).
- **B7 absorbs all containerization + CI mentions** — the original draft had Docker/CI scattered across 3 bullets (B4, B7, B8). Consolidated into one.
- **B8 dropped Docker/local-env mention** — covered in B7. B8 is purely about cross-team API contract design.
- **"Dockware Images from Docker, docker-compose"** in the original was doubled up. Final form: *Dockware* (Shopware-tuned Docker images) + *docker-compose*.

### 3.4 Cabeza Rota — Digital Marketing & Web Developer (2021–2021)
**Created:** 2026-05-09 | **Last updated:** 2026-05-09
**Status:** Final form. 3 bullets.

User instruction was "do without asking for context" — bullet showcase intent inferred from existing copy + prior pattern.

| # | Action | Artifact | Tech | Distinct outcome |
|---|---|---|---|---|
| B1 | Built | data-driven design loop | A/B Testing, GA4, Hotjar | Measurable engagement + conversion gains, not guesswork |
| B2 | Built | marketing websites + landing pages (agency clients) | Next, React, Vue, Nest, Redux, SASS/SCSS, Tailwind, Bootstrap, Wordpress | Fast launches without sacrificing scalability |
| B3 | Designed | reusable custom components | (frontend stack from B2) | Faster features + pattern consistency as codebase grows |

Typos fixed in this pass: `campagins` → bullet rewritten without the word; `scalabale` → `scalability`; `well structured user flows experience` → rewritten.

### 3.5 Top Description (Summary Paragraph)
**Created:** 2026-05-09 | **Last updated:** 2026-05-09
**Status:** Final form.

**Final shape:**
> *Senior FullStack Web Developer with 8 years of experience delivering scalable, adaptable, and high-performance frontend solutions for national and international clients. Strong background in defining workflows, planning, architecting web systems, and integrating AI tooling to automate repetitive work and keep delivery fast. Skilled on performance analysis, optimization, automated testing, accessibility, structured-data SEO, leading refactors to reduce technical debt, and platform migrations that preserve full functionality. Efficient on remote work environments.*

Decisions:
- **Title shifted to "Senior FullStack"** (from "Senior Frontend") on 2026-05-09 by user edit (linter-applied).
- **Closing line**: `Efficient on remote work environments.` — full stop. No AI, no buzz, no "ownership". *Several earlier drafts ("Comfortable across…", "Works remote and end-to-end…", "Self-organizing in remote, async-first teams") were rejected as buzzy.*
- **AI woven into sentence 2**: `…architecting web systems, and integrating AI tooling to automate repetitive work and keep delivery fast.` — slotted into the workflows clause, not headlined.
- **"Reduce technical debt" anchored to refactors** (not migrations).
- **"Migrations" reframed**: `platform migrations that preserve full functionality` — matches Softtek B1 (admin → e-commerce, no business operation lost).

### 3.6 Skills & Tools Table — Madison Reed Stack + AI Tooling
**Created:** 2026-05-09 | **Last updated:** 2026-05-09
**Status:** Final form.

7 new rows added; 3 entries removed:

```
+ | Vue 3        | Vuex          | Vue Router         | Pug               | Stylus             | Vitest                  |
+ | Playwright   | E2E Testing   | TDD                | Express           | MongoDB            | MySQL                   |
+ | JSON-LD      | SEO           | AEO                | Lighthouse        | Core Web Vitals    | ADA Accessibility       |
+ | Birdeye      | Dynamic Yield | Dash Hudson        | Segment           | Sentry             | FedCM                   |
+ | Claude Code  | GPTel (Emacs) | LiteLLM            | GPT               | Gemini             | Grok                    |
+ | AI Skills    | AI Workflows  | Prompt Engineering | LLM Orchestration | Storybook          | Prettier                |
+ | Vue Test Utils | Swiper      | Dayjs              | Figma             | Documentation      | Agile/SCRUM             |
- (removed) Leadership, Mentoring, PR Reviews
```

LLMs are listed individually (not "GPT/Gemini/Grok" combined) for ATS keyword matching. Tools confirmed via codebase audit at `/Volumes/dev-partition/github-madison-reed/the-code/` (`package.json` for Storybook, Prettier, Vitest, Vue Test Utils, Swiper, Dayjs; `mr_modules/birdeye/` for Birdeye API wrappers; recent ticket nodes for Dynamic Yield + Dash Hudson + FedCM + Segment).

### 3.7 LaTeX Export Pipeline Fix
**Created:** 2026-05-09 | **Last updated:** 2026-05-09
**Status:** Fixed and verified setup. PDF export should produce no font warnings.

**Problem observed:**
> `Warning (ox-latex): PDF file produced with warnings: [Missing character(s): please load an appropriate font with the fontspec package]`

**Three issues mixed in one error stream**, only the third blocks export:
1. **Websocket errors** — `org-roam-ui` reconnect noise. Ignorable. Toggle via `M-x org-roam-ui-mode` to silence.
2. **Tree-sitter TSX grammar missing** — unrelated to LaTeX export. Fix by `M-x treesit-install-language-grammar RET tsx RET`, accept default URL, type `tsx/src` for source dir.
3. **Font / Unicode failure (the actual blocker)** — pdflatex-only packages mixed with fontspec, Carlito font not installed, no fallback for `→`.

**Fix applied to `/Users/col-ae-052/.brain.d/latex/cv-latex-export.org`:**
- Removed `\RequirePackage{etex}`, `\usepackage{lmodern}`, `\usepackage[T1]{fontenc}`, `\usepackage[AUTO]{inputenc}`, duplicate `\usepackage{etex}` — these are pdflatex packages that conflict with `fontspec` under xelatex.
- Kept `\usepackage{fontspec}` + `\setmainfont{Carlito}` + `\newfontface\boldfont{Carlito}` + `\newfontface\italicfont{Carlito}`.
- Added `\usepackage{newunicodechar}` + `\newfontfamily{\fallbackfont}{Apple Symbols}[Scale=MatchLowercase]` + `\DeclareTextFontCommand{\textfallback}{\fallbackfont}` + `\newunicodechar{→}{\textfallback{→}}`.

**Font installation:** `brew install --cask font-carlito` ran on 2026-05-09. Files now in `~/Library/Fonts/Carlito-{Regular,Bold,Italic,BoldItalic}.ttf`.

**Unicode characters in the CV** (audit via Python script): `ñ` (Carlito ✓), `á` (Carlito ✓), `→` (Apple Symbols fallback ✓), `ø` (Carlito ✓). No em-dashes.

**Doom Emacs export config** (read from `config.org:2330`): `org-latex-pdf-process` runs `xelatex -shell-escape -interaction nonstopmode -output-directory %o %f` 3× (for cross-references). Trigger via `SPC m e` → `l p` (or `l o` to open after build).

---

## SECTION 4: FILE INDEX

> Quick reference for all files relevant to this session.

| File | Association |
|---|---|
| `~/.brain.d/roam-nodes/personal_stuff/2025-08-26-cristian_d_moreno_jsfr.org` | The English Frontend CV org file. All bullet refinements + description rewrite + skills table changes happen here. Tags `:JS-FR: :CV:`. Title now "Senior FullStack Web Developer". |
| `~/.brain.d/roam-nodes/personal_stuff/2025-06-24-crstian_david_moreno_js-full-es.org` | The Spanish Full Stack CV org file. Refined 2026-05-09 to mirror the English structure with Full Stack scope (backend/payments/DB skills retained). Tags `:JS-FS: :CV:`. SETUPFILE path corrected to `../../latex/cv-latex-export.org`. |
| `~/.brain.d/roam-nodes/personal_stuff/2025-08-27-cristian_d_moreno_js_full.org` | English Full Stack CV — **NOT refined yet** as of session reset. Still in old form (no MR entry, has typos like "Buils", "scalabale"). Tags `:JS-FS: :CV:`. If reopened, follow the same pattern as the Spanish Full Stack refinement. |
| `~/.brain.d/latex/cv-latex-export.org` | LaTeX setup file pulled in via `#+SETUPFILE: ../../latex/cv-latex-export.org`. Cleaned 2026-05-09 — fontspec only, no conflicting pdflatex packages, Apple Symbols Unicode fallback for `→`. |
| `~/Library/Fonts/Carlito-{Regular,Bold,Italic,BoldItalic}.ttf` | Carlito font installed via `brew install --cask font-carlito` 2026-05-09. Required for `\setmainfont{Carlito}`. |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/config.org` | Doom Emacs literate config. Lines ~2310–2360 hold the org→LaTeX export config (`org-latex-pdf-process` xelatex 3-pass, minted with shell-escape, hyperref colorlinks). |
| `/Volumes/dev-partition/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/cv-bullet-refinement-pattern.md` | This session file. |
| `~/.brain.d/roam-nodes/madison_reed/2025-11-18-index_madison_reed.org` | Madison Reed project index — sprint board, tickets, PRs, Figma links. Source for B6 SDK list (DOTCOMPB-7942 Google SSO, DOTCOMPB-8174 FedCM hotfix, DOTCOMPB-7527 Dash Hudson). |
| `/Volumes/dev-partition/github-madison-reed/the-code/` | Madison Reed monorepo. Used to verify tech stack (Vue 3.5, Pug, Stylus, Vite + Webpack 4, Express, MongoDB), Storybook setup, Birdeye custom wrapper modules (`mr_modules/birdeye/Birdeye*.js`). |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last (2026-05-09)

- **Spanish Full Stack CV refined in full** (`2025-06-24-crstian_david_moreno_js-full-es.org`): Madison Reed entry added (translated), all other experiences rewritten to 5-layer pattern in Spanish (Zerønet 7 bullets, Softtek 8 bullets, Cabeza Rota Senior FS 4 bullets, Cabeza Rota DM 3 bullets, Cabeza Rota Growth 5 bullets), description rewritten, skills table expanded to 19 rows with Full Stack-specific entries (Express.js, Symfony PHP, JWT, Patrones API, Cybersource, PayU, etc.) plus the new MR + AI block, SETUPFILE path fixed, location set to Bogotá.
- Both PDFs (English Frontend + Spanish Full Stack) exported successfully by the user.
- Earlier in the session: refined English Frontend CV (MR added + Zerønet + Softtek + Cabeza Rota DM), full audit + conciseness pass, expanded Skills table with MR stack + AI tooling, rewrote top description (AI placement + technical-debt re-anchoring), fixed LaTeX export pipeline (Carlito install + setup-file cleanup + Apple Symbols Unicode fallback for `→`).

### Pending / Not yet started

Nothing in scope. If the user reopens the remaining Cabeza Rota roles, follow §1.4 (ask context questions before refining) and the differentiation notes in §2.4.

### Where to resume

- If the user asks to **continue refining the remaining Cabeza Rota roles**: read §2.4 and §1.4. The Senior FE 2021–2023 has 4 bullets currently (multi-client high-performance + A/B testing + Agile collaboration + UI translation). The Growth Marketer 2020–2021 has 4 bullets currently (social growth + analytics-driven content + brand websites + custom components). Ask one short question per bullet about showcase intent.
- If the user asks to **export the PDF**: confirm Carlito font is still installed (`fc-list | grep -i carlito`), then `SPC m e` → `l p` from inside the CV org buffer.
- If the user asks to **add a new experience**: follow the 5-layer pattern. Open with the action verb + concrete artifact, embed tech inline, end on a distinct outcome that doesn't repeat any other bullet's outcome shape.
- If the user asks for a **new task**: check §2.4. Otherwise, treat as new — ask context questions first.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session. Newest row first.

| Datetime         | Duration | Type           | Reference | Description |
|------------------+----------+----------------+-----------+-------------|
| 2026-05-09 23:30 | —        | session-reset  | this      | Second session-reset — added Spanish CV refinement to scope/files/last-interaction; preparing to switch context to kyo-web-online session |
| 2026-05-09 23:00 | 1h       | refinement     | n/a       | Spanish Full Stack CV (`2025-06-24-crstian_david_moreno_js-full-es.org`) refined in full — MR entry added, all 5 experiences rewritten in Spanish, skills table 19 rows, SETUPFILE path fixed, location → Bogotá |
| 2026-05-09 22:30 | —        | documentation  | n/a       | User exported both English Frontend + Spanish Full Stack CV PDFs successfully (xelatex pipeline working with Carlito + Apple Symbols fallback) |
| 2026-05-09 22:00 | —        | session-reset  | this      | First session-reset — created session file, captured 5-layer pattern + all 4 refined experiences + LaTeX pipeline fix |
| 2026-05-09 21:30 | 0.5h     | refinement     | n/a       | Cabeza Rota — Digital Marketing & Web Developer refined (3 bullets, typos fixed, distinct outcomes) |
| 2026-05-09 21:00 | 0.5h     | configuration  | n/a       | LaTeX export pipeline fix — installed Carlito via brew, cleaned cv-latex-export.org, added Apple Symbols Unicode fallback for → |
| 2026-05-09 20:30 | 1h       | refinement     | n/a       | Conciseness audit pass #3 — 4 trims across MR/Zerønet (drop "as a development guide", "to validate each behaved as expected", "and customized", "from the legacy page", "supporting", "operational") |
| 2026-05-09 20:00 | 0.5h     | refinement     | n/a       | Conciseness audit pass #2 — drop scope-specifiers when context implies (pattern abstracted from "for every requirement" example) |
| 2026-05-09 19:30 | 0.5h     | refinement     | n/a       | Conciseness audit pass #1 — 17 trims across all 3 main experiences |
| 2026-05-09 19:00 | 0.5h     | documentation  | n/a       | Skills & Tools table expanded — 7 new rows (Vue 3 stack + AI tooling) + Storybook/Prettier/Vue Test Utils/Swiper/Dayjs/Figma; removed Leadership/Mentoring/PR Reviews |
| 2026-05-09 18:30 | 0.5h     | refinement     | n/a       | Top description rewrite — AI woven into workflow sentence, "Efficient on remote work environments" closer, technical-debt re-anchored to refactors, migrations reframed |
| 2026-05-09 18:00 | 1h       | refinement     | n/a       | Softtek — 8 bullets refined per 5-layer pattern, CI/Docker consolidation into single bullet, "Led onboarding" + "Owned PR reviews" confirmed |
| 2026-05-09 17:00 | 1h       | refinement     | n/a       | Zerønet Labs — 6 bullets refined per 5-layer pattern, distinct outcomes per bullet |
| 2026-05-09 14:00 | 3h       | refinement     | n/a       | Madison Reed — 7 bullets iterated through ~15 user-feedback rounds, final form: senior-IC verbs, no buzz, distinct outcomes, full Vue 3 + AI + SEO/AEO + CMS + SDK stack |
| 2026-05-09 13:30 | —        | research       | n/a       | Audited MR monorepo for tech stack confirmation (Vue 3.5, Pug, Stylus, Vite + Webpack 4, Express, MongoDB, Birdeye wrappers, Storybook) |
| 2026-05-09 13:00 | —        | planning       | n/a       | Started session — user requested CV refinement starting with Madison Reed addition |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
