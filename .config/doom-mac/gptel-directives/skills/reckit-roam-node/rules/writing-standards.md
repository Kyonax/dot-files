---
title: Writing Standards — Tone, Clarity, and Content Quality for Plan Sections
impact: HIGH
impactDescription: Plan nodes serve as the source of truth for commits, PR bodies, decision records, and validation. Vague, inconsistent, or poorly structured content propagates into weak commits, unclear PRs, and unreliable validation — defeating the documentation system's purpose.
tags: writing, tone, clarity, accuracy, consistency, voice, style, content-quality, documentation, commit, pr, development, user-centric, active-voice, terminology, structure, propose, outline, section-writing, requirements, decisions, qa-instructions, deployment-notes, unit-test-coverage, findings
---

This rule governs how content is written in any RECKIT plan section. Plans are not just records — they are the **single source of truth** the user pulls from to write commits (`COMMIT.org`), PR bodies (`PR.org`), validate work, and track progress. Vague writing in a plan propagates into vague commits and unclear PRs. Every sentence in a plan must earn its place.

## Guiding Principles

These four principles apply to ALL content written into any plan section:

| Principle       | Rule                                                                                                                                                                                                                              | Anti-Pattern                                                                                                                    |
|-----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------|
| **Clarity**     | Write in simple, clear, unambiguous language. One idea per sentence. No jargon without definition.                                                                                                                                | "The thing was updated to handle the case" — what thing? which case?                                                            |
| **Accuracy**    | Ensure all information — component names, file paths, alias paths, decisions, line numbers — is correct and verifiable against the codebase + session file.                                                                       | Guessing a path like `src/components/Hud.vue` when the actual path is `src/shared/components/hud/frame.vue`                     |
| **Consistency** | Use the same term for the same concept throughout the plan. If a component is `=<UiStatusDot>=`, never call it "the dot indicator" or "the recording light" interchangeably.                                                       | Switching between "context strip", "lower third", "news ticker", "context bar" for the same element                              |
| **Specificity** | Name files, components, classes, methods, decisions. Never say "the composable" when you can say `=useContextChannel=`. Never say "added styles" when you can say "added `.context-strip` with `transform: translateY(...)` opt-in halo via `var(--hud-halo-text)`". | "Fixed the issue with the strip" — which strip? what fix? where?                                                                |

## Voice and Tone

### Always Use Active Voice

The user is the actor. The plan documents what *will be* or *was* done, not what "happened."

**Incorrect:**
```org
The composable was refactored to be a singleton.
Styles were applied to the strip.
The bug was resolved.
```

**Correct:**
```org
Refactored =useContextChannel= to a module-level singleton (D1) — N consumers share one =BroadcastChannel= subscription.
Applied =transform: translateY(0)= + =opacity= animations to =.context-strip= and the reveal panel; no =height= or =filter= per frame.
Fixed sub-pixel iframe-scale jitter by reading container width via =getBoundingClientRect().width= instead of =clientWidth=.
```

### Be Terse, Not Verbose

Plans are reference documents, not narratives. Lead with the fact; add context only if non-obvious.

**Incorrect:**
```org
After thinking about how OBS browser sources work and how the landing page should communicate with them, I decided to use BroadcastChannel because it's a same-origin browser API that doesn't require a server and is supported by the OBS Chromium runtime.
```

**Correct:**
```org
*D1 — BroadcastChannel over OBS WebSocket relay* (2026-04-26)
- *Choice:* same-origin =BroadcastChannel= for landing→HUD control plane.
- *Why:* zero latency, zero server, OBS Chromium 32+ supports it. Frees the OBS WS budget (§1.14.5) for video state.
- *Alternatives:* OBS WS custom event (server schema, latency); URL hash (no event-driven update); SharedWorker (overkill).
```

### Use Technical Precision

When documenting decisions or implementation, include the **what**, **where**, and **why** — skip the **how** unless non-obvious.

| Include                   | Example                                                                            |
|---------------------------|------------------------------------------------------------------------------------|
| Component name (verbatim) | `=<UiStatusDot>=`, `=<HudFrame>=`                                                  |
| Composable name           | `=useContextChannel=`, `=useObsWebsocket=`                                         |
| File path                 | `=@kyonax_on_tech/sources/hud/context-screen.vue=`                                 |
| Alias path                | `=@composables/use-context-channel.js=`                                            |
| CSS class                 | `=.context-strip=`, `=.hud-group--top-left=`                                       |
| CSS custom property       | `=var(--hud-halo-text)=`, `=var(--hud-glow)=`                                      |
| Method or function name   | `=parseOrg()=`, `=cycleCodeReveal()=`                                              |
| Config or env             | `=OBS_CONFIG.host=`, `=VITE_OBS_PORT=`                                             |
| Reason for the decision   | "Used =BroadcastChannel= over OBS WS to keep the OBS WS budget free per §1.14.5"    |

---

## Section-Specific Writing Rules

### Summary Paragraph (top of plan)

**Purpose:** What this plan ships, why now, scope boundaries. ≤ 5 sentences.

- One sentence: what is being built.
- One sentence: why now (driving constraint or opportunity).
- One sentence: scope boundary (what this plan does NOT touch — pointer to OUT OF SCOPE).
- One sentence: cross-link to relevant references (architecture, conventions, session-file sections).

### REQUIREMENTS (Phase 1 — frozen at first commit)

**Purpose:** Capture what must be built, in GIVEN/WHEN/THEN form.

- Each requirement is `*R<n> — <short title>*`.
- One R = one user-observable behavior. Don't bundle multiple behaviors per R.
- GIVEN clauses describe preconditions; WHEN clauses describe actions; THEN clauses describe observable outcomes.
- Use AND clauses to chain related expectations under one R.
- Every component, file, alias, or technical term gets `=verbatim=`.
- 4–8 R-items is typical. >12 means the plan is too big — split it.

### OUT OF SCOPE (Phase 1)

**Purpose:** What this plan deliberately does NOT cover.

- Bullet list, terse. One thing per bullet.
- Phrase positively: "X is not covered in this plan" — not "we don't do X".
- Cross-reference future plans if the scope is deferred (e.g., "see backlog: future Plan #X").

### OPEN QUESTIONS (Phase 1, resolved in Phase 0 of TODO)

**Purpose:** Unknowns the user hasn't decided yet at planning time.

- Each is `*Q<n> — <question>*`.
- Always include a `*Recommendation:*` line — your lean.
- Use `[ ]` checkbox until resolved; flip to `[X]` and add `→ resolved by D<n>` when promoted to a decision.

### DECISIONS (Phase 1 + Phase 2 as questions resolve)

**Purpose:** Log of design choices with rationale.

- Each is `*D<n> — <title>* (<date>)`.
- Four bold field labels: `*Choice:*`, `*Why:*`, `*Alternatives:*`, `*Cross-ref:*`.
- `*Cross-ref:*` field links to session-file decision number when the decision is globally relevant (e.g., "session-file decision #132"); use "—" or omit if local-only.
- Deferred decisions read `(deferred — see Q<n>)` until Phase 0 closes them.
- New decisions appended chronologically.

### TRADE-OFFS (Phase 1)

**Purpose:** Costs paid by the chosen design.

- Bullets, each one trade-off.
- Distinct from Out of Scope: trade-offs are *paid* by the chosen design; out-of-scope items are *not built at all*.
- Phrase as a fact: "Code-reveal index resets on reload" — not as an apology.

### TODO PLAN TASKs (Phase 1, checked through Phase 2)

**Purpose:** Phased committable units.

- Phase 0 always = "Lock Decisions" if open questions exist. Otherwise skip.
- Each phase is a `**` heading with a name (e.g., `** Phase 1 — Org Parser + Content Storage`).
- Each task under a phase is a single committable line item.
- Verb-led: "Build", "Add", "Wire", "Test", "Update".
- One task = one logical unit. Avoid "Implement everything" lines.

### DESIGN AND ARCHITECTURE / DATA FLOW (Phase 1)

**Purpose:** Sketch the architecture so future-you (and reviewers) understand at a glance.

- Use ASCII trees inside `#+begin_src txt` blocks for data-flow diagrams.
- Describe the components, the data, and the events that move data between them.
- Mirror the style of session-file §3.x architecture diagrams.

### STRUCTURE AND FUNCTIONALITY (Phase 2)

**Purpose:** Architecture description for future reference. Input/Process/Output pattern.

**Writing rule:** Describe data flow, not implementation details. Someone reading should understand *what the feature does* without reading the code.

```org
** STRUCTURE AND FUNCTIONALITY
*** Input
- =.org= files at =@<brand>/data/contexts/*.org= discovered via =import.meta.glob=
- =BroadcastChannel('reckit:context-screen')= state from landing page

*** Process
- =parseOrg(raw)= splits the file into =title=, =subtitle=, =description=, =tags[]=, =code_blocks[]=
- =useContextChannel= owns =active_slug=, =visible= flags, =code_reveal_index=; persists in =localStorage=
- HUD source subscribes to channel; renders bound subtree per visible flags

*** Output
- =.context-strip= rendered with title/description/tags per visible flags
- =.reveal-panel= shown when =visible.code_reveal && code_blocks[code_reveal_index]= present
```

### DEPLOYMENT NOTEs (Phase 2)

**Purpose:** Everything needed beyond merging code.

- If nothing: write "No CI, release, or env changes required." Never blank.
- For RECKIT: typical entries are CHANGELOG entries, README markers, =.env.example= changes, version triad implications.

### UNIT TEST COVERAGE (Phase 2)

**Purpose:** Document what tests were written, what they cover.

```org
** UNIT TEST COVERAGE
*** =src/shared/utils/org.test.js= (new)
- parses =#+TITLE:= keyword
- parses =:tag1:tag2:= filetags
- extracts =#+BEGIN_SRC ... #+END_SRC= blocks with language tag
- handles missing fields gracefully (returns =null= for absent keys)

*** =src/shared/composables/composables.test.js= (extended, 33 → 38)
- =useContextChannel=: singleton identity contract
- =useContextChannel=: hydrates from =localStorage= at module load
- =useContextChannel=: =postMessage= on =setActiveSlug=
```

### QA INSTRUCTIONs (Phase 2)

**Purpose:** Step-by-step manual verification.

- Numbered steps. Each step has an action and an expected result.
- Include URLs, OBS scene/source names, breakpoints when relevant.
- Mark expected results with `*EXPECTED:*`.

```org
** QA INSTRUCTIONs
1. Run =npm run dev=. Open =http://localhost:5173/= in browser.
2. Navigate to the new "Contexts" section. Click on a context card.
3. *EXPECTED:* Card flips to =active= state.
4. In a second tab, open =http://localhost:5173/@kyonax_on_tech/context-screen=.
5. *EXPECTED:* The HUD renders the active context's title + description in the lower-third strip within ≤ 200 ms.
6. Click "hide title" toggle on the landing page card.
7. *EXPECTED:* Title disappears from the HUD; description + tags remain.
```

### FINDINGS (Phase 2)

**Purpose:** Retrospective. Surprises, gotchas, follow-ups discovered during the work. Feeds future plans.

**Writing rule:** Bullets, terse. Format: *what surprised me / what I learned*.

```org
** FINDINGS
- =BroadcastChannel= delivery is synchronous within the same origin — no race-condition guard needed for repeated rapid toggles.
- Vite's =import.meta.glob= with =query: '?raw'= adds the =.org= as a string at build time; HMR re-runs the parser. No file-watcher needed.
- Followup: when a context has 0 code blocks, the cycle-code button should be disabled (currently no-ops silently — confusing UX).
```

### INVESTIGATION NOTEs (Bug variant)

**Purpose:** Debugging diary.

- Initial section captures the investigation that led to ROOT CAUSE.
- After fix, append `** UPDATE [<date>]` sub-headings for any new findings (regression discoveries, related issues, root-cause refinement).

### COMMENTs (catch-all)

**Purpose:** Free-form notes that don't fit elsewhere.

- Use sparingly. If a comment recurs, promote it to a proper section.
- Prefix with date for posterity: `[<date>] <note>`.

### DELIVERABLEs (Phase 2 / post-merge)

**Purpose:** Concrete artifacts produced. Auto-fillable per `gh-parsing.md`.

```org
* DELIVERABLEs
- *Branch:* =context-screen=
- *Commits:* =a1b2c3d feat(context): add .org parser + channel composable=, =e4f5g6h feat(context): hud source + control surface=
- *PR:* =#7= — merged 2026-05-02
- *Files touched:* 14
- *Tests added:* 8 (5 in =org.test.js=, 3 in =composables.test.js=)
- *Session file refs:* §3.8 update (context-screen ready), decisions #133–#136
- *Tag:* part of =v0.5= release
```

---

## Terminology Consistency Rules

Once a term is introduced in a plan, use it identically throughout:

| Establish Once             | Use Consistently           | Never Switch To                                                  |
|----------------------------|----------------------------|------------------------------------------------------------------|
| `=<UiStatusDot>=`          | `=<UiStatusDot>=`          | "the dot", "status indicator", "recording light"                 |
| `=useContextChannel=`      | `=useContextChannel=`      | "the channel composable", "the BroadcastChannel hook"            |
| `=.context-strip=`         | `=.context-strip=`         | "the lower third", "the news ticker", "the context bar"          |
| `=@composables/use-X.js=`  | `=@composables/use-X.js=`  | "the composable file", "the hook"                                 |
| "lower-third HUD"          | "lower-third HUD"          | "lower third", "news lower third", "context HUD"                 |

**Exception:** First mention may include a parenthetical alias for clarity:
```org
Created =<UiStatusDot>= (the breathing recording-state dot) — outer span owns static halo, inner =.glow= span owns animated opacity.
```
After the first mention, use only the canonical form.

---

## The Propose-Before-Fill Workflow

When the user asks to fill Phase 2 sections (DOCUMENTATION subsections, FINDINGS, DELIVERABLEs after work proceeds):

1. **Read the plan node** — understand REQUIREMENTS, DECISIONS, OPEN QUESTIONS' resolution.
2. **Read git/gh state** — `git diff dev...<branch>`, `gh pr view <N>`, the actual changed files.
3. **Propose a structure** — present a bulleted outline of what each section will contain, with specific component names, files, and decisions called out.
4. **Await approval** — the user confirms or adjusts.
5. **Generate content** — write the full section content following all standards above.

**Why propose first:** The user may have context not visible in the code (rejected approaches, verbal agreements, scope changes during build). Proposing first catches misalignment before writing 200 lines.

**Exception:** If the user says "just fill it" / "write it all" without asking for a proposal, skip steps 3–4 and generate directly. The workflow is a default, not a gate.

## Cross-Linking Discipline

Every plan node MUST link in `RELEVANT LINKs` to:

1. The branch URL (`https://github.com/Kyonax/reckit/tree/<branch>`).
2. PR URL once opened.
3. Index Reckit (`[[id:93c9b466-676d-48bf-9d1b-ec8b93816b5d][Index Reckit]]`).
4. Reckit Architecture (`[[id:ac49fa6e-4520-47b4-b01a-477d7f135add][Reckit Architecture]]`).
5. Reckit Naming Conventions (`[[id:680e3a77-9f0f-477b-b9ec-94ce7a87416b][Reckit Naming Conventions]]`).
6. Session-file path with relevant section numbers (e.g., `§1.14`).
7. Any related plan nodes (parent/dependent plans).

This ensures that opening any plan node, the reader can navigate to the broader project context with one click.
