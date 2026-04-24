---
title: Architectural Knowledge Extraction Criteria — What to Store and What to Filter
impact: CRITICAL
impactDescription: Defines exactly what qualifies as reusable architectural memory versus implementation noise. Without these criteria, session files accumulate one-off fixes, debugging steps, and code-style rules that bloat context and drown out high-value decisions.
tags: extraction, criteria, filter, architectural, decision, pattern, data-flow, abstraction, naming, schema, constraint, reusable, noise, debugging, one-off, code-review, implementation
---

This rule defines the mandatory criteria for determining what knowledge extracted from a session qualifies as **architectural memory** — reusable decisions, patterns, and constraints that inform future work — versus **implementation noise** that should be discarded. Without clear extraction criteria, session memory files become dumps of everything that happened, defeating the purpose of precise knowledge retrieval.

## Core Principle: Architecture Over Implementation

Architectural memory captures **decisions that shape how systems are built**, not the specific code that implements them. The test: "Would this knowledge influence a decision in a future session that works on a different feature?" If yes, it's architectural. If it only matters for the specific feature that produced it, it's implementation detail.

## What to Extract (Inclusion Criteria)

### Category 1: Architecture Decisions

Decisions that establish patterns, constrain future work, or define system-wide behavior.

| Signal | Example | Why It Qualifies |
|---|---|---|
| Chose pattern A over pattern B with rationale | "CSS `max-height` accordion, not `TransitionExpand` — TransitionExpand causes content flash" | Future accordion work needs this decision |
| Established a data flow direction | "Cookie-based cross-app state transfer — Vuex `$router.push` doesn't survive page reloads" | Any future cross-app state transfer hits this constraint |
| Defined a system boundary | "CMS Partials render during SSR — global components' store dependencies must be satisfied in `serverPrefetch`" | Every new CMS Partial integration needs this |
| Set a cross-feature constraint | "`100dvh` for mobile viewports, not `100vh` + `env(safe-area-inset-bottom)` — viewport meta lacks `viewport-fit=cover`" | Any future mobile viewport work hits this |

### Category 2: Design Patterns

Reusable structural approaches that emerged from specific implementations but apply broadly.

| Signal | Example | Why It Qualifies |
|---|---|---|
| Component composition pattern | "Self-contained landmarks — each component owns `role='region'` + `aria-labelledby` in its own template, parent wrappers are purely structural" | Pattern applies to every new accessible component |
| State management pattern | "Cross-store data bridging — parent `serverPrefetch` copies data to dependent store modules before CMS Partials render" | Pattern applies to any multi-store SSR page |
| Integration pattern | "Experiment-gated redirect inside existing component — `defineAsyncComponent` + `v-if` on experiment flag, not new routes" | Pattern applies to any future experiment implementation |
| Error handling pattern | "`res.data \|\| {}` before destructuring CMS API responses" | Defensive pattern for all CMS API consumers |

### Category 3: Data Flow Decisions

How data moves through the system — sources, transformations, destinations.

| Signal | Example | Why It Qualifies |
|---|---|---|
| Data source resolution | "Navigation data is Tophat-driven via Data Tool — `dataToolSvc.getData({ mixinKey: 'sr-top-nav' })` stored in `siteNav` Vuex module" | Understanding data provenance prevents re-discovery |
| Transform chain | "CMS image URLs → strip query params → ImgBox handles crop/format" | Image pipeline knowledge prevents redundant CDN work |
| Ownership boundary | "`FeaturedServicesV2.selectService()` uses cookie + redirect — Vuex + `$router.push` doesn't survive cross-app reloads" | State transfer boundary affects all booking flow work |

### Category 4: Key Abstractions

Services, modules, global states, and shared infrastructure that future work will depend on.

| Signal | Example | Why It Qualifies |
|---|---|---|
| Global service | "`useScroll` composable — modern, reactive, 200ms throttled, SSR-safe. Codebase avoids CSS `position: sticky`" | Every sticky/scroll feature needs this |
| Shared module | "`.sticky-header-wrap.is-sticky` is the single site-wide sticky scaffold, gated on experiment B" | Any component adding sticky behavior below the nav needs this |
| Dead/misleading abstraction | "`--mr-sticky-header-height` is referenced but NEVER set — dead variable. `--mr-navigation-height` only measures nav, not full sticky wrap" | Prevents future developers from relying on broken abstractions |

### Category 5: Constraints and Non-Obvious Limitations

Performance, SEO, ADA, platform, or business constraints that constrain future decisions.

| Signal | Example | Why It Qualifies |
|---|---|---|
| Platform limitation | "CMS `parseUrl` validates segment count against Tophat `urlParameterList.length` — extra segments fail validation → 404" | Any future child route work hits this |
| SSR constraint | "`this.experiments` is `{}` during SSR — experiment B components only render after client `mounted()`" | Every experiment-gated component needs this |
| Accessibility constraint | "WCAG 2.5.3 — `<div role='link'>` for multi-content cards, not native `<a>`, when inner text doesn't match accessible name" | Pattern for all interactive card components |

### Category 6: Reusable Structures

APIs, schemas, contracts, and data shapes that future work will consume.

| Signal | Example | Why It Qualifies |
|---|---|---|
| API contract | "`colorbarCache` media objects shape: `{ _id, url, width, height, alt_text, aspects[], ... }`. Top-level `width`/`height` enable dynamic aspect ratio" | Any component consuming cache images needs the shape |
| CMS schema | "Tophat optional URL parameter: `urlParameterList: [{ name, optional }]` — makes `parseUrl` accept variable-length paths" | Schema knowledge prevents re-discovery of CMS routing rules |
| Route pattern | "Express `:path?` generic optional param — handler body unchanged, only uses `req.params.urlKey`" | Reusable pattern for any route needing sub-path validation |

---

## What NOT to Extract (Exclusion Criteria)

| Category | Example | Why It's Excluded |
|---|---|---|
| **Pure code-review rules** | "Always use brackets for if/else" | Belongs in a code-review or coding-standards skill, not session memory |
| **One-off fixes** | "Changed line 377 from `/shop/all-hair-color` to `/shop-all`" | The fix is in the code; the commit message has the context |
| **Temporary debugging steps** | "Added `console.log` to trace the SSR hydration mismatch" | Debugging artifacts have zero future value |
| **Implementation noise** | "Component X has 14 tests passing" | Test counts change constantly — not architectural |
| **Formatting/style decisions** | "Alphabetize CSS properties" | Belongs in a style guide skill, not session memory |
| **PR workflow details** | "Labels: DOTCOM TEAM, Pending Code Review" | Process metadata, not architectural knowledge |
| **Git branch/commit specifics** | "Branch: `DOTCOMPB-7289_new_feat`, commit `329bce55079`" | Git history is authoritative for this — no need to duplicate |
| **Person-specific review comments** | "Andris said to use optional chaining on line 42" | If the pattern is generalizable, extract the pattern, not the comment |

---

## The Extraction Decision Flowchart

```
For each piece of session knowledge:
    │
    ├── Does it influence decisions in FUTURE sessions on DIFFERENT features?
    │   ├── Yes → Candidate for extraction
    │   └── No → Discard (implementation noise)
    │
    ├── Is it already captured in a loaded skill's rule files?
    │   ├── Yes → Discard (redundant — skill is authoritative)
    │   └── No → Continue evaluation
    │
    ├── Can it be derived from reading the current codebase?
    │   ├── Yes, trivially → Discard (code is authoritative)
    │   └── No, or only with significant effort → Extract
    │
    ├── Is the knowledge generalizable beyond its original context?
    │   ├── Yes → Extract and generalize before storing
    │   └── No, but it's a critical constraint → Extract as-is with context flag
    │
    └── Which extraction category does it fit?
        ├── Architecture Decision → Store with rationale + date
        ├── Design Pattern → Store with pattern name + when to apply
        ├── Data Flow → Store with source + transform + destination
        ├── Key Abstraction → Store with location + behavior + gotchas
        ├── Constraint → Store with scope + workaround if any
        └── Reusable Structure → Store with shape + consumer guidance
```

---

## Correct vs. Incorrect Extraction

### Example 1: Decision vs. Implementation

**Incorrect (too specific):**
```markdown
## Architectural Decision
HairColorBarLocationHeroV2 uses local `matchMedia('max-width: 559px)')` for
mobile detection because the global getter is 960px.
```

**Correct (generalizable):**
```markdown
## Architectural Decision: Local matchMedia for Non-Standard Breakpoints
**Pattern:** Use `global/isDesktop` (960px+) for standard desktop detection.
Use local `matchMedia` only when the breakpoint differs from the global getter.
**Rationale:** Avoids duplicating resize listeners while allowing feature-specific thresholds.
**Constraint:** Different breakpoints serve different UI decisions — only replace when thresholds exactly align.
```

**Why:** The first version is tied to one component. The second captures the reusable pattern that applies to any future component needing a custom breakpoint.

### Example 2: Constraint vs. Debugging Step

**Incorrect (debugging noise):**
```markdown
## Constraint
Added `console.log` in `serverPrefetch` to trace why FeaturedServicesV2 rendered empty.
Found that `hairColorBarBooking.location` was null during SSR.
```

**Correct (constraint):**
```markdown
## Constraint: CMS Partial Store Dependencies in SSR
CMS Partials render during SSR. Their globally registered components execute
lifecycle hooks and access store state during server rendering. If a component
inside a partial accesses store state that hasn't been populated, SSR crashes with 500.
**Required pattern:** Parent `serverPrefetch` must bridge data to dependent stores
before the partial's own `serverPrefetch` runs.
```

**Why:** The debugging steps are ephemeral. The underlying constraint — that CMS Partials execute during SSR and need pre-populated store state — is a permanent architectural fact that affects every future partial integration.
