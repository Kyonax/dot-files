---
title: Conflict Resolution and Edge Cases — Contradictions, Partial Decisions, and Consolidation
impact: MEDIUM
impactDescription: Defines how to handle contradictory architectural decisions across sessions, incomplete implementations, over-specific patterns, missing context, and rapid iteration sessions. Without these rules, architecture memory accumulates silent conflicts that produce contradictory guidance.
tags: conflict, resolution, contradiction, partial, decision, incomplete, over-specific, generalize, missing-context, uncertain, rapid-iteration, consolidate, edge-case, supersede, divergence
---

This rule defines how to handle edge cases that arise when extracting and maintaining architectural memory across multiple sessions — contradictory decisions, partial implementations, over-specific patterns, missing context, and rapid iteration sessions. Without explicit conflict resolution rules, architecture memory silently accumulates contradictions that produce inconsistent guidance for future work.

## Edge Case 1: Contradictory Decisions Across Sessions

**Situation:** Session A decides "use pattern X" and Session B decides "use pattern Y" for the same problem domain.

### Detection

Contradictions are detected during Step 2.5 (extraction) when a new candidate conflicts with an existing architecture memory entry:

| Signal | Example |
|---|---|
| Same `scope` or `context` with different `decision` | Session A: "Cookie-based state transfer" vs. Session B: "URL query parameter state transfer" |
| Same `when-to-use` with different `structure` | Session A: "CSS `max-height` accordion" vs. Session B: "`TransitionExpand` component" |
| Explicitly `superseded` reference | Decision says "supersedes ad-003" |

### Resolution Protocol

```
1. Check dates — is the newer decision explicitly aware of the older one?
   ├── Yes, and it says "supersedes" → Update old entry to status: superseded, link to new
   ├── Yes, and it says "this approach failed" → Same as above
   └── No — the sessions are unaware of each other → Flag and isolate (Step 2)

2. If flagged (neither session references the other):
   a. Create BOTH entries in architecture memory
   b. Mark both with status: "contested"
   c. Add a cross-reference between them:
      "See also: [ad-XXX] for an alternative approach to the same problem"
   d. Add a `conflict-note` field to both:
      "This decision conflicts with [ad-XXX]. They address the same problem
       with different approaches. Resolution pending."
   e. Surface to user: "[CONFLICT] Two decisions about {topic} —
      ad-XXX (Session A, date) vs ad-YYY (Session B, date). Which is current?"

3. When user resolves:
   a. Update the winning entry to status: active
   b. Update the losing entry to status: superseded-by: ad-XXX
   c. Remove conflict-note from both
   d. Add resolution date
```

### Conflict Entry Format

```markdown
### ad-015: transition-expand-for-accordions
**Date:** 2026-04-10 | **Status:** contested
**Source:** `feature-x-session.md`
**Conflict:** Contradicts [ad-001: css-accordion-over-transition-expand]. See conflict note.

**Context:** [...]
**Decision:** Use `TransitionExpand` component for accordion animations.
**Conflict note:** This decision conflicts with ad-001 (which chose CSS `max-height`
over `TransitionExpand`). ad-001 was made in the context of FAQ sections where the
flash was visible. This decision was made for a context where the element starts hidden.
Resolution pending — may be context-dependent (both valid in different scenarios).
```

---

## Edge Case 2: Incomplete Implementations (Partial Decisions)

**Situation:** A session makes an architectural decision but the implementation is not finished — the pattern hasn't been fully validated.

### Detection

| Signal | How to Detect |
|---|---|
| Decision was made but code wasn't written | Session Section 2.4 (Pending) lists implementation |
| Pattern was implemented in one place but not validated across features | `validated-in` has only 1 entry |
| Decision has caveats like "if this works" or "TBD" | Language indicates uncertainty |

### Storage Rule

```
1. Create the entry with status: "partial"
2. Add a `maturity` field:
   - "decided" = decision made, not yet implemented
   - "implemented-once" = implemented in one place, not yet validated
   - "validated" = implemented and confirmed in 2+ places
3. Add a `pending` field describing what's needed for full validation
```

### Partial Entry Format

```markdown
### ad-020: dual-route-section-extraction
**Date:** 2026-04-02 | **Status:** partial | **Maturity:** implemented-once
**Source:** `site-revolution-redesign.md`

**Context:** HcbLocationPageV2 needed to serve both location details and photos page.
**Decision:** Extract all visible content into `HcbLocationSections`, thin parent + `router-view`.
**Pending:** Only validated on location pages. Pattern should be tested on other
multi-section pages before promoting to "validated" status.

**Consequences:**
- Parent becomes thin (data loading + router-view only)
- All visible content moves to a dedicated sections component
- New sub-pages are separate components loaded by Vue Router children
```

### Maturity Promotion

```
partial (decided) → implemented-once (after first implementation)
    → validated (after 2+ implementations confirm the pattern)
    → active (after validation, status changes to active)
```

---

## Edge Case 3: Over-Specific Patterns

**Situation:** A pattern extracted from a session is too specific to one implementation to be reusable.

### Detection

| Signal | Example |
|---|---|
| Pattern references specific component names | "HairColorBarLocationHeroV2 uses local matchMedia at 559px" |
| Pattern references specific ticket IDs | "DOTCOMPB-7290 uses skeleton loading" |
| Pattern only works in one context | "This only applies when the parent is a grid with 6 columns" |

### Resolution: Generalize Before Storing

```
1. Strip specific component/ticket/file references
2. Abstract the underlying principle
3. Keep the specific implementation as a "validated-in" example
```

### Before (Over-Specific):
```markdown
### dp-015: hero-v2-local-match-media
HairColorBarLocationHeroV2 uses `window.matchMedia('(max-width: 559px)')` stored in data,
with listener in mounted and cleanup in beforeUnmount.
```

### After (Generalized):
```markdown
### dp-015: local-matchmedia-for-non-standard-breakpoints
**Name:** Local matchMedia for Non-Standard Breakpoints
**When to use:** When a component needs a breakpoint that differs from `global/isDesktop` (960px+).
**Structure:**
- Store `MediaQueryList` in `data` (SSR-safe — only assign in `mounted`)
- Add `change` listener in `mounted`, remove in `beforeUnmount`
- Use this instead of `window.resize` or duplicating the global resize listener

**Validated in:** HairColorBarLocationHeroV2 (559px mobile threshold for image layout switching)
```

---

## Edge Case 4: Missing Context

**Situation:** A decision or pattern was discovered during extraction but the rationale is not clear from the session.

### Detection

| Signal | How to Detect |
|---|---|
| Decision exists but no "why" is documented | Session mentions "we decided X" without explaining alternatives or rationale |
| Pattern is used consistently but never explained | Code shows the pattern but session doesn't discuss it |
| Constraint is discovered but cause is unknown | "This doesn't work" without "because..." |

### Resolution: Annotate as Uncertain

```
1. Create the entry with what IS known
2. Add an `uncertainty` field describing what's missing
3. Mark affected fields with `[UNCERTAIN]` prefix
4. Do NOT infer or guess — leave the gap explicit
```

### Uncertain Entry Format

```markdown
### ad-025: no-router-push-cross-app
**Date:** ~2026-03-30 | **Status:** active
**Source:** `site-revolution-redesign.md`
**Uncertainty:** The root cause of why `$router.push` fails across app boundaries
is not documented. It was discovered empirically. The cause may be related to
SSR hydration context, CMS page routing, or Vue Router instance isolation.

**Context:** Needed to navigate from location page to booking flow while preserving state.
**Decision:** Use cookie + hard redirect instead of `$router.push`.
**Alternatives considered:**
- `$router.push` — rejected: [UNCERTAIN] "doesn't survive cross-app page reloads" (empirical)
- URL query params — rejected: booking flow doesn't read them for service selection

**Consequences:**
- Cookie name becomes a contract between producer and consumer
- Hard redirect means full SSR cycle on destination page
```

---

## Edge Case 5: Rapid Iteration Sessions

**Situation:** A session involves many quick iterations, experiments, and direction changes. Multiple decisions are made and reversed within the same session.

### Detection

| Signal | Example |
|---|---|
| Decision X made, then reversed, then re-adopted with modifications | "Tried CSS Grid → reverted to Flexbox → adopted CSS Grid with modifications" |
| Multiple approaches tried for the same problem | "Option A failed, Option B was too complex, settled on Option C" |
| Session Section 2.3 has superseded decisions | Multiple `DEPRECATED` or `SUPERSEDES` markers |

### Resolution: Consolidate Before Saving

```
1. Identify the FINAL decision for each topic
2. Collapse the iteration history into the "alternatives considered" field
3. Store only ONE entry per topic — the final decision
4. Mention the iteration in a "notes" field if the journey informs the decision
```

### Before (Unconsolidated):
```markdown
### ad-030: use-css-grid-masonry
**Date:** 2026-03-30 | **Status:** active
### ad-031: revert-to-flexbox-masonry
**Date:** 2026-03-30 | **Status:** superseded
### ad-032: use-css-columns-masonry
**Date:** 2026-04-03 | **Status:** active (supersedes ad-030 and ad-031)
```

### After (Consolidated):
```markdown
### ad-030: css-columns-for-photo-masonry
**Date:** 2026-04-03 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** Photos page needed a responsive masonry-style grid layout.
**Decision:** CSS-only `column-count: 2/3/4` via media queries. Zero JS.

**Alternatives considered:**
- CSS Grid with `grid-auto-rows: 1px` + ResizeObserver — rejected (2026-03-30): worked but
  required JS measurement, caused hydration mismatch between SSR and client column counts
- Flexbox masonry — rejected (2026-03-30): flex doesn't support true masonry flow
- JS `matchMedia` + `columnCount` data property — rejected (2026-04-03): SSR/client divergence

**Notes:** This was the result of 3 iterations over 4 days. The CSS-only approach was the
simplest and eliminated all hydration issues. The Grid approach (ad-030 original) works
technically but is over-engineered for this use case.
```

---

## Edge Case Summary Table

| Edge Case | Detection Signal | Resolution Strategy |
|---|---|---|
| **Contradictory decisions** | Same scope, different decision | Flag as "contested", surface to user, resolve explicitly |
| **Partial decisions** | Implementation pending or only done once | Store with `status: partial`, track maturity |
| **Over-specific patterns** | References specific components/tickets | Generalize the principle, keep specifics as validated-in examples |
| **Missing context** | Decision without rationale | Annotate with `[UNCERTAIN]`, do not infer |
| **Rapid iteration** | Multiple superseded decisions in one session | Consolidate to final decision, collapse iterations into alternatives |

---

## Correct vs. Incorrect Conflict Handling

### Example: Superseded Decision

**Incorrect (silent override):**
```markdown
### ad-001: css-accordion-over-transition-expand
**Status:** active
[Entry unchanged — new contradicting decision stored elsewhere without linkage]
```

**Correct (explicit supersession chain):**
```markdown
### ad-001: css-accordion-over-transition-expand
**Status:** active (general case)
**Note:** For elements that start hidden (not visible during transition setup),
see ad-015 which uses TransitionExpand. Both are valid — context-dependent.

### ad-015: transition-expand-for-initially-hidden
**Status:** active (specific case: elements starting hidden)
**Related:** ad-001 (general accordion pattern). This is a context-specific
exception, not a contradiction.
```

**Why:** Silent overrides create contradictory guidance. Explicit supersession chains or context-dependent coexistence notes let the AI choose the right approach based on the current situation.
