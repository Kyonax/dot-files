---
title: Architecture Memory File Structure — Sections, Format, and Template
impact: CRITICAL
impactDescription: Defines the mandatory structure for architecture memory files. Without a consistent format, memory files become unsearchable free-form notes that cannot be programmatically referenced or efficiently loaded into AI context windows.
tags: session-file, template, structure, sections, format, architecture, memory, summary, decisions, patterns, state, dependencies, constraints, references, reusable
---

This rule defines the mandatory structure for architecture memory files — persistent documents that store extracted architectural knowledge from sessions. Every memory file follows a consistent structure designed for two purposes: (1) efficient AI context loading and (2) precise cross-session referencing via the reference syntax system.

## What Is an Architecture Memory File?

An architecture memory file is the output of the extraction process. Unlike a session context block (which captures everything needed to resume a specific session), an architecture memory file captures **only the reusable architectural knowledge** — patterns, decisions, constraints, and abstractions that inform future work across sessions.

**Key distinction from session files:**

| Property | Session Context Block | Architecture Memory File |
|---|---|---|
| **Purpose** | Resume a specific session | Inform future sessions across projects |
| **Content** | Everything: guidelines, implementations, file index, resume points | Only: architectural decisions, patterns, constraints, abstractions |
| **Lifespan** | Active during session, compressed over time | Permanent — grows and evolves across sessions |
| **Loading** | Full block loaded at session start | Referenced sections loaded on-demand |
| **Structure** | 5-section context block | 6-section architecture memory (below) |

## File Naming Convention

Architecture memory files live in the same sessions directory as context block files.

| Convention | Example |
|---|---|
| Kebab-case, descriptive of the architectural domain | `site-revolution-architecture.md` |
| Use `-architecture` suffix to distinguish from context blocks | `navigation-redesign-architecture.md` |
| Project-scoped when knowledge is project-specific | `mr-ssr-architecture.md` |
| Domain-scoped when knowledge is cross-project | `vue3-ssr-patterns-architecture.md` |

## The 6-Section Architecture

Every architecture memory file is organized into **6 mandatory sections**. Each section serves a distinct purpose and is independently referenceable via the reference syntax system.

| Section | Purpose | Reference Use Case |
|---|---|---|
| **1. Summary** | High-level overview of what this memory covers | Quick orientation — "What is this about?" |
| **2. Architecture Decisions** | Decisions that constrain or direct future work | "What was decided and why?" |
| **3. Design Patterns** | Reusable structural approaches | "How should I build X?" |
| **4. Shared State & Data Flow** | Global states, data sources, transformation chains | "Where does data come from and how does it move?" |
| **5. Constraints & Limitations** | Platform, performance, ADA, business constraints | "What can't I do, and why?" |
| **6. Reusable References** | APIs, schemas, contracts, structures | "What shape is the data?" |

**Section independence:** Each section answers a different question. A future session may reference only Section 3 (patterns) without loading Section 2 (decisions). The reference syntax enables this precision.

---

## Section 1: Summary

**Purpose:** Provide a high-level orientation for any AI or human loading this file.

**Mandatory elements:**
- Domain/project scope (what area of the codebase/system this covers)
- Source sessions (which sessions contributed to this memory, with dates)
- Knowledge categories present (which of sections 2-6 have content)
- Last updated date

**Format:**

```markdown
## 1. Summary

**Domain:** [What area this memory covers]
**Project:** [Project or system name, if project-specific]
**Last updated:** YYYY-MM-DD

**Source sessions:**
- `session-name.md` (YYYY-MM-DD) — [What was extracted]
- `another-session.md` (YYYY-MM-DD) — [What was extracted]

**Knowledge categories:**
- Architecture Decisions: N entries
- Design Patterns: N entries
- Shared State & Data Flow: N entries
- Constraints & Limitations: N entries
- Reusable References: N entries
```

---

## Section 2: Architecture Decisions

**Purpose:** Store decisions that constrain or direct future work, with full rationale.

**Each entry requires:**

| Field | Required | Description |
|---|---|---|
| `id` | Yes | Unique, stable identifier for reference syntax (kebab-case) |
| `date` | Yes | When the decision was made (YYYY-MM-DD) |
| `title` | Yes | Concise decision statement |
| `context` | Yes | What problem or situation prompted this decision |
| `decision` | Yes | What was chosen |
| `alternatives` | Yes | What was considered and rejected, with reasons |
| `consequences` | Yes | What this decision constrains or enables for future work |
| `status` | Yes | `active`, `superseded`, `deprecated`, or `partial` |
| `superseded-by` | If status=superseded | Reference to the replacing decision |
| `source-session` | Yes | Session file where this originated |

**Format:**

```markdown
## 2. Architecture Decisions

### ad-001: css-accordion-over-transition-expand
**Date:** 2026-03-15 | **Status:** active
**Source:** `site-revolution-redesign.md`

**Context:** Needed expand/collapse animation for FAQ sections and review text.

**Decision:** CSS `max-height` transition with `overflow: hidden`.

**Alternatives considered:**
- `TransitionExpand` component — rejected: causes content flash (uses `position: absolute` + `visibility: hidden` measurement that leaks a frame)
- JS-measured height + inline style — rejected: unnecessary complexity when CSS handles it

**Consequences:**
- Content stays in DOM (no `v-if`) — use `aria-hidden` to toggle accessibility
- Animation duration is approximate (max-height must overshoot actual height)
- Pattern applies to all accordion/expand-collapse UI in the project
```

**Rules:**
- IDs must be globally unique within the file and stable (never renumbered)
- Use `ad-NNN` prefix (architecture decision) for consistent reference syntax
- When a decision is superseded, keep the old entry with `status: superseded` and a pointer to the new one — never delete
- Date every entry — the reference system and conflict resolution depend on temporal ordering

---

## Section 3: Design Patterns

**Purpose:** Store reusable structural approaches that apply broadly.

**Each entry requires:**

| Field | Required | Description |
|---|---|---|
| `id` | Yes | Unique identifier (kebab-case, `dp-NNN` prefix) |
| `name` | Yes | Pattern name |
| `when-to-use` | Yes | Conditions that trigger this pattern |
| `structure` | Yes | How the pattern is implemented (can include code snippets) |
| `why` | Yes | Rationale — what problem it solves |
| `validated-in` | Yes | Where this pattern has been successfully used |
| `anti-pattern` | If exists | What NOT to do, and why |

**Format:**

```markdown
## 3. Design Patterns

### dp-001: self-contained-landmarks
**Name:** Self-Contained Landmarks
**When to use:** Any component that needs an ARIA landmark (section, region, navigation).

**Structure:**
- Component owns `role="region"` + `aria-labelledby` + heading `id` in its own template
- Parent wrappers are purely structural (no ARIA attributes)
- `v-if` on component root guards heading and `aria-labelledby` together
- For delegated headings (e.g., PageIntro), pass `titleId` prop

**Why:** Cross-component `aria-labelledby` coordination via `@ready` events caused ADA violations — `aria-labelledby` on a `div` without a `role` is invalid. Self-contained landmarks eliminate timing issues and dangling references.

**Validated in:** HeroV2, About, Services, Reviews, FeaturedDeals, ShadeShopPage

**Anti-pattern:** Putting `aria-labelledby` on parent wrapper `div`s without a `role`. Never use `@ready` events to coordinate heading IDs between parent and child.
```

---

## Section 4: Shared State & Data Flow

**Purpose:** Document global states, data sources, and transformation chains.

**Each entry requires:**

| Field | Required | Description |
|---|---|---|
| `id` | Yes | Unique identifier (`sf-NNN` prefix) |
| `name` | Yes | What this state/flow is |
| `source` | Yes | Where the data originates |
| `transforms` | If any | Steps between source and consumer |
| `consumers` | Yes | What reads this data |
| `gotchas` | If any | Non-obvious behavior or timing issues |

**Format:**

```markdown
## 4. Shared State & Data Flow

### sf-001: navigation-data-pipeline
**Name:** Navigation Data Pipeline
**Source:** Tophat Data Tool — `dataToolSvc.getData({ mixinKey: 'sr-top-nav' })`
**Transforms:** CMS API → `siteNav.js` Vuex action → `res.data || {}` null guard → individual mutations
**Consumers:** `SiteNavDesktopV2`, `SiteNavMobileV2MainNav`, `SiteNavShopContent`, `SiteNavMobileWrapper`
**Gotchas:**
- `res.data` can be null — always destructure with `|| {}` guard
- All nested property access on CMS objects requires `?.` — 17 spots audited
- `mixinKey` changed from `'top-nav'` to `'sr-top-nav'` for Site Revolution
```

---

## Section 5: Constraints & Limitations

**Purpose:** Document platform, performance, ADA, or business constraints.

**Each entry requires:**

| Field | Required | Description |
|---|---|---|
| `id` | Yes | Unique identifier (`cl-NNN` prefix) |
| `scope` | Yes | What area of the system this constrains |
| `constraint` | Yes | The limitation itself |
| `cause` | Yes | Why this limitation exists |
| `workaround` | If exists | How to work within the limitation |
| `impact` | Yes | What happens if you violate this constraint |

**Format:**

```markdown
## 5. Constraints & Limitations

### cl-001: cms-parseurl-segment-validation
**Scope:** Vue Router children under CMS-managed routes
**Constraint:** CMS `parseUrl` validates URL segment count against Tophat `urlParameterList.length`. Extra segments fail validation → 404.
**Cause:** `mr_modules/cms/lib/router.js:650` — server-side URL resolution happens before Vue Router.
**Workaround:** Define optional parameters in Tophat for each child route segment.
**Impact:** Without the Tophat parameter, child routes return 404 on direct access and full-page refresh.
```

---

## Section 6: Reusable References

**Purpose:** Store API contracts, data shapes, schemas, and structural references.

**Each entry requires:**

| Field | Required | Description |
|---|---|---|
| `id` | Yes | Unique identifier (`rr-NNN` prefix) |
| `name` | Yes | What this reference describes |
| `shape` | Yes | The data structure, API contract, or schema |
| `source` | Yes | Where this comes from |
| `consumers` | Yes | What uses this |

**Format:**

```markdown
## 6. Reusable References

### rr-001: colorbar-cache-media-object
**Name:** ColorBar Cache Media Object Shape
**Source:** `colorbarCache.getLocation()` → `carouselImages[]`
**Shape:**
```json
{
  "_id": "string",
  "url": "string (full CDN URL with possible query params)",
  "width": "number (pixels — enables dynamic aspect ratio)",
  "height": "number (pixels)",
  "alt_text": "string",
  "file_name": "string",
  "file_type": "string",
  "aspects": ["array of aspect ratio objects"],
  "versionInfo": ["array"]
}
```
**Consumers:** `HairColorBarLocationHeroV2` (gallery images), `HcbLocationPhotosPage` (masonry grid), `LocationImageCarousel`
```

---

## Complete Template (New Architecture Memory File)

```markdown
# [Domain] Architecture Memory

## 1. Summary

**Domain:** [What area this memory covers]
**Project:** [Project name]
**Last updated:** YYYY-MM-DD

**Source sessions:**
- `session-name.md` (YYYY-MM-DD) — [What was extracted]

**Knowledge categories:**
- Architecture Decisions: 0 entries
- Design Patterns: 0 entries
- Shared State & Data Flow: 0 entries
- Constraints & Limitations: 0 entries
- Reusable References: 0 entries

---

## 2. Architecture Decisions

[Entries with `ad-NNN` IDs]

---

## 3. Design Patterns

[Entries with `dp-NNN` IDs]

---

## 4. Shared State & Data Flow

[Entries with `sf-NNN` IDs]

---

## 5. Constraints & Limitations

[Entries with `cl-NNN` IDs]

---

## 6. Reusable References

[Entries with `rr-NNN` IDs]
```

---

## Correct vs. Incorrect Structure

### Example 1: Summary Completeness

**Incorrect:** Missing source sessions and knowledge counts.
```markdown
## 1. Summary
This file covers the Site Revolution redesign architecture.
```

**Correct:** Full metadata for context and reference tracking.
```markdown
## 1. Summary
**Domain:** Hair Color Bar location pages — SSR, CMS Partials, booking flow
**Project:** Madison Reed Site Revolution
**Last updated:** 2026-04-23
**Source sessions:**
- `site-revolution-redesign.md` (2026-03-02 → 2026-04-23) — 107 decisions, 15+ patterns
**Knowledge categories:**
- Architecture Decisions: 12 entries
- Design Patterns: 8 entries
- Shared State & Data Flow: 5 entries
- Constraints & Limitations: 7 entries
- Reusable References: 4 entries
```

**Why:** Source session tracking prevents duplicate extraction. Knowledge counts let consumers know if the file has relevant content before loading sections.

### Example 2: Decision Entry Completeness

**Incorrect:** Missing alternatives and consequences.
```markdown
### ad-005: cookie-based-service-preselection
We use cookies to pre-select services in the booking flow.
```

**Correct:** Full decision record with rationale chain.
```markdown
### ad-005: cookie-based-service-preselection
**Date:** 2026-03-30 | **Status:** active
**Source:** `site-revolution-redesign.md`
**Context:** FeaturedServicesV2 CTA needs to pre-select a service in the booking flow on a different page.
**Decision:** Set `selected_service` cookie + `trackMREventAndRedirect` to services URL.
**Alternatives considered:**
- Vuex `setSelectedService` + `$router.push` — rejected: doesn't survive cross-app page reloads
- URL query parameter — rejected: booking flow doesn't read query params for service selection
**Consequences:**
- Consumer (`ServicesPage.setServiceFromCookie()`) must exist on the destination page
- Cookie name `selected_service` is a contract between producer and consumer
- Pattern applies to any future cross-page service pre-selection
```

**Why:** Alternatives and consequences are the most valuable parts of a decision record. Without them, a future session knows WHAT was chosen but not WHY, and cannot evaluate whether the decision still applies to a new context.
