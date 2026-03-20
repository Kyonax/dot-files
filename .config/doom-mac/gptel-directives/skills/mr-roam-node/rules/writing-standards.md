---
title: Writing Standards — Tone, Clarity, and Content Quality for Node Sections
impact: HIGH
impactDescription: Nodes serve as the source of truth for commits, PRs, and development tracking. Vague, inconsistent, or poorly structured content in node sections produces weak commit messages, unclear PR descriptions, and unreliable validation — defeating the purpose of the documentation system.
tags: writing, tone, clarity, accuracy, consistency, voice, style, content-quality, documentation, commit, pr, development, user-centric, active-voice, terminology, structure, propose, outline, section-writing, ticket-context, development-ac, qa-instructions, deployment-notes, unit-test-coverage
---

This rule governs how content should be written when populating node sections — the tone, voice, clarity, and structural quality of every piece of text that goes into a roam node. Nodes are not just records; they are the **single source of truth** the developer pulls from to create commits, write PR descriptions, validate work, and track progress. Vague or inconsistent writing in a node propagates into vague commits, unclear PRs, and unreliable validation. Every sentence in a node must earn its place.

## Guiding Principles

These four principles apply to ALL content written into any node section:

| Principle       | Rule                                                                                                                                                                                                                               | Anti-Pattern                                                                                                                    |
|-----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------|
| **Clarity**     | Write in simple, clear, and unambiguous language. One idea per sentence. No jargon without definition.                                                                                                                             | "The thing was updated to handle the case" — what thing? which case?                                                            |
| **Accuracy**    | Ensure all information — component names, file paths, CSS classes, API endpoints, config values — is correct and verifiable against the codebase.                                                                                  | Guessing a file path like `src/components/Hero.vue` when the actual path is `src/vuescripts/hairColorBar/HcbLocationPageV2.vue` |
| **Consistency** | Use the same term for the same concept throughout the node. If a component is `=HcbLocationPageV2=`, never call it "the HCB page component" or "the location hero" interchangeably.                                                | Switching between "sticky CTA", "fixed button", "Book Services bar", and "bottom banner" for the same element                   |
| **Specificity** | Name files, components, classes, methods, and line numbers. Never say "the component" when you can say `=HcbLocationPageV2=`. Never say "updated styles" when you can say "added `.sticky-cta-container` with `position: sticky`". | "Fixed the issue with the button" — which button? what fix? where?                                                              |

## Voice and Tone

### Always Use Active Voice

The developer is the actor. The node documents what **was done**, not what "happened."

**Incorrect:**
```org
The component was refactored to improve performance.
Styles were applied to the container.
The bug was resolved.
```

**Correct:**
```org
Refactored =HcbLocationPageV2= to lazy-load the map iframe, reducing initial bundle by ~15KB.
Applied =position: sticky= with =bottom: 0= to =.book-services-cta= container.
Fixed race condition in =fetchLocationData= where async response overwrote user-selected location.
```

### Be Terse, Not Verbose

Nodes are reference documents, not narratives. Lead with the fact, then add context only if it's not obvious.

**Incorrect:**
```org
After looking at the ticket requirements and understanding that the user story mentions we need to add a sticky CTA, I decided to implement this using a new component that would sit at the bottom of the page and follow the user as they scroll.
```

**Correct:**
```org
Created =StickyBookServicesCta.vue= — fixed-position CTA at viewport bottom, visible on scroll past hero section. Uses =IntersectionObserver= on =.hero-section= to toggle visibility.
```

### Use Technical Precision

When documenting code decisions, include the **what**, **where**, and **why** — skip the **how** unless it's non-obvious.

| Include                   | Example                                                                            |
|---------------------------|------------------------------------------------------------------------------------|
| Component name (verbatim) | `=HairColorBarLocationHeroV2=`                                                     |
| File path                 | `=website/src/vuescripts/hairColorBar/HcbLocationPageV2.vue=`                      |
| CSS class or utility      | `=.sticky-cta-container=`, `=.mt-100m=`                                            |
| Method or function name   | `=fetchLocationDetails()=`                                                         |
| Vuex action/mutation      | `=dispatch('locations/setSelectedLocation')=`                                      |
| Config or feature flag    | `=hcbStickyCtaEnabled=` in CMS                                                     |
| Reason for the decision   | "Used =IntersectionObserver= instead of scroll listener to avoid layout thrashing" |

---

## Section-Specific Writing Rules

### TICKET CONTEXT (Phase 1 — JIRA Source)

**Purpose:** Capture requirements the development must fulfill. Written at creation from JIRA.

- Translate ALL AC to GIVEN/WHEN/THEN — no paraphrasing that loses meaning
- Preserve the original intent — if JIRA says "44x44px touch target", don't simplify to "tappable"
- Add `*** EVENT TRACKING` org table if JIRA specifies tracking events — copy cell values exactly
- Use `=verbatim=` for any component, class, or technical term mentioned in the JIRA AC

### DEVELOPMENT AC (Phase 2 — Developer Fills)

**Purpose:** Document implementation-specific decisions and technical AC that emerge during development. This section is what the developer writes to track *how* they chose to build the solution.

**Writing workflow:** Before filling, propose a structure:
1. List the main implementation decisions (component architecture, state management, API changes)
2. For each decision, write a concise AC in the same GIVEN/WHEN/THEN or declarative format
3. Include component names, file paths, and the reasoning behind non-obvious choices

**Example:**
```org
** DEVELOPMENT AC
- *Component Architecture*
  - Created =HcbLocationPageV2.vue= as wrapper, receives location data via =locationSlug= prop from route params.
  - =HairColorBarLocationHeroV2.vue= handles hero rendering — receives =locationData= as prop, emits =book-services-clicked=.
  - =StickyBookServicesCta.vue= — new component, uses =IntersectionObserver= on hero to toggle sticky visibility.

- *State Management*
  - Location data fetched in =beforeMount= via =dispatch('hcbLocations/fetchLocationBySlug')=.
  - No new Vuex module — reused existing =hcbLocations= store with new getter =getLocationBySlug=.

- *Why =IntersectionObserver= over scroll listener*
  - Scroll listener caused measurable jank on mobile (16ms+ frame drops during scroll).
  - =IntersectionObserver= is passive and only fires on threshold cross.
```

### STRUCTURE AND FUNCTIONALITY (Phase 2)

**Purpose:** Architecture description for future reference. Follow the Input/Process/Output pattern.

**Writing rule:** Describe data flow, not implementation details. Someone reading this section should understand *what the feature does* without reading the code.

**Example:**
```org
** STRUCTURE AND FUNCTIONALITY
*** Input
- Route param =:locationSlug= from =/hair-color-bar/locations/:locationSlug=
- CMS data: =hcbLocationPages= collection filtered by =slug= match

*** Process
- =HcbLocationPageV2= resolves location from slug, fetches detailed data (hours, services, staff)
- Hero section renders location name, address, phone, hero image from CMS
- Sticky CTA appears when hero scrolls out of viewport (=IntersectionObserver=)

*** Output
- Rendered location detail page with hero, services list, map embed, and sticky Book Services CTA
- =trackMREvent= fires on CTA click: =HCB Location Page – Book now clicked=
```

### DEPLOYMENT NOTEs (Phase 2)

**Purpose:** Everything needed to deploy this change beyond merging code. Written as a checklist.

**Writing rule:** If there's nothing to deploy beyond the code, write "No CMS or config changes required." Never leave blank without explanation.

**Example:**
```org
** DEPLOYMENT NOTEs
- [ ] CMS: Add =stickyCtaEnabled= boolean field to =hcbLocationPages= content type
- [ ] CMS: Set =stickyCtaEnabled: true= for all active location pages
- [ ] Feature flag: =hcb_sticky_cta= in LaunchDarkly — deploy with flag OFF, enable after QA
```

### UNIT TEST COVERAGE (Phase 2)

**Purpose:** Document what tests were written, what they cover, and where they live.

**Writing rule:** List test file paths, then describe test scenarios grouped by component. Use the test description as written in the `describe`/`it` blocks.

**Example:**
```org
** UNIT TEST COVERAGE
*** =HcbLocationPageV2.spec.js=
- renders location hero when locationData is provided
- shows loading skeleton when locationData is null
- dispatches =fetchLocationBySlug= on mount with route slug

*** =StickyBookServicesCta.spec.js=
- hidden by default when hero is in viewport
- becomes visible when hero scrolls out of viewport
- fires =trackMREvent= with correct event name on click
- does not render when =stickyCtaEnabled= is false
```

### QA INSTRUCTIONs (Phase 2)

**Purpose:** Step-by-step manual testing guide. Written so someone unfamiliar with the code can verify the feature.

**Writing rule:** Numbered steps. Each step has an action and an expected result. Include URLs, breakpoints, and device specifics.

**Example:**
```org
** QA INSTRUCTIONs
1. Navigate to =/hair-color-bar/locations/bethesda= on desktop (>1024px)
2. Verify hero section shows location name, address, and hero image
3. Scroll past the hero section
4. *EXPECTED:* Sticky "Book Services" CTA appears fixed at viewport bottom
5. Click the sticky CTA
6. *EXPECTED:* Redirects to =/hair-color-bar/booking?location=bethesda=
7. Resize browser to mobile (<768px)
8. *EXPECTED:* Sticky CTA still appears and is tappable with adequate touch target
9. Check Segment debugger: =HCB Location Page – Book now clicked= event fires on CTA click
```

### COMMENTs (Phase 1 + Phase 2)

**Purpose:** Relevant JIRA comments at creation; developer notes during development.

**Writing rule:** Prefix each comment with the author and date. Skip bot/automated comments. For developer notes, use `** UPDATE [date]` sub-headings.

### COMMIT MSG (Phase 2 — Post-PR)

**Purpose:** The PR description, stored for reference.

**Writing rule:** Copy the PR summary as-is. If the PR description was generated from the node's own sections, note that to avoid circular reference confusion.

---

## Terminology Consistency Rules

Once a term is introduced in a node, use it identically throughout:

| Establish Once             | Then Use Consistently      | Never Switch To                                            |
|----------------------------|----------------------------|------------------------------------------------------------|
| `=HcbLocationPageV2=`      | `=HcbLocationPageV2=`      | "the page component", "the HCB component", "location page" |
| `=.sticky-cta-container=`  | `=.sticky-cta-container=`  | "the sticky div", "CTA wrapper", "bottom bar"              |
| `=fetchLocationBySlug=`    | `=fetchLocationBySlug=`    | "the fetch method", "the API call", "data getter"          |
| "sticky Book Services CTA" | "sticky Book Services CTA" | "fixed button", "bottom banner", "floating CTA"            |

**Exception:** The first mention may include a parenthetical alias for clarity:
```org
Created =StickyBookServicesCta.vue= (the sticky Book Services CTA) — fixed-position...
```
After that, use only `=StickyBookServicesCta.vue=` or "sticky Book Services CTA" — not both interchangeably.

---

## The Propose-Before-Fill Workflow

When the developer asks to fill Phase 2 sections (DEVELOPMENT AC, DOCUMENTATION subsections), follow this workflow:

1. **Read the node** — understand TICKET CONTEXT AC, existing content, and ticket scope
2. **Read the codebase** — examine the actual implementation (git diff, changed files, component structure)
3. **Propose a structure** — present a bulleted outline of what each section will contain, with specific component names and decisions identified
4. **Await approval** — the developer confirms or adjusts the proposed structure
5. **Generate content** — write the full section content following all writing standards in this rule

**Why propose first:** The developer may have context not visible in the code (rejected approaches, verbal agreements, scope changes). Proposing first catches misalignment before writing 200 lines of documentation.

**Exception:** If the developer says "fill it" or "just write it" without asking for a proposal, skip steps 3-4 and generate directly. The workflow is a default, not a gate.
