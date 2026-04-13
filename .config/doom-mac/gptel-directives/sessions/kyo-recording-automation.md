<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the Kyo Recording Automation project. It tracks the design, implementation, and session state for building a custom OBS recording/streaming automation system with a cyberpunk/sci-fi HUD overlay — built specifically to power @kyonax_on_tech content creation by minimizing post-production editing through capture-time automation. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read the sections in order on first load — after that, reference them by number as needed. The data is organized into 5 sections:

| Section                        | Purpose                                                                                              | When to reference                                                                       |
|--------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| **1. Global Guidelines**       | Design constraints, visual language rules, and technical requirements that apply to ALL work.        | Before creating or modifying any overlay, script, or automation component. Mandatory.   |
| **2. Session Overview**        | High-level context: project scope, goals, current status, key decisions, and pending work.           | When starting a new task — understand scope and status first.                           |
| **3. Implementations**         | Per-component detail: what was built, file structure, decisions made, current state.                 | When resuming work on a specific component or reviewing what happened.                  |
| **4. File Index**              | Quick-reference of all relevant file paths: overlays, scripts, assets, configs, docs.                | When locating files or assets without searching.                                        |
| **5. Last Interaction**        | Short-term memory: what was done last, what's pending, where to resume.                              | At the very start of a new conversation — entry point for continuing work.              |

**Operational Rule:** Throughout the conversation, the user will give new tasks. **Always look for the last request, identified by a markdown title starting with `###`.** Any `###` title means it is the newest request.

**Key principle:** Data may appear in multiple sections with different framing. Section 1 frames it as a rule to follow. Section 2 frames it as context to understand. Section 3 frames it as an implementation to reference. This is intentional — each section answers a different question about the same knowledge.

---

## SECTION 1: GLOBAL GUIDELINES & DESIGN CONSTRAINTS

> **Apply these rules to every task in this session.** No domain skills are loaded for OBS automation specifically — these guidelines stage session-specific design and tech rules that may eventually graduate into a dedicated skill.

### 1.1 Visual Language

*   **Color palette:** Black background (#000000), gold/yellow accents (#FFD700), white structural lines and text. No other colors unless explicitly approved.
*   **Aesthetic:** Cyberpunk/sci-fi HUD — think military-grade surveillance UI, not gamer/streamer. Clean, grid-aligned, monospaced typography.
*   **HUD elements vocabulary:** SYS.LOG, REC FRAME, DATA STREAM, DATA FRAME, GRID SYNC, MODULE ACTIVE, CORE INPUT, NODE LINK, REC MODE, CAM ONLINE — use these labels consistently across overlays.
*   **Layout zones:** Main recording frame (center), webcam frame (top-left or right sidebar), data/status bars (bottom), audio visualizer area, metadata readouts.
*   **Corner brackets and crosshairs** on frames — not rounded borders. Tactical/angular styling.

### 1.2 Technical Requirements

*   **OBS compatibility:** All overlays must work as OBS browser sources or image sources.
*   **Resolution targets:** 1920x1080 (primary), 2560x1440 (secondary). Overlays should scale cleanly.
*   **Automation scope:** OBS scene switching, recording triggers, overlay state changes, data stream updates (time, system metrics, etc.).
*   **Tooling preferences:** HTML/CSS/JS for dynamic overlays, SVG or PNG for static elements, OBS WebSocket API for automation scripts.
*   **Edit-minimization principle:** Every automation choice should reduce post-production work. If a feature can be baked into the live capture (transitions, lower thirds, layout swaps), prefer that over fixing it in editing.

### 1.3 File Organization

*   **`assets/`** — Inspiration images, reference materials, exported static assets.
*   **`overlays/`** — HTML/CSS/JS browser source overlays (one directory per overlay component).
*   **`scripts/`** — Automation scripts (OBS WebSocket, scene control, recording triggers).
*   **`config/`** — OBS scene collection exports, profile configs, automation settings.

### 1.4 Documentation & README Rendering

*   **GitHub does NOT support `ansi` code blocks.** Escape codes (`\033[33m` etc.) are stripped or shown as raw text. Confirmed via investigation 2026-04-08 — `ansi` is not in GitHub's linguist language list.
*   **Color options for README ASCII art** (in priority order):
    *   **`diff`** — lines prefixed with `+` render green, `-` render red. Reliable, limited palette.
    *   **SVG image** — for full color control, embed an `.svg` with styled ASCII art via `<img>`.
    *   **Plain code block** — no colors, but cleanest visual.
*   **Concise README rule:** Project README stays minimal. ASCII art banner + one-paragraph description + structure tree. No "Why" sections, no extensive feature lists — the visual language carries the brand.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Build a complete OBS recording/streaming automation system with a custom cyberpunk/sci-fi HUD overlay, specifically to support **@kyonax_on_tech** content creation. The system should handle automated scene switching, recording controls, and dynamic overlay updates — getting capture right at recording time so that editing is minimized to near-zero.

**Cross-reference (project context):** See roam node `~/.brain.d/roam-nodes/content_creation/2025-02-10-content_creation_kyonax_on_tech.org` for full details on the @kyonax_on_tech personal brand (Cristian D. Moreno / Kyonax — software dev & content creator focused on Doom Emacs tutorials, CV/portfolio content, short-form video for TikTok/YouTube, en/es).

### 2.2 Scope

| Phase                              | Type       | Summary                                                              | Status          |
|------------------------------------|------------|----------------------------------------------------------------------|-----------------|
| Phase 0: Inspiration & Design      | Design     | Collect reference images, define visual language, plan layout         | **DONE**        |
| Phase 0.5: Project Documentation   | Docs       | README with HUD ASCII art and minimal project description           | **DONE**        |
| Phase 1: Static Overlay Design     | Design     | Create base HUD overlay frames (HTML/CSS or SVG)                     | NOT STARTED     |
| Phase 2: Dynamic Overlays          | Dev        | Add JS-driven dynamic elements (clock, status, audio vis)            | NOT STARTED     |
| Phase 3: OBS Integration           | Dev        | OBS WebSocket automation, scene switching, recording triggers        | NOT STARTED     |
| Phase 4: Polish & Packaging        | Refinement | Final adjustments, documentation, OBS scene collection export        | NOT STARTED     |

### 2.3 Key Decisions (Session-Wide)

1.  **(2026-04-08)** Project initialized with 4 inspiration images defining the visual direction: cyberpunk HUD with black/gold/white palette.
2.  **(2026-04-08)** Repo created at `github.com/kyonax/kyo-recording-automation`, single "Init Project" commit.
3.  **(2026-04-13)** Project explicitly scoped to @kyonax_on_tech content creation. Edit-minimization is the **primary success metric** — every automation should remove an editing step.
4.  **(2026-04-13)** README ASCII art finalized with `diff` code block syntax for green-line coloring on the HUD frame edges. ANSI escape codes were attempted but rejected — GitHub does not render them.
5.  **(2026-04-13)** README style locked in as **minimal**: ASCII banner + one-line description + structure tree only. No Why/Stack/Design sections.

### 2.4 Inspiration Analysis

The 4 reference images share consistent design DNA:

| Element              | Description                                                                    |
|----------------------|--------------------------------------------------------------------------------|
| **Main frame**       | Large center recording area with corner brackets, crosshair markers            |
| **Webcam frame**     | Smaller inset frame (top-left in inspo 1/4, right sidebar in inspo 4)          |
| **Status bar**       | Bottom bar with hex color codes, timestamps, REC MODE indicator                |
| **Data readouts**    | SYS.LOG, DATA STREAM, GRID SYNC labels with small data visualizations          |
| **Audio bars**       | Small audio level indicators near webcam or bottom bar                          |
| **Typography**       | Monospaced, uppercase, small size — functional not decorative                   |
| **Accent color**     | Gold (#FFD700) used sparingly for highlights, indicators, and active elements   |

### 2.5 Pending Work

*   [ ] Decide which overlay component to build first (main frame? webcam frame? status bar?)
*   [ ] Choose implementation tech: pure HTML/CSS browser sources vs. SVG vs. canvas-rendered
*   [ ] Begin Phase 1 — base HUD frame overlay
*   [ ] Set up `overlays/`, `scripts/`, `config/` directories (currently only `assets/` exists)

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 Repository Structure (Current)

**Created:** 2026-04-08 | **Last updated:** 2026-04-13
**Status:** Bootstrap phase — only assets and README exist

```
kyo-recording-automation/
  README.md                       — minimal project README with HUD ASCII art (diff-colored)
  assets/
    temp_obs_inspo_2.png          — HUD overlay reference (variant 2)
    temp_obs_inspo_4.png          — HUD overlay reference (variant 4, sidebar webcam)
    temporal_obs_inspo_1.jpg      — HUD overlay reference (variant 1, primary)
    temporal_obs_inspo_3.jpg      — HUD overlay reference (variant 3, angular corners)
```

### 3.2 README.md

**Created:** 2026-04-13 | **Last updated:** 2026-04-13
**Status:** Final — minimal style locked in

**Structure:**
*   ASCII art HUD frame (uses `diff` code block — green lines on `+` prefixes for outer frame and REC bar)
*   Inner crosshair brackets simulate the webcam frame corners and viewfinder
*   Vertical "OBS" letters appear on right side of the frame
*   Center crosshair `[ + ]` marks viewfinder center
*   Status bar: `[REC] 36:09:33` + `@kyonax_on_tech` handle + `//` separator
*   One-paragraph description below banner referencing @kyonax_on_tech
*   Minimal directory tree showing planned structure
*   Footer tagline: `build | code | document`

#### Key Decisions

| Decision                           | Date       | Rationale                                                                                                                  |
|------------------------------------|------------|----------------------------------------------------------------------------------------------------------------------------|
| Use `diff` for ASCII art coloring  | 2026-04-13 | GitHub doesn't render `ansi` escape blocks (verified). `diff` reliably gives green/red line coloring with `+`/`-` prefixes. |
| Strip Why/Stack/Design sections    | 2026-04-13 | User wanted concise README — visual identity carries the brand, not prose                                                  |
| Keep ASCII minimalist              | 2026-04-13 | Initial elaborate KYO block-letter banner replaced with clean HUD frame — "minimalist and clean" per user                  |

### 3.3 ANSI Color Investigation (Closed)

**Date:** 2026-04-13
**Outcome:** Confirmed GitHub does NOT support `ansi` fenced code blocks. Investigation included:
*   Web searches for current GitHub ANSI support status
*   Inspection of `github-linguist/linguist` `languages.yml` — no `ansi` entry exists
*   Review of GitHub Community discussion #31570 (no ANSI mention; confirmed workarounds are LaTeX-math `$${\color{...}}$$` and `diff` syntax)
**Resolution:** Switched README to `diff` syntax. Documented finding in Section 1.4 so future tasks don't re-attempt ANSI.

---

## SECTION 4: FILE INDEX

### Project Files

| Path                                        | Type        | Description                                              |
|---------------------------------------------|-------------|----------------------------------------------------------|
| `README.md`                                 | Docs        | Minimal HUD-styled README with `diff`-colored ASCII art  |
| `assets/temporal_obs_inspo_1.jpg`           | Reference   | HUD inspo: gold accent, bottom hex bar, SYS.LOG          |
| `assets/temp_obs_inspo_2.png`               | Reference   | HUD inspo: minimal variant, timestamps, CAM ONLINE       |
| `assets/temporal_obs_inspo_3.jpg`           | Reference   | HUD inspo: angular corners, crosshair center             |
| `assets/temp_obs_inspo_4.png`               | Reference   | HUD inspo: sidebar webcam + data panels layout           |

### External References

| Path                                                                          | Type       | Description                                                 |
|-------------------------------------------------------------------------------|------------|-------------------------------------------------------------|
| `~/.brain.d/roam-nodes/content_creation/2025-02-10-content_creation_kyonax_on_tech.org` | Roam node  | @kyonax_on_tech personal brand context (purpose, topics)    |

---

## SECTION 5: LAST INTERACTION

### What was done last

*   Created `README.md` for the project — went through 4 iterations: elaborate KYO banner → minimalist HUD frame → ANSI-colored attempt (rejected) → `diff`-colored final version
*   Investigated and confirmed GitHub does not support `ansi` code blocks; documented this finding in Section 1.4
*   Locked in the project's primary mission: support @kyonax_on_tech content creation through capture-time OBS automation that minimizes editing
*   Cross-referenced the @kyonax_on_tech roam node to ground the project purpose

### Pending / Not yet started

*   [ ] Choose first overlay component to build (main frame / webcam frame / status bar)
*   [ ] Decide implementation tech (HTML/CSS browser sources vs SVG vs canvas)
*   [ ] Phase 1: Build base HUD frame overlay
*   [ ] Create `overlays/`, `scripts/`, `config/` directories
*   [ ] Phases 2–4 (dynamic overlays, OBS WebSocket integration, packaging)

### Where to resume

If the user asks to **start Phase 1** (overlay design): begin by asking which component to prioritize and which tech stack (see Section 2.5). Then create the chosen overlay under `overlays/<component-name>/` following the visual language in Section 1.1 and resolution targets in Section 1.2.

If the user asks to **modify the README**: see Section 3.2 for current structure. Remember: minimal style is locked in (no Why/Stack/Design). Use `diff` for any new colored ASCII (Section 1.4).

If the user asks for a **new task**: check Section 2.5 (Pending Work).

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
