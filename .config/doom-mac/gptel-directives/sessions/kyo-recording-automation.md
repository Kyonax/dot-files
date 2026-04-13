<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the Kyo Recording Automation project. It tracks the design, implementation, and session state for building a custom OBS recording/streaming automation system with a cyberpunk/sci-fi HUD overlay. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read the sections in order on first load — after that, reference them by number as needed. The data is organized into 5 sections:

| Section                        | Purpose                                                                                              | When to reference                                                                       |
|--------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| **1. Global Guidelines**       | Design constraints, visual language rules, and technical requirements that apply to ALL work.        | Before creating or modifying any overlay, script, or automation component. Mandatory.   |
| **2. Session Overview**        | High-level context: project scope, goals, current status, key decisions, and pending work.           | When starting a new task — understand scope and status first.                           |
| **3. Implementations**         | Per-component detail: what was built, file structure, decisions made, current state.                 | When resuming work on a specific component or reviewing what happened.                  |
| **4. File Index**              | Quick-reference of all relevant file paths: overlays, scripts, assets, configs.                      | When locating files or assets without searching.                                        |
| **5. Last Interaction**        | Short-term memory: what was done last, what's pending, where to resume.                              | At the very start of a new conversation — entry point for continuing work.              |

**Operational Rule:** Throughout the conversation, the user will give new tasks. **Always look for the last request, identified by a markdown title starting with `###`.** Any `###` title means it is the newest request.

**Key principle:** Data may appear in multiple sections with different framing. Section 1 frames it as a rule to follow. Section 2 frames it as context to understand. Section 3 frames it as an implementation to reference. This is intentional — each section answers a different question about the same knowledge.

---

## SECTION 1: GLOBAL GUIDELINES & DESIGN CONSTRAINTS

> **Apply these rules to every task in this session.** These are the visual language and technical constraints derived from the inspiration assets and project goals.

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

### 1.3 File Organization

*   **`assets/`** — Inspiration images, reference materials, exported static assets.
*   **`overlays/`** — HTML/CSS/JS browser source overlays (one directory per overlay component).
*   **`scripts/`** — Automation scripts (OBS WebSocket, scene control, recording triggers).
*   **`config/`** — OBS scene collection exports, profile configs, automation settings.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Build a complete OBS recording/streaming automation system with a custom cyberpunk/sci-fi HUD overlay. The system should handle automated scene switching, recording controls, and dynamic overlay updates — creating a distinctive visual identity for Kyonax's recording setup.

### 2.2 Scope

| Phase                              | Type       | Summary                                                              | Status          |
|------------------------------------|------------|----------------------------------------------------------------------|-----------------|
| Phase 0: Inspiration & Design      | Design     | Collect reference images, define visual language, plan layout         | **IN PROGRESS** |
| Phase 1: Static Overlay Design     | Design     | Create base HUD overlay frames (HTML/CSS or SVG)                     | NOT STARTED     |
| Phase 2: Dynamic Overlays          | Dev        | Add JS-driven dynamic elements (clock, status, audio vis)            | NOT STARTED     |
| Phase 3: OBS Integration           | Dev        | OBS WebSocket automation, scene switching, recording triggers        | NOT STARTED     |
| Phase 4: Polish & Packaging        | Refinement | Final adjustments, documentation, OBS scene collection export        | NOT STARTED     |

### 2.3 Key Decisions (Session-Wide)

1.  **(2026-04-08)** Project initialized with 4 inspiration images defining the visual direction: cyberpunk HUD with black/gold/white palette.
2.  **(2026-04-08)** Repo created at `github.com/kyonax/kyo-recording-automation`, single "Init Project" commit.

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

---

## SECTION 3: IMPLEMENTATIONS

> No implementations yet — project is in the inspiration/design phase.

### 3.1 Repository Structure (Current)

```
kyo-recording-automation/
  assets/
    temp_obs_inspo_2.png      — HUD overlay reference (variant 2)
    temp_obs_inspo_4.png      — HUD overlay reference (variant 4, sidebar webcam)
    temporal_obs_inspo_1.jpg   — HUD overlay reference (variant 1, primary)
    temporal_obs_inspo_3.jpg   — HUD overlay reference (variant 3, angular corners)
```

---

## SECTION 4: FILE INDEX

| Path                                        | Type        | Description                                      |
|---------------------------------------------|-------------|--------------------------------------------------|
| `assets/temporal_obs_inspo_1.jpg`           | Reference   | HUD inspo: gold accent, bottom hex bar, SYS.LOG  |
| `assets/temp_obs_inspo_2.png`               | Reference   | HUD inspo: minimal variant, timestamps, CAM ONLINE |
| `assets/temporal_obs_inspo_3.jpg`           | Reference   | HUD inspo: angular corners, crosshair center     |
| `assets/temp_obs_inspo_4.png`               | Reference   | HUD inspo: sidebar webcam + data panels layout   |

---

## SECTION 5: LAST INTERACTION

### 5.1 Last Action (2026-04-08)

*   **What:** Session file created. Repo analyzed — single init commit, 4 inspiration images in `assets/`.
*   **Finding:** All 4 images share a consistent cyberpunk HUD visual language (black/gold/white, monospaced, tactical frames). This defines the design direction.
*   **Status:** Phase 0 (Inspiration & Design) in progress. No code or overlays exist yet.

### 5.2 Next Steps

1.  Define which overlay components to build first (main recording frame? webcam frame? status bar?).
2.  Decide on implementation approach: pure HTML/CSS browser sources vs. SVG-based vs. canvas-rendered.
3.  Start Phase 1 with the base HUD frame overlay.

<!-- DESCRIPTION AND USER CONTEXT END -->
<!-- INIT OF THE USER PROMPT END -->
