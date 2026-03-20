# Roam Node Skill — Architectural Guide

## Core Philosophy: JIRA as Source, Org-Roam as System of Record

This skill bridges two systems: JIRA (where tickets live) and org-roam (where structured documentation lives). The fundamental principle is that **JIRA data flows in once at creation, then the org-roam node becomes the single source of truth** for all development validation, progress tracking, and documentation.

This one-way flow is deliberate:
- JIRA descriptions are inconsistent (6 format variants, no enforced structure)
- Org-roam nodes enforce a consistent GIVEN/WHEN/THEN structure that enables mechanical validation
- Once translated, the node is the contract — not the JIRA ticket

The only ongoing sync from JIRA is **status** (for index lane placement) and **resolution** (for checkbox state). Content never syncs back.

## The Two-Phase Node Lifecycle

Every node exists in two phases, and understanding this separation is critical:

1. **Phase 1 — Creation:** The skill reads JIRA and populates the node. Everything validation-relevant must be captured. The skill is the author.
2. **Phase 2 — Development:** The developer fills empty sections (DEVELOPMENT AC, DOCUMENTATION, QA INSTRUCTIONS). The developer is the author.

These phases have different validation rules. Phase 1 content is the *requirement* (what must be built). Phase 2 content is the *evidence* (what was built and how to verify it). Validation cross-references both against git changes.

## The Index as a Dashboard

The index file is not just a list — it's a two-layer dashboard that mirrors JIRA's workflow while maintaining org-roam's linking model:

- **BACKLOG** is the source of truth — it holds the org-roam `id:` links to actual node files
- **SPRINT BOARD** is a derived view — it holds internal `file:` references that point to BACKLOG anchors

This architecture means the Sprint Board can be completely regenerated from the BACKLOG + current JIRA statuses. The BACKLOG is the durable layer; the Sprint Board is the ephemeral workflow layer.

## Why Format Parsing Matters

The biggest challenge in this skill is JIRA's lack of description standardization. Six different format variants exist across the DOTCOMPB project, and new ones may appear. The parsing rules are based on real ticket analysis (5 ticket pairs compared JIRA-to-node), not assumptions.

The critical insight from that analysis: **event tracking tables and Figma links were previously lost in all analyzed nodes.** These are now marked CRITICAL capture priority because they're validation-relevant data that the developer needs.

## Connecting the Rules

| Rule                 | Role in the System                                                  |
|----------------------|---------------------------------------------------------------------|
| `templates`          | Defines the exact output structure — what the node looks like       |
| `jira-parsing`       | Defines the input processing — how to read JIRA data                |
| `org-mode-reference` | Defines the syntax constraints — how org-mode renders content       |
| `index-management`   | Defines the dashboard operations — how the index stays in sync      |
| `node-lifecycle`     | Defines the process — when each operation happens and in what order |

The flow is: **parse JIRA** (jira-parsing) → **fill template** (templates) → **verify syntax** (org-mode-reference) → **update index** (index-management) → **validate later** (node-lifecycle).
