---
name: emacs-expert
description: Best practices guide for Emacs, Emacs Lisp, and Doom Emacs configuration. Use when writing or modifying Elisp code for the user's setup to ensure stability, maintainability, and consistency. Triggers on tasks involving Doom Emacs, config.org, or custom Emacs Lisp functions.
metadata:
  author: Kyonax
  version: "1.0.0"
---

# Emacs Expert Skill Guide
Comprehensive guide to best practices, guidelines, and scenarios for leveraging Emacs, Emacs Lisp, and Doom Emacs, specifically tailored to the user's configuration and workflow.

## When to Apply
Reference these guidelines when:

*   Writing new Emacs Lisp functions or modifying existing ones.
*   Configuring Doom Emacs modules or creating custom layers.
*   Troubleshooting Emacs behavior or performance issues.
*   Seeking optimal workflows and keybindings.
*   Integrating new packages or tools into the Emacs environment.

## Rule Categories

| Priority | Category            | Impact   | Prefix     |
|----------|---------------------|----------|------------|
| 1        | Testing & Stability | CRITICAL | `testing-` |
| 2        | Naming & Structure  | HIGH     | `naming-`  |

## Quick Reference

### 1. Testing & Stability (CRITICAL)
- `testing-rule` - Mandates testing all code changes in the Doom Scratch Buffer (`SPC b x`) *before* integrating them into the main configuration to prevent startup failures.

### 2. Naming & Structure (HIGH)
- `naming-conventions` - Defines standards for naming functions, variables, constants, and predicates to improve readability and prevent namespace collisions.

## How to Use
Read the individual rule files for detailed explanations and code examples:

*   `rules/testing-rule.md`
*   `rules/naming-conventions.md`

Each rule file contains:
- A clear explanation of the rule's purpose.
- Incorrect code examples with explanations.
- Correct code examples with explanations.
- Additional context and workflow guidance.

## Full Compiled Document
For the complete guide with all rules expanded: `AGENTS.md`
