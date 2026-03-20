# Emacs & Doom Emacs Best Practices

**Version 1.0.0**
Kyonax
February 2026

## Abstract
A comprehensive guide to best practices for Emacs, Emacs Lisp, and Doom Emacs, designed for AI agents. Contains rules across 2 categories, prioritized by impact from critical (ensuring stability) to high (ensuring maintainability). Each rule includes detailed explanations, real-world examples comparing incorrect vs. correct implementations, and specific context to guide code generation and configuration management.

## 1. Referenced Files
This guide relies on the following detailed rule files. They must be consulted for specific implementation patterns and examples.

*   `rules/testing-rule.md`
*   `rules/naming-conventions.md`

---

## 2. Testing & Stability
**Impact: CRITICAL**
Failure to follow these rules can lead to a non-functional Emacs configuration, preventing the editor from starting. This is the highest priority category.

### 2.1 Mandatory Scratch Buffer Testing
**Impact: CRITICAL (prevents configuration failure)**
**When to Apply:** This rule must be applied whenever any new Emacs Lisp code is written or existing code is modified, *before* it is saved into the primary `config.org` file.
**How it Works:** All Elisp code is tested in isolation within the **Doom Scratch Buffer** (`SPC b x`). This creates a safe, ephemeral environment where syntax errors or runtime bugs will not affect the main Emacs process. A broken function in a scratch buffer is a minor issue; a broken function tangled into `config.el` can prevent Emacs from starting entirely.
**Reference:** For a detailed workflow and examples, see `rules/testing-rule.md`.

---

## 3. Naming & Structure
**Impact: HIGH**
Consistent naming conventions are essential for long-term maintainability, readability, and preventing conflicts with other packages in the Emacs ecosystem.

### 3.1 Apply Naming Conventions
**Impact: HIGH (improves readability and prevents namespace collisions)**
**When to Apply:** This rule must be applied whenever a new function, variable, constant, or any other named symbol is defined within the user's configuration.

**How it Works:** Emacs Lisp operates within a single, global namespace. To prevent collisions, all custom definitions must be namespaced with a personal prefix (e.g., `kyo/`). This practice makes the user's code distinct, easy to identify, and safe from conflicts.

This `AGENTS.md` file mandates the *use* of these conventions, but the specific casing rules (e.g., `kebab-case` for functions, `-p` suffix for predicates) are defined in the referenced file.
**Reference:** For a complete list of naming patterns and examples, you **must consult** `rules/naming-conventions.md`.
