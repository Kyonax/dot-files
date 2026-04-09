<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the Doom Emacs Configuration Analysis session. It documents a comprehensive audit of the `config.org` literate configuration, package management (`packages.el`), module system (`init.el`), org-mode, and org-roam setup. It is loaded at the start of every conversation to give the AI full context without re-discovering anything. Read the sections in order on first load — after that, reference them by number as needed. The data is organized into 5 sections:

| Section                        | Purpose                                                                                              | When to reference                                                                       |
|--------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| **1. Global Guidelines**       | Doom Emacs configuration conventions, literate config rules, and structural constraints.             | Before modifying any config file. These are mandatory constraints.                      |
| **2. Session Overview**        | High-level context: how config.org works, architecture decisions, tangling pipeline, known issues.   | When starting a new config task — understand the system first.                          |
| **3. Implementations**         | Per-subsystem detail: packages, org-mode, org-roam, gptel, editor modes, keybindings, OS probing.   | When working on a specific subsystem or debugging a configuration issue.                |
| **4. File Index**              | Quick-reference of all relevant file paths: configs, templates, brain.d structure, skills.           | When you need to locate or reference a specific file without searching.                 |
| **5. Last Interaction**        | Short-term memory: what was done last, findings, issues identified, where to resume.                 | At the very start of a new conversation — entry point for continuing work.              |

**Operational Rule:** Throughout the conversation, the user will give new tasks. **Always look for the last request, identified by a markdown title starting with `###`.** Any `###` title means it is the newest request. Based on that section, load relevant context and apply the rules from Section 1.

**Key principle:** Data may appear in multiple sections with different framing. Section 1 frames it as a rule to follow. Section 2 frames it as context to understand. Section 3 frames it as an implementation to reference. This is intentional — each section answers a different question about the same knowledge.

---

## SECTION 1: GLOBAL GUIDELINES & CONVENTIONS

> **Apply these rules to every config modification in this session.** These are the structural conventions derived from the existing `config.org` literate configuration and Doom Emacs best practices.

### 1.1 Literate Config Pipeline (config.org -> .el files)

*   **`config.org` is the master file.** It tangles to `config.el`, `packages.el`, and `init.el`. Direct edits to `.el` files will be overwritten on next tangle.
*   **Tangling is triggered by `#+auto_tangle: t`** at the top of `config.org`. The custom function `kyo/tangle-on-save` runs `org-babel-tangle` on every save when this tag is present.
*   **`:tangle` property controls output.** Each `#+begin_src emacs-lisp` block has a `:tangle` property specifying which file it writes to: `config.el`, `packages.el`, `init.el`, or `no` (skip).
*   **Conditional tangling via OS/profile probing.** Blocks use `(org-babel-ref-resolve "get-os-type")` and `(org-babel-ref-resolve "get-profile")` to conditionally tangle only on the correct OS/profile.
*   **Never edit `.el` files directly** — all changes go through `config.org` source blocks.

### 1.2 Multi-OS / Multi-Profile Architecture

*   **OS detection:** `get-os-type` named block returns `"linux"`, `"mac"`, `"windows"`, `"cygwin"`, or `"unknown"`.
*   **Username/Hostname detection:** `get-username` and `get-hostname` named blocks resolve per-OS.
*   **Profile resolution:** `get-profile` maps hostnames to profiles: `"personal-linux"` (kyo-labs), `"work-mac"` (MacBook-Pro-de-COL-AE-052.local), `"default"`.
*   **Identity resolution:** `get-name` and `get-email` return profile-specific values (Kyonax vs Agile Engine).
*   **Conditional tangle pattern:** `:tangle (if (string= (org-babel-ref-resolve "get-profile") "work-mac") "config.el" "no")`.

### 1.3 Doom Emacs Module Conventions

*   **`init.el` declares modules** using `(doom! ...)`. Modules are enabled/disabled by commenting/uncommenting. Flags like `+lsp`, `+tree-sitter`, `+icons` add features.
*   **`packages.el` declares packages** using `(package! name)`. Run `doom sync` after changes. Use `:recipe` for non-MELPA sources, `:disable` to disable built-ins, `unpin!` to track latest.
*   **`config.el` configures packages** using `use-package!`, `after!`, `map!`, `setq`, etc. Doom-specific macros (`use-package!`, `after!`, `map!`) preferred over vanilla Emacs equivalents.
*   **`after!` is critical** — wrap config for lazy-loaded packages in `(after! package-name ...)` to ensure the package is loaded before configuring it.

### 1.4 Org-Roam Conventions (Doom + Brain.d)

*   **Org-roam directory:** `~/.brain.d/roam-nodes` — all nodes live here in subdirectories.
*   **Dailies directory:** `~/.brain.d/agenda` — separate from roam-nodes.
*   **Capture templates:** Defined in `my-org-roam-capture-templates` variable, loaded via `(after! org-roam ...)`.
*   **`kyo/org-roam-prompted-file`** prompts for subdirectory and title, generating date-prefixed slugs.
*   **`org-roam-v2-ack t`** is set — acknowledges v2 migration.
*   **`org-roam-completion-everywhere t`** — enables node completion in all buffers.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Full audit of the Doom Emacs configuration to understand how `config.org` works as a literate config, how packages are installed, and how org-mode and org-roam are configured. This session identifies architectural patterns, potential issues, and documents the full system for future reference.

### 2.2 Architecture Summary

The configuration follows a **literate programming** approach using `org-babel-tangle`:

```
config.org (master, ~2400 lines)
  ├── tangles to -> config.el (main configuration)
  ├── tangles to -> packages.el (package declarations)
  └── tangles to -> init.el (Doom module declarations)
```

The `literate` module is enabled in `init.el` (line 184: `:config literate`), which tells Doom to automatically tangle `config.org` to `config.el` on startup. However, the user also has a **custom auto-tangle system** (`kyo/tangle-on-save`) that tangles ALL three files on every save of `config.org`.

### 2.3 Key Findings & Issues

| Finding | Severity | Description |
|---------|----------|-------------|
| **Dual dashboard definition** | MEDIUM | Two `kyo/my-shit-is-always-greater` functions defined — one tangled (block-art), one not tangled (braille-art). The tangled one uses block characters, the `:tangle no` one uses braille. No conflict at runtime, but the org file has redundant definitions. |
| **Duplicate `(package! orderless)`** | LOW | Declared twice in packages.el: once under CORFU (line 67) and once under ORDERLESS (line 178). `doom sync` handles this gracefully but it's unnecessary. |
| **Duplicate `(package! rainbow-mode)`** | LOW | Declared at line 150 (under LANG) and again at line 190 (under RAINBOW-MODE). |
| **Duplicate `(package! web-mode)`** | LOW | Declared at line 155 (under LANG) and again at line 202 (under WEB-MODE). |
| **`:ensure t` in Doom** | LOW | Used in `editorconfig` and `org-fancy-priorities` config. In Doom Emacs, `:ensure` is unnecessary — packages are managed by `packages.el`, not `use-package`'s built-in installer. Harmless but misleading. |
| **`use-package` vs `use-package!`** | LOW | Some blocks use vanilla `use-package` (e.g., `beacon`, `editorconfig`, `doom-modeline`) instead of Doom's `use-package!`. Both work, but `use-package!` integrates better with Doom's lazy-loading. |
| **MELPA manual config** | MEDIUM | `(package-initialize)` is called manually in the MELPA section. In Doom, `straight.el` manages packages, not `package.el`. Calling `(package-initialize)` can conflict with straight's sandboxed approach. Consider removing this block or guarding it. |
| **`org-directory` mismatch** | LOW | `org-directory` is set to `"~/org"` but all actual org content lives in `~/.brain.d/`. This variable is used by some org functions as a default. Consider setting it to `"~/.brain.d/"`. |
| **org-babel languages** | LOW | `(web . t)` and `(json . t)` may not have built-in babel support. These could produce warnings on load. |
| **`config.no` file** | INFO | Contains a separate flycheck/eslint config. This appears to be a standalone file (not tangled from config.org) — possibly a work-in-progress or manual override. |
| **Auto-tangle vs Doom literate** | INFO | Both systems are active: Doom's `:config literate` module tangles `config.org -> config.el` on startup, AND `kyo/tangle-on-save` tangles on every save. The custom system tangles to all 3 files (config.el, packages.el, init.el), while Doom's literate module only handles config.el. The custom system is more complete. |

### 2.4 Scope

| Area | Status | Summary |
|------|--------|---------|
| Literate config pipeline | **ANALYZED** | config.org tangles to 3 .el files via org-babel + auto-tangle |
| Multi-OS architecture | **ANALYZED** | OS/profile probing via named blocks, conditional tangle |
| Package management | **ANALYZED** | 70+ packages declared via `(package!)`, some duplicates |
| Doom modules (init.el) | **ANALYZED** | Full module set with literate config enabled |
| Org-mode config | **ANALYZED** | TODO keywords, agenda, babel, priorities, superstar |
| Org-roam config | **ANALYZED** | v2, custom capture templates, prompted file function, roam-ui |
| GPTel / AI integration | **ANALYZED** | Multi-backend (Azure + Local), agentic send, skill system |
| Editor modes | **ANALYZED** | JS2, TypeScript, Web, SCSS, CSS, JSON, LSP integration |
| Flycheck / Linting | **ANALYZED** | Custom eslint-d-strict checker, profile-conditional |
| Keybindings | **ANALYZED** | Extensive custom keybindings under SPC leader |

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 Literate Config System (config.org)

The `config.org` file (~2400 lines) is the single source of truth. It uses org-babel's `:tangle` property to generate three output files. Key architectural pattern:

**Named blocks for OS/profile detection:**
- `get-os-type` → returns `"linux"`, `"mac"`, `"windows"`, etc.
- `get-username` → OS-aware username resolution
- `get-hostname` → OS-aware hostname resolution
- `get-profile` → maps hostname to profile (`"personal-linux"`, `"work-mac"`, `"default"`)
- `get-name` / `get-email` → profile-aware identity

**Conditional tangle pattern:** Blocks reference these named blocks to decide whether to tangle:
```elisp
:tangle (if (string= (org-babel-ref-resolve "get-profile") "work-mac") "config.el" "no")
```

This enables **one config.org file across all OS/machines** — blocks only tangle for the correct environment.

**Auto-tangle:** `kyo/tangle-on-save` in `after-save-hook` checks for `#+auto_tangle: t` header, then runs `org-babel-tangle`. This is separate from Doom's built-in literate module but more powerful (tangles all 3 files, not just config.el).

### 3.2 Package Management (packages.el)

**70+ packages** declared via `(package! name)`. Organization in config.org:
- Each package group has its own `***` heading under `** packages.el`
- Some use conditional tangle for OS-specific packages (e.g., `osx-dictionary`, `osx-trash`)
- `(unpin! org-roam company-org-roam)` unpins org-roam for latest updates
- `(disable-packages! apheleia)` disables Doom's built-in formatter

**Package categories:**
- **Completion:** corfu, cape, orderless, helm (full suite)
- **Navigation:** ace-jump, beacon, treemacs, winum
- **Org ecosystem:** org-roam, org-roam-ui, org-superstar, org-fancy-priorities, org-auto-tangle, org-appear, org-modern, org-download, org-noter, org-pomodoro, org-contacts, org-web-tools, org-tree-slide, org-re-reveal, ox-hugo, ox-pandoc
- **Dev tools:** lsp-mode, lsp-ui, lsp-treemacs, flycheck-posframe, prettier-js, editorconfig
- **AI:** gptel, gptel-quick, gptel-magit
- **Web dev:** web-mode, pug-mode, scss-mode, stylus-mode, emmet-mode, haml-mode, slim-mode
- **Git:** evil-magit, forge, code-review
- **Lang support:** js2, typescript, json, swift, cc, latex, markdown, rest+jq

### 3.3 Doom Modules (init.el)

**Key enabled modules and their flags:**

| Category | Module | Flags | Notes |
|----------|--------|-------|-------|
| Completion | corfu | +orderless +icons | Primary completion framework |
| Completion | helm | +childframe +fuzzy +icons | Search/navigation |
| UI | treemacs | +lsp | File tree |
| UI | ligatures | +extras | Font ligatures |
| Editor | evil | +everywhere | Vim keybindings |
| Editor | format | +onsave +lsp | Auto-format on save |
| Editor | whitespace | +guess +trim | Whitespace management |
| Tools | lsp | +peek | Language Server Protocol |
| Tools | magit | +forge | Git porcelain + GitHub |
| Tools | llm | | AI/LLM integration |
| Lang | org | +crypt +gnuplot +hugo +journal +dragndrop +present | Full org suite |
| Lang | javascript | +lsp +tree-sitter | JS development |
| Lang | web | +lsp +tree-sitter | Web development |
| Config | **literate** | | Enables config.org tangling |

**Notable:** The `literate` module at line 184 is what makes Doom recognize `config.org` and tangle it to `config.el` on startup. Without this, only the custom `kyo/tangle-on-save` would handle tangling.

### 3.4 Org-Mode Configuration

**TODO keywords (profile-dependent):**
```
TODO(t) | CODE(m) | TEST | DEVELOP | MEET(5) | PROYECT(p) | REVIEW(r) | WAIT(w) || DONE(d) | CANCELLED(c)
```
Note: `TEST` shortcut differs between profiles — `(c)` on Linux, `(s)` on Mac. `DEVELOP` shortcut also differs — `(d)` on Linux, `(o)` on Mac. This avoids shortcut conflicts with `DONE(d)` on Mac.

**Agenda:** Single agenda file at `~/.brain.d/agenda/agenda.org`. Custom agenda views with priority-based filtering and tag-based sections (work, meeting, madison-reed, agile-engine, dot-com on Mac; home, health, kyo, event on Linux).

**Babel languages:** emacs-lisp, typescript, javascript, js, json, php, web. TypeScript uses `npx -p typescript -- tsc`. Babel evaluation confirmation is disabled (`org-confirm-babel-evaluate nil`).

**Visual:** org-superstar with custom bullets (`    󰺕 󰻂 󰪥 󰻃`), org-fancy-priorities with nerd-font icons, emphasis markers hidden.

### 3.5 Org-Roam Configuration

**Core setup (inside `after! org-roam`):**
- `org-roam-directory` → `~/.brain.d/roam-nodes`
- `org-roam-dailies-directory` → `~/.brain.d/agenda`
- `org-roam-completion-everywhere` → `t`
- `org-roam-v2-ack` → `t`
- `org-roam-capture-templates` → `my-org-roam-capture-templates`

**Capture templates (5 types):**

| Key | Type | Template Source | Target |
|-----|------|-----------------|--------|
| `d` | Default | Inline `%?` | `kyo/org-roam-prompted-file` |
| `x` | New Index | Inline with TITLE, AUTHOR, EMAIL, FILETAGS prompts | Date-slugged file |
| `i` | Sentinel Inspection | `~/.brain.d/templates/new-node-sentinel-inspection.org` | `kyo/org-roam-prompted-file` |
| `v` | Invoice | `~/.brain.d/templates/new-node-invoice.org` | `kyo/org-roam-prompted-file` |
| `p` | Jira Ticket | `~/.brain.d/templates/new-node-project.org` | `kyo/org-roam-prompted-file` |
| `c` | Doc | `~/.brain.d/templates/new-node-doc.org` | `kyo/org-roam-prompted-file` |

**`kyo/org-roam-prompted-file`:** Custom function that prompts for a subdirectory within `~/.brain.d/roam-nodes`, asks for a title, generates a slug, and returns a date-prefixed filepath (e.g., `madison_reed/2026-04-09-123456-my_ticket.org`).

**Org-roam-ui:** Configured with websocket dependency, auto-starts on init, syncs theme, follows cursor, updates on save.

**Keybindings:**
- `SPC n D` prefix → Dailies capture (Yesterday/Today/Tomorrow/Calendar)
- `SPC n e` prefix → Dailies goto (Date/Yesterday/Today/Tomorrow)
- `SPC n r` prefix → Node operations (Find/Insert/Update directory/DB sync)

### 3.6 GPTel / AI Integration

**Multi-backend architecture:**
- **Backend A (Azure):** Github Copilot Models via `models.inference.ai.azure.com`. Models: codestral-2501, grok-3, phi-4-reasoning, phi-4, phi-4-mini-instruct, mai-ds-r1, gpt-4.1.
- **Backend B (Local):** Gemini via LiteLLM at `localhost:4100`. Models: gemini-gemma-free, gemini-backup-flash, gemini-pro-paid.

**Intelligent model switching:** `my-gptel-model-map` maps each model name to its backend. `my-gptel-switch-model` (bound to `C-c m`) switches both backend and model in one step.

**Agentic send pipeline (`kyo/gptel-send-agentic`):**
1. Extracts prompt text from buffer/region
2. Builds skill summaries from `gptel-directives/skills/` (reads SKILL.md + rule YAML frontmatter)
3. Sends to analyzer model (gemini-backup-flash) for semantic skill matching
4. Parses static skill declarations (`// SKILLS:`) from directive + dynamic skills from analyzer response
5. Loads matched skill files (SKILL.md, AGENTS.md, rules/*.md)
6. Injects skills into directive payload at `<!--DIRECTIVE START SECTION-->`
7. Sends final enriched payload to user's selected model
8. Logs both analysis step and final payload

**Directive loading:** Scans `gptel-directives/*.md`, prepends shared context from `context/agent/`, `context/memory/`, `context/user/` folders, wraps in priority markers.

### 3.7 Editor Modes & LSP

**Mode configurations:**
- **js2-mode:** `.js`, `.jsx`, `.mjs`, `.cjs`, `.es6`, `.plugin.js` → LSP deferred
- **typescript-mode:** `.ts`, `.tsx` → LSP deferred
- **web-mode:** `.html`, `.vue`, `.html.twig` → custom setup with pug/stylus engine detection, 2-space indent
- **scss-mode / css-mode:** LSP deferred, flycheck, whitespace
- **json-mode:** `.json` files

**LSP:** Breadcrumb mode, `C-c l` prefix, which-key integration. LSP-UI shows docs on cursor with 0.2s delay.

**Flycheck (profile-dependent):**
- **work-mac:** Custom `eslint-d-strict` checker with stdin piping, `eslint_d_strict` wrapper
- **personal-linux:** Custom `eslint` checker with source-inplace
- Both disable default JS/TS checkers and force their custom checker

**Formatter strategy:** Prettier-js is explicitly disabled (hooks removed, aliased to `ignore`). Formatting handled by LSP (`+format +onsave +lsp` in init.el) and custom eslint-fix commands. Format tracer utilities available for debugging.

---

## SECTION 4: FILE INDEX

### 4.1 Doom Config Files

| File | Purpose |
|------|---------|
| `~/.config/doom/config.org` | Master literate config (~2400 lines), tangles to all .el files |
| `~/.config/doom/config.el` | Generated — main configuration (DO NOT EDIT DIRECTLY) |
| `~/.config/doom/packages.el` | Generated — package declarations (DO NOT EDIT DIRECTLY) |
| `~/.config/doom/init.el` | Generated — Doom module declarations (DO NOT EDIT DIRECTLY) |
| `~/.config/doom/custom.el` | Emacs custom-set-variables (auto-generated) |
| `~/.config/doom/config.no` | Standalone flycheck/eslint config (not tangled, possibly WIP) |
| `~/.config/doom/env-extra` | Extra environment variables loaded on work-mac profile |
| `~/.config/doom/contacts.org` | Org-contacts file |
| `~/.config/doom/templates/` | File templates for new buffers |
| `~/.config/doom/snippets/` | Yasnippet snippets |

### 4.2 GPTel / AI System

| File | Purpose |
|------|---------|
| `~/.config/doom/gptel-directives/` | Root for all GPTel directives |
| `~/.config/doom/gptel-directives/skills/` | Skill definitions (SKILL.md, AGENTS.md, rules/) |
| `~/.config/doom/gptel-directives/sessions/` | Session context files (this file lives here) |
| `~/.config/doom/gptel-directives/logs/` | Agentic request logs |
| `~/.config/doom/gptel-directives/logs/analysis/` | Analyzer step logs |
| `~/.config/doom/gptel-directives/context/agent/` | Agent context files |
| `~/.config/doom/gptel-directives/context/memory/` | Memory context files |
| `~/.config/doom/gptel-directives/context/user/` | User context files |
| `~/.config/doom/gptel-directives/context/system/skill-analyzer.md` | Analyzer prompt template |

### 4.3 Brain.d (Org-Roam Knowledge Base)

| Path | Purpose |
|------|---------|
| `~/.brain.d/` | Root of org-roam knowledge base |
| `~/.brain.d/roam-nodes/` | Org-roam node directory |
| `~/.brain.d/roam-nodes/madison_reed/` | Madison Reed work tickets and docs |
| `~/.brain.d/roam-nodes/cyber_code_syndicate/` | CCS project nodes |
| `~/.brain.d/roam-nodes/gentleman_staff/` | Gentleman Staff project nodes |
| `~/.brain.d/roam-nodes/knowledge/` | General knowledge nodes |
| `~/.brain.d/roam-nodes/personal_stuff/` | Personal nodes |
| `~/.brain.d/roam-nodes/content_creation/` | Content creation nodes |
| `~/.brain.d/agenda/` | Org-agenda + org-roam dailies |
| `~/.brain.d/bookmarks/bookmarks` | Emacs bookmarks file |
| `~/.brain.d/templates/` | Org-roam capture templates |
| `~/.brain.d/templates/new-node-project.org` | Jira ticket template |
| `~/.brain.d/templates/new-node-doc.org` | Documentation template |
| `~/.brain.d/templates/new-node-invoice.org` | Invoice template |
| `~/.brain.d/templates/new-node-sentinel-inspection.org` | Sentinel inspection template |
| `~/.brain.d/latex/` | LaTeX export support |

---

## SECTION 5: LAST INTERACTION

### 5.1 Session State (2026-04-09)

**What was done:**
- Full read and analysis of `config.org` (~2400 lines), `config.el`, `packages.el`, `init.el`, `config.no`
- Documented the literate config pipeline (config.org -> 3 .el files via org-babel-tangle)
- Documented the multi-OS/multi-profile conditional tangle system
- Cataloged all 70+ packages and their organization
- Analyzed org-mode config: TODO keywords, agenda, babel, priorities
- Analyzed org-roam config: directory structure, capture templates, prompted-file function, roam-ui
- Analyzed GPTel multi-backend architecture and agentic send pipeline
- Analyzed editor modes, LSP, flycheck, and formatter strategy
- Identified 11 findings (see Section 2.3) ranging from duplicates to potential conflicts

**Issues identified for potential action:**
1. Duplicate package declarations (orderless, rainbow-mode, web-mode) — cleanup opportunity
2. `(package-initialize)` call may conflict with Doom's straight.el — investigate
3. `org-directory` set to `~/org` but all content is in `~/.brain.d/` — consider updating
4. `:ensure t` usage is unnecessary in Doom — cosmetic cleanup
5. `use-package` vs `use-package!` inconsistency — cosmetic

**Where to resume:**
- User may want to fix the identified issues
- User may want to restructure or optimize specific subsystems
- The GPTel agentic pipeline is particularly sophisticated and could benefit from its own dedicated session

### 5.2 Package Audit & Fix (2026-04-09)

**Problem:** `custom.el` contained `package-selected-packages` with 11 packages installed via Emacs' `package.el` instead of Doom's `package!` system. These packages only exist on this machine and won't sync to other devices via the dot-files repo.

**Audit results:**

| Package | In custom.el | In packages.el | Used in config | Action |
|---------|-------------|----------------|----------------|--------|
| `ace-jump-mode` | YES | NO | YES (keybindings SPC j) | **ADDED to packages.el via config.org** |
| `crystal-point` | YES | NO | YES (prog-mode hook) | **ADDED to packages.el via config.org** |
| `apheleia` | YES | DISABLED | Explicitly disabled | No action needed |
| `js2-mode` | YES | NO | YES | Built-in via Doom's `javascript` module — no action needed |
| `pug-mode` | YES | YES | YES | Already correct |
| `eslintd-fix` | YES | NO | NO (uses custom `kyo/eslint-strict-fix-file` instead) | **REMOVED from custom.el** |
| `elisp-format` | YES | NO | NO | **REMOVED from custom.el** |
| `git-lens` | YES | NO | NO | **REMOVED from custom.el** |
| `gruber-darker-theme` | YES | NO | Mentioned as option only | **REMOVED from custom.el** |
| `nano-theme` | YES | NO | NO | **REMOVED from custom.el** |
| `vc-msg` | YES | NO | NO | **REMOVED from custom.el** |

**Additional fixes:**
- Removed stale `org-agenda-files` override from `custom.el` — it was adding a specific Madison Reed ticket (`dotcompb_6853.org`) alongside `agenda.org`, overriding the config.org setting at runtime. This was likely set interactively and persisted unintentionally.
- Kept `helm-minibuffer-history-key "M-p"` in custom.el (valid customize setting).

**Other files audited:**
- `config.no` — Dead file, not loaded by anything. Contains an older flycheck/eslint config. Superseded by config.org's profile-conditional flycheck blocks.
- `env-extra` — Environment variables snapshot loaded only on `work-mac` profile. Contains PATH, NVM, NODE_ENV, GPG, SSH config. Working as intended.
- `ai-proxy/` — LiteLLM config (`config.yaml`) for local Gemini routing (free -> paid fallback). Not an Emacs package concern.

### Latest user request below this line:
