<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **single source of truth** for the **`Kyonax` GitHub profile repo** session — the special `username/username` repository at `/run/media/kyonax/Da_ Disk/dev/github-kyonax/Kyonax/` whose `README.md` renders on Cristian's GitHub profile page. Load this at the start of every conversation about the profile repo. As of **2026-05-21 v2.0**, the repo is reduced to a single `README.md` (78 lines) — the `src/` directory and both raster assets (`KYO.jpg`, `professional_photo.png`) were deleted in the structural rewrite. The README is a **text-only artifact**: no build, no JS, no tests, no dependencies. The aesthetic it projects is a compressed, text-only echo of the broader **cyberpunk + sci-fi design language** developed in the `kyo-web-online` Vue landing. Section 1 abstracts the portable parts of that language, plus carries the conventions for *this* repo (top-head comment shape, badge registers, soft-CTA pattern, neofetch vs kyo-data.sh distinction).

| Section | Purpose | When to reference |
|---|---|---|
| **1. Global Guidelines** | Cyberpunk/sci-fi vibe abstractions, top-head comment convention, badge registers, soft-CTA pattern, R-block anatomy (neofetch vs kyo-data.sh), git-write prohibition. | Before any README edit. Mandatory constraints. |
| **2. Session Overview** | What this repo is (single-file profile README), scope, decisions, pending. | When starting a new task. |
| **3. Implementations** | Per-artifact detail: the current README structure (post-2026-05-21 rewrite), the roam-node plan, the kyo-web-online cross-reference. | When resuming or referencing existing work. |
| **4. File Index** | Quick path table for README + roam node + cross-session references. | When reading, editing, or locating files. |
| **5. Last Interaction** | What was just done (2026-05-21 v2.0 rewrite: 119 → 78 lines, header restored, src/ deleted, soft CTA, email migrated); where to resume. | At conversation start. |
| **6. Activity Log** | Datetime-stamped table of every meaningful event in this session. | When you need exact "what was done when". |

**Operational Rule:** This repo's entire surface is `README.md`. When the user asks for a change, edit the README directly — no scaffolding, no migration, no test step. **Hard rule: NEVER run any git command** (commit / push / tag / merge / rebase / reset / etc.) — the user manages all git operations manually. Generating commit text means *writing to a file*, never invoking `git`. (Reinforces `~/.claude/CLAUDE.md` global prohibition.)

**Key principle:** The cyberpunk/sci-fi guidelines in §1 are **abstracted** from `kyo-web-online.md` — they describe the *brand vocabulary* (HUD labels, kanji, glyphs, palette intent), not the Vue-specific implementation surface (no SCSS tokens, no `_theme.scss`, no `.element-flare` API). When a guideline needs the full Vue implementation, follow the cross-session pointer in the entry.

**Cross-session sources:**
- `kyo-web-online.md` (2026-05-08 reset) — full cyberpunk design system, all CSS APIs, palette tokens, kanji watermarks, HUD utilities. **Read on first load if the work involves visual/brand decisions.**
- `~/.brain.d/roam-nodes/2026-05-21-kyonax_profile_readme_refinement.org` — the v2.0 refinement plan + checklist + open questions. Companion to this file.

---

## SECTION 1: GLOBAL GUIDELINES & REUSABLE PATTERNS

> **Apply these rules to every task in this session.** Loaded skills: `session-reset` (this file). The Vue landing at `kyo-web-online/` is the canonical home for the full cyberpunk system; this section carries the portable, README-friendly subset *plus* conventions specific to this profile-page artifact.

### 1.1 Repo identity (`username/username` special repo)
*   GitHub treats `Kyonax/Kyonax` as a profile-page README repo — its `README.md` renders at the top of `github.com/Kyonax`. Every edit is publicly visible the moment it's pushed.
*   The README sits *above* GitHub's native profile UI (bio, pinned repos, contribution graph, achievements, sponsor button, social links). **Anything the README restates that GitHub already shows natively is dead weight.** This is the strongest single simplification rule. Verified by inspecting the live profile on 2026-05-21.
*   Audience is mixed: recruiters skim first; OSS contributors / CCS members read for projects + links; donors check the wallet block. Optimize copy for the recruiter scan but never strip the cyberpunk identity — the visual personality is part of the signal.
*   The repo has **no build, no CI, no test, no dependencies, no assets** as of 2026-05-21 — `README.md` is the entire surface. Treat any tooling temptation as out of scope.

### 1.2 Cyberpunk / sci-fi vibe (abstracted from `kyo-web-online.md`)
*   **Visual personality:** terminal HUD chrome + kanji watermarks + neutral-on-black palette with a single saturated primary-yellow. The README's R-code block (RENDOM-like figlet + `arch in ~` prompt + `デベロッパー` column + soft signature flourish) is the print-mode echo of that language.
*   **Identity glyphs (recurring across all surfaces):**
    *   **`京`** (kyō / capital) — Kyonax personal mark. Always present in any signature block. Tail of name = `Kyonax<sup>京</sup>` in HTML, or just `京` standalone in monospaced contexts.
    *   **`▣`** (U+25A3) — **Cyber Code Syndicate (CCS)** signature. Use sparingly, only when referencing CCS. In the Vue landing this glyph is wrapped in `.ccs-glyph` for scaling — see [session: kyo-web-online > §1.19] for the full sizing rule. In plain markdown there's no equivalent class, so use it inline at native size.
    *   **`デベロッパー`** (katakana for "developer") — body-copy kanji column for atmospheric texture. In the current README it sits to the right of the figlet in the R-code block.
    *   **Kanji watermark vocabulary** (only relevant if expanding visuals): `開発者` developer, `過去` past, `未来` future, `進化` evolution. Don't introduce new ones without checking the Vue landing's watermark map first.
*   **HUD label register** (when adding chrome-style decorations): all-caps SpaceMono-like, slash-prefixed, `::` separator. Examples from the Vue landing: `// HANDSHAKE :: VERIFIED`, `// VECTOR :: KYO-001`, `// BEACON :: ON`, `// PIPELINE :: OPEN`, `// CHANNEL :: CCS // KYONAX // ZERONET`. Pick a verb-noun pair that matches the surface's role.
*   **Typography pairing** (Vue landing convention — useful when generating PR/social copy that matches the brand): **Geomanist** (900 weight) for headlines + watermarks, **SpaceMono** (uppercase, 0.16em letter-spacing) for HUD labels, badges, chip text. README uses GitHub default fonts, so the pairing applies only when authoring linked content (article PDFs, social cards, talk slides).

### 1.3 Color usage (60/30/10) — README scope
*   Color in the README is shaped by GitHub's rendered shields/badges. Pick badge `color` params that respect the 60/30/10 split: ~60% neutral (gray / dark), ~30% primary (yellow), ~10% semantic state.
*   **Primary-yellow palette** used in this README (and in the kyo-web-online README.org):
    *   `FFE900` — original free/open-software badge color (legacy, deeper saturation).
    *   `FFE564` — kyo-web-online identity-badge body color (lighter, used as the badge fill for CCS Member + Free/Open Software).
    *   `FFD400` — kyo-web-online identity-badge `labelColor` (deeper, used as the badge label background).
*   The canonical Vue-landing token `--clr-primary-100` (HSL `hsl(54, 100%, 50%)`) is close to `FFE900`. The `FFE564`/`FFD400` pair was introduced by kyo-web-online for the badge register and adopted here on 2026-05-21 for cross-surface consistency.
*   **Forbidden:** any third color family in the README that hasn't appeared in the Vue landing palette. The Vue landing has 8 families (primary / secondary / neutral / border / success / warning / error / accent-magenta) — README badges should map into those.

### 1.4 Top-head comment convention (CRITICAL — from kyo-web-online)
*   **Source pattern:** kyo-web-online `vite.config.js` + `README.org`. Two HTML/org comment blocks at the top of every artifact, separated by a blank line.
*   **Block 1 — Copyright (mandatory).** One line: `Copyright (c) <year> Cristian D. Moreno — @Kyonax`. For *code* artifacts in kyo-web-online, a second line follows: `Distributed under the terms of GPL-2.0-only — see LICENSE.` For the **profile README, this second line is omitted** — the README is content, not a project (no LICENSE file). The Copyright line stands alone in its own comment block to preserve the visual structure.
*   **Block 2 — ASCII art + metadata (mandatory).** Contents in order:
    1. Figlet-style ASCII rendering of the project / file name (kyo-web-online: `KYO-WEB-ONLINE` rendered with one of the figlet small-style fonts). For the profile README, the existing 4-line ASCII serves this role.
    2. Blank line.
    3. `<filename> — <one-line role description>`.
    4. `<YYYY-MM-DD> — v<version>` metadata line.
    5. Blank line.
    6. Identity block: full name, ORCID URL, email + handle.
*   **Profile README implementation (current, 2026-05-21):**
    ```html
    <!--
     * Copyright (c) 2026 Cristian D. Moreno — @Kyonax
    -->

    <!--
     *    ___  _______   ___  __  _______
     *   / _ \/ __/ _ | / _ \/  |/  / __/
     *  / , _/ _// __ |/ // / /|_/ / _/
     * /_/|_/___/_/ |_/____/_/  /_/___
     *
     * README.md — GitHub profile-page artifact for Kyonax
     * 2026-05-21 — v2.0
     *
     * Cristian D. Moreno — Kyonax
     * ORCID: https://orcid.org/0009-0006-4459-5538
     * cristian.moreno@kyonax.com — @kyonax_on_tech
    -->
    ```
*   **Hard rule: DO NOT REMOVE THE TOP-HEAD HTML COMMENT.** It was removed in the early 2026-05-21 simplification pass on the grounds that "it doesn't render" — that was wrong. The comment is an authorship + provenance stamp that follows kyo-web-online's repo-wide convention. Its non-rendering nature is the *point* — it's source-visible metadata.
*   **Version bump rule:** patch (`v1.0.2 → v1.0.3`) for copy edits; minor for additive sections; major (`v1.0.2 → v2.0`) for structural rewrites. 2026-05-21 = v2.0 because the rewrite halved the line count and removed the assets.

### 1.5 README content conventions (post-2026-05-21 v2.0)
*   **Hello There paragraph** — recruiter-scan, one paragraph. Carries: full name, nickname (`Kyonax<sup>京</sup>`) with the Kyonax name **hyperlinked to `https://kyonax.com`** (this is the woven-in soft CTA — see §1.7), role (Senior Full-Stack), employer chain (Agile Engine → @MadisonReed e-commerce — *do not name the framework here; pinned repos surface it*), founder roles (@ZerønetLabs, @CyberCodeSyndicate), what you build (high-perf frontends, scalable microservices, reusable UI libs, AI automation), self-tag `(es/en)`. **Do not restate "Cyberpunk and Sci-Fi enthusiast"** — the live bio already says that, and the R-block's `Setup:` line carries the aesthetic tag.
*   **R-code block** has TWO halves, treat them separately. See §1.6 for full anatomy.
*   **Badge cluster** — 4 badges in two registers. See §1.8.
*   **My Work section** — bulleted list of shipped tools. Add new entries as `- [<name>](<link>) - <one-line description>`. Sort: newest first. Drop any "I enjoy building..." or filler intro lines.
*   **Featured Articles section** — publications. Pattern (set 2025-12-21, tightened 2026-05-21):
    *   Top-level bullet → article landing page / Zenodo DOI / canonical URL with a *single* short descriptor. Do not double-mention the same name (e.g., don't write "...for Cyber Code Syndicate (CCS)" when the heading already named CCS).
    *   Nested sub-bullets → language variants with explicit `[English Version]` / `[Versión en Español]` link to the PDF in the article's GitHub repo.
*   **`I use emacs btw.`** is a deliberate line — keep it.
*   **`#`** below it acts as a horizontal rule.
*   **No bottom table** anymore. The 3-cell portrait+stats+kanji table was deleted on 2026-05-21 (duplicated GitHub's native contribution-graph + native avatar).
*   **Centered signature block** at the very bottom — `京` link, Ko-Fi / Patreon / GitHub Sponsor links, three crypto wallet addresses (USDC/SOL, ETC Ethereum, BTC). Treat wallet addresses as immutable — never paraphrase, never partially copy.

### 1.6 R-code block anatomy (CRITICAL — two simulations, not one)
*   The R-code block (`\`\`\`r ... \`\`\``) is rendered with the R syntax hint to pick up GitHub's yellow tint on comment-like strings — that's what gives the block its HUD-readout aesthetic.
*   **PART 1 — The neofetch (top half). KEEP VERBATIM.** The ASCII KYO figlet on the left + the 5 key:value identity lines on the right (`Contact Me` / `Name` / `Nickname` / `Working as` / `XP`) + the small `デベロッパー` decoration *IS* the neofetch. It mirrors how real `neofetch` renders a distro logo with user info beside it — Kyonax is the "distro". This is the identity portrait. **Do NOT add a second neofetch elsewhere — there is exactly one.**
*   **PART 2 — The `kyo-data.sh` script run (bottom half). 3 lines of complementary data.** Anatomy:
    1. `arch in ~` prompt.
    2. `   ø :: ~/.config/brain/kyo-data.sh »` script invocation.
    3. `    'Important Data'  [########################] 100% loaded` loading bar.
    4. Blank line.
    5. Three `key: value` lines indented with 7 spaces, keys padded to align values:
        *   `Location:   <city, country · timezone · spoken languages>`
        *   `Channels:   <CCS // ZerønetLabs // MadisonReed>` (mirrors the Vue landing's footer channel manifest)
        *   `Setup:      <WM · editor · aesthetic tag>` (currently `Hyprland · Doom Emacs · Cyberpunk + Sci-Fi`)
    6. Blank line.
    7. **Soft CTA signature** — single right-aligned line: `~ visit https://kyonax.com :: 京` (the user introduced this exact form on 2026-05-21; the earlier attempt at a duplicated `arch in ~ » visit ...` prompt was replaced because it looked like a hard banner, not a signature flourish). See §1.7 for the soft-CTA pattern in full.
*   **Forbidden in the kyo-data.sh block:**
    *   No skills inventory / programming-languages list / Links rows — GitHub's pinned repos and badges already surface those.
    *   No literal fastfetch fields (CPU / GPU / Memory / Uptime / Packages) — too on-the-nose; the `kyo-data.sh` framing should *suggest* a curated script output, not duplicate a system audit.
    *   No icons / Nerd Font glyphs — GitHub markdown can't render PUA codepoints reliably; ASCII-only.
*   **Why 3 lines:** anything less drops `Setup:` (which is the only place "Cyberpunk + Sci-Fi" appears now that the bio doesn't mention it). Anything more drifts back toward the pre-v2.0 dump.

### 1.7 Soft CTA pattern (introduced 2026-05-21)
*   **Goal:** direct readers to the live site at `https://kyonax.com` without a banner-style "VISIT NOW" button. Soft, seamless, on-vibe.
*   **Two woven layers, both deliberate:**
    1. **Hello There — the Kyonax name itself is the hyperlink.** Markdown: `aka [**Kyonax**](https://kyonax.com)**<sup>京</sup>**`. The reader doesn't see a CTA — they see the identity. Click flow emerges from natural reading attention on the name.
    2. **R-code block end — right-aligned signature flourish.** A single line `~ visit https://kyonax.com :: 京`, indented far enough right to read as a margin signature (not a centered/left-aligned headline). The `~` leading character mimics a home-directory shorthand; the `:: 京` trailing pair mirrors the HUD `::` separator and stamps the Kyonax glyph at the end. The line sits *below* the kyo-data.sh data with a blank line separating them — it reads like a sign-off, not a banner.
*   **What this pattern explicitly avoids:**
    *   Shields-style "Visit Live Site" badge (too loud, doesn't fit the terminal aesthetic).
    *   Centered HTML `<p>` block with link (too marketing).
    *   Multiple CTA repetitions (would feel desperate). Two woven instances is the cap.
    *   Hard prompt-style `arch in ~ » visit ...` continuation lines (tried and rejected on 2026-05-21 — read as a banner, not a signature).
*   **Future CTA additions should follow the same rule:** weave into existing content, don't add a new section.

### 1.8 Badge inventory — two registers (post-2026-05-21)
*   **Register A — Identity badges (`flat-square` + yellow palette).** Static badges asserting affiliation / values. Pattern from kyo-web-online README.org:
    *   `https://img.shields.io/badge/<Label>-FFE564?&logo=<base64-svg>&labelColor=FFD400&style=flat-square`
    *   Current set: **CCS Member** (with base64-encoded SVG of the `▣` square frame icon — lifted verbatim from kyo-web-online), **Free / Open Software** (with `❤️ free/open software` text + URL-encoded `❤️`).
*   **Register B — Social-count badges (`?style=social`).** Dynamic-count GitHub-style social shields with icon + live number. Reserved for *external platform follower counts*.
    *   Current set: **YouTube subs** (`/youtube/channel/subscribers/<channelId>?style=social`), **Twitter follow** (`/twitter/follow/<handle>?style=social`).
    *   **Why a different style:** GitHub's social-count style is the platform-native idiom for live follower numbers — it reads as a *live signal*, not an identity stamp. Mixing the two styles loses the semantic.
*   **Deleted (2026-05-21):** GitHub Followers + GitHub Stars badges — sidebar shows them natively. Plus the previously commented-out social-badge block (Reddit / Instagram / LinkedIn shields) — never re-enable; either the social link sidebar (native) or a register-A identity badge if a specific platform earns one.
*   **Placement order:** Register A first (CCS Member → Free/Open Software), then Register B (YouTube → Twitter). One badge per line; markdown collapses them onto one rendered row.

### 1.9 Asset rules (`src/` directory)
*   **As of 2026-05-21, the `src/` directory does NOT exist.** Both `KYO.jpg` and `professional_photo.png` were deleted along with the bottom 3-cell table.
*   If a future change reintroduces an asset, recreate `src/` and follow the previous filename convention (`UPPERCASE.jpg/.png` or `lowercase_words.png` — pick one and stick to it).
*   **Before reintroducing a portrait,** confirm it's not duplicating the GitHub native avatar. If the avatar is the same image, the README inline copy is redundant.

### 1.10 Glyph storage — typed glyphs vs `\uXXXX` escapes
*   In `.md` files rendered by GitHub, typed Unicode glyphs (`京`, `▣`, `デベロッパー`, `♥`) are safe — GitHub's UTF-8 round-trip is reliable.
*   In any **JS / Vue / config** file (none exist in this repo today, but if added), follow the Vue landing's rule: use `'\uXXXX'` escapes for any PUA glyph (Nerd Font codepoints `≥ U+E000`). See [session: kyo-web-online > §1.15] for the rationale (editor encoding determinism).
*   The `▣` glyph is `U+25A3` (BMP, not PUA) → safe to type directly even in code.

### 1.11 Tilde scrub (Spanish + SpaceMono surfaces)
*   If a future surface (a linked article, presentation slide, social card) uses SpaceMono for Spanish copy, **drop the tildes** from uppercase headers: `MENÚ → MENU`, `UBICACIÓN → UBICACION`, `BOGOTÁ → BOGOTA`, `CONTÁCTAME → CONTACTAME`. SpaceMono's tilde diacritics render misaligned at HUD-label sizes.
*   This README itself uses GitHub-default fonts, so tildes in body Spanish copy are fine. The rule applies only to brand collateral that *mirrors* the Vue landing's SpaceMono register.
*   Full Vue-landing rationale: [session: kyo-web-online > §1.15].

### 1.12 Language + locale
*   Bio paragraph is English-only with an `(es/en)` self-tag. Don't dual-language the README itself — that's the Vue landing's job (vue-i18n powered).
*   Featured-article entries may carry both language variants as sub-bullets (set on 2025-12-21). Pattern: English first, Spanish second, both with explicit `[English Version]` / `[Versión en Español]` link text.

### 1.13 Email — `cristian.moreno@kyonax.com` (migrated 2026-05-21)
*   The canonical contact email across all profile-README surfaces is now **`cristian.moreno@kyonax.com`**.
*   Previous email `kyonax.corp@gmail.com` was retired on 2026-05-21 across both the README (header + R-block Contact Me) and the refinement roam node.
*   The global `~/.claude/CLAUDE.md` still references `cristian.moreno@madison-reed.com` — that's the work email (Agile Engine engagement), kept separate. **Do not** unify them.

### 1.14 Git write prohibition (mirrors global CLAUDE.md)
*   **NEVER** run any of: `git commit`, `git push`, `git tag`, `git merge`, `git rebase`, `git reset --hard`, `git checkout -- .`, `git restore .`, `git clean`, `git stash drop`, `git branch -D`, `gh pr create`, `gh pr merge`, `gh release create` — under **any** circumstance unless the user explicitly and directly requests it.
*   "Prepare a commit message" / "draft the PR text" = write to a file. The user runs git themselves.
*   Source: `~/.claude/CLAUDE.md` global instructions.

---

## SECTION 2: SESSION OVERVIEW

> Project context, scope, and current phase status.

### 2.1 Purpose
Maintain the GitHub profile-page README at `Kyonax/Kyonax`. The README is Cristian's public landing page on GitHub: a tight cyberpunk-themed bio + curated work index + Featured Articles + donation block. The cyberpunk visual DNA is shared with the `kyo-web-online` Vue portfolio (which is the maximalist version of the brand) — this README is the print-mode / text-only echo.

### 2.2 Scope (post-2026-05-21 v2.0)
| Item | Type | Summary | Status |
|---|---|---|---|
| `README.md` | content | 78 lines: 2-block top-head comment + Hello There + R-code block (neofetch + kyo-data.sh + soft CTA) + badges + My Work + Featured Articles + emacs line + signature/wallet block | **LIVE v2.0** (last meaningful edit 2026-05-21) |
| `src/` directory | assets | (deleted) | **REMOVED** (2026-05-21) |
| Featured Articles | section | CCS article (Zenodo + English PDF + Spanish PDF) | **LIVE** (added 2025-12-17, tightened 2026-05-21) |

### 2.3 Key Decisions (Session-Wide)
1. **(2026-05-21)** v2.0 structural rewrite of the README — 119 → 78 lines. **Why:** the README sat above GitHub's native profile UI but duplicated much of it (My Work overlapped pinned repos, stats card overlapped contribution graph, GitHub followers/stars overlapped sidebar). Stripped duplication, kept only what native UI can't show.
2. **(2026-05-21)** Soft CTA pattern (§1.7): Kyonax name in bio = hyperlink to `kyonax.com`; right-aligned signature flourish `~ visit https://kyonax.com :: 京` at end of R-block. **Why:** loud banner-style CTAs would break the cyberpunk terminal aesthetic; weaving the link into existing content keeps the vibe and still surfaces the destination twice.
3. **(2026-05-21)** Header restored + upgraded to kyo-web-online 2-block convention (§1.4). **Why:** the early simplification pass wrongly removed the top-head comment ("doesn't render"); reinstated because the convention is repo-wide (vite.config.js, README.org all carry it) and the comment is the authorship + provenance stamp. v2.0 also bumped here.
4. **(2026-05-21)** kyo-data.sh block rewritten: 13-line skills/programming/links dump → 3 lines `Location` / `Channels` / `Setup`. **Why:** "less is better" applied with the constraint that the kyo-data.sh output should *complement* the neofetch above, not duplicate it. See §1.6.
5. **(2026-05-21)** Vue 3 / framework specialization removed from Hello There. **Why:** pinned repos surface the language tag natively; the bio shouldn't restate it.
6. **(2026-05-21)** Badges split into two registers (§1.8). Identity badges (CCS Member, Free/Open Software) use `flat-square` + `FFE564`/`FFD400` matching kyo-web-online; social-count badges (YouTube, Twitter) restored to `?style=social`. **Why:** different semantic roles (identity stamp vs live-count signal) deserve different styles.
7. **(2026-05-21)** `src/` directory deleted (both KYO.jpg and professional_photo.png removed). **Why:** bottom 3-cell table was deleted (duplicated GitHub native contribution graph); after that, both files were unreferenced and added ~1.2 MB to clone size. User explicitly approved deletion.
8. **(2026-05-21)** No LICENSE file added. **Why:** the profile README is content, not a project (kyo-web-online has GPL-2.0 because it's software); the Copyright line in the header stands alone without a "Distributed under..." clause.
9. **(2026-05-21)** Email migrated `kyonax.corp@gmail.com` → `cristian.moreno@kyonax.com` across README + roam node. Madison Reed work email stays separate (§1.13).
10. **(2026-05-20)** Session file scope: cover only what's portable to a text-only README — abstract cyberpunk system from `kyo-web-online.md` rather than duplicate it.
11. **(2025-12-21)** Featured Articles uses a top-level entry → nested language-variant sub-bullets pattern. Tightened 2026-05-21 to drop the doubled "Cyber Code Syndicate (CCS)" mention.
12. **(2025-12-17)** Heading level for Featured Articles set to `####` to match `#### My Work`. Sibling sections at same depth.

### 2.4 Pending Work
*   **None active.** Per user 2026-05-21 sign-off: "the next steps will be for the future update of it for now is okay." The v2.0 README is stable and shipped (pending the user's own git commit/push).
*   Future candidates (when re-engaged):
    *   Confirm the `https://kyonax.com` URL is live and serves the kyo-web-online Vue landing. If still on `kyonax.github.io`, decide whether to swap the CTA target.
    *   Bump the v2.0 → v2.x in the header when meaningful content edits ship.
    *   Add a Featured Articles entry for the eventual public write-up of the Vue migration (Phase 0–8 retrospective).
    *   Audit the Hello There paragraph if the employment chain changes (Agile Engine → Madison Reed could shift; ZerønetLabs role could evolve).

---

## SECTION 3: IMPLEMENTATIONS

> Each subsection documents a specific artifact's current state.

### 3.1 README.md — v2.0 structure (post-2026-05-21)
**Created:** historical (pre-2023) | **v2.0 rewrite:** 2026-05-21 | **Line count:** 78
**Status:** LIVE v2.0

Top-down layout (current state):

1. **Block 1 head comment** (lines 1-3) — `Copyright (c) 2026 Cristian D. Moreno — @Kyonax`. License clause omitted (content, not project).
2. **Block 2 head comment** (lines 5-17) — figlet ASCII (4 lines) + role line (`README.md — GitHub profile-page artifact for Kyonax`) + version line (`2026-05-21 — v2.0`) + identity block (name + ORCID + email + handle).
3. **`### Hello There,` heading** (line 19) + recruiter-scan paragraph (line 20). Kyonax name hyperlinked to `https://kyonax.com` (woven-in soft CTA, §1.7).
4. **R-code block** (lines 22-40):
    *   Lines 23-30: neofetch — figlet KYO logo + 5 identity key:values + `デベロッパー`.
    *   Lines 31-37: kyo-data.sh — prompt + script invocation + loading bar + 3 complementary lines (Location / Channels / Setup).
    *   Line 39: soft-CTA signature `~ visit https://kyonax.com :: 京`.
5. **Badge cluster** (lines 41-44) — CCS Member + Free/Open Software (Register A, `flat-square`) → YouTube subs + Twitter follow (Register B, `?style=social`).
6. **`#### My Work` section** (lines 46-48) — `org2html` (npm), `webcam2ascii` (GitHub).
7. **`#### Featured Articles` section** (lines 50-53) — CCS article (Zenodo DOI top-level + English + Spanish PDFs nested).
8. **`I use emacs btw.` line** (line 55) — deliberate signature.
9. **`#`** horizontal rule (line 56).
10. **Centered signature + wallet block** (lines 58-78) — `京` link, Ko-Fi / Patreon / GitHub Sponsor, USDC/SOL + ETC + BTC wallet addresses.

### 3.2 Top-head HTML comment (current v2.0 form)
**Status:** LIVE (restored + upgraded on 2026-05-21)

Two-block HTML comment at the top of `README.md`. Block 1 = single-line Copyright. Block 2 = figlet ASCII + role/date/version + identity (name / ORCID / email / handle). Full text in §1.4. Convention sourced from kyo-web-online `vite.config.js` and `README.org`. **Do not remove this block** — it's source-visible metadata that follows the repo-wide convention; the "doesn't render" argument is a misunderstanding of its role.

### 3.3 R-code block (neofetch + kyo-data.sh + soft CTA)
**Status:** LIVE v2.0

Single fenced code block with `r` language hint. Three logical zones (see §1.6 for the full anatomy rule):

*   **Neofetch (lines 23-30):** KYO figlet + Contact / Name / Nickname / Working as / XP / `デベロッパー`. Untouched in v2.0 — this is the identity portrait, change only if the underlying facts change.
*   **kyo-data.sh (lines 31-37):** `arch in ~` prompt + script invocation + `'Important Data' [####] 100% loaded` + 3 lines: `Location: Villavicencio, Colombia · GMT-5 · en/es`, `Channels: CCS // ZerønetLabs // MadisonReed`, `Setup: Hyprland · Doom Emacs · Cyberpunk + Sci-Fi`.
*   **Soft CTA signature (line 39):** right-aligned `~ visit https://kyonax.com :: 京`. Introduced by the user 2026-05-21 in this exact form.

### 3.4 Featured Articles entry (CCS article, 2025-12-16)
**Created:** 2025-12-17 (`0b7df01`) | **Tightened:** 2026-05-21
**Status:** LIVE

Structure:
*   Top-level: link → Zenodo DOI for "Cyber Code Syndicate (CCS) Article" with tightened descriptor "Article advocating Free Software, open collaboration, and ethical development for CCS." (Previous version doubled the "Cyber Code Syndicate (CCS)" mention — fixed 2026-05-21.)
*   Nested EN: `[English Version]` → `Kyonax/ccs-article-2025-12-16-inclusive-pragmatic-free-software-community/.../2025-12-16-building-an-ethical-and-inclusive-coding-community.en.pdf` — "An Inclusive Pragmatic Free Software Community"
*   Nested ES: `[Versión en Español]` → same repo, `.es.pdf` file — "Una Comunidad de Software Libre Inclusiva y Pragmática"

### 3.5 Refinement plan roam node
**Created:** 2026-05-21 | **Location:** `~/.brain.d/roam-nodes/2026-05-21-kyonax_profile_readme_refinement.org`
**Status:** LIVE (planning artifact — useful for retrospective + future iterations)

Carries: principles, current/target state, code-block anatomy (Part 1 neofetch vs Part 2 kyo-data.sh), phase-by-phase checklist (Deletions / kyo-data.sh rewrite / Badge restyle / Copy tightening / Verify+ship), kyo-data.sh block spec, badge spec with the lifted CCS Member base64 SVG, copy-tightening before/after, deletions list, open questions. Companion to this session file. Read on first load when iterating on the README.

---

## SECTION 4: FILE INDEX

> Quick reference for all files relevant to this session.

| File | Association |
|---|---|
| `/run/media/kyonax/Da_ Disk/dev/github-kyonax/Kyonax/README.md` | The profile-page artifact (v2.0, 78 lines) |
| `~/.brain.d/roam-nodes/2026-05-21-kyonax_profile_readme_refinement.org` | v2.0 refinement plan + checklist |
| `~/.claude/CLAUDE.md` | Global git-write prohibition (§1.14) |
| `/run/media/kyonax/Da_ Disk/dev/github-kyonax/dot-files/.config/doom-mac/gptel-directives/sessions/kyo-web-online.md` | **Cross-session reference** — full cyberpunk design system + badge pattern source |
| `/run/media/kyonax/Da_ Disk/dev/github-kyonax/kyo-web-online/` | Sibling repo (Vue landing) — source of the 2-block top-head comment convention + identity-badge pattern |
| `/run/media/kyonax/Da_ Disk/dev/github-kyonax/kyo-web-online/README.org` | Direct source of the CCS Member badge base64 SVG (lifted verbatim into the Kyonax README) |
| `/run/media/kyonax/Da_ Disk/dev/github-kyonax/kyo-web-online/vite.config.js` | Direct source of the 2-block head-comment file convention |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last (2026-05-21)
*   **Full v2.0 structural rewrite of `README.md`** — 119 → 78 lines. Removed bottom 3-cell table, commented-out social-badge block, stray `<div/>` + `<table/>` tags, GitHub Followers + GitHub Stars badges (sidebar duplicates), "I enjoy building..." filler line. Deleted `src/KYO.jpg`, `src/professional_photo.png`, and the empty `src/` directory.
*   **kyo-data.sh block rewrite** — replaced the 13-line skills/programming/links dump with 3 complementary lines (`Location` / `Channels` / `Setup`). Neofetch top half kept verbatim.
*   **Top-head HTML comment restored + upgraded** to the kyo-web-online 2-block convention (Copyright block + ASCII/metadata block). Version bumped to v2.0.
*   **Badges split into two registers** — CCS Member + Free/Open Software in `flat-square` + `FFE564`/`FFD400` (identity); YouTube + Twitter in `?style=social` (live-count). CCS Member badge with base64 SVG logo lifted verbatim from kyo-web-online README.org.
*   **Soft CTA pattern introduced** — Kyonax name in Hello There paragraph hyperlinked to `https://kyonax.com`; right-aligned signature flourish `~ visit https://kyonax.com :: 京` at the end of the R-code block (user-authored exact form, replacing an earlier `arch in ~ » visit ...` continuation that read too banner-like).
*   **Email migrated** `kyonax.corp@gmail.com` → `cristian.moreno@kyonax.com` (2 occurrences in README header + R-block, 1 occurrence in roam node).
*   **Vue 3 / framework specialization removed** from Hello There — pinned repos surface it.
*   **Refinement roam node created** at `~/.brain.d/roam-nodes/2026-05-21-kyonax_profile_readme_refinement.org` with full plan + checklist + kyo-data.sh block spec + badge spec + copy-tightening templates + open questions.
*   **This session file (kyonax-profile.md) reset** to capture the v2.0 state — §1.4 (top-head convention), §1.6 (R-block anatomy), §1.7 (soft-CTA pattern), §1.8 (two badge registers) added as new mandatory rules.

### Pending / Not yet started
*   **Nothing active.** User explicitly closed the iteration: "the next steps will be for the future update of it for now is okay."
*   The README diff (modified `README.md` + deleted `src/KYO.jpg` + deleted `src/professional_photo.png`) is awaiting the user's manual git commit + push. **Do not run git commands.**

### Where to resume
*   **If the user asks to edit the README:** open `README.md`. Follow §1.4 (top-head comment is mandatory), §1.5 (content conventions), §1.6 (R-block anatomy — never collapse the two halves), §1.7 (soft-CTA pattern), §1.8 (badge two-register split). Bump the v2.0 in the header on any meaningful edit per §1.4 version-bump rule.
*   **If the user asks about the cyberpunk system:** stay in §1.2 / §1.3 for the README-scoped subset. For full SCSS tokens / CSS APIs / palette tokens, open `kyo-web-online.md` (path in §4) and read its §1.5–§1.34.
*   **If the user asks to add a new article to Featured Articles:** follow the pattern in §3.4 (top-level Zenodo/canonical link + nested EN/ES PDF sub-bullets).
*   **If the user asks for the refinement plan:** open the roam node at `~/.brain.d/roam-nodes/2026-05-21-kyonax_profile_readme_refinement.org` — open questions are flagged there.
*   **If the user asks anything git-write-shaped:** stop and refuse per §1.14 — they manage git themselves.

---

## SECTION 6: ACTIVITY LOG

> Append-only chronological table of every meaningful event in this session. Newest row first. See `~/.claude/skills/session-reset/rules/activity-log.md` for the schema.

| Datetime         | Duration | Type            | Reference | Description |
|------------------|----------|-----------------|-----------|-------------|
| 2026-05-21 00:59 | 0.25h    | session-reset   | this      | Compacted v2.0 README rewrite session into session file; added §1.4 top-head convention, §1.6 R-block anatomy, §1.7 soft-CTA pattern, §1.8 badge two-register rule; refreshed §2.2 scope (src/ removed), §2.3 decisions (10 new entries), §3.1 README structure (v2.0 lines), §3.5 roam node entry, §4 file index, §5 last interaction |
| 2026-05-21 00:55 | —        | refinement      | this      | User wove CTA into Kyonax name in Hello There (`aka [**Kyonax**](https://kyonax.com)**<sup>京</sup>**`) and replaced the `arch in ~ » visit` prompt continuation with a right-aligned signature flourish `~ visit https://kyonax.com :: 京` at the end of the R-block — final form of the soft-CTA pattern (§1.7) |
| 2026-05-21 00:50 | 0.1h     | refinement      | this      | Three refinements on README: removed Vue 3 mention from Hello There; added first-pass soft CTA (later replaced by user); reverted YouTube + Twitter badges from `flat-square` back to `?style=social` — established the two-register badge rule (§1.8) |
| 2026-05-21 00:45 | 0.1h     | refinement      | this      | Email migration `kyonax.corp@gmail.com` → `cristian.moreno@kyonax.com` across README.md (header + R-block) + refinement roam node `#+EMAIL` |
| 2026-05-21 00:40 | 0.25h    | documentation   | this      | Researched kyo-web-online file-head + root-file conventions (LICENSE, NOTICE, vite.config.js, README.org); restored + upgraded the Kyonax README top-head HTML comment to the 2-block convention (Copyright block + ASCII/metadata block); bumped to v2.0; included ORCID URL; dropped GPL license clause per user decision (content not project) |
| 2026-05-21 00:35 | —        | implementation  | this      | Deleted `src/KYO.jpg` + `src/professional_photo.png` + empty `src/` directory — repo now contains only `README.md` + `.git/` |
| 2026-05-21 00:33 | 0.5h     | implementation  | this      | Wrote v2.0 README per the refinement plan: 119 → 58 lines (pre-header restore); kept neofetch top half verbatim; replaced kyo-data.sh bottom half with 3-line Location/Channels/Setup readout; CCS Member badge (base64 SVG) + Free/Open Software in flat-square FFE564/FFD400; YouTube + Twitter in flat-square (later reverted); tightened Hello There; tightened Featured Articles parent; deleted bottom table + commented-out social-badge block + stray closing tags; dropped "I enjoy building..." filler |
| 2026-05-21 00:10 | 0.5h     | documentation   | this      | Created refinement roam node `~/.brain.d/roam-nodes/2026-05-21-kyonax_profile_readme_refinement.org` with principles, current/target state, code-block anatomy section, 5-phase plan checklist, kyo-data.sh block spec (with rationale per line), badge spec (with full base64 SVG), copy-tightening before/after, deletions list, open questions; updated post-feedback to reclassify the kyo-data.sh portion as separate from the neofetch (not a second system-info readout) |
| 2026-05-20 23:50 | 0.75h    | planning        | this      | Multi-round simplification analysis: read GitHub docs on profile READMEs; inspected the live Kyonax profile (pinned repos + bio + sponsor button + social links + achievements); established the "README sits above native UI" simplification principle; ran fastfetch on this device for reference (Omarchy 3.6.0 / Hyprland 0.54.3 / Linux 6.19.13-arch1-1); iterated through three rounds of cut-list proposals as user pushed back with their own framing of the R-code block intent |
| 2026-05-20 23:45 | 0.25h    | session-reset   | this      | Bootstrapped session file for `Kyonax/Kyonax` profile repo; abstracted cyberpunk/sci-fi vibe guidelines from kyo-web-online.md (§1.2 glyphs, §1.3 palette, §1.4 README conventions, §1.6 glyph escapes, §1.7 tilde scrub); seeded §3 implementations from README + src/ snapshot |

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
