<!-- INIT OF THE USER PROMPT START -->
<!-- DESCRIPTION AND USER CONTEXT START -->

This file is the **session context block** for the Omarchy Installation project. It tracks progress, decisions, and session state for recovering data from a broken Arch Linux + CachyOS system and installing Omarchy v3.4.2 as the replacement OS. The detailed step-by-step plan lives in the org-roam node: `~/.brain.d/roam-nodes/knowledge/2026-03-20-omarchy_installation.org` — this context block is a summary and session resume, not the plan itself. The data is organized into 5 sections:

| Section                        | Purpose                                                                                              | When to reference                                                                       |
|--------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| **1. Global Guidelines**       | Safety constraints for disk operations and recovery that apply to ALL work.                          | Before any disk, mount, or install operation. Mandatory constraints.                    |
| **2. Session Overview**        | High-level context: project scope, current system state, phase status, key decisions, pending work.  | When starting a new task — understand scope and status first.                           |
| **3. Implementations**         | Summary of what was done per phase, outcomes, issues encountered, decisions made.                    | When resuming a specific phase or reviewing what happened.                              |
| **4. File Index**              | Quick-reference of all relevant file paths: org nodes, backup locations, disk devices, scripts.      | When locating files, disks, or backup paths without re-discovering.                     |
| **5. Last Interaction**        | Short-term memory: what was done last, what's pending, where to resume.                              | At the very start of a new conversation — entry point for continuing work.              |

**Operational Rule:** Throughout the conversation, the user will give new tasks. **Always look for the last request, identified by a markdown title starting with `###`.** Any `###` title means it is the newest request. For detailed steps and commands, reference the org-roam plan file.

**Key principle:** This context block summarizes and tracks progress. The org-roam node (`2026-03-20-omarchy_installation.org`) contains the full plan with commands, validation gates, and step-by-step instructions. Always cross-reference both.

---

## SECTION 1: GLOBAL GUIDELINES & SAFETY CONSTRAINTS

> **Apply these rules to every task in this session.** These are non-negotiable safety constraints for disk operations during recovery and installation. The full plan in the org-roam node includes commands — these guidelines govern how those commands are executed.

### 1.1 Disk Safety

*   **NVME is READ-ONLY during recovery** — mount `/dev/nvme0n1p3` with `-o ro` only. Zero writes until recovery is fully validated.
*   **Verify UUIDs before mounting** — always run `blkid` on the target device before mounting. Confirm UUID matches the disk map before proceeding.
*   **DO NOT TOUCH `/dev/nvme1n1`** — this is the Windows/Apps NTFS disk (238.5G, "Apps"/"Disk"). Completely off-limits throughout the entire process.
*   **No format on `/dev/sda3`** — it's NTFS with existing data labeled "Storage". Backup goes alongside existing content, never format.
*   **No format without explicit user confirmation** — never `mkfs`, `dd`, or overwrite a partition without the user confirming the target device.
*   **Double-check mount points** — verify source and destination with `mount | grep` and `blkid` before any copy operation.

### 1.2 NTFS Hybrid Backup Strategy

*   **Direct file copies (`cp -a`)** for most data — makes files browsable on Storage, but Linux permissions/ACLs/xattrs are lost on NTFS.
*   **Additional tar archives** for permission-critical directories (`.ssh`, `.gnupg`) — these preserve full Linux metadata inside the tarball.
*   **Phase 3 restoration** uses tar archives for SSH/GPG keys (permissions matter) and direct copies for everything else.
*   **Run checker before mover** — always review the backup report before transferring any data.
*   **Re-run checker after mover** — confirm nothing was left behind before wiping the NVME.

### 1.3 Phase Ordering

*   **Data first, OS second** — Phase 1 (recovery) must be 100% validated before Phase 2 (Omarchy install) begins.
*   **Recovery priority:** SSH/GPG keys > `.brain.d` > dotfiles/git repos > `.config/` > Documents > system configs > package lists > everything else.
*   **Script-driven recovery** — backup is managed by `checker.sh` (analysis) and `mover.sh` (transfer) from the `pc-migration-scripts` repo. No manual tar commands.

### 1.4 Omarchy Prerequisites

*   **BIOS:** Secure Boot OFF, TPM OFF, UEFI ON.
*   **Encryption:** Full-disk LUKS mandatory (Omarchy default).
*   **Filesystem:** Btrfs recommended (enables snapshot rollbacks).
*   **Install target:** `/dev/nvme0n1` (931.5G) — NOT `nvme1n1`, NOT `sda`.

---

## SECTION 2: SESSION OVERVIEW

### 2.1 Purpose

Recover all critical data from a broken Arch Linux + CachyOS Kernel system on `/dev/nvme0n1p3` (921.2G, ext4, no encryption), then install Omarchy v3.4.2 on `/dev/nvme0n1`. Backup is managed by recovery scripts from `kyonax/pc-migration-scripts` that perform incremental, file-level comparison between the broken system and Storage, then transfer with verbose progress.

### 2.2 Scope

| Phase                            | Type       | Summary                                                              | Status          |
|----------------------------------|------------|----------------------------------------------------------------------|-----------------|
| Phase 1: Data Recovery           | Recovery   | All user data backed up to SDA3 Storage via scripts                  | **DONE**        |
| pc-migration-scripts repo        | Tooling    | GitHub repo with checker.sh, mover.sh, config.sh                     | **DONE**        |
| Phase 2a: Flash Omarchy ISO      | Install    | Flash ISO to USB, boot from it (blockers resolved, dd pending)       | **IN PROGRESS** |
| Phase 2b: Install Omarchy        | Install    | Run Omarchy installer on target disk                                 | NOT STARTED     |
| Phase 3: Post-Install Restore    | Config     | Restore dotfiles, SSH/GPG keys, Doom Emacs, .brain.d, dev tools     | NOT STARTED     |

### 2.3 Key Decisions (Session-Wide)

1.  **(2026-03-20)** Omarchy v3.4.2 chosen as replacement OS — opinionated Arch + Hyprland distro by DHH/37signals.
2.  **(2026-03-20)** Recovery-first approach — NVME strictly read-only until all data is safe on SDA.
3.  **(2026-03-20)** Plan documented as org-roam node under the Arch Linux index — not inside this context block.
4.  **(2026-03-20)** Disk layout confirmed via `lsblk` and `lsblk -f`: NVME root is plain ext4 (no LUKS, no LVM, no separate home). SDA3 is NTFS with existing data.
5.  **(2026-03-20)** UUIDs extracted from `lsblk -f` screenshots and added to disk map in roam node — enables safe partition verification before mount operations.
6.  **(2026-03-20)** `/dev/nvme1n1` (238.5G, NTFS, "Apps"/"Disk") identified as Windows disk — off-limits.
7.  **(2026-03-20)** SDA3 free space confirmed: 754G available — plenty of room for full backup.
8.  **(2026-03-20)** Replaced tar-only backup strategy with script-based incremental approach — `checker.sh` compares file-by-file (source vs Storage), `mover.sh` transfers only what's needed with progress. Hybrid NTFS strategy: direct copies + tar for `.ssh`/`.gnupg`.
9.  **(2026-03-20)** Recovery scripts live in GitHub repo `kyonax/pc-migration-scripts` — cloned to `/tmp/recovery-scripts` on the live USB.
10. **(2026-03-20)** `gh` CLI installed and authenticated on kyo-labs.
11. **(2026-03-26)** Phase 1 COMPLETE — all user data backed up to SDA3 Storage disk.
12. **(2026-03-26)** Flashing Omarchy ISO from kyo-labs (ThinkPad T460s, Arch + CachyOS kernel). This machine's `usb-storage` module was MISSING from kernel `6.19.3-2-cachyos` — resolved after kernel update to `6.19.9-1-cachyos`.
13. **(2026-03-26)** Omarchy ISO re-downloaded successfully — `~/Downloads/omarchy-3.4.2-2.iso` is 6.8G (was 0 bytes from failed download).
14. **(2026-03-26)** Both Phase 2a blockers resolved: USB storage working (kernel `6.19.9-1-cachyos`), ISO fully downloaded (6.8G). Only the `dd` flash remains.
15. **(2026-03-26)** USB drive is 57.8G (`/dev/sda` with partition `sda1`) — different/larger than the original 7.5G Verbatim from the live USB session.

### 2.4 Pending Work

- [x] Boot live USB and identify disks
- [x] Confirm disk layout: encryption, LVM, home partition, SDA state
- [x] Check free space on `/dev/sda3` — **754G free** (932G total, 178G used, 20%)
- [x] Confirm network connectivity — **working**
- [x] Mount SDA3 at `/mnt/recovery`
- [x] Add UUIDs to disk map in roam node
- [x] Redesign Phase 1 with script-based incremental backup approach
- [x] Add RECOVERY SCRIPTS section to roam node with full specs for checker.sh and mover.sh
- [x] Update Phase 3 in roam node to reference new backup structure (direct copies + tar)
- [x] Install `gh` CLI — **done** (2026-03-21)
- [x] Implement scripts — **done** (2026-03-21)
- [x] Execute Phase 1 data recovery — **done** (2026-03-26, all user data backed up to SDA3)
- [x] Fix `usb-storage` on kyo-labs — **done** (2026-03-26, kernel updated to `6.19.9-1-cachyos`)
- [x] Re-download Omarchy ISO — **done** (2026-03-26, `~/Downloads/omarchy-3.4.2-2.iso` is 6.8G)
- [ ] Flash Omarchy ISO to USB with `dd`
- [ ] Boot broken Arch machine from Omarchy USB and install
- [ ] Execute Phase 3 (post-install restoration)

### 2.5 System Context (Confirmed)

| Detail              | Value                                                         |
|---------------------|---------------------------------------------------------------|
| Broken OS           | Arch Linux + CachyOS Kernel                                   |
| Failure cause       | System upgrade broke the OS — unbootable                      |
| Root partition      | `/dev/nvme0n1p3` (921.2G, ext4, UUID `e727f38a-c63b-411f-8ccf-0f36f189b2c4`) |
| EFI partition       | `/dev/nvme0n1p1` (1.3G, vfat, UUID `4768-4024`)              |
| Swap                | `/dev/nvme0n1p2` (9G, UUID `9f9e00c4-0ae0-4225-b87f-bfd02d434f75`) |
| Home                | No separate partition — inside root                           |
| Recovery disk       | `/dev/sda3` (932G, NTFS, label "Storage", UUID `D8F24019F23FFA78`, 754G free) |
| Windows disk        | `/dev/nvme1n1` (238.5G, NTFS, "Apps"/"Disk" — DO NOT TOUCH)  |
| Live USB (recovery) | `/dev/sdb` (7.5G, Arch ISO 2026-01) — used for Phase 1      |
| Flash USB (kyo-labs)| `/dev/sda` (57.8G) — target for Omarchy ISO flash            |
| kyo-labs machine    | ThinkPad T460s, Arch + CachyOS 6.19.9-1-cachyos, i7-6600U, nvme0n1 (476.9G, cryptroot+crypthome btrfs) |
| Target OS           | Omarchy v3.4.2 (Arch + Hyprland)                             |
| Install target      | `/dev/nvme0n1` (same broken disk, will be wiped)             |
| User                | Kyonax (Cristian D. Moreno)                                   |
| Editor              | Doom Emacs (Omarchy defaults to Neovim — will add alongside) |

---

## SECTION 3: IMPLEMENTATIONS

### 3.1 Session Setup & Plan Creation

**Created:** 2026-03-20 | **Last updated:** 2026-03-20
**Status:** DONE

*   Researched Omarchy v3.4.2 — full feature set, requirements, installation process, comparison with CachyOS/vanilla Arch.
*   Created detailed step-by-step plan as org-roam node: `~/.brain.d/roam-nodes/knowledge/2026-03-20-omarchy_installation.org`.
*   Linked to Arch Linux index node: `~/.brain.d/roam-nodes/2026-03-20-arch_linux.org`.
*   Plan covers 3 phases: Recovery (8 sub-steps with validation gates), Installation (3 sub-steps), Post-Install Restoration (4 sub-steps).

### 3.2 Disk Discovery & UUID Extraction

**Created:** 2026-03-20 | **Last updated:** 2026-03-20
**Status:** DONE

*   User booted Arch Linux live USB and ran `lsblk` + `lsblk -f`.
*   Confirmed 4 block devices: `sda` (SATA, 931.5G), `sdb` (USB, 7.5G), `nvme0n1` (NVME, 931.5G), `nvme1n1` (NVME, 238.5G).
*   Key findings: NVME root is plain ext4 (no LUKS, no LVM, no separate home). SDA3 is NTFS with existing data. Second NVME is Windows.
*   Three screenshots verified as source of truth: `screenshot_lsblk_brokenarch.jpeg`, `lsblk_with_f_flag.jpeg`, `df_sda3.jpeg` (all in `~/Downloads/`).
*   UUIDs extracted from `lsblk -f` screenshot and added to disk map table in roam node (FSVER, LABEL, UUID columns).
*   UUID-based verification commands added to mount steps in the plan.

### 3.3 Phase 1 Redesign: Script-Based Recovery

**Created:** 2026-03-20 | **Last updated:** 2026-03-20
**Status:** DONE (plan redesigned, scripts NOT YET IMPLEMENTED)

*   Replaced old tar-based Phase 1 steps (1d Copy Critical, 1e Copy Secondary, 1f Validate) with script-based incremental approach.
*   New Phase 1 flow: 1d Setup Scripts → 1e Run Checker → 1f Review Report → 1g Run Mover → 1h Validate.
*   Added RECOVERY SCRIPTS section to roam node with full specs for both scripts.
*   Updated NTFS limitation note to explain hybrid approach (direct copies + tar for `.ssh`/`.gnupg`).
*   Updated Safety Rules section with new checker/mover workflow rules.
*   Updated Phase 3 restoration to reference direct file copies and tar archives.

#### Key Decisions

| Decision | Date | Rationale |
|---|---|---|
| Script-based incremental backup over tar-all | 2026-03-20 | SDA already has 178G data — checker compares file-by-file so only new/changed data transfers |
| Hybrid NTFS strategy (direct + tar) | 2026-03-20 | Direct copies are browsable on any OS; tar preserves permissions for SSH/GPG keys specifically |
| JSON manifest as interface between scripts | 2026-03-20 | Checker produces `backup_manifest.json`, mover consumes it — decouples analysis from transfer |
| Three priority tiers for directories | 2026-03-20 | CRITICAL (SSH, GPG, .brain.d, git repos), IMPORTANT (.config, Documents), SYSTEM (/etc, pacman) |
| Repo name: `pc-migration-scripts` | 2026-03-20 | User's existing repo on GitHub at `kyonax/pc-migration-scripts` |

### 3.4 pc-migration-scripts Repo

**Created:** 2026-03-20 | **Last updated:** 2026-03-21
**Status:** IMPLEMENTED — pushed to `main` on GitHub (commit `4643e4c`)

Repo: `github.com/kyonax/pc-migration-scripts` | Local: `~/Documents/github-kyonax/pc-migration-scripts/`

#### Data Flow

```
config.sh (sourced)          config.sh (sourced)
      │                            │
      ▼                            ▼
 checker.sh ──────────────► mover.sh
      │    backup_manifest.json    │
      │    backup_report.md        │
      │                            ├──► direct file copies on Storage
      │                            ├──► ssh-keys.tar.gz, gnupg-keys.tar.gz
      │                            ├──► ntfs-incompatible.tar.gz
      │                            ├──► system/ (package lists, crontabs)
      │                            └──► mover_log.txt
      └──► Re-run after mover to validate (NEEDS_BACKUP table should be empty)
```

#### config.sh (166 lines) — Shared Configuration
Sourced (not executed) by both scripts. Contains:
*   **Defaults:** `DEFAULT_SOURCE=/mnt`, `DEFAULT_TARGET=/mnt/recovery/backup-arch-2026-03-20`, `DEFAULT_USER=kyonax`
*   **SCAN_DIRS array:** 16 entries in `priority|path|description` format. `~` prefix expands to `$SOURCE/home/$USER/`. Three tiers: CRITICAL (5 dirs), IMPORTANT (6 dirs), SYSTEM (4 dirs)
*   **EXCLUDE_PATTERNS array:** 21 patterns (`.cache`, `node_modules`, `__pycache__`, `Trash`, `.venv`, `.cargo/registry`, etc.) — substring match on relative paths
*   **NTFS_BAD_CHARS:** regex `[:<>?*"|\\]` for detecting NTFS-incompatible filenames
*   **PERMISSION_CRITICAL_DIRS:** `[".ssh", ".gnupg"]` — get tar archives in addition to direct copies
*   **LARGE_FILE_THRESHOLD:** 1 GiB — files above this flagged in report
*   **Helper functions:** `human_size()`, `resolve_path()`, `is_excluded()`, `has_ntfs_bad_chars()`, `log_msg()`

#### checker.sh (396 lines) — Backup Analysis & Comparison
Walks every file in scan dirs, classifies each, builds manifest + report.

**Flow:** Parse args → validate source → iterate `SCAN_DIRS` (with dedup check for parent-child overlap) → `find -print0` per directory → `scan_file()` per entry → classify (SYMLINK → EXCLUDED → ALREADY_SAVED/PARTIAL/NEEDS_BACKUP) → generate JSON manifest → generate markdown report → print summary

**Key design:**
*   `find -print0` + `read -d ''` for safe filenames with spaces/newlines
*   Size comparison at **individual file level** (not dir totals) — catches partial backups
*   Dedup: `scanned_paths` array prevents scanning `~/Documents/github-kyonax` twice (once as priority 1, again inside `~/Documents` as priority 2)
*   JSON built as bash string array (no `jq` dependency during scan loop)
*   Report tables capped at 100 rows for readability
*   Progress every 500 files

#### mover.sh (301 lines) — Backup Transfer with Progress
Reads manifest, copies files with real-time progress.

**Flow:** Parse args → validate manifest + source + `jq` → pre-flight (count entries, sum bytes, check disk space) → transfer loop (`jq -r ... | @tsv` → `cp -a` per file) → NTFS-incompatible bundle → permission-critical tar archives → system data (package lists, crontabs, systemd) → summary

**Key design:**
*   `jq` parses manifest; entries filtered to `NEEDS_BACKUP` + `PARTIAL` only
*   Progress: `\r` carriage return for in-place update `[N/total] [%] [bytes/total] Copying path (size)`
*   Disk space pre-check prevents mid-transfer failures
*   Errors non-fatal: logs failure, continues to next file, reports count at end
*   NTFS-incompatible files queued and bundled into `ntfs-incompatible.tar.gz` via `tar -T`
*   Permission-critical dirs (`.ssh`, `.gnupg`) get `tar -czf` archives **after** direct copies
*   System data (package lists via `arch-chroot`/fallback, crontabs, systemd services) saved last to `system/` subdir
*   Resume: implicit via re-running checker (marks already-transferred as `ALREADY_SAVED`)

Full specs and edge case handling documented in the RECOVERY SCRIPTS section of the roam node.

### 3.5 Phase 2: Omarchy Installation

**Created:** 2026-03-20 | **Last updated:** 2026-03-20
**Status:** NOT STARTED — blocked by Phase 1

Install Omarchy on `/dev/nvme0n1`. Key considerations:
*   Installer will wipe `/dev/nvme0n1` — must select correct disk (NOT `nvme1n1` or `sda`).
*   Omarchy defaults to Neovim (LazyVim) — user needs Doom Emacs alongside.
*   Omarchy uses Zsh + Starship — may want to adapt existing shell config.
*   Omarchy uses Hyprland (Wayland) — different keybindings from previous WM.

### 3.6 Phase 3: Post-Install Restoration

**Created:** 2026-03-20 | **Last updated:** 2026-03-20
**Status:** NOT STARTED — blocked by Phase 2

Restore from backup on `/dev/sda3`:
*   SSH/GPG keys restored from **tar archives** (preserves Linux permissions).
*   All other data restored from **direct file copies** (browsable on Storage).
*   Dotfiles cloned fresh from git (preferred) or copied from backup.
*   Doom Emacs installed alongside Omarchy's default Neovim.
*   Do NOT overwrite Omarchy-managed configs (Hyprland, Waybar, Ghostty, Walker).

---

## SECTION 4: FILE INDEX

### Org-Roam Nodes

| File                                                                  | Role                                        |
|-----------------------------------------------------------------------|---------------------------------------------|
| `~/.brain.d/roam-nodes/2026-03-20-arch_linux.org`                    | Index node — Arch Linux projects             |
| `~/.brain.d/roam-nodes/knowledge/2026-03-20-omarchy_installation.org` | Full plan — recovery + install steps (697 lines) |

### Recovery Scripts (to be implemented)

| File                                    | Role                                      |
|-----------------------------------------|-------------------------------------------|
| `pc-migration-scripts/checker.sh`       | Backup analysis & comparison script        |
| `pc-migration-scripts/mover.sh`         | Backup transfer with progress script       |
| `pc-migration-scripts/config.sh`        | Shared configuration (paths, excludes)     |
| `pc-migration-scripts/backup_report.md` | Generated report (not committed)           |
| `pc-migration-scripts/backup_manifest.json` | Generated manifest (not committed)     |

### Disk Map (Confirmed with UUIDs)

| Device          | Size    | FSTYPE | Label   | UUID                                    | Mount Point        | Role                         |
|-----------------|---------|--------|---------|-----------------------------------------|--------------------|------------------------------|
| `/dev/nvme0n1p3`| 921.2G  | ext4   |         | `e727f38a-c63b-411f-8ccf-0f36f189b2c4` | `/mnt` (read-only) | Broken Arch root             |
| `/dev/nvme0n1p1`| 1.3G    | vfat   |         | `4768-4024`                             |                     | EFI                          |
| `/dev/nvme0n1p2`| 9G      | swap   |         | `9f9e00c4-0ae0-4225-b87f-bfd02d434f75` |                     | Swap                         |
| `/dev/sda3`     | 931.5G  | ntfs   | Storage | `D8F24019F23FFA78`                      | `/mnt/recovery`    | Recovery target (754G free)  |
| `/dev/nvme1n1`  | 238.5G  | ntfs   | Apps    | `B62C0D342C0CF0E3`                      |                     | Windows — DO NOT TOUCH       |
| `/dev/sdb`      | 7.5G    | iso9660|         |                                         |                     | Live USB                     |

### Backup Structure (on `/dev/sda3`)

| Path                                                         | Contents                              |
|--------------------------------------------------------------|---------------------------------------|
| `/mnt/recovery/backup-arch-2026-03-20/home/kyonax/`         | Direct file copies (browsable)        |
| `/mnt/recovery/backup-arch-2026-03-20/ssh-keys.tar.gz`      | SSH keys with preserved permissions   |
| `/mnt/recovery/backup-arch-2026-03-20/gnupg-keys.tar.gz`    | GPG keys with preserved permissions   |
| `/mnt/recovery/backup-arch-2026-03-20/system/`              | Package lists, crontabs, systemd      |
| `/mnt/recovery/backup-arch-2026-03-20/ntfs-incompatible.tar.gz` | Files with NTFS-incompatible names |

### Source Screenshots (in `~/Downloads/`)

| File                                | Contents                        |
|-------------------------------------|---------------------------------|
| `screenshot_lsblk_brokenarch.jpeg`  | `lsblk` output — disk layout   |
| `lsblk_with_f_flag.jpeg`           | `lsblk -f` output — UUIDs/types |
| `df_sda3.jpeg`                      | `df -h` — SDA3 free space      |

---

## SECTION 5: LAST INTERACTION (SHORT-TERM MEMORY)

> **Start here when resuming.**

### What was done last
*   Confirmed USB storage working on kyo-labs after kernel update to `6.19.9-1-cachyos`
*   Verified USB drive detected as `/dev/sda` (57.8G) via `lsblk`
*   Verified Omarchy ISO fully downloaded: `~/Downloads/omarchy-3.4.2-2.iso` (6.8G)
*   System confirmed via `fastfetch`: Arch Linux x86_64, ThinkPad T460s, kernel 6.19.9-1-cachyos
*   `dd` flash command prepared but requires interactive `sudo` — user must run manually

### Phase 2 Instructions — Flash & Boot Omarchy

**Step 1: Flash ISO to USB** (run manually — needs interactive sudo)
```bash
sudo dd if=~/Downloads/omarchy-3.4.2-2.iso of=/dev/sda bs=4M status=progress oflag=sync
sync
```
Target is `/dev/sda` (57.8G USB). **NEVER use the nvme device.**

**Step 2: Boot broken Arch machine from Omarchy USB**
1. Plug flashed USB into the broken Arch machine (931.5G NVME + 238.5G Windows NVME + 931.5G SATA Storage)
2. Enter BIOS: press DEL or F2 on boot
3. Verify BIOS settings:
   - Secure Boot: **OFF**
   - TPM: **OFF**
   - UEFI: **ON**
4. Set USB as first boot device, save and reboot
5. Omarchy installer starts automatically

**Step 3: Install Omarchy**
1. Select install target: `/dev/nvme0n1` (931.5G) — the broken Arch disk
   - **DO NOT** select `/dev/nvme1n1` (238.5G, Windows)
   - **DO NOT** select `/dev/sda` (931.5G, Storage with your backup)
2. Set a LUKS encryption passphrase when prompted
3. Wait for installation (5-30 min, needs internet)
4. Reboot when complete — enter LUKS passphrase at boot

**Step 4: After Omarchy boots — proceed to Phase 3**
Phase 3 restores data from the SDA3 backup. See roam node for full instructions.

### Where to resume
If **dd flash fails or USB not bootable**: Verify with `lsblk` after flash, try re-flashing. If kyo-labs sudo doesn't work in Claude, run the `dd` command directly in a terminal.
If **Omarchy is installed**: Start Phase 3 — mount SDA3, restore SSH/GPG keys from tar, copy data back.
If the user asks for a **new task**: Check Section 2.4.

<!-- DESCRIPTION AND USER CONTEXT END -->



<!-- INIT OF THE USER PROMPT END -->
