# History

### Initial setup (a112597–7419e8d)
Background: Manage dotfiles on GitHub
Changes: Added `.bashrc`, `.vimrc`, `.editorconfig`, `.gitconfig`

### Zsh migration (cdcd088–f2e7309)
Background: Switch to Zsh as primary shell
Changes: `.zshrc` + Zinit, `.profile_common` separation, git-prompt migration

### Git config relocation (ce8aa0a–f16c281)
Background: Move `.gitconfig` to XDG-compliant location
Changes: Migrated to `.config/git/config` + `ignore`, separated `config.local`

### Emacs enhancement (f835f5a–d45bdd8)
Background: Modularize Emacs config
Changes: init-loader pattern, use-package, LSP, Ivy/Counsel

### OS detection refactor (acb488b–e6aa321)
Background: Unify OS detection logic
Changes: Created `bin/detectos.sh`, rewrote `.bash_profile` to use `case` statements

### Apple Silicon support (1ff488d–d72a3b5)
Background: Support M1/M2 Macs
Changes: Added `$ISM1`, updated Homebrew/fnm paths for Apple Silicon

### Repository merge (b436d88–7abe347)
Background: Merge install repo into dotfiles
Changes: Subtree merge into `install/linux/`, unified `install.sh` entry point. The separate [nirecom/install](https://github.com/nirecom/install) repository is no longer needed

### Windows support (8281602–e51b61b)
Background: Use dotfiles on Windows
Changes: `install/win/dotfileslink.ps1`, Developer Mode check, PowerShell 5 compatibility

### #1: `dotfileslink.sh` aborts with `set -e` (b3ea03f)
Cause: Some commands return non-zero exit
Fix: Fixed `set -e` compatibility

### #2: Symlink creation fails on PowerShell 5 (06f6640, be85569)
Cause: PS5 has a different Developer Mode check API
Fix: Switched to direct registry check

### Starship introduction (f43b6d9, 25b7147)
Background: Add Starship prompt
Changes: Linux Zsh + Windows PowerShell configurations

### Claude Code management (664d7ce–2e96bbf)
Background: Centralize Claude Code config
Changes: `claude-global/` directory, install integration, security deny rules

### PowerShell curl issue (60c48fb, eb9d164)
Background: curl doesn't work in PowerShell
Changes: Added `curl.exe` rule and single-quote rule to CLAUDE.md

### Claude Code commands (8418c36)
Background: Manage Claude Code commands in dotfiles
Changes: Added global commands directory symlink support

### QNAP vim plugins (c3754ce)
Background: vim errors on startup in QNAP
Changes: Added pathogen + solarized plugin installation to `dotfileslink.sh`

### QNAP support (19f488d–f2d62a5)
Background: Use dotfiles on QNAP NAS
Changes: OS detection added, minimal symlinks, autorun.sh, sh→bash auto-switch

### #3: Claude Code installed on QNAP (89e9e98)
Cause: `install.sh` had no QNAP branch
Fix: Added early exit for QNAP

### #4: `arch: command not found` on QNAP (ed56971)
Cause: `arch` is macOS-only; not in BusyBox
Fix: Replaced with `uname -m`

### #5: No colored prompt on QNAP SSH (7e780cc)
Cause: Default shell `/bin/sh` doesn't read `.bash_profile`
Fix: `.profile_qnap` runs `exec bash -l`

### #6: `.profile_qnap` exec fails (d38ee0a)
Cause: Bash path hardcoded (`/opt/bin/bash`)
Fix: Changed to `command -v bash` for dynamic resolution

### #7: `git: command not found` on QNAP (584e6dc)
Cause: Entware `/opt/bin` not in PATH
Fix: Added Entware PATH to `.profile_common`

### #8: Cursor position misaligned on QNAP (f2d62a5)
Cause: Entware bash doesn't handle `\[...\]` correctly
Fix: Used `\001`/`\002` (interim) → removed in #10 after root cause fix

### QNAP Entware fix (7533ba7–401097e)
Background: git disappears after reboot
Changes: Entware QPKG activation, autorun.sh filename fix, flash auto-deployment

### QNAP prompt wrapping fix (107b3a6–1ae045e)
Background: Long input wraps incorrectly in QNAP bash
Changes: terminfo auto-install, `TERMINFO` env var, PS1 unification (`\001`/`\002` → `\[`/`\]`), TERM fallback

### #9: git disappears after QNAP reboot (7533ba7, 401097e)
Cause: Entware QPKG disabled (`Enable != TRUE`), startup script name wrong
Fix: `setcfg Enable TRUE` + fixed to `Entware.sh start`

### #10: Long input overwrites prompt on QNAP bash (107b3a6–1ae045e)
Cause: terminfo not installed + `TERMINFO` not set → readline can't detect auto-margin
Fix: Auto-install terminfo, set `TERMINFO=/opt/share/terminfo`, unified PS1 to standard `\[`/`\]`

### #11: `E117: Unknown function: pathogen#infect` on QNAP vim (c3754ce)
Cause: `.vimrc` symlinked but pathogen/plugins not installed
Fix: Added vim plugin setup to `dotfileslink.sh`

### Claude Code skill management (9ec0c0b, 359d929, e442685)
Background: Manage Claude Code skills (commands) in dotfiles, improve symlinks
Changes: Changed commands symlink from per-file to directory-level, added langchain/instruction update skills

### Claude Code security hardening (6e9eeb1, 18b0fd7)
Background: Review settings.json allow/deny rules. Cross-reviewed with ChatGPT: (1) adopt `git -C` as primary method (avoids compound commands), (2) `cd && git` fallback limited to status/diff/log minimum set, (3) wildcard prefix on deny rules (defense in depth), (4) curl/wget pipe deny acknowledged as glob-limited (future PreToolUse hook for strict enforcement)
Changes: Added `git -C` allow rules, `cd &&` fallback allow (minimal set), strengthened deny rule `*` prefixes, added `git -C` preference to CLAUDE.md, added local path rule to private information

### Stale symlink detection (Windows) (0c83762)
Background: dotfiles moved from `~/git/dotfiles` to `~/dotfiles`; old symlinks remained and caused broken configs
Changes: `dotfileslink.ps1` now compares symlink target against `$DotfilesDir` and relinks automatically if mismatched

### #12: No colored prompt on Windows PowerShell (one PC only) (0c83762)
Cause: (1) Starship not installed — profile silently skips init. (2) Starship config symlinks pointed to old `~/git/dotfiles` location after dotfiles were moved
Fix: Installed Starship via `install/win/starship.ps1`; re-ran `dotfileslink.ps1` as admin to fix stale symlinks

### Claude Code deny rule false-positive fix (c8095dc)
Background: `git add` auto-denied without prompt — deny rule `Bash(*dd *)` matched "add" substring. Cross-reviewed with ChatGPT: (1) root cause is deny `*dd *` not missing allow rules, (2) replaced with 4 specific patterns (`dd *`, `*&& dd *`, `*; dd *`, `*
Changes: dd *`), (3) HEREDOC commit format confirmed working with existing `git commit -m *` allow rule, (4) settings changes require session restart to take effect

### #13: `git add CLAUDE.md` auto-denied by Claude Code (c8095dc)
Cause: Deny rule `Bash(*dd *)` glob-matched the "dd" in "add", overriding the allow rule `Bash(git add *)`. Deny rules take precedence regardless of order
Fix: Replaced `*dd *` with 4 patterns: `dd *`, `*&& dd *`, `*; dd *`, `*| dd *`

### AutoHotkey integration (3ca2dcf, 1c905ec)
Background: Force Japanese keyboard layout via AHK when Windows UI is English + Japanese preferred. Added `config/win/autohotkey/force-japanese-layout.ahk`, `install/win/autohotkey.ps1` (language detection + winget install + startup shortcut), `install/win/install-obsolete.ps1` (auto-cleanup of old OneDrive AHK files and startup shortcuts)
Changes: AHK script moved from OneDrive to dotfiles repo. Install script detects English UI + Japanese preferred language (strict), installs AHK v2 via winget, creates startup shortcut. Obsolete script auto-detects old AHK shortcuts by TargetPath pattern (excludes dotfiles repo paths for idempotency) and old OneDrive AHK files by glob. `install-obsolete.ps1` runs always (not only `-Full`), matching Linux `install.sh` pattern. Per-user install path fallback added (1a15c1f)

### Starship install guard (c7f02ea, 1766674)
Background: `install.ps1 -Full` tried to reinstall Starship even when already installed — `Get-Command` missed binaries not yet in PATH
Changes: Switched to `winget list --id Starship.Starship` for definitive install detection, matching AutoHotkey's `winget list` pattern

### #14: `.config/git/config` always dirty after shell startup (dd787a4)
Cause: `profile.ps1` ran `git config --global core.sshCommand` on every PowerShell startup, appending `[core] sshCommand` directly to tracked `.config/git/config`. The setting was already provided via `config.local` (generated by `dotfileslink.ps1`) and included via `[include] path = config.local`
Fix: Removed redundant sshCommand block from `profile.ps1`; setting remains via `config.local` include

### fnm install script (267d793, d2dadcc)
Background: Add fnm to Windows installer
Changes: `install/win/fnm.ps1` with winget install, `profile.ps1` defensive init with try/catch for SAC

### Starship git timeout fix (4024b77)
Background: Starship showed `git.exe timed out` warning on Windows PowerShell startup — NTFS overhead makes `git status` exceed default 500ms timeout
Changes: (1) Enabled Git built-in FSMonitor (`core.fsmonitor = true`) and `core.untrackedCache = true` in Windows `config.local` for root-cause fix, (2) increased Starship `command_timeout` to 1000ms in `starship-powershell.toml` as safety net for FSMonitor cold start, (3) split `git pull` into `git fetch` (3s timeout) + `git merge --ff-only` to avoid corrupting working tree on kill

### uv install scripts (85f3d7f)
Background: Add uv (Python package manager) to dotfiles
Changes: Added `install/win/uv.ps1` (PowerShell installer) and `install/linux/uv.sh` (curl installer), integrated into `install.ps1 -Full`. uv installs to `~/.local/bin` on all platforms (Win/Mac/Linux) — already in PATH via `.profile_common` and `profile.ps1`. No shell init (unlike fnm) needed

### Claude Code PreToolUse hook investigation ()
Background: Investigated using PreToolUse hooks to force diff display before Edit (CLAUDE.md instruction sometimes ignored). Created node-based hook script outputting diff via stderr and `additionalContext`. **Result: neither method is visible to users in VS Code extension UI.** VS Code already shows a built-in diff in the Edit approval dialog, making the hook redundant. Hook removed. Findings: (1) PreToolUse hook stderr not shown in VS Code UI, (2) `additionalContext` not shown in VS Code UI, (3) `/dev/stdin` does not exist on Windows — use `fs.readSync(0, buf)` for cross-platform stdin, (4) VS Code Claude Code provides diff review via standard Edit approval dialog when Edit is not auto-allowed
Changes: No commit (reverted)

### Windows notification muting (45ca237)
Background: Mute toast/system notification sounds
Changes: `sounds.ps1` sets registry to empty for 3 sound events; runs always (not only `-Full`)

### Claude Code git write permissions (e94a2b5)
Background: `git push` failed with permission denied — only read-only git commands were in allow list. Cross-reviewed with ChatGPT: (1) allow `git commit -m *` only (not broad `git commit *` which passes `--amend`/`--no-verify`), (2) allow `git push` / `git push origin *` only (not broad `git push *` which passes `--force-with-lease`/`--mirror`/`--delete`), (3) deny side strengthened with `--force-with-lease`, `--mirror`, `--delete`, `--amend`, `--no-verify`
Changes: Added git add/commit/push allow rules (minimal), added 7 deny rules for dangerous git options

### Claude Code permissions refinement (99e2d4f, d9d691b, 99858f4, 98ae129)
Background: Expand Claude Code allow rules for daily use
Changes: Allow `chmod +x`, piped `ls`, `find` (with deny for dangerous options), commit message blocklist scan

### claude-code → claude-global rename (597bf88–f83e98c)
Background: Rename directory to avoid project-level conflict
Changes: Renamed `claude-code/` to `claude-global/`, migration logic in install scripts, handles empty dir left by git, `-Recurse` fix for non-empty directory

### uv install PATH fix (d82c026)
Background: `uv --version` fails on fresh install
Changes: Refresh `$env:Path` (Win) / `$PATH` (Linux) after uv install so version check succeeds

### #15: `uv --version` fails after fresh install (d82c026)
Cause: uv installer adds `~/.local/bin` to system PATH but current shell session is not refreshed
Fix: Add `~/.local/bin` to session PATH before calling `uv --version`

### Profile hardening (Windows) (a64aca9)
Background: SSH key loading and migration symlink fail on some machines
Changes: SSH key loading now globs `$HOME\.ssh\id_*` instead of hardcoded `id_ed25519`/`id_rsa`. Migration symlink (claude-code → claude-global) checks Developer Mode / admin before creating. PS5 `New-Item` wrapped in try/catch for permission errors. Added Pester tests

### Claude Code rules reorganization ()
Background: Split monolithic `claude-global/CLAUDE.md` into `rules/` directory (6 rule files) + docs lifecycle rule. Unified langchain-specific commands into generic `/update-docs`, `/start-task`, `/complete-task`. Deleted 7 old langchain-specific commands. Added `rules/` symlink to install scripts (Linux + Windows). Added `tests/test-claude-rules.sh` verification script
Changes: (pending)

### Claude Code commands → skills migration ()
Background: Migrated `commands/*.md` to `skills/*/SKILL.md` with YAML frontmatter (description, disable-model-invocation, argument-hint). start-task/complete-task are manual-only; update-docs/update-instruction allow auto-invocation. Updated dotfileslink (Win/Linux), CLAUDE.md, docs
Changes: (pending)

### #16: `New-Item -ItemType SymbolicLink` error on PS5 startup (a64aca9)
Cause: `profile.ps1` migration block called `New-Item` without permission check. PS5 requires admin for symlinks even with Developer Mode enabled (unlike pwsh which supports `SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE`)
Fix: Added Developer Mode / admin pre-check (a64aca9), then wrapped `New-Item` in try/catch with `-ErrorAction Stop` for defense in depth

### Windows symlink auto-repair (08141cf, 9ab6b54)
Background: `install.ps1` warns "Exists (not a symlink)" for 4 files — Windows atomic save (editors, Claude Code/Node.js) silently replaces symlinks with regular files
Changes: `dotfileslink.ps1`: removed file-skip logic, unified file/directory backup-and-relink. `profile.ps1`: added startup detection of broken symlinks (~20ms) with auto-repair via `dotfileslink.ps1`. Extracted test rules from `workflow.md` into dedicated `rules/test.md` with edge case category, added test-first reminder to global `CLAUDE.md`. Fixed Pester 5 `-Skip` scoping bug (script-level detection needed for discovery-time evaluation)

### Docs enforcement via hook ((pending))
Background: Wanted docs updated before every commit — rules/ is best-effort, not enforced
Changes: Created `check-docs-updated.js` PreToolUse hook (blocks `git commit` when source changes staged without `docs/`), consolidated `rules/docs-lifecycle.md` into `/update-docs` skill (self-contained), deleted rule file. Enforcement hierarchy: hook (hard block) → skill (procedure) → rule (removed as redundant)

### Docs convention rule ((pending))
Background: Document structure rules were only in ai-specs history.md (mixed with migration history) — not auto-loaded by Claude Code
Changes: Created `rules/docs-convention.md` (auto-loaded by Claude Code), moved file role/cascade/content rules from SKILL.md. SKILL.md now references the rule file and retains only procedural steps

### Claude Code PermissionRequest hook (feature/permission-hook2) (41f092c, (pending))
Background: Fixed settings.json to nested format, limited PreToolUse hook to Bash only, added MSYS/WSL path detection to private info scanner. Investigated VSCode "Ask before edits" Bash behavior: (1) `Edit
Changes: Write` PreToolUse hook presence does not affect Bash Ask behavior, (2) discovered `permissions.ask` setting — adding `Bash(git commit *)` triggers Ask dialog in "Ask before edits" mode, (3) "Edit automatically" mode ignores `permissions.ask` (auto-allows all). Achieved git commit confirmation control via `permissions.ask` — PermissionRequest hook unnecessary

### Private info leak prevention ()
Background: Automated scanning for private information (RFC 1918 IP, email, MAC, absolute paths) at git commit time and Claude Code edit time. Private repo whitelist skips scanning for private repos (safe default: unregistered repos are scanned). Fixed existing leaks: `.zshrc` SDKMAN hardcoded paths → `$HOME`, `.config/git/config` email → GitHub noreply. Key files: `bin/check-private-info.sh` (scanner), `bin/update-private-repos.sh` (whitelist generator), `hooks/pre-commit` (git hook), `claude-global/hooks/check-private-info.js` (PreToolUse hook), `.private-info-allowlist`, `.private-info-blocklist`
Changes: b5f60d8, d0cea99, 98ae129, 733ec29

### master → main cleanup (07045be)
Background: Remove temporary migration code after all PCs migrated
Changes: Deleted `BEGIN temporary: main branch upstream tracking fix` blocks from `.profile_common` and `install/win/profile.ps1`

### PowerToys Keyboard Manager (b453ee8)
Background: Manage PowerToys Keyboard Manager (Emacs-style shortcuts) via dotfiles
Changes: Added `install/win/powertoys.ps1` (winget install + Keyboard Manager config deploy), `config/win/powertoys/keyboard-manager/default.json` (Emacs keybindings: Ctrl+A/B/D/E/F/N/P → navigation, Alt+A/B/F → original Ctrl shortcuts). Uses file copy (not symlink) because PowerToys may not follow symlinks. Idempotent: skips if config identical, backs up `.bak` if different

### VS 2022 C++ dev tools (1982d8e)
Background: `install.ps1 -Develop` for llama.cpp compilation
Changes: VS 2022 + CMake needed to build llama-server (llama.cpp) from source. Added `install/win/vs-cpp.ps1` (VS 2022 Community + NativeDesktop workload via bootstrapper, vswhere idempotency, UAC auto-elevation with decline handling). Added to `-Develop`/`-Full` block in `install.ps1`. Pester tests added

### Dotenv access blocking (76b61ca)
Background: Prevent Claude Code from reading .env files
Changes: PreToolUse hook (`block-dotenv.js`) blocks Bash/Read/Grep/Glob access to `.env*` files. Deny rules provide first layer; hook catches bypass patterns (e.g., `bash -c "cat .env"`, `/bin/cat .env`). Git commit messages containing `.env` excluded from false positives via sanitization. 59 test cases

### claude-code migration cleanup ((pending))
Background: All PCs migrated; remove temporary code to prevent claude-code/claude-global name confusion
Changes: Deleted `BEGIN temporary: claude-code → claude-global` blocks from `.profile_common` and `profile.ps1`. Removed `.gitignore` (only held `claude-code` entry). Removed `claude-code/` from `check-test-updated.js` EXEMPT_DIRS. Deleted migration Pester test. `install-obsolete.sh/ps1` retained as safety net

### Markdown exempt from code detection in hooks ((pending))
Background: `git commit` in ai-specs (docs-only repo) was blocked by PreToolUse hooks (check-test-updated.js, check-docs-updated.js) because `.md` files under `projects/` were classified as "source code".
Changes: Added `/\.md$/i` to EXEMPT_FILES in both hooks. `hasDocChanges` in check-docs-updated.js now recognizes `.md` as documentation changes. Added tests: git -C variant, uppercase .MD, idempotency, stale review marker

### Unified Edit confirmation flow ((pending))
Background: Edit confirmation in VSCode was inconsistent (1–3 prompts). Three sources of confirmation: (1) chat-based diff presentation triggered by CLAUDE.md/memory "show diff" instructions, (2) VSCode Ask mode's built-in diff dialog, (3) permission confirmation dialog.
Root cause: When CLAUDE.md and memory contain duplicate or conflicting instructions, the LLM follows them only probabilistically, causing inconsistent behavior. "Always show diff" coexisting with "not needed in VSCode" resulted in intermittent diff presentation.
Alternatives considered:
  (a) Force diff display via PreToolUse hook — not possible because VSCode UI does not display hook stderr/additionalContext (verified previously)
  (b) Add `Edit` to `permissions.ask` for confirmation every time — causes double confirmation with the diff dialog
  (c) Remove prompt instructions and rely solely on VSCode's built-in diff dialog — mechanically guaranteed, always exactly one confirmation
  (d) Add `Edit(**)` to `allow` to skip the permission layer — additional mitigation for first-session double confirmation (found unnecessary)
Decision: Adopted (c). Kept a conditional rule to show diff in chat only when using the CLI.
Changes: Changed `workflow.md` File Edits rule to CLI-conditional, removed diff instructions from memory, added feedback memory, changed `docs-convention.md` history.md format from table to section style.
Follow-up (2026-03-23): (c) proved unreliable — VSCode built-in diff dialog sometimes shows diff, sometimes shows bare permission prompt with no diff. Tested Edit, Write (new file), Write (overwrite): all showed diff, so the inconsistency is not tool-type-dependent. Searched exhaustively: no mechanism in Claude Code permissions/hooks/extension settings to distinguish "diff shown" vs "diff not shown". Additionally tested PermissionRequest hook (unlike PreToolUse, does not bypass Ask dialog). Hook fires for Edit/Write and receives full tool_input (old_string/new_string for Edit, content for Write). However, stderr and additionalContext are not displayed in VSCode UI — same limitation as PreToolUse. Conclusion: no hook-based path exists to force diff display in VSCode; chat-based diff presentation via prompt instruction is the only reliable method. Reverted to "always show diff in chat" policy (double confirmation acceptable, zero confirmation not). Removed CLI-only condition from `workflow.md`, updated feedback memory

### Private repo detection: non-GitHub hosts skip scanning
Background: `gh api` returned 404 for non-GitHub remotes (e.g., GitLab, Bitbucket), causing them to be treated as public via fail-open, which triggered private info scan blocking.
Changes: Added `extractHost()` to `is-private-repo.js` to extract hostname from remote URL. Non-`github.com` hosts (GitLab, Bitbucket, GHE, etc.) are treated as private and skip scanning. Added the same hostname check to git hooks (pre-commit, commit-msg). Added 12 tests.

### #17: Test execution hangs indefinitely (9b2e88f)
Cause: Claude Code auto-generated Pester tests had bugs (WSL UTF-16 output encoding mismatch, Pester scope issue), causing background test execution to hang indefinitely.
Fix: Added mandatory timeout rule to `test.md` (`timeout 120` wrapper).

### #18: install.sh doesn't reach Rize / Claude Usage Widget steps
Cause: `exec $SHELL -l` in `source-highlight.sh` replaced the `install-base.sh` subprocess, preventing control from returning to `install.sh`. Additionally, `rize.sh` / `claude-usage-widget.sh` lacked execute permissions, DMG mount volume path parsing broke on volume names with spaces, and `.app` filenames were hardcoded instead of matching actual names.
Fix: Moved `exec $SHELL -l` from `source-highlight.sh` to end of `install.sh`. Added execute permissions. Dynamically detect `.app` in DMG via `find`. Removed macOS special-casing (`$OSDIST = "macos"` always running base) from `install.sh` to unify with `install.ps1` orthogonality (`--base`/`--full` flags required).

### #19: Session sync init deletes other machines' sessions (a8b8e5b)
Cause: `session-sync-init.ps1` / `session-sync-init.sh` used `git reset origin/main` (mixed reset) during initialization. Mixed reset moves HEAD and index to origin/main but leaves the working tree unchanged. The subsequent `git add .` overwrote the index from the working tree (which only contained local files), staging deletions for all remote-only files (other machines' sessions). When a second PC ran init, the primary and other machines' session files were deleted. Additionally, the Windows script had a secondary bug: `$ErrorActionPreference = "Stop"` combined with `try/catch` caused git stderr output to throw a terminating exception, silently skipping the fetch/reset entirely
Fix: Changed `git reset` to `git reset --hard` so remote files are checked out into the working tree. Fixed PS scripts to temporarily set `$ErrorActionPreference = "Continue"` around git commands that may produce stderr or non-zero exit codes. Also fixed the same stderr handling bug in `session-sync.ps1` `pull --rebase`. Data was restored in `a8b8e5b`; diff between `199a29d` (pre-deletion) and `acc5467` (current) is empty — no data loss

### #20: Session sync propagates Claude Code format migration deletions (28f343b)
Cause: Claude Code migrated session storage from `UUID.jsonl` (flat file) to `UUID/subagents/` (directory). On TEC, old `.jsonl` files were deleted locally by Claude Code. `session-sync push` (`git add .`) staged these deletions and pushed them to remote, removing 35 files from other machines' history
Fix: No action taken. 25/35 files had directory versions (no data loss). Remaining 10 were short sessions (1–15 lines) likely auto-pruned. One-time format migration event — will not recur. `--ignore-removal` rejected as it would block legitimate manual deletions


### Private repo detection: static list → dynamic gh API (ab6820b)
Background: Keeping a full repository list locally in `private-repos.txt` risked exposing private repository names if the file was accidentally shared. Also, PreToolUse hooks could not determine private repo status when running `git -C <path> commit` (always scanned because `filePath` was empty).
Changes: Switched to dynamic detection via `gh api repos/{owner}/{repo} --jq .private`. Created shared module `claude-global/hooks/lib/is-private-repo.js`, used by all 3 PreToolUse hooks (check-private-info/check-docs-updated/check-test-updated) and 2 git hooks (pre-commit/commit-msg). Fail-open when `gh` is not installed or API call fails. Deleted `bin/update-private-repos.sh` and `.context-private/private-repos.txt`.

### commands → skills migration cleanup (ab6820b)
Background: commands → skills migration completed on all PCs.
Changes: Removed `BEGIN temporary: commands → skills` blocks from `.profile_common` and `install/win/profile.ps1`. Kept claude-code symlink cleanup code in `install-obsolete.sh/ps1` as a safety net.

### Claude Code branch/push delete deny rules ((pending))
Background: In another project (langchain-stack), Claude deleted local and remote branches without confirmation via `git -C`. `git push --delete` was already denied, but the allow rule `git -C * push origin *` may have been approved via dynamic permission ("Yes, don't ask again"). `git branch -D` was not in deny or ask lists. Cross-reviewed with ChatGPT and also covered `git push origin :branch` (refspec-style deletion).
Changes: Added deny rules: `*git branch -D*`, `*git branch -d*`, `*git push origin :*`, `*git push *origin :*`.

### dotfiles clone recovery after git rebase ()
Background: After git rebase, dotfiles clones on each PC diverged from origin/main.
Changes: Ran `git fetch origin && git reset --hard origin/main` on all PCs. Verified complete.

### Claude Tabs installer ((pending))
Background: Claude Tabs (Tauri v2) is a Windows-native Claude Code multi-session management app. Tab UI + Activity Feed for real-time agent status display.
Changes: Created `install/win/claude-tabs.ps1` (fetches latest release via GitHub API, /S silent install). Falls back to scanning the 10 most recent releases when `/releases/latest` has no attached assets. Created `.cross-platform-skiplist` (hook exclusion list for Windows-only tools).

### Node.js version manager: platform split (fnm → nvm on Unix) (1b74132, (pending))
Background: NemoClaw official installer unconditionally installs nvm. Conflicts with dotfiles' "fnm everywhere" rule — npm install fails when prek refuses to install hooks with core.hooksPath set. fnm has no advantage over nvm on Unix; nvm is the ecosystem standard. Windows needs fnm (nvm has no Windows support)
Changes: New rule: Windows=fnm, WSL2/macOS/Linux=nvm. Replaced `install/linux/fnm.sh` with `nvm.sh`. Removed fnm from `.profile_common` PATH and init (nvm init already existed at lines 190-206). Added fnm cleanup to `install-obsolete.sh`. Updated `coding.md` rule to platform-specific. Fixed nvm.sh execute permission (1b74132)

### Keychain SSH key auto-detect (6b39058)
Background: SSH key specification for keychain was hardcoded.
Changes: Promoted keychain step in `install.sh` to run by default. `.profile_common` now auto-detects keys by globbing `~/.ssh/id_*`. Created `install/linux/keychain.sh`.

### Claude Code session sync (513e3a4, (pending))
Background: Claude Code session history (`~/.claude/projects/`) is stored locally on each machine with no official cross-device sync. Wanted to reference past conversations across multiple Windows PCs.
Alternatives considered:
  (a) Use only Claude Code Web mode (claude.ai/code) — Local mode is required for tasks needing local filesystem, MCP servers, or NSSM service operations (NemoClaw, llama-swap, etc.). Sessions are split between Local and Web. Rejected
  (b) perfectra1n/claude-code-sync (Rust-based external tool) — the needed functionality is implementable in under 50 lines and integrates naturally into dotfiles, so chose to build in-house
  (c) Remote Control (`claude remote-control`) — requires the original PC to be running, disconnects after 10 minutes. Not suitable for history reference
  (d) Wait for Anthropic official sync (Issue #22648) — timeline unknown
Design decisions:
  (1) Path unification: unified under drive root with dedicated directories for LLM infrastructure (existing, immovable) and `C:\git\` (new). Eliminated `~/git/` which contains the username, ensuring identical absolute paths across all machines
  (2) Line ending control: cross-reviewed with ChatGPT, compared 3 approaches. `core.autocrlf=false` (stops conversion only, no LF guarantee), `* -text` (same), `* text eol=lf` (explicitly declares LF, propagates via repo). Adopted `* text eol=lf` in `.gitattributes`. Reason: JSONL needs guaranteed LF on all machines + auto-propagation on clone/pull
  (3) File writing: `Set-Content` (PowerShell cmdlet) outputs CRLF on Windows, causing git add warnings. Switched to `[System.IO.File]::WriteAllText()` with CRLF→LF replacement for LF output. Cross-reviewed with ChatGPT for compatibility (.NET standard API, PS5/PS7 compatible, no UTF-8 BOM)
  (4) Sync scope: `projects/` only. `settings.json`, `rules/`, `skills/` etc. managed by dotfiles. `statsig/`, `ide/` are machine-specific
  (5) Conflict strategy: JSONL is append-only. Avoided by push→pull routine. On conflict: `git pull --rebase`
Changes: Created `install/win/session-sync-init.ps1` (initialization), `bin/session-sync.ps1` (push/pull/status), integrated into `install.ps1`, created `nirecom/claude-sessions` private repo. 12 Pester test cases.

### Cross-platform check hook (5c7714e)
Background: Prevent cases where `install/win/` is modified but `install/linux/` counterpart is forgotten.
Changes: Created `check-cross-platform.js` PreToolUse hook. Detects missing counterpart changes for platform-specific files at `git commit` time. `.cross-platform-skiplist` for permanent exclusion, `.git/.cross-platform-reviewed` for one-time exclusion. 220 test cases.

### ~/dotfiles → C:\git\dotfiles path unification ((pending))
Background: To share Claude Code session history (`~/.claude/projects/`) across Windows PCs, all machines need a uniform dotfiles path. `~/dotfiles` (= `C:\Users\<user>\dotfiles`) includes the username, causing project directory names to differ per PC. `C:\git\dotfiles` makes the project key `c--git-dotfiles` on all PCs.
Alternatives considered:
  (a) Auto-migrate via robocopy in install.ps1 — `[IO.Directory]::Move` fails because AutoHotkey holds file locks. robocopy can read-copy locked files, but source cleanup is best-effort. Worse, profile.ps1's git-fetch pulls the new code immediately, breaking all path references before install.ps1 (manual execution) can run
  (b) Auto-migrate in profile.ps1 at startup — solves the timing issue, but the temporary migration code must be managed (removed after all PCs migrate), adding maintenance cost
  (c) No migration; dynamically rewrite JSONL session history paths on git pull — JSONL files contain absolute paths in multiple notations (Windows backslash, MSYS forward-slash, tilde) scattered across file_path fields, conversation text, and tool results. Reliable rewriting across all notations is fragile and risks data corruption. Rejected
  (d) No migration code at all; manually `git clone` to `C:\git\dotfiles` on each PC — simplest approach. Path references updated in code, install.ps1 re-run handles the rest
Decision: (d). No automated migration. Each PC is migrated by manual clone + install.ps1.
Cross-platform issue: settings.json hook paths (`node ~/dotfiles/...`) are shared with macOS/Linux. Hardcoding `C:\git\dotfiles` would break macOS. Solved with `$DOTFILES_DIR` environment variable — Windows sets it as a persistent user env var in install.ps1, macOS/Linux exports it in `.profile_common`. Hook commands use `node "$DOTFILES_DIR/..."` which the shell expands.
Changes:
  - `install.ps1`: removed migration block, added `DOTFILES_DIR` persistent user env var registration
  - `profile.ps1`: `$DotfilesDir = "C:\git\dotfiles"` + session `$env:DOTFILES_DIR`
  - `.profile_common`: added `export DOTFILES_DIR="$HOME/dotfiles"`, replaced all `~/dotfiles` references with `$DOTFILES_DIR`
  - `settings.json`: changed 5 hook paths to `$DOTFILES_DIR/...`
  - `autohotkey.ps1`: added Step 3a to detect AHK running from old path and restart from new
  - `install-obsolete.ps1`: added cleanup for `~/dotfiles`, `~/dotfiles-private`, `~/git`
  - `migrate-repos.ps1` + tests: deleted (no longer needed)
  - `main-install-obsolete-migration.Tests.ps1`: new (4 tests)

### Session sync git root relocation (dbd003a)
Background: `~/.claude/` was used as git root for session sync, but only `projects/` was tracked. This required a complex `.gitignore` (exclude-all + re-include pattern) and caused a misleading warning in `dotfileslink.ps1` about symlink conflict with `.git`. Moving git root to `~/.claude/projects/` eliminates both issues.
Changes:
  - `session-sync-init.ps1`: git init target changed from `$ClaudeDir` to `$ClaudeDir/projects`. Added migration logic to remove old `.git`/`.gitignore`/`.gitattributes` from `~/.claude/`. Removed `.gitignore` (no longer needed). `.gitattributes` created in `projects/`
  - `bin/session-sync.ps1`: all `git -C` paths changed from `$ClaudeDir` to `$ProjectsDir` (= `$ClaudeDir/projects`)
  - `dotfileslink.ps1`: removed "dotclaude" warning about `.git` in `~/.claude/`
  - Remote repo `nirecom/claude-sessions` recreated (old layout had `projects/` prefix in tree; new layout stores files at root)
  - Tests: 12 tests updated for new layout + migration test added

### AutoHotkey per-user path fallback (1a15c1f, 29bfd8a)
Background: AutoHotkey v2 per-user installs (`%LOCALAPPDATA%\Programs\`) were not found because only `%ProgramFiles%` was searched
Changes: Added per-user install path fallback to `autohotkey.ps1`. Also fixed shortcut argument update bug and path matching bug

### Session sync: terminal startup fetch + codes function (d3c3a38, 7d7a98e, 4f8e4c3, 20ebb49)
Background: session-sync push/pull required manual execution. Wanted automatic fetch on terminal startup and automatic push when closing VS Code
Changes:
  - `.profile_common`: added `git fetch + merge --ff-only` for `~/.claude/projects/` on terminal startup (3s timeout). Added `codes` function (opens VS Code, runs `session-sync.sh push` on exit)
  - `install/win/profile.ps1`: equivalent fetch logic added
  - `bin/session-sync.sh`: new Linux/macOS session sync script (push/pull/status subcommands)

### Session sync: cross-platform support (d50203c, (pending))
Background: Session sync was Windows-only but the same session sharing is needed on macOS/Linux
Changes:
  - `install/linux/session-sync-init.sh`: new (equivalent to `session-sync-init.ps1` — git init, migration, remote setup, initial commit+push)
  - `install.sh`: added session-sync-init call after Claude Code install (`type claude` guard)
  - `install-obsolete.sh`: added Homebrew fnm cleanup (`brew list fnm && brew uninstall fnm`; not originally installed via dotfiles but cleaned up as a precaution)

### Session sync: init/push reliability fix (efbd047)
Background: `session-sync-init.sh`'s initial commit flow did not account for cases where remote already has history, causing init → push to fail on 2nd+ machines. Push also failed when remote was ahead.
Design decisions:
  (1) `reset --hard` vs `reset` (mixed): `reset --hard` risks overwriting local JSONL files that share names with remote (happens when two machines use the same path). `reset` (mixed) only moves HEAD, preserving local files. File checkout is handled by normal pull
  (2) Background push output from `codes()`: output interrupts after prompt, requiring an extra Enter. Attempted SIGWINCH-triggered prompt redraw but it had no effect. Suppressed output to `/dev/null`
Changes:
  - `session-sync-init.sh`/`.ps1`: added `fetch origin main` + `reset origin/main` before initial commit to merge remote history. Tolerate `commit` failure when there are no changes (`|| true`)
  - `bin/session-sync.sh`/`.ps1`: added `pull --rebase origin main` before push
  - `.profile_common`: suppressed `codes()` push output with `>/dev/null 2>&1`

### Session sync: separate init from sync, add reset action (fc71b9e)
Background: Init script's fetch/reset/commit/push block was dangerous — it could overwrite remote data when a new PC joined (#19). Also, auto-running sync during install made it hard to control which PC should sync first
Changes:
  - `session-sync-init.ps1`/`.sh`: removed all fetch/reset/commit/push logic. Init now only sets up plumbing (git init, remote, .gitattributes, hooks)
  - `session-sync.ps1`/`.sh`: added `reset` action (`git fetch + reset --hard origin/main`) for manual initial sync or recovery. After reset, normal bidirectional push/pull works
  - New PC flow: `install.ps1` → init (safe), then manual `session-sync reset` to catch up to remote

### Session sync: history.jsonl sync and mtime restore (d214411, 77d00e4)
Background: Sessions synced from other machines were invisible or misordered on the receiving PC. Two root causes:
  (1) Claude Code uses `~/.claude/history.jsonl` (outside the sync git root `~/.claude/projects/`) for session listing. Without syncing this file, the receiving PC had no record of remote sessions
  (2) Claude Code uses file mtime for session ordering. `git reset --hard` sets all file mtimes to the current time, making all synced sessions appear as "just now"
  Investigation of OSS tools (x-cmd/x-cmd ★4k, mad0ps/claude-code-server-syncer) confirmed that history.jsonl rewriting and/or sessions-index.json manipulation is standard practice. x-cmd rewrites history.jsonl `.project` field on import; mad0ps deletes sessions-index.json and lets Claude Code regenerate it. Neither preserves file mtime.
  Additionally, session directory names encode the working directory path (`C:\git\dotfiles` → `c--git-dotfiles`). When the same project was opened from different paths on different machines (or after a path migration), sessions were stored in separate directories and invisible to each other. Resolved by copying the old path directory to the new path name and rewriting history.jsonl project fields.
Changes:
  - Push: copies `~/.claude/history.jsonl` into sync area as `.history.jsonl`
  - Pull/Reset: merges remote `.history.jsonl` with local history (line-level dedup, remote-first ordering)
  - Reset: restores mtime on each `.jsonl` from its last (or first, as fallback) `timestamp` field

### Session sync: Claude Code format migration causes bulk deletion (28f343b)
Cause: Claude Code migrated session storage format from `UUID.jsonl` (flat file) to `UUID/subagents/` (directory structure). PENPEN pushed both old and new formats on 3/29. When TEC ran `session-sync push` on 4/2, Claude Code had already deleted the old `.jsonl` files locally, so `git add .` staged 35 deletions and propagated them to remote.
Fix: No action needed (keep as-is). 25 of 35 files have directory versions — no data loss. Remaining 10 are short sessions (1–15 lines) likely pruned by Claude Code, with no practical impact. The format migration is a one-time event and will not recur. Changing `git add .` to `--ignore-removal` was considered but rejected — it would also block intentional manual deletions and legitimate prunes.

### Notification hook for permission_prompt (f447e11, 7ef7776)
Background: Claude Code permission_prompt dialogs were easy to miss, leaving sessions idle
Changes: Added `check-notification.js` PreToolUse hook that sends OS toast notification on `permission_prompt` events. Moved WebSearch/WebFetch from auto-allow to `permissions.ask` (now requires explicit confirmation)

### Add VS Code and extensions installer (aa3f295)
Background: Setting up a new development machine required manually installing VS Code and re-adding extensions one by one. Needed automated install via `-Develop`/`--develop` flag.
Changes: Created `config/vscode-extensions.txt` (shared extension list), `install/win/vscode.ps1` (winget-based), and `install/linux/vscode.sh` (apt/brew, WSL skip). Added to `-Develop`/`--develop` section in both `install.ps1` and `install.sh`.

### Auto-unstage effortLevel-only changes in settings.json (84db276)
Background: effortLevel is written to settings.json by Claude Code's /fast toggle and effort level changes. Since settings.json is tracked in git, effortLevel-only changes kept appearing in git diff, requiring manual cleanup. Previous fix (84db276) removed effortLevel from the committed file, but it reappears whenever Claude Code writes it.
Changes: Added auto-unstage logic to claude-global/hooks/pre-commit. On commit, compares HEAD vs staged settings.json after stripping effortLevel (via node JSON parse). If identical, auto-unstages the file. Handles addition, value change, and removal of effortLevel. Mixed changes (effortLevel + real content) are preserved. Also fixed `core.hooksPath` on Windows: shared `.config/git/config` has `~/dotfiles/...` (works on Linux/macOS), but Windows dotfiles is at `C:\git\dotfiles`. Added `core.hooksPath` override in `config.local` via `dotfileslink.ps1`.

### Add research phase to workflow
Background: Investigated well-known Claude Code frameworks (Alex Kurkin's Research→Plan→Implement, everything-claude-code, claude-research-plan-implement). Found that a research phase before planning prevents implementing against wrong assumptions. Separated into two skills: `/survey-code` (codebase exploration) and `/deep-research` (web research), both at effort: medium — low loses cross-source reasoning, high is overkill for information gathering. Name choices: `research` alone risks future built-in conflict (precedent: `/plan` → `/make-plan`). `deep-research` aligns with established OSS convention. `survey-code` distinguishes from web research.
Changes: Added `claude-global/skills/survey-code/SKILL.md` and `claude-global/skills/deep-research/SKILL.md`. Updated `claude-global/CLAUDE.md` workflow from 7 steps to 8 (Research as step 1).

### Fix update-docs skill to detect uncommitted changes
Background: In the workflow, /update-docs runs before commit (step 5), but change detection relied solely on `git log`. Uncommitted session changes were invisible, causing documentation gaps.
Changes: Added `git diff` / `git diff --cached` to the gather step to detect unstaged and staged changes. Removed LangChain-specific instruction from the procedure, delegating to the Project Detection section.

### Workflow rules reorganization
Background: workflow.md contained a mix of procedural rules (verification, diff approval) and design policies (cross-platform/naming orthogonality). Rules files are always loaded into context, so there was no stage-gate mechanism — workflow rules had no more prominence than any other rule file. Community research showed plan mode thresholds vary, with Anthropic's official guidance being "skip if the task is describable in one sentence."
Changes:
- Dissolved `workflow.md` — procedural steps moved to global `CLAUDE.md` as numbered Workflow (Plan → Write tests → Code → Test & Verify → Docs → User verification → Commit)
- Orthogonality rules extracted to `rules/orthogonality.md` (referenced by `coding.md` and `make-plan/SKILL.md`)
- `test.md` reordered: Test Case Categories first, removed duplicate "test before code" instruction (now in CLAUDE.md)
- Removed `Rules are in rules/. Skills are in skills/.` pointer from CLAUDE.md (auto-discovered by Claude Code)

### Add write-tests and make-plan skills with effort: high (23e4eba, 72f4168)
Background: Procedural instructions in rules/test.md were sometimes ignored under context pressure, and effort level could not be controlled. Specifying effort: high in the skill frontmatter raises reasoning effort only during skill execution.
Changes: Created /write-tests and /make-plan skills (effort: high). Replaced procedural instructions in rules/test.md with /write-tests invocation. Category definitions, naming conventions, and timeout rules remain in rules/ (shared SSOT with /review-tests). Initially created as /plan but renamed to /make-plan due to built-in command conflict. effort frontmatter is an official feature added in v2.1.80 (priority: env var > skill frontmatter > /effort session > model default). Known bug: VS Code UI does not reflect effort changes (anthropics/claude-code#31751), but the switch works internally.

### Force push divergence detection on shell startup (257958a)
Background: After git rebase, dotfiles clones on each PC diverged from origin/main. `merge --ff-only` failed silently, leaving no indication that a reset was needed. With 5 PCs, forgetting to reset one was likely.
Changes: Added divergence detection to `.profile_common` auto-fetch. When `merge --ff-only` fails and histories have diverged, prompts the user with y/N to reset (10s timeout, defaults to N). `~/.dotfiles-no-auto-reset` marker file suppresses the prompt on the master PC (warning only). Non-interactive shells skip silently.

### Add -Toolchain option and AWS CLI installer for Windows
Background: vs-cpp (Visual Studio C++ workload) is a heavy install that doesn't belong in the general `-Develop` tier. AWS CLI was available on Linux but missing from Windows.
Changes: Added `-Toolchain` parameter to `install.ps1` for heavy build toolchain installs (vs-cpp moved here from `-Develop`). Created `install/win/awscli.ps1` using winget, added to `-Develop`. `-Full` now includes Base + Develop + Toolchain.

### Fix codes function to survive terminal close (781f512)
Background: `codes` alias uses `Start-Job` to push session sync after VS Code closes, but `Start-Job` is tied to the parent PowerShell session. Closing the terminal before VS Code was confirmed to sometimes kill the job before push ran.
Changes: Replaced `Start-Job` with `Start-Process pwsh -WindowStyle Hidden` so the push process is independent of the terminal lifecycle.

### Fix codes session sync not firing per-window
Background: `code --new-window --wait`'s `--wait` flag waits for the entire VS Code server process to exit. When multiple windows are open, closing an individual window did not trigger session-sync push. No push ran unless all windows were closed.
Changes: Replaced `--wait` with window title polling via Win32 EnumWindows API (Windows) / xdotool, wmctrl, osascript (Linux/macOS). Created `bin/wait-vscode-window.ps1` and `bin/wait-vscode-window.sh`. The `codes` function resolves the workspace name and detects the target window's appearance then disappearance before running push. Also handles `.code-workspace` files with `(Workspace)` title suffix.

### Fix codes multi-instance support
Background: Opening a second workspace with `codes` caused VS Code to reuse the existing window, making the first one disappear.
Changes: Added `--new-window` flag to `code --wait` (in `.profile_common` and `profile.ps1`). Each invocation now opens in an independent window.

### Remove container extensions from VS Code auto-install (0a3ba69)
Background: Dev Containers extension (`ms-vscode-remote.remote-containers`) silently installed Docker Desktop on macOS without user prompt. On company PCs, Docker Desktop Personal edition may violate commercial licensing terms.
Changes: Removed `ms-azuretools.vscode-containers` and `ms-vscode-remote.remote-containers` from `config/vscode-extensions.txt`. Users who need them can install manually.

### Security enhancement research and planning (uncommitted)
Background: No systematic security checklist during architecture planning (information leakage, third-party access, external access). No security test coverage guidelines. OWASP MCP Top 10 identified prompt injection via MCP plugins as a new threat vector.
Changes: Researched external sources (OWASP WSTG/ASVS/LLM Top 10/MCP Top 10, everything-claude-code, lasso-security/claude-hooks, Gitleaks vs Semgrep comparison). Documented 4-phase plan in `docs/plan.md`. Key design decision: extract security checklists and patterns into skills (not rules/) to minimize always-loaded context window consumption. Skill naming validated against major frameworks (everything-claude-code `/security-scan`, qdhenry `/security:security-audit`, etc.) — `verb-noun` kebab-case is the dominant convention.

### Installer robustness and option hierarchy fix (uncommitted)
Background: On a secondary PC, `install.ps1 -Develop` failed: AWS CLI winget install hit MSI mutex (exit code 1618) but reported success; Python was missing because `-Develop` didn't include `-Base` packages; PowerShell profile lacked diverge detection (bash-only).
Changes: Added `Wait-MsiMutex` function to `install.ps1` (waits for running MSI before proceeding). Unified `$LASTEXITCODE` check after `winget install` in all 8 scripts. Added `uv python install` step to `uv.ps1`/`uv.sh` with `UV_NATIVE_TLS=1` for proxy environments. Ported force-push diverge detection from `.profile_common` to `profile.ps1` (y/N prompt, 10s timeout, marker file support). Changed option hierarchy to cumulative: `-Develop` includes `-Base`, `-Toolchain` includes `-Develop`. Extracted installer rules from `coding.md` to `installer.md`.

### Reduce WebSearch/WebFetch permission prompts (uncommitted)
Background: During deep-research skill execution, repeated permission dialogs for WebSearch and WebFetch were a UX obstacle. Users could not assess the risk of unknown URLs, and the permission prompts provided no real security value. Conducted two security reviews with ChatGPT, which led to excluding user-generated content domains like GitHub from the allow list.
Changes: Added WebSearch and 8 low-risk documentation domain WebFetch entries (MDN, Python docs, Microsoft Learn, man7, Anthropic docs, OpenAI docs, Google AI docs, GitHub Docs) to settings.json allow. Removed WebSearch and WebFetch(domain:github.com) from ask. Domain selection criterion was not "trust" but "whether existing defense layers (deny rules, diff review, pre-commit hook) prevent damage in case of incident."

### Restructure /update-docs skill for ai-specs project coverage (uncommitted)
Background: /update-docs Project Detection only covered langchain-stack projects. As ai-specs grew to include llama-swap, judgeclaw, and others, coverage was insufficient. The llama-swap docs update procedure was also scattered in ai-specs/CLAUDE.md separately.
Changes: Renamed LangChain projects to ai-specs projects. Added llama-swap and judgeclaw to Source repos. Reordered sections (General projects first, ai-specs second). Simplified ai-specs/CLAUDE.md llama-swap section to a pointer to the /update-docs skill. Attempted external file separation via `!`cat`` preprocessing but abandoned due to security sandbox (blocks cat outside working directory).

### history.md unified chronological format (2026-04-07, (pending))
Background: history.md had inconsistent ordering across projects (some ascending, some descending) and split Change/Incident sections that broke chronological flow. Research confirmed ascending order is optimal for LLM context processing (recency bias > primacy bias in deep transformer layers; context compression preserves later content). Research documented in ai-specs `engineering/research-results/llm-document-ordering.md`.
Changes: Created `bin/sort-history.py` (anchor-block algorithm: resolves commit hashes to git dates, groups dateless entries with preceding anchor, sorts blocks ascending). Merged `## Change History` and `## Incident History` into single chronological stream. Incident entries retain `#N:` prefix for identification. Updated `docs-convention.md`: ascending order mandatory, `##` section split prohibited, date `YYYY-MM-DD` mandatory in entry titles (rebase-proof), sort tool reference added. 24 test cases.

### history.md Japanese translation tool (2026-04-07, (pending))
Background: Public repo history.md contained 2 entries with Japanese text, violating the "public repos → English" rule. Manual LLM translation risks omissions, so a reusable tool was needed.
Changes: Created `bin/translate-history.py` (two-phase workflow: `--extract` outputs JSON manifest of Japanese entries, `--apply` replaces them with translations from a completed manifest). Detection uses Unicode range matching (Hiragana/Katakana/Kanji). Includes `is_public_repo()` (ported from `hooks/lib/is-private-repo.js`) for auto-detection with `--public`/`--private` override. Translated 2 Japanese entries in `docs/history.md`. 16 test cases (21 assertions).

### Consolidate subagent instructions to skill definitions (2026-04-08, (pending))
Background: rules/test.md contained subagent implementation details (how subagent works, what it launches) that duplicated the authoritative definitions in write-tests and review-tests skills. Prior commit a951983 had added subagent enforcement to rules/test.md because procedural instructions were sometimes ignored, but this created a maintenance burden — two places to update when the mechanism changed.
Changes: Replaced "Test Iteration Subagent" section with "Test Writing" (pointer to `/write-tests` only). Removed Explore subagent explanation from "Test Coverage Review" (pointer to `/review-tests` only). Subagent implementation details now live exclusively in skill SKILL.md files.

### Session sync: silent push failure notification (2026-04-08, (pending))
Background: `codes` function ran session-sync push in a hidden process (Windows: `-WindowStyle Hidden`, Linux: `>/dev/null 2>&1`). Push failures were completely invisible — cross-machine sync silently failed for 6 days (diverged branch from 4/2 to 4/8) with no indication.
Changes: Added `--quiet` flag (sh) / `-Quiet` switch (ps1) to session-sync scripts. In quiet mode, success is silent; failure triggers OS notification (Windows: `System.Windows.Forms.MessageBox`, Linux: `notify-send` with stderr fallback). `codes` function now passes the quiet flag instead of discarding all output.

### Enforce workflow via TodoWrite checklist (2026-04-08, (pending))
Background: CLAUDE.md workflow steps (Research, Plan) were frequently skipped at session start. The "describable in one sentence" skip criterion was too vague, allowing rationalization.
Changes: Rewrote workflow preamble to require TodoWrite checklist creation for every task. Removed `as needed` from Research step. Replaced vague skip criteria with concrete conditions (single-file change AND no design decision). Added explicit rule that skipping Research does not justify skipping Plan.

### Commit confirmation reduced from 2 to 1 (2026-04-08, 74662ae, 4223e09)
Background: Committing required two approvals — skill chat confirmation and settings.json permission dialog. Hooks (pre-commit, commit-msg) now reliably block private info, making the permission dialog redundant.
Changes: Added commit message presentation step to commit-push skill (diff stats + message shown in chat, wait for approval). Moved `Bash(git commit *)` and `Bash(git -C * commit *)` from ask to allow in settings.json. `cd && git commit` pattern remains in ask (prohibited by rules/git.md).

### Session sync: toast notifications and output cleanup (2026-04-08, (pending))
Background: Quiet-mode push showed no indication of sync start/end, making it impossible to know when it was safe to shut down the laptop. Additionally, `git commit` output included noisy "create mode" / "delete mode" lines. Failure notification used `MessageBox` (modal, requires click to dismiss).
Changes: Added `git commit -q` flag to suppress file mode messages (both sh/ps1). Replaced `MessageBox` with Windows toast notifications (`WinRT ToastNotificationManager` API, no external modules). Quiet mode now shows toast at push start ("pushing..."), completion ("push complete"), and failure. Because pwsh 7+ cannot directly load WinRT types, `Show-SessionToast` in ps1 shells out to `powershell.exe` (Windows PowerShell 5.1) to run the toast code — this works from both pwsh 7 and PS 5.1 callers. Bash/WSL2 also calls `powershell.exe` for toast; native Linux falls back to `notify-send`. Fixed `git add`/`git push` stderr handling in ps1 (`$ErrorActionPreference` temporarily set to `Continue` to prevent CRLF warnings from throwing).

### todo.md completion cleanup rule (2026-04-09, (pending))
Background: After verification passed, completed phases/steps were sometimes left behind in todo.md as `[x]` checkboxes or stub pointers back to history.md, causing todo.md to bloat and duplicate history.md content.
Changes: Updated `claude-global/rules/docs-convention.md` todo.md rule to explicitly require full removal from todo.md after verification passes — no leftover checkboxes, sub-steps, or stub pointers back to history.md. Entry must exist in exactly one place.

### make-plan: planner/reviewer discussion loop (2026-04-09, (pending))
Background: The Plan phase used a single `/make-plan` skill drafting the plan in the main conversation. No independent critique step — quality relied on the main Claude catching its own gaps, and thorough review was inconsistent.
Changes: Introduced `claude-global/agents/` with two subagents — `planner` (drafts and revises plans) and `reviewer` (critically reviews, surfaces minor and major issues). `/make-plan` now orchestrates a discussion loop: planner drafts → reviewer returns `APPROVED` or `NEEDS_REVISION` with numbered concerns → planner revises → re-review. Escalates to the user after 3 rounds without approval. Skip conditions (single-file change AND no design decision) are preserved. Added `agents` symlink to install scripts (`install/linux/dotfileslink.sh`, `install/win/dotfileslink.ps1`). Updated `claude-global/CLAUDE.md` workflow Plan step description.

### Session sync: consolidate toast to single completion notification (2026-04-09, (pending))
Background: Quiet-mode push showed two toast banners per push — "pushing..." at start and "push complete" at end — which felt redundant. The original purpose was to signal when it was safe to shut down the laptop, so only the completion state is essential. Attempted in-place text update via `NotificationData` + `ToastNotifier.Update()`, but PS 5.1's COM interop cannot project WinRT `IMap<string,string>` to `IDictionary`, so neither indexer assignment nor `.Insert()` method works. Tag+Group replacement shows a second banner pop rather than a smooth in-place update.
Changes: Removed the "pushing..." toast call from both `bin/session-sync.ps1` and `bin/session-sync.sh`. Only "push complete" / "push failed (...)" is shown now. Kept Tag+Group on the ps1 toast so consecutive pushes replace the previous entry in Action Center instead of accumulating. Updated `docs/architecture.md` to reflect single-notification behavior.

### write-tests subagent mode: auto → acceptEdits (2026-04-09, (pending))
Background: `/write-tests` was designed to run test iteration inside a subagent to avoid Edit confirmation dialogs, but dialogs for `tests/` files were still appearing in the parent conversation. Root cause: VSCode extension's "Ask before edits" mode gates Edit/Write at the extension level, which the subagent's `mode: "auto"` parameter does not override.
Changes: Changed subagent launch mode in `claude-global/skills/write-tests/SKILL.md` from `"auto"` to `"acceptEdits"` to explicitly pre-authorize edits. If insufficient, next step is to add `Edit(tests/**)` / `Write(tests/**)` to `permissions.allow` in `settings.json` (recorded in memory for follow-up).

### commit-push: restore permission dialog as single gate (2026-04-09, (pending))
Background: Prior fix (4223e09, 5b0ceed) moved `Bash(git commit *)` and `Bash(git -C * commit *)` from ask to allow, expecting the skill's chat confirmation to serve as the sole approval point. In practice Claude frequently skipped the chat step and ran add→commit→push without any gate, because no tool call enforced a pause. The chat-only gate was unreliable.
Changes: Reverted both patterns back to `permissions.ask` in `claude-global/settings.json`. Removed "Wait for user approval before proceeding" from step 3 of `claude-global/skills/commit-push/SKILL.md` — the chat message is now informational, and the commit permission dialog is the single enforced gate. Net result: skill presents drafted message in chat → `git commit` triggers one dialog → approval cascades through to `git push` (still allow-listed). Same 1-confirmation goal as 4223e09 but achieved via a mechanism that does not depend on Claude honoring a string instruction.

### VS Code installer: extension existence check bugfix (2026-04-10, (pending))
Background: During verification of the VS Code installer, every `install.ps1 -Develop` run was reinstalling all extensions instead of skipping existing ones. Root cause: `install/win/vscode.ps1` joined `code --list-extensions` output into a single newline-delimited string and matched with `-match "^...$"`. .NET regex defaults to single-line mode, so `^`/`$` only anchor to the start/end of the whole string — only the first extension in the list could ever match, and all others were treated as missing.
Changes: Replaced the string+regex approach with an array-based case-insensitive `-contains` check (`@(code --list-extensions) | ForEach-Object { $_.ToLowerInvariant() }`). Linux `vscode.sh` was unaffected (uses `grep -qi "^${ext}$"` which matches per-line).

### Session sync: filter create/delete mode from pull output (2026-04-10, (pending))
Background: Previous output cleanup (22fed74) added `git commit -q` to suppress `create mode` / `delete mode` lines during push. However, the same lines still appeared during `pull` — they come from git's fast-forward merge output, not from commit.
Changes: Added `Where-Object` filter to `git pull --rebase` output in `bin/session-sync.ps1` pull action, excluding lines matching `^\s*(create|delete) mode `. Extended the same filter to `bin/session-sync.sh` pull action using `grep -Ev` with `PIPESTATUS[0]` to preserve git's exit code.

### Optimize skill token usage with model and effort tuning (2026-04-10, 986d925)
Background: Total token usage was frequently hitting limits. All skills inherited the session model (Opus) and effort level, even for mechanical tasks like git operations or doc updates. Official docs confirmed `model` and `effort` frontmatter are supported in both skills and subagents (https://code.claude.com/docs/en/skills, https://code.claude.com/docs/en/sub-agents).
Changes: Set `model: haiku` + `effort: low` on `commit-push` (git-only). Set `model: sonnet` + `effort: low` on `survey-code`, `review-tests`, `update-docs`, `update-instruction`. Reasoning-heavy skills (`deep-research`, `review-security`, `write-tests`) and agents (`planner`, `reviewer`) remain unchanged (inherit session model).

### make-plan token optimization: reviewer checklist and loop limit (2026-04-11, (pending))
Background: make-plan planner/reviewer loop consumed excessive tokens. Two causes identified: (1) reviewer's 7-item checklist with individual rule file references encouraged redundant Read calls for rules already loaded as Memory Files (~3.6k tokens wasted per round), (2) 3-round loop limit allowed up to 3 expensive revision cycles.
Research: Surveyed GitHub Claude Code patterns (HumanLayer, everything-claude-code, anthropic repos, lst97/claude-code-sub-agents). Key findings: community consensus is lean subagent prompts; rules/ files loaded via Memory Files should not be re-read; skills: frontmatter enables on-demand injection but is unsuitable for always-applicable rules. Reviewer effort: high retained (core quality mechanism).
Changes: Consolidated reviewer checklist from 7 items to 4 (correctness+completeness, rules compliance, risks+edge cases, scope). Added explicit instruction not to re-read rules via Read tool. Reduced loop escalation limit from 3 to 2 rounds.

### Explicit model pinning for quality-critical skills and agents (2026-04-11, (pending))
Background: Preparing to set CLI default model to haiku for cost reduction. `model: inherit` and unspecified model fields would inherit haiku, degrading quality for reasoning-heavy tasks. Research confirmed `CLAUDE_CODE_SUBAGENT_MODEL` env var overrides ALL subagents including frontmatter (official docs priority order: env var > call-time > frontmatter > parent model), making it unsuitable for selective control.
Changes: Set `model: opus` on 6 definitions: planner, reviewer (previously `inherit`), deep-research, make-plan, review-security, write-tests (previously unspecified). Mechanical skills remain unchanged (commit-push: haiku, survey-code/review-tests/update-docs/update-instruction: sonnet).

### save-research skill (2026-04-11, (pending))
Background: Useful research findings from conversations were lost after the session ended, requiring re-investigation on the same topics.
Changes: Added `claude-global/skills/save-research/SKILL.md`. The skill saves conversation research findings to `../ai-specs/projects/engineering/research-results/<slug>.md` using relative paths (dotfiles is a public repo). Follows the established format of the existing `llm-document-ordering.md` research file.

### check-private-info.sh: external allowlist and glob file-scope support (2026-04-11, (pending))
Background: Test-file allowlist entries were listed individually per file, making it verbose to add new test files. Environment-specific exceptions (e.g., company IP ranges) had no place to live without modifying the public .private-info-allowlist.
Changes: Added ALLOWLIST_PRIVATE loading from ../dotfiles-private/.private-info-allowlist when present — environment-specific exceptions can live in the private repo without touching the public file. Added glob matching for file-scoped allowlist patterns (e.g., tests/*:@example.com), consolidating per-file entries into single glob lines.

### git config: work email isolation via dotfiles-private (2026-04-11, (pending))
Background: Work email and company GitLab URL need to be kept out of the public dotfiles repo. config.local was auto-generated by install scripts and not managed centrally.
Changes: Added .config/git/config-work to global gitignore. dotfiles-private now owns config.local (OS-specific settings + includeIf for ~/work/**) and config-work (work email). install/win/dotfileslink.ps1 added to dotfiles-private to symlink both files into dotfiles/.config/git/.

### git: remove private emails from history and guard against ~/.gitconfig override (2026-04-11, (pending))
Background: Private email addresses (personal and old work accounts, plus local-hostname-based addresses) were present in ~600 commits of the public dotfiles repo. Root cause: ~/.gitconfig on the Windows machine contained a private email, which takes precedence over the dotfiles-managed ~/.config/git/config (XDG). Git reads ~/.gitconfig after XDG config, so XDG values are silently overridden even when the symlink is correctly set up.
Changes: Deleted ~/.gitconfig on the affected machine. Rewrote all commit history with git filter-repo to replace private emails with the GitHub no-reply address. Force-pushed to origin. Added interactive prompt to install-obsolete.ps1 (Windows) and install-obsolete.sh (Linux) to detect and offer deletion of ~/.gitconfig when it exists as a regular file.

### README.md added to doc management framework (2026-04-11, (pending))
Background: docs-convention.md Standard Files only covered internal docs (architecture.md, todo.md, history.md, ops.md, infrastructure.md). Public repos lacked guidance on maintaining README.md for external users. /update-docs skill mentioned README.md only conditionally ("if file tree or installation procedure changed").
Changes: Added README.md to Standard Files table (role: public-facing entry point, size: compact, when: public repos). Added content rules: external-facing overview, delegate internals to other docs, source repo root for ai-specs projects. Promoted README.md to always-target in /update-docs General projects; added source repo root target to ai-specs projects.

### Language policy: move rules to dotfiles-private (2026-04-11, (pending))
Background: Language-related rules (code comments in English, public docs in English, private docs in Japanese) were hardcoded in the public dotfiles repo. This prevented English-speaking users from forking the repo without inheriting Japanese-specific preferences.
Changes: Removed language rules from `claude-global/rules/coding.md` and `docs-convention.md`. Created `dotfiles-private/claude-global/rules/language.md` (conversation, commit, code, docs language policy). Added gitignore entry and symlink support in both install scripts (win/linux). `install.ps1` and `install.sh` now auto-call `dotfiles-private/install.ps1`/`install.sh` when present, passing the same flags.

### Rize installer: move to dotfiles-private (2026-04-11, (pending))
Background: Rize is a personal preference app, not a general dotfiles tool. Keeping its installer in the public repo exposes an opinionated choice that forkable dotfiles shouldn't impose.
Changes: Moved `install/win/rize.ps1` and `install/linux/rize.sh` to `dotfiles-private/install/`. Removed Rize steps from `install.ps1` (Step 8) and `install.sh` (Step 7), renumbered subsequent steps. Added Rize steps to `dotfiles-private/install.ps1` (Step 3, under `-Base`/`-Full`) and `dotfiles-private/install.sh` (Step 2, under `--base`/`--full`). Moved tests (`main-rize.Tests.ps1`, `main-rize.sh`) to `dotfiles-private/tests/`. Updated `docs/architecture.md` (removed file table entries and execution order references).

### commit-push: delegate missing-test case to write-tests skill (2026-04-11, (pending))
Background: When /commit-push detected missing tests, Claude was writing test files directly in the main conversation instead of via the write-tests subagent. This bypassed acceptEdits mode, caused VSCode "Make this edit" dialogs to appear, and allowed source file edits that the write-tests rules prohibit.
Changes: Added "Pre-commit check" section to commit-push/SKILL.md instructing Claude to never write tests directly in the main conversation — invoke /write-tests first, then resume commit-push.

### session-sync reset: pipefail early-exit fix (2026-04-11, (pending))
Background: `reset` (and `pull`) exited before "Reset to remote state." due to two `set -euo pipefail` pitfalls: (1) `ts=$(... | grep | ...)` in the mtime loop exits 1 when grep finds no match in JSONL files lacking a timestamp field — primary early-exit cause; (2) `cat .history.jsonl history.jsonl` exits 1 when `history.jsonl` is absent on a fresh follower machine. `2>/dev/null` suppresses stderr but not the exit code. The outer test script also had unguarded reset calls, masking failures as test-suite aborts.
Changes: Added `|| true` to both `ts=$(...)` lines in the mtime restoration loop. Added `[ -f history.jsonl ]` existence check before `cat` in both `reset` and `pull`. Added `|| true` guards to 4 reset calls in `tests/main-session-sync.sh`. Added test "[reset] Reset succeeds when local history.jsonl is absent". All 37 tests PASS.

### session-sync: macOS toast notification (2026-04-11, 2882723)
Background: Toast notification on push/pull completion existed for Windows (PowerShell) and Linux (notify-send) but not macOS.
Changes: Added osascript branch to _toast() in session-sync.sh, placed between the Windows and Linux paths.

### dotfiles-private: startup fetch in .profile_common (2026-04-11, pending)
Background: dotfiles-private is a sibling git repo managed alongside dotfiles, but had no automatic pull on shell startup. Separately, "model": "sonnet" was added as a default model to claude-global/settings.json.
Changes: Added dotfiles-private fetch block to .profile_common, after the dotfiles fetch and before the session-sync fetch. Added "model": "sonnet" to claude-global/settings.json.

### pre-commit: auto-unstage model field in settings.json (2026-04-11, pending)
Background: Claude Code writes "model" to settings.json dynamically (same as effortLevel), causing it to appear in git diff across sessions and prompting unnecessary commit confirmations.
Changes: Added `delete j.model` to STRIP_EFFORT in pre-commit hook alongside `delete j.effortLevel`. Added 5 test cases to tests/main-effort-level-unstage.sh (model addition, value change, removal, combined effortLevel+model, mixed with real change). Fixed pre-existing test bug: setup_repo now sets `core.hooksPath /dev/null` to prevent global hook from interfering with intermediate test commits.

### session-sync: notification timing and quiet-mode suppression (2026-04-11, pending)
Background: Two notification issues in session-sync: (1) WARNING message was printed even in --quiet mode (auto-runs), polluting terminal output. (2) "push complete" notification appeared even when VS Code was still open — specifically when closing one of multiple simultaneously open windows, or because the VS Code process lingers after window close, causing a process-based check (_vscode_running) to give false positives.
Changes: Added --quiet guard to WARNING in session-sync.sh. Replaced process-based _vscode_running check with a --toast opt-in flag. Added _any_vscode_window() to .profile_common (osascript on macOS, xdotool/wmctrl on Linux) that checks open windows, not processes. codes() now checks for remaining VS Code windows before push and only passes --toast when none are open. Updated tests/main-session-sync.sh.

### check-private-info: allow Docker internal network and Node.js base image paths (2026-04-11, (pending))
Background: judgeclaw's docker-compose.yml uses Docker's default bridge network (172.24.0.x) and the official Node.js base image home directory (/home/node). The former matched the RFC 1918 private address pattern; the latter resembled a real user home path — both caused false positives in the private-info scanner.
Changes: Added two scoped entries to `.private-info-allowlist`: `docker-compose.yml:172\.24\.0` (Docker internal network, not routable) and `docker-compose.yml:/home/node` + `*/generate-web-access-section.sh:/home/node` (Node.js base image path, not a real user). Also fixed the path-capture regex in `bin/check-private-info.sh` from `/home/[a-zA-Z]` (one char) to `/home/[a-zA-Z][a-zA-Z0-9_.-]*` so the full username component is captured and allowlist entries can match precisely.

### write-tests subagent: mode acceptEdits → default (2026-04-11, (pending))
Background: write-tests subagent was launched with mode: "acceptEdits" to bypass VSCode's "Ask before edits" dialogs for tests/ files. After settings.json was updated with Write(**/tests/**) and Edit(**/tests/**) allow rules, acceptEdits became unnecessarily broad — it bypassed all file permission checks, allowing the subagent to silently edit any file including source code. This was confirmed when a test subagent modified bin/session-sync.sh and settings.json without authorization.
Changes: Changed subagent launch mode in claude-global/skills/write-tests/SKILL.md from "acceptEdits" to "default". The existing allow rules in settings.json continue to auto-approve test file writes without confirmation dialogs.

### session-sync push: retry loop for simultaneous-commit race condition (2026-04-11, (pending))
Background: When Windows (PENPEN-11H) and macOS (mbp-m4pro-nire) push at the same time, both machines commit session data independently, creating a diverged state. The original code ran git pull --rebase once before push, but if Claude Code wrote new session data to a .jsonl file after the initial git add/commit but before pull --rebase, the pull failed with "unstaged changes" — leaving push to fail and trigger the "push failed" toast.
Changes: Replaced the single pull+push block in bin/session-sync.ps1 and bin/session-sync.sh with a retry loop (max 3 iterations). Each iteration commits any newly-written session files, rebases over remote, and retries push. Also added `:` no-op at end of quiet-mode success path in session-sync.sh to ensure exit code 0. Added two test cases per platform: static check for retry loop presence, and functional test verifying recovery from pre-diverged state with unstaged changes. Also suppressed pull --rebase stdout inside the retry loop (was only suppressing stderr) to match Windows behavior of silent quiet-mode operation. Added two quiet-mode stdout/stderr separation tests.

### Workflow state machine: deterministic 8-step enforcement (2026-04-12, (pending))
Background: CLAUDE.md workflow instructions were text-based and relied on Claude's judgment, making step skipping undetectable. check-tests-updated.js and check-docs-updated.js enforced single concerns independently but were not unified. Goal was to replace both with a single state-machine-based gate.
Changes: Created workflow state machine (lib/workflow-state.js shared module, workflow-gate.js PreToolUse commit gate replacing check-tests-updated.js and check-docs-updated.js, mark-step.js step completion CLI with --reset-from bypass, session-start.js SessionStart hook). Fail-safe design: missing or corrupted state blocks commits. Added Completion sections to 5 global skill SKILL.md files (survey-code, deep-research, make-plan, write-tests, update-docs). 36 tests in tests/feature-robust-workflow.sh.

### #21: Hook integration path untested — CLAUDE_ENV_FILE format bug (2026-04-12)
Cause: session-start.js wrote `export CLAUDE_SESSION_ID=...` to CLAUDE_ENV_FILE, but Claude Code's env-file parser expects `KEY=VALUE` format (no `export` prefix). Test 30 used `grep -q` (substring match), so `export KEY=VALUE` passed despite being wrong. Root cause: tests only validated the JS script in isolation — the handoff point from script output to Claude Code's environment injection was never verified. In hook-based systems, the correctness of output format is only meaningful relative to the consuming system's spec, which unit tests cannot cover. More broadly, when writing tests, only single-file normal/error/edge cases were considered; integration paths across multiple components (hook → JS → env injection → Bash tool) were not enumerated as test targets.
Fix: Removed `export` prefix in session-start.js. Tightened test 30 to use `grep -qx` (exact line match). Follow-up investigation confirmed CLAUDE_ENV_FILE path is not propagated to Bash tool subprocesses (tested on Windows; macOS/Linux behavior unconfirmed).

### Workflow State Machine: mark-step.js session ID resolution redesign (2026-04-12)
Background: Empirical testing (Windows) showed that both CLAUDE_ENV_FILE path and CLAUDE_SESSION_ID are absent from Bash tool subprocess environment. The CLI design of passing session ID as an argument ($CLAUDE_SESSION_ID) was broken — always expanded to empty string. CLAUDE_ENV_FILE is available in hook contexts (SessionStart, PreToolUse, etc.) via process.env, but not in Bash subprocesses.
Changes: Removed session ID CLI argument from mark-step.js. Added resolveSessionId() to lib/workflow-state.js, reading CLAUDE_ENV_FILE from process.env to extract CLAUDE_SESSION_ID. workflow-gate.js usage messages updated. 41 tests pass.

### Workflow State Machine: E2E verification + WSL path bug fix (2026-04-13)
Background: Manual E2E verification of the workflow state machine in a real Claude Code session after implementation was complete.
Changes: Confirmed all six test scenarios — normal cases 1–4 (SessionStart writes CLAUDE_SESSION_ID, mark-step.js records steps, git commit blocked on incomplete steps, git commit passes when all steps complete) and error cases 2–3 (--reset-from recovery from corrupted state file, partial reset from mid-workflow step). During verification, found and fixed a bug: on Windows, the hook resolved the WSL-style drive path extracted from the git -C flag using Windows Node.js, causing git rev-parse to fail with ENOENT and the gate to report "state not found" instead of listing incomplete steps. Fixed by adding toNativePath() to is-private-repo.js (converts bash-style drive paths to Windows paths on win32) and wiring it into resolveRepoDir. Added tests 42–49 to the test suite (all 49 tests pass).

### PostToolUse marker interception: fix mark-step.js in Bash subprocess context (2026-04-13)
Background: CLAUDE_ENV_FILE is not propagated to Bash tool subprocesses (Anthropic bug #27987). mark-step.js called from skill Completion sections via Bash always failed to resolve session_id, silently leaving workflow steps unrecorded.
Changes: Added workflow-mark.js (PostToolUse Bash hook) that intercepts `echo "<<WORKFLOW_MARK_STEP:step:status>>"` via strict anchored regex on tool_input.command — rejects pipes, &&, cat/grep output containing the marker, etc. Hook reads session_id directly from hook stdin JSON and uses CLAUDE_PROJECT_DIR (documented env var, available in all hook types) for repo resolution. HOOK_CWD was excluded as undocumented with no confirmed usage evidence. Updated 5 skill Completion sections (survey-code, deep-research, make-plan, write-tests, update-docs). user_verification remains CLI-only to preserve the ask-rule gate. Added 26 test cases (normal, must-not-mark strict regex, error/edge, idempotency). Key finding: additionalContext surfaces messages to Claude; systemMessage reaches users only.

### #22: settings.json PostToolUse misplacement (2026-04-13)
Cause: commit 01a9110 placed the PostToolUse hook inside the `permissions` block instead of the `hooks` block. Unit tests (direct JS invocation) pass regardless because they call the hook handler directly — they cannot detect whether Claude Code will ever invoke the hook at all.
Fix: Moved PostToolUse registration to the `hooks` block. Added SR1-SR4 structural assertions to the test suite to automatically detect misplacement. Added E2E test E1 (`claude -p` real invocation with `--setting-sources project`) confirming the hook fires and records the step in the state file. Added "Test Layer Selection" section to `claude-global/rules/test.md` documenting that wiring bugs (config misplacement, hook registration errors) require integration/E2E tests — not just unit tests (based on Fowler and Kent C. Dodds research).

### Workflow State Machine: PostToolUse Windows E2E verified + CLAUDE_PROJECT_DIR migration confirmed (2026-04-13)
Background: Windows E2E smoke test for the PostToolUse hook and simultaneous verification that the HOOK_CWD → CLAUDE_PROJECT_DIR migration (commit 264ab0a) works correctly in a real session.
Changes: E1 E2E test (`claude -p --setting-sources project --session-id`) passed: PostToolUse hook fires in a real Claude session, `CLAUDE_PROJECT_DIR` resolves the repo correctly, and the step is written to the state file. All tests pass with `RUN_E2E=1`. macOS/Linux verification remains pending.

### Workflow State Machine: Robust Improvements — Windows verification (2026-04-13)
Background: User verification of three robustness improvements on Windows (VSCode extension).
Changes: (1) `echo "<<WORKFLOW_USER_VERIFIED>>"` ask dialog confirmed — permissions.ask rule fires and PostToolUse hook records user_verification as complete. (2) SessionStart creates UUID-format state file at session start (.git/workflow/<session-id>.json confirmed). (3) isPrivateRepo bypass removal confirmed via git diff — a6ad6ab had `if (isPrivateRepo(repoDir)) approve()`, current code imports only resolveRepoDir.

### Workflow State Machine: .git/workflow deny rule verified (2026-04-13)
Background: New session (VSCode extension, Windows) で deny ルールが機能するか確認。settings.json の `Edit(**/.git/workflow/**)` / `Write(**/.git/workflow/**)` deny ルールは Claude Code 再起動後に有効になる。
Changes: Write(**/.git/workflow/**) と Edit(**/.git/workflow/**) の両ルールが Claude Code のパーミッションシステムにより自動ブロックされることを確認。Windows での全動作確認完了。

### starship: increase command_timeout to 2000ms (2026-04-13)
Background: starship prompt showed timeout warnings when changing into the dotfiles directory. Root cause: git fsmonitor daemon was not yet running, and daemon startup exceeded starship's default 500ms command_timeout.
Changes: Set `command_timeout = 2000` in `.config/starship.toml`.

### Fix wait-vscode-window.sh: add WSL2 window detection (2026-04-13, pending)
Background: In WSL2, VS Code runs as a Windows process so xdotool/wmctrl (X11 tools) are unavailable. The `codes` function called wait-vscode-window.sh which fell through to the "No window detection tool found" warning and skipped session sync.
Changes: Added WSL2 branch to bin/wait-vscode-window.sh that uses powershell.exe to enumerate Code.exe MainWindowTitle values via [System.Diagnostics.Process]::GetProcessesByName. Detects WSL via WSL_DISTRO_NAME env var. Updated tests/main-wait-vscode-window.sh to cover WSL_DISTRO_NAME detection. Updated docs/architecture.md.

### Fix QNAP dotfileslink.sh: guard autorun.sh install against mount failure (2026-04-13)
Background: On QNAP, the first run of install.sh printed "autorun.sh installed." even though the mount failed — /tmp/config directory did not exist, causing sudo mount to fail silently. The script had no set -e, so errors were ignored.
Changes: Added mkdir -p /tmp/config before mounting. Wrapped mount in an if block; success message and subsequent cp/chmod/umount only execute when mount succeeds. Added WARNING message on failure.

### Workflow State Machine: marker format `:` → `_` (2026-04-13)
Background: Claude Code's permission glob parser treats `:` as a named-parameter separator inside `Bash(...)` rules, causing silent match failure (confirmed via anthropics/claude-code#33601). `WORKFLOW_RESET_FROM` ask rule was not triggering a dialog; investigation showed `Bash(echo "<<WORKFLOW_USER_VERIFIED>>")` (no colon) worked while any pattern containing `:` silently failed to match. Same issue applied to `WORKFLOW_MARK_STEP` allow rules. Default behavior when no rule matches is auto-allow, masking the problem.
Changes: Changed both marker formats to use `_` as separator: `<<WORKFLOW_MARK_STEP:step:status>>` → `<<WORKFLOW_MARK_STEP_step_status>>`, `<<WORKFLOW_RESET_FROM:step>>` → `<<WORKFLOW_RESET_FROM_step>>`. Updated workflow-mark.js regexes, settings.json allow/ask rules (RESET_FROM single-quote variant removed — DQ only), workflow-gate.js block message, 5 skill Completion sections, and test suite. Windows re-verification confirmed: normal cases 1–4 and error cases 1–2 all pass.

### Workflow State Machine: cross-platform verification complete (2026-04-14, 60aa84c)
Background: Windows・WSL・macOS の全プラットフォームで正常系・異常系の動作確認を完了。macOS は初のネイティブ E2E 実行。
Changes: 正常系（セッション開始・ステップ記録・commit ブロック・commit 通過・PostToolUse 実発火）と異常系（ステートファイル破損 → fail-safe ブロック → --reset-from リカバリ、部分リセット）をすべて確認。macOS E2E で3つの移植性バグを発見・修正：(1) `timeout` 非対応 → `run_with_timeout()` perl フォールバック、(2) `CLAUDECODE` 継承によるネストセッションエラー → `unset CLAUDECODE`、(3) `disableBypassPermissionsMode: disable` が `--dangerously-skip-permissions` を無効化 → minimal settings.json 使用。WSL は Windows ブリッジ経由のため3つとも顕在化しなかった（`CLAUDECODE` 非伝播・Windows 側プロファイル参照）。test.md に `run_with_timeout` パターンと `claude -p` E2E 注意点を追記、Installer Testing を test-installer.md に分離。

### #23: workflow-gate block message as bypass attractor (2026-04-14)
Cause: workflow-gate.js:113-117 included a literal `echo "<<WORKFLOW_RESET_FROM_<step>>>"` recipe
in its "commit blocked" error message. When a skill's test setup ran `git commit` in a temp
repository, workflow-gate blocked it and the model read the hint — triggering autonomous
WORKFLOW_RESET_FROM calls to clear the gate. The model lacked holistic project context to judge
whether a reset was warranted; from inside a skill the hint looked like a legitimate shortcut.
Root cause is structural: the mechanism that should be a last resort was advertised in an error
message visible to subagents that cannot evaluate the full situation.

Fix: Removed the RESET_FROM hint from workflow-gate.js entirely (the block message now lists only
incomplete steps and the skill to run for each). Added a "Workflow State Recovery" section to
CLAUDE.md framing WORKFLOW_RESET_FROM as a last resort that "only the main conversation can use
when it has enough holistic context to judge that a reset is genuinely warranted." The settings.json
`ask` guard remains as a backstop. The RESET_FROM mechanism itself (workflow-mark.js PostToolUse
interception, required because CLAUDE_ENV_FILE does not propagate to Bash subprocesses per
Anthropic bug #27987) is unchanged.

Discussion notes: Several alternative fixes were considered and rejected.
(1) Obfuscating the hint — rejected because it merely makes bypass harder to find, not
structurally prevented.
(2) Moving documentation to ops.md only — would have required the user to manually run
mark-step.js even in cases where the main conversation can judge correctly.
(3) Manager subagent — rejected as overskill; the main conversation already has full context
and IS the manager.
The key insight: the right place for reset authority is the entity with holistic project context,
which is the main conversation (or the user directly). Skills and subagents must not reset.
