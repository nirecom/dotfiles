# History (dotfiles/legacy)

### FEATURE: Initial setup (a112597–7419e8d)
Background: Manage dotfiles on GitHub
Changes: Added `.bashrc`, `.vimrc`, `.editorconfig`, `.gitconfig`


### FEATURE: Zsh migration (cdcd088–f2e7309)
Background: Switch to Zsh as primary shell
Changes: `.zshrc` + Zinit, `.profile_common` separation, git-prompt migration


### FEATURE: Git config relocation (ce8aa0a–f16c281)
Background: Move `.gitconfig` to XDG-compliant location
Changes: Migrated to `.config/git/config` + `ignore`, separated `config.local`


### FEATURE: Emacs enhancement (f835f5a–d45bdd8)
Background: Modularize Emacs config
Changes: init-loader pattern, use-package, LSP, Ivy/Counsel


### REFACTOR: OS detection refactor (acb488b–e6aa321)
Background: Unify OS detection logic
Changes: Created `bin/detectos.sh`, rewrote `.bash_profile` to use `case` statements


### FEATURE: Apple Silicon support (1ff488d–d72a3b5)
Background: Support M1/M2 Macs
Changes: Added `$ISM1`, updated Homebrew/fnm paths for Apple Silicon


### FEATURE: Repository merge (b436d88–7abe347)
Background: Merge install repo into dotfiles
Changes: Subtree merge into `install/linux/`, unified `install.sh` entry point. The separate [nirecom/install](https://github.com/nirecom/install) repository is no longer needed


### FEATURE: Windows support (8281602–e51b61b)
Background: Use dotfiles on Windows
Changes: `install/win/dotfileslink.ps1`, Developer Mode check, PowerShell 5 compatibility


### INCIDENT: #1: `dotfileslink.sh` aborts with `set -e` (b3ea03f)
Cause: Some commands return non-zero exit
Fix: Fixed `set -e` compatibility


### INCIDENT: #2: Symlink creation fails on PowerShell 5 (06f6640, be85569)
Cause: PS5 has a different Developer Mode check API
Fix: Switched to direct registry check


### FEATURE: Starship introduction (f43b6d9, 25b7147)
Background: Add Starship prompt
Changes: Linux Zsh + Windows PowerShell configurations


### FEATURE: PowerShell curl issue (60c48fb, eb9d164)
Background: curl doesn't work in PowerShell
Changes: Added `curl.exe` rule and single-quote rule to CLAUDE.md


### FEATURE: QNAP vim plugins (c3754ce)
Background: vim errors on startup in QNAP
Changes: Added pathogen + solarized plugin installation to `dotfileslink.sh`


### FEATURE: QNAP support (19f488d–f2d62a5)
Background: Use dotfiles on QNAP NAS
Changes: OS detection added, minimal symlinks, autorun.sh, sh→bash auto-switch


### INCIDENT: #4: `arch: command not found` on QNAP (ed56971)
Cause: `arch` is macOS-only; not in BusyBox
Fix: Replaced with `uname -m`


### INCIDENT: #5: No colored prompt on QNAP SSH (7e780cc)
Cause: Default shell `/bin/sh` doesn't read `.bash_profile`
Fix: `.profile_qnap` runs `exec bash -l`


### INCIDENT: #6: `.profile_qnap` exec fails (d38ee0a)
Cause: Bash path hardcoded (`/opt/bin/bash`)
Fix: Changed to `command -v bash` for dynamic resolution


### INCIDENT: #7: `git: command not found` on QNAP (584e6dc)
Cause: Entware `/opt/bin` not in PATH
Fix: Added Entware PATH to `.profile_common`


### INCIDENT: #8: Cursor position misaligned on QNAP (f2d62a5)
Cause: Entware bash doesn't handle `\[...\]` correctly
Fix: Used `\001`/`\002` (interim) → removed in #10 after root cause fix


### BUGFIX: QNAP Entware fix (7533ba7–401097e)
Background: git disappears after reboot
Changes: Entware QPKG activation, autorun.sh filename fix, flash auto-deployment


### BUGFIX: QNAP prompt wrapping fix (107b3a6–1ae045e)
Background: Long input wraps incorrectly in QNAP bash
Changes: terminfo auto-install, `TERMINFO` env var, PS1 unification (`\001`/`\002` → `\[`/`\]`), TERM fallback


### INCIDENT: #9: git disappears after QNAP reboot (7533ba7, 401097e)
Cause: Entware QPKG disabled (`Enable != TRUE`), startup script name wrong
Fix: `setcfg Enable TRUE` + fixed to `Entware.sh start`


### INCIDENT: #10: Long input overwrites prompt on QNAP bash (107b3a6–1ae045e)
Cause: terminfo not installed + `TERMINFO` not set → readline can't detect auto-margin
Fix: Auto-install terminfo, set `TERMINFO=/opt/share/terminfo`, unified PS1 to standard `\[`/`\]`


### INCIDENT: #11: `E117: Unknown function: pathogen#infect` on QNAP vim (c3754ce)
Cause: `.vimrc` symlinked but pathogen/plugins not installed
Fix: Added vim plugin setup to `dotfileslink.sh`


### FEATURE: Stale symlink detection (Windows) (0c83762)
Background: dotfiles moved from `~/git/dotfiles` to `~/dotfiles`; old symlinks remained and caused broken configs
Changes: `dotfileslink.ps1` now compares symlink target against `$DotfilesDir` and relinks automatically if mismatched


### INCIDENT: #12: No colored prompt on Windows PowerShell (one PC only) (0c83762)
Cause: (1) Starship not installed — profile silently skips init. (2) Starship config symlinks pointed to old `~/git/dotfiles` location after dotfiles were moved
Fix: Installed Starship via `install/win/starship.ps1`; re-ran `dotfileslink.ps1` as admin to fix stale symlinks


### FEATURE: AutoHotkey integration (3ca2dcf, 1c905ec)
Background: Force Japanese keyboard layout via AHK when Windows UI is English + Japanese preferred. Added `config/win/autohotkey/force-japanese-layout.ahk`, `install/win/autohotkey.ps1` (language detection + winget install + startup shortcut), `install/win/install-obsolete.ps1` (auto-cleanup of old OneDrive AHK files and startup shortcuts)
Changes: AHK script moved from OneDrive to dotfiles repo. Install script detects English UI + Japanese preferred language (strict), installs AHK v2 via winget, creates startup shortcut. Obsolete script auto-detects old AHK shortcuts by TargetPath pattern (excludes dotfiles repo paths for idempotency) and old OneDrive AHK files by glob. `install-obsolete.ps1` runs always (not only `-Full`), matching Linux `install.sh` pattern. Per-user install path fallback added (1a15c1f)


### FEATURE: Starship install guard (c7f02ea, 1766674)
Background: `install.ps1 -Full` tried to reinstall Starship even when already installed — `Get-Command` missed binaries not yet in PATH
Changes: Switched to `winget list --id Starship.Starship` for definitive install detection, matching AutoHotkey's `winget list` pattern


### INCIDENT: #14: `.config/git/config` always dirty after shell startup (dd787a4)
Cause: `profile.ps1` ran `git config --global core.sshCommand` on every PowerShell startup, appending `[core] sshCommand` directly to tracked `.config/git/config`. The setting was already provided via `config.local` (generated by `dotfileslink.ps1`) and included via `[include] path = config.local`
Fix: Removed redundant sshCommand block from `profile.ps1`; setting remains via `config.local` include


### FEATURE: fnm install script (267d793, d2dadcc)
Background: Add fnm to Windows installer
Changes: `install/win/fnm.ps1` with winget install, `profile.ps1` defensive init with try/catch for SAC


### BUGFIX: Starship git timeout fix (4024b77)
Background: Starship showed `git.exe timed out` warning on Windows PowerShell startup — NTFS overhead makes `git status` exceed default 500ms timeout
Changes: (1) Enabled Git built-in FSMonitor (`core.fsmonitor = true`) and `core.untrackedCache = true` in Windows `config.local` for root-cause fix, (2) increased Starship `command_timeout` to 1000ms in `starship-powershell.toml` as safety net for FSMonitor cold start, (3) split `git pull` into `git fetch` (3s timeout) + `git merge --ff-only` to avoid corrupting working tree on kill


### FEATURE: uv install scripts (85f3d7f)
Background: Add uv (Python package manager) to dotfiles
Changes: Added `install/win/uv.ps1` (PowerShell installer) and `install/linux/uv.sh` (curl installer), integrated into `install.ps1 -Full`. uv installs to `~/.local/bin` on all platforms (Win/Mac/Linux) — already in PATH via `.profile_common` and `profile.ps1`. No shell init (unlike fnm) needed


### FEATURE: Windows notification muting (45ca237)
Background: Mute toast/system notification sounds
Changes: `sounds.ps1` sets registry to empty for 3 sound events; runs always (not only `-Full`)


### REFACTOR: claude-code → claude-global rename (597bf88–f83e98c)
Background: Rename directory to avoid project-level conflict
Changes: Renamed `claude-code/` to `claude-global/`, migration logic in install scripts, handles empty dir left by git, `-Recurse` fix for non-empty directory


### BUGFIX: uv install PATH fix (d82c026)
Background: `uv --version` fails on fresh install
Changes: Refresh `$env:Path` (Win) / `$PATH` (Linux) after uv install so version check succeeds


### INCIDENT: #15: `uv --version` fails after fresh install (d82c026)
Cause: uv installer adds `~/.local/bin` to system PATH but current shell session is not refreshed
Fix: Add `~/.local/bin` to session PATH before calling `uv --version`


### FEATURE: Profile hardening (Windows) (a64aca9)
Background: SSH key loading and migration symlink fail on some machines
Changes: SSH key loading now globs `$HOME\.ssh\id_*` instead of hardcoded `id_ed25519`/`id_rsa`. Migration symlink (claude-code → claude-global) checks Developer Mode / admin before creating. PS5 `New-Item` wrapped in try/catch for permission errors. Added Pester tests


### INCIDENT: #16: `New-Item -ItemType SymbolicLink` error on PS5 startup (a64aca9)
Cause: `profile.ps1` migration block called `New-Item` without permission check. PS5 requires admin for symlinks even with Developer Mode enabled (unlike pwsh which supports `SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE`)
Fix: Added Developer Mode / admin pre-check (a64aca9), then wrapped `New-Item` in try/catch with `-ErrorAction Stop` for defense in depth


### FEATURE: Windows symlink auto-repair (08141cf, 9ab6b54)
Background: `install.ps1` warns "Exists (not a symlink)" for 4 files — Windows atomic save (editors, Claude Code/Node.js) silently replaces symlinks with regular files
Changes: `dotfileslink.ps1`: removed file-skip logic, unified file/directory backup-and-relink. `profile.ps1`: added startup detection of broken symlinks (~20ms) with auto-repair via `dotfileslink.ps1`. Extracted test rules from `workflow.md` into dedicated `rules/test.md` with edge case category, added test-first reminder to global `CLAUDE.md`. Fixed Pester 5 `-Skip` scoping bug (script-level detection needed for discovery-time evaluation)


### REFACTOR: master → main cleanup (07045be)
Background: Remove temporary migration code after all PCs migrated
Changes: Deleted `BEGIN temporary: main branch upstream tracking fix` blocks from `.profile_common` and `install/win/profile.ps1`


### FEATURE: PowerToys Keyboard Manager (b453ee8)
Background: Manage PowerToys Keyboard Manager (Emacs-style shortcuts) via dotfiles
Changes: Added `install/win/powertoys.ps1` (winget install + Keyboard Manager config deploy), `config/win/powertoys/keyboard-manager/default.json` (Emacs keybindings: Ctrl+A/B/D/E/F/N/P → navigation, Alt+A/B/F → original Ctrl shortcuts). Uses file copy (not symlink) because PowerToys may not follow symlinks. Idempotent: skips if config identical, backs up `.bak` if different


### FEATURE: VS 2022 C++ dev tools (1982d8e)
Background: `install.ps1 -Develop` for llama.cpp compilation
Changes: VS 2022 + CMake needed to build llama-server (llama.cpp) from source. Added `install/win/vs-cpp.ps1` (VS 2022 Community + NativeDesktop workload via bootstrapper, vswhere idempotency, UAC auto-elevation with decline handling). Added to `-Develop`/`-Full` block in `install.ps1`. Pester tests added


### INCIDENT: #18: install.sh doesn't reach Rize / Claude Usage Widget steps
Cause: `exec $SHELL -l` in `source-highlight.sh` replaced the `install-base.sh` subprocess, preventing control from returning to `install.sh`. Additionally, `rize.sh` / `claude-usage-widget.sh` lacked execute permissions, DMG mount volume path parsing broke on volume names with spaces, and `.app` filenames were hardcoded instead of matching actual names.
Fix: Moved `exec $SHELL -l` from `source-highlight.sh` to end of `install.sh`. Added execute permissions. Dynamically detect `.app` in DMG via `find`. Removed macOS special-casing (`$OSDIST = "macos"` always running base) from `install.sh` to unify with `install.ps1` orthogonality (`--base`/`--full` flags required).


### FEATURE: dotfiles clone recovery after git rebase ()
Background: After git rebase, dotfiles clones on each PC diverged from origin/main.
Changes: Ran `git fetch origin && git reset --hard origin/main` on all PCs. Verified complete.


### FEATURE: Claude Tabs installer ((pending))
Background: Claude Tabs (Tauri v2) is a Windows-native Claude Code multi-session management app. Tab UI + Activity Feed for real-time agent status display.
Changes: Created `install/win/claude-tabs.ps1` (fetches latest release via GitHub API, /S silent install). Falls back to scanning the 10 most recent releases when `/releases/latest` has no attached assets. Created `.cross-platform-skiplist` (hook exclusion list for Windows-only tools).


### REFACTOR: Node.js version manager: platform split (fnm → nvm on Unix) (1b74132, (pending))
Background: NemoClaw official installer unconditionally installs nvm. Conflicts with dotfiles' "fnm everywhere" rule — npm install fails when prek refuses to install hooks with core.hooksPath set. fnm has no advantage over nvm on Unix; nvm is the ecosystem standard. Windows needs fnm (nvm has no Windows support)
Changes: New rule: Windows=fnm, WSL2/macOS/Linux=nvm. Replaced `install/linux/fnm.sh` with `nvm.sh`. Removed fnm from `.profile_common` PATH and init (nvm init already existed at lines 190-206). Added fnm cleanup to `install-obsolete.sh`. Updated `coding.md` rule to platform-specific. Fixed nvm.sh execute permission (1b74132)


### FEATURE: Keychain SSH key auto-detect (6b39058)
Background: SSH key specification for keychain was hardcoded.
Changes: Promoted keychain step in `install.sh` to run by default. `.profile_common` now auto-detects keys by globbing `~/.ssh/id_*`. Created `install/linux/keychain.sh`.


### REFACTOR: ~/dotfiles → C:\git\dotfiles path unification ((pending))
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


### FEATURE: AutoHotkey per-user path fallback (1a15c1f, 29bfd8a)
Background: AutoHotkey v2 per-user installs (`%LOCALAPPDATA%\Programs\`) were not found because only `%ProgramFiles%` was searched
Changes: Added per-user install path fallback to `autohotkey.ps1`. Also fixed shortcut argument update bug and path matching bug


### FEATURE: Add VS Code and extensions installer (aa3f295)
Background: Setting up a new development machine required manually installing VS Code and re-adding extensions one by one. Needed automated install via `-Develop`/`--develop` flag.
Changes: Created `config/vscode-extensions.txt` (shared extension list), `install/win/vscode.ps1` (winget-based), and `install/linux/vscode.sh` (apt/brew, WSL skip). Added to `-Develop`/`--develop` section in both `install.ps1` and `install.sh`.


### FEATURE: Force push divergence detection on shell startup (257958a)
Background: After git rebase, dotfiles clones on each PC diverged from origin/main. `merge --ff-only` failed silently, leaving no indication that a reset was needed. With 5 PCs, forgetting to reset one was likely.
Changes: Added divergence detection to `.profile_common` auto-fetch. When `merge --ff-only` fails and histories have diverged, prompts the user with y/N to reset (10s timeout, defaults to N). `~/.dotfiles-no-auto-reset` marker file suppresses the prompt on the master PC (warning only). Non-interactive shells skip silently.


### FEATURE: Add -Toolchain option and AWS CLI installer for Windows
Background: vs-cpp (Visual Studio C++ workload) is a heavy install that doesn't belong in the general `-Develop` tier. AWS CLI was available on Linux but missing from Windows.
Changes: Added `-Toolchain` parameter to `install.ps1` for heavy build toolchain installs (vs-cpp moved here from `-Develop`). Created `install/win/awscli.ps1` using winget, added to `-Develop`. `-Full` now includes Base + Develop + Toolchain.


### BUGFIX: Fix codes function to survive terminal close (781f512)
Background: `codes` alias uses `Start-Job` to push session sync after VS Code closes, but `Start-Job` is tied to the parent PowerShell session. Closing the terminal before VS Code was confirmed to sometimes kill the job before push ran.
Changes: Replaced `Start-Job` with `Start-Process pwsh -WindowStyle Hidden` so the push process is independent of the terminal lifecycle.


### BUGFIX: Fix codes multi-instance support
Background: Opening a second workspace with `codes` caused VS Code to reuse the existing window, making the first one disappear.
Changes: Added `--new-window` flag to `code --wait` (in `.profile_common` and `profile.ps1`). Each invocation now opens in an independent window.


### FEATURE: Remove container extensions from VS Code auto-install (0a3ba69)
Background: Dev Containers extension (`ms-vscode-remote.remote-containers`) silently installed Docker Desktop on macOS without user prompt. On company PCs, Docker Desktop Personal edition may violate commercial licensing terms.
Changes: Removed `ms-azuretools.vscode-containers` and `ms-vscode-remote.remote-containers` from `config/vscode-extensions.txt`. Users who need them can install manually.


### BUGFIX: Installer robustness and option hierarchy fix (uncommitted)
Background: On a secondary PC, `install.ps1 -Develop` failed: AWS CLI winget install hit MSI mutex (exit code 1618) but reported success; Python was missing because `-Develop` didn't include `-Base` packages; PowerShell profile lacked diverge detection (bash-only).
Changes: Added `Wait-MsiMutex` function to `install.ps1` (waits for running MSI before proceeding). Unified `$LASTEXITCODE` check after `winget install` in all 8 scripts. Added `uv python install` step to `uv.ps1`/`uv.sh` with `UV_NATIVE_TLS=1` for proxy environments. Ported force-push diverge detection from `.profile_common` to `profile.ps1` (y/N prompt, 10s timeout, marker file support). Changed option hierarchy to cumulative: `-Develop` includes `-Base`, `-Toolchain` includes `-Develop`. Extracted installer rules from `coding.md` to `installer.md`.


