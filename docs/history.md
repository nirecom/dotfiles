# Change and Incident History

## Change History

| Phase | User Request | Implementation | Key Commits |
|:---|:---|:---|:---|
| Initial setup | Manage dotfiles on GitHub | Added `.bashrc`, `.vimrc`, `.editorconfig`, `.gitconfig` | a112597–7419e8d |
| Emacs enhancement | Modularize Emacs config | init-loader pattern, use-package, LSP, Ivy/Counsel | f835f5a–d45bdd8 |
| Zsh migration | Switch to Zsh as primary shell | `.zshrc` + Zinit, `.profile_common` separation, git-prompt migration | cdcd088–f2e7309 |
| Git config relocation | Move `.gitconfig` to XDG-compliant location | Migrated to `.config/git/config` + `ignore`, separated `config.local` | ce8aa0a–f16c281 |
| OS detection refactor | Unify OS detection logic | Created `bin/detectos.sh`, rewrote `.bash_profile` to use `case` statements | acb488b–e6aa321 |
| Apple Silicon support | Support M1/M2 Macs | Added `$ISM1`, updated Homebrew/fnm paths for Apple Silicon | 1ff488d–d72a3b5 |
| Repository merge | Merge install repo into dotfiles | Subtree merge into `install/linux/`, unified `install.sh` entry point. The separate [nirecom/install](https://github.com/nirecom/install) repository is no longer needed | b436d88–7abe347 |
| Windows support | Use dotfiles on Windows | `install/win/dotfileslink.ps1`, Developer Mode check, PowerShell 5 compatibility | 8281602–e51b61b |
| Starship introduction | Add Starship prompt | Linux Zsh + Windows PowerShell configurations | f43b6d9, 25b7147 |
| Claude Code management | Centralize Claude Code config | `claude-code/` directory, install integration, security deny rules | 664d7ce–2e96bbf |
| QNAP support | Use dotfiles on QNAP NAS | OS detection added, minimal symlinks, autorun.sh, sh→bash auto-switch | 19f488d–f2d62a5 |
| QNAP Entware fix | git disappears after reboot | Entware QPKG activation, autorun.sh filename fix, flash auto-deployment | 7533ba7–401097e |
| QNAP prompt wrapping fix | Long input wraps incorrectly in QNAP bash | terminfo auto-install, `TERMINFO` env var, PS1 unification (`\001`/`\002` → `\[`/`\]`), TERM fallback | 107b3a6–1ae045e |
| PowerShell curl issue | curl doesn't work in PowerShell | Added `curl.exe` rule and single-quote rule to CLAUDE.md | 60c48fb, eb9d164 |
| Claude Code commands | Manage Claude Code commands in dotfiles | Added global commands directory symlink support | 8418c36 |
| QNAP vim plugins | vim errors on startup in QNAP | Added pathogen + solarized plugin installation to `dotfileslink.sh` | c3754ce |
| Claude Code skill management | Manage Claude Code skills (commands) in dotfiles, improve symlinks | Changed commands symlink from per-file to directory-level, added langchain/instruction update skills | 9ec0c0b, 359d929, e442685 |
| Claude Code security hardening | Review settings.json allow/deny rules. Cross-reviewed with ChatGPT: (1) adopt `git -C` as primary method (avoids compound commands), (2) `cd && git` fallback limited to status/diff/log minimum set, (3) wildcard prefix on deny rules (defense in depth), (4) curl/wget pipe deny acknowledged as glob-limited (future PreToolUse hook for strict enforcement) | Added `git -C` allow rules, `cd &&` fallback allow (minimal set), strengthened deny rule `*` prefixes, added `git -C` preference to CLAUDE.md, added local path rule to private information | 6e9eeb1, 18b0fd7 |
| Claude Code git write permissions | `git push` failed with permission denied — only read-only git commands were in allow list. Cross-reviewed with ChatGPT: (1) allow `git commit -m *` only (not broad `git commit *` which passes `--amend`/`--no-verify`), (2) allow `git push` / `git push origin *` only (not broad `git push *` which passes `--force-with-lease`/`--mirror`/`--delete`), (3) deny side strengthened with `--force-with-lease`, `--mirror`, `--delete`, `--amend`, `--no-verify` | Added git add/commit/push allow rules (minimal), added 7 deny rules for dangerous git options | TBD |
| Claude Code deny rule false-positive fix | `git add` auto-denied without prompt — deny rule `Bash(*dd *)` matched "add" substring. Cross-reviewed with ChatGPT: (1) root cause is deny `*dd *` not missing allow rules, (2) replaced with 4 specific patterns (`dd *`, `*&& dd *`, `*; dd *`, `*| dd *`), (3) HEREDOC commit format confirmed working with existing `git commit -m *` allow rule, (4) settings changes require session restart to take effect | c8095dc |
| Stale symlink detection (Windows) | dotfiles moved from `~/git/dotfiles` to `~/dotfiles`; old symlinks remained and caused broken configs | `dotfileslink.ps1` now compares symlink target against `$DotfilesDir` and relinks automatically if mismatched | 0c83762 |
| AutoHotkey integration | Force Japanese keyboard layout via AHK when Windows UI is English + Japanese preferred. Added `win/config/autohotkey/force-japanese-layout.ahk`, `install/win/autohotkey.ps1` (language detection + winget install + startup shortcut), `install/win/home-obsolete.ps1` (auto-cleanup of old OneDrive AHK files and startup shortcuts) | AHK script moved from OneDrive to dotfiles repo. Install script detects English UI + Japanese preferred language (strict), installs AHK v2 via winget, creates startup shortcut. Obsolete script auto-detects old AHK shortcuts by TargetPath pattern (excludes dotfiles repo paths for idempotency) and old OneDrive AHK files by glob. `home-obsolete.ps1` runs always (not only `-Full`), matching Linux `install.sh` pattern | TBD |
| Starship install guard | `install.ps1 -Full` tried to reinstall Starship even when already installed — `Get-Command` missed binaries not yet in PATH | Switched to `winget list --id Starship.Starship` for definitive install detection, matching AutoHotkey's `winget list` pattern | c7f02ea, 1766674 |
| Starship git timeout fix | Starship showed `git.exe timed out` warning on Windows PowerShell startup — NTFS overhead makes `git status` exceed default 500ms timeout | (1) Enabled Git built-in FSMonitor (`core.fsmonitor = true`) and `core.untrackedCache = true` in Windows `config.local` for root-cause fix, (2) increased Starship `command_timeout` to 1000ms in `starship-powershell.toml` as safety net for FSMonitor cold start, (3) split `git pull` into `git fetch` (3s timeout) + `git merge --ff-only` to avoid corrupting working tree on kill | 4024b77 |
| uv install scripts | Add uv (Python package manager) to dotfiles | Added `install/win/uv.ps1` (PowerShell installer) and `install/linux/uv.sh` (curl installer), integrated into `install.ps1 -Full`. uv installs to `~/.local/bin` on all platforms (Win/Mac/Linux) — already in PATH via `.profile_common` and `profile.ps1`. No shell init (unlike fnm) needed | 85f3d7f |
| Claude Code PreToolUse hook investigation | Investigated using PreToolUse hooks to force diff display before Edit (CLAUDE.md instruction sometimes ignored). Created node-based hook script outputting diff via stderr and `additionalContext`. **Result: neither method is visible to users in VS Code extension UI.** VS Code already shows a built-in diff in the Edit approval dialog, making the hook redundant. Hook removed. Findings: (1) PreToolUse hook stderr not shown in VS Code UI, (2) `additionalContext` not shown in VS Code UI, (3) `/dev/stdin` does not exist on Windows — use `fs.readSync(0, buf)` for cross-platform stdin, (4) VS Code Claude Code provides diff review via standard Edit approval dialog when Edit is not auto-allowed | No commit (reverted) |

---

## Incident History

| # | Incident | Cause | Fix | Commit |
|:---|:---|:---|:---|:---|
| 1 | `dotfileslink.sh` aborts with `set -e` | Some commands return non-zero exit | Fixed `set -e` compatibility | b3ea03f |
| 2 | Symlink creation fails on PowerShell 5 | PS5 has a different Developer Mode check API | Switched to direct registry check | 06f6640, be85569 |
| 3 | Claude Code installed on QNAP | `install.sh` had no QNAP branch | Added early exit for QNAP | 89e9e98 |
| 4 | `arch: command not found` on QNAP | `arch` is macOS-only; not in BusyBox | Replaced with `uname -m` | ed56971 |
| 5 | No colored prompt on QNAP SSH | Default shell `/bin/sh` doesn't read `.bash_profile` | `.profile_qnap` runs `exec bash -l` | 7e780cc |
| 6 | `.profile_qnap` exec fails | Bash path hardcoded (`/opt/bin/bash`) | Changed to `command -v bash` for dynamic resolution | d38ee0a |
| 7 | `git: command not found` on QNAP | Entware `/opt/bin` not in PATH | Added Entware PATH to `.profile_common` | 584e6dc |
| 8 | Cursor position misaligned on QNAP | Entware bash doesn't handle `\[...\]` correctly | Used `\001`/`\002` (interim) → removed in #10 after root cause fix | f2d62a5 |
| 9 | git disappears after QNAP reboot | Entware QPKG disabled (`Enable != TRUE`), startup script name wrong | `setcfg Enable TRUE` + fixed to `Entware.sh start` | 7533ba7, 401097e |
| 10 | Long input overwrites prompt on QNAP bash | terminfo not installed + `TERMINFO` not set → readline can't detect auto-margin | Auto-install terminfo, set `TERMINFO=/opt/share/terminfo`, unified PS1 to standard `\[`/`\]` | 107b3a6–1ae045e |
| 11 | `E117: Unknown function: pathogen#infect` on QNAP vim | `.vimrc` symlinked but pathogen/plugins not installed | Added vim plugin setup to `dotfileslink.sh` | c3754ce |
| 12 | No colored prompt on Windows PowerShell (one PC only) | (1) Starship not installed — profile silently skips init. (2) Starship config symlinks pointed to old `~/git/dotfiles` location after dotfiles were moved | Installed Starship via `install/win/starship.ps1`; re-ran `dotfileslink.ps1` as admin to fix stale symlinks | 0c83762 |
| 13 | `git add CLAUDE.md` auto-denied by Claude Code | Deny rule `Bash(*dd *)` glob-matched the "dd" in "add", overriding the allow rule `Bash(git add *)`. Deny rules take precedence regardless of order | Replaced `*dd *` with 4 patterns: `dd *`, `*&& dd *`, `*; dd *`, `*| dd *` | c8095dc |
| 14 | `.config/git/config` always dirty after shell startup | `profile.ps1` ran `git config --global core.sshCommand` on every PowerShell startup, appending `[core] sshCommand` directly to tracked `.config/git/config`. The setting was already provided via `config.local` (generated by `dotfileslink.ps1`) and included via `[include] path = config.local` | Removed redundant sshCommand block from `profile.ps1`; setting remains via `config.local` include | dd787a4 |
