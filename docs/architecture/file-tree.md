# File Tree and Module Responsibilities

## Shell Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [.profile_common](https://github.com/nirecom/dotfiles/blob/main/.profile_common) | Shared shell config (aliases, PATH, tool init) for all OSes | Sourced by both Zsh and Bash |
| [.zshrc](https://github.com/nirecom/dotfiles/blob/main/.zshrc) | Zsh-specific settings (Zinit plugin management) | Sources `.profile_common` |
| [.bash_profile](https://github.com/nirecom/dotfiles/blob/main/.bash_profile) | Bash-specific settings (PS1, git-completion) | Sources `.profile_common` |
| [.profile_qnap](https://github.com/nirecom/dotfiles/blob/main/.profile_qnap) | QNAP: auto-switch from sh to bash | `exec bash -l` |
| [bin/detectos.sh](https://github.com/nirecom/dotfiles/blob/main/bin/detectos.sh) | OS detection ($OSDIST, $ISWSL, $ISM1) | See §5 in [architecture.md](../architecture.md) |

## Install Scripts

| File | Responsibility | Notes |
|:---|:---|:---|
| [install.sh](https://github.com/nirecom/dotfiles/blob/main/install.sh) | Unified entry point (Linux/macOS) | `--full` includes packages. Auto-calls `dotfiles-private/install.sh` if present |
| [install.ps1](https://github.com/nirecom/dotfiles/blob/main/install.ps1) | Unified entry point (Windows) | `-Base`/`-Develop`/`-Toolchain`/`-Full`. Auto-calls `dotfiles-private/install.ps1` if present |
| [install/linux/dotfileslink.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/dotfileslink.sh) | Create symlinks on Linux/macOS | |
| [install/win/dotfileslink.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/dotfileslink.ps1) | Create symlinks on Windows | Requires Developer Mode or admin |
| [install/win/autohotkey.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/autohotkey.ps1) | Install AutoHotkey v2 and Japanese layout enforcer | English UI + Japanese preferred only |
| [install/win/powertoys.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/powertoys.ps1) | Install PowerToys and deploy Keyboard Manager settings | |
| [install/win/claude-usage-widget.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/claude-usage-widget.ps1) | Install Claude Usage Widget | |
| [install/win/claude-tabs.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/claude-tabs.ps1) | Install Claude Tabs (multi-session terminal) | |
| [install/win/install-obsolete.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/install-obsolete.ps1) | Remove obsolete files and shortcuts | |
| [install/win/sounds.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/sounds.ps1) | Mute notification sounds | |
| [install/win/fnm.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/fnm.ps1) | Install fnm via winget (Windows only) | |
| [install/linux/nvm.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/nvm.sh) | Install nvm + Node.js LTS (WSL2/macOS/Linux) | |
| [install/win/awscli.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/awscli.ps1) | Install AWS CLI via winget | |
| [install/win/vscode.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/vscode.ps1) | Install VS Code and extensions | Extensions from `config/vscode-extensions.txt` |
| [install/linux/vscode.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/vscode.sh) | Install VS Code and extensions | WSL: skip (Windows host). Extensions from shared list |
| [install/win/vs-cpp.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/vs-cpp.ps1) | Install VS 2022 Community with C++ Desktop workload | For llama.cpp source build (MSVC compiler + CMake bundled). Auto-elevates via UAC |
| [install/win/session-sync-init.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/session-sync-init.ps1) | Initialize `~/.claude/projects/` as git repo for session sync | Called by `install.ps1` |
| [install/linux/session-sync-init.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/session-sync-init.sh) | Initialize `~/.claude/projects/` as git repo for session sync | Called by `install.sh` |
| [bin/session-sync.ps1](https://github.com/nirecom/dotfiles/blob/main/bin/session-sync.ps1) | Session sync daily operation (push/pull/status) | Windows |
| [bin/session-sync.sh](https://github.com/nirecom/dotfiles/blob/main/bin/session-sync.sh) | Session sync daily operation (push/pull/status) | Linux/macOS |
| [bin/wait-vscode-window.ps1](https://github.com/nirecom/dotfiles/blob/main/bin/wait-vscode-window.ps1) | Wait for VS Code window close by title polling | Windows (Win32 EnumWindows) |
| [bin/wait-vscode-window.sh](https://github.com/nirecom/dotfiles/blob/main/bin/wait-vscode-window.sh) | Wait for VS Code window close by title polling | Linux/macOS (xdotool/wmctrl/osascript); WSL2 (powershell.exe) |
| [install/win/profile.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/profile.ps1) | PowerShell profile (SSH agent, auto-pull, migration) | Symlinked to PS5 and PS7 profile paths |
| [install/win/uv.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/uv.ps1) | Install uv (Python package manager) | |
| [install/linux/uv.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/uv.sh) | Install uv (Python package manager) | |
| [install/qnap/dotfileslink.sh](https://github.com/nirecom/dotfiles/blob/main/install/qnap/dotfileslink.sh) | Minimal symlinks for QNAP | Skips Zsh, tmux, Emacs |
| [install/qnap/autorun.sh](https://github.com/nirecom/dotfiles/blob/main/install/qnap/autorun.sh) | QNAP boot-time Entware activation + dotfiles auto-recovery | Runs as root. `dotfileslink.sh` auto-deploys to flash |
| [install/linux/claude-code.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/claude-code.sh) | Claude Code installation | |
| [install/linux/claude-usage-widget.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/claude-usage-widget.sh) | Install Claude Usage Widget | macOS/Ubuntu (skips WSL) |
| [install/linux/keychain.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/keychain.sh) | Install keychain and configure SSH key auto-detection | |

## Editor and Tool Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [.vimrc](https://github.com/nirecom/dotfiles/blob/main/.vimrc) | Vim configuration | |
| [.editorconfig](https://github.com/nirecom/dotfiles/blob/main/.editorconfig) | EditorConfig (4-space default, 2-space for TS/JS/JSON/YAML/CSS) | |
| [.tmux.conf](https://github.com/nirecom/dotfiles/blob/main/.tmux.conf) | tmux configuration | |
| `.emacs.d/` | Emacs configuration (init-loader pattern) | |
| `.config/starship.toml` | Starship prompt (Linux/macOS) | |
| `.config/starship-powershell.toml` | Starship prompt (Windows PowerShell) | |
| [config/vscode-extensions.txt](https://github.com/nirecom/dotfiles/blob/main/config/vscode-extensions.txt) | VS Code extension list (shared across platforms) | Used by `vscode.ps1` / `vscode.sh` |
| [config/win/autohotkey/force-japanese-layout.ahk](https://github.com/nirecom/dotfiles/blob/main/config/win/autohotkey/force-japanese-layout.ahk) | AutoHotkey v2: force Japanese keyboard layout | Windows only |
| [config/win/powertoys/keyboard-manager/default.json](https://github.com/nirecom/dotfiles/blob/main/config/win/powertoys/keyboard-manager/default.json) | PowerToys Keyboard Manager: Emacs-style key remapping | Windows only |

## Claude Code Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [claude-global/CLAUDE.md](https://github.com/nirecom/dotfiles/blob/main/claude-global/CLAUDE.md) | Global Claude Code instructions (workflow steps) | Symlinked to `~/.claude/CLAUDE.md` |
| [claude-global/rules/*.md](https://github.com/nirecom/dotfiles/tree/main/claude-global/rules) | Behavioral rules (coding, git, shell, orthogonality, privacy, docs-convention, test) | Symlinked to `~/.claude/rules/` |
| `claude-global/rules/language.md` | Language policy (conversation, commit, code, docs) | Gitignored; symlinked from `dotfiles-private` |
| [claude-global/skills/*/SKILL.md](https://github.com/nirecom/dotfiles/tree/main/claude-global/skills) | Skills (`/commit-push`, `/deep-research`, `/make-plan`, `/review-code-security`, `/review-plan-security`, `/review-tests`, `/survey-code`, `/update-docs`, `/update-instruction`, `/write-tests`) | Symlinked to `~/.claude/skills/` |
| [claude-global/agents/*.md](https://github.com/nirecom/dotfiles/tree/main/claude-global/agents) | Subagents (`@planner`, `@reviewer`) invoked by `/make-plan` in a discussion loop | Symlinked to `~/.claude/agents/` |
| [claude-global/settings.json](https://github.com/nirecom/dotfiles/blob/main/claude-global/settings.json) | Security allow/deny rules, hooks | Symlinked to `~/.claude/settings.json` |
| [claude-global/hooks/scan-outbound.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/scan-outbound.js) | PreToolUse hook for private info scanning | Scans Edit/Write content |
| [claude-global/hooks/block-dotenv.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/block-dotenv.js) | PreToolUse hook for dotenv file access blocking | Blocks Read/Grep/Glob/Bash access to .env files |
| [claude-global/hooks/workflow-gate.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/workflow-gate.js) | PreToolUse commit gate: enforces all 7 workflow steps | Fail-safe: blocks on missing/corrupted state. Replaces check-docs-updated.js and check-tests-updated.js |
| [claude-global/hooks/workflow-mark.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/workflow-mark.js) | PostToolUse step marker hook | Intercepts `echo "<<WORKFLOW_MARK_STEP_step_status>>"` and `echo "<<WORKFLOW_RESET_FROM_step>>"` via strict regex on `tool_input.command`; marks step using `session_id` from hook stdin |
| [claude-global/hooks/session-start.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/session-start.js) | SessionStart hook | Sets CLAUDE_SESSION_ID via CLAUDE_ENV_FILE; runs zombie state file cleanup |
| [claude-global/hooks/workflow-run-tests.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/workflow-run-tests.js) | PostToolUse hook: auto-marks `run_tests` by Bash exit code | Detects test runner commands; last-run-wins |
| [claude-global/hooks/lib/workflow-state.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/lib/workflow-state.js) | Shared state module for workflow hooks | Reads/writes `~/.claude/projects/workflow/<session-id>.json` (session-scoped) |

## Tests

| File | Responsibility | Notes |
|:---|:---|:---|
| [tests/profile-ssh-keys.Tests.ps1](https://github.com/nirecom/dotfiles/blob/main/tests/profile-ssh-keys.Tests.ps1) | Pester tests for SSH key discovery | Covers glob-based key loading |
| [tests/main-symlink-repair.Tests.ps1](https://github.com/nirecom/dotfiles/blob/main/tests/main-symlink-repair.Tests.ps1) | Pester tests for file symlink backup and broken symlink detection | Normal/error/edge cases for atomic save repair |
| [tests/main-block-dotenv.sh](https://github.com/nirecom/dotfiles/blob/main/tests/main-block-dotenv.sh) | block-dotenv.js hook tests | 59 test cases: Bash/Read/Grep/Glob blocking, false-positive prevention |
| [tests/main-keychain-ssh-agent.sh](https://github.com/nirecom/dotfiles/blob/main/tests/main-keychain-ssh-agent.sh) | keychain SSH agent tests | install.sh inclusion + .profile_common auto-detection |
| [tests/main-claude-tabs.sh](https://github.com/nirecom/dotfiles/blob/main/tests/main-claude-tabs.sh) | claude-tabs.ps1 installer tests | Structure validation (19 test cases) |
| [tests/feature-robust-workflow.sh](https://github.com/nirecom/dotfiles/blob/main/tests/feature-robust-workflow.sh) | Workflow state machine tests | workflow-gate.js, workflow-mark.js, session-start.js |
| [tests/main-workflow-run-tests.sh](https://github.com/nirecom/dotfiles/blob/main/tests/main-workflow-run-tests.sh) | workflow-run-tests.js unit tests | Command heuristics, exit-code marking, idempotency |
| [tests/main-workflow-migration.sh](https://github.com/nirecom/dotfiles/blob/main/tests/main-workflow-migration.sh) | readState() migration tests | verify→run_tests, code removal, review_security addition |
| [tests/feature-doc-tools.py](https://github.com/nirecom/dotfiles/blob/main/tests/feature-doc-tools.py) | doc-append.py and doc-rotate.py tests | 24 pytest cases |

## Git Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [.config/git/config](https://github.com/nirecom/dotfiles/blob/main/.config/git/config) | Git global settings (alias, color, push, fetch) | Default branch: `main` |
| [.config/git/ignore](https://github.com/nirecom/dotfiles/blob/main/.config/git/ignore) | Global gitignore | |
| `.config/git/config.local` | OS-specific settings (gitignored) | Loaded via `[include]` |
| `.config/git/config-work` | Work identity (email/name) (gitignored) | Symlinked from `dotfiles-private` |
