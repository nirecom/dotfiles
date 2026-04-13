# Architecture

## 1. Design Principles

- **Cross-platform**: Ubuntu (native/WSL2), macOS (Intel/Apple Silicon), Windows, QNAP NAS
- **Single Source of Truth**: `.profile_common` is the central shell configuration file. Sourced by both Zsh and Bash
- **OS detection with conditional branching**: `bin/detectos.sh` sets `$OSDIST`; `.profile_common` branches on `case "$OSDIST"`. No hardcoded OS-specific paths
- **Symlink pattern**: Files in the dotfiles repo are symlinked into `$HOME`. Never copied directly
- **Defensive loading**: All external tools/files are checked before use (`type cmd >/dev/null 2>&1`, `[ -e $FILE ]`)
- **Public repository**: No personal information or network details are committed

---

## 2. Architecture Overview

```
┌──────────────────────────────────────────────────────────┐
│  ~/dotfiles (GitHub: nirecom/dotfiles)                    │
│                                                           │
│  ┌──────────────────┐    ┌────────────────────────────┐  │
│  │ Shell Config      │    │ Install Scripts             │  │
│  │ .profile_common   │    │ install.sh   (Linux/macOS)  │  │
│  │ .zshrc            │    │ install.ps1  (Windows)      │  │
│  │ .bash_profile     │    │ install/linux/              │  │
│  │ .profile_qnap     │    │ install/win/                │  │
│  │ bin/detectos.sh   │    │ install/qnap/               │  │
│  └───────┬──────────┘    └─────────┬──────────────────┘  │
│          │ symlink                  │ symlink              │
│          ▼                          ▼                      │
│  $HOME/.zshrc                $HOME/.claude/                │
│  $HOME/.bash_profile         $HOME/.config/git/            │
│  $HOME/.vimrc                $HOME/.config/starship.toml   │
└──────────────────────────────────────────────────────────┘
```

**Symlink scope by platform**:

| Platform | Symlink targets |
|:---|:---|
| Linux/macOS | All files (shell, editor, tmux, Claude Code, git, starship) |
| Windows | `.editorconfig`, `.config/git/`, Claude Code settings, starship |
| QNAP NAS | Minimal set (`.bash_profile`, `.profile_common`, `.vimrc`, `.inputrc`, `.profile` → `.profile_qnap`) |

---

## 3. File Tree and Module Responsibilities

### Shell Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [.profile_common](https://github.com/nirecom/dotfiles/blob/main/.profile_common) | Shared shell config (aliases, PATH, tool init) for all OSes | Sourced by both Zsh and Bash |
| [.zshrc](https://github.com/nirecom/dotfiles/blob/main/.zshrc) | Zsh-specific settings (Zinit plugin management) | Sources `.profile_common` |
| [.bash_profile](https://github.com/nirecom/dotfiles/blob/main/.bash_profile) | Bash-specific settings (PS1, git-completion) | Sources `.profile_common` |
| [.profile_qnap](https://github.com/nirecom/dotfiles/blob/main/.profile_qnap) | QNAP: auto-switch from sh to bash | `exec bash -l` |
| [bin/detectos.sh](https://github.com/nirecom/dotfiles/blob/main/bin/detectos.sh) | OS detection ($OSDIST, $ISWSL, $ISM1) | See §5 |

### Install Scripts

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

### Editor and Tool Configuration

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

### Claude Code Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [claude-global/CLAUDE.md](https://github.com/nirecom/dotfiles/blob/main/claude-global/CLAUDE.md) | Global Claude Code instructions (workflow steps) | Symlinked to `~/.claude/CLAUDE.md` |
| [claude-global/rules/*.md](https://github.com/nirecom/dotfiles/tree/main/claude-global/rules) | Behavioral rules (coding, git, shell, orthogonality, privacy, docs-convention, test) | Symlinked to `~/.claude/rules/` |
| `claude-global/rules/language.md` | Language policy (conversation, commit, code, docs) | Gitignored; symlinked from `dotfiles-private` |
| [claude-global/skills/*/SKILL.md](https://github.com/nirecom/dotfiles/tree/main/claude-global/skills) | Skills (`/commit-push`, `/deep-research`, `/make-plan`, `/review-security`, `/review-tests`, `/survey-code`, `/update-docs`, `/update-instruction`, `/write-tests`) | Symlinked to `~/.claude/skills/` |
| [claude-global/agents/*.md](https://github.com/nirecom/dotfiles/tree/main/claude-global/agents) | Subagents (`@planner`, `@reviewer`) invoked by `/make-plan` in a discussion loop | Symlinked to `~/.claude/agents/` |
| [claude-global/settings.json](https://github.com/nirecom/dotfiles/blob/main/claude-global/settings.json) | Security allow/deny rules, hooks | Symlinked to `~/.claude/settings.json` |
| [claude-global/hooks/check-private-info.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/check-private-info.js) | PreToolUse hook for private info scanning | Scans Edit/Write content |
| [claude-global/hooks/block-dotenv.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/block-dotenv.js) | PreToolUse hook for dotenv file access blocking | Blocks Read/Grep/Glob/Bash access to .env files |
| [claude-global/hooks/workflow-gate.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/workflow-gate.js) | PreToolUse commit gate: enforces all 7 workflow steps | Fail-safe: blocks on missing/corrupted state. Replaces check-docs-updated.js and check-tests-updated.js |
| [claude-global/hooks/workflow-mark.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/workflow-mark.js) | PostToolUse step marker hook | Intercepts `echo "<<WORKFLOW_MARK_STEP_step_status>>"` via strict regex on `tool_input.command`; marks step using `session_id` from hook stdin. Uses `CLAUDE_PROJECT_DIR` for repo resolution |
| [claude-global/hooks/mark-step.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/mark-step.js) | Workflow step completion CLI | `node mark-step.js <step> <status>` or `--reset-from <step>`. Required for `user_verification` (triggers ask-rule) |
| [claude-global/hooks/session-start.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/session-start.js) | SessionStart hook | Sets CLAUDE_SESSION_ID via CLAUDE_ENV_FILE; runs zombie state file cleanup |
| [claude-global/hooks/lib/workflow-state.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/lib/workflow-state.js) | Shared state module for workflow hooks | Reads/writes `.git/workflow/<session-id>.json` |

### Tests

| File | Responsibility | Notes |
|:---|:---|:---|
| [tests/profile-ssh-keys.Tests.ps1](https://github.com/nirecom/dotfiles/blob/main/tests/profile-ssh-keys.Tests.ps1) | Pester tests for SSH key discovery | Covers glob-based key loading |
| [tests/main-symlink-repair.Tests.ps1](https://github.com/nirecom/dotfiles/blob/main/tests/main-symlink-repair.Tests.ps1) | Pester tests for file symlink backup and broken symlink detection | Normal/error/edge cases for atomic save repair |
| [tests/main-block-dotenv.sh](https://github.com/nirecom/dotfiles/blob/main/tests/main-block-dotenv.sh) | block-dotenv.js hook tests | 59 test cases: Bash/Read/Grep/Glob blocking, false-positive prevention |
| [tests/main-keychain-ssh-agent.sh](https://github.com/nirecom/dotfiles/blob/main/tests/main-keychain-ssh-agent.sh) | keychain SSH agent tests | install.sh inclusion + .profile_common auto-detection |
| [tests/main-claude-tabs.sh](https://github.com/nirecom/dotfiles/blob/main/tests/main-claude-tabs.sh) | claude-tabs.ps1 installer tests | Structure validation (19 test cases) |
| [tests/feature-robust-workflow.sh](https://github.com/nirecom/dotfiles/blob/main/tests/feature-robust-workflow.sh) | Workflow state machine tests | workflow-gate.js, mark-step.js, session-start.js |

### Git Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [.config/git/config](https://github.com/nirecom/dotfiles/blob/main/.config/git/config) | Git global settings (alias, color, push, fetch) | Default branch: `main` |
| [.config/git/ignore](https://github.com/nirecom/dotfiles/blob/main/.config/git/ignore) | Global gitignore | |
| `.config/git/config.local` | OS-specific settings (gitignored) | Loaded via `[include]` |
| `.config/git/config-work` | Work identity (email/name) (gitignored) | Symlinked from `dotfiles-private` |

---

## 4. Shell Startup Flow

### Linux/macOS (Zsh)

```
login
  → .zshrc
    → source detectos.sh (sets $OSDIST, $ISWSL, $ISM1)
    → source .profile_common (aliases, PATH, tool init)
    → Zinit plugins (autosuggestions, syntax-highlighting, completions)
    → Starship prompt
```

### Linux/macOS (Bash)

```
login
  → .bash_profile
    → source detectos.sh
    → source .profile_common
    → PS1 with __git_ps1
    → git auto-pull (fast-forward only, first shell only)
      → If diverged (force push): prompt y/N to reset (marker file skips)
```

### Windows PowerShell (PS5 / PS7)

```
PowerShell startup
  → Microsoft.PowerShell_profile.ps1 (symlink → install/win/profile.ps1)
    → ssh-agent start + load all keys ($HOME\.ssh\id_*)
    → git fetch + merge --ff-only (auto-pull dotfiles, 3s timeout)
    → Repair broken file symlinks (atomic save detection, ~20ms)
    → ~/.local/bin PATH addition
    → Starship prompt init
    → fnm init (try/catch for SAC App Control)
```

### QNAP NAS

```
SSH login (shell: /bin/sh)
  → ~/.profile (symlink → .profile_qnap)
    → exec bash -l (dynamic path resolution: command -v bash)
      → ~/.bash_profile
        → source detectos.sh (OSDIST=qnap)
        → source .profile_common (Entware PATH, aliases, git-prompt)
        → PS1 (raw readline markers \001/\002 for ANSI escape handling)
```

---

## 5. OS Detection

Variables set by `bin/detectos.sh`:

| Variable | Value | Detection method |
|:---|:---|:---|
| `$OSDIST` | `macos` | `uname == Darwin` |
| `$OSDIST` | `ubuntu` | `/etc/os-release` contains `NAME="Ubuntu"` |
| `$OSDIST` | `amazon` | `/etc/os-release` contains `NAME="Amazon"` |
| `$OSDIST` | `centos` | `/etc/os-release` contains `NAME="CentOS"` |
| `$OSDIST` | `qnap` | `/sbin/getcfg` exists |
| `$OSDIST` | `mingw` | `uname -s` starts with `MINGW` |
| `$ISWSL` | `true`/`false` | `/proc/sys/fs/binfmt_misc/WSLInterop` exists |
| `$ISM1` | `true`/`false` | `uname -m == arm64` |

**Design guideline**: When adding new tools, use `case "$OSDIST"` blocks in `.profile_common` for OS-specific paths and behavior. Use `case` statements consistently — not nested `if` blocks.

---

## 6. Installation Details

### Execution order

`install.sh` (Linux/macOS) runs scripts in this order: `dotfileslink.sh` → `claude-code.sh` → `session-sync-init.sh` (if Claude Code installed) → `keychain.sh` → `nvm.sh` → `install-obsolete.sh` → (`--base`/`--full`: `install-base.sh` → `claude-usage-widget.sh`) → (`--develop`/`--full`: `install-develop.sh` → `vscode.sh`)

`install.ps1` (Windows) runs scripts in this order: `dotfileslink.ps1` → `claude-code.ps1` → `session-sync-init.ps1` (if Claude Code installed) → `fnm.ps1` → `install-obsolete.ps1` → `sounds.ps1` → (`-Base`/`-Develop`/`-Toolchain`/`-Full`: `starship.ps1` → `uv.ps1` → `google-japanese-input.ps1` → `autohotkey.ps1` → `powertoys.ps1` → `claude-usage-widget.ps1` → `claude-tabs.ps1`) → (`-Develop`/`-Toolchain`/`-Full`: `awscli.ps1` → `vscode.ps1`) → (`-Toolchain`/`-Full`: `vs-cpp.ps1`)

See [README.md](../README.md) for full platform-specific installation instructions.

---

## 7. Claude Code Configuration

### Session Sync

Syncs Claude Code session history (`~/.claude/projects/`) across multiple machines (Windows, macOS, Linux).

**Problem**: Claude Code indexes projects by absolute path (e.g., `C:\Users\<user>\git\repo` → `~/.claude/projects/-C-Users-<user>-git-repo/`). Paths containing the username differ across machines, making session reference and resume impossible.

**Solution**: Use drive-root unified paths (dedicated directories for LLM infrastructure and `C:\git\` on Windows) to eliminate username dependency, and sync history via a private GitHub repo.

| Path | Purpose |
|---|---|
| (drive-root LLM dir) | LLM infrastructure (existing, immovable — tightly coupled with NSSM services, TLS certs, GGUF model paths) |
| `C:\git\` | All other git repositories (Windows) |

**Sync method**: Initialize `~/.claude/projects/` as a git repo, syncing to `nirecom/claude-sessions` (private). Init fetches existing remote history (`fetch` + `reset`) before first commit so that 2nd+ machines inherit prior session data without conflict.

| File | Repository | Responsibility |
|---|---|---|
| [install/win/session-sync-init.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/session-sync-init.ps1) | dotfiles | Initialization — Windows (git init, .gitattributes, remote setup, initial commit+push) |
| [install/linux/session-sync-init.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/session-sync-init.sh) | dotfiles | Initialization — Linux/macOS (equivalent to ps1 version) |
| [bin/session-sync.ps1](https://github.com/nirecom/dotfiles/blob/main/bin/session-sync.ps1) | dotfiles | Daily operation — Windows (push/pull/status subcommands) |
| [bin/session-sync.sh](https://github.com/nirecom/dotfiles/blob/main/bin/session-sync.sh) | dotfiles | Daily operation — Linux/macOS (push/pull/status subcommands) |

**Sync scope**:

| Path | Synced | Reason |
|---|---|---|
| `~/.claude/projects/` | Yes | Session history (JSONL) |
| `~/.claude/settings.json` | No | Managed by dotfiles |
| `~/.claude/CLAUDE.md`, `rules/`, `skills/` | No | Managed by dotfiles |
| `~/.claude/statsig/`, `ide/`, `history.jsonl` | No | Machine-specific |

**Line ending control**: `.gitattributes` declares `* text eol=lf`. Preserves LF line endings in JSONL files generated by Claude Code without conversion.

**Relationship with Web mode**: VS Code's Local/Web toggle provides Web mode (claude.ai/code), but tasks requiring local filesystem, MCP servers, or NSSM service operations must use Local mode. Local sessions are invisible to Web and vice versa. Self-hosted sync is necessary for sharing Local mode session history.

**Automatic sync**:
- **Terminal startup**: `.profile_common` (Linux/macOS) and `profile.ps1` (Windows) run `git fetch + merge --ff-only` on `~/.claude/projects/` with 3-second timeout
- **`codes` function**: Opens VS Code in a new window (`--new-window`), polls for window closure via title matching (`wait-vscode-window.ps1` / `.sh`), then runs session-sync push. Each instance independently detects its own window, so closing one window triggers push even while other windows remain open. Windows: Win32 EnumWindows API. Linux: xdotool/wmctrl. macOS: osascript. Push runs in quiet mode (`-Quiet` / `--quiet`): shows a single Windows toast notification on completion or failure, indicating when it's safe to shut down (WinRT API, no external modules). Linux: `notify-send` fallback

**Manual sync**:
```
End of work:    Close all Claude Code sessions → session-sync push
Other machine:  session-sync pull → Launch Claude Code
```

**Known behavior**: `session-sync push` uses `git add .` to stage all working tree changes. If Claude Code deletes session files locally (format migration, pruning, etc.), those deletions propagate to remote. In 2026-04, a format migration (`UUID.jsonl` → `UUID/subagents/`) caused 35 files to be bulk-deleted, but most had directory versions with no data loss. One-time event — no mitigation needed.

The `claude-global/` directory manages global Claude Code settings centrally. The directory is named `claude-global/` (not `.claude/`) to avoid conflicts with project-level `.claude/` directories.

**Symlink structure**:
- `claude-global/CLAUDE.md` → `~/.claude/CLAUDE.md`
- `claude-global/settings.json` → `~/.claude/settings.json`
- `claude-global/skills/` → `~/.claude/skills/`
- `claude-global/rules/` → `~/.claude/rules/`
- `claude-global/agents/` → `~/.claude/agents/`

### settings.json Design

**Allow rules** — read-only operations only:
- Git read commands (`git status`, `git log`, `git diff`, `git branch`, etc.)
- `git -C <path>` for cross-directory git reads (status/diff/log) — preferred method
- `cd <path> && git` fallback (status/diff/log minimal set only)
- Filesystem reads (`ls`, `tree`, `head`, `tail`, `grep`, `wc`, etc.)
- `.env.example` reads (`.env` itself is denied)

**Deny rules** — four categories (wildcard prefix `*` to catch compound commands):

| Category | Target | Examples |
|:---|:---|:---|
| Environment files | `.env`, `.env.*` | Denied in Read, Grep, and Bash |
| Destructive commands | Force push, hard reset, deletion | `git push --force`, `rm -rf`, `dd` |
| Credentials | SSH keys, AWS, Docker, kube | `~/.ssh/**`, `~/.aws/**`, `~/.kube/**`, etc. |
| Direct dotfile editing | Home directory dotfiles | `~/.bashrc`, `~/.zshrc`, etc. denied in Edit |

**Hook format**: Nested format — `matcher` + `hooks` array. Timeout in seconds.

```json
{ "matcher": "Edit|Write", "hooks": [{ "type": "command", "command": "node .../hook.js", "timeout": 5 }] }
```

**Hooks**:
- `check-private-info.js` (matcher: `Bash`) — scans Bash commands for private info patterns
- `block-dotenv.js` (matcher: `Bash|Read|Grep|Glob`) — blocks `.env` file access. Sanitizes git commit messages to avoid false positives
- `workflow-gate.js` (matcher: `Bash`) — enforces all 7 workflow steps before `git commit`. Reads state from `.git/workflow/<session-id>.json`. Fail-safe: missing session_id, missing state file, or corrupted JSON → block. Block message lists incomplete steps with remediation commands. `research` and `plan` can be `skipped`; all other steps must be `complete`. Replaces `check-docs-updated.js` and `check-tests-updated.js`
- `session-start.js` (SessionStart event) — appends `CLAUDE_SESSION_ID=<sid>` to `CLAUDE_ENV_FILE`; creates `.git/workflow/<session-id>.json` with all steps `pending` if it does not already exist (idempotent, fail-open for non-git dirs); runs zombie cleanup (deletes state files with all timestamps older than 7 days)
- `check-cross-platform.js` (matcher: `Bash`) — blocks `git commit` when platform-specific files (install/win/ ↔ install/linux/) are staged without counterpart changes
  - Skip mechanisms: `.cross-platform-skiplist` (permanent, base tool names) and `.git/.cross-platform-reviewed` (one-time, HEAD hash)

**Permission glob matching**: settings.json permissions (allow/deny/ask) are glob-matched against the entire command string. `&&` does not split into subcommands. `Bash(git commit *)` does not match `cd /path && git commit -m msg` (because it starts with `cd`). Deny rules use a leading `*` (e.g., `*git commit --amend*`) to catch compound commands. Only interactive approval ("Yes, don't ask again") splits subcommands and saves individual rules (separate mechanism).

**Known limitations**:
- When a PreToolUse hook is set on Edit|Write, the "Ask before edits" dialog is bypassed (hook success is interpreted as permission granted). Delegate Edit|Write private info scanning to the pre-commit hook.
- Hook format must be nested format. Flat format (matcher/command/timeout at the same level) causes the entire settings.json to be skipped.
- VSCode extension's "Ask before edits" mode only covers Edit/Write. Bash commands do not trigger the ask dialog. "Ask permissions" (ask for all tools) mode does not exist in VSCode.
- Hot-reloading of settings.json hook changes is unreliable. Restart Claude Code after changes.

### Workflow State Machine

All 7 workflow steps are tracked in a per-session JSON state file and enforced at `git commit` time by a PreToolUse hook.

#### State file

Path: `.git/workflow/<session-id>.json` (auto-gitignored — never committed)

```json
{
  "version": 1,
  "session_id": "abc123",
  "created_at": "2026-04-12T10:00:00.000Z",
  "steps": {
    "research":          { "status": "complete", "updated_at": "..." },
    "plan":              { "status": "complete", "updated_at": "..." },
    "write_tests":       { "status": "complete", "updated_at": "..." },
    "code":              { "status": "complete", "updated_at": "..." },
    "verify":            { "status": "complete", "updated_at": "..." },
    "docs":              { "status": "complete", "updated_at": "..." },
    "user_verification": { "status": "complete", "updated_at": "..." }
  }
}
```

Statuses: `pending` | `in_progress` | `complete` | `skipped`
- `skipped`: allowed only for `research` and `plan` (CLAUDE.md skip conditions)
- `user_verification`: cannot be `skipped` — enforced at CLI and permission level

#### Steps and owners

| Step | How completed |
|---|---|
| `research` | `/survey-code` or `/deep-research` skill (emits `WORKFLOW_MARK_STEP` marker) |
| `plan` | `/make-plan` skill (emits marker) |
| `write_tests` | `/write-tests` skill (emits marker) |
| `code` | `node mark-step.js code complete` |
| `verify` | `node mark-step.js verify complete` |
| `docs` | `/update-docs` skill (emits marker) |
| `user_verification` | `echo "<<WORKFLOW_USER_VERIFIED>>"` — triggers `ask` permission dialog; user must approve; PostToolUse hook marks step complete |

Each skill's `## Completion` section runs `echo "<<WORKFLOW_MARK_STEP_<step>_complete>>"` as the sole Bash command (no pipes, no `&&`, no redirection). The PostToolUse hook (`workflow-mark.js`) intercepts this via strict anchored regex on `tool_input.command` and calls `markStep()` directly using `session_id` from the hook's stdin JSON. This bypasses the `CLAUDE_ENV_FILE` propagation issue in Bash tool subprocesses (Anthropic bug #27987).

Note: marker format uses `_` as separator (not `:`). Claude Code's permission glob parser treats `:` as a named-parameter separator inside `Bash(...)` rules, causing silent match failure (anthropics/claude-code#33601). Using `_` avoids this and allows `allow`/`ask` rules to match correctly.

`user_verification` uses a dedicated marker `echo "<<WORKFLOW_USER_VERIFIED>>"` (DQ only, single space, no SQ variant). This command is in the `ask` permission category — Claude must request user approval via dialog before the echo runs. The PostToolUse hook intercepts it identically to `WORKFLOW_MARK_STEP`.

#### Session ID flow

```
Session start → session-start.js (SessionStart hook)
  appends CLAUDE_SESSION_ID=<sid> to CLAUDE_ENV_FILE
  creates .git/workflow/<sid>.json with all steps pending (if not exists)
  runs zombie cleanup

Skill runs (/make-plan, /write-tests, etc.)
  → Completion section emits: echo "<<WORKFLOW_MARK_STEP_<step>_complete>>"
  → workflow-mark.js (PostToolUse hook) intercepts command
     reads session_id from hook stdin JSON (not CLAUDE_ENV_FILE)
     calls markStep(CLAUDE_PROJECT_DIR, session_id, step, status)

git commit attempt → workflow-gate.js (PreToolUse hook)
  reads session_id from hook stdin JSON
  loads .git/workflow/<session_id>.json
  approves if all steps complete/skipped; blocks with remediation message otherwise
```

#### Fail-safe behavior

| Condition | Result |
|---|---|
| `session_id` missing from hook stdin | block |
| State file not found | block |
| State file corrupted (bad JSON) | block |
| Step `pending` or `in_progress` | block |
| Non-skippable step marked `skipped` | block |

To reset from a specific step (e.g., re-running code phase):
```
node "$DOTFILES_DIR/claude-global/hooks/mark-step.js" --reset-from <step>
```
Marks all prior steps `complete`, resets target step and after to `pending`.

---

### Test Iteration Workflow

TDD test writing uses a subagent (`mode: "auto"`) to run the write → run → fix loop autonomously. This reduces user confirmations from O(N) (per-edit approval) to exactly 2: (a) test case plan approval, (b) final test file review. The subagent is instructed to edit only test files, never source code. See `write-tests` skill for the procedure.

---

## 8. Git Global Configuration

Located in `.config/git/` (XDG-compliant, not `~/.gitconfig`).

| File | Responsibility |
|:---|:---|
| `config` | Common settings: aliases (`br`, `ch`, `st`, `co`, `fe`, `gr`), color, push, fetch prune, `init.defaultBranch = main` |
| `config.local` | OS-specific settings. macOS: `credential.helper = osxkeychain`, Windows: `core.sshCommand`, etc. |
| `ignore` | Global gitignore: node_modules, .class, .terraform/, editor temps, OS files, AI artifacts |

`config.local` is auto-generated by `install/linux/dotfileslink.sh` or `install/win/dotfileslink.ps1` based on OS detection. Loaded via `[include] path = config.local` in `config`.

---

## 9. Risks and Mitigations

| Risk | Mitigation |
|:---|:---|
| QNAP firmware update removes Entware | `autorun.sh` auto-recovers at boot |
| QNAP Entware QPKG gets disabled | `autorun.sh` includes `setcfg Enable TRUE` fallback |
| Windows symlinks require Developer Mode or admin | `dotfileslink.ps1` checks prerequisites and skips on failure |
| `.profile_common` grows too large | OS-specific logic is consolidated in `case` blocks; new tools follow the same pattern |
| `claude-global/settings.json` deny rule gaps | Review allow/deny rules when adding new tools |
| `.env` leak via Claude Code | `block-dotenv.js` PreToolUse hook + deny rules (defense in depth). Static analysis limit: variable/subshell indirection not detectable |
| `git auto-pull` merge conflict | Fast-forward only; on conflict, displays error and continues |
| QNAP default shell resets to `/bin/sh` on reboot | `.profile_qnap` → `exec bash -l` handles this on every login |

---

## 10. Private Information Scanning

Two checkpoints prevent private information from being committed:

| Checkpoint | Mechanism | Script |
|:---|:---|:---|
| Git commit | `claude-global/hooks/pre-commit` (via `core.hooksPath`) | Scans staged file content |
| Git commit (effortLevel/model) | `claude-global/hooks/pre-commit` (via `core.hooksPath`) | Auto-unstages settings.json if only effortLevel and/or model changed |
| Git commit (.sh perms) | `claude-global/hooks/pre-commit` (via `core.hooksPath`) | Blocks commit if `.sh` files lack execute permission (`100644`) |
| Claude Code edit | `claude-global/hooks/check-private-info.js` (PreToolUse) | Scans Edit/Write content |

Both call `bin/check-private-info.sh` (single source of truth for detection patterns).

**Detection patterns**: RFC 1918 IPv4 (`10.x`, `172.16-31.x`, `192.168.x`), email addresses, MAC addresses, absolute local paths (`/Users/`, `/home/`, `C:\Users\`). Additional patterns via `.private-info-blocklist`.

**Exception handling**: `.private-info-allowlist` for known-safe patterns (e.g., `git@github.com` SSH URLs, `noreply.github.com` email). Environment-specific exceptions can be added to `../dotfiles-private/.private-info-allowlist`. File-scoped patterns support glob matching (e.g., `tests/*:@example.com`).

**Private repo detection**: non-GitHub hosts (GitLab, Bitbucket, GitHub Enterprise, etc.) are automatically treated as private and skip scanning entirely. For `github.com` remotes, visibility is checked via `gh api repos/{owner}/{repo}`. Private repos are not scanned. If `gh` is unavailable or the API call fails, scanning proceeds (fail-open, safe default). Shared logic in `claude-global/hooks/lib/is-private-repo.js` (Node.js hooks) and inline host extraction (git hooks).

**Additional patterns** (personal information, hardware model numbers, hostnames, etc.) can be detected by adding regex patterns to `.private-info-blocklist`.

For detailed usage, see [private-info-scanning.md](private-info-scanning.md).

---

## 11. References

- Dotfiles repository: https://github.com/nirecom/dotfiles
- Zinit: https://github.com/zdharma-continuum/zinit
- Starship: https://starship.rs/
- Entware (QNAP): https://github.com/Entware/Entware
- EditorConfig: https://editorconfig.org/
- Claude Code: https://docs.anthropic.com/en/docs/claude-code
