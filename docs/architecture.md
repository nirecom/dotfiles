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
| [install.sh](https://github.com/nirecom/dotfiles/blob/main/install.sh) | Unified entry point (Linux/macOS) | `--full` includes packages |
| [install.ps1](https://github.com/nirecom/dotfiles/blob/main/install.ps1) | Unified entry point (Windows) | `-Full` includes setup |
| [install/linux/dotfileslink.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/dotfileslink.sh) | Create symlinks on Linux/macOS | |
| [install/win/dotfileslink.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/dotfileslink.ps1) | Create symlinks on Windows | Requires Developer Mode or admin |
| [install/win/autohotkey.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/autohotkey.ps1) | Install AutoHotkey v2 and Japanese layout enforcer | English UI + Japanese preferred only |
| [install/win/home-obsolete.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/home-obsolete.ps1) | Remove obsolete files and shortcuts | |
| [install/win/sounds.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/sounds.ps1) | Mute notification sounds | |
| [install/win/fnm.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/fnm.ps1) | Install fnm via winget | |
| [install/win/profile.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/profile.ps1) | PowerShell profile (SSH agent, auto-pull, migration) | Symlinked to PS5 and PS7 profile paths |
| [install/win/uv.ps1](https://github.com/nirecom/dotfiles/blob/main/install/win/uv.ps1) | Install uv (Python package manager) | |
| [install/linux/uv.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/uv.sh) | Install uv (Python package manager) | |
| [install/qnap/dotfileslink.sh](https://github.com/nirecom/dotfiles/blob/main/install/qnap/dotfileslink.sh) | Minimal symlinks for QNAP | Skips Zsh, tmux, Emacs |
| [install/qnap/autorun.sh](https://github.com/nirecom/dotfiles/blob/main/install/qnap/autorun.sh) | QNAP boot-time Entware activation + dotfiles auto-recovery | Runs as root. `dotfileslink.sh` auto-deploys to flash |
| [install/linux/claude-code.sh](https://github.com/nirecom/dotfiles/blob/main/install/linux/claude-code.sh) | Claude Code installation | |

### Editor and Tool Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [.vimrc](https://github.com/nirecom/dotfiles/blob/main/.vimrc) | Vim configuration | |
| [.editorconfig](https://github.com/nirecom/dotfiles/blob/main/.editorconfig) | EditorConfig (4-space default, 2-space for TS/JS/JSON/YAML/CSS) | |
| [.tmux.conf](https://github.com/nirecom/dotfiles/blob/main/.tmux.conf) | tmux configuration | |
| `.emacs.d/` | Emacs configuration (init-loader pattern) | |
| `.config/starship.toml` | Starship prompt (Linux/macOS) | |
| `.config/starship-powershell.toml` | Starship prompt (Windows PowerShell) | |
| [config/win/autohotkey/force-japanese-layout.ahk](https://github.com/nirecom/dotfiles/blob/main/config/win/autohotkey/force-japanese-layout.ahk) | AutoHotkey v2: force Japanese keyboard layout | Windows only |

### Claude Code Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [claude-global/CLAUDE.md](https://github.com/nirecom/dotfiles/blob/main/claude-global/CLAUDE.md) | Global Claude Code instructions (pointer to `rules/` and `skills/`) | Symlinked to `~/.claude/CLAUDE.md` |
| [claude-global/rules/*.md](https://github.com/nirecom/dotfiles/tree/main/claude-global/rules) | Behavioral rules (coding, git, shell, workflow, privacy, docs-lifecycle, test) | Symlinked to `~/.claude/rules/` |
| [claude-global/skills/*/SKILL.md](https://github.com/nirecom/dotfiles/tree/main/claude-global/skills) | Skills (`/update-docs`, `/start-task`, `/complete-task`, `/update-instruction`) | Symlinked to `~/.claude/skills/` |
| [claude-global/settings.json](https://github.com/nirecom/dotfiles/blob/main/claude-global/settings.json) | Security allow/deny rules, hooks | Symlinked to `~/.claude/settings.json` |
| [claude-global/hooks/check-private-info.js](https://github.com/nirecom/dotfiles/blob/main/claude-global/hooks/check-private-info.js) | PreToolUse hook for private info scanning | Scans Edit/Write content |

### Tests

| File | Responsibility | Notes |
|:---|:---|:---|
| [tests/profile-migration-symlink.Tests.ps1](https://github.com/nirecom/dotfiles/blob/main/tests/profile-migration-symlink.Tests.ps1) | Pester tests for claude-code → claude-global migration | Covers permission check, symlink creation, edge cases |
| [tests/profile-ssh-keys.Tests.ps1](https://github.com/nirecom/dotfiles/blob/main/tests/profile-ssh-keys.Tests.ps1) | Pester tests for SSH key discovery | Covers glob-based key loading |
| [tests/test-claude-rules.sh](https://github.com/nirecom/dotfiles/blob/main/tests/test-claude-rules.sh) | claude-global rules reorganization verification | File structure, content, symlink checks |
| [tests/main-symlink-repair.Tests.ps1](https://github.com/nirecom/dotfiles/blob/main/tests/main-symlink-repair.Tests.ps1) | Pester tests for file symlink backup and broken symlink detection | Normal/error/edge cases for atomic save repair |

### Git Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [.config/git/config](https://github.com/nirecom/dotfiles/blob/main/.config/git/config) | Git global settings (alias, color, push, fetch) | Default branch: `main` |
| [.config/git/ignore](https://github.com/nirecom/dotfiles/blob/main/.config/git/ignore) | Global gitignore | |
| `.config/git/config.local` | OS-specific settings (gitignored) | Loaded via `[include]` |

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
```

### Windows PowerShell (PS5 / PS7)

```
PowerShell startup
  → Microsoft.PowerShell_profile.ps1 (symlink → install/win/profile.ps1)
    → ssh-agent start + load all keys ($HOME\.ssh\id_*)
    → git fetch + merge --ff-only (auto-pull dotfiles, 3s timeout)
    → Repair broken file symlinks (atomic save detection, ~20ms)
    → claude-code → claude-global migration symlink (one-time, permission check + try/catch)
    → claude settings symlink migration (if still pointing to claude-code)
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

`install.sh` (Linux/macOS) runs scripts in this order: `dotfileslink.sh` → `claude-code.sh` → `home-obsolete.sh` → (`--full`: `home-init.sh` + package scripts)

`install.ps1` (Windows) runs scripts in this order: `dotfileslink.ps1` → `home-obsolete.ps1` → `sounds.ps1` → (`-Full`: `claude-code.ps1` → `starship.ps1` → `fnm.ps1` → `uv.ps1` → `autohotkey.ps1`)

See [README.md](../README.md) for full platform-specific installation instructions.

---

## 7. Claude Code Configuration

The `claude-global/` directory manages global Claude Code settings centrally. The directory is named `claude-global/` (not `.claude/`) to avoid conflicts with project-level `.claude/` directories.

**Symlink structure**:
- `claude-global/CLAUDE.md` → `~/.claude/CLAUDE.md`
- `claude-global/settings.json` → `~/.claude/settings.json`
- `claude-global/skills/` → `~/.claude/skills/`
- `claude-global/rules/` → `~/.claude/rules/`

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
| `git auto-pull` merge conflict | Fast-forward only; on conflict, displays error and continues |
| QNAP default shell resets to `/bin/sh` on reboot | `.profile_qnap` → `exec bash -l` handles this on every login |

---

## 10. Private Information Scanning

Two checkpoints prevent private information from being committed:

| Checkpoint | Mechanism | Script |
|:---|:---|:---|
| Git commit | `claude-global/hooks/pre-commit` (via `core.hooksPath`) | Scans staged file content |
| Claude Code edit | `claude-global/hooks/check-private-info.js` (PreToolUse) | Scans Edit/Write content |

Both call `bin/check-private-info.sh` (single source of truth for detection patterns).

**Detection patterns**: RFC 1918 IPv4 (`10.x`, `172.16-31.x`, `192.168.x`), email addresses, MAC addresses, absolute local paths (`/Users/`, `/home/`, `C:\Users\`). Additional patterns via `.private-info-blocklist`.

**Exception handling**: `.private-info-allowlist` for known-safe patterns (e.g., `git@github.com` SSH URLs, `noreply.github.com` email).

**Private repo whitelist**: `.context-private/private-repos.txt` (gitignored). Generated by `bin/update-private-repos.sh` via `gh repo list`. Repos in this list are not scanned. Unregistered repos are scanned by default (safe default).

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
