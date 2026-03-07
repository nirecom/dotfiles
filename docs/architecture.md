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

### Claude Code Configuration

| File | Responsibility | Notes |
|:---|:---|:---|
| [claude-code/CLAUDE.md](https://github.com/nirecom/dotfiles/blob/main/claude-code/CLAUDE.md) | Global Claude Code instructions | Symlinked to `~/.claude/CLAUDE.md` |
| [claude-code/settings.json](https://github.com/nirecom/dotfiles/blob/main/claude-code/settings.json) | Security allow/deny rules | Symlinked to `~/.claude/settings.json` |

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

`install.sh` runs scripts in this order: `dotfileslink.sh` → `claude-code.sh` → `home-obsolete.sh` → (`--full`: `home-init.sh` + package scripts)

See [README.md](../README.md) for full platform-specific installation instructions.

---

## 7. Claude Code Configuration

The `claude-code/` directory manages global Claude Code settings centrally. The directory is named `claude-code/` (not `.claude/`) to avoid conflicts with project-level `.claude/` directories.

**Symlink structure**:
- `claude-code/CLAUDE.md` → `~/.claude/CLAUDE.md`
- `claude-code/settings.json` → `~/.claude/settings.json`

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
| `claude-code/settings.json` deny rule gaps | Review allow/deny rules when adding new tools |
| `git auto-pull` merge conflict | Fast-forward only; on conflict, displays error and continues |
| QNAP default shell resets to `/bin/sh` on reboot | `.profile_qnap` → `exec bash -l` handles this on every login |

---

## 10. References

- Dotfiles repository: https://github.com/nirecom/dotfiles
- Zinit: https://github.com/zdharma-continuum/zinit
- Starship: https://starship.rs/
- Entware (QNAP): https://github.com/Entware/Entware
- EditorConfig: https://editorconfig.org/
- Claude Code: https://docs.anthropic.com/en/docs/claude-code
