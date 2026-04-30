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
| Linux/macOS | All files (shell, editor, tmux, git, starship) |
| Windows | `.editorconfig`, `.config/git/`, Claude Code settings, starship |
| QNAP NAS | Minimal set (`.bash_profile`, `.profile_common`, `.vimrc`, `.inputrc`, `.profile` → `.profile_qnap`) |

---

## 3. File Tree and Module Responsibilities

See [architecture/file-tree.md](architecture/file-tree.md) for shell configuration, install scripts, editor config, Claude Code config, tests, and git configuration tables.

---

## 4. Shell Startup Flow

See [architecture/shell-startup.md](architecture/shell-startup.md) for startup sequences on Linux/macOS (Zsh/Bash), Windows PowerShell, and QNAP NAS.

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

### Execution order (dotfiles only)

`install.sh` (Linux/macOS) runs scripts in this order: `dotfileslink.sh` → `keychain.sh` → `nvm.sh` → `install-obsolete.sh` → `gh.sh` → (`--base`/`--develop`/`--full`: `install-base.sh` → `uv.sh` → `claude-usage-widget.sh`) → (`--develop`/`--full`: `install-develop.sh` → `vscode.sh`)

`install.ps1` (Windows) runs scripts in this order: `dotfileslink.ps1` → `fnm.ps1` → `install-obsolete.ps1` → `sounds.ps1` → `snipping-tool.ps1` → input language hotkeys → `pwsh.ps1` → `gh.ps1` → (`-Base`/`-Develop`/`-Toolchain`/`-Full`: `starship.ps1` → `uv.ps1` → `google-japanese-input.ps1` → `autohotkey.ps1` → `powertoys.ps1` → `claude-usage-widget.ps1` → `claude-tabs.ps1`) → (`-Develop`/`-Toolchain`/`-Full`: `awscli.ps1` → `vscode.ps1`) → (`-Toolchain`/`-Full`: `vs-cpp.ps1`)

The full install chain (dotfiles → dotfiles-private → agents → fornix) is orchestrated by the sibling `dotfiles-private` hub installer. See its `docs/architecture.md` for the complete execution order and clone-if-missing design.

See [README.md](../README.md) for platform-specific installation instructions.

---

## 7. Claude Code Configuration

The Claude Code workflow framework — state machine, session sync, settings.json, hooks, and test iteration — lives in the companion [nirecom/agents](https://github.com/nirecom/agents) repo. See its [architecture doc](https://github.com/nirecom/agents/blob/main/docs/architecture/claude-code.md) for details. Wiring agents into the environment is handled by the `dotfiles-private` hub installer.

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
| `git auto-pull` merge conflict | Fast-forward only; on conflict, displays error and continues |
| QNAP default shell resets to `/bin/sh` on reboot | `.profile_qnap` → `exec bash -l` handles this on every login |

---

## 10. Private Information Scanning

Two checkpoints prevent private information from reaching public repositories: a `git pre-commit` hook and a Claude Code PreToolUse hook — both provided by the companion [nirecom/agents](https://github.com/nirecom/agents) repo. They detect RFC 1918 IP addresses, email addresses, MAC addresses, absolute local paths, hard-coded secrets, PEM private keys, and Trojan Source Unicode. Private repositories (detected via `gh api`) are skipped automatically.

See [nirecom/agents: docs/scan-outbound.md](https://github.com/nirecom/agents/blob/main/docs/scan-outbound.md) for detection patterns, exception handling, and configuration.

---

## 11. References

- Dotfiles repository: https://github.com/nirecom/dotfiles
- Zinit: https://github.com/zdharma-continuum/zinit
- Starship: https://starship.rs/
- Entware (QNAP): https://github.com/Entware/Entware
- EditorConfig: https://editorconfig.org/
- Claude Code: https://docs.anthropic.com/en/docs/claude-code
