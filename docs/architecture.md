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

### Execution order

`install.sh` (Linux/macOS) runs scripts in this order: `dotfileslink.sh` → `claude-code.sh` → `session-sync-init.sh` (if Claude Code installed) → `keychain.sh` → `nvm.sh` → `install-obsolete.sh` → (`--base`/`--full`: `install-base.sh` → `claude-usage-widget.sh`) → (`--develop`/`--full`: `install-develop.sh` → `vscode.sh`)

`install.ps1` (Windows) runs scripts in this order: `dotfileslink.ps1` → `claude-code.ps1` → `session-sync-init.ps1` (if Claude Code installed) → `fnm.ps1` → `install-obsolete.ps1` → `sounds.ps1` → (`-Base`/`-Develop`/`-Toolchain`/`-Full`: `starship.ps1` → `uv.ps1` → `google-japanese-input.ps1` → `autohotkey.ps1` → `powertoys.ps1` → `claude-usage-widget.ps1` → `claude-tabs.ps1`) → (`-Develop`/`-Toolchain`/`-Full`: `awscli.ps1` → `vscode.ps1`) → (`-Toolchain`/`-Full`: `vs-cpp.ps1`)

See [README.md](../README.md) for full platform-specific installation instructions.

---

## 7. Claude Code Configuration

See [architecture/claude-code.md](architecture/claude-code.md) for the workflow state machine, session sync, settings.json design, and test iteration workflow.

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
| Claude Code edit | `claude-global/hooks/scan-outbound.js` (PreToolUse) | Scans Edit/Write content |

Both call `bin/scan-outbound.sh` (single source of truth for detection patterns).

**Detection patterns**: RFC 1918 IPv4 (`10.x`, `172.16-31.x`, `192.168.x`), email addresses, MAC addresses, absolute local paths (`/Users/`, `/home/`, `C:\Users\`). Additional patterns via `.private-info-blocklist`.

**Exception handling**: `.private-info-allowlist` for known-safe patterns (e.g., `git@github.com` SSH URLs, `noreply.github.com` email). Environment-specific exceptions can be added to `../dotfiles-private/.private-info-allowlist`. File-scoped patterns support glob matching (e.g., `tests/*:@example.com`).

**Private repo detection**: non-GitHub hosts (GitLab, Bitbucket, GitHub Enterprise, etc.) are automatically treated as private and skip scanning entirely. For `github.com` remotes, visibility is checked via `gh api repos/{owner}/{repo}`. Private repos are not scanned. If `gh` is unavailable or the API call fails, scanning proceeds (fail-open, safe default). Shared logic in `claude-global/hooks/lib/is-private-repo.js` (Node.js hooks) and inline host extraction (git hooks).

**Additional patterns** (personal information, hardware model numbers, hostnames, etc.) can be detected by adding regex patterns to `.private-info-blocklist`.

For detailed usage, see [scan-outbound.md](scan-outbound.md).

---

## 11. References

- Dotfiles repository: https://github.com/nirecom/dotfiles
- Zinit: https://github.com/zdharma-continuum/zinit
- Starship: https://starship.rs/
- Entware (QNAP): https://github.com/Entware/Entware
- EditorConfig: https://editorconfig.org/
- Claude Code: https://docs.anthropic.com/en/docs/claude-code
