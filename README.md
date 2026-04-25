# dotfiles

Cross-platform dotfiles and install scripts for shell, editor, git, and prompt configuration — Ubuntu (WSL2), macOS, Windows, and QNAP NAS.

## What You Get

`install.sh` / `install.ps1` sets up shell, editor, git, and prompt configuration across Ubuntu (native and WSL2), macOS (Intel and Apple Silicon), Windows, and even QNAP NAS. A single `~/.profile_common` drives all Unix variants; OS detection branches automatically with no hardcoded platform paths.

## Companion: Claude Code Framework

**[nirecom/agents](https://github.com/nirecom/agents)** is a standalone [Claude Code](https://docs.anthropic.com/en/docs/claude-code) framework that pairs naturally with this repo:

- **Hook-enforced workflow** — encodes research → plan → write-tests → code → run-tests → security-review → docs → user-verification as a per-session state machine; a PreToolUse hook physically blocks `git commit` until every step completes.
- **Cross-machine session continuity** — normalizes project paths to drive-root form and syncs `~/.claude/projects/` through a private GitHub repo, so a conversation started on Windows resumes on macOS/Linux.
- **Standards-backed testing and security** — concrete test categories (Normal, Error, Edge, Idempotency, Security) with explicit OWASP ASVS V8, CWE Top 25, OWASP LLM Top 10 citations; test-layer selection follows Martin Fowler's narrow/broad integration distinction.
- **TDD via subagent isolation** — test writing runs in an autonomous `mode: "auto"` subagent restricted to test files only; confirmations drop from O(N) edits to two.
- **Private information scanning** — dual-checkpoint (pre-commit hook + Claude Code PreToolUse hook) scanning for IP addresses, secrets, local paths, and Trojan Source Unicode.

When `~/agents/` is present as a sibling of `~/dotfiles/`, the dotfiles installer wires it in automatically.

## Supported Platforms

- Ubuntu (native or WSL2)
- macOS (Intel / Apple Silicon)
- Windows (native, for git config and editorconfig)
- QNAP NAS (minimal set via Entware)

## Requirements

### Required (Linux / macOS)

| Tool | Purpose |
|------|---------|
| `bash` | `install.sh` and all `install/linux/*.sh` scripts |
| `git` | Repo clone; symlinks reference repo paths |
| `curl` | Bootstrap fetches (nvm, uv, starship, awscli installers) |

> sudo access is required for apt/Homebrew installs (`--base` and above).

### Required (Windows)

| Tool | Purpose |
|------|---------|
| PowerShell 5+ (7+ recommended) | `install.ps1` and all `install/win/*.ps1` |
| `winget` | All Windows package installs (fnm, pwsh, starship, uv, vscode, …) |

> Symlink creation requires Developer Mode (Settings → System → For developers) or Administrator privileges.

### Required (QNAP NAS)

| Tool | Purpose |
|------|---------|
| Entware (`opkg`) | Install `git`, `bash`, `vim-full`, `curl` before clone (see Install § QNAP) |

### Optional

| Linux | Windows | Adds |
|-------|---------|------|
| `--base` | `-Base` | zsh, vim, tmux, starship, source-highlight, AWS CLI v2, Google Japanese Input, AutoHotkey v2, PowerToys |
| `--develop` | `-Develop` | uv (Python), VS Code + extensions, Claude Tabs, Claude Usage Widget |
| `--full` | `-Full` | All of the above + Visual Studio C++ build tools (Windows) |

Note: no-flag install also sets up Node.js (nvm on Linux/macOS, fnm on Windows). nvm is pinned to v0.40.4 (`install/linux/nvm.sh`).

## Install

### Linux / macOS

```bash
git clone git@github.com:nirecom/dotfiles.git ~/dotfiles
cd ~/dotfiles
./install.sh            # Symlinks only
./install.sh --base     # Symlinks + base packages
./install.sh --develop  # Symlinks + dev tools
./install.sh --full     # Symlinks + base + dev tools
```

### Windows (PowerShell)

```powershell
git clone https://github.com/nirecom/dotfiles.git $HOME\dotfiles
cd $HOME\dotfiles
.\install.ps1            # Symlinks only
.\install.ps1 -Base      # Symlinks + base packages
.\install.ps1 -Develop   # Symlinks + dev tools
.\install.ps1 -Full      # Symlinks + base + dev tools
```

Requires Developer Mode (Settings > System > For developers) or Administrator privileges. On PowerShell 5, the execution policy is automatically set to `RemoteSigned` via the registry.

### QNAP NAS

**Initial setup**:

1. Install Entware from QNAP App Center
2. Install required packages: `opkg install git git-http bash vim-full curl`
3. Clone and install:
   ```bash
   git clone git@github.com:nirecom/dotfiles.git ~/dotfiles
   cd ~/dotfiles
   ./install.sh
   ```
   QNAP is auto-detected — only the minimal symlink set is deployed.
4. Enable the Entware QPKG: `sudo /sbin/setcfg Entware Enable TRUE -f /etc/config/qpkg.conf`

**Post-reboot auto-recovery**: QTS automatically runs the Entware startup script (`Entware.sh start`) for enabled QPKGs, which creates the `/opt` symlink to the Entware installation on the storage volume. Packages persist across reboots.

**autorun.sh fallback**: `dotfileslink.sh` deploys `autorun.sh` to the flash config partition via `hal_app`. Enable "Run user defined startup processes" in QNAP Control Panel > Hardware > General (one-time manual step, required on QTS 4.3.3+). This handles auto-recovery if Entware is removed by a firmware update.

## Repository Structure

```
dotfiles/
├── .config/
│   ├── git/                 # Git config and global gitignore
│   ├── starship.toml        # Starship prompt (Linux/macOS)
│   └── starship-powershell.toml  # Starship prompt (Windows)
├── .emacs.d/                # Emacs config
├── bin/                     # OS detection, tmux launch, VS Code window utilities
├── config/
│   └── vscode-extensions.txt  # VS Code extensions (shared across platforms)
├── docs/                    # Architecture and history documentation
├── source-highlight/        # GNU source-highlight config
├── install/
│   ├── linux/               # Linux/macOS install scripts
│   ├── win/                 # Windows install scripts
│   └── qnap/                # QNAP NAS install scripts
├── install.sh               # Unified installer (Linux/macOS)
├── install.ps1              # Unified installer (Windows)
├── .bash_profile
├── .editorconfig
├── .inputrc
├── .profile_common          # Shared shell config (aliases, PATH, tools)
├── .profile_qnap            # QNAP sh-to-bash bootstrap
├── .tmux.conf
├── .vimrc
└── .zshrc
```
