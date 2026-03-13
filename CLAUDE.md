# CLAUDE.md

## Overview

Personal dotfiles and install scripts repository for cross-platform use (Ubuntu/WSL2, macOS Intel & Apple Silicon, Windows native, QNAP NAS). Configs are symlinked into `$HOME` via `install/linux/dotfileslink.sh` (Linux/macOS) or `install/win/dotfileslink.ps1` (Windows).

## Installation

```bash
git clone git@github.com:nirecom/dotfiles.git ~/dotfiles
cd ~/dotfiles
./install.sh          # Symlinks only
./install.sh --full   # Symlinks + package installation
```

```powershell
# Windows (PowerShell)
.\install.ps1         # Symlinks only
.\install.ps1 -Full   # Symlinks + additional setup
```

## Architecture

### Shell Configuration

- **Primary shell:** Zsh (`.zshrc`) with Zinit plugin manager
- **Shared config:** `.profile_common` — the main configuration file (aliases, PATH, tool setup). Sourced by both `.zshrc` and `.bash_profile`.
- **OS detection:** `bin/detectos.sh` sets `$OSDIST` (macos/ubuntu/amazon/centos/qnap/mingw), `$ISWSL`, `$ISM1`. Used throughout `.profile_common` for conditional config via `case "$OSDIST"`.

### Key Patterns

- **Defensive loading:** All external tools/files are checked before use (`type cmd >/dev/null 2>&1`, `[ -e $FILE ]`)
- **Cross-platform branching:** Feature setup branches on `$OSDIST` — always maintain this pattern when adding new tool configurations
- **Modular Emacs:** `.emacs.d/inits/` uses numbered prefixes (20_, 30_, 50_, etc.) loaded by `init-loader.el`
- **Claude Code config:** `claude-global/` stores global Claude Code settings (CLAUDE.md, settings.json). Symlinked to `~/.claude/` by install scripts. Named `claude-global/` (not `.claude/`) to avoid conflict with project-level settings

## Documentation

- `docs/architecture.md` — detailed architecture, design principles, file responsibilities, and OS detection logic
- `docs/history.md` — change history and incident log with commit references