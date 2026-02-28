# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles and install scripts repository for cross-platform use (Ubuntu/WSL2, macOS Intel & Apple Silicon, Windows native). Configs are symlinked into `$HOME` via `install/linux/dotfileslink.sh` (Linux/macOS) or `install/win/dotfileslink.ps1` (Windows).

## Installation

```bash
git clone git@github.com:nirecom/dotfiles.git ~/dotfiles
cd ~/dotfiles
./install.sh          # Symlinks only
./install.sh --full   # Symlinks + package installation
```

## Architecture

### Shell Configuration

- **Primary shell:** Zsh (`.zshrc`) with Zinit plugin manager
- **Shared config:** `.profile_common` — the main configuration file (aliases, PATH, tool setup). Sourced by both `.zshrc` and `.bash_profile`.
- **OS detection:** `bin/detectos.sh` sets `$OSDIST` (macos/ubuntu/amazon/centos/mingw), `$ISWSL`, `$ISM1`. Used throughout `.profile_common` for conditional config via `case "$OSDIST"`.

### Key Patterns

- **Defensive loading:** All external tools/files are checked before use (`type cmd >/dev/null 2>&1`, `[ -e $FILE ]`)
- **Cross-platform branching:** Feature setup branches on `$OSDIST` — always maintain this pattern when adding new tool configurations
- **Modular Emacs:** `.emacs.d/inits/` uses numbered prefixes (20_, 30_, 50_, etc.) loaded by `init-loader.el`
- **Claude Code config:** `claude-code/` stores global Claude Code settings (CLAUDE.md, settings.json). Symlinked to `~/.claude/` by install scripts. Named `claude-code/` (not `.claude/`) to avoid conflict with project-level settings

### EditorConfig

Default: 4-space indent, LF, UTF-8. TypeScript/JavaScript/JSON/YAML/CSS use 2-space indent.

## Key Aliases (defined in .profile_common)

- Docker: `d`, `dx`, `dc`, `dcb`, `dcu`, `dcub`, `dcd`
- Git: `g`, `gd`, `gs`, `gl`, `gp`, `gr`
- Kubernetes: `k`
- `gitpush()` function: add → commit → push workflow

## Git Config

Located at `.config/git/config` and `.config/git/ignore` (not `~/.gitconfig`). Default branch is `main`. Global gitignore covers node_modules, .class, Terraform state, editor temps, OS files, and AI tool artifacts.
