# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Personal development environment provisioning repository. Automates OS configuration, tool installation, dotfiles setup, and language/framework environments across Ubuntu 20.04 LTS (native or WSL2), macOS, and Git for Windows.

## Installation Flow

```
# 1. OS-level setup (run as ubuntu/ec2-user)
./os-init.sh        # Updates packages, creates user, configures sudoers/SSH

# 2. Home directory setup (run as nire user)
./home-init.sh      # Orchestrates: brew-git → awscli → dotfiles → fnm → zsh → vim → tmux

# 3. Optional development tools
./home-develop.sh   # Installs fnm, optionally kotlin/ruby/flutter

# 4. Cleanup deprecated configs
./home-obsolete.sh
```

Any script can also be run independently.

## Architecture

**OS Detection**: All scripts source `./bin/detectos.sh` which sets:
- `OSDIST` (macos, ubuntu, centos, amazon, mingw)
- `ISWSL` (1 if WSL2)
- `ISM1` (1 if ARM64 Mac)

**Dependency chain**: `os-init.sh` (system) → `home-init.sh` (user env) → `home-develop.sh` (dev tools)

**External integrations**:
- Dotfiles linked from `nirecom/dotfiles` repository
- SSH keys retrieved from AWS S3 bucket `nirecom-home`

**Key directories**:
- `bin/` - Shared utilities (detectos.sh)
- `react/` - React/Next.js project scaffolding scripts
- `notused/` - Deprecated scripts (anyenv, nvm, docker, etc.)

## Shell Script Conventions

- Shebang: `#!/bin/bash`
- Source OS detection at top: `source ./bin/detectos.sh`
- Uppercase variable names (USERNAME, OSDIST, BUCKET)
- Script names: lowercase with hyphens (e.g., `home-init.sh`, `fnm.sh`)
- Some scripts use `set -e` for fail-fast
- OS-specific logic handled via case statements on `$OSDIST`
- Scripts check if a tool is already installed before re-installing

## Git Commit Style

Concise, descriptive messages. Tool-prefixed when relevant (e.g., "fnm: Install scripts does rehash..."). Focus on what changed and why.
