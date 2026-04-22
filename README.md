# dotfiles

Cross-platform dotfiles and a custom [Claude Code](https://docs.anthropic.com/en/docs/claude-code) framework — shipped together as a single installer.

## What You Get

**Dotfiles** — `install.sh` / `install.ps1` sets up shell, editor, git, and prompt configuration across Ubuntu (native and WSL2), macOS (Intel and Apple Silicon), Windows, and even QNAP NAS. A single `~/.profile_common` drives all Unix variants; OS detection branches automatically with no hardcoded platform paths.

**Claude Code framework** — `claude-global/` adds a structured AI-assisted development workflow on top of Claude Code: hook-enforced step tracking from research to commit, cross-machine session continuity, standards-backed test and security guidance, and TDD via subagent isolation.

## Claude Code Framework

The `claude-global/` directory is a custom Claude Code framework installed as symlinks under `~/.claude/` by the platform-specific `dotfileslink` script. See [docs/architecture/claude-code.md](docs/architecture/claude-code.md) for implementation details.

### Hook-enforced end-to-end workflow

Most agent frameworks rely on the model to remember process steps. This framework encodes the dev workflow — research → plan → write-tests → code → run-tests → security-review → docs → user-verification — as a per-session state machine, and a PreToolUse hook physically blocks `git commit` until every required step completes or is explicitly skipped with a reason.

- **Evidence-based completion**: staged `tests/` and `docs/*.md` files automatically satisfy their corresponding steps — no manual marker required.
- **State inheritance**: after context compaction or a fresh session on the same cwd+branch, prior workflow state is inherited so progress is not lost.
- **Docs-only short-circuit**: when every staged file is human-facing documentation (any `.md` under `docs/`, or root `README.md`/`CHANGELOG.md`/`CONTRIBUTING.md`/`LICENSE.md`), steps 1–6 are bypassed automatically for documentation-only commits.

### Cross-machine session continuity

Claude Code indexes projects by absolute path, which normally breaks session resume across machines with different usernames. This framework normalizes project paths to drive-root form (`C:\git\`, `/git/`) and syncs `~/.claude/projects/` through a private GitHub repo — so a conversation started on Windows can be resumed on macOS/Linux, and vice versa. Session history is pulled automatically on terminal startup and pushed after VS Code window close.

### Standards-backed testing and security

Rather than generic "write tests" guidance, the framework provides concrete test categories — Normal, Error, Edge, Idempotency, and Security — with explicit citations: OWASP ASVS V8, OWASP WSTG Input Validation, CWE Top 25, OWASP LLM Top 10, and MCP Top 10. Test layer selection (unit / narrow integration / broad integration / smoke) follows Martin Fowler's narrow/broad integration distinction and Kent C. Dodds' Testing Trophy. Security skills apply the same references at both design time (`/review-plan-security`) and implementation time (`/review-code-security`).

### TDD via subagent isolation

Test writing runs in a `mode: "auto"` subagent that loops write → run → fix autonomously, restricted to test files only. This reduces user confirmations from O(N) per-edit approvals to exactly two: test plan approval and final review.

### Private information scanning

Two checkpoints prevent private information from reaching public repositories: a `git pre-commit` hook and a Claude Code PreToolUse hook. Both detect RFC 1918 IP addresses, email addresses, MAC addresses, absolute local paths, hard-coded secrets (AWS, Anthropic, OpenAI, GitHub, Slack, and others), PEM private keys, and Trojan Source hidden Unicode characters. Repositories identified as private via `gh api` are skipped automatically. See [docs/scan-outbound.md](docs/scan-outbound.md) for details.

## Supported Platforms

- Ubuntu (native or WSL2)
- macOS (Intel / Apple Silicon)
- Windows (native, for git config and editorconfig)
- QNAP NAS (minimal set via Entware)

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
├── bin/                     # Utility scripts (detectos.sh, etc.)
├── config/
│   └── vscode-extensions.txt  # VS Code extensions (shared across platforms)
├── claude-global/             # Claude Code framework
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
