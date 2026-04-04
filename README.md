# dotfiles

Personal dotfiles and install scripts for cross-platform development environments.

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
├── claude-global/             # Claude Code global settings
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

## Private Information Scanning

Automated scanning prevents private information (IP addresses, email addresses, etc.) from being committed to public repositories. See [docs/private-info-scanning.md](docs/private-info-scanning.md) for details.
