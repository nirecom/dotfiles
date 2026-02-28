# dotfiles

Personal dotfiles and install scripts for cross-platform development environments.

## Supported Platforms

- Ubuntu (native or WSL2)
- macOS (Intel / Apple Silicon)
- Windows (native, for git config and editorconfig)

## Install

### Linux / macOS

```bash
git clone git@github.com:nirecom/dotfiles.git ~/dotfiles
cd ~/dotfiles
./install.sh          # Symlinks only
./install.sh --full   # Symlinks + package installation
```

### Windows (PowerShell)

```powershell
git clone https://github.com/nirecom/dotfiles.git $HOME\dotfiles
cd $HOME\dotfiles
.\install\win\install.ps1          # Symlinks only
.\install\win\install.ps1 -Full    # Symlinks + additional setup
```

Requires Developer Mode (Settings > System > For developers) or Administrator privileges.

## Repository Structure

```
dotfiles/
├── .config/git/         # Git config and global gitignore
├── .emacs.d/            # Emacs config
├── bin/                 # Utility scripts (detectos.sh, etc.)
├── source-highlight/    # GNU source-highlight config
├── install/
│   ├── linux/           # Linux/macOS install scripts
│   └── win/             # Windows install scripts
├── install.sh           # Unified installer (Linux/macOS)
├── .bash_profile
├── .editorconfig
├── .inputrc
├── .profile_common      # Shared shell config (aliases, PATH, tools)
├── .tmux.conf
├── .vimrc
└── .zshrc
```

## Migration from nirecom/install

The [nirecom/install](https://github.com/nirecom/install) repository has been merged into `install/linux/`. The separate install repo is no longer needed and can be deleted.
