# Shell Startup Flow

## Linux/macOS (Zsh)

```
login
  → .zshrc
    → source detectos.sh (sets $OSDIST, $ISWSL, $ISM1)
    → source .profile_common (aliases, PATH, tool init)
    → Zinit plugins (autosuggestions, syntax-highlighting, completions)
    → Starship prompt
```

## Linux/macOS (Bash)

```
login
  → .bash_profile
    → source detectos.sh
    → source .profile_common
    → PS1 with __git_ps1
    → git auto-pull (fast-forward only, first shell only)
      → If diverged (force push): prompt y/N to reset (marker file skips)
```

## Windows PowerShell (PS5 / PS7)

```
PowerShell startup
  → Microsoft.PowerShell_profile.ps1 (symlink → install/win/profile.ps1)
    → ssh-agent start + load all keys ($HOME\.ssh\id_*)
    → git fetch + merge --ff-only (auto-pull dotfiles, 3s timeout)
    → Repair broken file symlinks (atomic save detection, ~20ms)
    → ~/.local/bin PATH addition
    → Starship prompt init
    → fnm init (try/catch for SAC App Control)
```

## QNAP NAS

```
SSH login (shell: /bin/sh)
  → ~/.profile (symlink → .profile_qnap)
    → exec bash -l (dynamic path resolution: command -v bash)
      → ~/.bash_profile
        → source detectos.sh (OSDIST=qnap)
        → source .profile_common (Entware PATH, aliases, git-prompt)
        → PS1 (raw readline markers \001/\002 for ANSI escape handling)
```
