# Sibling Repos

dotfiles, dotfiles-private, and agents are sibling repos that work together.
Their installers write to overlapping files, which makes knowledge fragmentation easy to miss.

## Shared files — check sibling repos' history.md before touching these

| File | Owner | Notes |
|------|-------|-------|
| `~/.gitconfig` | agents | intentionally written for core.hooksPath (d8b7ee7) |
| `~/.config/git/config` | dotfiles | XDG global git config — tracked, do not write directly |
| `~/.config/git/config.local` | dotfiles-private | symlink target is tracked — do not write directly |
| `~/.local/bin/` | dotfiles + agents | launcher generation target |
| `~/.claude/` | agents | settings.json, skills/, rules/, etc. |

## install-obsolete rules

Before deleting or modifying a file in install-obsolete.{ps1,sh}:
1. Verify the file is not intentionally created or written by a sibling repo's installer.
2. Check with `git -C <sibling-repo> log --all -p -- <file-pattern>` or read the sibling's history.md.

## Cross-repo entry policy

When a decision in one repo affects another repo's behavior:
- Add a pointer entry to the affected repo's `docs/history.md` explaining the rationale.
- Example: agents changes hooksPath write destination → record the policy change in dotfiles history.md.
