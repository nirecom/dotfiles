# history/legacy.md Entry Classification

Classification for `docs/history/legacy.md` → agents repo split.
Tags: `@claude` = move to agents repo | `@dotfiles` = keep in dotfiles repo | `@both` = appears in both

| # | Subject (digest) | Tag |
|---|-----------------|-----|
| 1 | FEATURE: Initial setup | @dotfiles |
| 2 | FEATURE: Zsh migration | @dotfiles |
| 3 | FEATURE: Git config relocation | @dotfiles |
| 4 | FEATURE: Emacs enhancement | @dotfiles |
| 5 | REFACTOR: OS detection refactor | @dotfiles |
| 6 | FEATURE: Apple Silicon support | @dotfiles |
| 7 | FEATURE: Repository merge | @dotfiles |
| 8 | FEATURE: Windows support | @dotfiles |
| 9 | INCIDENT #1: dotfileslink.sh aborts with set -e | @dotfiles |
| 10 | INCIDENT #2: Symlink creation fails on PowerShell 5 | @dotfiles |
| 11 | FEATURE: Starship introduction | @dotfiles |
| 12 | FEATURE: Claude Code management | @claude |
| 13 | FEATURE: PowerShell curl issue | @dotfiles |
| 14 | FEATURE: Claude Code commands | @claude |
| 15 | FEATURE: QNAP vim plugins | @dotfiles |
| 16 | FEATURE: QNAP support | @dotfiles |
| 17 | INCIDENT #3: Claude Code installed on QNAP | @claude |
| 18 | INCIDENT #4: arch: command not found on QNAP | @dotfiles |
| 19 | INCIDENT #5: No colored prompt on QNAP SSH | @dotfiles |
| 20 | INCIDENT #6: .profile_qnap exec fails | @dotfiles |
| 21 | INCIDENT #7: git: command not found on QNAP | @dotfiles |
| 22 | INCIDENT #8: Cursor position misaligned on QNAP | @dotfiles |
| 23 | BUGFIX: QNAP Entware fix | @dotfiles |
| 24 | BUGFIX: QNAP prompt wrapping fix | @dotfiles |
| 25 | INCIDENT #9: git disappears after QNAP reboot | @dotfiles |
| 26 | INCIDENT #10: Long input overwrites prompt on QNAP bash | @dotfiles |
| 27 | INCIDENT #11: E117: Unknown function: pathogen#infect on QNAP vim | @dotfiles |
| 28 | FEATURE: Claude Code skill management | @claude |
| 29 | SECURITY: Claude Code security hardening | @claude |
| 30 | FEATURE: Stale symlink detection (Windows) | @dotfiles |
| 31 | INCIDENT #12: No colored prompt on Windows PowerShell (one PC only) | @dotfiles |
| 32 | BUGFIX: Claude Code deny rule false-positive fix | @claude |
| 33 | INCIDENT #13: git add CLAUDE.md auto-denied by Claude Code | @claude |
| 34 | FEATURE: AutoHotkey integration | @dotfiles |
| 35 | FEATURE: Starship install guard | @dotfiles |
| 36 | INCIDENT #14: .config/git/config always dirty after shell startup | @dotfiles |
| 37 | FEATURE: fnm install script | @dotfiles |
| 38 | BUGFIX: Starship git timeout fix | @dotfiles |
| 39 | FEATURE: uv install scripts | @dotfiles |
| 40 | FEATURE: Claude Code PreToolUse hook investigation | @claude |
| 41 | FEATURE: Windows notification muting | @dotfiles |
| 42 | FEATURE: Claude Code git write permissions | @claude |
| 43 | FEATURE: Claude Code permissions refinement | @claude |
| 44 | REFACTOR: claude-code → claude-global rename | @both |
| 45 | BUGFIX: uv install PATH fix | @dotfiles |
| 46 | INCIDENT #15: uv --version fails after fresh install | @dotfiles |
| 47 | FEATURE: Profile hardening (Windows) | @dotfiles |
| 48 | REFACTOR: Claude Code rules reorganization | @claude |
| 49 | REFACTOR: Claude Code commands → skills migration | @claude |
| 50 | INCIDENT #16: New-Item -ItemType SymbolicLink error on PS5 startup | @dotfiles |
| 51 | FEATURE: Windows symlink auto-repair | @dotfiles |
| 52 | FEATURE: Docs enforcement via hook | @claude |
| 53 | FEATURE: Docs convention rule | @claude |
| 54 | FEATURE: Claude Code PermissionRequest hook | @claude |
| 55 | SECURITY: Private info leak prevention | @claude |
| 56 | REFACTOR: master → main cleanup | @dotfiles |
| 57 | FEATURE: PowerToys Keyboard Manager | @dotfiles |
| 58 | FEATURE: VS 2022 C++ dev tools | @dotfiles |
| 59 | SECURITY: Dotenv access blocking | @claude |
| 60 | REFACTOR: claude-code migration cleanup | @claude |
| 61 | FEATURE: Markdown exempt from code detection in hooks | @claude |
| 62 | FEATURE: Unified Edit confirmation flow | @claude |
| 63 | FEATURE: Private repo detection: non-GitHub hosts skip scanning | @claude |
| 64 | INCIDENT #17: Test execution hangs indefinitely | @claude |
| 65 | INCIDENT #18: install.sh doesn't reach Rize / Claude Usage Widget steps | @dotfiles |
| 66 | INCIDENT #19: Session sync init deletes other machines' sessions | @claude |
| 67 | INCIDENT #20: Session sync propagates Claude Code format migration deletions | @claude |
| 68 | REFACTOR: Private repo detection: static list → dynamic gh API | @claude |
| 69 | REFACTOR: commands → skills migration cleanup | @claude |
| 70 | FEATURE: Claude Code branch/push delete deny rules | @claude |
| 71 | FEATURE: dotfiles clone recovery after git rebase | @dotfiles |
| 72 | FEATURE: Claude Tabs installer | @dotfiles |
| 73 | REFACTOR: Node.js version manager: platform split (fnm → nvm on Unix) | @dotfiles |
| 74 | FEATURE: Keychain SSH key auto-detect | @dotfiles |
| 75 | FEATURE: Claude Code session sync | @claude |
| 76 | FEATURE: Cross-platform check hook | @claude |
| 77 | REFACTOR: ~/dotfiles → C:\git\dotfiles path unification | @dotfiles |
| 78 | FEATURE: Session sync git root relocation | @claude |
| 79 | FEATURE: AutoHotkey per-user path fallback | @dotfiles |
| 80 | FEATURE: Session sync: terminal startup fetch + codes function | @claude |
| 81 | FEATURE: Session sync: cross-platform support | @claude |
| 82 | BUGFIX: Session sync: init/push reliability fix | @claude |
| 83 | FEATURE: Session sync: separate init from sync, add reset action | @claude |
| 84 | FEATURE: Session sync: history.jsonl sync and mtime restore | @claude |
| 85 | FEATURE: Session sync: Claude Code format migration causes bulk deletion | @claude |
| 86 | FEATURE: Notification hook for permission_prompt | @claude |
| 87 | FEATURE: Add VS Code and extensions installer | @dotfiles |
| 88 | FEATURE: Auto-unstage effortLevel-only changes in settings.json | @claude |
| 89 | FEATURE: Add research phase to workflow | @claude |
| 90 | BUGFIX: Fix update-docs skill to detect uncommitted changes | @claude |
| 91 | REFACTOR: Workflow rules reorganization | @claude |
| 92 | FEATURE: Add write-tests and make-plan skills with effort: high | @claude |
| 93 | FEATURE: Force push divergence detection on shell startup | @dotfiles |
| 94 | FEATURE: Add -Toolchain option and AWS CLI installer for Windows | @dotfiles |
| 95 | BUGFIX: Fix codes function to survive terminal close | @dotfiles |
| 96 | BUGFIX: Fix codes session sync not firing per-window | @claude |
| 97 | BUGFIX: Fix codes multi-instance support | @dotfiles |
| 98 | FEATURE: Remove container extensions from VS Code auto-install | @dotfiles |
| 99 | SECURITY: Security enhancement research and planning | @claude |
| 100 | BUGFIX: Installer robustness and option hierarchy fix | @dotfiles |
| 101 | FEATURE: Reduce WebSearch/WebFetch permission prompts | @claude |
| 102 | FEATURE: Restructure /update-docs skill for ai-specs project coverage | @claude |

## Summary

| Tag | Count |
|-----|-------|
| @claude | 46 |
| @dotfiles | 55 |
| @both | 1 |
| **Total** | **102** |
