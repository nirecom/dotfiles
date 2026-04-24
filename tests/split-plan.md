# Tests Split Plan

76 test files classified for the dotfiles → agents repo split.
`@claude` = move to agents repo | `@dotfiles` = keep in dotfiles repo | `@both` = move to agents, rewrite to make `$DOTFILES_DIR` optional

## Bash (.sh) — 52 files

| Filename | Tag | Reason |
|----------|-----|--------|
| main-block-dotenv.sh | @claude | PreToolUse hook for .env blocking |
| main-check-cross-platform.sh | @claude | PreToolUse hook for cross-platform detection |
| main-check-japanese-in-docs.sh | @claude | PreToolUse hook for Japanese-in-docs check |
| main-check-tests-updated.sh | @claude | PreToolUse hook check-tests-updated.js |
| main-claude-agents-symlink.sh | @claude | agents symlink in dotfileslink.sh |
| main-commit-push-workflow-gate.sh | @claude | commit-push skill workflow-gate regression |
| main-effort-level-unstage.sh | @claude | pre-commit effortLevel auto-unstage hook |
| main-env-file-block.sh | @claude | pre-commit .env blocking hook |
| main-explicit-model-opus.sh | @claude | reasoning-heavy skills require Opus model |
| main-hard-secret-scan.sh | @claude | scan-outbound.sh hard-secret pattern detection |
| main-hidden-char-scan.sh | @claude | scan-outbound.sh hidden/bidi-override char detection |
| main-install-obsolete-workflow-migration.sh | @claude | workflow migration cleanup logic |
| main-private-allowlist-external.sh | @claude | scan-outbound.sh external allowlist loading |
| main-private-blocklist-external.sh | @claude | scan-outbound.sh external blocklist loading |
| main-private-repo-detection.sh | @claude | is-private-repo.js dynamic detection |
| main-review-code-security.sh | @claude | review-code-security skill structural tests |
| main-review-plan-security.sh | @claude | review-plan-security skill structural tests |
| main-save-research-skill.sh | @claude | save-research skill structural tests |
| main-scan-inbound.sh | @claude | scan-inbound.js prompt injection detection |
| main-session-sync.sh | @claude | bin/session-sync.sh and session-sync-init.sh |
| main-workflow-and-chain.sh | @claude | workflow-mark.js && chain handling |
| main-workflow-evidence.sh | @claude | evidence-based write_tests/docs enforcement |
| main-workflow-gate-regex.sh | @claude | workflow-gate.js commit detection regex |
| main-workflow-migration.sh | @claude | workflow-state.js readState migration |
| main-workflow-run-tests.sh | @claude | workflow-run-tests.js PostToolUse hook |
| main-workflow-skip-sentinels.sh | @claude | WORKFLOW_*_NOT_NEEDED skip sentinels |
| main-workflow-state-machine.sh | @claude | workflow state machine integration tests |
| feature-agents-repo-split.sh | @claude | agents repo split step 2–4 smoke tests |
| feature-block-tests-direct.sh | @claude | block-tests-direct.js PreToolUse hook |
| feature-doc-append-wrapper-permissions-static.sh | @claude | doc-append wrapper permissions static validation |
| feature-doc-append-wrapper.sh | @claude | bin/doc-append wrapper integration tests |
| feature-make-plan-research-escalation.sh | @claude | make-plan research escalation contract tests |
| feature-permission-hook2.sh | @claude | permission-hook2 scanner and PermissionRequest |
| feature-robust-workflow.sh | @claude | workflow state machine (gate + session-start) |
| feature-robust-workflow-settings.sh | @claude | workflow settings.json structural tests |
| feature-workflow-inherit-state.sh | @claude | session-start.js state inheritance TDD |
| fix-workflow-gate-unix-path.sh | @claude | workflow-gate.js path resolution functions |
| main-claude-tabs.sh | @dotfiles | claude-tabs.ps1 installer |
| main-claude-usage-widget.sh | @dotfiles | claude-usage-widget.sh installer |
| main-convert-history-table.sh | @dotfiles | bin/convert-history-table.py |
| main-dotfileslink-dir-backup.sh | @dotfiles | dotfileslink.sh directory backup/symlink |
| main-force-push-reset.sh | @dotfiles | force-push detection in .profile_common |
| main-git-fetch-sync.sh | @dotfiles | git fetch+merge sync pattern |
| main-installer-idempotency.sh | @dotfiles | installer idempotency checks |
| main-keychain-ssh-agent.sh | @dotfiles | SSH keychain agent integration |
| main-language-policy.sh | @dotfiles | rule file migration to dotfiles-private |
| main-profile-codes.sh | @dotfiles | codes function in .profile_common |
| main-remove-master-migration.sh | @dotfiles | master→main migration cleanup |
| main-sort-history.sh | @dotfiles | bin/sort-history.py |
| main-translate-history.sh | @dotfiles | bin/translate-history.py |
| main-vscode.sh | @dotfiles | VS Code installer scripts |
| main-wait-vscode-window.sh | @dotfiles | bin/wait-vscode-window.sh |

## PowerShell (.Tests.ps1) — 24 files

| Filename | Tag | Reason |
|----------|-----|--------|
| main-claude-agents-symlink.Tests.ps1 | @claude | agents symlink in dotfileslink.ps1 |
| main-session-sync.Tests.ps1 | @claude | session-sync-init.ps1 and session-sync.ps1 |
| main-autohotkey-shortcut-migration.Tests.ps1 | @dotfiles | AutoHotkey shortcut migration logic |
| main-awscli.Tests.ps1 | @dotfiles | awscli.ps1 installer |
| main-claude-usage-widget.Tests.ps1 | @dotfiles | claude-usage-widget.ps1 installer |
| main-dotfileslink-dir-backup.Tests.ps1 | @dotfiles | dotfileslink.ps1 directory backup/symlink |
| main-force-push-reset.Tests.ps1 | @dotfiles | force-push diverge detection in profile.ps1 |
| main-input-hotkey-detection.Tests.ps1 | @dotfiles | keyboard hotkey detection in install.ps1 |
| main-input-language-hotkey.Tests.ps1 | @dotfiles | input language hotkey disabling |
| main-install-obsolete-gitconfig.Tests.ps1 | @dotfiles | ~/.gitconfig deletion logic |
| main-install-obsolete-migration.Tests.ps1 | @dotfiles | directory migration cleanup |
| main-powertoys.Tests.ps1 | @dotfiles | PowerToys install script |
| main-profile-codes.Tests.ps1 | @dotfiles | codes function in profile.ps1 |
| main-profile-fnm-cd.Tests.ps1 | @dotfiles | fnm cd wrapper in profile.ps1 |
| main-profile-ssh-keys.Tests.ps1 | @dotfiles | SSH key discovery in profile.ps1 |
| main-pwsh.Tests.ps1 | @dotfiles | pwsh.ps1 installer |
| main-snipping-tool.Tests.ps1 | @dotfiles | Snipping Tool notification disabling |
| main-symlink-repair.Tests.ps1 | @dotfiles | symlink repair in dotfileslink.ps1 |
| main-uv-python.Tests.ps1 | @dotfiles | uv python install step |
| main-vs-cpp.Tests.ps1 | @dotfiles | vs-cpp.ps1 installer |
| main-wait-vscode-window.Tests.ps1 | @dotfiles | wait-vscode-window.ps1 utility |
| main-win-installer-options.Tests.ps1 | @dotfiles | install.ps1 parameter options |
| main-winget-install.Tests.ps1 | @dotfiles | winget error checking and MSI mutex |

## Summary

| Tag | Count |
|-----|-------|
| @claude | 39 |
| @dotfiles | 36 |
| @both | 0 |
| **Total** | **75** |

Note: `@both` entries (tests with dotfiles dependencies) would be moved to agents repo
and rewritten to make `$DOTFILES_DIR` an optional env var.
