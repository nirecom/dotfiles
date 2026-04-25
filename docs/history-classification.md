# history.md Entry Classification

Classification for `docs/history.md` → agents repo split.
Tags: `@claude` = move to agents repo | `@dotfiles` = keep in dotfiles repo | `@both` = appears in both

| # | Subject (digest) | Tag |
|---|-----------------|-----|
| 1 | SECURITY: check-private-info: allow Docker internal network and Node.js base image paths | @claude |
| 2 | REFACTOR: write-tests subagent: mode acceptEdits → default | @claude |
| 3 | FEATURE: session-sync push: retry loop for simultaneous-commit race condition | @claude |
| 4 | FEATURE: Workflow state machine: deterministic 8-step enforcement | @claude |
| 5 | INCIDENT #21: Hook integration path untested — CLAUDE_ENV_FILE format bug | @claude |
| 6 | FEATURE: Workflow State Machine: mark-step.js session ID resolution redesign | @claude |
| 7 | BUGFIX: Workflow State Machine: E2E verification + WSL path bug fix | @claude |
| 8 | BUGFIX: PostToolUse marker interception: fix mark-step.js in Bash subprocess context | @claude |
| 9 | INCIDENT #22: settings.json PostToolUse misplacement | @claude |
| 10 | FEATURE: Workflow State Machine: PostToolUse Windows E2E verified | @claude |
| 11 | FEATURE: Workflow State Machine: Robust Improvements — Windows verification | @claude |
| 12 | FEATURE: Workflow State Machine: .git/workflow deny rule verified | @claude |
| 13 | CONFIG: starship: increase command_timeout to 2000ms | @dotfiles |
| 14 | BUGFIX: Fix wait-vscode-window.sh: add WSL2 window detection | @dotfiles |
| 15 | BUGFIX: Fix QNAP dotfileslink.sh: guard autorun.sh install against mount failure | @dotfiles |
| 16 | REFACTOR: Workflow State Machine: marker format `:` → `_` | @claude |
| 17 | FEATURE: Workflow State Machine: cross-platform verification complete | @claude |
| 18 | FEATURE: session-sync: push conflict auto-resolution | @claude |
| 19 | INCIDENT #23: workflow-gate block message as bypass attractor | @claude |
| 20 | INCIDENT #24: Workflow State Machine — multi-repo state mismatch | @claude |
| 21 | FEATURE: Workflow State Machine: session-scoped state migration | @claude |
| 22 | FEATURE: Workflow State Machine — E2E verification across all 3 environments | @claude |
| 23 | FEATURE: Workflow State Machine — block user_verification direct bypass in mark-step.js | @claude |
| 24 | REFACTOR: install-obsolete: .git/workflow → ~/.claude/projects/workflow cleanup | @claude |
| 25 | BUGFIX: Fix install.ps1: missing registry values crash on keyboard hotkey setup | @dotfiles |
| 26 | REFACTOR: Workflow State Machine: mark-step.js removal | @claude |
| 27 | FEATURE: Workflow State Machine: evidence-based write_tests/docs enforcement | @claude |
| 28 | FEATURE: doc-append/doc-rotate tools + architecture.md split | @claude |
| 29 | BUGFIX: Fix: commit-push skill model directive removed | @claude |
| 30 | FEATURE: doc-rotate.py: line-count trigger and floor-based archiving | @claude |
| 31 | BUGFIX: Fix workflow-gate: Unix-style drive path normalization in resolveRepoDir | @claude |
| 32 | BUGFIX: fix: add uv call to install.sh and align --develop inclusion with Windows | @dotfiles |
| 33 | FEATURE: doc-append/doc-rotate + architecture.md split (2026-04-17, verification) | @claude |
| 34 | REFACTOR: install-obsolete: .git/workflow cleanup (2026-04-17, verified) | @claude |
| 35 | CONFIG: profile.ps1: suppress create/delete mode output in startup git pull | @dotfiles |
| 36 | FEATURE: Workflow State Machine: session ID injection and state inheritance across VS Code restarts | @claude |
| 37 | REFACTOR: mark-step.js removal and workflow-gate/mark message migration verified | @claude |
| 38 | FEATURE: workflow-mark: require reason argument for WORKFLOW_DOCS_NOT_NEEDED sentinel | @claude |
| 39 | FEATURE: claude-usage-widget: auto update support | @dotfiles |
| 40 | FEATURE: WORKFLOW_DOCS_NOT_NEEDED reason enforcement verification | @claude |
| 41 | BUGFIX: workflow-gate: fix hasStagedTestChanges for Unix-style Git Bash paths | @claude |
| 42 | BUGFIX: workflow-gate: fix commit regex false-positive on argument text | @claude |
| 43 | FEATURE: Workflow State Inheritance across VS Code Restarts | @claude |
| 44 | SECURITY: Enforce doc-append for history.md; block Japanese in public-repo doc-append | @claude |
| 45 | FEATURE: Workflow State Inheritance across VS Code Restarts (2026-04-18, 2852d03, bug fix) | @claude |
| 46 | FEATURE: make-plan: NEEDS_RESEARCH mid-plan research escalation | @claude |
| 47 | SECURITY: Security Enhancement Phase 1: /review-security skill | @claude |
| 48 | SECURITY: Security Enhancement Phase 2: Security cases in test.md + test-rules/ restructure | @claude |
| 49 | BUGFIX: Fix findLatestStateForContext: collect all session IDs and skip completed workflows | @claude |
| 50 | FEATURE: Workflow skip sentinels unified + DOCS_NOT_NEEDED removed | @claude |
| 51 | SECURITY: Security Enhancement Phase 3: /review-plan-security + /review-code-security + hard-secret detection | @claude |
| 52 | SECURITY: Security Enhancement Phase 4: Trojan Source detection + scan-outbound rename | @claude |
| 53 | REFACTOR: workflow-mark: && chaining support + workflow step rename | @claude |
| 54 | BUGFIX: Fix workflow-gate: quoted -C path causes hasStagedDocChanges false-negative + fail-open bypass | @claude |
| 55 | FEATURE: doc-append: global launcher + category prefix system | @claude |
| 56 | FEATURE: doc-rotate: index.md category badges + distribution summary + --rebuild-index flag | @claude |
| 57 | SECURITY: Phase 5: scan-inbound PostToolUse hook for prompt injection defense | @claude |
| 58 | FEATURE: doc-append: auto-rotate when history.md >= 500 lines | @claude |
| 59 | CONFIG: Add WebFetch allow domains for deep-research | @claude |
| 60 | FEATURE: write-tests skill: enumerate call paths step | @claude |
| 61 | FEATURE: workflow-gate: docs-only commit short-circuit | @claude |
| 62 | FEATURE: Consolidate Workflow State Machine tests into single integration file | @claude |
| 63 | FEATURE: block-tests-direct.js: enforce test writes via /write-tests hook | @claude |
| 64 | FEATURE: workflow-gate: extend docs-only short-circuit to root human-facing .md files | @claude |
| 65 | FEATURE: update-docs skill: structure README.md into routine (step 2/3/Completion) | @claude |
| 66 | FEATURE: installer: add PowerShell Core install/update to install.ps1 | @dotfiles |
| 67 | INCIDENT #25: winget corrupts WinHTTP state in-process — Invoke-RestMethod throws FileNotFoundException | @dotfiles |
| 68 | BUGFIX: pwsh.ps1: update path rewrite — GitHub API, elevation, binary version, WinHTTP isolation | @dotfiles |
| 69 | BUGFIX: install.ps1: add Invoke-ScriptIsolated helper to fix WinHTTP corruption for all scripts | @dotfiles |
| 70 | BUGFIX: install.ps1: fix Wait-MsiMutex to check Global\_MSIExecute mutex instead of process presence | @dotfiles |
| 71 | BUGFIX: autohotkey.ps1: suppress WinPSCompat warning when loading International module | @dotfiles |
| 72 | FEATURE: docs/history.md classification -- history-classification.md + split-history.py | @claude |
| 73 | FEATURE: agents-split step 2: introduce AGENTS_CONFIG_DIR abstraction | @claude |
| 74 | FEATURE: agents-split step 3: abstract hook scanner path via AGENTS_CONFIG_DIR | @claude |
| 75 | FEATURE: agents-split step 4: abstract session-sync path via AGENTS_DIR | @claude |
| 76 | FEATURE: agents-split step 5: classify 76 tests for repo split (tests/split-plan.md) | @claude |

## Summary

| Tag | Count |
|-----|-------|
| @claude | 63 |
| @dotfiles | 13 |
| @both | 0 |
| **Total** | **76** |
