# History

## Archived
- [2026](history/2026.md) — 30 entries
- [legacy](history/legacy.md) — 102 entries

### check-private-info: allow Docker internal network and Node.js base image paths (2026-04-11, (pending))
Background: judgeclaw's docker-compose.yml uses Docker's default bridge network (172.24.0.x) and the official Node.js base image home directory (/home/node). The former matched the RFC 1918 private address pattern; the latter resembled a real user home path — both caused false positives in the private-info scanner.
Changes: Added two scoped entries to `.private-info-allowlist`: `docker-compose.yml:172\.24\.0` (Docker internal network, not routable) and `docker-compose.yml:/home/node` + `*/generate-web-access-section.sh:/home/node` (Node.js base image path, not a real user). Also fixed the path-capture regex in `bin/check-private-info.sh` from `/home/[a-zA-Z]` (one char) to `/home/[a-zA-Z][a-zA-Z0-9_.-]*` so the full username component is captured and allowlist entries can match precisely.

### write-tests subagent: mode acceptEdits → default (2026-04-11, (pending))
Background: write-tests subagent was launched with mode: "acceptEdits" to bypass VSCode's "Ask before edits" dialogs for tests/ files. After settings.json was updated with Write(**/tests/**) and Edit(**/tests/**) allow rules, acceptEdits became unnecessarily broad — it bypassed all file permission checks, allowing the subagent to silently edit any file including source code. This was confirmed when a test subagent modified bin/session-sync.sh and settings.json without authorization.
Changes: Changed subagent launch mode in claude-global/skills/write-tests/SKILL.md from "acceptEdits" to "default". The existing allow rules in settings.json continue to auto-approve test file writes without confirmation dialogs.

### session-sync push: retry loop for simultaneous-commit race condition (2026-04-11, (pending))
Background: When Windows (PENPEN-11H) and macOS (mbp-m4pro-nire) push at the same time, both machines commit session data independently, creating a diverged state. The original code ran git pull --rebase once before push, but if Claude Code wrote new session data to a .jsonl file after the initial git add/commit but before pull --rebase, the pull failed with "unstaged changes" — leaving push to fail and trigger the "push failed" toast.
Changes: Replaced the single pull+push block in bin/session-sync.ps1 and bin/session-sync.sh with a retry loop (max 3 iterations). Each iteration commits any newly-written session files, rebases over remote, and retries push. Also added `:` no-op at end of quiet-mode success path in session-sync.sh to ensure exit code 0. Added two test cases per platform: static check for retry loop presence, and functional test verifying recovery from pre-diverged state with unstaged changes. Also suppressed pull --rebase stdout inside the retry loop (was only suppressing stderr) to match Windows behavior of silent quiet-mode operation. Added two quiet-mode stdout/stderr separation tests.

### Workflow state machine: deterministic 8-step enforcement (2026-04-12, (pending))
Background: CLAUDE.md workflow instructions were text-based and relied on Claude's judgment, making step skipping undetectable. check-tests-updated.js and check-docs-updated.js enforced single concerns independently but were not unified. Goal was to replace both with a single state-machine-based gate.
Changes: Created workflow state machine (lib/workflow-state.js shared module, workflow-gate.js PreToolUse commit gate replacing check-tests-updated.js and check-docs-updated.js, mark-step.js step completion CLI with --reset-from bypass, session-start.js SessionStart hook). Fail-safe design: missing or corrupted state blocks commits. Added Completion sections to 5 global skill SKILL.md files (survey-code, deep-research, make-plan, write-tests, update-docs). 36 tests in tests/feature-robust-workflow.sh.

### #21: Hook integration path untested — CLAUDE_ENV_FILE format bug (2026-04-12)
Cause: session-start.js wrote `export CLAUDE_SESSION_ID=...` to CLAUDE_ENV_FILE, but Claude Code's env-file parser expects `KEY=VALUE` format (no `export` prefix). Test 30 used `grep -q` (substring match), so `export KEY=VALUE` passed despite being wrong. Root cause: tests only validated the JS script in isolation — the handoff point from script output to Claude Code's environment injection was never verified. In hook-based systems, the correctness of output format is only meaningful relative to the consuming system's spec, which unit tests cannot cover. More broadly, when writing tests, only single-file normal/error/edge cases were considered; integration paths across multiple components (hook → JS → env injection → Bash tool) were not enumerated as test targets.
Fix: Removed `export` prefix in session-start.js. Tightened test 30 to use `grep -qx` (exact line match). Follow-up investigation confirmed CLAUDE_ENV_FILE path is not propagated to Bash tool subprocesses (tested on Windows; macOS/Linux behavior unconfirmed).

### Workflow State Machine: mark-step.js session ID resolution redesign (2026-04-12)
Background: Empirical testing (Windows) showed that both CLAUDE_ENV_FILE path and CLAUDE_SESSION_ID are absent from Bash tool subprocess environment. The CLI design of passing session ID as an argument ($CLAUDE_SESSION_ID) was broken — always expanded to empty string. CLAUDE_ENV_FILE is available in hook contexts (SessionStart, PreToolUse, etc.) via process.env, but not in Bash subprocesses.
Changes: Removed session ID CLI argument from mark-step.js. Added resolveSessionId() to lib/workflow-state.js, reading CLAUDE_ENV_FILE from process.env to extract CLAUDE_SESSION_ID. workflow-gate.js usage messages updated. 41 tests pass.

### Workflow State Machine: E2E verification + WSL path bug fix (2026-04-13)
Background: Manual E2E verification of the workflow state machine in a real Claude Code session after implementation was complete.
Changes: Confirmed all six test scenarios — normal cases 1–4 (SessionStart writes CLAUDE_SESSION_ID, mark-step.js records steps, git commit blocked on incomplete steps, git commit passes when all steps complete) and error cases 2–3 (--reset-from recovery from corrupted state file, partial reset from mid-workflow step). During verification, found and fixed a bug: on Windows, the hook resolved the WSL-style drive path extracted from the git -C flag using Windows Node.js, causing git rev-parse to fail with ENOENT and the gate to report "state not found" instead of listing incomplete steps. Fixed by adding toNativePath() to is-private-repo.js (converts bash-style drive paths to Windows paths on win32) and wiring it into resolveRepoDir. Added tests 42–49 to the test suite (all 49 tests pass).

### PostToolUse marker interception: fix mark-step.js in Bash subprocess context (2026-04-13)
Background: CLAUDE_ENV_FILE is not propagated to Bash tool subprocesses (Anthropic bug #27987). mark-step.js called from skill Completion sections via Bash always failed to resolve session_id, silently leaving workflow steps unrecorded.
Changes: Added workflow-mark.js (PostToolUse Bash hook) that intercepts `echo "<<WORKFLOW_MARK_STEP:step:status>>"` via strict anchored regex on tool_input.command — rejects pipes, &&, cat/grep output containing the marker, etc. Hook reads session_id directly from hook stdin JSON and uses CLAUDE_PROJECT_DIR (documented env var, available in all hook types) for repo resolution. HOOK_CWD was excluded as undocumented with no confirmed usage evidence. Updated 5 skill Completion sections (survey-code, deep-research, make-plan, write-tests, update-docs). user_verification remains CLI-only to preserve the ask-rule gate. Added 26 test cases (normal, must-not-mark strict regex, error/edge, idempotency). Key finding: additionalContext surfaces messages to Claude; systemMessage reaches users only.

### #22: settings.json PostToolUse misplacement (2026-04-13)
Cause: commit 01a9110 placed the PostToolUse hook inside the `permissions` block instead of the `hooks` block. Unit tests (direct JS invocation) pass regardless because they call the hook handler directly — they cannot detect whether Claude Code will ever invoke the hook at all.
Fix: Moved PostToolUse registration to the `hooks` block. Added SR1-SR4 structural assertions to the test suite to automatically detect misplacement. Added E2E test E1 (`claude -p` real invocation with `--setting-sources project`) confirming the hook fires and records the step in the state file. Added "Test Layer Selection" section to `claude-global/rules/test.md` documenting that wiring bugs (config misplacement, hook registration errors) require integration/E2E tests — not just unit tests (based on Fowler and Kent C. Dodds research).

### Workflow State Machine: PostToolUse Windows E2E verified + CLAUDE_PROJECT_DIR migration confirmed (2026-04-13)
Background: Windows E2E smoke test for the PostToolUse hook and simultaneous verification that the HOOK_CWD → CLAUDE_PROJECT_DIR migration (commit 264ab0a) works correctly in a real session.
Changes: E1 E2E test (`claude -p --setting-sources project --session-id`) passed: PostToolUse hook fires in a real Claude session, `CLAUDE_PROJECT_DIR` resolves the repo correctly, and the step is written to the state file. All tests pass with `RUN_E2E=1`. macOS/Linux verification remains pending.

### Workflow State Machine: Robust Improvements — Windows verification (2026-04-13)
Background: User verification of three robustness improvements on Windows (VSCode extension).
Changes: (1) `echo "<<WORKFLOW_USER_VERIFIED>>"` ask dialog confirmed — permissions.ask rule fires and PostToolUse hook records user_verification as complete. (2) SessionStart creates UUID-format state file at session start (.git/workflow/<session-id>.json confirmed). (3) isPrivateRepo bypass removal confirmed via git diff — a6ad6ab had `if (isPrivateRepo(repoDir)) approve()`, current code imports only resolveRepoDir.

### Workflow State Machine: .git/workflow deny rule verified (2026-04-13)
Background: In a new session (VSCode extension, Windows), confirmed that the deny rules work. The Edit(**/.git/workflow/**) and Write(**/.git/workflow/**) deny rules in settings.json take effect after restarting Claude Code.
Changes: Confirmed that both Write(**/.git/workflow/**) and Edit(**/.git/workflow/**) rules are automatically blocked by the Claude Code permission system. Full verification on Windows complete.

### starship: increase command_timeout to 2000ms (2026-04-13)
Background: starship prompt showed timeout warnings when changing into the dotfiles directory. Root cause: git fsmonitor daemon was not yet running, and daemon startup exceeded starship's default 500ms command_timeout.
Changes: Set `command_timeout = 2000` in `.config/starship.toml`.

### Fix wait-vscode-window.sh: add WSL2 window detection (2026-04-13, pending)
Background: In WSL2, VS Code runs as a Windows process so xdotool/wmctrl (X11 tools) are unavailable. The `codes` function called wait-vscode-window.sh which fell through to the "No window detection tool found" warning and skipped session sync.
Changes: Added WSL2 branch to bin/wait-vscode-window.sh that uses powershell.exe to enumerate Code.exe MainWindowTitle values via [System.Diagnostics.Process]::GetProcessesByName. Detects WSL via WSL_DISTRO_NAME env var. Updated tests/main-wait-vscode-window.sh to cover WSL_DISTRO_NAME detection. Updated docs/architecture.md.

### Fix QNAP dotfileslink.sh: guard autorun.sh install against mount failure (2026-04-13)
Background: On QNAP, the first run of install.sh printed "autorun.sh installed." even though the mount failed — /tmp/config directory did not exist, causing sudo mount to fail silently. The script had no set -e, so errors were ignored.
Changes: Added mkdir -p /tmp/config before mounting. Wrapped mount in an if block; success message and subsequent cp/chmod/umount only execute when mount succeeds. Added WARNING message on failure.

### Workflow State Machine: marker format `:` → `_` (2026-04-13)
Background: Claude Code's permission glob parser treats `:` as a named-parameter separator inside `Bash(...)` rules, causing silent match failure (confirmed via anthropics/claude-code#33601). `WORKFLOW_RESET_FROM` ask rule was not triggering a dialog; investigation showed `Bash(echo "<<WORKFLOW_USER_VERIFIED>>")` (no colon) worked while any pattern containing `:` silently failed to match. Same issue applied to `WORKFLOW_MARK_STEP` allow rules. Default behavior when no rule matches is auto-allow, masking the problem.
Changes: Changed both marker formats to use `_` as separator: `<<WORKFLOW_MARK_STEP:step:status>>` → `<<WORKFLOW_MARK_STEP_step_status>>`, `<<WORKFLOW_RESET_FROM:step>>` → `<<WORKFLOW_RESET_FROM_step>>`. Updated workflow-mark.js regexes, settings.json allow/ask rules (RESET_FROM single-quote variant removed — DQ only), workflow-gate.js block message, 5 skill Completion sections, and test suite. Windows re-verification confirmed: normal cases 1–4 and error cases 1–2 all pass.

### Workflow State Machine: cross-platform verification complete (2026-04-14, 60aa84c)
Background: Completed happy-path and error-path verification across Windows, WSL, and macOS. macOS was the first native E2E run.
Changes: Verified happy path (session start, step recording, commit block, commit pass, PostToolUse real-fire) and error path (state file corruption -> fail-safe block -> --reset-from recovery, partial reset). Found and fixed three portability bugs during macOS E2E: (1) timeout not available -> run_with_timeout() perl fallback; (2) nested session error from inherited CLAUDECODE -> unset CLAUDECODE; (3) disableBypassPermissionsMode: disable neutralizing --dangerously-skip-permissions -> use minimal settings.json. WSL did not surface any of the three (CLAUDECODE not propagated, Windows profile referenced via bridge). Added run_with_timeout pattern and claude -p E2E caveats to test.md; extracted Installer Testing into test-installer.md.

### session-sync: push conflict auto-resolution (2026-04-14)
Background: Session sync was silently failing with "push failed" toast when ~/.claude/projects
received diverging commits from multiple machines (e.g. macOS and Windows both committing
session files before either pushes). git pull --rebase inside the retry loop swallowed JSONL
conflict errors via `|| true`, causing repeated push failures with no recovery.
Changes: Two fixes to bin/session-sync.sh push action. (1) Interrupted rebase detection: at
push entry, detect .git/rebase-merge or .git/rebase-apply and abort (rm -rf fallback for
fake/corrupt state that git rebase --abort cannot clean). (2) JSONL conflict auto-resolution:
when git pull --rebase fails, find conflicted *.jsonl files via git diff --diff-filter=U, strip
conflict marker lines (^<<<<<<<, ^=======, ^>>>>>>>), dedup via awk, git add, then
GIT_EDITOR=true git rebase --continue. Non-JSONL conflicts abort and retry. Also fixed the
"No changes to push" early exit to additionally check git log origin/main..HEAD, so push
proceeds when local branch is ahead of remote despite a clean working tree. 57 tests pass.

### #23: workflow-gate block message as bypass attractor (2026-04-14)
Cause: workflow-gate.js:113-117 included a literal `echo "<<WORKFLOW_RESET_FROM_<step>>>"` recipe
in its "commit blocked" error message. When a skill's test setup ran `git commit` in a temp
repository, workflow-gate blocked it and the model read the hint — triggering autonomous
WORKFLOW_RESET_FROM calls to clear the gate. The model lacked holistic project context to judge
whether a reset was warranted; from inside a skill the hint looked like a legitimate shortcut.
Root cause is structural: the mechanism that should be a last resort was advertised in an error
message visible to subagents that cannot evaluate the full situation.

Fix: Removed the RESET_FROM hint from workflow-gate.js entirely (the block message now lists only
incomplete steps and the skill to run for each). Added a "Workflow State Recovery" section to
CLAUDE.md framing WORKFLOW_RESET_FROM as a last resort that "only the main conversation can use
when it has enough holistic context to judge that a reset is genuinely warranted." The settings.json
`ask` guard remains as a backstop. The RESET_FROM mechanism itself (workflow-mark.js PostToolUse
interception, required because CLAUDE_ENV_FILE does not propagate to Bash subprocesses per
Anthropic bug #27987) is unchanged.

Discussion notes: Several alternative fixes were considered and rejected.
(1) Obfuscating the hint — rejected because it merely makes bypass harder to find, not
structurally prevented.
(2) Moving documentation to ops.md only — would have required the user to manually run
mark-step.js even in cases where the main conversation can judge correctly.
(3) Manager subagent — rejected as overskill; the main conversation already has full context
and IS the manager.
The key insight: the right place for reset authority is the entity with holistic project context,
which is the main conversation (or the user directly). Skills and subagents must not reset.

### #24: Workflow State Machine — multi-repo state mismatch (2026-04-14)
Cause: echo markers resolved repoDir from CLAUDE_PROJECT_DIR (primary project), while workflow-gate resolved repoDir from the git -C flag in the commit command. When committing to a secondary repo with git -C, the state was written to primary-project/.git/workflow/ but read from secondary-repo/.git/workflow/ — permanently blocking commits with no recovery path from inside a Bash subprocess (CLAUDE_ENV_FILE not propagated per Anthropic bug #27987).
Fix: Removed repoDir from all state operations. State moved to ~/.claude/projects/workflow/{session-id}.json (session-scoped). All hooks and CLI tools now use a single path, regardless of which repo is being committed. CLAUDE_WORKFLOW_DIR env var override added for test isolation. Migration block added to session-start.js to auto-delete old .git/workflow/ files. settings.json deny rules updated to new path. /workflow/*.tmp added to .gitignore in session-sync-init scripts to prevent race with session-sync git push.

### Workflow State Machine: session-scoped state migration (2026-04-15, (pending))
Background: Incident #24 root cause: per-repo state storage caused permanent blocking when committing across repos. Session-scoped storage eliminates the mismatch because all hooks and CLI tools use the same path for a given session, independent of which repo is targeted.
Changes: workflow-state.js: removed execSync/repoDir, added os.homedir()-based path (~/.claude/projects/workflow/), CLAUDE_WORKFLOW_DIR override for tests, cleanupZombies now removes stale .tmp files. workflow-gate.js, workflow-mark.js, mark-step.js, session-start.js: removed all repoDir arguments. session-start.js: added BEGIN/END temporary migration block to delete old .git/workflow/<session-id>.json on each session start. session-sync-init.ps1/.sh: added .gitignore with /workflow/*.tmp to prevent half-written state files from being synced. settings.json: deny rules updated from **/.git/workflow/** to ~/.claude/projects/workflow/**. Tests: added WS-UNIT-1~6, WS-IDEM-1 (unit), T-D1-4~7 (session-start without CLAUDE_PROJECT_DIR), T-MIG-1~2 (migration idempotency), INT-REGR-1 (cross-repo regression), SR-DENY-1~2 (deny rule validation). All pass on Windows.

### Workflow State Machine — E2E verification across all 3 environments (2026-04-15)
Background: After migrating state storage from {repo}/.git/workflow/ to ~/.claude/projects/workflow/, re-ran E2E verification on all platforms to confirm the new path is used end-to-end.
Changes: No code changes. Verified on Windows, macOS, and WSL: (1) SessionStart creates state file at new path; (2) WORKFLOW_MARK_STEP_* markers record steps via PostToolUse hook; (3) incomplete steps block git commit; (4) all steps complete allows git commit; (5) PostToolUse real invocation via RUN_E2E=1 (WSL skipped due to environment constraints — verified via settings.json structure instead); (6) git -C <other-repo> commit reads the same session state, no #24 regression; (7) WORKFLOW_RESET_FROM_* triggers ask dialog and hook rolls back state to the named step (manually confirmed including WSL). This also serves as user verification for the session-scoped state migration (previously tracked as "Verifying" in todo.md).

### Workflow State Machine — block user_verification direct bypass in mark-step.js (2026-04-15)
Background: mark-step.js user_verification complete could mark the step complete without going through the echo "<<WORKFLOW_USER_VERIFIED>>" path that triggers the settings.json ask dialog, discovered during WSL E2E verification.
Changes: mark-step.js: extended user_verification guard to also block status "complete" (was only blocking "skipped"), with error message pointing to the correct echo path. workflow-mark.js: corrected misleading comment ("must go through CLI") and fallback error message that incorrectly referenced mark-step.js as the manual path. Tests: added WS-UV-BLOCK-1~4 to feature-robust-workflow.sh (error, error, normal, edge cases). All tests pass.

### install-obsolete: .git/workflow → ~/.claude/projects/workflow cleanup (2026-04-15, (pending))
Background: Workflow state was migrated from {repo}/.git/workflow/ to ~/.claude/projects/workflow/ (commit 738cb1c, incident #24). The session-start.js migration block handles new sessions automatically, but old .git/workflow/ directories left on disk before the migration remained unhandled.
Changes: install-obsolete.sh and install-obsolete.ps1 each received a BEGIN/END temporary migration block: searches $HOME (maxdepth 4) / C:\git repos for .git/workflow/ directories, salvages JSON files newer than 7 days to ~/.claude/projects/workflow/, then removes the old directory. Tests: main-install-obsolete-workflow-migration.sh (T1–T9, all pass).

### Fix install.ps1: missing registry values crash on keyboard hotkey setup (2026-04-15)
Background: On Windows machines where Keyboard Layout\Toggle registry key values (Language Hotkey, Layout Hotkey, Hotkey) have never been set, Get-ItemProperty threw a terminating error that -ErrorAction SilentlyContinue could not suppress.
Changes: Replaced Get-ItemProperty with Get-Item + RegistryKey.GetValue(), which returns $null for missing values without throwing.

### Workflow State Machine: mark-step.js removal (2026-04-16, (pending))
Background: mark-step.js was designed as a CLI tool to mark workflow steps, but relied on CLAUDE_ENV_FILE which is not propagated to Bash tool subprocesses (Anthropic bug #27987). All functionality was already covered by the echo marker mechanism in workflow-mark.js (PostToolUse hook), making mark-step.js dead code.
Changes: Deleted mark-step.js. workflow-gate.js block messages updated from `node mark-step.js` to `echo "<<WORKFLOW_RESET_FROM_research>>"` / `echo "<<WORKFLOW_MARK_STEP_<step>_complete>>"`. workflow-mark.js fallback error messages likewise updated. settings.json: removed allow rule for mark-step.js and ask rule for mark-step.js user_verification bypass (now moot). Tests: removed all mark-step.js-specific test sections from feature-robust-workflow.sh (tests 17–34, WS-UV-BLOCK-1~4); replaced mark-step.js state setup in integration test with direct workflow-state.js call. All remaining tests pass.

### Workflow State Machine: evidence-based write_tests/docs enforcement (2026-04-16, bf95596+)
Background: write_tests and docs steps could be bypassed by running echo "<<WORKFLOW_MARK_STEP_*_complete>>" directly, without actually writing tests or updating docs. The user_verification step had protection via WORKFLOW_USER_VERIFIED ask dialog, but write_tests and docs had none. Additionally, the old check-tests-updated.js and check-docs-updated.js hooks (replaced by workflow-gate.js) used git staged-file evidence rather than state flags, and exempted claude-global/ from checks — an exemption that was found to be unjustified.
Changes: workflow-gate.js: added evidence-based checks — write_tests passes if tests/ changes are staged; docs passes if docs/*.md or *.md changes are staged; no claude-global/ exemption. Added resolveRepoDir(), hasStagedTestChanges(), hasStagedDocChanges() helpers. workflow-mark.js: MARK_STEP sentinel now rejected for write_tests and docs (like user_verification), with messages directing to staged-file path or NOT_NEEDED sentinels. Added WORKFLOW_WRITE_TESTS_NOT_NEEDED and WORKFLOW_DOCS_NOT_NEEDED handlers (mark step complete via ask dialog when evidence is genuinely absent). settings.json: added ask permissions for both new sentinels. write-tests/SKILL.md, update-docs/SKILL.md: completion instructions updated to git add instead of echo marker; NOT_NEEDED path documented.
Also: workflow-mark.js user_verification rejection message clarified to lead with "NOT recorded" (bf95596), fixing incident where Claude misread guidance as optional and proceeded to commit prematurely.

### doc-append/doc-rotate tools + architecture.md split (2026-04-16, (pending))

Background: history.md had no append-without-read strategy (full Read consumed context on every entry); architecture.md was 480 lines making context-efficient updates hard.

Changes: Added bin/doc-append.py (tail-seek append, CRLF/BOM-safe, ascending date invariant, incident #N numbering) and bin/doc-rotate.py (year-based archive, history/index.md generation, floor protection). Split architecture.md into architecture/file-tree.md, architecture/shell-startup.md, architecture/claude-code.md (156 lines from 480). Updated docs-convention.md with append-only tool table and rotation thresholds. Added settings.json allow/ask permissions for both tools. Tests: tests/feature-doc-tools.py (24 pytest integration cases).

### Fix: commit-push skill model directive removed (2026-04-17, (pending))

Background: commit-push SKILL.md had model: haiku and effort: low directives, causing the Skill runner to spawn a new agent with a separate session UUID. The new session had all workflow steps pending, so workflow-gate blocked every commit attempt from within the skill.

Changes: Removed model: haiku and effort: low from commit-push/SKILL.md so the skill executes in the caller's session. Added regression test tests/main-commit-push-workflow-gate.sh (2 static checks verifying no model/effort directives).

### doc-rotate.py: line-count trigger and floor-based archiving (2026-04-17, (pending))

Background: doc-rotate.py archived entries based on a 365-day hardcoded cutoff, making rotation impossible when all entries are recent. Additionally, undated entries (pre-dating convention, 102 entries) were never archived. The threshold-warn/hard parameters were parsed but unused.

Changes: Replaced date-based cutoff with floor-based archiving: keep last --floor entries (default 20), archive the rest. threshold-warn gate (default 0 = always run) skips rotation when file is under the threshold. Undated entries treated as oldest and archived to history/legacy.md. Date regex made comma-optional to correctly parse entries without commit hashes. Removed --max-age-days. Tests: T-THRESH-1..5 added to feature-doc-tools.py (29 total, all pass).

### Fix workflow-gate: Unix-style drive path normalization in resolveRepoDir (2026-04-17, pending)
Background: On Windows with Git Bash, `git -C /<drive>/path commit` passed a Unix-style drive path (e.g. `/<drive>/git/dotfiles`) to workflow-gate.js. Windows Node.js does not accept Unix-style paths as `cwd` in `execSync`, causing `hasStagedTestChanges` and `hasStagedDocChanges` to silently return `false` even when tests/ or docs/ were staged — blocking commits with a spurious "write_tests not complete" error.
Changes: `resolveRepoDir` now normalizes `/<drive>/path` → `C:\path` patterns (single drive letter followed by `/`). `hasStagedTestChanges` and `hasStagedDocChanges` catch blocks now write a warning to `process.stderr` instead of silently swallowing the error. Main execution wrapped in `require.main === module` guard; `resolveRepoDir`, `hasStagedTestChanges`, `hasStagedDocChanges` exported for testability. 13 tests added in `tests/fix-workflow-gate-unix-path.sh`.

### fix: add uv call to install.sh and align --develop inclusion with Windows (2026-04-17, pending)
Background: install/linux/uv.sh existed but was never called from install.sh. The --develop flag also did not execute the base package block, breaking orthogonality with Windows install.ps1 (-Develop ⊇ -Base).
Changes: Added uv.sh call to the base block in install.sh. Changed the base block condition to --base || --develop || --full to align with the Windows inclusion relationship (Full ⊇ Develop ⊇ Base).

### doc-append/doc-rotate + architecture.md split (2026-04-17, 28fa673)
Background: Added doc-append.py and doc-rotate.py to manage growth of docs/history.md. Split docs/architecture.md into an architecture/ subdirectory in preparation for exceeding 300 lines.
Changes: Added bin/doc-append.py and bin/doc-rotate.py. Split docs/architecture/ into claude-code.md, file-tree.md, and shell-startup.md. Added uv run permissions to settings.json.

### install-obsolete: .git/workflow cleanup (2026-04-17, 5b3bf91)
Background: .git/workflow was the old storage location for workflow state, but migration to ~/.claude/projects/workflow was complete. Added salvage and deletion logic to install-obsolete.
Changes: Added .git/workflow -> ~/.claude/projects/workflow salvage logic to install-obsolete.sh and install-obsolete.ps1. Copies JSON files updated within 7 days to the new path and removes the old directory.

### profile.ps1: suppress create/delete mode output in startup git pull (2026-04-18, pending)
Background: On pwsh startup, the auto-pull's `git merge --ff-only` emitted the full summary (dozens to hundreds of `create mode` / `delete mode` lines, especially for the session sync repo) after the diffstat. The Fast-forward signal and change-size indicator (`++++`) are desired, but the summary is noise.
Changes: Added `--no-summary` to both `git merge --ff-only` invocations in `install/win/profile.ps1` (dotfiles auto-pull and session sync). Keeps the `Fast-forward` line and diffstat; suppresses `create mode` / `delete mode` lines only.
### Workflow State Machine: session ID injection and state inheritance across VS Code restarts (2026-04-18, (pending))
Background: When VS Code restarts (on hook changes, shutdown, etc.), a new session_id is issued and the old session workflow state file becomes orphaned, causing workflow-gate to treat all steps as pending and block commits.
Changes: Resolved by three combined changes. (1) session-start.js: added inheritance logic — on new session start, scans ~/.claude/projects/<encoded-cwd>/<session_id>.jsonl files by mtime descending (up to 10), searches stdout of SessionStart/PostCompact attachment entries for 'Current workflow session_id: <uuid>'. If a state with the same cwd+branch exists and is not all-pending, copies its steps. (2) session-start.js: changed output from {} to additionalContext JSON (session_id + state path) to record in transcript for future sessions to reference. (3) post-compact.js (new PostCompact hook): re-injects session_id after compaction to preserve transcript markers. workflow-state.js: added getCurrentContext() and findLatestStateForContext(ctx), added optional ctx (cwd/git_branch) argument to createInitialState(). settings.json: registered PostCompact hook. Tests: tests/feature-workflow-inherit-state.sh (A1-A12, T-C1-T-C3, 15 cases) and tests/feature-workflow-post-compact.sh (B1-B3, 3 cases), 18 cases total, all PASS.

### mark-step.js removal and workflow-gate/mark message migration verified (2026-04-18, 9340267)
Background: Migrated workflow-gate/mark block messages from node mark-step.js call format to echo marker format. mark-step.js deleted and consolidated into workflow-mark.js.
Changes: Verified on all platforms: Windows, WSL, and macOS. Confirmed that block messages no longer contain node mark-step.js calls and that WORKFLOW_MARK_STEP / WORKFLOW_RESET_FROM echo markers are correctly reflected in state files.

### workflow-mark: require reason argument for WORKFLOW_DOCS_NOT_NEEDED sentinel (2026-04-18, (pending))
Background: The bare <<WORKFLOW_DOCS_NOT_NEEDED>> sentinel allowed reflexive skipping of the docs step without the model articulating why. Several sessions skipped docs when a history.md entry was warranted.
Changes: Sentinel extended to <<WORKFLOW_DOCS_NOT_NEEDED: REASON>> where REASON is validated (>=3 non-space chars, no '>', not a bilingual placeholder). Reason stored as state.steps.docs.skip_reason via new extraFields parameter on markStep(). settings.json ask glob updated. workflow-mark.js looks-like fallback rejects legacy bare form and reason-with->. WRITE_TESTS_NOT_NEEDED mirror change deferred (inline TODO). Tests WS-EV-9 through WS-EV-17 added to tests/main-workflow-evidence.sh.

### claude-usage-widget: auto update support (2026-04-18, pending)
Background: When running install.ps1 / install.sh, claude-usage-widget was always skipped if already installed with no version check. User wanted install.ps1 to update to the latest version automatically. Scope intentionally limited to claude-usage-widget only (other installers not changed due to effort).
Changes: install/win/claude-usage-widget.ps1: fetch GitHub release tag_name up-front, compare against installed ProductVersion via VersionInfo, skip if up to date, reinstall if newer. Poll loop now checks version equality instead of mere exe existence. install/linux/claude-usage-widget.sh: macOS reads CFBundleShortVersionString and rm -rf old .app before reinstall on update; Linux uses APPIMAGE_PATH.version sidecar file for version tracking, writes version after install. get_latest_version updated with sed s/^v// for explicit v-prefix stripping. tests: Update cases section added (12 cases) to both test files; existing already-installed assertions updated to up-to-date.

Follow-up: two issues surfaced on first run — (a) running widget locks its exe so silent installer fails with a dialog (user sees perpetual "Waiting for install to complete"); (b) ProductVersion is 4-part "1.7.2.0" while tag_name is 3-part "1.7.2" so the post-install poll never matches. Added ConvertTo-NormalizedWidgetVersion helper (strips trailing .0) used in both update-check and poll comparisons. Before install, Get-Process detects running widget and Stop-Process -Force kills it (autostart restarts it later). 4 additional Pester cases cover Stop-Process, Get-Process detection, version normalization regex, and normalization-intent documentation. 22 total Pester tests green.

### WORKFLOW_DOCS_NOT_NEEDED reason enforcement verification (2026-04-18, d5544f2)
Background: WORKFLOW_DOCS_NOT_NEEDED sentinel now requires a meaningful reason string (>=3 non-space chars, not a placeholder). Bare form is rejected. Verified via WS-EV-9 through WS-EV-18 in tests/main-workflow-evidence.sh.
Changes: All 18 test cases passed. No code changes needed.

### workflow-gate: fix hasStagedTestChanges for Unix-style Git Bash paths (2026-04-18, 876d12e)

Background: Git Bash passes MSYS2-style paths (/<letter>/rest) to workflow-gate when using git -C. hasStagedTestChanges used this path as cwd for execSync, but Node.js on Windows cannot resolve MSYS2 drive paths, causing the function to throw and return false — the test-evidence override never triggered even when tests/ files were staged.

Changes: Added resolveRepoDir() function that converts MSYS2 paths to Windows paths. Called from the main hook entrypoint to normalize repoDir before passing to hasStagedTestChanges/hasStagedDocChanges. Both functions now accept an explicit repoDir parameter. Exported all three functions for unit testing. Tests: fix-workflow-gate-unix-path.sh — 13 cases (A: resolveRepoDir unit x7, B: staged-file integration x4, C: error handling x2), all PASS.

### workflow-gate: fix commit regex false-positive on argument text (2026-04-18, pending)

Background: /git\s+(?:-C\s+\S+\s+)?commit\s/ had no line-anchor, so a Bash command whose arguments contained 'git commit' as text (e.g. doc-append.py --background '... git commit ...') triggered the commit block even though no git commit was being run.

Changes: Added ^ anchor to the regex (/^git\s+(?:-C\s+\S+\s+)?commit\s/). Regression test tests/main-workflow-gate-regex.sh added (7 cases: normal x4, bug-regression x2, edge x1), all PASS.

### Workflow State Inheritance across VS Code Restarts (2026-04-18, c6159ea)

Background: Implemented session state inheritance so that workflow steps (research/plan completion state) carry over to new sessions after VS Code restarts. findLatestStateForContext scans transcript JSONL files to find the most recently used session state for the current cwd+branch.

Changes: Manual smoke tests all passed: (1) steps inherited after VS Code restart, (2) most-recently-used session selected when multiple parallel sessions exist, (3) session_id preserved after /compact via SessionStart:compact hook.

### Enforce doc-append for history.md; block Japanese in public-repo doc-append (2026-04-18, pending)

Background: Claude was occasionally using the Edit tool directly on history.md instead of doc-append.py, violating the append-only convention. Japanese text was also leaking into history.md in public repos despite the language policy.

Changes: Added Edit(**/history.md) and Write(**/history.md) to the deny list in settings.json to hard-block direct edits. Added check-japanese-in-docs.js PreToolUse hook that blocks doc-append.py calls containing Japanese characters when the target repo is public. Tests: tests/main-check-japanese-in-docs.sh (10 cases, all PASS).

### Workflow State Inheritance across VS Code Restarts (2026-04-18, 2852d03)

Background: Workflow steps (research/plan completion state) were lost on VS Code restart because each new session started fresh. findLatestStateForContext was added to scan transcript JSONL files and inherit state from the most recently used session with matching cwd+branch.

Changes: Manual smoke tests all passed: steps inherited after restart, most-recent session wins among parallel sessions, session_id preserved after /compact via SessionStart:compact hook. Also fixed a bug where Windows cwd was not lowercased before encoding, causing transcript directory lookup to fail (C--git-dotfiles vs c--git-dotfiles).

### make-plan: NEEDS_RESEARCH mid-plan research escalation (2026-04-18, pending)
Background: The planner/reviewer loop had no escape hatch when the planner lacked external knowledge — it had to guess or stall. Adopted Option B (inlining): research findings are embedded verbatim into the next planner prompt, with no file persistence.
Changes: Added Research Escalation section to ~/.claude/skills/make-plan/SKILL.md: NEEDS_RESEARCH protocol (skill:/question:/reason: fields, first-non-whitespace-token detection, malformed handling), independent round counters (revision_rounds cap=2, research_rounds cap=2, malformed_retries cap=1 — never reset), re-prompt template with verbatim findings, and escalation message format. Added NEEDS_RESEARCH escape hatch, Research Findings section, and citation tag rules ([research: tag], format [a-z0-9-]+) to ~/.claude/agents/planner.md. Added citation integrity checklist item to ~/.claude/agents/reviewer.md. Contract tests: tests/feature-make-plan-research-escalation.sh (27 cases, all PASS).

### Security Enhancement Phase 1: /review-security skill (2026-04-18, 7330de5)
Background: Added architecture-phase security review skill (implemented 2026-04-06). Design decision: extract checklist details into skill (not rules/) to minimize context consumption per session. Skill naming follows existing verb-noun kebab-case convention.
Changes: Created ~/.claude/skills/review-security/SKILL.md with 3-axis checklist (Information Leakage: OWASP ASVS V8/V6; Third-Party Access: OWASP MCP Top 10 MCP03/MCP04, LLM Top 10 LLM03; External Access: OWASP WSTG, CWE Top 25). TodoWrite mandatory — no inline evaluation. Verified 2026-04-18: 15-item checklist registered and evaluated correctly in a live session.

### Security Enhancement Phase 2: Security cases in test.md + test-rules/ restructure (2026-04-18, pending)
Background: test.md lacked security-specific test case categories. Reviewers had no guidance on what security scenarios to cover when writing tests. test.md also exceeded 100 lines with low-priority detail mixed in.
Changes: Added Security cases bullet to Test Case Categories in claude-global/rules/test.md (secret leakage OWASP ASVS V8, input injection OWASP WSTG CWE-78/CWE-22, permission OWASP ASVS V4, prompt injection LLM Top 10 LLM01/MCP Top 10 MCP06, security idempotency). Extracted lower-priority sections to new rules/test-rules/ subdirectory: macos-timeout.md (paths-gated), claude-e2e.md (always loaded), installer.md (paths-gated, moved from test-installer.md). Removed Test Layer Selection reference links. test.md reduced from 149 to 106 lines.

### Fix findLatestStateForContext: collect all session IDs and skip completed workflows (2026-04-18, 0ba7e13)
Background: findLatestStateForContext had two bugs: (1) it stopped at the first SessionStart/PostCompact session_id found in a JSONL file, ignoring later PostCompact entries that carry the more recent session_id; (2) it inherited state from completed workflows (user_verification=complete), causing the gate to demand USER_VERIFIED even at the start of a brand-new task. Both bugs were triggered together when a session ended with user verification and a new task was started in the same VS Code window.
Changes: workflow-state.js findLatestStateForContext: collect ALL session_id matches from SessionStart and PostCompact entries per JSONL, try them in reverse order (PostCompact/most-recent first). When the most-recent session has user_verification=complete, break out of the JSONL entirely — no fallback to older sessions in the same file. CLAUDE_TRANSCRIPT_BASE_DIR env override added for test isolation. Six new tests added to tests/feature-workflow-inherit-state.sh covering PostCompact priority, completed-workflow exclusion, and multi-ID scenarios. Also fixed Windows test infrastructure (TMPDIR_BASE path conversion, node_path() helper, stdin read without /dev/stdin, WORKFLOW_STATE_LIB_NODE path conversion).

### Workflow skip sentinels unified + DOCS_NOT_NEEDED removed (2026-04-18, pending)
Background: workflow-gate.js treated research/plan as SKIPPABLE_STEPS but no echo sentinel existed to declare them skipped — only workaround was manual JSON state editing. WORKFLOW_WRITE_TESTS_NOT_NEEDED allowed bare declaration without reason (abuse observed). WORKFLOW_DOCS_NOT_NEEDED allowed skipping docs, contradicting the policy that docs must always be actually updated (complete = actually completed).
Changes: Added WORKFLOW_RESEARCH_NOT_NEEDED and WORKFLOW_PLAN_NOT_NEEDED sentinels with mandatory reason (>=3 non-space chars, not a placeholder, no '>'). Migrated WORKFLOW_WRITE_TESTS_NOT_NEEDED to reason-required form; all three record status: skipped with skip_reason. Removed WORKFLOW_DOCS_NOT_NEEDED — replaced with deprecation message. Updated SKIPPABLE_STEPS (+ write_tests), settings.json ask block, CLAUDE.md workflow steps, workflow-gate.js SKILL_MAP, and skill completion sections (survey-code, deep-research, make-plan, update-docs). Tests: main-workflow-skip-sentinels.sh (18 cases, all PASS), main-workflow-evidence.sh updated (WS-EV-8/8b updated, WS-EV-9-18 deleted).

### Security Enhancement Phase 3: /review-plan-security + /review-code-security + hard-secret detection (2026-04-18, pending)
Background: Phase 3 redesigned /review-security into two companion skills with clear invocation timing: /review-plan-security (Step 2, planning-time checklist) and /review-code-security (Step 5, code-review-time patterns). Original plan used /scan-security but the name implied a different tool type; renamed to reflect when each skill is called. check-private-info.sh lacked coverage of structured API secrets (LLM keys, GitHub tokens, etc.) - only personal information was detected.
Changes: git mv review-security/ -> review-plan-security/: updated frontmatter name/description, added companion reference to /review-code-security. New claude-global/skills/review-code-security/SKILL.md: same 3-axis structure (### Axis 1/2/3), each axis uses italic Axis scope: instead of Source: because patterns derive from Gitleaks/CWE not OWASP directly; Automated coverage note explains hard-secret patterns are handled by check-private-info.sh. bin/check-private-info.sh extended with 11 hard-secret detection patterns: AWS AKIA key, PEM private key header, GitHub PAT, Slack token, OpenAI, Anthropic (checked before OpenAI to prevent sk- prefix overlap), Google, HuggingFace, Groq, Replicate, Cohere. claude-global/hooks/pre-commit extended to block dotenv file commits (allows dotenv.example/.sample/.template). claude-global/CLAUDE.md Step 2 updated to /review-plan-security; Step 5 adds conditional /review-code-security call. 4 new/updated test files: main-review-code-security.sh (renamed from main-scan-security.sh, 20 cases), main-review-plan-security.sh (new, 16 cases, axis consistency test), main-hard-secret-scan.sh (new, 11 normal + edge cases), main-env-file-block.sh (new, pre-commit dotenv block tests). All tests PASS.
