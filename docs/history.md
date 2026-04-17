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
Background: New session (VSCode extension, Windows) で deny ルールが機能するか確認。settings.json の `Edit(**/.git/workflow/**)` / `Write(**/.git/workflow/**)` deny ルールは Claude Code 再起動後に有効になる。
Changes: Write(**/.git/workflow/**) と Edit(**/.git/workflow/**) の両ルールが Claude Code のパーミッションシステムにより自動ブロックされることを確認。Windows での全動作確認完了。

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
Background: Windows・WSL・macOS の全プラットフォームで正常系・異常系の動作確認を完了。macOS は初のネイティブ E2E 実行。
Changes: 正常系（セッション開始・ステップ記録・commit ブロック・commit 通過・PostToolUse 実発火）と異常系（ステートファイル破損 → fail-safe ブロック → --reset-from リカバリ、部分リセット）をすべて確認。macOS E2E で3つの移植性バグを発見・修正：(1) `timeout` 非対応 → `run_with_timeout()` perl フォールバック、(2) `CLAUDECODE` 継承によるネストセッションエラー → `unset CLAUDECODE`、(3) `disableBypassPermissionsMode: disable` が `--dangerously-skip-permissions` を無効化 → minimal settings.json 使用。WSL は Windows ブリッジ経由のため3つとも顕在化しなかった（`CLAUDECODE` 非伝播・Windows 側プロファイル参照）。test.md に `run_with_timeout` パターンと `claude -p` E2E 注意点を追記、Installer Testing を test-installer.md に分離。

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

### fix: install.sh に uv 呼び出し追加・--develop の包含関係を Windows と統一 (2026-04-17, pending)
Background: install/linux/uv.sh が存在するにもかかわらず install.sh のどこからも呼び出されていなかった。また --develop フラグが base パッケージブロックを実行しない実装になっており、Windows の install.ps1 (-Develop ⊇ -Base) と直交性が欠如していた。
Changes: install.sh の base ブロックに uv.sh 呼び出しを追加。base ブロックの条件を --base || --develop || --full に変更し、Windows の包含関係 (Full ⊇ Develop ⊇ Base) と揃えた。

### doc-append/doc-rotate + architecture.md split (2026-04-17, 28fa673)
Background: docs/history.md の肥大化対策として doc-append.py / doc-rotate.py を追加。docs/architecture.md も 300 行超に備えて architecture/ サブディレクトリへ分割。
Changes: bin/doc-append.py, bin/doc-rotate.py 追加。docs/architecture/ に claude-code.md, file-tree.md, shell-startup.md を分割。settings.json に uv run 権限追加。

### install-obsolete: .git/workflow クリーンアップ (2026-04-17, 5b3bf91)
Background: .git/workflow ディレクトリが旧 workflow state の保存場所だったが、~/.claude/projects/workflow に移行済み。install-obsolete でサルベージ・削除する処理を追加。
Changes: install-obsolete.sh / install-obsolete.ps1 に .git/workflow → ~/.claude/projects/workflow サルベージ処理追加。7日以内に更新された JSON ファイルを新パスにコピーし、旧ディレクトリを削除。

### profile.ps1: suppress create/delete mode output in startup git pull (2026-04-18, pending)
Background: On pwsh startup, the auto-pull's `git merge --ff-only` emitted the full summary (dozens to hundreds of `create mode` / `delete mode` lines, especially for the session sync repo) after the diffstat. The Fast-forward signal and change-size indicator (`++++`) are desired, but the summary is noise.
Changes: Added `--no-summary` to both `git merge --ff-only` invocations in `install/win/profile.ps1` (dotfiles auto-pull and session sync). Keeps the `Fast-forward` line and diffstat; suppresses `create mode` / `delete mode` lines only.
### Workflow State Machine: session ID injection and state inheritance across VS Code restarts (2026-04-18, (pending))
Background: VS Code 再起動時（hook 変更・シャットダウン等）に新 session_id が発行され、旧 session の workflow state file が孤立する。workflow-gate がすべてのステップを pending と判定してコミットをブロックする問題を解消する。
Changes: 3つの変更を組み合わせて解消。(1) session-start.js: 引き継ぎロジック追加 — 新セッション開始時、~/.claude/projects/<encoded-cwd>/<session_id>.jsonl をmtime降順(最大10件)スキャンし、SessionStart/PostCompact attachment エントリの stdout から「Current workflow session_id: <uuid>」を検索。同一 cwd+branch かつ all-pending でない state があれば steps をコピー引き継ぎ。(2) session-start.js: 出力を {} から additionalContext JSON (session_id + state path) に変更し、transcript に記録して将来のセッションが参照できるようにする。(3) post-compact.js (新規 PostCompact hook): compaction 後に session_id を再注入し transcript のマーカーを保持。workflow-state.js: getCurrentContext()・findLatestStateForContext(ctx) 追加、createInitialState() に optional ctx (cwd/git_branch) 引数追加。settings.json: PostCompact hook 登録追加。テスト: tests/feature-workflow-inherit-state.sh (A1–A12, T-C1–T-C3, 15 ケース) と tests/feature-workflow-post-compact.sh (B1–B3, 3 ケース)、計 18 ケース全 PASS。

### mark-step.js 削除・workflow-gate/mark メッセージ修正 検証完了 (2026-04-18, 9340267)
Background: workflow-gate/mark のブロックメッセージを node mark-step.js 呼び出し形式から echo マーカー形式に移行。mark-step.js を削除し workflow-mark.js に統合済み。
Changes: Windows / WSL / macOS 全環境で動作確認完了。ブロックメッセージに node mark-step.js が含まれないこと、WORKFLOW_MARK_STEP / WORKFLOW_RESET_FROM echo マーカーが state ファイルに正しく反映されることを確認。

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
