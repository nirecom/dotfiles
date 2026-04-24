# History (agents/legacy)

### FEATURE: Claude Code management (664d7ce–2e96bbf)
Background: Centralize Claude Code config
Changes: `claude-global/` directory, install integration, security deny rules


### FEATURE: Claude Code commands (8418c36)
Background: Manage Claude Code commands in dotfiles
Changes: Added global commands directory symlink support


### INCIDENT: #3: Claude Code installed on QNAP (89e9e98)
Cause: `install.sh` had no QNAP branch
Fix: Added early exit for QNAP


### FEATURE: Claude Code skill management (9ec0c0b, 359d929, e442685)
Background: Manage Claude Code skills (commands) in dotfiles, improve symlinks
Changes: Changed commands symlink from per-file to directory-level, added langchain/instruction update skills


### SECURITY: Claude Code security hardening (6e9eeb1, 18b0fd7)
Background: Review settings.json allow/deny rules. Cross-reviewed with ChatGPT: (1) adopt `git -C` as primary method (avoids compound commands), (2) `cd && git` fallback limited to status/diff/log minimum set, (3) wildcard prefix on deny rules (defense in depth), (4) curl/wget pipe deny acknowledged as glob-limited (future PreToolUse hook for strict enforcement)
Changes: Added `git -C` allow rules, `cd &&` fallback allow (minimal set), strengthened deny rule `*` prefixes, added `git -C` preference to CLAUDE.md, added local path rule to private information


### BUGFIX: Claude Code deny rule false-positive fix (c8095dc)
Background: `git add` auto-denied without prompt — deny rule `Bash(*dd *)` matched "add" substring. Cross-reviewed with ChatGPT: (1) root cause is deny `*dd *` not missing allow rules, (2) replaced with 4 specific patterns (`dd *`, `*&& dd *`, `*; dd *`, `*
Changes: dd *`), (3) HEREDOC commit format confirmed working with existing `git commit -m *` allow rule, (4) settings changes require session restart to take effect


### INCIDENT: #13: `git add CLAUDE.md` auto-denied by Claude Code (c8095dc)
Cause: Deny rule `Bash(*dd *)` glob-matched the "dd" in "add", overriding the allow rule `Bash(git add *)`. Deny rules take precedence regardless of order
Fix: Replaced `*dd *` with 4 patterns: `dd *`, `*&& dd *`, `*; dd *`, `*| dd *`


### FEATURE: Claude Code PreToolUse hook investigation ()
Background: Investigated using PreToolUse hooks to force diff display before Edit (CLAUDE.md instruction sometimes ignored). Created node-based hook script outputting diff via stderr and `additionalContext`. **Result: neither method is visible to users in VS Code extension UI.** VS Code already shows a built-in diff in the Edit approval dialog, making the hook redundant. Hook removed. Findings: (1) PreToolUse hook stderr not shown in VS Code UI, (2) `additionalContext` not shown in VS Code UI, (3) `/dev/stdin` does not exist on Windows — use `fs.readSync(0, buf)` for cross-platform stdin, (4) VS Code Claude Code provides diff review via standard Edit approval dialog when Edit is not auto-allowed
Changes: No commit (reverted)


### FEATURE: Claude Code git write permissions (e94a2b5)
Background: `git push` failed with permission denied — only read-only git commands were in allow list. Cross-reviewed with ChatGPT: (1) allow `git commit -m *` only (not broad `git commit *` which passes `--amend`/`--no-verify`), (2) allow `git push` / `git push origin *` only (not broad `git push *` which passes `--force-with-lease`/`--mirror`/`--delete`), (3) deny side strengthened with `--force-with-lease`, `--mirror`, `--delete`, `--amend`, `--no-verify`
Changes: Added git add/commit/push allow rules (minimal), added 7 deny rules for dangerous git options


### FEATURE: Claude Code permissions refinement (99e2d4f, d9d691b, 99858f4, 98ae129)
Background: Expand Claude Code allow rules for daily use
Changes: Allow `chmod +x`, piped `ls`, `find` (with deny for dangerous options), commit message blocklist scan


### REFACTOR: claude-code → claude-global rename (597bf88–f83e98c)
Background: Rename directory to avoid project-level conflict
Changes: Renamed `claude-code/` to `claude-global/`, migration logic in install scripts, handles empty dir left by git, `-Recurse` fix for non-empty directory


### REFACTOR: Claude Code rules reorganization ()
Background: Split monolithic `claude-global/CLAUDE.md` into `rules/` directory (6 rule files) + docs lifecycle rule. Unified langchain-specific commands into generic `/update-docs`, `/start-task`, `/complete-task`. Deleted 7 old langchain-specific commands. Added `rules/` symlink to install scripts (Linux + Windows). Added `tests/test-claude-rules.sh` verification script
Changes: (pending)


### REFACTOR: Claude Code commands → skills migration ()
Background: Migrated `commands/*.md` to `skills/*/SKILL.md` with YAML frontmatter (description, disable-model-invocation, argument-hint). start-task/complete-task are manual-only; update-docs/update-instruction allow auto-invocation. Updated dotfileslink (Win/Linux), CLAUDE.md, docs
Changes: (pending)


### FEATURE: Docs enforcement via hook ((pending))
Background: Wanted docs updated before every commit — rules/ is best-effort, not enforced
Changes: Created `check-docs-updated.js` PreToolUse hook (blocks `git commit` when source changes staged without `docs/`), consolidated `rules/docs-lifecycle.md` into `/update-docs` skill (self-contained), deleted rule file. Enforcement hierarchy: hook (hard block) → skill (procedure) → rule (removed as redundant)


### FEATURE: Docs convention rule ((pending))
Background: Document structure rules were only in ai-specs history.md (mixed with migration history) — not auto-loaded by Claude Code
Changes: Created `rules/docs-convention.md` (auto-loaded by Claude Code), moved file role/cascade/content rules from SKILL.md. SKILL.md now references the rule file and retains only procedural steps


### FEATURE: Claude Code PermissionRequest hook (feature/permission-hook2) (41f092c, (pending))
Background: Fixed settings.json to nested format, limited PreToolUse hook to Bash only, added MSYS/WSL path detection to private info scanner. Investigated VSCode "Ask before edits" Bash behavior: (1) `Edit
Changes: Write` PreToolUse hook presence does not affect Bash Ask behavior, (2) discovered `permissions.ask` setting — adding `Bash(git commit *)` triggers Ask dialog in "Ask before edits" mode, (3) "Edit automatically" mode ignores `permissions.ask` (auto-allows all). Achieved git commit confirmation control via `permissions.ask` — PermissionRequest hook unnecessary


### SECURITY: Private info leak prevention ()
Background: Automated scanning for private information (RFC 1918 IP, email, MAC, absolute paths) at git commit time and Claude Code edit time. Private repo whitelist skips scanning for private repos (safe default: unregistered repos are scanned). Fixed existing leaks: `.zshrc` SDKMAN hardcoded paths → `$HOME`, `.config/git/config` email → GitHub noreply. Key files: `bin/check-private-info.sh` (scanner), `bin/update-private-repos.sh` (whitelist generator), `hooks/pre-commit` (git hook), `claude-global/hooks/check-private-info.js` (PreToolUse hook), `.private-info-allowlist`, `.private-info-blocklist`
Changes: b5f60d8, d0cea99, 98ae129, 733ec29


### SECURITY: Dotenv access blocking (76b61ca)
Background: Prevent Claude Code from reading .env files
Changes: PreToolUse hook (`block-dotenv.js`) blocks Bash/Read/Grep/Glob access to `.env*` files. Deny rules provide first layer; hook catches bypass patterns (e.g., `bash -c "cat .env"`, `/bin/cat .env`). Git commit messages containing `.env` excluded from false positives via sanitization. 59 test cases


### REFACTOR: claude-code migration cleanup ((pending))
Background: All PCs migrated; remove temporary code to prevent claude-code/claude-global name confusion
Changes: Deleted `BEGIN temporary: claude-code → claude-global` blocks from `.profile_common` and `profile.ps1`. Removed `.gitignore` (only held `claude-code` entry). Removed `claude-code/` from `check-test-updated.js` EXEMPT_DIRS. Deleted migration Pester test. `install-obsolete.sh/ps1` retained as safety net


### FEATURE: Markdown exempt from code detection in hooks ((pending))
Background: `git commit` in ai-specs (docs-only repo) was blocked by PreToolUse hooks (check-test-updated.js, check-docs-updated.js) because `.md` files under `projects/` were classified as "source code".
Changes: Added `/\.md$/i` to EXEMPT_FILES in both hooks. `hasDocChanges` in check-docs-updated.js now recognizes `.md` as documentation changes. Added tests: git -C variant, uppercase .MD, idempotency, stale review marker


### FEATURE: Unified Edit confirmation flow ((pending))
Background: Edit confirmation in VSCode was inconsistent (1–3 prompts). Three sources of confirmation: (1) chat-based diff presentation triggered by CLAUDE.md/memory "show diff" instructions, (2) VSCode Ask mode's built-in diff dialog, (3) permission confirmation dialog.
Root cause: When CLAUDE.md and memory contain duplicate or conflicting instructions, the LLM follows them only probabilistically, causing inconsistent behavior. "Always show diff" coexisting with "not needed in VSCode" resulted in intermittent diff presentation.
Alternatives considered:
  (a) Force diff display via PreToolUse hook — not possible because VSCode UI does not display hook stderr/additionalContext (verified previously)
  (b) Add `Edit` to `permissions.ask` for confirmation every time — causes double confirmation with the diff dialog
  (c) Remove prompt instructions and rely solely on VSCode's built-in diff dialog — mechanically guaranteed, always exactly one confirmation
  (d) Add `Edit(**)` to `allow` to skip the permission layer — additional mitigation for first-session double confirmation (found unnecessary)
Decision: Adopted (c). Kept a conditional rule to show diff in chat only when using the CLI.
Changes: Changed `workflow.md` File Edits rule to CLI-conditional, removed diff instructions from memory, added feedback memory, changed `docs-convention.md` history.md format from table to section style.
Follow-up (2026-03-23): (c) proved unreliable — VSCode built-in diff dialog sometimes shows diff, sometimes shows bare permission prompt with no diff. Tested Edit, Write (new file), Write (overwrite): all showed diff, so the inconsistency is not tool-type-dependent. Searched exhaustively: no mechanism in Claude Code permissions/hooks/extension settings to distinguish "diff shown" vs "diff not shown". Additionally tested PermissionRequest hook (unlike PreToolUse, does not bypass Ask dialog). Hook fires for Edit/Write and receives full tool_input (old_string/new_string for Edit, content for Write). However, stderr and additionalContext are not displayed in VSCode UI — same limitation as PreToolUse. Conclusion: no hook-based path exists to force diff display in VSCode; chat-based diff presentation via prompt instruction is the only reliable method. Reverted to "always show diff in chat" policy (double confirmation acceptable, zero confirmation not). Removed CLI-only condition from `workflow.md`, updated feedback memory


### FEATURE: Private repo detection: non-GitHub hosts skip scanning
Background: `gh api` returned 404 for non-GitHub remotes (e.g., GitLab, Bitbucket), causing them to be treated as public via fail-open, which triggered private info scan blocking.
Changes: Added `extractHost()` to `is-private-repo.js` to extract hostname from remote URL. Non-`github.com` hosts (GitLab, Bitbucket, GHE, etc.) are treated as private and skip scanning. Added the same hostname check to git hooks (pre-commit, commit-msg). Added 12 tests.


### INCIDENT: #17: Test execution hangs indefinitely (9b2e88f)
Cause: Claude Code auto-generated Pester tests had bugs (WSL UTF-16 output encoding mismatch, Pester scope issue), causing background test execution to hang indefinitely.
Fix: Added mandatory timeout rule to `test.md` (`timeout 120` wrapper).


### INCIDENT: #19: Session sync init deletes other machines' sessions (a8b8e5b)
Cause: `session-sync-init.ps1` / `session-sync-init.sh` used `git reset origin/main` (mixed reset) during initialization. Mixed reset moves HEAD and index to origin/main but leaves the working tree unchanged. The subsequent `git add .` overwrote the index from the working tree (which only contained local files), staging deletions for all remote-only files (other machines' sessions). When a second PC ran init, the primary and other machines' session files were deleted. Additionally, the Windows script had a secondary bug: `$ErrorActionPreference = "Stop"` combined with `try/catch` caused git stderr output to throw a terminating exception, silently skipping the fetch/reset entirely
Fix: Changed `git reset` to `git reset --hard` so remote files are checked out into the working tree. Fixed PS scripts to temporarily set `$ErrorActionPreference = "Continue"` around git commands that may produce stderr or non-zero exit codes. Also fixed the same stderr handling bug in `session-sync.ps1` `pull --rebase`. Data was restored in `a8b8e5b`; diff between `199a29d` (pre-deletion) and `acc5467` (current) is empty — no data loss


### INCIDENT: #20: Session sync propagates Claude Code format migration deletions (28f343b)
Cause: Claude Code migrated session storage from `UUID.jsonl` (flat file) to `UUID/subagents/` (directory). On TEC, old `.jsonl` files were deleted locally by Claude Code. `session-sync push` (`git add .`) staged these deletions and pushed them to remote, removing 35 files from other machines' history
Fix: No action taken. 25/35 files had directory versions (no data loss). Remaining 10 were short sessions (1–15 lines) likely auto-pruned. One-time format migration event — will not recur. `--ignore-removal` rejected as it would block legitimate manual deletions



### REFACTOR: Private repo detection: static list → dynamic gh API (ab6820b)
Background: Keeping a full repository list locally in `private-repos.txt` risked exposing private repository names if the file was accidentally shared. Also, PreToolUse hooks could not determine private repo status when running `git -C <path> commit` (always scanned because `filePath` was empty).
Changes: Switched to dynamic detection via `gh api repos/{owner}/{repo} --jq .private`. Created shared module `claude-global/hooks/lib/is-private-repo.js`, used by all 3 PreToolUse hooks (check-private-info/check-docs-updated/check-test-updated) and 2 git hooks (pre-commit/commit-msg). Fail-open when `gh` is not installed or API call fails. Deleted `bin/update-private-repos.sh` and `.context-private/private-repos.txt`.


### REFACTOR: commands → skills migration cleanup (ab6820b)
Background: commands → skills migration completed on all PCs.
Changes: Removed `BEGIN temporary: commands → skills` blocks from `.profile_common` and `install/win/profile.ps1`. Kept claude-code symlink cleanup code in `install-obsolete.sh/ps1` as a safety net.


### FEATURE: Claude Code branch/push delete deny rules ((pending))
Background: In another project (langchain-stack), Claude deleted local and remote branches without confirmation via `git -C`. `git push --delete` was already denied, but the allow rule `git -C * push origin *` may have been approved via dynamic permission ("Yes, don't ask again"). `git branch -D` was not in deny or ask lists. Cross-reviewed with ChatGPT and also covered `git push origin :branch` (refspec-style deletion).
Changes: Added deny rules: `*git branch -D*`, `*git branch -d*`, `*git push origin :*`, `*git push *origin :*`.


### FEATURE: Claude Code session sync (513e3a4, (pending))
Background: Claude Code session history (`~/.claude/projects/`) is stored locally on each machine with no official cross-device sync. Wanted to reference past conversations across multiple Windows PCs.
Alternatives considered:
  (a) Use only Claude Code Web mode (claude.ai/code) — Local mode is required for tasks needing local filesystem, MCP servers, or NSSM service operations (NemoClaw, llama-swap, etc.). Sessions are split between Local and Web. Rejected
  (b) perfectra1n/claude-code-sync (Rust-based external tool) — the needed functionality is implementable in under 50 lines and integrates naturally into dotfiles, so chose to build in-house
  (c) Remote Control (`claude remote-control`) — requires the original PC to be running, disconnects after 10 minutes. Not suitable for history reference
  (d) Wait for Anthropic official sync (Issue #22648) — timeline unknown
Design decisions:
  (1) Path unification: unified under drive root with dedicated directories for LLM infrastructure (existing, immovable) and `C:\git\` (new). Eliminated `~/git/` which contains the username, ensuring identical absolute paths across all machines
  (2) Line ending control: cross-reviewed with ChatGPT, compared 3 approaches. `core.autocrlf=false` (stops conversion only, no LF guarantee), `* -text` (same), `* text eol=lf` (explicitly declares LF, propagates via repo). Adopted `* text eol=lf` in `.gitattributes`. Reason: JSONL needs guaranteed LF on all machines + auto-propagation on clone/pull
  (3) File writing: `Set-Content` (PowerShell cmdlet) outputs CRLF on Windows, causing git add warnings. Switched to `[System.IO.File]::WriteAllText()` with CRLF→LF replacement for LF output. Cross-reviewed with ChatGPT for compatibility (.NET standard API, PS5/PS7 compatible, no UTF-8 BOM)
  (4) Sync scope: `projects/` only. `settings.json`, `rules/`, `skills/` etc. managed by dotfiles. `statsig/`, `ide/` are machine-specific
  (5) Conflict strategy: JSONL is append-only. Avoided by push→pull routine. On conflict: `git pull --rebase`
Changes: Created `install/win/session-sync-init.ps1` (initialization), `bin/session-sync.ps1` (push/pull/status), integrated into `install.ps1`, created `nirecom/claude-sessions` private repo. 12 Pester test cases.


### FEATURE: Cross-platform check hook (5c7714e)
Background: Prevent cases where `install/win/` is modified but `install/linux/` counterpart is forgotten.
Changes: Created `check-cross-platform.js` PreToolUse hook. Detects missing counterpart changes for platform-specific files at `git commit` time. `.cross-platform-skiplist` for permanent exclusion, `.git/.cross-platform-reviewed` for one-time exclusion. 220 test cases.


### FEATURE: Session sync git root relocation (dbd003a)
Background: `~/.claude/` was used as git root for session sync, but only `projects/` was tracked. This required a complex `.gitignore` (exclude-all + re-include pattern) and caused a misleading warning in `dotfileslink.ps1` about symlink conflict with `.git`. Moving git root to `~/.claude/projects/` eliminates both issues.
Changes:
  - `session-sync-init.ps1`: git init target changed from `$ClaudeDir` to `$ClaudeDir/projects`. Added migration logic to remove old `.git`/`.gitignore`/`.gitattributes` from `~/.claude/`. Removed `.gitignore` (no longer needed). `.gitattributes` created in `projects/`
  - `bin/session-sync.ps1`: all `git -C` paths changed from `$ClaudeDir` to `$ProjectsDir` (= `$ClaudeDir/projects`)
  - `dotfileslink.ps1`: removed "dotclaude" warning about `.git` in `~/.claude/`
  - Remote repo `nirecom/claude-sessions` recreated (old layout had `projects/` prefix in tree; new layout stores files at root)
  - Tests: 12 tests updated for new layout + migration test added


### FEATURE: Session sync: terminal startup fetch + codes function (d3c3a38, 7d7a98e, 4f8e4c3, 20ebb49)
Background: session-sync push/pull required manual execution. Wanted automatic fetch on terminal startup and automatic push when closing VS Code
Changes:
  - `.profile_common`: added `git fetch + merge --ff-only` for `~/.claude/projects/` on terminal startup (3s timeout). Added `codes` function (opens VS Code, runs `session-sync.sh push` on exit)
  - `install/win/profile.ps1`: equivalent fetch logic added
  - `bin/session-sync.sh`: new Linux/macOS session sync script (push/pull/status subcommands)


### FEATURE: Session sync: cross-platform support (d50203c, (pending))
Background: Session sync was Windows-only but the same session sharing is needed on macOS/Linux
Changes:
  - `install/linux/session-sync-init.sh`: new (equivalent to `session-sync-init.ps1` — git init, migration, remote setup, initial commit+push)
  - `install.sh`: added session-sync-init call after Claude Code install (`type claude` guard)
  - `install-obsolete.sh`: added Homebrew fnm cleanup (`brew list fnm && brew uninstall fnm`; not originally installed via dotfiles but cleaned up as a precaution)


### BUGFIX: Session sync: init/push reliability fix (efbd047)
Background: `session-sync-init.sh`'s initial commit flow did not account for cases where remote already has history, causing init → push to fail on 2nd+ machines. Push also failed when remote was ahead.
Design decisions:
  (1) `reset --hard` vs `reset` (mixed): `reset --hard` risks overwriting local JSONL files that share names with remote (happens when two machines use the same path). `reset` (mixed) only moves HEAD, preserving local files. File checkout is handled by normal pull
  (2) Background push output from `codes()`: output interrupts after prompt, requiring an extra Enter. Attempted SIGWINCH-triggered prompt redraw but it had no effect. Suppressed output to `/dev/null`
Changes:
  - `session-sync-init.sh`/`.ps1`: added `fetch origin main` + `reset origin/main` before initial commit to merge remote history. Tolerate `commit` failure when there are no changes (`|| true`)
  - `bin/session-sync.sh`/`.ps1`: added `pull --rebase origin main` before push
  - `.profile_common`: suppressed `codes()` push output with `>/dev/null 2>&1`


### FEATURE: Session sync: separate init from sync, add reset action (fc71b9e)
Background: Init script's fetch/reset/commit/push block was dangerous — it could overwrite remote data when a new PC joined (#19). Also, auto-running sync during install made it hard to control which PC should sync first
Changes:
  - `session-sync-init.ps1`/`.sh`: removed all fetch/reset/commit/push logic. Init now only sets up plumbing (git init, remote, .gitattributes, hooks)
  - `session-sync.ps1`/`.sh`: added `reset` action (`git fetch + reset --hard origin/main`) for manual initial sync or recovery. After reset, normal bidirectional push/pull works
  - New PC flow: `install.ps1` → init (safe), then manual `session-sync reset` to catch up to remote


### FEATURE: Session sync: history.jsonl sync and mtime restore (d214411, 77d00e4)
Background: Sessions synced from other machines were invisible or misordered on the receiving PC. Two root causes:
  (1) Claude Code uses `~/.claude/history.jsonl` (outside the sync git root `~/.claude/projects/`) for session listing. Without syncing this file, the receiving PC had no record of remote sessions
  (2) Claude Code uses file mtime for session ordering. `git reset --hard` sets all file mtimes to the current time, making all synced sessions appear as "just now"
  Investigation of OSS tools (x-cmd/x-cmd ★4k, mad0ps/claude-code-server-syncer) confirmed that history.jsonl rewriting and/or sessions-index.json manipulation is standard practice. x-cmd rewrites history.jsonl `.project` field on import; mad0ps deletes sessions-index.json and lets Claude Code regenerate it. Neither preserves file mtime.
  Additionally, session directory names encode the working directory path (`C:\git\dotfiles` → `c--git-dotfiles`). When the same project was opened from different paths on different machines (or after a path migration), sessions were stored in separate directories and invisible to each other. Resolved by copying the old path directory to the new path name and rewriting history.jsonl project fields.
Changes:
  - Push: copies `~/.claude/history.jsonl` into sync area as `.history.jsonl`
  - Pull/Reset: merges remote `.history.jsonl` with local history (line-level dedup, remote-first ordering)
  - Reset: restores mtime on each `.jsonl` from its last (or first, as fallback) `timestamp` field


### FEATURE: Session sync: Claude Code format migration causes bulk deletion (28f343b)
Cause: Claude Code migrated session storage format from `UUID.jsonl` (flat file) to `UUID/subagents/` (directory structure). PENPEN pushed both old and new formats on 3/29. When TEC ran `session-sync push` on 4/2, Claude Code had already deleted the old `.jsonl` files locally, so `git add .` staged 35 deletions and propagated them to remote.
Fix: No action needed (keep as-is). 25 of 35 files have directory versions — no data loss. Remaining 10 are short sessions (1–15 lines) likely pruned by Claude Code, with no practical impact. The format migration is a one-time event and will not recur. Changing `git add .` to `--ignore-removal` was considered but rejected — it would also block intentional manual deletions and legitimate prunes.


### FEATURE: Notification hook for permission_prompt (f447e11, 7ef7776)
Background: Claude Code permission_prompt dialogs were easy to miss, leaving sessions idle
Changes: Added `check-notification.js` PreToolUse hook that sends OS toast notification on `permission_prompt` events. Moved WebSearch/WebFetch from auto-allow to `permissions.ask` (now requires explicit confirmation)


### FEATURE: Auto-unstage effortLevel-only changes in settings.json (84db276)
Background: effortLevel is written to settings.json by Claude Code's /fast toggle and effort level changes. Since settings.json is tracked in git, effortLevel-only changes kept appearing in git diff, requiring manual cleanup. Previous fix (84db276) removed effortLevel from the committed file, but it reappears whenever Claude Code writes it.
Changes: Added auto-unstage logic to claude-global/hooks/pre-commit. On commit, compares HEAD vs staged settings.json after stripping effortLevel (via node JSON parse). If identical, auto-unstages the file. Handles addition, value change, and removal of effortLevel. Mixed changes (effortLevel + real content) are preserved. Also fixed `core.hooksPath` on Windows: shared `.config/git/config` has `~/dotfiles/...` (works on Linux/macOS), but Windows dotfiles is at `C:\git\dotfiles`. Added `core.hooksPath` override in `config.local` via `dotfileslink.ps1`.


### FEATURE: Add research phase to workflow
Background: Investigated well-known Claude Code frameworks (Alex Kurkin's Research→Plan→Implement, everything-claude-code, claude-research-plan-implement). Found that a research phase before planning prevents implementing against wrong assumptions. Separated into two skills: `/survey-code` (codebase exploration) and `/deep-research` (web research), both at effort: medium — low loses cross-source reasoning, high is overkill for information gathering. Name choices: `research` alone risks future built-in conflict (precedent: `/plan` → `/make-plan`). `deep-research` aligns with established OSS convention. `survey-code` distinguishes from web research.
Changes: Added `claude-global/skills/survey-code/SKILL.md` and `claude-global/skills/deep-research/SKILL.md`. Updated `claude-global/CLAUDE.md` workflow from 7 steps to 8 (Research as step 1).


### BUGFIX: Fix update-docs skill to detect uncommitted changes
Background: In the workflow, /update-docs runs before commit (step 5), but change detection relied solely on `git log`. Uncommitted session changes were invisible, causing documentation gaps.
Changes: Added `git diff` / `git diff --cached` to the gather step to detect unstaged and staged changes. Removed LangChain-specific instruction from the procedure, delegating to the Project Detection section.


### REFACTOR: Workflow rules reorganization
Background: workflow.md contained a mix of procedural rules (verification, diff approval) and design policies (cross-platform/naming orthogonality). Rules files are always loaded into context, so there was no stage-gate mechanism — workflow rules had no more prominence than any other rule file. Community research showed plan mode thresholds vary, with Anthropic's official guidance being "skip if the task is describable in one sentence."
Changes:
- Dissolved `workflow.md` — procedural steps moved to global `CLAUDE.md` as numbered Workflow (Plan → Write tests → Code → Test & Verify → Docs → User verification → Commit)
- Orthogonality rules extracted to `rules/orthogonality.md` (referenced by `coding.md` and `make-plan/SKILL.md`)
- `test.md` reordered: Test Case Categories first, removed duplicate "test before code" instruction (now in CLAUDE.md)
- Removed `Rules are in rules/. Skills are in skills/.` pointer from CLAUDE.md (auto-discovered by Claude Code)


### FEATURE: Add write-tests and make-plan skills with effort: high (23e4eba, 72f4168)
Background: Procedural instructions in rules/test.md were sometimes ignored under context pressure, and effort level could not be controlled. Specifying effort: high in the skill frontmatter raises reasoning effort only during skill execution.
Changes: Created /write-tests and /make-plan skills (effort: high). Replaced procedural instructions in rules/test.md with /write-tests invocation. Category definitions, naming conventions, and timeout rules remain in rules/ (shared SSOT with /review-tests). Initially created as /plan but renamed to /make-plan due to built-in command conflict. effort frontmatter is an official feature added in v2.1.80 (priority: env var > skill frontmatter > /effort session > model default). Known bug: VS Code UI does not reflect effort changes (anthropics/claude-code#31751), but the switch works internally.


### BUGFIX: Fix codes session sync not firing per-window
Background: `code --new-window --wait`'s `--wait` flag waits for the entire VS Code server process to exit. When multiple windows are open, closing an individual window did not trigger session-sync push. No push ran unless all windows were closed.
Changes: Replaced `--wait` with window title polling via Win32 EnumWindows API (Windows) / xdotool, wmctrl, osascript (Linux/macOS). Created `bin/wait-vscode-window.ps1` and `bin/wait-vscode-window.sh`. The `codes` function resolves the workspace name and detects the target window's appearance then disappearance before running push. Also handles `.code-workspace` files with `(Workspace)` title suffix.


### SECURITY: Security enhancement research and planning (uncommitted)
Background: No systematic security checklist during architecture planning (information leakage, third-party access, external access). No security test coverage guidelines. OWASP MCP Top 10 identified prompt injection via MCP plugins as a new threat vector.
Changes: Researched external sources (OWASP WSTG/ASVS/LLM Top 10/MCP Top 10, everything-claude-code, lasso-security/claude-hooks, Gitleaks vs Semgrep comparison). Documented 4-phase plan in `docs/plan.md`. Key design decision: extract security checklists and patterns into skills (not rules/) to minimize always-loaded context window consumption. Skill naming validated against major frameworks (everything-claude-code `/security-scan`, qdhenry `/security:security-audit`, etc.) — `verb-noun` kebab-case is the dominant convention.


### FEATURE: Reduce WebSearch/WebFetch permission prompts (uncommitted)
Background: During deep-research skill execution, repeated permission dialogs for WebSearch and WebFetch were a UX obstacle. Users could not assess the risk of unknown URLs, and the permission prompts provided no real security value. Conducted two security reviews with ChatGPT, which led to excluding user-generated content domains like GitHub from the allow list.
Changes: Added WebSearch and 8 low-risk documentation domain WebFetch entries (MDN, Python docs, Microsoft Learn, man7, Anthropic docs, OpenAI docs, Google AI docs, GitHub Docs) to settings.json allow. Removed WebSearch and WebFetch(domain:github.com) from ask. Domain selection criterion was not "trust" but "whether existing defense layers (deny rules, diff review, pre-commit hook) prevent damage in case of incident."


### FEATURE: Restructure /update-docs skill for ai-specs project coverage (uncommitted)
Background: /update-docs Project Detection only covered langchain-stack projects. As ai-specs grew to include llama-swap, judgeclaw, and others, coverage was insufficient. The llama-swap docs update procedure was also scattered in ai-specs/CLAUDE.md separately.
Changes: Renamed LangChain projects to ai-specs projects. Added llama-swap and judgeclaw to Source repos. Reordered sections (General projects first, ai-specs second). Simplified ai-specs/CLAUDE.md llama-swap section to a pointer to the /update-docs skill. Attempted external file separation via `!`cat`` preprocessing but abandoned due to security sandbox (blocks cat outside working directory).

