# History (dotfiles)

### CONFIG: starship: increase command_timeout to 2000ms (2026-04-13)
Background: starship prompt showed timeout warnings when changing into the dotfiles directory. Root cause: git fsmonitor daemon was not yet running, and daemon startup exceeded starship's default 500ms command_timeout.
Changes: Set `command_timeout = 2000` in `.config/starship.toml`.

### BUGFIX: Fix wait-vscode-window.sh: add WSL2 window detection (2026-04-13, pending)
Background: In WSL2, VS Code runs as a Windows process so xdotool/wmctrl (X11 tools) are unavailable. The `codes` function called wait-vscode-window.sh which fell through to the "No window detection tool found" warning and skipped session sync.
Changes: Added WSL2 branch to bin/wait-vscode-window.sh that uses powershell.exe to enumerate Code.exe MainWindowTitle values via [System.Diagnostics.Process]::GetProcessesByName. Detects WSL via WSL_DISTRO_NAME env var. Updated tests/main-wait-vscode-window.sh to cover WSL_DISTRO_NAME detection. Updated docs/architecture.md.

### BUGFIX: Fix QNAP dotfileslink.sh: guard autorun.sh install against mount failure (2026-04-13)
Background: On QNAP, the first run of install.sh printed "autorun.sh installed." even though the mount failed — /tmp/config directory did not exist, causing sudo mount to fail silently. The script had no set -e, so errors were ignored.
Changes: Added mkdir -p /tmp/config before mounting. Wrapped mount in an if block; success message and subsequent cp/chmod/umount only execute when mount succeeds. Added WARNING message on failure.

### BUGFIX: Fix install.ps1: missing registry values crash on keyboard hotkey setup (2026-04-15)
Background: On Windows machines where Keyboard Layout\Toggle registry key values (Language Hotkey, Layout Hotkey, Hotkey) have never been set, Get-ItemProperty threw a terminating error that -ErrorAction SilentlyContinue could not suppress.
Changes: Replaced Get-ItemProperty with Get-Item + RegistryKey.GetValue(), which returns $null for missing values without throwing.

### BUGFIX: fix: add uv call to install.sh and align --develop inclusion with Windows (2026-04-17, pending)
Background: install/linux/uv.sh existed but was never called from install.sh. The --develop flag also did not execute the base package block, breaking orthogonality with Windows install.ps1 (-Develop ⊇ -Base).
Changes: Added uv.sh call to the base block in install.sh. Changed the base block condition to --base || --develop || --full to align with the Windows inclusion relationship (Full ⊇ Develop ⊇ Base).

### CONFIG: profile.ps1: suppress create/delete mode output in startup git pull (2026-04-18, pending)
Background: On pwsh startup, the auto-pull's `git merge --ff-only` emitted the full summary (dozens to hundreds of `create mode` / `delete mode` lines, especially for the session sync repo) after the diffstat. The Fast-forward signal and change-size indicator (`++++`) are desired, but the summary is noise.
Changes: Added `--no-summary` to both `git merge --ff-only` invocations in `install/win/profile.ps1` (dotfiles auto-pull and session sync). Keeps the `Fast-forward` line and diffstat; suppresses `create mode` / `delete mode` lines only.
### FEATURE: claude-usage-widget: auto update support (2026-04-18, pending)
Background: When running install.ps1 / install.sh, claude-usage-widget was always skipped if already installed with no version check. User wanted install.ps1 to update to the latest version automatically. Scope intentionally limited to claude-usage-widget only (other installers not changed due to effort).
Changes: install/win/claude-usage-widget.ps1: fetch GitHub release tag_name up-front, compare against installed ProductVersion via VersionInfo, skip if up to date, reinstall if newer. Poll loop now checks version equality instead of mere exe existence. install/linux/claude-usage-widget.sh: macOS reads CFBundleShortVersionString and rm -rf old .app before reinstall on update; Linux uses APPIMAGE_PATH.version sidecar file for version tracking, writes version after install. get_latest_version updated with sed s/^v// for explicit v-prefix stripping. tests: Update cases section added (12 cases) to both test files; existing already-installed assertions updated to up-to-date.

Follow-up: two issues surfaced on first run — (a) running widget locks its exe so silent installer fails with a dialog (user sees perpetual "Waiting for install to complete"); (b) ProductVersion is 4-part "1.7.2.0" while tag_name is 3-part "1.7.2" so the post-install poll never matches. Added ConvertTo-NormalizedWidgetVersion helper (strips trailing .0) used in both update-check and poll comparisons. Before install, Get-Process detects running widget and Stop-Process -Force kills it (autostart restarts it later). 4 additional Pester cases cover Stop-Process, Get-Process detection, version normalization regex, and normalization-intent documentation. 22 total Pester tests green.

### FEATURE: installer: add PowerShell Core install/update to install.ps1 (2026-04-22, pending)
Background: pwsh (PowerShell Core) install/update functionality was missing from the Windows installer. New machines required manual pwsh installation before running dotfiles scripts.
Changes: Added install/win/pwsh.ps1: installs Microsoft.PowerShell via winget if absent, runs winget upgrade when already installed. Added Step 7 in install.ps1 default section (always runs, not gated behind -Base).

### INCIDENT: #25: winget corrupts WinHTTP state in-process — Invoke-RestMethod throws FileNotFoundException (2026-04-23, pending)
Cause: winget (Windows App Installer) calls WinHTTP APIs internally during winget list / winget upgrade. On return, it leaves the process-wide WinHTTP proxy/handler state in a corrupted configuration. Any subsequent Invoke-RestMethod call in the same session (which uses WinHTTP on Windows) throws System.IO.FileNotFoundException with an empty exception message — misleading because the actual failure is in the HTTP layer, not file I/O. Reproduces reliably: call winget in a PowerShell session, then call Invoke-RestMethod.
Fix: Run Invoke-RestMethod (or any PowerShell web cmdlet) in a fresh subprocess (pwsh.exe -NoProfile) rather than in the same session that called winget. In install.ps1, pwsh.ps1 is now launched as a -NoProfile subprocess so its GitHub API call is isolated from the winget-polluted parent session. Alternative mitigation: call Invoke-RestMethod before any winget invocation in the same session.

### BUGFIX: pwsh.ps1: update path rewrite — GitHub API, elevation, binary version, WinHTTP isolation (2026-04-23, pending)

Background: Initial winget upgrade approach had 5 bugs found during testing. See INCIDENT #25 for the WinHTTP issue.

Changes: (1) Switched update detection from winget upgrade to GitHub Releases API — locale-independent and faster than winget package sync. (2) Fixed Start-Process exit code check: use $proc.ExitCode (-PassThru) instead of $LASTEXITCODE. (3) Added -Verb RunAs to msiexec for UAC elevation. (4) Reads installed version from pwsh.exe binary instead of $PSVersionTable.PSVersion (running session reflects pre-upgrade version). (5) install.ps1 now spawns pwsh.ps1 as -NoProfile subprocess to avoid WinHTTP corruption from prior winget calls. Tests: 45 Pester unit tests in tests/main-pwsh.Tests.ps1.

### BUGFIX: install.ps1: add Invoke-ScriptIsolated helper to fix WinHTTP corruption for all scripts (2026-04-23, 2e1c75a)

Background: INCIDENT #25 identified that winget corrupts the process-wide WinHTTP state, breaking subsequent Invoke-RestMethod calls with a silent FileNotFoundException. The fix was applied to pwsh.ps1 only (subprocess isolation), but all other scripts called from install.ps1 that use web requests were equally affected: claude-usage-widget.ps1, claude-tabs.ps1, uv.ps1, claude-code.ps1, awscli.ps1, vs-cpp.ps1.

Changes: Added Invoke-ScriptIsolated helper to install.ps1 that runs each script via pwsh.exe -NoProfile -ExecutionPolicy Bypass -File, creating a fresh process that does not inherit the corrupted WinHTTP state. Replaced all web-request script calls (claude-code, fnm, pwsh, uv, claude-usage-widget, claude-tabs, awscli, vs-cpp) with Invoke-ScriptIsolated. Removed the ad-hoc pwsh.ps1 subprocess block and unified it under the helper.

### BUGFIX: install.ps1: fix Wait-MsiMutex to check Global\_MSIExecute mutex instead of process presence (2026-04-25, pending)

Background: Wait-MsiMutex used Get-Process msiexec to detect a running installer. On Windows, msiexec.exe is always present as an idle process, so the check always triggered and the installer always printed 'Waiting for another installer to finish...' even when no MSI was active.

Changes: Replaced Get-Process msiexec with [System.Threading.Mutex]::TryOpenExisting('Global\_MSIExecute') which is only held when MSI is actively performing an installation. The acquired mutex handle is disposed immediately after the check to avoid holding it.

### BUGFIX: autohotkey.ps1: suppress WinPSCompat warning when loading International module (2026-04-25, pending)

Background: autohotkey.ps1 used Import-Module International -UseWindowsPowerShell to load a Windows PowerShell-only module in pwsh. PowerShell emits a verbose WinPSCompatSession warning on every load. A prior attempt to suppress it with -SkipEditionCheck caused the nested module Microsoft.InternationalSettings.Commands to fail to load, silently skipping the entire AutoHotkey setup.

Changes: Wrap the Import-Module call with $WarningPreference = 'SilentlyContinue' / restore pattern. -UseWindowsPowerShell is kept (correct for this module); only the cosmetic warning is suppressed.

### REFACTOR: agents-split step 13: dotfiles cleanup (2026-04-25, pending)
Background: agents repo split の step 13。claude-global/ ディレクトリ、claude 関連 bin/ ツール、install スクリプトの Claude Code 呼び出し、@claude 分類テスト（39 本）を dotfiles から削除。
Changes: git rm claude-global/(45 files), bin/{doc-append,doc-rotate,sort-history,translate-history,convert-history-table,migrate-history-categories,scan-outbound,session-sync,split-history}.py/.sh, install/win/{claude-code,session-sync-init}.ps1, docs/{scan-outbound,hook-block-tests-direct}.md, docs/architecture/claude-code.md。docs/history.md を history-dotfiles.md で置換。install.sh/ps1 から claude-code.sh/ps1 呼び出し削除。dotfileslink.{sh,ps1} から claude-global symlink・hooksPath 設定・doc-append launcher 生成を削除。@claude 分類テスト 39 本を dotfiles/tests/ から削除（agents/tests/ に移送済み）。

### REFACTOR: Remove claude-global and agents framework (step 13) (2026-04-25, 40666ea)
Background: Step 13 of dotfiles to agents repo split: remove all Claude Code framework files from dotfiles now that they live in nirecom/agents repo.
Changes: Deleted claude-global/ (CLAUDE.md, settings.json, hooks/, rules/, skills/, agents/), bin/ (doc tools, scan-outbound.sh, session-sync scripts), docs/ (claude-code.md, scan-outbound.md, history-agents.md, history-classification*), install/win/claude-code.ps1 and session-sync-init.ps1, and 96 test files classified as @claude or belonging to agents-side bin/ tools. Updated install.sh/install.ps1 to remove claude-code and session-sync-init steps. Updated dotfileslink.sh/ps1 to remove claude-global symlink blocks, core.hooksPath config, and doc-append launcher generation. Fixed tests/main-vscode.sh hook path to use sibling agents/hooks directly.

### FEATURE: Step 16: remove compat blocks, use sibling-detected profile-snippet (2026-04-25, )
Background: Step 16 of dotfiles→agents repo split. dotfiles side: remove temporary compat blocks and delegate Claude-related env var setup to agents repo via sibling detection.
Changes: profile.ps1: removed BEGIN/END temporary compat block (AGENTS_CONFIG_DIR/AGENTS_DIR), replaced hardcoded $AgentsDir='C:\git\agents' with sibling detection Join-Path (Split-Path -Parent $DotfilesDir) 'agents', added Test-Path-guarded sourcing of agents/profile-snippet.ps1, removed CLAUDE.md and settings.json from $symlinkFiles (now agents responsibility). .profile_common: replaced ~/.agents_profile sourcing with sibling-detected agents/profile-snippet.sh sourcing via _agents_dir variable.

### FEATURE: agents repo split project complete (Steps 1-16) (2026-04-25, pending)
Background: 16-step migration of Claude Code framework (claude-global/) from this dotfiles repo into standalone public repo nirecom/agents. Driver: GitHub awesome-lists (awesome-claude-code, awesome-claude-code-toolkit, awesome-claude-skills, awesome-agent-skills) only discover standalone repos; mixed dotfiles-style configurations are invisible. Secondary: reduce LLM attention load (213 dotfiles + 46 claude-global files now separated). Future-proofing: leaves room for Codex/Cursor/Gemini CLI configs in same repo as AGENTS.md emerges as universal standard.
Changes: Final architecture: sibling repos at $HOME/git/{dotfiles,dotfiles-private,agents}. dotfiles install.{sh,ps1} optionally invokes sibling agents/install.{sh,ps1} (mirrors dotfiles-private pattern). Step 16 originally specified generating ~/.agents_profile.{ps1} via agents installer; changed to Option B (sibling-static profile-snippet.{ps1,sh} committed in agents repo, sourced via sibling detection $DOTFILES_DIR/../agents) — eliminates install-time-generated files and removes hardcoded C:\git\agents from profile.ps1. dotfiles profile.ps1 $symlinkFiles no longer includes CLAUDE.md/settings.json (now agents responsibility). agents-only mode supported via idempotent BEGIN/END marker rc append in install.{sh,ps1}. Step 17 (awesome-lists posting) deferred to docs/todo.md.

### REFACTOR: Remove stale claude-global/rules/language.md symlink and directory (2026-04-25, pending)
Background: agents-split step 13 removed claude-global/ from dotfiles, but the dotfiles-private installer had left a language.md symlink under dotfiles/claude-global/rules/. The directory was empty otherwise and should have been removed at split time.
Changes: Removed dotfiles/claude-global/rules/language.md symlink, dotfiles/claude-global/rules/, and dotfiles/claude-global/. Extended tests/main-language-policy.sh with 11 new assertions covering new agents-side paths (31 total).

### install-obsolete: leftover dotfiles/claude-global/ directory cleanup (2026-04-26, pending)
Background: agents-split で claude-global/ がトラックから外れた後、git reset --hard では untracked files として残った dotfiles/claude-global/ ディレクトリが除去されない。commit 6ceb67e の install-obsolete 修正は ~/.claude/ のシンボリックリンク削除のみ対象で、dotfiles/claude-global/ 本体には触れていなかった。
Changes: install/linux/install-obsolete.sh と install/win/install-obsolete.ps1 の両方に dotfiles/claude-global/ ディレクトリ削除ブロックを追加。安全性確保のため `git ls-tree HEAD claude-global` で HEAD トラック状態を検査し、トラックされていない場合のみ削除する。これにより agents-split 前の状態のマシン（まだ pull していない / 古い checkout）では誤削除を防止。dotfiles/.git が無い場合も no-op。tests/main-install-obsolete-claude-links.sh と tests/main-install-obsolete-claude-links.Tests.ps1 にケース C1〜C5 を追加（untracked 削除 / tracked 保持 / missing / non-git / nested 再帰）、Linux 13 ケース全 PASS。

### CONFIG: install-obsolete: remove ~/.gitconfig deletion prompt (agents uses it for hooksPath) (2026-04-26, pending)
Background: commit 6caa366 (2026-04-11) added a prompt in install-obsolete to delete ~/.gitconfig because it was overriding the dotfiles-managed XDG git config and causing private emails to leak into commits. Later, the agents installer (commit d8b7ee7) intentionally changed core.hooksPath write destination to ~/.gitconfig. Reason: config.local is a symlink to dotfiles-private/config.local.linux (tracked), so writing an absolute path there dirties a tracked file; --global falls back to ~/.config/git/config (dotfiles-managed, also tracked) under XDG precedence. ~/.gitconfig is untracked by any repo and is the correct place for machine-local per-user config. This created a conflict: dotfiles install-obsolete would delete what agents intentionally wrote.
Changes: Removed the ~/.gitconfig deletion prompt block from install-obsolete.ps1 and install-obsolete.sh. Deleted tests/main-install-obsolete-gitconfig.Tests.ps1 (the logic it tested no longer exists).

### FEATURE: dotfiles relocatable 化 — ~/git/dotfiles へ移動完了 (2026-04-27, 82852bc)
Background: dotfiles を ~/dotfiles から ~/git/dotfiles へ移動。82852bc で relocatable 化（ハードコードパス除去）を実施済み。移動後に install.sh が ~/.dotfiles_env を新パスで再生成し symlink を張り直す設計。
Changes: ~/dotfiles → ~/git/dotfiles へ移動完了。DOTFILES_DIR, ~/.bash_profile symlink, ~/.dotfiles_env すべて新パスに更新。tests/main-installer-idempotency.sh 10/10 pass 確認。

### CONFIG: install.sh: add ANSI color output (2026-04-27, pending)
Background: On macOS/Linux, install.sh used plain echo with no color formatting, while the Windows install.ps1 already used Write-Host -ForegroundColor. The visual gap made the macOS installer harder to follow at a glance.
Changes: Added TTY-detected ANSI color variables (C_CYAN, C_GREEN, C_YELLOW, C_BOLD, C_RESET) to install.sh. === headers print in cyan, --- section markers in bold, === Done === in green, error/warning messages in yellow. No escape codes emitted when stdout is not a terminal (pipe/log safe).

### CONFIG: installer: gray color for already-installed/linked messages on Linux/WSL (2026-04-27, pending)
Background: On Windows, already-handled items (Already linked, already installed) appear in DarkGray via Write-Host, reducing visual noise. On Linux/WSL, these were plain white echo — making re-runs harder to scan.
Changes: Added bin/colors.sh shared color utility (C_GRAY=\033[0;90m, TTY-detected). install.sh now sources it instead of inline definitions. dotfileslink.sh gains a link_file() function that prints Already linked in gray, Relinking in yellow, and Linked in green, replacing blind ln -sf calls. keychain.sh and nvm.sh print already installed messages in gray. dotfiles-private/install/linux/dotfileslink.sh updated to use the same color scheme via sourced colors.sh.

### FEATURE: installer: add gh (GitHub CLI) to default install steps (2026-04-28, pending)
Background: hooks/check-japanese-in-docs.js and hooks/lib/is-private-repo.js use gh CLI to detect whether a repo is private via GitHub API. Without gh, isPrivateRepo() catches the error and returns false (fail-open), causing Japanese content to be blocked even in private repos. gh was missing from all installers.
Changes: Added install/linux/gh.sh (ubuntu: apt with GitHub keyring; macos: brew install gh; gray already-installed output). Added install/win/gh.ps1 (winget install GitHub.cli; DarkGray already-installed output). Both placed in the default (no-flag) install step — not gated behind --base/-Base — so gh is available on every machine. Tests: tests/main-gh.sh (15 cases, all pass) and tests/main-gh.Tests.ps1 (static + dynamic Pester cases). Also fixed check-cross-platform.js error message to specify --short hash for the one-time skip marker.

### BUGFIX: Fix codes function: session-sync path pointed to dotfiles/bin instead of agents/bin (2026-04-29, pending)
Background: The codes function in profile.ps1 set $syncScript to $DotfilesDir\bin\session-sync.ps1, but session-sync.ps1 lives in agents/bin. The background process failed silently (hidden window) and no toast notification appeared after VS Code was closed.
Changes: Changed $syncScript to use $AgentsDir\bin\session-sync.ps1 in install/win/profile.ps1. Also removed a duplicate agents profile sourcing block at the bottom of profile.ps1 (already sourced conditionally at the top via $AgentsDir).

### FEATURE: install-obsolete: remove stale core.hooksPath from config.local with y/n prompt (2026-04-29, pending)
Background: core.hooksPath was moved from the dotfiles-tracked .config/git/config to ~/.gitconfig (written by agents installer). Machines that had hooksPath in config.local (untracked machine-local override) would retain the stale entry indefinitely since git pull does not update untracked files.
Changes: install/linux/install-obsolete.sh and install/win/install-obsolete.ps1: added a block that detects hooksPath matching *agents/hooks* in config.local, explains it is now managed via ~/.gitconfig, and prompts y/N before removing.

### CONFIG: Move dotfiles.code-workspace to dotfiles-private (2026-04-30, -)
Background: dotfiles.code-workspace existed in both repos; dotfiles-private had a stale 2-folder version
Changes: Removed from dotfiles repo; dotfiles-private now has the canonical 4-folder layout (dotfiles, dotfiles-private, agents, fornix)

### FEATURE: installer: migrate install hub to dotfiles-private (2026-04-30, pending)
Background: dotfiles/install.{ps1,sh} was the install chain hub, calling dotfiles-private and agents at the end. This inverted the ownership: the public repo was driving the private one. Migrated the hub role to dotfiles-private/install.{ps1,sh}, which clones sibling repos (dotfiles, agents, fornix) via clone_if_missing / Initialize-Repo helpers if absent, then calls each installer in order. dotfiles/install.{ps1,sh} is now self-contained. IS_DOTFILES_SLAVE env var lets dotfiles/install.sh skip its own exec $SHELL -l when called from the hub, so the hub can continue to agents/fornix after dotfiles completes.
Changes: Removed dotfiles-private and agents chain calls from dotfiles/install.ps1 and install.sh. Added IS_DOTFILES_SLAVE guard around exec $SHELL -l in install.sh. Rewrote dotfiles-private/install.{ps1,sh} as hub with Initialize-Repo / clone_if_missing helpers. Created dotfiles-private/docs/architecture.md documenting the hub design.

### BUGFIX: Fix session-sync file-tree links to point to agents repo (2026-05-01, 02680bc)
Background: After the dotfiles-to-agents repo split, four file-tree.md entries for session-sync scripts still referenced nirecom/dotfiles URLs. The actual files had moved to nirecom/agents.
Changes: docs/architecture/file-tree.md: updated session-sync-init.ps1, session-sync-init.sh, session-sync.ps1, session-sync.sh links from nirecom/dotfiles to nirecom/agents.

### FEATURE: installer platform guards + test suite (install.sh/install.ps1) (2026-05-01, pending)
Background: install.sh only blocked MINGW via detectos.sh ($OSDIST=mingw), leaving MSYS2 and Cygwin undetected. install.ps1 had no guard against being run on Linux/macOS via PowerShell Core 7+. Tests written TDD-first before implementation.
Changes: install.sh: replaced OSDIST=mingw check with direct uname -s check covering MINGW*/MSYS*/CYGWIN* with printf %s format injection safety. install.ps1: added $IsWindows -eq $false guard before Set-StrictMode. New tests/main-install-guards.sh (36 cases: S syntax/G grep presence/B behavioral/I idempotency) and tests/main-install-guards.Tests.ps1 (14 Pester cases: S syntax/G guard presence/L logic/P placement), all PASS.

### FEATURE: add fornix-agent to parallel git fetch on shell startup (2026-05-01, pending)
Background: dotfiles-private install.ps1 clones fornix-agent as a sibling repo, but profile.ps1 did not include it in the parallel fetch list on PowerShell startup.
Changes: install/win/profile.ps1: added $FornixAgentDir variable and fornix-agent fetch/merge block alongside dotfiles, dotfiles-private, agents, and session-sync.

### FEATURE: gh: prompt auth login if not authenticated after install (2026-05-01, pending)
Background: repo-visibility tool requires gh to be authenticated. Running install.sh without gh auth login caused silent misdetection of repo visibility.
Changes: gh.sh and gh.ps1: skip exit 0 when gh is installed but not authenticated; fall through to gh auth login. Both platforms updated.

### BUGFIX: profile.ps1: fnm init order fix + duplicate snippet sourcing removed (2026-05-02, pending)
Background: dotfileslink.ps1 calls node to run assemble-settings.js. The agents profile-snippet sources dotfileslink.ps1 when symlinks need repair, but fnm was initialized after the snippet was sourced, so node was not in PATH on pwsh startup. Additionally, profile-snippet.ps1 was sourced twice (dynamic path near top + hardcoded BEGIN/END block at bottom), triggering double symlink repair and double settings assembly on every shell start.
Changes: Moved fnm initialization in install/win/profile.ps1 to before the agents snippet sourcing. Removed the duplicate hardcoded BEGIN/END snippet sourcing block.

### FEATURE: AWS profile auto-switch in bash/zsh and PowerShell profiles (2026-05-03, pending)
Background: AWS personal/work profiles auto-switch based on directory prefix. When inside AWS_WORK_DIR the shell uses the work profile; otherwise personal. Block is skipped entirely if AWS_WORK_DIR is unset, leaving existing AWS_PROFILE untouched.
Changes: .profile_common: added _aws_select_profile function with bash PROMPT_COMMAND / zsh chpwd_functions hook (idempotency guards on both). install/win/profile.ps1: added Select-AwsProfile + Set-LocationWithFnmAndAws (integrates fnm cd-hook). Trailing-slash normalization and false-prefix guard on both platforms. Tests: tests/main-aws-profile.sh (14 cases), tests/main-aws-profile-ps.Tests.ps1 (13 cases), all pass.

### BUGFIX: AWS profile switch: unset AWS_PROFILE outside work dir instead of forcing personal (2026-05-03, pending)
Background: Setting AWS_PROFILE=personal when outside AWS_WORK_DIR silently overrode any manually set profile (Codex concern #2). Using a fixed name also required users to create a named personal profile even if they only had the default profile.
Changes: .profile_common and install/win/profile.ps1: changed outside-work-dir branch to unset AWS_PROFILE and AWS_DEFAULT_REGION instead of setting them to personal/us-east-1. Tests updated: bash 15 cases, Pester 13 cases, all pass.

### FEATURE: source ~/.config/dotfiles/local.env for machine-specific overrides (2026-05-03, pending)
Background: dotfiles-private's install.sh needs a way to inject machine-specific env vars (e.g. AWS_WORK_DIR) without modifying the public dotfiles repo directly. A sourcing hook in .profile_common provides a stable extension point for private overrides.
Changes: .profile_common: added sourcing of ~/.config/dotfiles/local.env (if it exists) immediately after DOTFILES_DIR is confirmed. File is generated by dotfiles-private install.sh.

### BUGFIX: Fix cd alias override in admin pwsh: use Remove-Item before Set-Alias (2026-05-03, pending)
Background: Set-Alias -Force cannot change the AllScope option on PowerShell's built-in cd alias, causing an error in admin sessions where fnm does not pre-empt it with its own non-AllScope alias.
Changes: Replace Set-Alias -Force with Remove-Item Alias:\cd -Force followed by Set-Alias (no -Force). Remove-Item takes a different code path that permits AllScope alias removal.
