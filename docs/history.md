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
