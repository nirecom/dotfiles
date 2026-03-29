# Change and Incident History

## Change History

### Fix codes function to survive terminal close (781f512)
Background: `codes` alias uses `Start-Job` to push session sync after VS Code closes, but `Start-Job` is tied to the parent PowerShell session. Closing the terminal before VS Code was confirmed to sometimes kill the job before push ran.
Changes: Replaced `Start-Job` with `Start-Process pwsh -WindowStyle Hidden` so the push process is independent of the terminal lifecycle.

### Initial setup (a112597–7419e8d)
Background: Manage dotfiles on GitHub
Changes: Added `.bashrc`, `.vimrc`, `.editorconfig`, `.gitconfig`

### Emacs enhancement (f835f5a–d45bdd8)
Background: Modularize Emacs config
Changes: init-loader pattern, use-package, LSP, Ivy/Counsel

### Zsh migration (cdcd088–f2e7309)
Background: Switch to Zsh as primary shell
Changes: `.zshrc` + Zinit, `.profile_common` separation, git-prompt migration

### Git config relocation (ce8aa0a–f16c281)
Background: Move `.gitconfig` to XDG-compliant location
Changes: Migrated to `.config/git/config` + `ignore`, separated `config.local`

### OS detection refactor (acb488b–e6aa321)
Background: Unify OS detection logic
Changes: Created `bin/detectos.sh`, rewrote `.bash_profile` to use `case` statements

### Apple Silicon support (1ff488d–d72a3b5)
Background: Support M1/M2 Macs
Changes: Added `$ISM1`, updated Homebrew/fnm paths for Apple Silicon

### Repository merge (b436d88–7abe347)
Background: Merge install repo into dotfiles
Changes: Subtree merge into `install/linux/`, unified `install.sh` entry point. The separate [nirecom/install](https://github.com/nirecom/install) repository is no longer needed

### Windows support (8281602–e51b61b)
Background: Use dotfiles on Windows
Changes: `install/win/dotfileslink.ps1`, Developer Mode check, PowerShell 5 compatibility

### Starship introduction (f43b6d9, 25b7147)
Background: Add Starship prompt
Changes: Linux Zsh + Windows PowerShell configurations

### Claude Code management (664d7ce–2e96bbf)
Background: Centralize Claude Code config
Changes: `claude-global/` directory, install integration, security deny rules

### QNAP support (19f488d–f2d62a5)
Background: Use dotfiles on QNAP NAS
Changes: OS detection added, minimal symlinks, autorun.sh, sh→bash auto-switch

### QNAP Entware fix (7533ba7–401097e)
Background: git disappears after reboot
Changes: Entware QPKG activation, autorun.sh filename fix, flash auto-deployment

### QNAP prompt wrapping fix (107b3a6–1ae045e)
Background: Long input wraps incorrectly in QNAP bash
Changes: terminfo auto-install, `TERMINFO` env var, PS1 unification (`\001`/`\002` → `\[`/`\]`), TERM fallback

### PowerShell curl issue (60c48fb, eb9d164)
Background: curl doesn't work in PowerShell
Changes: Added `curl.exe` rule and single-quote rule to CLAUDE.md

### Claude Code commands (8418c36)
Background: Manage Claude Code commands in dotfiles
Changes: Added global commands directory symlink support

### QNAP vim plugins (c3754ce)
Background: vim errors on startup in QNAP
Changes: Added pathogen + solarized plugin installation to `dotfileslink.sh`

### Claude Code skill management (9ec0c0b, 359d929, e442685)
Background: Manage Claude Code skills (commands) in dotfiles, improve symlinks
Changes: Changed commands symlink from per-file to directory-level, added langchain/instruction update skills

### Claude Code security hardening (6e9eeb1, 18b0fd7)
Background: Review settings.json allow/deny rules. Cross-reviewed with ChatGPT: (1) adopt `git -C` as primary method (avoids compound commands), (2) `cd && git` fallback limited to status/diff/log minimum set, (3) wildcard prefix on deny rules (defense in depth), (4) curl/wget pipe deny acknowledged as glob-limited (future PreToolUse hook for strict enforcement)
Changes: Added `git -C` allow rules, `cd &&` fallback allow (minimal set), strengthened deny rule `*` prefixes, added `git -C` preference to CLAUDE.md, added local path rule to private information

### Claude Code git write permissions (e94a2b5)
Background: `git push` failed with permission denied — only read-only git commands were in allow list. Cross-reviewed with ChatGPT: (1) allow `git commit -m *` only (not broad `git commit *` which passes `--amend`/`--no-verify`), (2) allow `git push` / `git push origin *` only (not broad `git push *` which passes `--force-with-lease`/`--mirror`/`--delete`), (3) deny side strengthened with `--force-with-lease`, `--mirror`, `--delete`, `--amend`, `--no-verify`
Changes: Added git add/commit/push allow rules (minimal), added 7 deny rules for dangerous git options

### Claude Code deny rule false-positive fix (c8095dc)
Background: `git add` auto-denied without prompt — deny rule `Bash(*dd *)` matched "add" substring. Cross-reviewed with ChatGPT: (1) root cause is deny `*dd *` not missing allow rules, (2) replaced with 4 specific patterns (`dd *`, `*&& dd *`, `*; dd *`, `*
Changes: dd *`), (3) HEREDOC commit format confirmed working with existing `git commit -m *` allow rule, (4) settings changes require session restart to take effect

### Stale symlink detection (Windows) (0c83762)
Background: dotfiles moved from `~/git/dotfiles` to `~/dotfiles`; old symlinks remained and caused broken configs
Changes: `dotfileslink.ps1` now compares symlink target against `$DotfilesDir` and relinks automatically if mismatched

### AutoHotkey integration (3ca2dcf, 1c905ec)
Background: Force Japanese keyboard layout via AHK when Windows UI is English + Japanese preferred. Added `config/win/autohotkey/force-japanese-layout.ahk`, `install/win/autohotkey.ps1` (language detection + winget install + startup shortcut), `install/win/install-obsolete.ps1` (auto-cleanup of old OneDrive AHK files and startup shortcuts)
Changes: AHK script moved from OneDrive to dotfiles repo. Install script detects English UI + Japanese preferred language (strict), installs AHK v2 via winget, creates startup shortcut. Obsolete script auto-detects old AHK shortcuts by TargetPath pattern (excludes dotfiles repo paths for idempotency) and old OneDrive AHK files by glob. `install-obsolete.ps1` runs always (not only `-Full`), matching Linux `install.sh` pattern. Per-user install path fallback added (1a15c1f)

### Starship install guard (c7f02ea, 1766674)
Background: `install.ps1 -Full` tried to reinstall Starship even when already installed — `Get-Command` missed binaries not yet in PATH
Changes: Switched to `winget list --id Starship.Starship` for definitive install detection, matching AutoHotkey's `winget list` pattern

### Starship git timeout fix (4024b77)
Background: Starship showed `git.exe timed out` warning on Windows PowerShell startup — NTFS overhead makes `git status` exceed default 500ms timeout
Changes: (1) Enabled Git built-in FSMonitor (`core.fsmonitor = true`) and `core.untrackedCache = true` in Windows `config.local` for root-cause fix, (2) increased Starship `command_timeout` to 1000ms in `starship-powershell.toml` as safety net for FSMonitor cold start, (3) split `git pull` into `git fetch` (3s timeout) + `git merge --ff-only` to avoid corrupting working tree on kill

### uv install scripts (85f3d7f)
Background: Add uv (Python package manager) to dotfiles
Changes: Added `install/win/uv.ps1` (PowerShell installer) and `install/linux/uv.sh` (curl installer), integrated into `install.ps1 -Full`. uv installs to `~/.local/bin` on all platforms (Win/Mac/Linux) — already in PATH via `.profile_common` and `profile.ps1`. No shell init (unlike fnm) needed

### Claude Code PreToolUse hook investigation ()
Background: Investigated using PreToolUse hooks to force diff display before Edit (CLAUDE.md instruction sometimes ignored). Created node-based hook script outputting diff via stderr and `additionalContext`. **Result: neither method is visible to users in VS Code extension UI.** VS Code already shows a built-in diff in the Edit approval dialog, making the hook redundant. Hook removed. Findings: (1) PreToolUse hook stderr not shown in VS Code UI, (2) `additionalContext` not shown in VS Code UI, (3) `/dev/stdin` does not exist on Windows — use `fs.readSync(0, buf)` for cross-platform stdin, (4) VS Code Claude Code provides diff review via standard Edit approval dialog when Edit is not auto-allowed
Changes: No commit (reverted)

### Claude Code PermissionRequest hook (feature/permission-hook2) (41f092c, (pending))
Background: Fixed settings.json to nested format, limited PreToolUse hook to Bash only, added MSYS/WSL path detection to private info scanner. Investigated VSCode "Ask before edits" Bash behavior: (1) `Edit
Changes: Write` PreToolUse hook presence does not affect Bash Ask behavior, (2) discovered `permissions.ask` setting — adding `Bash(git commit *)` triggers Ask dialog in "Ask before edits" mode, (3) "Edit automatically" mode ignores `permissions.ask` (auto-allows all). Achieved git commit confirmation control via `permissions.ask` — PermissionRequest hook unnecessary

### Private info leak prevention ()
Background: Automated scanning for private information (RFC 1918 IP, email, MAC, absolute paths) at git commit time and Claude Code edit time. Private repo whitelist skips scanning for private repos (safe default: unregistered repos are scanned). Fixed existing leaks: `.zshrc` SDKMAN hardcoded paths → `$HOME`, `.config/git/config` email → GitHub noreply. Key files: `bin/check-private-info.sh` (scanner), `bin/update-private-repos.sh` (whitelist generator), `hooks/pre-commit` (git hook), `claude-global/hooks/check-private-info.js` (PreToolUse hook), `.private-info-allowlist`, `.private-info-blocklist`
Changes: b5f60d8, d0cea99, 98ae129, 733ec29

### Windows notification muting (45ca237)
Background: Mute toast/system notification sounds
Changes: `sounds.ps1` sets registry to empty for 3 sound events; runs always (not only `-Full`)

### fnm install script (267d793, d2dadcc)
Background: Add fnm to Windows installer
Changes: `install/win/fnm.ps1` with winget install, `profile.ps1` defensive init with try/catch for SAC

### Claude Code permissions refinement (99e2d4f, d9d691b, 99858f4, 98ae129)
Background: Expand Claude Code allow rules for daily use
Changes: Allow `chmod +x`, piped `ls`, `find` (with deny for dangerous options), commit message blocklist scan

### claude-code → claude-global rename (597bf88–f83e98c)
Background: Rename directory to avoid project-level conflict
Changes: Renamed `claude-code/` to `claude-global/`, migration logic in install scripts, handles empty dir left by git, `-Recurse` fix for non-empty directory

### uv install PATH fix (d82c026)
Background: `uv --version` fails on fresh install
Changes: Refresh `$env:Path` (Win) / `$PATH` (Linux) after uv install so version check succeeds

### Profile hardening (Windows) (a64aca9)
Background: SSH key loading and migration symlink fail on some machines
Changes: SSH key loading now globs `$HOME\.ssh\id_*` instead of hardcoded `id_ed25519`/`id_rsa`. Migration symlink (claude-code → claude-global) checks Developer Mode / admin before creating. PS5 `New-Item` wrapped in try/catch for permission errors. Added Pester tests

### Claude Code rules reorganization ()
Background: Split monolithic `claude-global/CLAUDE.md` into `rules/` directory (6 rule files) + docs lifecycle rule. Unified langchain-specific commands into generic `/update-docs`, `/start-task`, `/complete-task`. Deleted 7 old langchain-specific commands. Added `rules/` symlink to install scripts (Linux + Windows). Added `tests/test-claude-rules.sh` verification script
Changes: (pending)

### Claude Code commands → skills migration ()
Background: Migrated `commands/*.md` to `skills/*/SKILL.md` with YAML frontmatter (description, disable-model-invocation, argument-hint). start-task/complete-task are manual-only; update-docs/update-instruction allow auto-invocation. Updated dotfileslink (Win/Linux), CLAUDE.md, docs
Changes: (pending)

### master → main cleanup (07045be)
Background: Remove temporary migration code after all PCs migrated
Changes: Deleted `BEGIN temporary: main branch upstream tracking fix` blocks from `.profile_common` and `install/win/profile.ps1`

### PowerToys Keyboard Manager (b453ee8)
Background: Manage PowerToys Keyboard Manager (Emacs-style shortcuts) via dotfiles
Changes: Added `install/win/powertoys.ps1` (winget install + Keyboard Manager config deploy), `config/win/powertoys/keyboard-manager/default.json` (Emacs keybindings: Ctrl+A/B/D/E/F/N/P → navigation, Alt+A/B/F → original Ctrl shortcuts). Uses file copy (not symlink) because PowerToys may not follow symlinks. Idempotent: skips if config identical, backs up `.bak` if different

### VS 2022 C++ dev tools (1982d8e)
Background: `install.ps1 -Develop` for llama.cpp compilation
Changes: llama-server (llama.cpp) をソースからビルドするために VS 2022 + CMake が必要。Added `install/win/vs-cpp.ps1` (VS 2022 Community + NativeDesktop workload via bootstrapper, vswhere idempotency, UAC auto-elevation with decline handling). Added to `-Develop`/`-Full` block in `install.ps1`. Pester tests added

### Windows symlink auto-repair (08141cf, 9ab6b54)
Background: `install.ps1` warns "Exists (not a symlink)" for 4 files — Windows atomic save (editors, Claude Code/Node.js) silently replaces symlinks with regular files
Changes: `dotfileslink.ps1`: removed file-skip logic, unified file/directory backup-and-relink. `profile.ps1`: added startup detection of broken symlinks (~20ms) with auto-repair via `dotfileslink.ps1`. Extracted test rules from `workflow.md` into dedicated `rules/test.md` with edge case category, added test-first reminder to global `CLAUDE.md`. Fixed Pester 5 `-Skip` scoping bug (script-level detection needed for discovery-time evaluation)

### Docs enforcement via hook ((pending))
Background: Wanted docs updated before every commit — rules/ is best-effort, not enforced
Changes: Created `check-docs-updated.js` PreToolUse hook (blocks `git commit` when source changes staged without `docs/`), consolidated `rules/docs-lifecycle.md` into `/update-docs` skill (self-contained), deleted rule file. Enforcement hierarchy: hook (hard block) → skill (procedure) → rule (removed as redundant)

### Docs convention rule ((pending))
Background: Document structure rules were only in ai-specs history.md (mixed with migration history) — not auto-loaded by Claude Code
Changes: Created `rules/docs-convention.md` (auto-loaded by Claude Code), moved file role/cascade/content rules from SKILL.md. SKILL.md now references the rule file and retains only procedural steps

### Dotenv access blocking (76b61ca)
Background: Prevent Claude Code from reading .env files
Changes: PreToolUse hook (`block-dotenv.js`) blocks Bash/Read/Grep/Glob access to `.env*` files. Deny rules provide first layer; hook catches bypass patterns (e.g., `bash -c "cat .env"`, `/bin/cat .env`). Git commit messages containing `.env` excluded from false positives via sanitization. 59 test cases

### claude-code migration cleanup ((pending))
Background: All PCs migrated; remove temporary code to prevent claude-code/claude-global name confusion
Changes: Deleted `BEGIN temporary: claude-code → claude-global` blocks from `.profile_common` and `profile.ps1`. Removed `.gitignore` (only held `claude-code` entry). Removed `claude-code/` from `check-test-updated.js` EXEMPT_DIRS. Deleted migration Pester test. `install-obsolete.sh/ps1` retained as safety net

### Markdown exempt from code detection in hooks ((pending))
Background: ai-specs（docs-only repo）で `git commit` が PreToolUse フック（check-test-updated.js, check-docs-updated.js）にブロックされる。`projects/` 配下の `.md` ファイルが "source code" として分類されるため
Changes: EXEMPT_FILES に `/\.md$/i` 追加（両フック）。check-docs-updated.js の `hasDocChanges` で `.md` をドキュメント変更として認識。テスト追加: git -C variant, uppercase .MD, idempotency, stale review marker

### Edit 確認フロー統一 ((pending))
Background: VSCode で Edit 確認が1〜3回と不安定。確認の発生源は3つ: (1) CLAUDE.md/メモリの「diff を見せろ」指示によるチャット内 diff 提示、(2) VSCode Ask モードの内蔵 diff ダイアログ、(3) パーミッション確認ダイアログ。
Root cause: CLAUDE.md とメモリに同じ指示や矛盾する指示があると、LLM は確率的にしか従わないため挙動が不安定になる。「常に diff を見せろ」と「VSCode 時は不要」が共存すると、見せたり見せなかったりする。
Alternatives considered:
  (a) PreToolUse hook で diff 表示を強制 → VSCode UI に hook の stderr/additionalContext が表示されないため不可（過去に検証済み）
  (b) `permissions.ask` に `Edit` を追加して毎回パーミッション確認 → diff ダイアログと二重確認になる
  (c) プロンプト指示を削除し VSCode 内蔵 diff ダイアログに一本化 → 仕組みで保証され、確認は常に1回
  (d) `Edit(**)` を `allow` に追加してパーミッション層をスキップ → セッション初回の二重確認が出た場合の追加対策（今回は不要と判明）
Decision: (c) を採用。CLI 使用時のみチャットで diff を提示する条件付きルールを残した。
Changes: `workflow.md` File Edits ルールを CLI 条件付きに変更、メモリから diff 指示削除、feedback メモリ追加、`docs-convention.md` の history.md フォーマットをテーブル→セクション形式に変更
Follow-up (2026-03-23): (c) proved unreliable — VSCode built-in diff dialog sometimes shows diff, sometimes shows bare permission prompt with no diff. Tested Edit, Write (new file), Write (overwrite): all showed diff, so the inconsistency is not tool-type-dependent. Searched exhaustively: no mechanism in Claude Code permissions/hooks/extension settings to distinguish "diff shown" vs "diff not shown". Additionally tested PermissionRequest hook (unlike PreToolUse, does not bypass Ask dialog). Hook fires for Edit/Write and receives full tool_input (old_string/new_string for Edit, content for Write). However, stderr and additionalContext are not displayed in VSCode UI — same limitation as PreToolUse. Conclusion: no hook-based path exists to force diff display in VSCode; chat-based diff presentation via prompt instruction is the only reliable method. Reverted to "always show diff in chat" policy (double confirmation acceptable, zero confirmation not). Removed CLI-only condition from `workflow.md`, updated feedback memory

### Private repo detection: static list → dynamic gh API (ab6820b)
Background: `private-repos.txt` にローカルで全リポジトリ一覧を保持すると、会社 scan で全体リストが見えるリスクがある。また PreToolUse フックが `git -C <path> commit` 実行時に private repo 判定できないバグがあった（`filePath` が空のため常にスキャン）
Changes: `gh api repos/{owner}/{repo} --jq .private` で動的に判定する方式に変更。共通モジュール `claude-global/hooks/lib/is-private-repo.js` を新設し、3つの PreToolUse フック（check-private-info/check-docs-updated/check-test-updated）と 2つの git フック（pre-commit/commit-msg）すべてで使用。`gh` 未インストール・API エラー時は fail-open。`bin/update-private-repos.sh` と `.context-private/private-repos.txt` を削除

### commands → skills migration cleanup (ab6820b)
Background: 全 PC で commands → skills 移行が完了
Changes: `.profile_common` と `install/win/profile.ps1` から `BEGIN temporary: commands → skills` ブロックを削除。`install-obsolete.sh/ps1` の claude-code symlink 削除コードは後始末用に残置

### Claude Code branch/push delete deny rules ((pending))
Background: 別 PJ（langchain-stack）で Claude が `git -C` 経由でローカル・リモートブランチを確認なしに削除。`git push --delete` は deny にあったが、allow の `git -C * push origin *` が動的承認（"Yes, don't ask again"）経由で突破した可能性。`git branch -D` は deny/ask いずれにも未登録だった。ChatGPT と cross-review し `git push origin :branch`（refspec 形式の削除）もカバー
Changes: deny に `*git branch -D*`, `*git branch -d*`, `*git push origin :*`, `*git push *origin :*` を追加

### dotfiles clone 復旧 — git rebase 後 ()
Background: git rebase 後、各 PC の dotfiles clone が origin/main と diverge した状態を復旧
Changes: 全 PC で `git fetch origin && git reset --hard origin/main` を実施。完了確認済み

### Claude Tabs installer ((pending))
Background: Claude Tabs (Tauri v2) は Windows ネイティブの Claude Code マルチセッション管理アプリ。タブ UI + Activity Feed でエージェント状態をリアルタイム表示
Changes: `install/win/claude-tabs.ps1` 新設（GitHub API で最新リリース取得、/S サイレントインストール）。`/releases/latest` がアセット未添付の場合に直近10リリースを走査するフォールバック。`.cross-platform-skiplist` 新設（Windows 専用ツールのフック除外リスト）

### Node.js version manager: platform split (fnm → nvm on Unix) (1b74132, (pending))
Background: NemoClaw official installer unconditionally installs nvm. Conflicts with dotfiles' "fnm everywhere" rule — npm install fails when prek refuses to install hooks with core.hooksPath set. fnm has no advantage over nvm on Unix; nvm is the ecosystem standard. Windows needs fnm (nvm has no Windows support)
Changes: New rule: Windows=fnm, WSL2/macOS/Linux=nvm. Replaced `install/linux/fnm.sh` with `nvm.sh`. Removed fnm from `.profile_common` PATH and init (nvm init already existed at lines 190-206). Added fnm cleanup to `install-obsolete.sh`. Updated `coding.md` rule to platform-specific. Fixed nvm.sh execute permission (1b74132)

### Keychain SSH key auto-detect (6b39058)
Background: keychain の SSH 鍵指定がハードコードだった
Changes: `install.sh` の keychain ステップをデフォルト実行に昇格。`.profile_common` で `~/.ssh/id_*` を glob して自動検出。`install/linux/keychain.sh` を新設

### Claude Code session sync (513e3a4, (pending))
Background: Claude Code のセッション履歴（`~/.claude/projects/`）は各マシンのローカルにのみ保存され、デバイス間同期機能は公式に提供されていない。複数 Windows PC（自宅 + 会社3台）間で過去の会話を参照したい。
Alternatives considered:
  (a) Claude Code Web モード（claude.ai/code）のみ使用 → ローカルファイルシステム・MCP サーバー・NSSM サービス操作が必要な作業（NemoClaw, llama-swap 等）では Local モード必須。Local と Web でセッションが分断される。却下
  (b) perfectra1n/claude-code-sync（Rust 製外部ツール）→ 必要な機能は 50 行以下で実装可能、dotfiles に自然に統合できるため自作を選択
  (c) Remote Control（`claude remote-control`）→ 元 PC 起動中のみ、10分で切断。履歴参照には不向き
  (d) Anthropic 公式 sync 機能待ち（Issue #22648）→ 時期未定
Design decisions:
  (1) Path unification: unified under drive root with dedicated directories for LLM infrastructure (existing, immovable) and `C:\git\` (new). Eliminated `~/git/` which contains the username, ensuring identical absolute paths across all machines
  (2) 行末制御: ChatGPT と cross-review し3方式を比較。`core.autocrlf=false`（変換停止のみ、LF 保証なし）、`* -text`（同上）、`* text eol=lf`（LF を明示的に宣言、repo で伝搬）。`* text eol=lf` を `.gitattributes` で採用。理由: JSONL は全マシンで LF を保証したい + clone/pull で自動伝搬
  (3) ファイル書き込み: `Set-Content`（PowerShell cmdlet）は Windows 上で CRLF を出力し git add 時に warning。`[System.IO.File]::WriteAllText()` + CRLF→LF 置換で LF 書き込みに変更。ChatGPT と cross-review し互換性確認（.NET 標準 API、PS5/PS7 両対応、UTF-8 BOM なし）
  (4) 同期対象: `projects/` のみ。`settings.json`, `rules/`, `skills/` 等は dotfiles で管理済み。`statsig/`, `ide/` はマシン固有
  (5) コンフリクト戦略: JSONL は追記型。push→pull ルーティンで回避。発生時は `git pull --rebase`
Changes: `install/win/session-sync-init.ps1`（初期化）、`bin/session-sync.ps1`（push/pull/status）、`install.ps1` へ統合、`nirecom/claude-sessions` private repo 作成。Pester テスト 12 ケース

### Cross-platform check hook (5c7714e)
Background: `install/win/` のみ変更して `install/linux/` 側を忘れるケースを防止
Changes: `check-cross-platform.js` PreToolUse hook 新設。`git commit` 時にプラットフォーム固有ファイルの counterpart 変更を検知。`.cross-platform-skiplist` で永続除外、`.git/.cross-platform-reviewed` で一時除外。220 テストケース

### ~/dotfiles → C:\git\dotfiles path unification ((pending))
Background: To share Claude Code session history (`~/.claude/projects/`) across Windows PCs, all machines need a uniform dotfiles path. `~/dotfiles` (= `C:\Users\<user>\dotfiles`) includes the username, causing project directory names to differ per PC. `C:\git\dotfiles` makes the project key `c--git-dotfiles` on all PCs.
Alternatives considered:
  (a) Auto-migrate via robocopy in install.ps1 — `[IO.Directory]::Move` fails because AutoHotkey holds file locks. robocopy can read-copy locked files, but source cleanup is best-effort. Worse, profile.ps1's git-fetch pulls the new code immediately, breaking all path references before install.ps1 (manual execution) can run
  (b) Auto-migrate in profile.ps1 at startup — solves the timing issue, but the temporary migration code must be managed (removed after all PCs migrate), adding maintenance cost
  (c) No migration; dynamically rewrite JSONL session history paths on git pull — JSONL files contain absolute paths in multiple notations (Windows backslash, MSYS forward-slash, tilde) scattered across file_path fields, conversation text, and tool results. Reliable rewriting across all notations is fragile and risks data corruption. Rejected
  (d) No migration code at all; manually `git clone` to `C:\git\dotfiles` on each PC — simplest approach. Path references updated in code, install.ps1 re-run handles the rest
Decision: (d). No automated migration. Each PC is migrated by manual clone + install.ps1.
Cross-platform issue: settings.json hook paths (`node ~/dotfiles/...`) are shared with macOS/Linux. Hardcoding `C:\git\dotfiles` would break macOS. Solved with `$DOTFILES_DIR` environment variable — Windows sets it as a persistent user env var in install.ps1, macOS/Linux exports it in `.profile_common`. Hook commands use `node "$DOTFILES_DIR/..."` which the shell expands.
Changes:
  - `install.ps1`: removed migration block, added `DOTFILES_DIR` persistent user env var registration
  - `profile.ps1`: `$DotfilesDir = "C:\git\dotfiles"` + session `$env:DOTFILES_DIR`
  - `.profile_common`: added `export DOTFILES_DIR="$HOME/dotfiles"`, replaced all `~/dotfiles` references with `$DOTFILES_DIR`
  - `settings.json`: changed 5 hook paths to `$DOTFILES_DIR/...`
  - `autohotkey.ps1`: added Step 3a to detect AHK running from old path and restart from new
  - `install-obsolete.ps1`: added cleanup for `~/dotfiles`, `~/dotfiles-private`, `~/git`
  - `migrate-repos.ps1` + tests: deleted (no longer needed)
  - `main-install-obsolete-migration.Tests.ps1`: new (4 tests)

### Session sync git root relocation (dbd003a)
Background: `~/.claude/` was used as git root for session sync, but only `projects/` was tracked. This required a complex `.gitignore` (exclude-all + re-include pattern) and caused a misleading warning in `dotfileslink.ps1` about symlink conflict with `.git`. Moving git root to `~/.claude/projects/` eliminates both issues.
Changes:
  - `session-sync-init.ps1`: git init target changed from `$ClaudeDir` to `$ClaudeDir/projects`. Added migration logic to remove old `.git`/`.gitignore`/`.gitattributes` from `~/.claude/`. Removed `.gitignore` (no longer needed). `.gitattributes` created in `projects/`
  - `bin/session-sync.ps1`: all `git -C` paths changed from `$ClaudeDir` to `$ProjectsDir` (= `$ClaudeDir/projects`)
  - `dotfileslink.ps1`: removed "dotclaude" warning about `.git` in `~/.claude/`
  - Remote repo `nirecom/claude-sessions` recreated (old layout had `projects/` prefix in tree; new layout stores files at root)
  - Tests: 12 tests updated for new layout + migration test added

### Notification hook for permission_prompt (f447e11, 7ef7776)
Background: Claude Code permission_prompt dialogs were easy to miss, leaving sessions idle
Changes: Added `check-notification.js` PreToolUse hook that sends OS toast notification on `permission_prompt` events. Moved WebSearch/WebFetch from auto-allow to `permissions.ask` (now requires explicit confirmation)

### AutoHotkey per-user path fallback (1a15c1f, 29bfd8a)
Background: AutoHotkey v2 per-user installs (`%LOCALAPPDATA%\Programs\`) were not found because only `%ProgramFiles%` was searched
Changes: Added per-user install path fallback to `autohotkey.ps1`. Also fixed shortcut argument update bug and path matching bug

### Session sync: terminal startup fetch + codes function (d3c3a38, 7d7a98e, 4f8e4c3, 20ebb49)
Background: session-sync push/pull required manual execution. Wanted automatic fetch on terminal startup and automatic push when closing VS Code
Changes:
  - `.profile_common`: added `git fetch + merge --ff-only` for `~/.claude/projects/` on terminal startup (3s timeout). Added `codes` function (opens VS Code, runs `session-sync.sh push` on exit)
  - `install/win/profile.ps1`: equivalent fetch logic added
  - `bin/session-sync.sh`: new Linux/macOS session sync script (push/pull/status subcommands)

### Session sync: cross-platform support (d50203c, (pending))
Background: Session sync was Windows-only but the same session sharing is needed on macOS/Linux
Changes:
  - `install/linux/session-sync-init.sh`: new (equivalent to `session-sync-init.ps1` — git init, migration, remote setup, initial commit+push)
  - `install.sh`: added session-sync-init call after Claude Code install (`type claude` guard)
  - `install-obsolete.sh`: added Homebrew fnm cleanup (`brew list fnm && brew uninstall fnm`; not originally installed via dotfiles but cleaned up as a precaution)

### Session sync: init/push reliability fix (efbd047)
Background: `session-sync-init.sh` の初回コミットフローが「リモートに既に履歴がある」ケースを想定しておらず、2台目以降のマシンで init → push が失敗する。また push 時にリモートが先行している場合も失敗する
Design decisions:
  (1) `reset --hard` vs `reset`（mixed）: `reset --hard` はリモートと同名のローカル JSONL を上書きする危険がある（同一パスの2台で発生）。`reset`（mixed）なら HEAD のみ移動しローカルファイルを保護。ファイル展開は通常の pull で行う
  (2) `codes()` のバックグラウンド push 出力: プロンプト後に出力が割り込み、Enter が必要になる問題。SIGWINCH によるプロンプト再描画を試みたが効果なし。出力を `/dev/null` に抑制
Changes:
  - `session-sync-init.sh`/`.ps1`: 初回コミット前に `fetch origin main` + `reset origin/main` でリモート履歴を取り込む。差分がない場合の `commit` 失敗を `|| true` で許容
  - `bin/session-sync.sh`/`.ps1`: push 前に `pull --rebase origin main` を追加
  - `.profile_common`: `codes()` の push 出力を `>/dev/null 2>&1` で抑制

### Session sync: separate init from sync, add reset action (fc71b9e)
Background: Init script's fetch/reset/commit/push block was dangerous — it could overwrite remote data when a new PC joined (#19). Also, auto-running sync during install made it hard to control which PC should sync first
Changes:
  - `session-sync-init.ps1`/`.sh`: removed all fetch/reset/commit/push logic. Init now only sets up plumbing (git init, remote, .gitattributes, hooks)
  - `session-sync.ps1`/`.sh`: added `reset` action (`git fetch + reset --hard origin/main`) for manual initial sync or recovery. After reset, normal bidirectional push/pull works
  - New PC flow: `install.ps1` → init (safe), then manual `session-sync reset` to catch up to remote

### Session sync: history.jsonl sync and mtime restore (d214411, 77d00e4)
Background: Sessions synced from other machines were invisible or misordered on the receiving PC. Two root causes:
  (1) Claude Code uses `~/.claude/history.jsonl` (outside the sync git root `~/.claude/projects/`) for session listing. Without syncing this file, the receiving PC had no record of remote sessions
  (2) Claude Code uses file mtime for session ordering. `git reset --hard` sets all file mtimes to the current time, making all synced sessions appear as "just now"
  Investigation of OSS tools (x-cmd/x-cmd ★4k, mad0ps/claude-code-server-syncer) confirmed that history.jsonl rewriting and/or sessions-index.json manipulation is standard practice. x-cmd rewrites history.jsonl `.project` field on import; mad0ps deletes sessions-index.json and lets Claude Code regenerate it. Neither preserves file mtime.
  Additionally, session directory names encode the working directory path (`C:\git\dotfiles` → `c--git-dotfiles`). When the same project was opened from different paths on different machines (or after a path migration), sessions were stored in separate directories and invisible to each other. Resolved by copying the old path directory to the new path name and rewriting history.jsonl project fields.
Changes:
  - Push: copies `~/.claude/history.jsonl` into sync area as `.history.jsonl`
  - Pull/Reset: merges remote `.history.jsonl` with local history (line-level dedup, remote-first ordering)
  - Reset: restores mtime on each `.jsonl` from its last (or first, as fallback) `timestamp` field

---

## Incident History

### #1: `dotfileslink.sh` aborts with `set -e` (b3ea03f)
Cause: Some commands return non-zero exit
Fix: Fixed `set -e` compatibility

### #2: Symlink creation fails on PowerShell 5 (06f6640, be85569)
Cause: PS5 has a different Developer Mode check API
Fix: Switched to direct registry check

### #3: Claude Code installed on QNAP (89e9e98)
Cause: `install.sh` had no QNAP branch
Fix: Added early exit for QNAP

### #4: `arch: command not found` on QNAP (ed56971)
Cause: `arch` is macOS-only; not in BusyBox
Fix: Replaced with `uname -m`

### #5: No colored prompt on QNAP SSH (7e780cc)
Cause: Default shell `/bin/sh` doesn't read `.bash_profile`
Fix: `.profile_qnap` runs `exec bash -l`

### #6: `.profile_qnap` exec fails (d38ee0a)
Cause: Bash path hardcoded (`/opt/bin/bash`)
Fix: Changed to `command -v bash` for dynamic resolution

### #7: `git: command not found` on QNAP (584e6dc)
Cause: Entware `/opt/bin` not in PATH
Fix: Added Entware PATH to `.profile_common`

### #8: Cursor position misaligned on QNAP (f2d62a5)
Cause: Entware bash doesn't handle `\[...\]` correctly
Fix: Used `\001`/`\002` (interim) → removed in #10 after root cause fix

### #9: git disappears after QNAP reboot (7533ba7, 401097e)
Cause: Entware QPKG disabled (`Enable != TRUE`), startup script name wrong
Fix: `setcfg Enable TRUE` + fixed to `Entware.sh start`

### #10: Long input overwrites prompt on QNAP bash (107b3a6–1ae045e)
Cause: terminfo not installed + `TERMINFO` not set → readline can't detect auto-margin
Fix: Auto-install terminfo, set `TERMINFO=/opt/share/terminfo`, unified PS1 to standard `\[`/`\]`

### #11: `E117: Unknown function: pathogen#infect` on QNAP vim (c3754ce)
Cause: `.vimrc` symlinked but pathogen/plugins not installed
Fix: Added vim plugin setup to `dotfileslink.sh`

### #12: No colored prompt on Windows PowerShell (one PC only) (0c83762)
Cause: (1) Starship not installed — profile silently skips init. (2) Starship config symlinks pointed to old `~/git/dotfiles` location after dotfiles were moved
Fix: Installed Starship via `install/win/starship.ps1`; re-ran `dotfileslink.ps1` as admin to fix stale symlinks

### #13: `git add CLAUDE.md` auto-denied by Claude Code (c8095dc)
Cause: Deny rule `Bash(*dd *)` glob-matched the "dd" in "add", overriding the allow rule `Bash(git add *)`. Deny rules take precedence regardless of order
Fix: Replaced `*dd *` with 4 patterns: `dd *`, `*&& dd *`, `*; dd *`, `*| dd *`

### #14: `.config/git/config` always dirty after shell startup (dd787a4)
Cause: `profile.ps1` ran `git config --global core.sshCommand` on every PowerShell startup, appending `[core] sshCommand` directly to tracked `.config/git/config`. The setting was already provided via `config.local` (generated by `dotfileslink.ps1`) and included via `[include] path = config.local`
Fix: Removed redundant sshCommand block from `profile.ps1`; setting remains via `config.local` include

### #15: `uv --version` fails after fresh install (d82c026)
Cause: uv installer adds `~/.local/bin` to system PATH but current shell session is not refreshed
Fix: Add `~/.local/bin` to session PATH before calling `uv --version`

### #16: `New-Item -ItemType SymbolicLink` error on PS5 startup (a64aca9)
Cause: `profile.ps1` migration block called `New-Item` without permission check. PS5 requires admin for symlinks even with Developer Mode enabled (unlike pwsh which supports `SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE`)
Fix: Added Developer Mode / admin pre-check (a64aca9), then wrapped `New-Item` in try/catch with `-ErrorAction Stop` for defense in depth

### #17: テスト実行がハングして返ってこない (9b2e88f)
Cause: Claude Code が自動生成した Pester テストにバグ（WSL の UTF-16 出力の encoding mismatch、Pester スコープ問題）があり、バックグラウンド実行したテストが無限待ちに
Fix: `test.md` にタイムアウト必須ルール追加（`timeout 120` ラッパー）

### #18: install.sh が Rize / Claude Usage Widget ステップに到達しない
Cause: `source-highlight.sh` の `exec $SHELL -l` が `install-base.sh` のサブプロセスを置き換え、`install.sh` に制御が戻らなかった。加えて `rize.sh` / `claude-usage-widget.sh` に実行権限がなく、DMG マウント時のボリュームパスパースが空白入りボリューム名で破損、`.app` ファイル名がハードコードされ実際の名前と不一致
Fix: `exec $SHELL -l` を `source-highlight.sh` から削除し `install.sh` 末尾へ移動。実行権限を付与。DMG 内の `.app` を `find` で動的検出。`install.sh` の macOS 特別扱い（`$OSDIST = "macos"` で常に base 実行）を削除し `install.ps1` と直交性を統一（`--base`/`--full` フラグ必須）

### #19: Session sync init deletes other machines' sessions (a8b8e5b)
Cause: `session-sync-init.ps1` / `session-sync-init.sh` used `git reset origin/main` (mixed reset) during initialization. Mixed reset moves HEAD and index to origin/main but leaves the working tree unchanged. The subsequent `git add .` overwrote the index from the working tree (which only contained local files), staging deletions for all remote-only files (other machines' sessions). When a second PC ran init, the primary and other machines' session files were deleted. Additionally, the Windows script had a secondary bug: `$ErrorActionPreference = "Stop"` combined with `try/catch` caused git stderr output to throw a terminating exception, silently skipping the fetch/reset entirely
Fix: Changed `git reset` to `git reset --hard` so remote files are checked out into the working tree. Fixed PS scripts to temporarily set `$ErrorActionPreference = "Continue"` around git commands that may produce stderr or non-zero exit codes. Also fixed the same stderr handling bug in `session-sync.ps1` `pull --rebase`. Data was restored in `a8b8e5b`; diff between `199a29d` (pre-deletion) and `acc5467` (current) is empty — no data loss

