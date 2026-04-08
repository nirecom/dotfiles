# TODO

## Current Work

### Security Enhancement — Phase 1 Verifying
Security checklist and test coverage improvements. Full plan in `docs/plan.md`.
Design decision: minimize rules/ context consumption by extracting details into skills.
Skill naming follows existing `verb-noun` (kebab-case) convention.
- [x] Phase 1: Architecture Security Checklist (`/review-security` skill)
  - [ ] Verify: invoke `/review-security` in a new session and confirm TodoWrite-based checklist execution
- [ ] Phase 2: Security Test Cases (`test.md` edit)
- [ ] Phase 3: Security Patterns Reference (`/scan-security` skill)
- [ ] Phase 4: Prompt Injection Defense

### dotfiles force push — Verifying
Auto-detection implemented in `.profile_common` and `install/win/profile.ps1`. On shell startup:
- Divergence detected → interactive prompt (y/N, 10s timeout)
- `~/.dotfiles-no-auto-reset` marker → warning message only (for master PC)
- Non-interactive shell → silently skipped
- [ ] Verify on a follower PC: open new shell after force push, confirm prompt appears
- [ ] Create `~/.dotfiles-no-auto-reset` on master PC (`New-Item -ItemType File -Path "$HOME\.dotfiles-no-auto-reset"` on pwsh)

### VS Code installer — Verifying
- [ ] Verify on another PC: `install.ps1 -Develop` installs VS Code and extensions from `config/vscode-extensions.txt`
- [ ] Verify on Linux/macOS: `install.sh --develop` installs VS Code and extensions

### winget MSI mutex wait — Verifying
`install.ps1` 冒頭で `Wait-MsiMutex` を実行。別の MSI が走っていれば最大120秒待つ。
- [ ] Verify: 別の MSI (Windows Update 等) 実行中に `install.ps1 -Base` を実行し、待機メッセージが出ることを確認

### SSOT 参照ルールの設計 — 検討中
ポート・URL・ホスト名を推測せず SSOT を確認させる仕組みの設計:
- [ ] claude-global/rules/ に汎用行動ルール追加（「SSOT を確認してから提示」— ファイル名は含めない）
- [ ] ai-specs/CLAUDE.md の Infrastructure SSOT セクションに行動指示を追記
- [ ] docs-convention.md の Standard Files が nirecom PJ 前提である点の整理（他 doc 体系との分離）

### commit 確認を 2回→1回に削減 — Verifying
commit-push skill で commit message をチャット表示→承認後に commit するフローに変更。
- `cd && git commit` のみ `ask` に残す（rules/git.md で禁止されたパターン）
- [ ] Verify: 次回 `/commit-push` で確認ダイアログが1回だけになることを確認

### Cross-platform skiplist — 要判断
以下のスクリプトの skiplist 分類を決定する（Windows counterpart が必要か）:
- [ ] go, terraformer — Windows でも使うツール
- [ ] flutter, react — cross-platform 開発ツール
- [ ] vim, tmux, source-highlight — エディタ/ターミナル系
- [ ] install-base, install-develop — メタスクリプト（構成が異なる）
- [ ] config/win ↔ config/mac ペアの hook 対応要否

