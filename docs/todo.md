# TODO

## Current Work

### VS Code installer — Verifying
- [ ] Verify on another PC: `install.ps1 -Develop` installs VS Code and extensions from `config/vscode-extensions.txt`
- [ ] Verify on Linux/macOS: `install.sh --develop` installs VS Code and extensions

### effortLevel auto-unstage — Verifying
- [ ] Verify: make an effortLevel-only change, stage, and commit other files — settings.json should be auto-unstaged

### SSOT 参照ルールの設計 — 検討中
ポート・URL・ホスト名を推測せず SSOT を確認させる仕組みの設計:
- [ ] claude-global/rules/ に汎用行動ルール追加（「SSOT を確認してから提示」— ファイル名は含めない）
- [ ] ai-specs/CLAUDE.md の Infrastructure SSOT セクションに行動指示を追記
- [ ] docs-convention.md の Standard Files が nirecom PJ 前提である点の整理（他 doc 体系との分離）

### Cross-platform skiplist — 要判断
以下のスクリプトの skiplist 分類を決定する（Windows counterpart が必要か）:
- [ ] go, terraformer — Windows でも使うツール
- [ ] flutter, react — cross-platform 開発ツール
- [ ] vim, tmux, source-highlight — エディタ/ターミナル系
- [ ] install-base, install-develop — メタスクリプト（構成が異なる）
- [ ] config/win ↔ config/mac ペアの hook 対応要否

### dotfiles force push — Verifying
Auto-detection implemented in `.profile_common`. On shell startup:
- Divergence detected → interactive prompt (y/N, 10s timeout)
- `~/.dotfiles-no-auto-reset` marker → warning message only (for master PC)
- Non-interactive shell → silently skipped
- [ ] Verify on a follower PC: open new shell after force push, confirm prompt appears
- [ ] Create `~/.dotfiles-no-auto-reset` on master PC (`New-Item -ItemType File -Path "$HOME\.dotfiles-no-auto-reset"` on pwsh)

