# TODO

## Current Work

### ~/dotfiles → C:\git\dotfiles path unification — Verifying
- [ ] Run `git clone https://github.com/nirecom/dotfiles.git C:\git\dotfiles` on this PC
- [ ] Run `C:\git\dotfiles\install.ps1` and confirm symlinks, AHK restart, DOTFILES_DIR env var
- [ ] Open new terminal and confirm profile.ps1 uses C:\git\dotfiles
- [ ] Open new Claude Code session and confirm hooks work via $DOTFILES_DIR

### Claude Tabs installer — Verifying
- [ ] `install.ps1 -Base` で Claude Tabs が正常にインストールされることを確認
- [ ] 再実行で "already installed" が表示されることを確認（冪等性）

### SSOT 参照ルールの設計 — 検討中
ポート・URL・ホスト名を推測せず SSOT を確認させる仕組みの設計:
- [ ] claude-global/rules/ に汎用行動ルール追加（「SSOT を確認してから提示」— ファイル名は含めない）
- [ ] ai-specs/CLAUDE.md の Infrastructure SSOT セクションに行動指示を追記
- [ ] docs-convention.md の Standard Files が nirecom PJ 前提である点の整理（他 doc 体系との分離）

### Cross-platform skiplist — 要判断
以下のスクリプトの skiplist 分類を決定する（Windows counterpart が必要か）:
- [ ] dotfiles (確認済み: counterpart あり → skiplist から除外すべき)
- [ ] awscli, go, terraformer — Windows でも使うツール
- [ ] flutter, react — cross-platform 開発ツール
- [ ] vim, tmux, source-highlight — エディタ/ターミナル系
- [ ] install-base, install-develop — メタスクリプト（構成が異なる）
- [ ] config/win ↔ config/mac ペアの hook 対応要否

### Session sync cross-platform — Verifying
- [x] `install.sh` calls `session-sync-init.sh` after Claude Code install (macOS tested)
- [x] `install-obsolete.sh` removes Homebrew fnm (macOS tested)
- [x] Init with existing remote: `rm -rf ~/.claude/projects/.git && session-sync-init.sh` fetches remote history (macOS tested)
- [x] Idempotent re-init: `install.sh --base` 2回目実行で問題なし (macOS tested)
- [ ] Verify on WSL2: `install.sh` runs session-sync-init without error
- [ ] Verify on Windows: `install.ps1` migrates old git root and re-initializes
- [ ] Run `session-sync.ps1 push` on Windows and confirm it works from `~/.claude/projects/`
