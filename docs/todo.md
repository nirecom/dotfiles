# TODO

## Current Work

### Claude Tabs installer — Verifying
- [ ] `install.ps1 -Base` で Claude Tabs が正常にインストールされることを確認
- [ ] 再実行で "already installed" が表示されることを確認（冪等性）

### Cross-platform skiplist — 要判断
以下のスクリプトの skiplist 分類を決定する（Windows counterpart が必要か）:
- [ ] dotfiles (確認済み: counterpart あり → skiplist から除外すべき)
- [ ] awscli, go, terraformer — Windows でも使うツール
- [ ] flutter, react — cross-platform 開発ツール
- [ ] vim, tmux, source-highlight — エディタ/ターミナル系
- [ ] install-base, install-develop — メタスクリプト（構成が異なる）
- [ ] config/win ↔ config/mac ペアの hook 対応要否
