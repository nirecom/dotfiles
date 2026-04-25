# TODO

## Current Work

### awesome-lists 投稿（agents repo split プロジェクトの残作業）
- [ ] [hesreallyhim/awesome-claude-code](https://github.com/hesreallyhim/awesome-claude-code) へエントリ追加 PR
- [ ] [rohitg00/awesome-claude-code-toolkit](https://github.com/rohitg00/awesome-claude-code-toolkit) へエントリ追加 PR
- [ ] [travisvn/awesome-claude-skills](https://github.com/travisvn/awesome-claude-skills) へエントリ追加 PR
- [ ] [VoltAgent/awesome-agent-skills](https://github.com/VoltAgent/awesome-agent-skills) へエントリ追加 PR

### scan-inbound 拡張候補 — 要検討
- [ ] **Read** 対象追加: git clone した悪意ファイルの injection 検出。誤検知（HTML/XML/コード）とのトレードオフを評価してから判断
- [ ] **Bash** 対象追加: git log / npm install 等の stdout injection 検出。誤検知（ビルド出力・テスト結果）が多いため慎重に評価

### セキュリティスキャンツール統合検討
- [ ] Gitleaks: git history 対応シークレットスキャン。scan-outbound.sh との役割分担を評価 (https://github.com/gitleaks/gitleaks)
- [ ] Semgrep: 構文認識型静的解析（shell, Python, JS）。review-code-security の手動パターンを自動化できるか評価 (https://github.com/semgrep/semgrep)
- [ ] detect-secrets: エントロピーベースの汎用シークレット検出。openssl rand -hex 32 系ジェネリック乱数をカバーできるか評価 (https://github.com/Yelp/detect-secrets)


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


### history.md 全エントリへの通し番号付与 — 将来タスク
- [ ] 現状: `### #N: ...` 形式は INCIDENT カテゴリのみ
- [ ] 動機: JIRA 移行・エントリ数増加時に、commit や別セッションから「前回の Task #N の続き」と参照できるようにしたい
- [ ] 検討: `doc-append` による自動採番、既存エントリの一括番号振りスクリプト
- docs-only short-circuit 導入により当面の必須条件ではない



### pwsh installer 別PC での動作確認 — Verifying
- [ ] 別PC（pwsh 7.6.0 など旧バージョン搭載）で `./install.ps1` を実行し、7.6.1 へのアップグレードが正常に行われることを確認
  - UAC ダイアログが出て承認後に "PowerShell Core updated to 7.6.1. Restart your terminal." が表示されること
  - 再実行時に "PowerShell Core is already up to date (7.6.1)." が表示されること
