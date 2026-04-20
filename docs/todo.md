# TODO

## Current Work

### Fix: ai-specs の history.md が Edit/Write deny で追記不能

`settings.json` の deny リストに `Edit(**/history.md)` / `Write(**/history.md)` があり、
`doc-append.py` 経由のみ許可する設計だが、ai-specs に `bin/doc-append.py` が存在しないため
history.md への追記が完全にブロックされる。

対処案:
- ai-specs に `bin/doc-append.py` を追加 (dotfiles の同スクリプトをコピー or シンボリックリンク)
- または ai-specs 用の allow ルールを settings.json に追加

### Phase 5: Prompt Injection Defense (PostToolUse hook) — 未着手
Prompt injection の本来の防御点（ツール結果が LLM に戻る段）を PostToolUse hook で実装。
- [ ] 設計：scan 対象 tool の選定（WebFetch / Read / Bash 等）
- [ ] 検出パターン：JudgeClaw `bridge/injection_signals.py` の `_PATTERNS` リストを baseline として移植
- [ ] Benign-context exclusion（PEM / data:image/ / コード文脈での Base64 誤検知回避）
- [ ] 実装：`claude-global/hooks/` 配下に hook 追加
- [ ] 参考：`judgeclaw`, lasso-security/claude-hooks

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


### テスト設計: インテグレーション / E2E 異常系の網羅 — 検討
背景: `docs/history.md` #21 参照。
- [x] 現行の `test.md`（テストケースカテゴリ定義）に「インテグレーション / E2E 視点」の観点を追記
- [ ] `/write-tests` スキルの Procedure に「呼び出し経路ごとの異常系洗い出し」ステップを追加
- [ ] Workflow State Machine の E2E 検証（全環境完了済み）— 着手可能

