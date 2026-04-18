# TODO

## Current Work

### Bug: workflow sentinel が `&&` チェーンで無視される — 要修正

**再現条件**: `echo "<<WORKFLOW_MARK_STEP_code_complete>>" && echo "<<WORKFLOW_MARK_STEP_verify_complete>>"` のように
sentinel echo を `&&` で繋いで 1 Bash 呼び出しにすると、`workflow-mark.js` の strict anchored regex
（`/^echo\s+"<<...>>"$/`）にマッチせず state が更新されない。結果として `workflow-gate.js` が
次の `git commit` で当該 step を incomplete と判定し block する。

**影響範囲**: 以下の全 sentinel が同じ regex 制約を持つ:
- `WORKFLOW_MARK_STEP_*`
- `WORKFLOW_RESEARCH_NOT_NEEDED` / `WORKFLOW_PLAN_NOT_NEEDED` / `WORKFLOW_WRITE_TESTS_NOT_NEEDED`
- `WORKFLOW_USER_VERIFIED`
- `WORKFLOW_RESET_FROM_*`

**修正候補**:
1. `workflow-mark.js` の入力をコマンド全体ではなく stdout で受け取るよう変更し、
   出力文字列で sentinel を検索するパターンに切り替える（複数 sentinel を一括処理可能に）
2. `workflow-mark.js` に `&&` 分割ロジックを追加し、各部分コマンドを個別に評価する
3. sentinel の echo は必ず単独 Bash 呼び出しとする旨を `workflow-mark.js` の冒頭コメントに明記し、
   feedback memory で運用回避（現状の暫定対処）

**現在の回避策**: sentinel は必ず別々の Bash 呼び出しで送る（`&&` 禁止）。

- [ ] 修正方針を決定（候補 1 / 2 / 3）
- [ ] 実装（修正候補 1 or 2 の場合）
- [ ] 関連 E2E テスト追加

### Security Enhancement — Phase 4 Verifying
Security checklist and test coverage improvements. Full plan in `docs/plan.md`.
Design decision: minimize rules/ context consumption by extracting details into skills.
Skill naming follows existing `verb-noun` (kebab-case) convention.
- [x] Phase 1: Architecture Security Checklist (renamed to `/review-plan-security`)
  - [x] Verify: invoke `/review-plan-security` in a new session and confirm TodoWrite-based checklist execution
- [x] Phase 2: Security Test Cases (`test.md` edit)
  - [x] Verify: confirm Security cases section in test.md with correct OWASP/CWE citations, and test-rules/ subdirectory loads correctly
- [x] Phase 3: Security Patterns (`/review-code-security` skill + `scan-outbound.sh` hard secrets)
  - [x] Verify: invoke `/review-code-security` and confirm pattern tables displayed; confirm hard-secret detection in scan-outbound.sh
- [x] Phase 4: Trojan Source 対策 + Prompt Injection 統合 (`scan-outbound.sh`, SKILL.md 両 Axis 拡張)
  - [ ] Verify: `main-hidden-char-scan.sh` 全 PASS 確認、スモーク検証済み（ユーザー確認待ち）

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

