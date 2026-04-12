# TODO

## Current Work

### Workflow State Machine — Verifying
mark-step.js を再設計:
- `resolveSessionId()` を `lib/workflow-state.js` に追加（`CLAUDE_ENV_FILE` から `CLAUDE_SESSION_ID` を取得）
- `mark-step.js` の session ID 引数を廃止
- `workflow-gate.js` のメッセージ更新（session ID 引数なし）
- テスト 41 件 PASS

動作確認（残タスク）:
- [ ] mark-step.js がスキルから呼ばれる際に `CLAUDE_ENV_FILE` が `process.env` に存在するか確認
- [ ] 異常系2: ステートファイル破損 → `--reset-from research` で自動リカバリ
- [ ] 異常系3: 途中からやり直し → `--reset-from <step>` で部分リセット

### Security Enhancement — Phase 1 Verifying
Security checklist and test coverage improvements. Full plan in `docs/plan.md`.
Design decision: minimize rules/ context consumption by extracting details into skills.
Skill naming follows existing `verb-noun` (kebab-case) convention.
- [x] Phase 1: Architecture Security Checklist (`/review-security` skill)
  - [ ] Verify: invoke `/review-security` in a new session and confirm TodoWrite-based checklist execution
- [ ] Phase 2: Security Test Cases (`test.md` edit)
- [ ] Phase 3: Security Patterns Reference (`/scan-security` skill)
- [ ] Phase 4: Prompt Injection Defense

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

### planner の mid-plan research escalation — 検討
planner サブエージェントが plan 作成中に調査不足 (未知の外部 API、欠損しているドメイン知識等) に気づいた場合、推測せず orchestrator にエスカレートする仕組み。
- [ ] `claude-global/agents/planner.md` に `RESEARCH_NEEDED: <question>` エスカレーションルール追加
- [ ] `claude-global/skills/make-plan/SKILL.md` の Procedure に受信時のハンドリング追加 (ユーザーに `/deep-research` 実行を促す)

### テスト設計: インテグレーション / E2E 異常系の網羅 — 検討
背景: `docs/history.md` #21 参照。
- [ ] 現行の `test.md`（テストケースカテゴリ定義）に「インテグレーション / E2E 視点」の観点を追記
- [ ] `/write-tests` スキルの Procedure に「呼び出し経路ごとの異常系洗い出し」ステップを追加
- [ ] Workflow State Machine の E2E 検証（新規セッション動作確認）が完了してから着手する

# test
