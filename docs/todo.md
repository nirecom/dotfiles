# TODO

## Current Work

### Workflow State Machine — Windows 再確認（マーカー形式変更）

`WORKFLOW_MARK_STEP:step:status` → `WORKFLOW_MARK_STEP_step_status`
`WORKFLOW_RESET_FROM:step` → `WORKFLOW_RESET_FROM_step`

マーカー形式の根幹変更のため、以前完了した Windows 動作確認を再実施する。

- [x] 正常系1: スキル完了後 `echo "<<WORKFLOW_MARK_STEP_research_complete>>"` → PostToolUse hook がステップを記録する
- [x] 正常系2: `echo "<<WORKFLOW_RESET_FROM_research>>"` → ask ダイアログが出る + hook がワークフローを巻き戻す
- [x] 正常系3: 未完了ステップがある状態で git commit がブロックされる
- [x] 正常系4: 全ステップ完了後に git commit が通る
- [x] 異常系1: 旧形式 `echo "<<WORKFLOW_MARK_STEP:research:complete>>"` が hook に無視される（state 変更なし）
- [x] 異常系2: 旧形式 `echo "<<WORKFLOW_RESET_FROM:research>>"` → ダイアログ不出・hook 無視

### Workflow State Machine — macOS 環境動作確認

- [ ] 正常系1: セッション開始 → CLAUDE_SESSION_ID が CLAUDE_ENV_FILE に書き込まれるか確認
- [ ] 正常系2: スキル完了後 `WORKFLOW_MARK_STEP_*` マーカーがステップを記録するか確認
- [ ] 正常系3: 未完了ステップがある状態で git commit がブロックされるか確認
- [ ] 正常系4: 全ステップ完了後に git commit が通るか確認
- [ ] 正常系5: PostToolUse hook 実発火・state file 記録を確認（`RUN_E2E=1`）
- [ ] 異常系1: 旧形式マーカー（`: `区切り）が hook に無視される
- [ ] 異常系2: `WORKFLOW_RESET_FROM_*` ask ダイアログが出る

### Workflow State Machine — WSL 環境動作確認

- [ ] 正常系1: セッション開始 → CLAUDE_SESSION_ID が CLAUDE_ENV_FILE に書き込まれるか確認
- [ ] 正常系2: スキル完了後 `WORKFLOW_MARK_STEP_*` マーカーがステップを記録するか確認
- [ ] 正常系3: 未完了ステップがある状態で git commit がブロックされるか確認
- [ ] 正常系4: 全ステップ完了後に git commit が通るか確認
- [ ] 正常系5: PostToolUse hook 実発火・state file 記録を確認（`RUN_E2E=1`）
- [ ] 異常系1: 旧形式マーカー（`: `区切り）が hook に無視される
- [ ] 異常系2: `WORKFLOW_RESET_FROM_*` ask ダイアログが出る

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
- [x] 現行の `test.md`（テストケースカテゴリ定義）に「インテグレーション / E2E 視点」の観点を追記
- [ ] `/write-tests` スキルの Procedure に「呼び出し経路ごとの異常系洗い出し」ステップを追加
- [ ] Workflow State Machine の E2E 検証（新規セッション動作確認）が完了してから着手する

# test
