# TODO

## Current Work


### PostToolUse Marker Interception — 実装

`mark-step.js` が Bash サブプロセスから `CLAUDE_ENV_FILE` にアクセスできない問題 (Anthropic bug #27987) の根本対処。
スキルの Completion section が `echo "<<WORKFLOW_MARK_STEP:step:status>>"` を出力し、PostToolUse hook がそれを検出して mark-step.js を呼ぶ。

- [x] `/make-plan` で設計確定 (PostToolUse hook 実装方針)
- [x] テスト先行: PostToolUse hook の marker 検出・ステップ更新テスト
- [x] `claude-global/hooks/` に PostToolUse hook 追加 (matcher: Bash, marker: `WORKFLOW_MARK_STEP`)
- [x] 各スキルの Completion section を `echo "<<WORKFLOW_MARK_STEP:step:complete>>"` に変更
- [x] 動作確認 Windows 正常系: E1 E2E テスト（`RUN_E2E=1`）で PostToolUse hook 実発火・state file 記録を確認
- [ ] 動作確認 macOS/Linux 正常系: 同上

### Workflow Step Gate (mid-workflow) — 検討中

PreToolUse hook で前提ステップ未完了なら Write/Edit をブロックし Claude を前のステップに戻す仕組み。
research 結果: `ai-specs/projects/engineering/research-results/claude-code-workflow-step-enforcement.md` 参照。
PostToolUse marker interception 安定後に着手する。
既知バグ (#37210, #18312) への対処が必要。

### Workflow State Machine — macOS/Linux 動作確認

macOS または Linux 環境（mbp-m4pro-nire 等）で同等の確認が必要。

- [ ] 正常系1: セッション開始 → `~/.claude/session-env/<session-id>/sessionstart-hook-0.sh` に `CLAUDE_SESSION_ID=<id>` が書き込まれるか確認
- [ ] 正常系2: スキル完了後 mark-step.js が実際にステップを記録するか確認
- [ ] 正常系3: 未完了ステップがある状態で git commit がブロックされるか確認
- [ ] 正常系4: 全ステップ完了後に git commit が通るか確認
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
- [x] 現行の `test.md`（テストケースカテゴリ定義）に「インテグレーション / E2E 視点」の観点を追記
- [ ] `/write-tests` スキルの Procedure に「呼び出し経路ごとの異常系洗い出し」ステップを追加
- [ ] Workflow State Machine の E2E 検証（新規セッション動作確認）が完了してから着手する

# test
