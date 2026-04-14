# TODO

## Current Work

### Workflow State Machine — 3環境 E2E 動作確認（session-scoped state 移行後）

state 保存先が `{repo}/.git/workflow/` → `~/.claude/projects/workflow/` に変わったため、
旧実装で完了した E2E 確認を全環境で再実施する。
確認ポイント: SessionStart が新パスに state ファイルを作成するか、PostToolUse hook が新パスに書き込むか、
workflow-gate が新パスの state を読んで正しくブロック／通過するか。

### Windows ✅ 確認完了（2026-04-15）

- [x] 正常系1: セッション開始 → `~/.claude/projects/workflow/<session-id>.json` が作成される
- [x] 正常系2: スキル完了後 `echo "<<WORKFLOW_MARK_STEP_research_complete>>"` → PostToolUse hook がステップを記録する
- [x] 正常系3: 未完了ステップがある状態で git commit がブロックされる
- [x] 正常系4: 全ステップ完了後に git commit が通る
- [x] 異常系1: `git -C <別リポジトリ> commit` でも同一 session state を参照してブロック／通過する（#24 再現なし確認）
- [x] 異常系2: `WORKFLOW_RESET_FROM_*` → ask ダイアログが出る + hook が巻き戻す

### macOS

- [ ] 正常系1: セッション開始 → `~/.claude/projects/workflow/<session-id>.json` が作成される
- [ ] 正常系2: スキル完了後 `WORKFLOW_MARK_STEP_*` マーカーがステップを記録する
- [ ] 正常系3: 未完了ステップがある状態で git commit がブロックされる
- [ ] 正常系4: 全ステップ完了後に git commit が通る
- [ ] 正常系5: PostToolUse hook 実発火・state file 記録を確認（`RUN_E2E=1`）
- [ ] 異常系1: `WORKFLOW_RESET_FROM_*` ask ダイアログが出る

### WSL

- [ ] 正常系1: セッション開始 → `~/.claude/projects/workflow/<session-id>.json` が作成される
- [ ] 正常系2: スキル完了後 `WORKFLOW_MARK_STEP_*` マーカーがステップを記録する
- [ ] 正常系3: 未完了ステップがある状態で git commit がブロックされる
- [ ] 正常系4: 全ステップ完了後に git commit が通る
- [ ] 正常系5: PostToolUse hook 実発火・state file 記録を確認（`RUN_E2E=1`）
- [ ] 異常系1: `WORKFLOW_RESET_FROM_*` ask ダイアログが出る

### Security Enhancement — Phase 1 Verifying
Security checklist and test coverage improvements. Full plan in `docs/plan.md`.
Design decision: minimize rules/ context consumption by extracting details into skills.
Skill naming follows existing `verb-noun` (kebab-case) convention.
- [x] Phase 1: Architecture Security Checklist (`/review-security` skill)
  - [ ] Verify: invoke `/review-security` in a new session and confirm TodoWrite-based checklist execution
- [ ] Phase 2: Security Test Cases (`test.md` edit)
- [ ] Phase 3: Security Patterns Reference (`/scan-security` skill)
- [ ] Phase 4: Prompt Injection Defense

### Workflow State Machine — Session-scoped state migration Verifying

セッション単位の state 管理に移行（`{repo}/.git/workflow/` → `~/.claude/projects/workflow/`）。
インシデント #24（マルチリポジトリ state 乖離）の根本修正。

- [x] workflow-state.js: repoDir 引数削除、`~/.claude/projects/workflow/` 固定 + `CLAUDE_WORKFLOW_DIR` テスト用 override
- [x] workflow-gate.js / workflow-mark.js / mark-step.js: repoDir 参照削除
- [x] session-start.js: migration block 追加（旧 `.git/workflow/` ファイルを削除）
- [x] session-sync-init.ps1 / .sh: `/workflow/*.tmp` を `.gitignore` に追加
- [x] settings.json: deny ルール `Edit(**/.git/workflow/**)` → `Edit(~/.claude/projects/workflow/**)`
- [x] テスト全 pass（feature-robust-workflow*.sh）
- [ ] User verification

---

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
