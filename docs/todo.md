# TODO

## Current Work

### doc-append/doc-rotate + architecture.md split — Verifying
- [ ] Verify: `bin/doc-append.py` / `bin/doc-rotate.py` が動作する、`docs/architecture/` 分割後のドキュメントが読める、`settings.json` の permission が正しい

### install-obsolete: .git/workflow クリーンアップ — Verifying
- [ ] Verify: `install-obsolete.sh` / `install-obsolete.ps1` の `.git/workflow` サルベージ・削除処理が正常動作することを確認

### mark-step.js 削除・workflow-gate/mark メッセージ修正 — Verifying

Windows / WSL / macOS それぞれで以下を確認:

**1. ブロックメッセージが echo マーカー形式になっている**
- 未完了状態で `git commit` を実行 → block メッセージに `node mark-step.js` が含まれないこと
- code / verify ステップ未完了時: `echo "<<WORKFLOW_MARK_STEP_code_complete>>"` が表示される
- state ファイル不在時: `echo "<<WORKFLOW_RESET_FROM_research>>"` が表示される

**2. echo マーカーが実際に機能する**
- `echo "<<WORKFLOW_MARK_STEP_code_complete>>"` → state ファイルに code=complete が記録される
- `echo "<<WORKFLOW_RESET_FROM_research>>"` → ask ダイアログが出て、承認後に全ステップが pending に戻る

- [ ] Windows
- [ ] WSL
- [ ] macOS




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
- [ ] Workflow State Machine の E2E 検証（全環境完了済み）— 着手可能

### [Bug] commit-push スキルが workflow-gate をすり抜けられない — 要修正

**再現手順:**
1. メインセッションで `echo "<<WORKFLOW_MARK_STEP_*_complete>>"` を複数回出力してステップをマーク
2. `/commit-push` スキルを起動 → サブエージェントとして別セッション ID で実行される
3. サブエージェント内で `git commit` → PreToolUse フック (workflow-gate) がサブエージェントの `CLAUDE_ENV_FILE` でセッション ID を解決
4. サブエージェントは新規セッション = 全ステップ pending → ブロック

**根本原因:** workflow-gate はセッション ID スコープでステートを管理するが、スキル（サブエージェント）はメインセッションとは別の UUID を持つ。メインセッションで完了済みのステップ状態がサブエージェントに引き継がれない。

**回避策（実施済み）:** `node -e` で `USERPROFILE` 経由のステートファイル (`~/.claude/projects/workflow/{session-id}.json`) を直接書き換え。

**修正候補:**
- [ ] スキル起動時に親セッション ID を `CLAUDE_PARENT_SESSION_ID` として渡し、workflow-gate がフォールバック参照する
- [ ] workflow-gate がサブエージェントコンテキストを検出した場合は gate をスキップ
- [ ] commit-push スキルを「メイン会話でのみ動作する」設計に変更し、サブエージェント化しない
