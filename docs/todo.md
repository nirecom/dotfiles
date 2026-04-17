# TODO

## Current Work

### Workflow State Inheritance across VS Code Restarts — Verifying

実装完了。以下の手動 Smoke 確認が残り:
- [ ] hook 変更 → VS Code 再起動 → 旧 session の steps が引き継がれることを確認
- [ ] 並行セッション2つ起動 → 最後に使った方が再起動後に選ばれることを確認
- [ ] `/compact` 実行後に session_id が維持されることを確認（PostCompact hook）


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

### [Bug] workflow-gate の hasStagedTestChanges が Unix スタイルパスを解決できない — Verifying

- [ ] Verify: Git Bash から `git -C /<drive>/git/dotfiles commit ...` を実行し、tests/ がステージ済みのとき commit がブロックされないことを確認

### [Bug] workflow-gate の research/plan スキップマーカーが未定義 — 要修正

**再現手順:**
1. 単一ファイルのドキュメント変更など、research/plan が不要なタスクで `/commit-push` を実行
2. workflow-gate が research/plan を `pending` のまま検出してブロック
3. `write_tests` には `<<WORKFLOW_WRITE_TESTS_NOT_NEEDED>>` echo マーカーがあるが、research/plan に相当するスキップマーカーが存在しない
4. 回避策として workflow state JSON を直接編集するしかない

**修正候補:**
- [ ] `echo "<<WORKFLOW_RESEARCH_NOT_NEEDED>>"` / `echo "<<WORKFLOW_PLAN_NOT_NEEDED>>"` マーカーを追加
- [ ] workflow-gate.js の block メッセージにもスキップ方法を案内する

### [Bug] workflow-gate の正規表現が引数テキスト内の "git commit" に誤マッチ — 要修正

**再現手順:**
1. `doc-append.py` 等の Bash コマンドの引数に `git commit` という文字列を含める
   （例: `--background "... workflow-gate blocked every git commit from within the skill."`）
2. PreToolUse フック (workflow-gate.js) が `command.match(/git\s+(?:-C\s+\S+\s+)?commit\s/)` を実行
3. コマンド文字列全体を対象に検索するため、引数テキスト内の "git commit" にもマッチする
4. コマンドは `git commit` ではないのに commit ブロックが発動

**修正候補:**
- [ ] 正規表現を行頭アンカー付き `/^git\s+(?:-C\s+\S+\s+)?commit\s/` に変更
- [ ] またはコマンドを空白分割して最初のトークンが `git`、次が `commit` であることを確認
