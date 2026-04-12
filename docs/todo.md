# TODO

## Current Work

### Workflow State Machine — Verifying
実装完了（workflow-gate.js, mark-step.js, session-start.js, lib/workflow-state.js、36テスト pass）。
`CLAUDE_ENV_FILE` フォーマット修正済み（`export KEY=VALUE` → `KEY=VALUE`）。

動作確認（4ケース）— **NEW SESSION REQUIRED**:
- [ ] 正常系: CLAUDE_ENV_FILE → CLAUDE_SESSION_ID 設定 → Step 2 以降がつながる
- [x] 異常系1: CLAUDE_SESSION_ID 取得不可 → hook stdin フォールバックで sessionid 取得（確認済み）
- [ ] 異常系2: ステートファイル破損 → Claude が block メッセージを読んで `--reset-from research` を自動実行
- [ ] 異常系3: 途中からやり直し → ユーザーが継続ステップを指定し `--reset-from <step>` を実行

#### 申し送り: 新規セッションでの確認手順

**前提確認 — デバッグログで書き込み状況を確認する**

まず以下でログを確認し、どの仮説に当てはまるか判断する:
```
cat "$TEMP/session-start-debug.log"
```

結果の解釈:
- `CLAUDE_ENV_FILE=(not set)` → **仮説2**: SessionStart フックが呼ばれていないか、settings.json の hooks 設定が効いていない（Claude Code 再起動を試みる）
- `CLAUDE_ENV_FILE=<path>` かつ `wrote CLAUDE_SESSION_ID` → **仮説1**: 書き込みは成功しているが Bash ツールのサブプロセスには注入されない設計の可能性。CLAUDE_ENV_FILE は Claude Code プロセス自身の環境変数として取り込まれるが、Bash ツール経由のシェルには伝播しないかもしれない
- `CLAUDE_ENV_FILE=<path>` かつ `write failed` → **仮説3**: パーミッション等の書き込みエラー。エラー内容を確認する

仮説1 が本命の場合は `CLAUDE_SESSION_ID` を env 経由で渡す設計を諦め、hook stdin から直接取得する現行フォールバック経路を主経路として確定させることを検討する。

**正常系**

Step 1 — `CLAUDE_SESSION_ID` が設定されているか
```
echo $CLAUDE_SESSION_ID
```
期待値: 非空 UUID

Step 2 — 未完了ステップがある状態で git commit がブロックされるか
```
cd c:/git/dotfiles && git add docs/todo.md
git commit -m "test"
```
期待値: blocked + 未完了ステップの一覧と mark-step.js の実行例が表示される

Step 3 — mark-step.js で全ステップをマーク
```
node c:/git/dotfiles/claude-global/hooks/mark-step.js $CLAUDE_SESSION_ID research complete
node c:/git/dotfiles/claude-global/hooks/mark-step.js $CLAUDE_SESSION_ID plan complete
node c:/git/dotfiles/claude-global/hooks/mark-step.js $CLAUDE_SESSION_ID write_tests complete
node c:/git/dotfiles/claude-global/hooks/mark-step.js $CLAUDE_SESSION_ID code complete
node c:/git/dotfiles/claude-global/hooks/mark-step.js $CLAUDE_SESSION_ID verify complete
node c:/git/dotfiles/claude-global/hooks/mark-step.js $CLAUDE_SESSION_ID docs complete
```

Step 4 — user_verification のみ残った状態でブロックされるか
```
git commit -m "test"
```
期待値: user_verification のみ未完了としてブロック

Step 5 — user_verification 完了後にコミットが通るか
```
node c:/git/dotfiles/claude-global/hooks/mark-step.js $CLAUDE_SESSION_ID user_verification complete
git commit -m "test"
```
期待値: コミット承認

**異常系2 — ステートファイル破損: Claude が自動リカバリ**

Step 6 — ステートファイルを破損させた状態でブロックされるか
```
echo "INVALID JSON" > .git/workflow/$CLAUDE_SESSION_ID.json
git commit -m "test"
```
期待値: blocked + `--reset-from research` の実行を指示するメッセージが表示される

Step 7 — Claude が自動で `--reset-from research` を実行し、全ステップが pending に戻るか  
期待値: research 以降が pending にリセットされ、再度 Step 3 から進められる状態になる

**異常系3 — 途中からやり直し: ユーザーがステップを指定**

Step 8 — いくつかステップを complete にした後、途中からリセットできるか
```
node c:/git/dotfiles/claude-global/hooks/mark-step.js $CLAUDE_SESSION_ID --reset-from write_tests
```
期待値: research/plan が complete のまま残り、write_tests 以降が pending に戻る（コマンド出力で確認）

#### Context
- State ファイルパス: `.git/workflow/<session-id>.json`（gitignore 済み）
- `DOTFILES_DIR` 未設定の場合は絶対パス `c:/git/dotfiles` で代替可

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
