# TODO

## Current Work

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

### session-sync reset バグ — 要対応
`reset` コマンドで `$CLAUDE_DIR/history.jsonl` が存在しない場合に `cat` が exit 1 を返し、
`set -euo pipefail` により即終了する (`2>/dev/null` では pipefail を回避できない)。
テストが reset セクション以降に進めない原因になっている。
- [ ] 修正: `$CLAUDE_DIR/history.jsonl` の存在チェックを追加するか `|| true` で pipefail を回避する
- [ ] テストが reset セクションを含め全 PASS になることを確認

