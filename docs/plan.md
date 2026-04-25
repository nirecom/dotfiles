# dotfiles → 2 repo 分割計画（nirecom/agents 切り出し）

> 旧 Security Enhancement Plan は `docs/plan-security.md` に退避（Phase 5 未着手）。

## Context

現在の `c:\git\dotfiles` は一般 dotfiles（shell, vim, tmux, installer）と Claude Code framework（`claude-global/`: CLAUDE.md, settings.json, rules/, skills/, hooks/, agents/）が同居している。後者を独立した公開 repo **`nirecom/agents`** として切り出す。

**動機**（ユーザー判断）:
- **主目的: 公開ブランド・露出** — 外部調査（WebSearch 2026-04）で確認: SuperClaude_Framework, awesome-claude-code, awesome-claude-code-toolkit, claude-skills, everything-claude-code など主要な公開 Claude Code framework は全て standalone repo。dotfiles 混在形態は **awesome-lists**（[hesreallyhim/awesome-claude-code](https://github.com/hesreallyhim/awesome-claude-code) など GitHub 上の curated list 群。カテゴリごとに人気 repo を収集し、公開 framework の主要発見チャネル）から発見されない。
- 副次効果: LLM 作業時の attention 軽減（dotfiles 213 ファイルと claude-global 46 ファイルが分離される）。
- 将来拡張: 単一 agent（Claude Code）に限定せず、Codex / Cursor / Gemini CLI など multi-agent 設定を同 repo で管理できる余地を残す（[amtiYo/agents](https://github.com/amtiYo/agents) と同系の設計）。`AGENTS.md` が emerging universal standard。

**許容するトレードオフ**:
- shell 起動時 git fetch が bash 3→4 本、pwsh 2→3 本に増加（並列化で緩和予定）。
- cross-cutting 変更（hook + bin/ 同時更新等）は framework repo 内で完結するので基本 1 commit。
- 初期の README / awesome-lists 投稿文を英語で用意する工数。

**今回スコープ外**:
- `dotfiles-private/` は分割しない（現状維持、sibling 参照のまま）。`claude-global/rules/language.md` の absolute symlink も維持。
- `~/.agents/` への convention 変更は見送り、`~/.claude/` の既存 symlink 先は保持（Claude Code 本体が期待するパスのため）。将来他 agent 対応時に再検討。

## 決定事項

### repo 名・位置付け
- **新 repo 名**: `nirecom/agents`（GitHub public, MIT）
- **README 記述方針**（英語）: "Personal agent configuration — currently focused on Claude Code (CLAUDE.md, skills, hooks, agents) with the structure ready to absorb Codex / Cursor / Gemini CLI settings as they stabilize." AGENTS.md universal trend に沿う旨も触れる。

### 新 repo のディレクトリ構成（root 直下）
- `CLAUDE.md`, `settings.json` — 現 `claude-global/` 配下を昇格
- `rules/`, `skills/`, `hooks/`, `agents/` — 現 `claude-global/` 配下を昇格
- `bin/` — claude workflow で使う **全て** の bin/ ツールを framework 側に集約（dotfiles 側は保持しない）:
  - `doc-append.py`, `doc-rotate.py`, `sort-history.py`, `translate-history.py`, `convert-history-table.py`, `migrate-history-categories.py`
  - `scan-outbound.sh`, `scan-inbound.js`（既に `claude-global/hooks/` 内）
  - `session-sync.sh`, `session-sync.ps1`
  - 新規: `split-history.py`（docs/history.md 分離用、下記）
- `install.sh`, `install.ps1`
- `install/{win,linux}/dotfileslink.{ps1,sh}` — claude 関連 symlink と `$AGENTS_CONFIG_DIR` 定義を担当
- `install/{win,linux}/session-sync-init.{ps1,sh}` — dotfiles から移送
- `docs/` — claude workflow 系 `history.md`, `architecture/claude-code.md`, `scan-outbound.md`, `hook-block-tests-direct.md`
- `tests/` — claude 関連テスト（既存 76 中 約 47%）
- `README.md` — public framework positioning（英語）
- `.private-info-allowlist` — framework 固有の allowlist（dotfiles 側もルート `.private-info-allowlist` を保持、scan-outbound.sh は両方を読む fallback）

### dotfiles 側の残留方針
- **claude-global 関連ファイルは一切残さない**（dotfiles-private パターンを採用）
- 残す: 一般 dotfiles (`.bashrc`, `.profile_common`, `.vimrc`, `.tmux.conf`, `.zshrc` 等), `install/{win,linux}/` の非 claude スクリプト, `config/`, `source-highlight/`, `filetype.vim`, `docs/` の dotfiles 履歴・アーキ
- `install/win/claude-usage-widget.ps1`, `install/win/claude-tabs.ps1` は **dotfiles 側に残す**（独立ツール扱い。Claude Code 有無に関わらず install 可）
- dotfiles 側 `bin/` は claude workflow 関連のみ framework へ移送。dotfiles 固有ツールは残る:
  - 残る: `detectos.sh`（OS 判定）, `tmux.sh`（tmux launcher）, `wait-vscode-window.sh`, `wait-vscode-window.ps1`（VSCode 起動待ち）
  - 移送: `doc-append.py`, `doc-rotate.py`, `sort-history.py`, `translate-history.py`, `convert-history-table.py`, `migrate-history-categories.py`, `scan-outbound.sh`, `session-sync.sh`, `session-sync.ps1`（新規 `split-history.py` は framework 側で作成）

### 新環境変数
- `$AGENTS_CONFIG_DIR` を framework の install が定義。`settings.json` 内 11 箇所の `$DOTFILES_DIR/claude-global/hooks/*.js` を `$AGENTS_CONFIG_DIR/hooks/*.js` にリネーム。
- `~/.local/bin/doc-append` symlink は framework install が作成（dotfiles 側からは `doc-append` 経由で呼べる）

### sibling install パターン（dotfiles-private 準拠）
- `$HOME/git/{dotfiles, dotfiles-private, agents}` の 3 repo 並列。
- dotfiles の `install.sh` / `install.ps1` 末尾に optional hook: sibling `agents/install.{sh,ps1}` が存在すれば呼ぶ。逆依存は作らない（agents 単独で動作）。
- dotfiles-private `install.ps1:155` / `install.sh:89` と同じパターン。

## 相互依存の解決方針

| # | 依存箇所 | 解決方針 |
|---|---|---|
| 1 | `claude-global/settings.json` L231,243,255,265,275,285,295,305,317,322,332 の `$DOTFILES_DIR/claude-global/hooks/*.js` | `$AGENTS_CONFIG_DIR/hooks/*.js` にリネーム |
| 2 | `claude-global/hooks/pre-commit` L7-8, `commit-msg` L7-8 の `../../bin/scan-outbound.sh` | `$AGENTS_CONFIG_DIR/bin/scan-outbound.sh`（framework 内で完結） |
| 3 | `bin/scan-outbound.sh` L14-15 の `$DOTFILES_DIR/../dotfiles-private/` sibling 参照 | framework 内に移した上で `${DOTFILES_PRIVATE_DIR:-$HOME/git/dotfiles-private}` optional fallback。未定義時は warning して allowlist 空で continue |
| 4 | `install/win/dotfileslink.ps1` L39-43, `install/linux/dotfileslink.sh` L41-63 の claude-global → ~/.claude/ 7 本 symlink | dotfiles 側から該当行削除。framework 側 dotfileslink が source を framework root に変えて同等の symlink を作成 |
| 5 | `install.sh` L37, `install.ps1` L65,127,132 の claude-code.*, claude-usage-widget, claude-tabs | claude-code.* / session-sync-init → framework 側へ移送。**claude-usage-widget, claude-tabs は dotfiles 側に残す**（独立ツール） |
| 6 | `install/win/dotfileslink.ps1` L124 `core.hooksPath` | framework 側 dotfileslink が `$AGENTS_CONFIG_DIR\hooks` に設定 |
| 7 | `claude-global/rules/language.md` → `$HOME/git/dotfiles-private/claude-global/rules/language.md` absolute symlink | symlink をそのまま framework `rules/language.md` として移送（target 絶対 path のまま機能） |
| 8 | `docs/history.md` の claude/dotfiles 混在 | 後述「docs/history.md 分離手順」参照（新規 `bin/split-history.py` で機械分離） |
| 9 | bin/ ツール（doc-append.py 他）の共有 | **framework 側に 1 箇所集約**（複製せず）。`~/.local/bin/` 経由で dotfiles 側からも `doc-append` 等で呼べる |
| 10 | `claude-global/rules/test.md` → `test-rules/*.md` 相対参照 | `rules/` と `test-rules/` を同時移送で無変更 |
| 11 | bash `.profile_common` L160-206, pwsh `install/win/profile.ps1` L22-65 の git fetch | bash: 4 つ目 (agents) 追加。pwsh: 2→3 (dotfiles-private + agents の parity 回復)。並列 fetch 化 |
| 12 | bash `.profile_common` L236,238 の `$DOTFILES_DIR/bin/session-sync.sh` | `${AGENTS_DIR:-$DOTFILES_DIR/../agents}/bin/session-sync.sh` に変更（framework install が `$AGENTS_DIR` export） |
| 13 | `install.ps1` L69 の `$DotfilesDir\install\win\session-sync-init.ps1` | framework 側に移送。dotfiles install からは削除 |

## docs/history.md 分離手順（機械化）

現状 540 行、`###` 単位で入れ混ざり。LLM 全件手作業は不確実なので以下で機械分離。

1. **index list 手動作成**: LLM が `docs/history.md` を読み、各 `###` エントリのタイトル + 1 行 digest を列挙した `docs/history-classification.md` を出力。各エントリに `@claude` / `@dotfiles` / `@both` のいずれかのタグを付与。`@both` は最小限に抑える。レビュー用途なのでユーザーが目視確認可能。
2. **新規スクリプト `bin/split-history.py` 作成**:
   - 入力: `docs/history.md` + `docs/history-classification.md`
   - 出力: `docs/history-dotfiles.md`（`@dotfiles` + `@both` の dotfiles 関連文脈）, `docs/history-agents.md`（`@claude` + `@both` の claude 関連文脈）
   - 実装: `###` で split → subject でマッチング → ファイル別に append → `bin/sort-history.py` 相当の昇順維持
3. **レビュー**: 出力 2 file を目視レビュー。境界エントリは分類表を手動修正 → 再実行（idempotent 設計）。
4. **確定**: `docs/history.md` → dotfiles 側 `docs/history.md`（`history-dotfiles.md` の内容）、framework 側 `docs/history.md`（`history-agents.md` の内容）。
5. **category 辞書分割**: `bin/doc-append.py` のカテゴリ定義が claude / dotfiles 両方カバーしているので、framework 側 `bin/doc-append.py` は full set を保持、dotfiles 側は不要（`~/.local/bin/doc-append` で framework のものを利用）。

## 実装ステップ（順序厳守）

各ステップは独立 commit、過渡互換は `BEGIN/END temporary` マーカーで明示。

- [x] 1. **GitHub 上に `nirecom/agents` 空 repo 作成**（public, MIT）。clone は後工程。
- [x] 2. **`$AGENTS_CONFIG_DIR` 抽象層導入（dotfiles 単体で）**: `claude-global/settings.json` の 11 箇所を `$DOTFILES_DIR/claude-global/hooks/*` → `$AGENTS_CONFIG_DIR/hooks/*` にリネーム。`.profile_common` と `install/win/profile.ps1` に:
   ```
   # --- BEGIN temporary: agents 分離前 compat ---
   export AGENTS_CONFIG_DIR="${AGENTS_CONFIG_DIR:-$DOTFILES_DIR/claude-global}"
   export AGENTS_DIR="${AGENTS_DIR:-$DOTFILES_DIR}"
   # --- END temporary ---
   ```
   1 repo のまま動作継続を確認。全テスト green。
- [x] 3. **hook 相対 path 調整**: `claude-global/hooks/pre-commit` と `commit-msg` の `../../bin/scan-outbound.sh` → `"$AGENTS_CONFIG_DIR/../bin/scan-outbound.sh"`（過渡期。step 7 以降は `$AGENTS_CONFIG_DIR/bin/` 配下）。`bin/scan-outbound.sh` L14-15 を `${DOTFILES_PRIVATE_DIR:-$DOTFILES_DIR/../dotfiles-private}` に。
- [x] 4. **session-sync path 抽象化**: `.profile_common` L236,238 の `$DOTFILES_DIR/bin/session-sync.sh` → `${AGENTS_DIR}/bin/session-sync.sh`。
- [x] 5. **tests 仕分け準備**: 76 本中 claude 関連約 36 本を特定する判定を列挙した table を作成（別ファイル `tests/split-plan.md`、レビュー用）。
- [x] 6. **docs/history.md 分類**: 上記「docs/history.md 分離手順」step 1-3 を実施。`docs/history-classification.md` と `bin/split-history.py` を dotfiles 側で作成・コミット。出力 2 file をレビューし、分類確定まで反復。
- [x] 7. **framework repo 初期化（history 保全）**:
   - dotfiles 上で `git subtree split --prefix=claude-global -b agents-split`
   - `nirecom/agents` clone → `git pull <dotfiles-path> agents-split`
   - これで claude-global の commit 史が framework repo に移植される
- [x] 8. **framework root 昇格**: framework 内で `git mv claude-global/* .` 相当。1 commit で済ませる。
- [x] 9. **framework bin/ 作成**: dotfiles の `bin/` から下記を framework `bin/` に **移動**（dotfiles 側からは削除）:
   - `doc-append.py`, `doc-rotate.py`, `sort-history.py`, `translate-history.py`, `convert-history-table.py`, `migrate-history-categories.py`, `scan-outbound.sh`, `session-sync.sh`, `session-sync.ps1`, `split-history.py`
- [x] 10. **framework install 作成**: `install.sh`, `install.ps1`, `install/{win,linux}/dotfileslink.{ps1,sh}`, `install/{win,linux}/session-sync-init.{sh,ps1}` を新規作成。dotfileslink は framework root の `CLAUDE.md, settings.json, rules, skills, agents` → `~/.claude/` 配下 symlink + `core.hooksPath = $AGENTS_CONFIG_DIR\hooks`。install は `$AGENTS_CONFIG_DIR` と `$AGENTS_DIR` を profile に export するロジックも持つ。加えて `~/.local/bin/doc-append` の symlink 作成。
- [x] 11. **framework docs/tests 移送**: step 6 で生成した `docs/history-agents.md` → framework `docs/history.md`。`docs/architecture/claude-code.md`, `docs/scan-outbound.md`, `docs/hook-block-tests-direct.md` を framework `docs/` へ。step 5 の list に従い claude 関連テスト（約 36 本）を framework `tests/` へ移動（dotfiles 側からは削除）。
- [x] 12. **framework README**: "Personal Agent Configuration" として Features / Install / Hooks / Skills / Agents の概要を英語で記載。AGENTS.md universal trend との関係性、将来 Codex / Cursor 対応の方向性にも触れる。
- [x] 13. **dotfiles 側 cleanup**:
    - `claude-global/` ディレクトリ **完全削除**
    - dotfiles `bin/` から step 9 で移送したファイルを削除
    - `install.sh` / `install.ps1` の `claude-code.*` / `session-sync-init.*` 呼び出し削除
    - `install/win/dotfileslink.ps1` の claude-global symlink・`core.hooksPath` 設定削除
    - `install/linux/dotfileslink.sh` の claude-global symlink 削除
    - dotfiles `docs/history.md` を `history-dotfiles.md` の内容に置換
    - dotfiles `docs/scan-outbound.md`, `docs/architecture/claude-code.md`, `docs/hook-block-tests-direct.md` 削除
    - `.private-info-allowlist` 削除（scanner は agents 側の allowlist を使用）
- [ ] 14. **dotfiles install に sibling hook 追加**: `install.sh` / `install.ps1` 末尾に dotfiles-private 呼び出しと同じパターンで `agents/install.{sh,ps1}` optional 呼び出しを追加。
- [ ] 15. **shell profile の fetch 追加**:
    - bash `.profile_common` に 4 つ目（agents）の fetch ブロック追加。既存 3 つと合わせて並列 fetch（`&` + `wait`）に書き換え、latency 抑制。
    - pwsh `install/win/profile.ps1` に dotfiles-private + agents の fetch 追加（bash と parity 回復）。Start-Job または ForEach-Object -Parallel で並列化。
    - `tests/main-git-fetch-sync.sh` と pwsh 等価物を新 fetch 対象含めて更新（dotfiles 側テスト）。
- [ ] 16. **一時互換ブロック除去**: step 2-4 の `BEGIN/END temporary` を全削除。`$AGENTS_CONFIG_DIR` と `$AGENTS_DIR` は framework install が独立定義する状態へ収束。
- [ ] 17. **awesome-lists 投稿**: [hesreallyhim/awesome-claude-code](https://github.com/hesreallyhim/awesome-claude-code), [rohitg00/awesome-claude-code-toolkit](https://github.com/rohitg00/awesome-claude-code-toolkit), [travisvn/awesome-claude-skills](https://github.com/travisvn/awesome-claude-skills), [VoltAgent/awesome-agent-skills](https://github.com/VoltAgent/awesome-agent-skills) へエントリ追加 PR。

## 主要変更ファイル

### framework 新規作成（step 7-12）
- `CLAUDE.md`, `settings.json`, `rules/`, `skills/`, `hooks/`, `agents/` — claude-global 昇格
- `bin/` — 10 ツール集約
- `install.sh`, `install.ps1`
- `install/win/{dotfileslink.ps1,session-sync-init.ps1}`, `install/linux/{dotfileslink.sh,session-sync-init.sh}`
- `docs/history.md`, `docs/architecture/claude-code.md`, `docs/scan-outbound.md`, `docs/hook-block-tests-direct.md`
- `tests/` — claude 関連 36 本
- `README.md`

### dotfiles 側修正
- `claude-global/settings.json` — step 2 の env var rename（削除前に通過する過渡）
- `claude-global/hooks/pre-commit`, `commit-msg` — step 3 の path 調整（削除前）
- `bin/scan-outbound.sh` — step 3 の optional fallback（削除前）
- `.profile_common` — step 2, 4, 15 で env var + session-sync path + fetch
- `install/win/profile.ps1` — step 2, 15
- `install.sh`, `install.ps1` — step 13 cleanup + step 14 sibling hook
- `install/win/dotfileslink.ps1`, `install/linux/dotfileslink.sh` — step 13 cleanup
- `docs/history.md`, `docs/architecture/claude-code.md`, `docs/scan-outbound.md`, `docs/hook-block-tests-direct.md` — step 13 削除・置換
- `tests/main-git-fetch-sync.sh` ほか — step 15
- dotfiles `docs/history-classification.md`, `bin/split-history.py` — step 6（作業成果物、分割後は framework 側に移す or 削除）

## 検証方法

### 分割前 regression（step 2-6 各時点）
- 既存全テスト（76 本）が green。`claude -p 'echo test'` で起動と hook load を確認。
- step 2 後: `env | grep AGENTS_CONFIG_DIR` が `$DOTFILES_DIR/claude-global` を指す。
- step 3 後: `git commit` で scan-outbound.sh が発火し allowlist を正しく読む。

### 分割後 e2e（step 16 完了後、clean VM）
```
git clone <dotfiles-url> ~/git/dotfiles
git clone <dotfiles-private-url> ~/git/dotfiles-private
git clone https://github.com/nirecom/agents ~/git/agents
cd ~/git/dotfiles && ./install.sh
# dotfiles install は agents/install.sh を sibling で optional 呼び出し
```
- `ls -la ~/.claude/` で `CLAUDE.md, settings.json, rules, skills, agents` が symlink として framework root を指す
- `git config --get core.hooksPath` が `~/git/agents/hooks`
- `~/.local/bin/doc-append` が framework `bin/doc-append.py` への symlink

### agents standalone 配布テスト
dotfiles を clone せず agents のみ:
```
git clone https://github.com/nirecom/agents ~/git/agents
cd ~/git/agents && ./install.sh
```
- `$DOTFILES_PRIVATE_DIR` 未定義でも scan-outbound が空 allowlist で動く（warning のみ）
- Claude Code が起動し全 hooks 発火

### 機能単位
- **pre-commit hook**: framework repo で `git commit` 時に scan-outbound が発火、`.private-info-allowlist` を正しく参照
- **settings.json hooks**: `$AGENTS_CONFIG_DIR` 解決で 11 hook 全て load される
- **session-sync**: `.profile_common:236,238` の相当コードが `$AGENTS_DIR/bin/session-sync.sh` を正しく起動
- **doc-append**: dotfiles の CWD から `doc-append docs/history.md --category FEATURE ...` が機能（framework bin への symlink 経由）
- **history 分割検証**: step 6 の split スクリプトが idempotent であること。再実行で diff が空。

### shell 起動 latency
- 分割前後で bash / pwsh の起動時間計測（`time bash -ic exit`, `Measure-Command { pwsh -NoLogo -Command exit }`）
- 並列 fetch 化の効果確認。目標: 並列 fetch 実装で +100ms 以内に収める

### tests 分離
- dotfiles 側: 残る約 40 本テストが green
- framework 側: 移送 36 本が green（各 test runner で実行）
- 両面依存判定されたテスト（例: `tests/main-commit-push-workflow-gate.sh`, `tests/main-private-repo-detection.sh`）は framework 側に配置し、`$DOTFILES_DIR` optional 前提に書き換え

## 懸念事項と fallback

- **subtree split で history が途切れる場合**（merge commit 等）: step 7 の subtree 失敗時は `git filter-repo --path claude-global/ --path-rename claude-global/:` で代替
- **dotfiles-private の allowlist が見えない環境**: step 3 fallback で optional 化済み、未定義でも warning のみで continue
- **awesome-lists 投稿の tone**: README は英語 factual 構成。過度な marketing 回避。personal brand としての運用を明示し、contributor 受付方針も README に明記
- **`nirecom/agents` 名前の混同リスク**: [amtiYo/agents](https://github.com/amtiYo/agents) precedent があり personal namespace なので実用上問題は低い。description 英語冒頭で scope を明示することで緩和

## Rollback 戦略

各 step は独立 commit で可逆:
- step 7-13 の commit を revert すれば dotfiles は元の単一 repo 構造に戻る
- framework repo は subtree 由来の独立履歴を持つので、仮に廃止する場合も dotfiles 側に git subtree pull で戻せる（理論上）
