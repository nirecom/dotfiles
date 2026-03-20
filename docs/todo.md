# TODO: claude-code → claude-global 移行の互換性レイヤー

## 作業一覧

### Phase 1+2: 互換 symlink + dotfileslink 自動実行
- [ ] `.profile_common` に migration コード追加（git pull 直後）
- [ ] `install/win/profile.ps1` に migration コード追加（git fetch/merge 直後）

### Phase 3: 後始末 (home-obsolete)
- [ ] `install/linux/install-obsolete.sh` に claude-code symlink 削除を追加
- [ ] `install/win/install-obsolete.ps1` に claude-code symlink 削除を追加

### 検証
- [x] Windows で migration 動作確認
- [x] コミット

### commands → skills 移行
- [x] `.profile_common` に migration コード追加（`BEGIN temporary: commands → skills`）
- [x] `install/win/profile.ps1` に migration コード追加（`BEGIN temporary: commands → skills`）
- [x] `dotfileslink` (Win + Linux) に旧 commands symlink 削除を追加
- [x] macOS で動作確認
- [ ] Linux (WSL2) で動作確認
- [ ] Windows 別 PC で動作確認

### 検証チェックリスト

#### macOS / Linux

```bash
# 1. claude-code が symlink で claude-global を指しているか
ls -la ~/dotfiles/claude-code
# 期待: claude-code -> claude-global

# 2. ~/.claude/settings.json が claude-global を指しているか
readlink ~/.claude/settings.json
# 期待: パスに claude-global を含む (claude-code ではない)

# 3. ~/.claude/CLAUDE.md が claude-global を指しているか
readlink ~/.claude/CLAUDE.md
# 期待: パスに claude-global を含む

# 4. ~/.claude/skills が claude-global を指しているか
readlink ~/.claude/skills
# 期待: パスに claude-global を含む

# 5. git hooksPath が claude-global を指しているか
git config core.hooksPath
# 期待: ~/dotfiles/claude-global/hooks (via ~/.config/git/config symlink)

# 6. settings.json 内の hook パスが claude-global を参照しているか
grep "claude-global" ~/.claude/settings.json
# 期待: check-private-info.js のパスに claude-global を含む

# 7. claude-global ディレクトリの実在確認
ls ~/dotfiles/claude-global/
# 期待: CLAUDE.md settings.json hooks/ skills/ rules/ 等が存在
```

#### Windows (PowerShell)

```powershell
# 1. claude-code が symlink で claude-global を指しているか
(Get-Item ~/dotfiles/claude-code -Force).Target
# 期待: claude-global を含むパス

# 2. ~/.claude/settings.json が claude-global を指しているか
(Get-Item ~/.claude/settings.json -Force).Target
# 期待: パスに claude-global を含む (claude-code ではない)

# 3. ~/.claude/CLAUDE.md が claude-global を指しているか
(Get-Item ~/.claude/CLAUDE.md -Force).Target
# 期待: パスに claude-global を含む

# 4. ~/.claude/skills が claude-global を指しているか
(Get-Item ~/.claude/skills -Force).Target
# 期待: パスに claude-global を含む

# 5. git hooksPath が claude-global を指しているか
git config core.hooksPath
# 期待: ~/dotfiles/claude-global/hooks (via ~/.config/git/config symlink)

# 6. settings.json 内の hook パスが claude-global を参照しているか
Select-String "claude-global" ~/.claude/settings.json
# 期待: check-private-info.js のパスに claude-global を含む

# 7. claude-global ディレクトリの実在確認
ls ~/dotfiles/claude-global/
# 期待: CLAUDE.md settings.json hooks/ skills/ rules/ 等が存在
```

## 後で削除すべき一時コード

以下のコードは全 PC で移行が完了した後に削除すること:

| ファイル | 削除対象 | 削除タイミング |
|:---|:---|:---|
| `.profile_common` | `BEGIN temporary: claude-code → claude-global` ブロック | 全 Linux/macOS PC で移行完了後 |
| `install/win/profile.ps1` | `BEGIN temporary: claude-code → claude-global` ブロック | 全 Windows PC で移行完了後 |
| `install/linux/install-obsolete.sh` | claude-code symlink 削除コード | 上記一時コード削除と同時 |
| `install/win/install-obsolete.ps1` | claude-code symlink 削除コード | 上記一時コード削除と同時 |
| `.gitignore` | `claude-code` エントリ | 上記一時コード削除と同時 |
| `.profile_common` | `BEGIN temporary: commands → skills` ブロック | 全 Linux/macOS PC で移行完了後 |
| `install/win/profile.ps1` | `BEGIN temporary: commands → skills` ブロック | 全 Windows PC で移行完了後 |

## commands → skills 移行の動作確認

### 前提

`git pull` 後の状態（マイグレーションコードが未適用の PC）：
- `claude-global/commands/` → git が削除済み
- `~/.claude/commands` → ダングリング symlink（リンク先が消えている）
- `~/.claude/skills` → 存在しない
- → スキルが動かない

### 確認手順（macOS / Linux）

```bash
# 1. git pull して最新にする
cd ~/dotfiles && git pull

# 2. ダングリング symlink の確認
ls -la ~/.claude/commands 2>&1
# 期待: 「No such file or directory」またはリンク切れ表示

# 3. dotfileslink を実行して symlink を更新
~/dotfiles/install/linux/dotfileslink.sh

# 4. skills symlink が正しく作られたか
readlink ~/.claude/skills
# 期待: ~/dotfiles/claude-global/skills

# 5. 旧 commands symlink が消えているか
test -L ~/.claude/commands && echo "FAIL: commands symlink still exists" || echo "OK"

# 6. skills の中身が見えるか
ls ~/.claude/skills/
# 期待: complete-task  start-task  update-docs  update-instruction

# 7. Claude Code で確認
# claude を起動し / を入力 → 4つのスキルが表示されることを確認
```

### 確認手順（Windows PowerShell）

```powershell
# 1. git pull して最新にする
cd ~/dotfiles; git pull

# 2. ダングリング symlink の確認
(Get-Item ~/.claude/commands -Force -ErrorAction SilentlyContinue).Target
# 期待: エラーまたはリンク切れ

# 3. dotfileslink を実行して symlink を更新（管理者 or Developer Mode 必要）
~/dotfiles/install/win/dotfileslink.ps1

# 4. skills symlink が正しく作られたか
(Get-Item ~/.claude/skills -Force).Target
# 期待: claude-global\skills を含むパス

# 5. 旧 commands symlink が消えているか
Test-Path ~/.claude/commands
# 期待: False

# 6. skills の中身が見えるか
ls ~/.claude/skills/
# 期待: complete-task  start-task  update-docs  update-instruction

# 7. Claude Code で確認
# claude を起動し / を入力 → 4つのスキルが表示されることを確認
```

### マイグレーションコード適用後の確認

マイグレーションコードが `.profile_common` / `profile.ps1` に入った後は、
`dotfileslink` を手動実行しなくても、シェル起動時に自動で symlink が更新される。
上記手順の 3 を省略して 4〜7 を確認すればよい。

---

## PermissionRequest hook & settings.json 改善

### 背景
- `feature/permission-hook` ブランチで着手したが、「Ask before edits」モードが動作しなくなる問題が発生し廃棄
- settings.json の hook 形式は nested format が正しい（flat format だと settings.json 全体がスキップされる）
- deny ルールが Bash コマンドに対して効かない既知バグあり（GitHub #25621, #18846, #6699）
- `Edit|Write` PreToolUse hook は Bash の Ask 挙動に影響しない（2026-03-18 再テスト済み）。以前の「Ask バイパス」報告は再起動不足による誤認の可能性。Edit|Write の private info スキャンは pre-commit hook でカバー（役割分離の観点）
- VSCode「Ask before edits」モードは Edit/Write のみ Ask 対象。main/feature 両ブランチで git commit に Ask が出ないことを確認済み（2026-03-18）。「Ask」モードは VSCode に存在しない
- PermissionRequest hook は settings.json の変更がホットリロードされるタイミングが不安定。変更後は必ず Claude Code を再起動すること

### タスク
- [x] settings.json を nested format に修正（`41f092c`）
- [x] PreToolUse hook を Bash のみに限定（Edit|Write は ask バイパス問題のため削除）
- [x] MSYS/WSL パス検出を check-private-info.sh に追加
- [x] テストスイート作成・全 44 テスト通過（`tests/feature-permission-hook2.sh`）
- [x] `git commit -m *` を allow リストから削除（ワークフロールールで commit message を確認する運用）
- [x] `permissions.ask` で Bash コマンドの Ask 制御が可能であることを発見（2026-03-18）
  - `Bash(git commit *)` / `Bash(git -C * commit *)` を `permissions.ask` に設定 → 「Ask before edits」モードで Ask ダイアログが出ることを確認
  - 「Edit automatically」モードでは `permissions.ask` は無視される（全自動許可）
  - PermissionRequest hook は不要 — `permissions.ask` で同等の制御が実現可能
- [ ] docs 更新（architecture.md, history.md）

### 注意事項
- PermissionRequest hook は `-p`（非対話）モードでは発火しない
- 各ステップで動作確認してから次へ進むこと
