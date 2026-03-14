# TODO: claude-code → claude-global 移行の互換性レイヤー

## 作業一覧

### Phase 1+2: 互換 symlink + dotfileslink 自動実行
- [ ] `.profile_common` に migration コード追加（git pull 直後）
- [ ] `install/win/profile.ps1` に migration コード追加（git fetch/merge 直後）

### Phase 3: 後始末 (home-obsolete)
- [ ] `install/linux/home-obsolete.sh` に claude-code symlink 削除を追加
- [ ] `install/win/home-obsolete.ps1` に claude-code symlink 削除を追加

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
| `install/linux/home-obsolete.sh` | claude-code symlink 削除コード | 上記一時コード削除と同時 |
| `install/win/home-obsolete.ps1` | claude-code symlink 削除コード | 上記一時コード削除と同時 |
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
