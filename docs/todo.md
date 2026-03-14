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

# 4. ~/.claude/commands が claude-global を指しているか
readlink ~/.claude/commands
# 期待: パスに claude-global を含む

# 5. git hooksPath が claude-global を指しているか
git config core.hooksPath
# 期待: ~/dotfiles/claude-global/hooks (via ~/.config/git/config symlink)

# 6. settings.json 内の hook パスが claude-global を参照しているか
grep "claude-global" ~/.claude/settings.json
# 期待: check-private-info.js のパスに claude-global を含む

# 7. claude-global ディレクトリの実在確認
ls ~/dotfiles/claude-global/
# 期待: CLAUDE.md settings.json hooks/ commands/ agents/ 等が存在
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

# 4. ~/.claude/commands が claude-global を指しているか
(Get-Item ~/.claude/commands -Force).Target
# 期待: パスに claude-global を含む

# 5. git hooksPath が claude-global を指しているか
git config core.hooksPath
# 期待: ~/dotfiles/claude-global/hooks (via ~/.config/git/config symlink)

# 6. settings.json 内の hook パスが claude-global を参照しているか
Select-String "claude-global" ~/.claude/settings.json
# 期待: check-private-info.js のパスに claude-global を含む

# 7. claude-global ディレクトリの実在確認
ls ~/dotfiles/claude-global/
# 期待: CLAUDE.md settings.json hooks/ commands/ agents/ 等が存在
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
