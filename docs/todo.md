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
- [x] mac 1台で検証チェックリスト完了（コミットした PC とは別）
- [x] win 1台で検証チェックリスト完了（コミットした PC とは別）

## 後で削除すべき一時コード

以下のコードは全 PC で移行が完了した後に削除すること:

| ファイル | 削除対象 | 削除タイミング |
|:---|:---|:---|
| `.profile_common` | "One-time migration: claude-code → claude-global" ブロック | 全 Linux/macOS PC で移行完了後 |
| `install/win/profile.ps1` | "One-time migration: claude-code → claude-global" ブロック | 全 Windows PC で移行完了後 |
| `install/linux/home-obsolete.sh` | claude-code symlink 削除コード | 上記一時コード削除と同時 |
| `install/win/home-obsolete.ps1` | claude-code symlink 削除コード | 上記一時コード削除と同時 |
| `.gitignore` | `claude-code` エントリ | 上記一時コード削除と同時 |
