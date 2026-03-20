# Windows 側: スクリプトリネーム & オプション体系変更

Linux 側で以下を実施済み。Windows 側も合わせる。

## 1. リネーム

- [ ] `home-obsolete.ps1` → `install-obsolete.ps1` にリネーム (`git mv`)
- [ ] `home-obsolete.ps1` 内コメント (1-2行目) を更新
- [ ] `docs/architecture.md` の `home-obsolete.ps1` 参照を更新
- [ ] `docs/todo.md` の `home-obsolete.ps1` 参照を更新 (2箇所)
- [ ] `docs/history.md` の `home-obsolete.ps1` 参照を更新 (2箇所)

## 2. インストールオプション再編

Linux 側の `install.sh` に合わせて、`install.ps1` のオプション体系を変更:

- `install.ps1`           : symlinks のみ (現行通り)
- `install.ps1 -Base`     : symlinks + base パッケージ (starship, fnm, uv 等)
- `install.ps1 -Develop`  : symlinks + dev tools
- `install.ps1 -Full`     : symlinks + base + dev tools

現状 `-Full` に全部入っているので、base と develop に分類する。

参考: Linux 側の分類
- base: install-base.sh (brew-git, awscli, fnm, zsh, vim, tmux) + rize.sh + claude-usage-widget.sh
- develop: install-develop.sh (fnm, flutter 等)

## 3. 完了後

- [ ] このファイルを削除
