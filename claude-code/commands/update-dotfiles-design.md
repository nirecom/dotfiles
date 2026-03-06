dotfiles リポジトリの最新変更を、設計ドキュメント `dotfiles-design.md` の履歴セクションに反映する

## 対象ファイル

- dotfiles repo は下記の優先順位で探索
  - Local directory `~/dotfiles`
  - git repo `git@github.com:nirecom/dotfiles.git`
- `dotfiles-design.md` は下記の優先順位で探索
  - Local directory `../ai-specs/projects/engineering/`
  - git repo `git@github.com:nirecom/ai-specs.git`

## 手順

1. **最新コミット取得**: dotfiles repo の `git log --oneline -20` を実行し、最近のコミットを確認する

2. **現在の履歴確認**: `dotfiles-design.md` の 変更履歴, 障害履歴を読み込む

3. **差分分析**: git log のコミットと design.md の履歴テーブルを比較し、未記録のコミット・フェーズを特定する

4. **更新提案**: 未記録の変更があれば、以下の形式でユーザに提案する:
   - 変更履歴: フェーズ単位（関連コミットをグループ化）
     - 列: フェーズ | ユーザリクエスト | 実装内容 | 主要コミット
   - 障害履歴: 障害が発生した場合のみ 1 件 1 行で追加
     - 列: # | 障害 | 原因 | 修正 | コミット

5. **ユーザ確認後に編集**: 承認を得てからテーブルに行を追加する

## 形式ルール

- GitHub リンクは使用しない（テーブル内はコミットハッシュのみ）
- コミットハッシュは短縮形（7 文字）
- フェーズ名は簡潔に（例: 「QNAP 対応」「Claude Code 管理」）
- 障害番号は連番を維持する
