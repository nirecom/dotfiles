# Security Enhancement Plan

セキュリティチェックリストとテストカバレッジの強化。Phase ごとにワークフロー（テスト → コード → 検証 → ドキュメント → コミット）を回す。

## 参照先一覧

| リソース | URL |
|---|---|
| OWASP WSTG v4.2 | https://owasp.org/www-project-web-security-testing-guide/v42/ |
| OWASP ASVS | https://owasp.org/www-project-application-security-verification-standard/ |
| CWE Top 25 (2025) | https://cwe.mitre.org/top25/archive/2025/2025_cwe_top25.html |
| OWASP LLM Top 10 (2025) | https://owasp.org/www-project-top-10-for-large-language-model-applications/ |
| OWASP MCP Top 10 (2025) | https://owasp.org/www-project-mcp-top-10/ |
| TikiTribe/claude-secure-coding-rules | https://github.com/TikiTribe/claude-secure-coding-rules |
| Anthropic/claude-code-security-review | https://github.com/anthropics/claude-code-security-review |
| claude-code-ultimate-guide (injection scanner) | https://github.com/FlorianBruniaux/claude-code-ultimate-guide/blob/main/examples/hooks/bash/unicode-injection-scanner.sh |
| lasso-security/claude-hooks | https://github.com/lasso-security/claude-hooks |
| Gitleaks | https://github.com/gitleaks/gitleaks |
| Semgrep | https://github.com/semgrep/semgrep |
| detect-secrets | https://github.com/Yelp/detect-secrets |

## Phase 1: Architecture Security Checklist

**Goal**: `/review-plan-security` skill（旧 `/review-security`）を作成。計画時にセキュリティ 3 軸を確認させる。

**変更ファイル**: `claude-global/skills/review-plan-security/SKILL.md` (review-security からリネーム)

**内容**:
- Information Leakage チェック (出典: OWASP ASVS V8 Data Protection, V6 Stored Cryptography)
- Third-Party Access チェック (出典: OWASP MCP Top 10 MCP03/MCP04, LLM Top 10 LLM03)
- External Access チェック (出典: OWASP WSTG Input Validation, CWE Top 25 #2 CWE-89)

## Phase 2: Security Test Cases

**Goal**: `claude-global/rules/test.md` に Security cases カテゴリを追加。

**変更ファイル**: `claude-global/rules/test.md` (編集)

**追加項目** (各出典付き):
- Secret leakage (OWASP ASVS V8, Anthropic security-review "Crypto & Secrets")
- Input injection (OWASP WSTG Input Validation, CWE-78, CWE-22)
- Permission (OWASP ASVS V4 Access Control)
- Prompt injection (OWASP LLM Top 10 LLM01, MCP Top 10 MCP06)
- Idempotency of security (既存 Idempotency cases のセキュリティ拡張)

## Phase 3: Security Patterns

**Goal**: `/review-code-security` skill を新規作成（コードレビュー時の具体パターン集）。`check-private-info.sh` にハードシークレット検出を追加。`pre-commit` に `.env` ファイルブロックを追加。

**変更ファイル**:
- `claude-global/skills/review-code-security/SKILL.md` (新規)
- `claude-global/skills/review-plan-security/SKILL.md` (相互参照追記)
- `bin/check-private-info.sh` (AWS/LLM API キー等 11 パターン追加)
- `claude-global/hooks/pre-commit` (`.env` ファイルコミットブロック追加)
- `docs/private-info-scanning.md` (パターン表と相互参照追記)

**内容**:
- review-plan-security と同一 3 軸構造、各軸に具体パターン表
- check-private-info.sh: AWS key, PEM header, GitHub PAT, Slack token, OpenAI/Anthropic/Google/HuggingFace/Groq/Replicate/Cohere API キー (Gitleaks 由来パターン)

## Phase 4: Prompt Injection Defense

**Goal**: Prompt injection 防御パターンを review-plan-security と review-code-security に追加。

**変更ファイル**: `claude-global/skills/review-plan-security/SKILL.md` (追記), `claude-global/skills/review-code-security/SKILL.md` (追記)

**内容**:
- Prompt Injection Defense セクション (出典: OWASP LLM Top 10 LLM01, MCP Top 10 MCP03/MCP06)
- MCP 固有攻撃パターン: Tool Poisoning, Rug Pull, Return Value Injection (出典: https://aminrj.com/posts/owasp-mcp-top-10/)
- 検出パターン: Zero-width chars, Bidi overrides, Base64 難読化, Instruction override
  (出典: claude-code-ultimate-guide unicode-injection-scanner.sh, lasso-security/claude-hooks)
- 将来の統合オプション: lasso-security/claude-hooks (PostToolUse hook)

## Status

- [x] Phase 1: Architecture Security Checklist
- [x] Phase 2: Security Test Cases
- [x] Phase 3: Security Patterns
- [ ] Phase 4: Prompt Injection Defense
