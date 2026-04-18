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
| Trojan Source (CVE-2021-42574) | https://trojansource.codes/ |

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

**Goal**: `/review-code-security` skill を新規作成（コードレビュー時の具体パターン集）。`scan-outbound.sh` にハードシークレット検出を追加。`pre-commit` に `.env` ファイルブロックを追加。

**変更ファイル**:
- `claude-global/skills/review-code-security/SKILL.md` (新規)
- `claude-global/skills/review-plan-security/SKILL.md` (相互参照追記)
- `bin/scan-outbound.sh` (AWS/LLM API キー等 11 パターン追加)
- `claude-global/hooks/pre-commit` (`.env` ファイルコミットブロック追加)
- `docs/scan-outbound.md` (パターン表と相互参照追記)

**内容**:
- review-plan-security と同一 3 軸構造、各軸に具体パターン表
- scan-outbound.sh: AWS key, PEM header, GitHub PAT, Slack token, OpenAI/Anthropic/Google/HuggingFace/Groq/Replicate/Cohere API キー (Gitleaks 由来パターン)

## Phase 4: Trojan Source 対策 + Prompt Injection 統合

**Goal**: Trojan Source 検出を `scan-outbound.sh` に追加。Prompt Injection 関連パターンを既存 Axis 2/3 に統合。

**変更ファイル**: `bin/scan-outbound.sh`, `tests/main-hidden-char-scan.sh` (新規), `claude-global/skills/review-plan-security/SKILL.md`, `claude-global/skills/review-code-security/SKILL.md`, `docs/scan-outbound.md`。また `check-private-info.sh` → `scan-outbound.sh`、`check-private-info.js` → `scan-outbound.js`、`private-info-scanning.md` → `scan-outbound.md` に改名。

**内容**:
- `scan-outbound.sh`: zero-width chars (U+200B/C/D, U+FEFF) と Bidi override chars (U+202D/E, U+2066–2069) を検出 (Trojan Source, CVE-2021-42574)
- Prompt Injection パターンは新 Axis 4 ではなく既存 Axis 2（Tool Poisoning / Rug Pull / Return Value Injection）と Axis 3（Instruction override / Base64）に統合
- 脅威モデル整理: pre-commit での Unicode 検出は Trojan Source 対策。Prompt injection 本体の防御は PostToolUse hook（Phase 5）

Revised scope: Prompt Injection patterns integrated into Axis 2/3 (no new Axis 4). Trojan Source (zero-width/bidi) detection added to `scan-outbound.sh`. True prompt-injection defense via PostToolUse hook deferred to Phase 5.

## Phase 5: Prompt Injection Defense (PostToolUse hook)

**Goal**: `PostToolUse` hook を新設し、ツール結果（WebFetch / Read / Bash 出力）に対して instruction override、Base64 難読化などを検出。

**Baseline**: 別プロジェクト `judgeclaw` (`bridge/injection_signals.py`) の `_PATTERNS` リストを移植（direct_override_en/jp, disregard_en/jp, role_override_en, chatml_tag, role_tag, expose_system_jp, base64_blob）。Benign-context exclusion（PEM / data:image/ / コード文脈での Base64 誤検知回避）も踏襲。

**参考**: `judgeclaw/bridge/injection_signals.py`, lasso-security/claude-hooks

**未着手 — 別セッションで計画から実施**

## Status

- [x] Phase 1: Architecture Security Checklist
- [x] Phase 2: Security Test Cases
- [x] Phase 3: Security Patterns
- [x] Phase 4: Trojan Source 対策 + Prompt Injection 統合
- [ ] Phase 5: Prompt Injection Defense (PostToolUse hook)
