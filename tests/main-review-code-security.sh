#!/usr/bin/env bash
# Structural tests for claude-global/skills/review-code-security/SKILL.md
set -euo pipefail

PASS=0
FAIL=0
pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SKILL="$DOTFILES_DIR/claude-global/skills/review-code-security/SKILL.md"
PRIVATE_INFO_DOC="$DOTFILES_DIR/docs/private-info-scanning.md"

echo "=== review-code-security skill structural tests ==="

# --- Normal case 1: SKILL.md exists ---
if [ -f "$SKILL" ]; then
    pass "SKILL.md exists"
else
    fail "SKILL.md does not exist"
fi

# --- Normal case 2: frontmatter has required fields ---
for field in name description model effort; do
    if [ -f "$SKILL" ] && grep -qE "^${field}:" "$SKILL" 2>/dev/null; then
        pass "frontmatter has '$field'"
    else
        fail "frontmatter missing '$field'"
    fi
done

# --- Normal case 3: name field is review-code-security ---
if [ -f "$SKILL" ] && grep -qE '^name: review-code-security$' "$SKILL" 2>/dev/null; then
    pass "name is 'review-code-security'"
else
    fail "name is not 'review-code-security'"
fi

# --- Normal case 4: has ## Procedure, ## Rules, ## Patterns by Axis sections ---
for section in "Procedure" "Rules" "Patterns by Axis"; do
    if [ -f "$SKILL" ] && grep -qE "^## ${section}" "$SKILL" 2>/dev/null; then
        pass "has ## $section section"
    else
        fail "missing ## $section section"
    fi
done

# --- Normal case 5: has the 3 axis headers ---
for axis in "Axis 1: Information Leakage" "Axis 2: Third-Party Access" "Axis 3: External Access"; do
    if [ -f "$SKILL" ] && grep -qF "### $axis" "$SKILL" 2>/dev/null; then
        pass "has '### $axis' header"
    else
        fail "missing '### $axis' header"
    fi
done

# --- Normal case 6: contains OWASP or CWE- citations ---
if [ -f "$SKILL" ] && grep -qE '(OWASP|CWE-)' "$SKILL" 2>/dev/null; then
    pass "contains OWASP/CWE citations"
else
    fail "missing OWASP/CWE citations"
fi

# --- Normal case 7: references Gitleaks, Semgrep, detect-secrets ---
for tool in Gitleaks Semgrep detect-secrets; do
    if [ -f "$SKILL" ] && grep -qi "$tool" "$SKILL" 2>/dev/null; then
        pass "references '$tool'"
    else
        fail "does not reference '$tool'"
    fi
done

# --- Normal case 8: cross-references /review-plan-security ---
if [ -f "$SKILL" ] && grep -qF '/review-plan-security' "$SKILL" 2>/dev/null; then
    pass "cross-references /review-plan-security"
else
    fail "does not cross-reference /review-plan-security"
fi

# --- Normal case 9: references docs/private-info-scanning.md ---
if [ -f "$SKILL" ] && grep -qF 'docs/private-info-scanning.md' "$SKILL" 2>/dev/null; then
    pass "references docs/private-info-scanning.md"
else
    fail "does not reference docs/private-info-scanning.md"
fi

# --- Normal case 10: contains 'Automated coverage' phrase in Axis 1 ---
if [ -f "$SKILL" ]; then
    # Extract Axis 1 section content (between '### Axis 1:' and the next '### ' or '## ' header)
    axis1_content=$(perl -0777 -ne 'if (/### Axis 1:.*?\n(.*?)(?=\n### |\n## |\z)/s) { print $1 }' "$SKILL" 2>/dev/null || true)
    if echo "$axis1_content" | grep -qiF 'Automated coverage'; then
        pass "Axis 1 contains 'Automated coverage' phrase"
    else
        fail "Axis 1 does not contain 'Automated coverage' phrase"
    fi
else
    fail "SKILL.md absent — Axis 1 'Automated coverage' check skipped"
fi

# --- Edge case 11: no absolute paths (public repo leak check) ---
if [ -f "$SKILL" ] && grep -qiE '(^|[^a-zA-Z])(c:/|/home/|/Users/)' "$SKILL" 2>/dev/null; then
    fail "absolute path found in SKILL.md (public repo leak)"
else
    pass "no absolute paths in SKILL.md"
fi

# --- Edge case 12: no references to dotfiles-private/ ---
if [ -f "$SKILL" ] && grep -qF 'dotfiles-private/' "$SKILL" 2>/dev/null; then
    fail "SKILL.md references dotfiles-private/ (private repo leak)"
else
    pass "no references to dotfiles-private/ in SKILL.md"
fi

# --- Security case 13: AWS key pattern appears only inside backticks/code blocks ---
# Strategy: strip fenced code blocks (``` ... ```) and inline backtick spans, then search.
if [ -f "$SKILL" ]; then
    stripped=$(perl -0777 -pe 's/```.*?```//gs; s/`[^`]*`//g' "$SKILL" 2>/dev/null || true)
    if echo "$stripped" | grep -qE 'AKIA[0-9A-Z]{16}'; then
        fail "AWS key pattern AKIA[0-9A-Z]{16} appears outside code blocks/backticks in SKILL.md"
    else
        pass "AWS key pattern only appears inside code blocks/backticks (or not at all)"
    fi
else
    pass "SKILL.md absent — security pattern check skipped (file-existence failure already recorded)"
fi

# --- Idempotency case 14: docs/private-info-scanning.md mentions review-code-security exactly once ---
if [ -f "$PRIVATE_INFO_DOC" ]; then
    count=$(grep -c 'review-code-security' "$PRIVATE_INFO_DOC" 2>/dev/null || true)
    : "${count:=0}"
    if [ "$count" -eq 1 ]; then
        pass "docs/private-info-scanning.md mentions review-code-security exactly once"
    else
        fail "docs/private-info-scanning.md mentions review-code-security $count times (expected exactly 1)"
    fi
else
    fail "docs/private-info-scanning.md does not exist"
fi

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
