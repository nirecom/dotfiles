#!/usr/bin/env bash
# Structural tests for claude-global/skills/review-plan-security/SKILL.md
set -euo pipefail

PASS=0
FAIL=0
pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SKILL="$DOTFILES_DIR/claude-global/skills/review-plan-security/SKILL.md"
CODE_SKILL="$DOTFILES_DIR/claude-global/skills/review-code-security/SKILL.md"

echo "=== review-plan-security skill structural tests ==="

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

# --- Normal case 3: name field is review-plan-security ---
if [ -f "$SKILL" ] && grep -qE '^name: review-plan-security$' "$SKILL" 2>/dev/null; then
    pass "name is 'review-plan-security'"
else
    fail "name is not 'review-plan-security'"
fi

# --- Normal case 4: has ## Procedure and ## Rules sections ---
for section in Procedure Rules; do
    if [ -f "$SKILL" ] && grep -qE "^## ${section}" "$SKILL" 2>/dev/null; then
        pass "has ## $section section"
    else
        fail "missing ## $section section"
    fi
done

# --- Normal case 5: has the 3 axis headers (same as review-code-security) ---
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

# --- Normal case 7: cross-references /review-code-security ---
if [ -f "$SKILL" ] && grep -qF '/review-code-security' "$SKILL" 2>/dev/null; then
    pass "cross-references /review-code-security"
else
    fail "does not cross-reference /review-code-security"
fi

# --- Consistency case 8: identical Axis headers between plan and code skills ---
if [ -f "$SKILL" ] && [ -f "$CODE_SKILL" ]; then
    plan_axes=$(grep -E '^### Axis [0-9]+:' "$SKILL" 2>/dev/null | sort || true)
    code_axes=$(grep -E '^### Axis [0-9]+:' "$CODE_SKILL" 2>/dev/null | sort || true)
    if [ "$plan_axes" = "$code_axes" ] && [ -n "$plan_axes" ]; then
        pass "Axis headers match between review-plan-security and review-code-security"
    else
        fail "Axis headers differ between review-plan-security and review-code-security"
    fi
else
    pass "one or both SKILL.md files absent — axis-header consistency check skipped"
fi

# --- Consistency case 9: Source: line following each axis header matches between the two files ---
if [ -f "$SKILL" ] && [ -f "$CODE_SKILL" ]; then
    mismatch=0
    checked=0
    for n in 1 2 3; do
        # Extract the first non-blank line beginning with 'Source:' that appears after '### Axis N:' and before the next '### ' header
        plan_src=$(perl -0777 -ne 'if (/### Axis '"$n"':.*?\n(.*?)(?=\n### |\z)/s) { my $b = $1; if ($b =~ /^(Source:[^\n]*)/m) { print $1 } }' "$SKILL" 2>/dev/null || true)
        code_src=$(perl -0777 -ne 'if (/### Axis '"$n"':.*?\n(.*?)(?=\n### |\z)/s) { my $b = $1; if ($b =~ /^(Source:[^\n]*)/m) { print $1 } }' "$CODE_SKILL" 2>/dev/null || true)
        if [ -z "$plan_src" ] && [ -z "$code_src" ]; then
            continue
        fi
        checked=$((checked + 1))
        if [ "$plan_src" != "$code_src" ]; then
            mismatch=1
            echo "    Axis $n Source mismatch:"
            echo "      plan: $plan_src"
            echo "      code: $code_src"
        fi
    done
    if [ "$checked" -eq 0 ]; then
        fail "no Source: lines found under any axis in either SKILL.md"
    elif [ "$mismatch" -eq 0 ]; then
        pass "Source: lines match between review-plan-security and review-code-security for all axes"
    else
        fail "Source: lines differ between review-plan-security and review-code-security"
    fi
else
    pass "one or both SKILL.md files absent — Source-line consistency check skipped"
fi

# --- Edge case 10: no absolute paths (public repo leak check) ---
if [ -f "$SKILL" ] && grep -qiE '(^|[^a-zA-Z])(c:/|/home/|/Users/)' "$SKILL" 2>/dev/null; then
    fail "absolute path found in SKILL.md (public repo leak)"
else
    pass "no absolute paths in SKILL.md"
fi

# --- Edge case 11: no references to dotfiles-private/ ---
if [ -f "$SKILL" ] && grep -qF 'dotfiles-private/' "$SKILL" 2>/dev/null; then
    fail "SKILL.md references dotfiles-private/ (private repo leak)"
else
    pass "no references to dotfiles-private/ in SKILL.md"
fi

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
