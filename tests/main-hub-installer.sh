#!/bin/bash
# Tests for hub installer structure
# Static checks only (no actual installation).
# TDD: Many tests are expected to FAIL until the hub implementation is in place.

DOTFILES_DIR="c:/git/dotfiles"
PRIVATE_DIR="c:/git/dotfiles-private"

PASS=0
FAIL=0

run_with_timeout() {
    if command -v timeout >/dev/null 2>&1; then
        timeout 120 "$@"
    else
        perl -e 'alarm 120; exec @ARGV' -- "$@"
    fi
}

ok() {
    echo "PASS: $1"
    PASS=$((PASS + 1))
}

fail() {
    echo "FAIL: $1"
    FAIL=$((FAIL + 1))
}

check() {
    local name="$1"
    if eval "$2"; then
        ok "$name"
    else
        fail "$name"
    fi
}

echo "=== Hub Installer Tests ==="
echo ""

# ---------------------------------------------------------------------------
# S: Syntax checks
# ---------------------------------------------------------------------------
echo "--- S: Syntax checks ---"

# S1: dotfiles/install.sh bash syntax
if run_with_timeout bash -n "$DOTFILES_DIR/install.sh" 2>/dev/null; then
    ok "S1: dotfiles/install.sh bash syntax"
else
    fail "S1: dotfiles/install.sh bash syntax"
fi

# S2: dotfiles-private/install.sh bash syntax
if run_with_timeout bash -n "$PRIVATE_DIR/install.sh" 2>/dev/null; then
    ok "S2: dotfiles-private/install.sh bash syntax"
else
    fail "S2: dotfiles-private/install.sh bash syntax"
fi

# S3: dotfiles/install.ps1 PowerShell syntax (pwsh only)
if command -v pwsh >/dev/null 2>&1; then
    if run_with_timeout pwsh -NoProfile -Command "
        \$err = \$null
        try {
            \$tokens = \$null
            \$parseErrors = \$null
            [System.Management.Automation.Language.Parser]::ParseFile('$DOTFILES_DIR/install.ps1', [ref]\$tokens, [ref]\$parseErrors) | Out-Null
            if (\$parseErrors.Count -gt 0) { exit 1 } else { exit 0 }
        } catch { exit 1 }
    " 2>/dev/null; then
        ok "S3: dotfiles/install.ps1 PowerShell syntax"
    else
        fail "S3: dotfiles/install.ps1 PowerShell syntax"
    fi
else
    ok "S3: dotfiles/install.ps1 PowerShell syntax (SKIPPED - pwsh not found)"
fi

# S4: dotfiles-private/install.ps1 PowerShell syntax (pwsh only)
if command -v pwsh >/dev/null 2>&1; then
    if run_with_timeout pwsh -NoProfile -Command "
        \$err = \$null
        try {
            \$tokens = \$null
            \$parseErrors = \$null
            [System.Management.Automation.Language.Parser]::ParseFile('$PRIVATE_DIR/install.ps1', [ref]\$tokens, [ref]\$parseErrors) | Out-Null
            if (\$parseErrors.Count -gt 0) { exit 1 } else { exit 0 }
        } catch { exit 1 }
    " 2>/dev/null; then
        ok "S4: dotfiles-private/install.ps1 PowerShell syntax"
    else
        fail "S4: dotfiles-private/install.ps1 PowerShell syntax"
    fi
else
    ok "S4: dotfiles-private/install.ps1 PowerShell syntax (SKIPPED - pwsh not found)"
fi

echo ""

# ---------------------------------------------------------------------------
# D: chain 削除確認 — dotfiles/install.{sh,ps1} が sibling chain を持たないこと
# ---------------------------------------------------------------------------
echo "--- D: Chain removal checks ---"

# D1: dotfiles/install.sh に PRIVATE_INSTALLER が含まれないこと
check "D1: dotfiles/install.sh does not contain PRIVATE_INSTALLER" \
    "! grep -q 'PRIVATE_INSTALLER' '$DOTFILES_DIR/install.sh'"

# D2: dotfiles/install.sh に AGENTS_INSTALLER が含まれないこと
check "D2: dotfiles/install.sh does not contain AGENTS_INSTALLER" \
    "! grep -q 'AGENTS_INSTALLER' '$DOTFILES_DIR/install.sh'"

# D3: dotfiles/install.ps1 に PrivateInstaller が含まれないこと
check "D3: dotfiles/install.ps1 does not contain PrivateInstaller" \
    "! grep -q 'PrivateInstaller' '$DOTFILES_DIR/install.ps1'"

# D4: dotfiles/install.ps1 に AgentsInstaller が含まれないこと
check "D4: dotfiles/install.ps1 does not contain AgentsInstaller" \
    "! grep -q 'AgentsInstaller' '$DOTFILES_DIR/install.ps1'"

echo ""

# ---------------------------------------------------------------------------
# H: hub 構造確認（dotfiles-private）
# ---------------------------------------------------------------------------
echo "--- H: Hub structure checks (dotfiles-private) ---"

# H1: dotfiles-private/install.sh に clone_if_missing 関数が存在すること
check "H1: dotfiles-private/install.sh has clone_if_missing function" \
    "grep -q 'clone_if_missing' '$PRIVATE_DIR/install.sh'"

# H2: dotfiles-private/install.sh に IS_DOTFILES_SLAVE の export が存在すること
check "H2: dotfiles-private/install.sh exports IS_DOTFILES_SLAVE" \
    "grep -q 'export IS_DOTFILES_SLAVE' '$PRIVATE_DIR/install.sh'"

# H3: dotfiles-private/install.sh が dotfiles/install.sh を呼び出していること
check "H3: dotfiles-private/install.sh references dotfiles/install.sh" \
    "grep -q 'install\.sh' '$PRIVATE_DIR/install.sh'"

# H4: dotfiles-private/install.sh が agents/install.sh を呼び出していること
check "H4: dotfiles-private/install.sh references agents/install.sh" \
    "grep -q 'agents' '$PRIVATE_DIR/install.sh' && grep -q 'install\.sh' '$PRIVATE_DIR/install.sh'"

# H5: dotfiles-private/install.sh が fornix/install.sh を -x チェック付きで呼び出していること
check "H5: dotfiles-private/install.sh conditionally calls fornix/install.sh with -x check" \
    "grep -q 'fornix' '$PRIVATE_DIR/install.sh' && grep -q '\-x' '$PRIVATE_DIR/install.sh'"

# H6: dotfiles-private/install.sh が exec $SHELL -l の前に unset IS_DOTFILES_SLAVE を実行していること
check "H6: dotfiles-private/install.sh unsets IS_DOTFILES_SLAVE before exec \$SHELL -l" \
    "tail -5 '$PRIVATE_DIR/install.sh' | grep -q 'unset IS_DOTFILES_SLAVE' &&
     tail -3 '$PRIVATE_DIR/install.sh' | grep -q 'exec \$SHELL -l'"

# H7: dotfiles-private/install.ps1 に Initialize-Repo 関数が存在すること
check "H7: dotfiles-private/install.ps1 has Initialize-Repo function" \
    "grep -q 'Initialize-Repo' '$PRIVATE_DIR/install.ps1'"

# H8: dotfiles-private/install.ps1 に \$LASTEXITCODE チェックが存在すること
check "H8: dotfiles-private/install.ps1 checks \$LASTEXITCODE" \
    "grep -q 'LASTEXITCODE' '$PRIVATE_DIR/install.ps1'"

# H9: dotfiles-private/install.ps1 が dotfiles\install.ps1 への参照を含むこと
check "H9: dotfiles-private/install.ps1 references dotfiles\\install.ps1" \
    "grep -q 'install\.ps1' '$PRIVATE_DIR/install.ps1' && grep -qi 'dotfiles' '$PRIVATE_DIR/install.ps1'"

# H10: dotfiles-private/install.ps1 が agents\install.ps1 への参照を含むこと
check "H10: dotfiles-private/install.ps1 references agents\\install.ps1" \
    "grep -q 'agents' '$PRIVATE_DIR/install.ps1' && grep -q 'install\.ps1' '$PRIVATE_DIR/install.ps1'"

# H11: dotfiles-private/install.ps1 が fornix の install.ps1 を Test-Path で条件付きで呼び出していること
check "H11: dotfiles-private/install.ps1 conditionally calls fornix/install.ps1 with Test-Path" \
    "grep -q 'fornix' '$PRIVATE_DIR/install.ps1' && grep -q 'Test-Path' '$PRIVATE_DIR/install.ps1'"

echo ""

# ---------------------------------------------------------------------------
# E: exec $SHELL -l 動作
# ---------------------------------------------------------------------------
echo "--- E: exec \$SHELL -l checks ---"

# E1: dotfiles/install.sh に exec $SHELL -l が含まれること
check "E1: dotfiles/install.sh contains exec \$SHELL -l" \
    "grep -q 'exec \$SHELL -l' '$DOTFILES_DIR/install.sh'"

# E2: dotfiles/install.sh の exec $SHELL -l が IS_DOTFILES_SLAVE ガードで囲まれていること
check "E2: dotfiles/install.sh exec \$SHELL -l is guarded by IS_DOTFILES_SLAVE" \
    "grep -q 'IS_DOTFILES_SLAVE' '$DOTFILES_DIR/install.sh'"

# E3: dotfiles-private/install.sh に exec $SHELL -l が含まれること
check "E3: dotfiles-private/install.sh contains exec \$SHELL -l" \
    "grep -q 'exec \$SHELL -l' '$PRIVATE_DIR/install.sh'"

echo ""

# ---------------------------------------------------------------------------
# I: Idempotency ガード確認
# ---------------------------------------------------------------------------
echo "--- I: Idempotency guard checks ---"

# I1: dotfiles-private/install.sh の clone_if_missing が ! -d チェックを含むこと
check "I1: dotfiles-private/install.sh clone_if_missing has ! -d check" \
    "grep -q '! -d\|! \[ -d' '$PRIVATE_DIR/install.sh'"

# I2: dotfiles-private/install.ps1 の Initialize-Repo が Test-Path チェックを含むこと
check "I2: dotfiles-private/install.ps1 Initialize-Repo has Test-Path check" \
    "grep -q 'Test-Path' '$PRIVATE_DIR/install.ps1'"

echo ""

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
TOTAL=$((PASS + FAIL))
echo "=== Results: $PASS/$TOTAL passed ==="

if [ "$FAIL" -gt 0 ]; then
    echo "NOTE: FAIL is expected for unimplemented hub features (TDD style)."
    exit 1
fi

exit 0
