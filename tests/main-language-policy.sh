#!/bin/bash
# Test: language policy changes — rules moved from public to dotfiles-private
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PRIVATE_DIR="$DOTFILES_DIR/../dotfiles-private"
AGENTS_DIR="$DOTFILES_DIR/../agents"

pass=0
fail=0

assert_true() {
    local desc="$1"
    if eval "$2"; then
        echo "PASS: $desc"
        pass=$((pass + 1))
    else
        echo "FAIL: $desc"
        fail=$((fail + 1))
    fi
}

assert_false() {
    local desc="$1"
    if eval "$2"; then
        echo "FAIL: $desc (should not match)"
        fail=$((fail + 1))
    else
        echo "PASS: $desc"
        pass=$((pass + 1))
    fi
}

echo "=== Normal Cases ==="

# 1. coding.md does NOT contain English-only language rule
assert_false "coding.md does not contain language-rule line" \
    "grep -q 'Source code and config file messages/comments must be in English' '$DOTFILES_DIR/claude-global/rules/coding.md'"

# 2. docs-convention.md does NOT contain language-specific lines
assert_false "docs-convention.md does not contain Public repositories English line" \
    "grep -qE 'Public repositories.*English' '$DOTFILES_DIR/claude-global/rules/docs-convention.md'"

assert_false "docs-convention.md does not contain Private repositories Japanese line" \
    "grep -qE 'Private repositories.*Japanese' '$DOTFILES_DIR/claude-global/rules/docs-convention.md'"

# 3. .config/git/ignore contains language.md entry
assert_true ".config/git/ignore contains language.md gitignore entry" \
    "grep -q 'claude-global/rules/language.md' '$DOTFILES_DIR/.config/git/ignore'"

# 4. install.ps1 references dotfiles-private installer
assert_true "install.ps1 references dotfiles-private installer" \
    "grep -q 'dotfiles-private' '$DOTFILES_DIR/install.ps1'"

# 5. install.sh references dotfiles-private/install.sh
assert_true "install.sh references dotfiles-private/install.sh" \
    "grep -q 'dotfiles-private/install.sh' '$DOTFILES_DIR/install.sh'"

# 6. Private repo language.md exists with all 4 section headers
assert_true "private language.md exists" \
    "[ -f '$PRIVATE_DIR/claude-global/rules/language.md' ]"

assert_true "private language.md has Conversation header" \
    "grep -q '## Conversation' '$PRIVATE_DIR/claude-global/rules/language.md'"

assert_true "private language.md has Commit Messages header" \
    "grep -q '## Commit Messages' '$PRIVATE_DIR/claude-global/rules/language.md'"

assert_true "private language.md has Code header" \
    "grep -q '## Code' '$PRIVATE_DIR/claude-global/rules/language.md'"

assert_true "private language.md has Documentation header" \
    "grep -q '## Documentation' '$PRIVATE_DIR/claude-global/rules/language.md'"

# 7. Private dotfileslink.ps1 contains language.md symlink entry
assert_true "private dotfileslink.ps1 contains language.md" \
    "grep -q 'language.md' '$PRIVATE_DIR/install/win/dotfileslink.ps1'"

# 8. Private dotfileslink.sh contains language.md symlink entry
assert_true "private dotfileslink.sh contains language.md" \
    "grep -q 'language.md' '$PRIVATE_DIR/install/linux/dotfileslink.sh'"

# 9. Private install.ps1 has param block with all 4 switches
assert_true "private install.ps1 has -Base param" \
    "grep -q 'Base' '$PRIVATE_DIR/install.ps1'"

assert_true "private install.ps1 has -Develop param" \
    "grep -q 'Develop' '$PRIVATE_DIR/install.ps1'"

assert_true "private install.ps1 has -Toolchain param" \
    "grep -q 'Toolchain' '$PRIVATE_DIR/install.ps1'"

assert_true "private install.ps1 has -Full param" \
    "grep -q 'Full' '$PRIVATE_DIR/install.ps1'"

# 10. agents repo .gitignore excludes rules/language.md (private file)
assert_true "agents .gitignore contains rules/language.md entry" \
    "grep -qE '^rules/language\.md\$' '$AGENTS_DIR/.gitignore'"

# 11. linux installer defines AGENTS_RULES_DIR
assert_true "linux installer defines AGENTS_RULES_DIR" \
    "grep -q '^AGENTS_RULES_DIR=' '$PRIVATE_DIR/install/linux/dotfileslink.sh'"

# 12. linux installer references AGENTS_RULES_DIR in language.md links entry
assert_true "linux installer language.md entry references AGENTS_RULES_DIR" \
    "grep -q 'language.md:\$AGENTS_RULES_DIR/language.md' '$PRIVATE_DIR/install/linux/dotfileslink.sh'"

# 13. windows installer defines $AgentsDir
assert_true "windows installer defines \$AgentsDir" \
    "grep -q '^\\\$AgentsDir' '$PRIVATE_DIR/install/win/dotfileslink.ps1'"

# 14. windows installer language.md link Dest uses $AgentsDir
assert_true "windows installer language.md Dest uses \$AgentsDir" \
    "grep -qE 'language\\.md.*Dest *= *Join-Path *\\\$AgentsDir' '$PRIVATE_DIR/install/win/dotfileslink.ps1'"

echo ""
echo "=== Edge Cases ==="

# 10. install.ps1 has Test-Path guard around dotfiles-private call
assert_true "install.ps1 has Test-Path guard for dotfiles-private" \
    "grep -q 'Test-Path' '$DOTFILES_DIR/install.ps1'"

# 11. install.sh has -x test guard around dotfiles-private call
assert_true "install.sh has -x test guard for dotfiles-private" \
    "grep -q '\-x.*PRIVATE_INSTALLER' '$DOTFILES_DIR/install.sh'"

# 12. linux installer language.md entry does NOT use the old claude-global Dest path
assert_false "linux installer language.md entry does not use old claude-global/rules/language.md as Dest" \
    "grep -q ':.*claude-global/rules/language.md\$' '$PRIVATE_DIR/install/linux/dotfileslink.sh'"

# 13. windows installer language.md Dest does NOT use the old \$DotfilesDir variable
assert_false "windows installer language.md Dest does not reference \$DotfilesDir" \
    "grep -qE 'language\\.md.*Dest *= *Join-Path *\\\$DotfilesDir' '$PRIVATE_DIR/install/win/dotfileslink.ps1'"

# 14. Old public claude-global directory has been removed (split to agents repo)
assert_false "public claude-global directory does not exist" \
    "[ -d '$DOTFILES_DIR/claude-global' ]"

echo ""
echo "=== Smoke (post-install) ==="

# 15. ~/.claude/rules/language.md exists as a symlink resolving to dotfiles-private
LIVE_LINK="$HOME/.claude/rules/language.md"
if [ -L "$LIVE_LINK" ]; then
    echo "PASS: ~/.claude/rules/language.md exists as a symlink"
    pass=$((pass + 1))
    target=$(readlink "$LIVE_LINK")
    case "$target" in
        *dotfiles-private/claude-global/rules/language.md)
            echo "PASS: symlink resolves to dotfiles-private/claude-global/rules/language.md"
            pass=$((pass + 1))
            ;;
        *)
            echo "FAIL: symlink target is '$target' (expected ending with dotfiles-private/claude-global/rules/language.md)"
            fail=$((fail + 1))
            ;;
    esac
else
    echo "FAIL: ~/.claude/rules/language.md is not a symlink"
    fail=$((fail + 1))
    echo "FAIL: symlink resolves to dotfiles-private/claude-global/rules/language.md (skipped — link missing)"
    fail=$((fail + 1))
fi

echo ""
echo "=== Idempotency Cases ==="

# 12. .config/git/ignore has exactly 1 line matching language.md
count=$(grep -c 'language\.md' "$DOTFILES_DIR/.config/git/ignore" 2>/dev/null || true)
count=${count:-0}
count=$(echo "$count" | tr -d '[:space:]')
if [ "$count" -eq 1 ]; then
    echo "PASS: .config/git/ignore has exactly 1 language.md entry"
    pass=$((pass + 1))
else
    echo "FAIL: .config/git/ignore has $count language.md entries (expected 1)"
    fail=$((fail + 1))
fi

# 13. agents .gitignore has exactly 1 line matching rules/language.md
count=$(grep -c 'rules/language\.md' "$AGENTS_DIR/.gitignore" 2>/dev/null || true)
count=${count:-0}
count=$(echo "$count" | tr -d '[:space:]')
if [ "$count" -eq 1 ]; then
    echo "PASS: agents .gitignore has exactly 1 rules/language.md entry"
    pass=$((pass + 1))
else
    echo "FAIL: agents .gitignore has $count rules/language.md entries (expected 1)"
    fail=$((fail + 1))
fi

echo ""
echo "Results: $pass passed, $fail failed"
exit "$fail"
