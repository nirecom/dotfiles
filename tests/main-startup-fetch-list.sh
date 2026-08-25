#!/usr/bin/env bash
# Test: startup fetch list ~/.config/dotfiles/fetch-repos.txt — bash side behavior
# Tests: .profile_common, install/linux/uninstall-obsolete.sh
# Tags: git-fetch, fetch-repos, fixture, pwsh-not-required, scope:common
# Entry resolution and the legacy-name migration are extracted VERBATIM from the
# shipped scripts and run against temp fixtures, so implementation drift fails here.
set -uo pipefail

# TL2 gap: no real login shell launches the fetches, and uninstall-obsolete.sh is
# not run end to end. Mitigated at WORKFLOW_USER_VERIFIED preflight via
# bin/check-verification-gate.sh category: installer.

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PROFILE_COMMON="$DOTFILES_DIR/.profile_common"
UNINSTALL_SH="$DOTFILES_DIR/install/linux/uninstall-obsolete.sh"

PASS=0
FAIL=0
FAIL_NAMES=()

assert_eq() {
    local name="$1" expected="$2" actual="$3"
    if [ "$expected" = "$actual" ]; then
        PASS=$((PASS + 1))
        echo "PASS: $name"
    else
        FAIL=$((FAIL + 1))
        FAIL_NAMES+=("$name")
        echo "FAIL: $name"
        echo "      expected: [$expected]"
        echo "      actual:   [$actual]"
    fi
}

assert_true() {
    local name="$1"
    shift
    if "$@"; then
        PASS=$((PASS + 1))
        echo "PASS: $name"
    else
        FAIL=$((FAIL + 1))
        FAIL_NAMES+=("$name")
        echo "FAIL: $name"
    fi
}

assert_false() {
    local name="$1"
    shift
    if "$@"; then
        FAIL=$((FAIL + 1))
        FAIL_NAMES+=("$name")
        echo "FAIL: $name"
    else
        PASS=$((PASS + 1))
        echo "PASS: $name"
    fi
}

TMPROOT="$(mktemp -d "${TMPDIR:-/tmp}/startup-fetch-list.XXXXXX")"
cleanup() { rm -rf "$TMPROOT"; }
trap cleanup EXIT

echo "=== startup fetch list (fetch-repos.txt) — bash ==="

# ---------------------------------------------------------------------------
# A. Entry resolution loop (verbatim from .profile_common)
# ---------------------------------------------------------------------------
echo ""
echo "--- A. Entry resolution: relative vs absolute, comments, missing .git ---"

# Pull the loop's decision lines straight out of the shipped profile so the
# harness cannot drift from the implementation.
RESOLVE_LINES="$(sed -n '/while IFS= read -r _xrepo/,/done < "\$_fetch_repos_file"/p' "$PROFILE_COMMON" \
    | grep -E 'case "\$_xrepo" in|\[ -d "\$_xrepo/\.git" \]')"

if [ -z "$RESOLVE_LINES" ]; then
    echo "FAIL: could not extract entry-resolution lines from .profile_common"
    exit 1
fi

HARNESS="$TMPROOT/resolve.sh"
{
    echo '#!/usr/bin/env bash'
    echo '_fetch_repos_file="$1"; _repo_root="$2"'
    echo 'while IFS= read -r _xrepo || [ -n "$_xrepo" ]; do'
    echo "$RESOLVE_LINES"
    echo '    echo "$_xrepo"'
    echo 'done < "$_fetch_repos_file"'
} > "$HARNESS"
chmod +x "$HARNESS"

REPO_ROOT_FIX="$TMPROOT/git"
mkdir -p "$REPO_ROOT_FIX/repo-a/.git" "$REPO_ROOT_FIX/repo-b/.git" "$REPO_ROOT_FIX/not-a-repo"
ABS_REPO="$TMPROOT/elsewhere/extra-repo"
mkdir -p "$ABS_REPO/.git"

LIST="$TMPROOT/fetch-repos.txt"
cat > "$LIST" <<EOF
# a comment line

repo-a
not-a-repo
missing-repo
$ABS_REPO
EOF

OUT="$(bash "$HARNESS" "$LIST" "$REPO_ROOT_FIX")"

assert_eq "A1. relative entry resolves against the repo-holding directory" \
    "$REPO_ROOT_FIX/repo-a" "$(echo "$OUT" | sed -n 1p)"
assert_eq "A2. absolute entry is used as-is" \
    "$ABS_REPO" "$(echo "$OUT" | sed -n 2p)"
assert_eq "A3. comments, blanks, non-repos and missing entries are skipped" \
    "2" "$(echo "$OUT" | grep -c .)"

# Windows drive-letter forms count as absolute on bash (Git Bash / MSYS).
# Same extraction minus the .git guard, so drive-letter paths that do not exist
# on this host still surface their resolved form.
WIN_LIST="$TMPROOT/fetch-repos-win.txt"
WIN_HARNESS="$TMPROOT/resolve-noguard.sh"
{
    echo '#!/usr/bin/env bash'
    echo '_fetch_repos_file="$1"; _repo_root="$2"'
    echo 'while IFS= read -r _xrepo || [ -n "$_xrepo" ]; do'
    echo "$RESOLVE_LINES" | grep -v '\.git'
    echo '    echo "$_xrepo"'
    echo 'done < "$_fetch_repos_file"'
} > "$WIN_HARNESS"
printf 'C:/git/repo-a\nC:\\git\\repo-b\n' > "$WIN_LIST"
WIN_OUT="$(bash "$WIN_HARNESS" "$WIN_LIST" "$REPO_ROOT_FIX")"
assert_eq "A4. Windows drive-letter forward-slash entry treated as absolute" \
    'C:/git/repo-a' "$(echo "$WIN_OUT" | sed -n 1p)"
assert_eq "A5. Windows drive-letter backslash entry treated as absolute" \
    'C:\git\repo-b' "$(echo "$WIN_OUT" | sed -n 2p)"

# Final line without a trailing newline must still be processed.
NONL_LIST="$TMPROOT/fetch-repos-nonl.txt"
printf 'repo-b' > "$NONL_LIST"
NONL_OUT="$(bash "$HARNESS" "$NONL_LIST" "$REPO_ROOT_FIX")"
assert_eq "A6. last line without trailing newline is still read" \
    "$REPO_ROOT_FIX/repo-b" "$NONL_OUT"

# ---------------------------------------------------------------------------
# B. Legacy fetch-repos -> fetch-repos.txt migration (verbatim from uninstall-obsolete.sh)
# ---------------------------------------------------------------------------
echo ""
echo "--- B. One-time migration of the legacy list name ---"

MIGRATE_BLOCK="$(sed -n '/BEGIN temporary: fetch-repos/,/END temporary: fetch-repos/p' "$UNINSTALL_SH")"
if [ -z "$MIGRATE_BLOCK" ]; then
    echo "FAIL: could not extract fetch-repos migration block from uninstall-obsolete.sh"
    exit 1
fi

MIGRATE_SH="$TMPROOT/migrate.sh"
{
    echo '#!/usr/bin/env bash'
    echo "$MIGRATE_BLOCK"
} > "$MIGRATE_SH"
chmod +x "$MIGRATE_SH"

# Each case gets its own fake HOME so nothing touches the real ~/.config/dotfiles.
new_home() {
    local h="$TMPROOT/home-$1"
    mkdir -p "$h/.config/dotfiles"
    echo "$h"
}

# B1: legacy present, new absent -> renamed, content preserved
H="$(new_home b1)"
printf 'repo-a\n' > "$H/.config/dotfiles/fetch-repos"
env HOME="$H" bash "$MIGRATE_SH" >/dev/null
assert_false "B1a. legacy fetch-repos is gone after migration" test -e "$H/.config/dotfiles/fetch-repos"
assert_true  "B1b. fetch-repos.txt exists after migration" test -f "$H/.config/dotfiles/fetch-repos.txt"
assert_eq    "B1c. migration preserves the list content" \
    "repo-a" "$(cat "$H/.config/dotfiles/fetch-repos.txt")"

# B2: both present -> new name wins, legacy left alone
H="$(new_home b2)"
printf 'legacy\n' > "$H/.config/dotfiles/fetch-repos"
printf 'current\n' > "$H/.config/dotfiles/fetch-repos.txt"
env HOME="$H" bash "$MIGRATE_SH" >/dev/null
assert_eq "B2a. existing fetch-repos.txt is not overwritten" \
    "current" "$(cat "$H/.config/dotfiles/fetch-repos.txt")"
assert_true "B2b. legacy file is left untouched when the new name exists" \
    test -f "$H/.config/dotfiles/fetch-repos"

# B3: new name is a symlink (installer-provided) -> no rename
H="$(new_home b3)"
printf 'legacy\n' > "$H/.config/dotfiles/fetch-repos"
printf 'linked\n' > "$TMPROOT/linked-list.txt"
ln -s "$TMPROOT/linked-list.txt" "$H/.config/dotfiles/fetch-repos.txt"
env HOME="$H" bash "$MIGRATE_SH" >/dev/null
# -L is not asserted: without developer mode, MSYS `ln -s` copies instead of linking.
assert_true "B3a. installer-provided fetch-repos.txt survives the migration" \
    test -e "$H/.config/dotfiles/fetch-repos.txt"
assert_true "B3c. legacy file is left in place next to the installer-provided one" \
    test -f "$H/.config/dotfiles/fetch-repos"
assert_eq "B3b. symlink still resolves to the installer-provided list" \
    "linked" "$(cat "$H/.config/dotfiles/fetch-repos.txt")"

# B4: neither present -> silent no-op
H="$(new_home b4)"
env HOME="$H" bash "$MIGRATE_SH" >/dev/null 2>&1
assert_eq "B4a. migration exits 0 when there is nothing to migrate" "0" "$?"
assert_false "B4b. migration does not create fetch-repos.txt out of nothing" \
    test -e "$H/.config/dotfiles/fetch-repos.txt"

# B5: idempotent - running twice changes nothing further
H="$(new_home b5)"
printf 'repo-a\n' > "$H/.config/dotfiles/fetch-repos"
env HOME="$H" bash "$MIGRATE_SH" >/dev/null
env HOME="$H" bash "$MIGRATE_SH" >/dev/null
assert_eq "B5a. second run leaves the migrated list intact" \
    "repo-a" "$(cat "$H/.config/dotfiles/fetch-repos.txt")"

echo ""
echo "=== Results ==="
echo "PASS: $PASS  FAIL: $FAIL"
if [ "$FAIL" -gt 0 ]; then
    for n in "${FAIL_NAMES[@]}"; do echo "  failed: $n"; done
    exit 1
fi
echo "All tests passed."
