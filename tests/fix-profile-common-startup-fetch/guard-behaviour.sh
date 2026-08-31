# Behavioural (table-driven) matrices for issue #333. Sourced by
# tests/fix-profile-common-startup-fetch.sh, which owns pass/fail/assert_eq and PROFILE.
# Tests: .profile_common
# Tags: git-fetch, mingw, pgrep-guard, table-driven, classifier, pwsh-not-required, scope:common

# ---------------------------------------------------------------------------
# Extraction (CPR-SSOT): the expressions under test are pulled out of
# .profile_common at run time. Nothing here is a hand-copied duplicate, so the
# matrices cannot drift away from the source and keep passing against a stale copy.
# ---------------------------------------------------------------------------
echo ""
echo "--- Behavioural: extract the guard expression from .profile_common ---"

GUARD_LINE=$(printf '%s\n' "$GUARD_REGION" | grep -E '^[[:space:]]*if \{ \[ -n "\$PID" \]' | head -n 1 || true)
GUARD_EXPR=${GUARD_LINE#*if }        # drop indentation + the leading `if `
GUARD_EXPR=${GUARD_EXPR%%; then*}    # drop `; then` and any trailing comment

if [ -n "$GUARD_EXPR" ] && [ "$GUARD_EXPR" != "$GUARD_LINE" ]; then
    pass "Extraction: guard condition pulled from the source file: $GUARD_EXPR"
else
    fail "Extraction: could not isolate the guard condition from .profile_common"
fi

# `$$` is the running shell's PID and cannot be assigned, so it is rewritten to a test
# variable before eval. This case proves the rewrite has something to rewrite — otherwise
# a shape change would silently turn the substitution into a no-op and every row would
# then compare against the *test harness* PID instead of the guard's own.
case "$GUARD_EXPR" in
    *'$$'*) pass "Extraction: guard expression contains \$\$ (self-PID rewrite is meaningful)" ;;
    *) fail "Extraction: guard expression has no \$\$ — the self-PID rewrite would be a no-op" ;;
esac
GUARD_EVAL=${GUARD_EXPR//'$$'/'$_TEST_SELF_PID'}

# The PID-assignment chain: everything between the `if type git` line and the first
# `fi` at that nesting level.
PID_BLOCK=$(printf '%s\n' "$GUARD_REGION" | sed -n '2,/^    fi$/p')
if printf '%s\n' "$PID_BLOCK" | grep -q 'pgrep'; then
    pass "Extraction: PID-assignment block pulled from the source file"
else
    fail "Extraction: could not isolate the PID-assignment block from .profile_common"
fi

# Evaluate the extracted condition with every input pinned. `-` means empty/unset.
run_guard() {
    local osdist="$1" pid="$2" self="$3" zsh="$4"
    [ "$pid" = "-" ] && pid=""
    [ "$zsh" = "-" ] && zsh=""
    (
        OSDIST="$osdist"
        PID="$pid"
        _TEST_SELF_PID="$self"
        if [ -n "$zsh" ]; then ZSH_VERSION="$zsh"; else unset ZSH_VERSION 2>/dev/null || true; fi
        if eval "$GUARD_EVAL"; then echo fetch; else echo skip; fi
    )
}

# `type` and `pgrep` are stubbed so the branch taken depends only on the pinned inputs,
# never on the host that runs the test. `preset_pid` seeds PID with a stale, non-empty
# value BEFORE evaluation — a real interactive shell can carry a leftover PID in its
# environment across sourcing — so the matrix can prove a stale value is never silently
# reused instead of being (re)computed or cleared. A file-backed counter records how
# many times the fake `pgrep` is actually invoked, so "PID ended up empty" can be told
# apart from "pgrep never ran" (a mingw fix that still shells out to pgrep and merely
# discards the result would pass a PID-only check while missing the point of the fix).
run_pid_block() {
    local osdist="$1" has_pgrep="$2" preset_pid="${3-}"
    local counter pidfile
    counter=$(mktemp); pidfile="$counter.pid"
    printf '0\n' > "$counter"
    (
        OSDIST="$osdist"
        _HAS_PGREP="$has_pgrep"
        _PGREP_COUNTER="$counter"
        PID="$preset_pid"
        type() { [ "$1" = "pgrep" ] && [ "$_HAS_PGREP" = "yes" ]; }
        pgrep() {
            local c; c=$(cat "$_PGREP_COUNTER" 2>/dev/null || echo 0)
            printf '%s\n' "$((c + 1))" > "$_PGREP_COUNTER"
            printf '4242\n'
        }
        eval "$PID_BLOCK"
        printf '%s\n' "${PID:-<empty>}"
    ) > "$pidfile"
    printf 'pid=%s|calls=%s\n' "$(cat "$pidfile")" "$(cat "$counter")"
    rm -f "$counter" "$pidfile"
}

echo ""
echo "--- Behavioural: fetch-guard verdict matrix (every verdict, both directions) ---"

while IFS='|' read -r name osdist pid self zsh want; do
    name="${name//[[:space:]]/}"
    if [ -z "$name" ] || [ "${name#\#}" != "$name" ]; then continue; fi
    osdist="${osdist//[[:space:]]/}"; pid="${pid//[[:space:]]/}"
    self="${self//[[:space:]]/}"; zsh="${zsh//[[:space:]]/}"
    want="${want//[[:space:]]/}"
    got=$(run_guard "$osdist" "$pid" "$self" "$zsh")
    assert_eq "guard/$name" "$want" "$got"
done <<'TABLE'
mingw-always-fetch     | mingw | -     | 12345 | -   | fetch
non-oldest-bash        | linux | 99999 | 12345 | -   | fetch
oldest-bash-skips      | linux | 12345 | 12345 | -   | skip
zsh-always-fetch       | linux | -     | 12345 | 5.9 | fetch
pgrep-absent-non-mingw | linux | -     | 12345 | -   | skip
mingw-even-when-oldest | mingw | 12345 | 12345 | -   | fetch
TABLE

echo ""
echo "--- Behavioural: PID-assignment matrix (mingw must not consult pgrep at all) ---"

while IFS='|' read -r name osdist has_pgrep preset want_pid want_calls; do
    name="${name//[[:space:]]/}"
    if [ -z "$name" ] || [ "${name#\#}" != "$name" ]; then continue; fi
    osdist="${osdist//[[:space:]]/}"; has_pgrep="${has_pgrep//[[:space:]]/}"
    preset="${preset//[[:space:]]/}"; [ "$preset" = "-" ] && preset=""
    want_pid="${want_pid//[[:space:]]/}"; want_calls="${want_calls//[[:space:]]/}"
    got=$(run_pid_block "$osdist" "$has_pgrep" "$preset")
    got_pid="${got#pid=}"; got_pid="${got_pid%%|*}"
    got_calls="${got##*calls=}"
    assert_eq "pid/$name" "$want_pid" "$got_pid"
    assert_eq "pid-calls/$name" "$want_calls" "$got_calls"
done <<'TABLE'
mingw-never-runs-pgrep                   | mingw | yes | -     | <empty> | 0
non-mingw-uses-pgrep                     | linux | yes | -     | 4242    | 1
pgrep-missing-stays-empty                | linux | no  | -     | <empty> | 0
mingw-stale-pid-not-reused               | mingw | yes | 99999 | <empty> | 0
non-mingw-pgrep-absent-stale-pid-cleared | linux | no  | 99999 | <empty> | 0
TABLE

echo ""
echo "--- Behavioural: the relocated source block stays safe without the agents repo ---"

# Runs the extracted source statement against a throwaway _agents_dir. Nothing outside
# the temp dir is touched, and the real agents sibling repo is never consulted.
probe_source() {
    local mode="$1" dir errfile marker rc=0
    dir=$(mktemp -d)
    marker="$dir/sourced.marker"
    errfile="$dir/err.txt"
    if [ "$mode" = "present" ]; then
        printf ': > "%s"\n' "$marker" > "$dir/profile-snippet.sh"
    fi
    ( _agents_dir="$dir"; eval "$SRC_STMT" ) 2>"$errfile" || rc=$?
    if [ -s "$errfile" ]; then printf 'stderr-output\n'
    elif [ -f "$marker" ]; then printf 'sourced\n'
    else printf 'quiet-noop\n'
    fi
    rm -rf "$dir"
}

if [ -n "$SRC_LINE" ]; then
    SRC_STMT=$(sed -n "${SRC_LINE}p" "$PROFILE")
    while IFS='|' read -r name mode want; do
        name="${name//[[:space:]]/}"
        if [ -z "$name" ] || [ "${name#\#}" != "$name" ]; then continue; fi
        mode="${mode//[[:space:]]/}"; want="${want//[[:space:]]/}"
        got=$(probe_source "$mode")
        assert_eq "source/$name" "$want" "$got"
    done <<'TABLE'
snippet-absent-is-silent    | absent  | quiet-noop
snippet-present-is-sourced  | present | sourced
TABLE
else
    fail "source/probe: could not locate the profile-snippet.sh source statement"
fi
