# Startup merge / diverged-notice output must stay off stdout (issue #338).
# Sourced by tests/fix-profile-common-startup-fetch.sh, which owns pass/fail/assert_eq
# and PROFILE; full-pipeline-integration.sh owns full_pipeline_probe/read_pipeline_record.
# Tests: .profile_common
# Tags: git-fetch, stdout-pollution, integration, hermetic, mutation, pwsh-not-required, scope:common

echo ""
echo "--- Integration (#338): merge diffstat goes to stderr, never stdout ---"

read_pipeline_record "$(full_pipeline_probe unset 1 mingw absent merge-stdout)"
assert_eq "merge338/merge-stdout-silent" "empty" "${stdout_field#stdout=}"
assert_eq "merge338/merge-stdout-exits-cleanly" "0" "${rc_field#rc=}"
# Catches the wrong order `2>/dev/null 1>&2`, which silences stdout by discarding it.
assert_eq "merge338/merge-text-on-stderr" "has-merge-text" "${mergetext_field#mergetext=}"
assert_eq "merge338/merge-stdout-tail-runs" "-R" "${post_field#post=}"
assert_eq "merge338/merge-stdout-all-fetches-reached" "3" "${tot_field#tot=}"
# One merge per repo (dotfiles/extra/agents): the stderr text above could come from just one of them.
assert_eq "merge338/merge-stdout-each-repo-merged-once" "1/1/1" "${merges_field#merges=}"

# Idempotency: a second sourcing in the same shell must not reopen the leak.
read_pipeline_record "$(full_pipeline_probe unset 2 mingw absent merge-stdout)"
assert_eq "merge338/merge-stdout-double-source-silent" "empty" "${stdout_field#stdout=}"
assert_eq "merge338/merge-stdout-double-source-exits-cleanly" "0" "${rc_field#rc=}"

echo ""
echo "--- Integration (#338): diverged notice (no-auto-reset marker) goes to stderr ---"

read_pipeline_record "$(full_pipeline_probe unset 1 mingw absent diverged)"
assert_eq "merge338/diverged-stdout-silent" "empty" "${stdout_field#stdout=}"
assert_eq "merge338/diverged-exits-cleanly" "0" "${rc_field#rc=}"
assert_eq "merge338/diverged-warn-on-stderr" "has-warn" "${warn_field#warn=}"
assert_eq "merge338/diverged-tail-runs" "-R" "${post_field#post=}"

echo ""
echo "--- Integration (#338): diverged without marker + non-TTY stdin skips the prompt ---"

# Other verdict of the L312 branch: no marker, stdin closed => no WARNING, no prompt, no hang.
read_pipeline_record "$(full_pipeline_probe unset 1 mingw absent diverged-no-marker)"
assert_eq "merge338/diverged-no-marker-stdout-silent" "empty" "${stdout_field#stdout=}"
assert_eq "merge338/diverged-no-marker-no-warn" "no-warn" "${warn_field#warn=}"
assert_eq "merge338/diverged-no-marker-exits-cleanly" "0" "${rc_field#rc=}"
assert_eq "merge338/diverged-no-marker-tail-runs" "-R" "${post_field#post=}"

echo ""
echo "--- Integration (#338): control — a silent merge leaves both streams free of merge text ---"

read_pipeline_record "$(full_pipeline_probe unset 1 mingw absent quiet)"
assert_eq "merge338/merge-quiet-control-stdout" "empty" "${stdout_field#stdout=}"
assert_eq "merge338/merge-quiet-control-no-merge-text" "no-merge-text" "${mergetext_field#mergetext=}"
assert_eq "merge338/merge-quiet-control-no-warn" "no-warn" "${warn_field#warn=}"

echo ""
echo "--- Integration (#338): the same holds when zsh sources the profile ---"

# The profile is sourced by zsh too (ZSH_VERSION branch). Skipped, not failed, where zsh is absent.
if command -v zsh >/dev/null 2>&1; then
    read_pipeline_record "$(full_pipeline_probe unset 1 mingw absent merge-stdout zsh)"
    assert_eq "merge338/zsh-merge-stdout-silent" "empty" "${stdout_field#stdout=}"
    assert_eq "merge338/zsh-merge-text-on-stderr" "has-merge-text" "${mergetext_field#mergetext=}"
    assert_eq "merge338/zsh-merge-exits-cleanly" "0" "${rc_field#rc=}"
    read_pipeline_record "$(full_pipeline_probe unset 1 mingw absent diverged zsh)"
    assert_eq "merge338/zsh-diverged-stdout-silent" "empty" "${stdout_field#stdout=}"
    assert_eq "merge338/zsh-diverged-warn-on-stderr" "has-warn" "${warn_field#warn=}"
else
    echo "SKIP: merge338/zsh-* (zsh not installed)"
fi

echo ""
echo "--- Static (#338): redirect shape of every merge / diverged-notice line ---"

MERGE338_MERGE_RE='merge --ff-only FETCH_HEAD'
MERGE338_NOTICE_RE='diverged from origin|Reset to origin/main|Skipped\. Run manually|reset --hard origin/main'

# Non-comment lines of $1 matching ERE $2, as "lineno:text".
merge338_code_lines() {
    grep -nE -- "$2" "$1" | grep -vE '^[0-9]+:[[:space:]]*#' || true
}

merge338_count() {
    local lines="$1"
    if [ -z "$lines" ]; then echo 0; else printf '%s\n' "$lines" | wc -l | tr -d ' '; fi
}

# S1 predicate (shared by the real file and the mutants): "pass" or "fail:<reason>".
merge338_s1_verdict() {
    local lines n bad
    lines=$(merge338_code_lines "$1" "$MERGE338_MERGE_RE")
    n=$(merge338_count "$lines")
    [ "$n" = "3" ] || { echo "fail:count=$n"; return; }
    bad=$(printf '%s\n' "$lines" | grep -F '2>/dev/null 1>&2' | cut -d: -f1 | tr '\n' ',' || true)
    [ -z "$bad" ] || { echo "fail:wrong-order:$bad"; return; }
    bad=$(printf '%s\n' "$lines" | grep -vF '1>&2 2>/dev/null' | cut -d: -f1 | tr '\n' ',' || true)
    [ -z "$bad" ] || { echo "fail:missing-stdout-to-stderr:$bad"; return; }
    echo "pass"
}

# S2 predicate: exactly the 5 notice/reset lines (L313/315/316/319/321), each ending in `>&2`.
merge338_s2_verdict() {
    local lines n bad
    lines=$(merge338_code_lines "$1" "$MERGE338_NOTICE_RE")
    n=$(merge338_count "$lines")
    [ "$n" = "5" ] || { echo "fail:count=$n"; return; }
    bad=$(printf '%s\n' "$lines" | grep -vE '>&2[[:space:]]*$' | cut -d: -f1 | tr '\n' ',' || true)
    [ -z "$bad" ] || { echo "fail:not-to-stderr:$bad"; return; }
    echo "pass"
}

assert_eq "merge338/S1-merge-line-count" "3" \
    "$(merge338_count "$(merge338_code_lines "$PROFILE" "$MERGE338_MERGE_RE")")"
assert_eq "merge338/S1-merge-redirect-order" "pass" "$(merge338_s1_verdict "$PROFILE")"
assert_eq "merge338/S2-notice-line-count" "5" \
    "$(merge338_count "$(merge338_code_lines "$PROFILE" "$MERGE338_NOTICE_RE")")"
assert_eq "merge338/S2-notice-lines-to-stderr" "pass" "$(merge338_s2_verdict "$PROFILE")"

echo ""
echo "--- Mutation (#338): S1/S2 predicates accept a reference fix and reject its mutants ---"

# The reference fix is synthesized from PROFILE (idempotent on an already-fixed file),
# so the predicates are exercised in both directions regardless of implementation state.
merge338_mutation_rows() {
    local tmp ref
    tmp=$(mktemp -d); ref="$tmp/ref"
    sed 's#merge --ff-only FETCH_HEAD 2>/dev/null#merge --ff-only FETCH_HEAD 1>\&2 2>/dev/null#' "$PROFILE" |
        MERGE338_RE="$MERGE338_NOTICE_RE" awk '$0 ~ ENVIRON["MERGE338_RE"] && $0 !~ /^[[:space:]]*#/ && $0 !~ />&2[[:space:]]*$/ { print $0 " >&2"; next } { print }' \
        > "$ref"
    awk '/merge --ff-only FETCH_HEAD/ { n++; if (n == 3) sub(/1>&2 2>\/dev\/null/, "2>/dev/null 1>\\&2") } { print }' \
        "$ref" > "$tmp/swap-one"
    awk '/merge --ff-only FETCH_HEAD/ { n++; if (n == 2) sub(/1>&2 /, "") } { print }' \
        "$ref" > "$tmp/drop-one"
    awk '/Reset to origin\/main\?/ { sub(/[[:space:]]*>&2[[:space:]]*$/, "") } { print }' \
        "$ref" > "$tmp/l316-stdout"
    printf 'ref-s1=%s|ref-s2=%s|swap=%s|drop=%s|l316=%s\n' \
        "$(merge338_s1_verdict "$ref")" "$(merge338_s2_verdict "$ref")" \
        "$(merge338_s1_verdict "$tmp/swap-one")" "$(merge338_s1_verdict "$tmp/drop-one")" \
        "$(merge338_s2_verdict "$tmp/l316-stdout")"
    rm -rf "$tmp"
}

IFS='|' read -r m338_ref_s1 m338_ref_s2 m338_swap m338_drop m338_l316 <<EOF_M338
$(merge338_mutation_rows)
EOF_M338
assert_eq "merge338/mutation-reference-fix-passes-S1" "pass" "${m338_ref_s1#ref-s1=}"
assert_eq "merge338/mutation-reference-fix-passes-S2" "pass" "${m338_ref_s2#ref-s2=}"
assert_eq "merge338/mutation-swapped-order-rejected" "fail:wrong-order" "$(printf '%s' "${m338_swap#swap=}" | cut -d: -f1-2)"
assert_eq "merge338/mutation-dropped-stdout-redirect-rejected" "fail:missing-stdout-to-stderr" "$(printf '%s' "${m338_drop#drop=}" | cut -d: -f1-2)"
assert_eq "merge338/mutation-l316-prompt-on-stdout-rejected" "fail:not-to-stderr" "$(printf '%s' "${m338_l316#l316=}" | cut -d: -f1-2)"
