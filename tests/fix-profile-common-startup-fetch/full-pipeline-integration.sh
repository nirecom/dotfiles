# Full-pipeline integration coverage for issue #333 codex round-3 finding C1.
# Sourced by tests/fix-profile-common-startup-fetch.sh, which owns
# pass/fail/assert_eq and PROFILE.
# Tests: .profile_common
# Tags: git-fetch, ssh, mingw, pgrep-guard, integration, hermetic, pwsh-not-required, scope:common

echo ""
echo "--- Integration (round 3): mingw + pgrep-absent reaches all 3 fetch subprocesses ---"

# Sources the REAL .profile_common end-to-end with OSDIST=mingw forced and
# pgrep genuinely absent from PATH, and proves all three fetches (dotfiles,
# extra-repo, agents) actually reach a fake `git`. Change 4 is not yet
# implemented, so mingw+pgrep-absent is expected to skip the fetch section
# entirely — the FAIL below IS the fail-before-fix evidence.
full_pipeline_probe() {
    local fx home dotfiles agents xrepo bindir rc=0
    fx=$(mktemp -d)
    home="$fx/home"; dotfiles="$fx/dotfiles"; agents="$fx/agents"
    xrepo="$fx/xrepo"; bindir="$fx/bin"
    mkdir -p "$home/.config/dotfiles" "$dotfiles/bin" "$agents/.git" "$xrepo/.git" "$bindir"

    printf 'OSDIST=mingw\nISWSL=false\nISM1=false\n' > "$dotfiles/bin/detectos.sh"
    printf '%s\n' "$xrepo" > "$home/.config/dotfiles/fetch-repos.txt"

    # Fake git: records every invocation's argv, never touches the network.
    {
        printf '#!/bin/bash\n'
        printf 'printf "ARGV:%%s\\n" "$*" >> "$GIT_RECORD_FILE"\n'
        printf 'exit 0\n'
    } > "$bindir/git"
    chmod +x "$bindir/git"

    (
        set +eu
        aws() { return 1; }
        export -f aws
        # Fake pgrep-absence with a `type` wrapper instead of pruning PATH directories
        # (round-4 C2): pruning whole PATH dirs risks taking bash/dirname/cygpath down
        # with pgrep if they happen to share a directory. `builtin type "$@"` still
        # answers every other lookup (git, ssh, cygpath, ...) from the real PATH; only
        # the "pgrep" name is made to look absent.
        type() {
            [ "$1" = "pgrep" ] && return 1
            builtin type "$@"
        }
        export -f type
        export HOME="$home"
        export DOTFILES_DIR="$dotfiles"
        export GIT_RECORD_FILE="$fx/record.txt"
        export PATH="$bindir:$PATH"
        bash -c 'source "$1"' _ "$PROFILE" >"$fx/stdout.txt" 2>"$fx/stderr.txt"
    )
    rc=$?
    printf 'df=%s|xr=%s|ag=%s|stdout=%s|stderr=%s|rc=%s\n' \
        "$(grep -Fc -- "-C $dotfiles fetch" "$fx/record.txt" 2>/dev/null || echo 0)" \
        "$(grep -Fc -- "-C $xrepo fetch" "$fx/record.txt" 2>/dev/null || echo 0)" \
        "$(grep -Fc -- "-C $agents fetch" "$fx/record.txt" 2>/dev/null || echo 0)" \
        "$([ -s "$fx/stdout.txt" ] && echo nonempty || echo empty)" \
        "$(grep -q 'git fetch' "$fx/stderr.txt" 2>/dev/null && echo has-progress || echo no-progress)" \
        "$rc"
    rm -rf "$fx"
}

PIPELINE_RESULT=$(full_pipeline_probe)
IFS='|' read -r df_field xr_field ag_field stdout_field stderr_field rc_field <<EOF_RESULT
$PIPELINE_RESULT
EOF_RESULT

assert_eq "integration/dotfiles-fetch-reached" "1" "${df_field#df=}"
assert_eq "integration/extra-repo-fetch-reached" "1" "${xr_field#xr=}"
assert_eq "integration/agents-fetch-reached" "1" "${ag_field#ag=}"
assert_eq "integration/stdout-quiet" "empty" "${stdout_field#stdout=}"
assert_eq "integration/stderr-has-progress" "has-progress" "${stderr_field#stderr=}"
assert_eq "integration/sourced-child-exits-cleanly" "0" "${rc_field#rc=}"
