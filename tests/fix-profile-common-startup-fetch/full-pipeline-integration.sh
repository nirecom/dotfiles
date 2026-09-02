# Full-pipeline integration coverage for issues #333 and #335.
# Sourced by tests/fix-profile-common-startup-fetch.sh, which owns
# pass/fail/assert_eq and PROFILE.
# Tests: .profile_common
# Tags: git-fetch, ssh, mingw, pgrep-guard, claudecode, integration, hermetic, pwsh-not-required, scope:common

echo ""
echo "--- Integration (round 3): mingw + pgrep-absent reaches all 3 fetch subprocesses ---"

# `grep -Fc` prints "0" AND exits 1 when nothing matches, so the usual
# `... || echo 0` idiom emits TWO lines and corrupts the `|`-joined record.
# The zero-fetch #335 rows below depend on that count, hence this helper.
count_matches() {
    local pattern="$1" file="$2" n=0
    if [ -f "$file" ]; then
        n=$(grep -Fc -- "$pattern" "$file" 2>/dev/null) || n=0
    fi
    printf '%s\n' "$n"
}

# Sources the REAL .profile_common end-to-end and proves the fetches actually
# reach (or, for #335, never reach) a fake `git`. Unlike guard-behaviour.sh this
# evaluates no extracted string: a guard that exists but is never wired to gate
# the trigger `if` (dead code, or placed after it) still fetches here and fails.
# The child's stdout is redirected to a file, so `[ -t 1 ]` is genuinely false
# inside it: the non-TTY half of the #335 guard is real here, not simulated. The
# TTY half has no real-pipeline counterpart in this harness — see the "round-6
# C1" note at the end of this file and the dispatcher's `# TL3 gap` block.
full_pipeline_probe() {
    #   $1 cc_mode      — "unset" (default) or the CLAUDECODE value to export
    #   $2 source_count — times to source the profile in ONE child shell (default 1)
    #   $3 osdist       — what detectos.sh reports (default "mingw"); round-6 C3
    #                     needs a non-mingw run so a guard nested inside the mingw
    #                     branch cannot pass vacuously.
    #   $4 pgrep_mode   — "absent" (default) or "other-pid". On non-mingw the
    #                     trigger only fires when pgrep reports a not-this-shell
    #                     PID, so "absent" there means "no fetch for pre-#335
    #                     reasons"; "other-pid" makes that pipeline want to fetch.
    local cc_mode="${1-unset}" source_count="${2-1}"
    local osdist="${3-mingw}" pgrep_mode="${4-absent}"
    local fx home dotfiles agents xrepo bindir rc=0
    fx=$(mktemp -d)
    home="$fx/home"; dotfiles="$fx/dotfiles"; agents="$fx/agents"
    xrepo="$fx/xrepo"; bindir="$fx/bin"
    mkdir -p "$home/.config/dotfiles" "$dotfiles/bin" "$agents/.git" "$xrepo/.git" "$bindir"

    printf 'OSDIST=%s\nISWSL=false\nISM1=false\n' "$osdist" > "$dotfiles/bin/detectos.sh"
    printf '%s\n' "$xrepo" > "$home/.config/dotfiles/fetch-repos.txt"

    # Fake git: records every invocation's argv, never touches the network.
    {
        printf '#!/bin/bash\n'
        printf 'printf "ARGV:%%s\\n" "$*" >> "$GIT_RECORD_FILE"\n'
        printf 'exit 0\n'
    } > "$bindir/git"
    chmod +x "$bindir/git"

    # 999999 is provably not the child shell's PID, so the trigger's
    # `[ $$ != "$PID" ]` half is true and the non-mingw path wants to fetch.
    if [ "$pgrep_mode" = "other-pid" ]; then
        {
            printf '#!/bin/bash\n'
            printf 'printf "999999\\n"\n'
        } > "$bindir/pgrep"
        chmod +x "$bindir/pgrep"
    fi

    (
        set +eu
        aws() { return 1; }
        export -f aws
        # Fake pgrep-absence with a `type` wrapper instead of pruning PATH directories
        # (round-4 C2): pruning whole PATH dirs risks taking bash/dirname/cygpath down
        # with pgrep if they happen to share a directory. `builtin type "$@"` still
        # answers every other lookup (git, ssh, cygpath, ...) from the real PATH; only
        # the "pgrep" name is made to look absent — and only in "absent" mode, so the
        # "other-pid" fixture lets the real lookup find the fake pgrep written above.
        _PGREP_MODE="$pgrep_mode"
        export _PGREP_MODE
        type() {
            [ "$_PGREP_MODE" = "absent" ] && [ "$1" = "pgrep" ] && return 1
            builtin type "$@"
        }
        export -f type
        # The suite itself runs inside a Claude Code Bash tool, so CLAUDECODE is
        # already exported here: the #333 rows must clear it explicitly or they
        # would silently become #335 rows once the guard lands.
        if [ "$cc_mode" = "unset" ]; then
            unset CLAUDECODE
        else
            export CLAUDECODE="$cc_mode"
        fi
        # The suite's own shell already sourced a .profile_common, so LESS is
        # exported here. Left inherited it would make the round-6 C2 sentinel
        # below read "-R" even when the child never reached the tail of the file.
        unset LESS
        export HOME="$home"
        export DOTFILES_DIR="$dotfiles"
        export GIT_RECORD_FILE="$fx/record.txt"
        export PATH="$bindir:$PATH"
        # $3 is the post-guard sentinel file (round-6 C2). `LESS` is exported at
        # column 0 well AFTER the fetch-trigger `if ... fi` closes, so a guard
        # written as a bare `return` (drops the whole rest of the file) or `exit`
        # (kills the shell) leaves it unset/absent here even though every
        # fetch-count assertion would still read a satisfying zero.
        bash -c 'i=0; while [ "$i" -lt "$2" ]; do . "$1"; i=$((i + 1)); done; printf "%s\n" "${LESS-<unset>}" > "$3"' \
            _ "$PROFILE" "$source_count" "$fx/post.txt" \
            >"$fx/stdout.txt" 2>"$fx/stderr.txt"
    )
    rc=$?
    printf 'df=%s|xr=%s|ag=%s|tot=%s|stdout=%s|stderr=%s|errsize=%s|post=%s|rc=%s\n' \
        "$(count_matches "-C $dotfiles fetch" "$fx/record.txt")" \
        "$(count_matches "-C $xrepo fetch" "$fx/record.txt")" \
        "$(count_matches "-C $agents fetch" "$fx/record.txt")" \
        "$(count_matches " fetch" "$fx/record.txt")" \
        "$([ -s "$fx/stdout.txt" ] && echo nonempty || echo empty)" \
        "$(grep -q 'git fetch' "$fx/stderr.txt" 2>/dev/null && echo has-progress || echo no-progress)" \
        "$([ -s "$fx/stderr.txt" ] && echo nonempty || echo empty)" \
        "$([ -f "$fx/post.txt" ] && head -n 1 "$fx/post.txt" || echo '<missing>')" \
        "$rc"
    rm -rf "$fx"
}

# Splits one `|`-joined probe record into its `name=value` fields.
read_pipeline_record() {
    IFS='|' read -r df_field xr_field ag_field tot_field stdout_field stderr_field \
        errsize_field post_field rc_field <<EOF_RECORD
$1
EOF_RECORD
}

read_pipeline_record "$(full_pipeline_probe unset 1)"

assert_eq "integration/dotfiles-fetch-reached" "1" "${df_field#df=}"
assert_eq "integration/extra-repo-fetch-reached" "1" "${xr_field#xr=}"
assert_eq "integration/agents-fetch-reached" "1" "${ag_field#ag=}"
assert_eq "integration/stdout-quiet" "empty" "${stdout_field#stdout=}"
assert_eq "integration/stderr-has-progress" "has-progress" "${stderr_field#stderr=}"
assert_eq "integration/sourced-child-exits-cleanly" "0" "${rc_field#rc=}"

# Baseline for the round-6 C2 sentinel: on the ordinary fetch path the tail of
# .profile_common demonstrably still runs, so `post=-R` below means "reached the
# end", not "this fixture always reports -R".
assert_eq "integration/post-guard-tail-runs-on-fetch-path" "-R" "${post_field#post=}"

echo ""
echo "--- Integration (#335): real end-to-end CLAUDECODE + non-TTY must not fetch at all ---"

# Paired with the control row above, which shares every input except CLAUDECODE:
# the control proves fetch DOES fire in this harness, so a zero here can only mean
# the guard skipped — never that the harness is vacuously fetch-free.
read_pipeline_record "$(full_pipeline_probe 1 1)"

assert_eq "integration/cc-non-tty-no-fetch-at-all" "0" "${tot_field#tot=}"
assert_eq "integration/cc-non-tty-no-dotfiles-fetch" "0" "${df_field#df=}"
assert_eq "integration/cc-non-tty-no-extra-repo-fetch" "0" "${xr_field#xr=}"
assert_eq "integration/cc-non-tty-no-agents-fetch" "0" "${ag_field#ag=}"
assert_eq "integration/cc-non-tty-stdout-quiet" "empty" "${stdout_field#stdout=}"
assert_eq "integration/cc-non-tty-exits-cleanly" "0" "${rc_field#rc=}"

# Round-6 C2: skipping the FETCH must not skip the rest of the file. A guard
# written as `return`/`exit` rather than a plain skip satisfies every count above
# while silently dropping the LESS/LESSOPEN/nvm/tmux tail for every Claude Code
# shell — this assertion is what tells the two implementations apart.
assert_eq "integration/cc-non-tty-post-guard-tail-still-runs" "-R" "${post_field#post=}"

# Round-6 C5: nothing may reach stderr on the skip path. The fetch path
# legitimately prints "git fetch ..." progress there, so an empty stderr is only
# meaningful once the skip is taken — a stray diagnostic (a `set -x` leftover, a
# `cygpath` complaint, an unguarded probe) would surface here and nowhere else.
assert_eq "integration/cc-non-tty-stderr-silent" "empty" "${errsize_field#errsize=}"

# Control (round-5 C1): identical fixture, CLAUDECODE cleared. Its non-zero count
# is what makes the zeros above meaningful.
read_pipeline_record "$(full_pipeline_probe unset 1)"
assert_eq "integration/control-no-claudecode-does-fetch" "3" "${tot_field#tot=}"

echo ""
echo "--- Integration (#335): sourcing twice in one Claude Code session stays fetch-free ---"

# Idempotency (round-5 C3): a second sourcing in the SAME shell must not leak
# state (a cleared PID, an unset marker) that re-opens the fetch path.
read_pipeline_record "$(full_pipeline_probe 1 2)"

assert_eq "integration/cc-double-source-no-fetch-at-all" "0" "${tot_field#tot=}"
assert_eq "integration/cc-double-source-stdout-quiet" "empty" "${stdout_field#stdout=}"
assert_eq "integration/cc-double-source-exits-cleanly" "0" "${rc_field#rc=}"

# Double-source control: without CLAUDECODE the same two sourcings DO fetch twice
# over, proving the idempotency row is not zero merely because the second sourcing
# is a no-op for unrelated reasons.
read_pipeline_record "$(full_pipeline_probe unset 2)"
assert_eq "integration/control-double-source-does-fetch" "6" "${tot_field#tot=}"

echo ""
echo "--- Integration (#335, round-6 C3): the skip must hold on a NON-mingw OSDIST too ---"

# Every real-pipeline case above forces OSDIST=mingw. A guard placed inside the
# `if [ "$OSDIST" = "mingw" ]` arm of the PID block — rather than ahead of the
# whole trigger `if` — would satisfy all of them while staying dead code for the
# Linux/macOS/WSL shells Claude Code equally runs in. The blocks below use the
# identical fixture with OSDIST=linux and a pgrep reporting a not-this-shell PID.
# Harness control first: same OSDIST, CLAUDECODE cleared, fetch must happen.
read_pipeline_record "$(full_pipeline_probe unset 1 linux other-pid)"
assert_eq "integration/control-linux-other-pid-does-fetch" "3" "${tot_field#tot=}"
assert_eq "integration/control-linux-post-guard-tail-runs" "-R" "${post_field#post=}"

read_pipeline_record "$(full_pipeline_probe 1 1 linux other-pid)"
assert_eq "integration/cc-linux-non-tty-no-fetch-at-all" "0" "${tot_field#tot=}"
assert_eq "integration/cc-linux-non-tty-no-dotfiles-fetch" "0" "${df_field#df=}"
assert_eq "integration/cc-linux-non-tty-post-guard-tail-still-runs" "-R" "${post_field#post=}"
assert_eq "integration/cc-linux-non-tty-stderr-silent" "empty" "${errsize_field#errsize=}"

echo ""
echo "--- Integration: harness self-check on a naturally fetch-free path ---"

# Attribution aid for the `stderr-silent` and `post-guard-tail` rows above.
# OSDIST=linux with pgrep genuinely absent leaves PID empty, so the trigger is
# false for pre-#335 reasons and .profile_common already takes a no-fetch path
# TODAY. These rows must therefore PASS right now: were any of them to fail, an
# empty stderr / a `-R` sentinel on a skip path would be unreachable for harness
# reasons and the #335 rows could not be read as "feature not implemented".
read_pipeline_record "$(full_pipeline_probe unset 1 linux absent)"
assert_eq "selfcheck/no-pgrep-linux-does-not-fetch" "0" "${tot_field#tot=}"
assert_eq "selfcheck/no-pgrep-linux-stderr-silent" "empty" "${errsize_field#errsize=}"
assert_eq "selfcheck/no-pgrep-linux-post-guard-tail-runs" "-R" "${post_field#post=}"
assert_eq "selfcheck/no-pgrep-linux-stdout-quiet" "empty" "${stdout_field#stdout=}"
assert_eq "selfcheck/no-pgrep-linux-exits-cleanly" "0" "${rc_field#rc=}"

# Round-6 C1 — real-TTY complement: NOT constructible here, deliberately.
# "CLAUDECODE set AND stdout IS a terminal must still fetch" needs a real pty.
# This environment was probed and has none: `script`/`expect`/`socat` are absent
# from the Git Bash PATH; `winpty` refuses without a tty on ITS stdin and the
# runner has none; `/dev/ptmx`, `/dev/ttyS0`, `/dev/ttyS3` and `/dev/windows` all
# yield `[ -t 1 ]` false (ptmx also blocks once its unread buffer fills); and
# `/dev/tty` cannot be opened at all. The TTY half is therefore asserted only
# symbolically in guard-behaviour.sh (`tty-with-claudecode-set-fetch`,
# `cc-value-metachar-tty-fetch`), with the residue in the dispatcher TL3 block.
