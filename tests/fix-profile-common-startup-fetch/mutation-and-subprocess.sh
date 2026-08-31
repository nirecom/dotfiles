# Mutation + subprocess coverage for issue #333 codex round-2 findings C1/C2.
# Sourced by tests/fix-profile-common-startup-fetch.sh, which owns
# pass/fail/assert_eq, PROFILE, SRC_LINE, SSH_CLEANUP_LINE, change1_order_verdict().
# Tests: .profile_common
# Tags: git-fetch, ssh, mingw, source-order, mutation, subprocess, pwsh-not-required, scope:common

# ---------------------------------------------------------------------------
# C1a — mutation fixture: a column-0 source line still trapped inside the
# SSH-setup block's elif branch must be REJECTED, not accepted.
# ---------------------------------------------------------------------------
echo ""
echo "--- Mutation (C1): column-0 source line still trapped inside the SSH elif branch ---"

mutation_case_c1() {
    local tmp verdict
    tmp=$(mktemp -d)
    # Build the mutant: drop the source line from its real position and
    # reinsert an unindented (column-0) copy right after 'unset _ssh_keys',
    # i.e. still textually inside the elif branch, before that branch's own
    # closing `fi`. This is the exact placement codex flagged as passing the
    # old (unset _ssh_keys)-only check while never actually reaching mingw.
    awk -v src="$SRC_LINE" '
        NR==src { next }
        /^[[:space:]]*unset _ssh_keys$/ {
            print
            print "[ -f \"$_agents_dir/profile-snippet.sh\" ] && . \"$_agents_dir/profile-snippet.sh\""
            next
        }
        { print }
    ' "$PROFILE" > "$tmp/mutant.profile_common"
    verdict=$(change1_order_verdict "$tmp/mutant.profile_common")
    rm -rf "$tmp"
    printf '%s\n' "$verdict"
}

MUTANT_VERDICT=$(mutation_case_c1)
if [ "$MUTANT_VERDICT" != "pass" ]; then
    pass "Mutation C1: strengthened Change-1 check correctly rejects the column-0-but-still-inside-elif mutant ($MUTANT_VERDICT)"
else
    fail "Mutation C1: Change-1 check incorrectly PASSES the column-0-but-still-inside-elif mutant — known blind spot reopened"
fi

# ---------------------------------------------------------------------------
# C1b — dynamic child-shell probe: source the REAL .profile_common with
# OSDIST=mingw forced, and prove the snippet executes AFTER SSH setup by
# checking whether the Windows OpenSSH PATH entry SSH setup prepends is
# already visible to the snippet at the moment it sources.
# ---------------------------------------------------------------------------
echo ""
echo "--- Dynamic (C1): profile-snippet.sh executes AFTER SSH setup completes (OSDIST=mingw) ---"

dynamic_order_probe() {
    local fx home dotfiles agents winroot marker rc=0
    fx=$(mktemp -d)
    home="$fx/home"; dotfiles="$fx/dotfiles"; agents="$fx/agents"; winroot="$fx/winroot"
    mkdir -p "$home" "$dotfiles/bin" "$agents" "$winroot/System32/OpenSSH"
    : > "$winroot/System32/OpenSSH/ssh.exe"
    chmod +x "$winroot/System32/OpenSSH/ssh.exe"

    # Fixture detectos.sh: forces OSDIST=mingw deterministically, independent
    # of the real host's `uname`. Hermetic — never touches the sibling agents
    # repo or the real DOTFILES_DIR/detectos.sh.
    printf 'OSDIST=mingw\nISWSL=false\nISM1=false\n' > "$dotfiles/bin/detectos.sh"

    marker="$fx/order.marker"
    # Fixture profile-snippet.sh: records whether the Windows OpenSSH dir the
    # SSH-setup block prepends is already on PATH at source time. If SSH
    # setup ran first (post-fix), it is; if the snippet still runs at the
    # pre-fix top-of-file position, it is not.
    {
        printf 'case ":$PATH:" in\n'
        printf '    *":$_C1_WIN_OPENSSH:"*) printf "openssh-in-path\\n" > "$_C1_MARKER" ;;\n'
        printf '    *) printf "openssh-not-in-path\\n" > "$_C1_MARKER" ;;\n'
        printf 'esac\n'
    } > "$agents/profile-snippet.sh"

    (
        set +eu
        aws() { return 1; }  # neutralize a real host AWS CLI if present on PATH
        export -f aws
        export HOME="$home"
        export DOTFILES_DIR="$dotfiles"
        SYSTEMROOT="$(cygpath -w "$winroot" 2>/dev/null || printf '%s' "$winroot")"
        export SYSTEMROOT
        _C1_WIN_OPENSSH="$(cygpath -u "$SYSTEMROOT" 2>/dev/null)/System32/OpenSSH"
        export _C1_WIN_OPENSSH
        export _C1_MARKER="$marker"
        bash -c 'source "$1"' _ "$PROFILE" >"$fx/stdout.txt" 2>"$fx/stderr.txt"
    )
    rc=$?
    if [ -f "$marker" ]; then
        cat "$marker"
    else
        printf 'no-marker-written(rc=%s)\n' "$rc"
    fi
    rm -rf "$fx"
}

DYNAMIC_VERDICT=$(dynamic_order_probe)
assert_eq "dynamic/snippet-sources-after-ssh-setup" "openssh-in-path" "$DYNAMIC_VERDICT"

# ---------------------------------------------------------------------------
# C1c (codex round 3) — presence on PATH is not the same as winning PATH
# resolution. Seed a competing fake MSYS ssh ahead of Windows OpenSSH in the
# child's starting PATH and prove `command -v ssh` actually resolves to the
# OpenSSH stub after sourcing, not just that the OpenSSH dir is present
# somewhere in PATH.
# ---------------------------------------------------------------------------
echo ""
echo "--- Dynamic (round 3): Windows OpenSSH wins PATH resolution over a competing MSYS ssh ---"

ssh_path_resolution_probe() {
    local fx home dotfiles winroot msysbin marker rc=0
    fx=$(mktemp -d)
    home="$fx/home"; dotfiles="$fx/dotfiles"; winroot="$fx/winroot"; msysbin="$fx/msys_bin"
    mkdir -p "$home" "$dotfiles/bin" "$winroot/System32/OpenSSH" "$msysbin"
    printf 'OPENSSH_STUB\n' > "$winroot/System32/OpenSSH/ssh.exe"
    chmod +x "$winroot/System32/OpenSSH/ssh.exe"
    # Competing fake MSYS ssh, placed ahead of Windows OpenSSH in the child's
    # starting PATH — mirrors Git Bash's real /usr/bin/ssh already winning
    # over the machine-wide agent; the exact append-vs-prepend regression
    # this probe guards against.
    printf 'MSYS_STUB\n' > "$msysbin/ssh"
    chmod +x "$msysbin/ssh"

    printf 'OSDIST=mingw\nISWSL=false\nISM1=false\n' > "$dotfiles/bin/detectos.sh"

    marker="$fx/resolved.marker"
    (
        set +eu
        aws() { return 1; }
        export -f aws
        export HOME="$home"
        export DOTFILES_DIR="$dotfiles"
        SYSTEMROOT="$(cygpath -w "$winroot" 2>/dev/null || printf '%s' "$winroot")"
        export SYSTEMROOT
        export PATH="$msysbin:$PATH"
        bash -c 'source "$1"; cat "$(command -v ssh)" > "$2" 2>/dev/null' _ "$PROFILE" "$marker" >"$fx/stdout.txt" 2>"$fx/stderr.txt"
    )
    rc=$?
    if [ -f "$marker" ]; then
        case "$(cat "$marker")" in
            OPENSSH_STUB) printf 'openssh-wins\n' ;;
            MSYS_STUB) printf 'msys-wins\n' ;;
            *) printf 'other\n' ;;
        esac
    else
        printf 'no-marker(rc=%s)\n' "$rc"
    fi
    rm -rf "$fx"
}

RESOLUTION_VERDICT=$(ssh_path_resolution_probe)
assert_eq "dynamic/openssh-wins-path-resolution" "openssh-wins" "$RESOLUTION_VERDICT"

# ---------------------------------------------------------------------------
# C2 — real subprocess coverage for the three fetch invocations. A fake
# `git` on PATH records its argv/env instead of touching the network. Proves
# stdout/stderr separation and env-var propagation dynamically, not lexically.
# ---------------------------------------------------------------------------
echo ""
echo "--- Subprocess (C2): dotfiles/extra-repo/agents fetches through a real (fake) git child ---"

# Extracted straight out of .profile_common (CPR-SSOT — never hand-copied),
# the same way guard-behaviour.sh extracts the guard expression.
extract_fetch_block() {
    local echo_pat="$1" fetch_pat="$2" echo_line fetch_line
    echo_line=$(grep -n "$echo_pat" "$PROFILE" | head -n 1 | cut -d: -f1)
    fetch_line=$(grep -n "$fetch_pat" "$PROFILE" | head -n 1 | cut -d: -f1)
    if [ -z "$echo_line" ] || [ -z "$fetch_line" ] || [ "$fetch_line" -lt "$echo_line" ]; then
        return 1
    fi
    sed -n "${echo_line},${fetch_line}p" "$PROFILE"
}

# Runs one extracted echo+fetch block through a fake `git`, real subprocess,
# real stdout/stderr separation — not a lexical grep.
run_fetch_block_subprocess() {
    local block="$1" var_name="$2" var_value="$3" fx bindir
    fx=$(mktemp -d)
    bindir="$fx/bin"
    mkdir -p "$bindir"
    {
        printf '#!/bin/bash\n'
        printf '{\n'
        printf '    printf "ARGV:%%s\\n" "$*"\n'
        printf '    printf "GIT_TERMINAL_PROMPT=%%s\\n" "${GIT_TERMINAL_PROMPT-<unset>}"\n'
        printf '    printf "GIT_SSH_COMMAND=%%s\\n" "${GIT_SSH_COMMAND-<unset>}"\n'
        printf '} >> "$GIT_RECORD_FILE"\n'
        printf 'exit 0\n'
    } > "$bindir/git"
    chmod +x "$bindir/git"
    (
        set +eu
        export PATH="$bindir:$PATH"
        export GIT_RECORD_FILE="$fx/record.txt"
        declare -x "$var_name=$var_value"
        _tc=""
        eval "$block"
        wait "$!" 2>/dev/null
    ) >"$fx/stdout.txt" 2>"$fx/stderr.txt"
    printf '%s|%s|%s|%s\n' \
        "$([ -s "$fx/stdout.txt" ] && echo nonempty || echo empty)" \
        "$(grep -q 'git fetch' "$fx/stderr.txt" 2>/dev/null && echo has-progress || echo no-progress)" \
        "$(grep -c 'GIT_TERMINAL_PROMPT=0' "$fx/record.txt" 2>/dev/null || echo 0)" \
        "$(grep -c 'GIT_SSH_COMMAND=ssh -o BatchMode=yes' "$fx/record.txt" 2>/dev/null || echo 0)"
    rm -rf "$fx"
}

DF_REPO=$(mktemp -d); git init -q "$DF_REPO" >/dev/null 2>&1; git -C "$DF_REPO" config core.hooksPath /dev/null
XR_REPO=$(mktemp -d); git init -q "$XR_REPO" >/dev/null 2>&1; git -C "$XR_REPO" config core.hooksPath /dev/null
AG_REPO=$(mktemp -d); git init -q "$AG_REPO" >/dev/null 2>&1; git -C "$AG_REPO" config core.hooksPath /dev/null

DF_BLOCK=$(extract_fetch_block 'echo "git fetch \$DOTFILES_DIR' 'git -C "\$DOTFILES_DIR" fetch') || DF_BLOCK=""
XR_BLOCK=$(extract_fetch_block 'echo "git fetch \$_xrepo' 'git -C "\$_xrepo" fetch') || XR_BLOCK=""
AG_BLOCK=$(extract_fetch_block 'echo "git fetch agents' 'git -C "\$_agents_dir" fetch') || AG_BLOCK=""

check_fetch_subprocess() {
    local label="$1" block="$2" var_name="$3" var_value="$4" result
    if [ -z "$block" ]; then
        fail "subprocess/$label: could not extract the echo+fetch block from .profile_common"
        return
    fi
    result=$(run_fetch_block_subprocess "$block" "$var_name" "$var_value")
    IFS='|' read -r stdout_state stderr_state prompt_count batchmode_count <<EOF_RESULT
$result
EOF_RESULT
    assert_eq "subprocess/$label-stdout-quiet" "empty" "$stdout_state"
    assert_eq "subprocess/$label-stderr-has-progress" "has-progress" "$stderr_state"
    assert_eq "subprocess/$label-terminal-prompt-disabled" "1" "$prompt_count"
    assert_eq "subprocess/$label-ssh-batchmode" "1" "$batchmode_count"
}

check_fetch_subprocess "dotfiles" "$DF_BLOCK" DOTFILES_DIR "$DF_REPO"
check_fetch_subprocess "extra-repo" "$XR_BLOCK" _xrepo "$XR_REPO"
check_fetch_subprocess "agents" "$AG_BLOCK" _agents_dir "$AG_REPO"

rm -rf "$DF_REPO" "$XR_REPO" "$AG_REPO"
