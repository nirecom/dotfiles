# Static / structural assertions for issue #333. Sourced by
# tests/fix-profile-common-startup-fetch.sh, which owns pass/fail/assert_eq,
# PROFILE, GUARD_REGION, SRC_LINE and SSH_CLEANUP_LINE.
# Tests: .profile_common
# Tags: git-fetch, ssh, mingw, stdout-pollution, pgrep-guard, source-order, pwsh-not-required, scope:common

# ---------------------------------------------------------------------------
# Change 1 — the agents profile-snippet source block runs AFTER the SSH setup
# ---------------------------------------------------------------------------
echo ""
echo "--- Normal: Change 1 — profile-snippet.sh is sourced after the SSH-setup block ---"

if [ -z "$SRC_LINE" ] || [ -z "$SSH_CLEANUP_LINE" ]; then
    fail "Change 1: could not locate the source line and/or the 'unset _ssh_keys' anchor"
else
    # change1_order_verdict (dispatcher) checks both column 0 AND position after the
    # SSH-setup block's own closing `fi` — not just after `unset _ssh_keys`. A line
    # placed at column 0 but still between `unset _ssh_keys` and that `fi` remains
    # trapped inside the elif branch (codex round-2 C1); mutation-and-subprocess.sh
    # proves this predicate actually rejects that mutant.
    VERDICT=$(change1_order_verdict "$PROFILE")
    case "$VERDICT" in
        pass)
            pass "Change 1: profile-snippet.sh is sourced at top level AFTER the whole SSH-setup block (line $SRC_LINE)"
            ;;
        fail:indented)
            fail "Change 1: source line is indented (line $SRC_LINE) — it must stay at column 0"
            ;;
        fail:before-block-end)
            fail "Change 1: source line (line $SRC_LINE) is not after the SSH-setup block's own closing 'fi' (still inside the if/elif)"
            ;;
        *)
            fail "Change 1: order check could not be evaluated ($VERDICT)"
            ;;
    esac
fi

# The move must not drop the existence guard: without it every shell on a machine that has
# no agents sibling repo would print an error at startup.
if grep -qE '\[ -f "\$_agents_dir/profile-snippet\.sh" \][[:space:]]*&&[[:space:]]*\. "\$_agents_dir/profile-snippet\.sh"' "$PROFILE"; then
    pass "Change 1: the source is still guarded by [ -f ... ] on the same line"
else
    fail "Change 1: the profile-snippet.sh source lost its [ -f ... ] existence guard"
fi

# ---------------------------------------------------------------------------
# Change 3 — progress echoes go to stderr, not stdout
# ---------------------------------------------------------------------------
echo ""
echo "--- Normal: Change 3 — fetch progress messages are written to stderr ---"

if grep -qE 'echo "git fetch \$DOTFILES_DIR[^"]*"[[:space:]]+>&2' "$PROFILE"; then
    pass "Change 3: dotfiles progress echo redirects to stderr (>&2)"
else
    fail "Change 3: dotfiles progress echo still pollutes stdout (missing >&2)"
fi

if grep -qE 'echo "git fetch \$_xrepo[^"]*"[[:space:]]+>&2' "$PROFILE"; then
    pass "Change 3: extra-repos progress echo redirects to stderr (>&2)"
else
    fail "Change 3: extra-repos progress echo still pollutes stdout (missing >&2)"
fi

if grep -qE 'echo "git fetch agents[^"]*"[[:space:]]+>&2' "$PROFILE"; then
    pass "Change 3: agents progress echo redirects to stderr (>&2)"
else
    fail "Change 3: agents progress echo still pollutes stdout (missing >&2)"
fi

# ---------------------------------------------------------------------------
# Change 4 — mingw skips the oldest-session dedup entirely (structure only;
# the behavioural matrix lives in guard-behaviour.sh)
# ---------------------------------------------------------------------------
echo ""
echo "--- Normal: Change 4 — Windows Git Bash (mingw) bypasses the PID dedup guard ---"

# Scoped to GUARD_REGION on purpose — see the comment on the extraction in the dispatcher.
if printf '%s\n' "$GUARD_REGION" | grep -qE '^[[:space:]]*if \[ "\$OSDIST" = "mingw" \]; then'; then
    pass "Change 4: fetch guard has an explicit \$OSDIST = mingw branch"
else
    fail "Change 4: fetch guard has no explicit \$OSDIST = mingw branch (matches elsewhere don't count)"
fi

if grep -qE 'if \{ \[ -n "\$PID" \].*\}[[:space:]]*\|\|[[:space:]]*\[ -n "\$\{ZSH_VERSION-\}" \][[:space:]]*\|\|[[:space:]]*\[ "\$OSDIST" = "mingw" \]' "$PROFILE"; then
    pass "Change 4: guard line combines PID condition, ZSH_VERSION fallback and the mingw clause"
else
    fail "Change 4: guard line does not append the mingw clause after the ZSH_VERSION fallback"
fi

if printf '%s\n' "$GUARD_REGION" | grep -q 'pgrep -fo bash'; then
    pass "Change 4 (non-regression): pgrep stays the first-choice PID source off mingw"
else
    fail "Change 4 (non-regression): pgrep PID detection was removed — non-mingw dedup is broken"
fi

# ---------------------------------------------------------------------------
# Change 2 — no-op: the BatchMode subshell guards survive the edit
# ---------------------------------------------------------------------------
echo ""
echo "--- Normal: Change 2 (no-op) — all three fetch subshells keep BatchMode=yes ---"

if grep -qE 'GIT_SSH_COMMAND=.ssh -o BatchMode=yes[^)]*git -C "\$DOTFILES_DIR" fetch' "$PROFILE"; then
    pass "Change 2: dotfiles fetch keeps GIT_SSH_COMMAND BatchMode=yes"
else
    fail "Change 2: dotfiles fetch lost its GIT_SSH_COMMAND BatchMode=yes guard"
fi

if grep -qE 'GIT_SSH_COMMAND=.ssh -o BatchMode=yes[^)]*git -C "\$_xrepo" fetch' "$PROFILE"; then
    pass "Change 2: extra-repos fetch keeps GIT_SSH_COMMAND BatchMode=yes"
else
    fail "Change 2: extra-repos fetch lost its GIT_SSH_COMMAND BatchMode=yes guard"
fi

if grep -qE 'GIT_SSH_COMMAND=.ssh -o BatchMode=yes[^)]*git -C "\$_agents_dir" fetch' "$PROFILE"; then
    pass "Change 2: agents fetch keeps GIT_SSH_COMMAND BatchMode=yes"
else
    fail "Change 2: agents fetch lost its GIT_SSH_COMMAND BatchMode=yes guard"
fi

# ---------------------------------------------------------------------------
# Error / rejected-design re-entry prevention
# ---------------------------------------------------------------------------
echo ""
echo "--- Error: rejected designs must never re-enter .profile_common ---"

# MSYS `ps` does not support -eo, so a ps-based PID fallback silently yields nothing.
if grep -qE 'ps[[:space:]]+-eo[[:space:]]+pid,comm' "$PROFILE"; then
    fail "Rejected design: 'ps -eo pid,comm' fallback reintroduced (MSYS ps lacks -eo)"
else
    pass "Rejected design absent: no 'ps -eo pid,comm' PID fallback"
fi

# Branching on core.sshCommand would drop the BatchMode=yes guarantee for the fetches.
if grep -qE 'git config([[:space:]]+[^[:space:]]+)*[[:space:]]+--get[[:space:]]+core\.sshCommand' "$PROFILE"; then
    fail "Rejected design: 'git config --get core.sshCommand' branch reintroduced"
else
    pass "Rejected design absent: no 'git config --get core.sshCommand' branch"
fi

# Partial Change 3: any progress echo on the fetch path left on stdout.
UNREDIRECTED=$(grep -n 'echo "git fetch' "$PROFILE" | grep -cv '>&2' || true)
if [ "${UNREDIRECTED:-0}" -eq 0 ]; then
    pass "Change 3 completeness: no fetch progress echo is left without >&2"
else
    fail "Change 3 completeness: $UNREDIRECTED fetch progress echo(es) still write to stdout"
fi

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------
echo ""
echo "--- Edge: syntax integrity and single occurrence of the moved block ---"

if bash -n "$PROFILE" 2>/dev/null; then
    pass "Edge: 'bash -n .profile_common' exits 0 (syntax intact after the new if/elif nesting)"
else
    fail "Edge: 'bash -n .profile_common' reported a syntax error"
fi

SRC_COUNT=$(grep -c '\. "\$_agents_dir/profile-snippet\.sh"' "$PROFILE" || true)
if [ "${SRC_COUNT:-0}" -eq 1 ]; then
    pass "Edge: profile-snippet.sh is sourced exactly once (no leftover pre-move copy)"
else
    fail "Edge: profile-snippet.sh source line appears ${SRC_COUNT:-0} time(s), expected exactly 1"
fi

# The moved block must keep its own `unset _agents_dir` trailer: it stops the temp var
# leaking into the git-fetch block (which redefines it) and normalises the block's exit
# status to 0 when the snippet is absent and the `&&` short-circuits.
UNSET_OK=1
if [ -n "$SRC_LINE" ]; then
    if sed -n "$((SRC_LINE + 1)),$((SRC_LINE + 3))p" "$PROFILE" | grep -qE '^[[:space:]]*unset _agents_dir[[:space:]]*$'; then
        UNSET_OK=0
    fi
fi
if [ "$UNSET_OK" -eq 0 ]; then
    pass "Edge: 'unset _agents_dir' trails the relocated source block"
else
    fail "Edge: 'unset _agents_dir' does not trail the relocated source block"
fi

# ---------------------------------------------------------------------------
# Classifier / guard cases — every fetch-guard verdict stays reachable
# ---------------------------------------------------------------------------
echo ""
echo "--- Classifier: every fetch-guard verdict stays reachable (structure) ---"

# non-mingw verdict: pgrep must be wired as the elif arm, not orphaned or deleted.
if printf '%s\n' "$GUARD_REGION" | grep -qE '^[[:space:]]*elif type pgrep[[:space:]]*>/dev/null 2>&1; then'; then
    pass "Classifier: non-mingw verdict reachable — pgrep is the elif arm after the mingw branch"
else
    fail "Classifier: non-mingw verdict unreachable — pgrep is not wired as an elif after mingw"
fi

# zsh verdict: the ZSH_VERSION fallback must survive on the guard line.
if printf '%s\n' "$GUARD_REGION" | grep -qE 'if \{ \[ -n "\$PID" \].*\}[[:space:]]*\|\|[[:space:]]*\[ -n "\$\{ZSH_VERSION-\}" \]'; then
    pass "Classifier: zsh verdict preserved — ZSH_VERSION fallback remains on the guard line"
else
    fail "Classifier: zsh verdict lost — ZSH_VERSION fallback missing from the guard line"
fi
