#!/bin/bash
# tests/main-aws-profile.sh
# Narrow integration tests for _aws_select_profile bash function
# The function is defined inline (spec-driven, TDD) — does not source .profile_common
# Run: bash tests/main-aws-profile.sh

PASS=0
FAIL=0
errors_list=""

pass() { echo "PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "FAIL: $1"; FAIL=$((FAIL + 1)); errors_list="${errors_list}\n  - $1"; }

# ---------------------------------------------------------------------------
# Helper: run a test in a clean subshell with the snippet sourced.
# Usage: run_snippet <AWS_WORK_DIR> <PWD_OVERRIDE> [extra_env]
# Echoes: "AWS_PROFILE=<val> AWS_DEFAULT_REGION=<val_or_UNSET>"
# ---------------------------------------------------------------------------
_run_snippet() {
    local work_dir="$1"
    local test_pwd="$2"

    bash --norc --noprofile - <<EOF
# Minimal env
OSDIST=ubuntu
DOTFILES_DIR=/tmp/dummy
AWS_WORK_DIR="$work_dir"
export OSDIST DOTFILES_DIR AWS_WORK_DIR

# Mock aws command
aws() {
    case "\$*" in
        "configure get region --profile work")    echo "ap-northeast-1" ;;
        "configure get region --profile personal") echo "us-east-1" ;;
        *) return 1 ;;
    esac
}

# Paste the snippet inline (from spec)
if type aws >/dev/null 2>&1 && [ "\$OSDIST" != "qnap" ] && [ -n "\${AWS_WORK_DIR:-}" ]; then
    _aws_select_profile() {
        local _profile _work_dir
        _work_dir="\${AWS_WORK_DIR%/}"
        if [ "\$PWD" = "\$_work_dir" ] || case "\$PWD" in "\$_work_dir"/*) true;; *) false;; esac; then
            _profile="work"
        else
            _profile="personal"
        fi
        if [ "\${AWS_PROFILE:-}" != "\$_profile" ]; then
            export AWS_PROFILE="\$_profile"
            local _region
            _region=\$(aws configure get region --profile "\$_profile" 2>/dev/null)
            if [ -n "\$_region" ]; then
                export AWS_DEFAULT_REGION="\$_region"
            else
                unset AWS_DEFAULT_REGION
            fi
        fi
    }
fi

# Change to test PWD via a subshell trick using cd
cd "$test_pwd" 2>/dev/null || true
_aws_select_profile

echo "AWS_PROFILE=\${AWS_PROFILE:-UNSET}"
echo "AWS_DEFAULT_REGION=\${AWS_DEFAULT_REGION:-UNSET}"
EOF
}

# ---------------------------------------------------------------------------
# Helper: run snippet in env where AWS_WORK_DIR is unset
# ---------------------------------------------------------------------------
_run_snippet_no_workdir() {
    bash --norc --noprofile - <<'EOF'
OSDIST=ubuntu
DOTFILES_DIR=/tmp/dummy
unset AWS_WORK_DIR

aws() { echo "us-east-1"; }

if type aws >/dev/null 2>&1 && [ "$OSDIST" != "qnap" ] && [ -n "${AWS_WORK_DIR:-}" ]; then
    _aws_select_profile() { echo "DEFINED"; }
fi

# Check if defined
if type _aws_select_profile >/dev/null 2>&1; then
    echo "FUNCTION_DEFINED"
else
    echo "FUNCTION_NOT_DEFINED"
fi
echo "AWS_PROFILE=${AWS_PROFILE:-UNSET}"
EOF
}

# ---------------------------------------------------------------------------
# Setup: temp dirs for tests
# ---------------------------------------------------------------------------
TMPBASE=$(mktemp -d /tmp/aws-test-XXXXXX)
WORK_DIR="$TMPBASE/work"
WORK_SUBDIR="$TMPBASE/work/project/subdir"
OTHER_DIR="$TMPBASE/other"
WORK_OTHER="$TMPBASE/workother"  # false prefix

mkdir -p "$WORK_DIR" "$WORK_SUBDIR" "$OTHER_DIR" "$WORK_OTHER"

echo "=== AWS profile auto-switch: bash narrow integration tests ==="
echo ""

# ---------------------------------------------------------------------------
# Normal cases
# ---------------------------------------------------------------------------
echo "--- Normal cases ---"

echo "=== N1: PWD = work_dir (exact) → AWS_PROFILE=work, AWS_DEFAULT_REGION=ap-northeast-1 ==="
out=$(_run_snippet "$WORK_DIR" "$WORK_DIR")
profile=$(echo "$out" | grep 'AWS_PROFILE=' | cut -d= -f2)
region=$(echo "$out" | grep 'AWS_DEFAULT_REGION=' | cut -d= -f2)
if [ "$profile" = "work" ]; then pass "N1: profile=work"; else fail "N1: profile should be work, got '$profile'"; fi
if [ "$region" = "ap-northeast-1" ]; then pass "N1: region=ap-northeast-1"; else fail "N1: region should be ap-northeast-1, got '$region'"; fi

echo ""
echo "=== N2: PWD = work_dir subdir → AWS_PROFILE=work ==="
out=$(_run_snippet "$WORK_DIR" "$WORK_SUBDIR")
profile=$(echo "$out" | grep 'AWS_PROFILE=' | cut -d= -f2)
if [ "$profile" = "work" ]; then pass "N2: subdir → profile=work"; else fail "N2: subdir should give profile=work, got '$profile'"; fi

echo ""
echo "=== N3: PWD outside work_dir → AWS_PROFILE=personal, AWS_DEFAULT_REGION=us-east-1 ==="
out=$(_run_snippet "$WORK_DIR" "$OTHER_DIR")
profile=$(echo "$out" | grep 'AWS_PROFILE=' | cut -d= -f2)
region=$(echo "$out" | grep 'AWS_DEFAULT_REGION=' | cut -d= -f2)
if [ "$profile" = "personal" ]; then pass "N3: outside → profile=personal"; else fail "N3: outside should give profile=personal, got '$profile'"; fi
if [ "$region" = "us-east-1" ]; then pass "N3: outside → region=us-east-1"; else fail "N3: outside should give region=us-east-1, got '$region'"; fi

echo ""
echo "=== N4: switch work→personal → AWS_DEFAULT_REGION updates to personal region ==="
out=$(bash --norc --noprofile - <<ENDSCRIPT
OSDIST=ubuntu
AWS_WORK_DIR="$WORK_DIR"
export OSDIST AWS_WORK_DIR

aws() {
    case "\$*" in
        "configure get region --profile work")    echo "ap-northeast-1" ;;
        "configure get region --profile personal") echo "us-east-1" ;;
        *) return 1 ;;
    esac
}

if type aws >/dev/null 2>&1 && [ "\$OSDIST" != "qnap" ] && [ -n "\${AWS_WORK_DIR:-}" ]; then
    _aws_select_profile() {
        local _profile _work_dir
        _work_dir="\${AWS_WORK_DIR%/}"
        if [ "\$PWD" = "\$_work_dir" ] || case "\$PWD" in "\$_work_dir"/*) true;; *) false;; esac; then
            _profile="work"
        else
            _profile="personal"
        fi
        if [ "\${AWS_PROFILE:-}" != "\$_profile" ]; then
            export AWS_PROFILE="\$_profile"
            local _region
            _region=\$(aws configure get region --profile "\$_profile" 2>/dev/null)
            if [ -n "\$_region" ]; then
                export AWS_DEFAULT_REGION="\$_region"
            else
                unset AWS_DEFAULT_REGION
            fi
        fi
    }
fi

# First: go to work dir
cd "$WORK_DIR"
_aws_select_profile
# Now go to other dir (personal)
cd "$OTHER_DIR"
_aws_select_profile

echo "AWS_PROFILE=\${AWS_PROFILE:-UNSET}"
echo "AWS_DEFAULT_REGION=\${AWS_DEFAULT_REGION:-UNSET}"
ENDSCRIPT
)
profile=$(echo "$out" | grep 'AWS_PROFILE=' | cut -d= -f2)
region=$(echo "$out" | grep 'AWS_DEFAULT_REGION=' | cut -d= -f2)
if [ "$profile" = "personal" ]; then pass "N4: after switch → profile=personal"; else fail "N4: after switch should be personal, got '$profile'"; fi
if [ "$region" = "us-east-1" ]; then pass "N4: after switch → region=us-east-1"; else fail "N4: region should update to us-east-1, got '$region'"; fi

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------
echo ""
echo "--- Edge cases ---"

echo "=== E1: false prefix: work_dir=/tmp/work, PWD=/tmp/workother → personal ==="
out=$(_run_snippet "$WORK_DIR" "$WORK_OTHER")
profile=$(echo "$out" | grep 'AWS_PROFILE=' | cut -d= -f2)
if [ "$profile" = "personal" ]; then pass "E1: false prefix → profile=personal"; else fail "E1: false prefix should give personal, got '$profile'"; fi

echo ""
echo "=== E2: trailing slash in AWS_WORK_DIR (/tmp/work/) → same as /tmp/work ==="
out=$(_run_snippet "${WORK_DIR}/" "$WORK_DIR")
profile=$(echo "$out" | grep 'AWS_PROFILE=' | cut -d= -f2)
if [ "$profile" = "work" ]; then pass "E2: trailing slash → still matches work_dir"; else fail "E2: trailing slash in AWS_WORK_DIR should still match, got '$profile'"; fi

echo ""
echo "=== E3: region unavailable (aws returns empty) → AWS_DEFAULT_REGION unset ==="
out=$(bash --norc --noprofile - <<ENDSCRIPT
OSDIST=ubuntu
AWS_WORK_DIR="$WORK_DIR"
export OSDIST AWS_WORK_DIR
AWS_DEFAULT_REGION="old-region"
export AWS_DEFAULT_REGION

# Mock aws: region lookup returns empty
aws() { echo ""; }

if type aws >/dev/null 2>&1 && [ "\$OSDIST" != "qnap" ] && [ -n "\${AWS_WORK_DIR:-}" ]; then
    _aws_select_profile() {
        local _profile _work_dir
        _work_dir="\${AWS_WORK_DIR%/}"
        if [ "\$PWD" = "\$_work_dir" ] || case "\$PWD" in "\$_work_dir"/*) true;; *) false;; esac; then
            _profile="work"
        else
            _profile="personal"
        fi
        if [ "\${AWS_PROFILE:-}" != "\$_profile" ]; then
            export AWS_PROFILE="\$_profile"
            local _region
            _region=\$(aws configure get region --profile "\$_profile" 2>/dev/null)
            if [ -n "\$_region" ]; then
                export AWS_DEFAULT_REGION="\$_region"
            else
                unset AWS_DEFAULT_REGION
            fi
        fi
    }
fi

cd "$WORK_DIR"
_aws_select_profile
echo "AWS_DEFAULT_REGION=\${AWS_DEFAULT_REGION:-UNSET}"
ENDSCRIPT
)
region=$(echo "$out" | grep 'AWS_DEFAULT_REGION=' | cut -d= -f2)
if [ "$region" = "UNSET" ]; then pass "E3: empty region → AWS_DEFAULT_REGION unset"; else fail "E3: should unset AWS_DEFAULT_REGION when region empty, got '$region'"; fi

echo ""
echo "=== E4: same profile on repeated call → aws configure NOT called again ==="
# Use a temp file as a call counter (avoids subshell variable loss)
E4_COUNTER_FILE=$(mktemp /tmp/aws-e4-counter-XXXXXX)
echo "0" > "$E4_COUNTER_FILE"
out=$(bash --norc --noprofile - <<ENDSCRIPT
OSDIST=ubuntu
AWS_WORK_DIR="$WORK_DIR"
export OSDIST AWS_WORK_DIR
E4_COUNTER_FILE="$E4_COUNTER_FILE"

aws() {
    local n
    n=\$(cat "\$E4_COUNTER_FILE")
    echo \$((n + 1)) > "\$E4_COUNTER_FILE"
    echo "ap-northeast-1"
}

if type aws >/dev/null 2>&1 && [ "\$OSDIST" != "qnap" ] && [ -n "\${AWS_WORK_DIR:-}" ]; then
    _aws_select_profile() {
        local _profile _work_dir
        _work_dir="\${AWS_WORK_DIR%/}"
        if [ "\$PWD" = "\$_work_dir" ] || case "\$PWD" in "\$_work_dir"/*) true;; *) false;; esac; then
            _profile="work"
        else
            _profile="personal"
        fi
        if [ "\${AWS_PROFILE:-}" != "\$_profile" ]; then
            export AWS_PROFILE="\$_profile"
            local _region
            _region=\$(aws configure get region --profile "\$_profile" 2>/dev/null)
            if [ -n "\$_region" ]; then
                export AWS_DEFAULT_REGION="\$_region"
            else
                unset AWS_DEFAULT_REGION
            fi
        fi
    }
fi

cd "$WORK_DIR"
_aws_select_profile
_aws_select_profile
_aws_select_profile
ENDSCRIPT
)
count=$(cat "$E4_COUNTER_FILE")
rm -f "$E4_COUNTER_FILE"
if [ "$count" = "1" ]; then pass "E4: repeated same-profile call → aws called only once"; else fail "E4: aws should be called once (not $count times) when profile unchanged"; fi

# ---------------------------------------------------------------------------
# Error cases
# ---------------------------------------------------------------------------
echo ""
echo "--- Error cases ---"

echo "=== ER1: AWS_WORK_DIR unset → _aws_select_profile not defined, AWS_PROFILE unchanged ==="
out=$(_run_snippet_no_workdir)
func_status=$(echo "$out" | grep 'FUNCTION_' | head -1)
profile=$(echo "$out" | grep 'AWS_PROFILE=' | cut -d= -f2)
if [ "$func_status" = "FUNCTION_NOT_DEFINED" ]; then pass "ER1: AWS_WORK_DIR unset → function not defined"; else fail "ER1: function should not be defined when AWS_WORK_DIR unset, got '$func_status'"; fi
if [ "$profile" = "UNSET" ]; then pass "ER1: AWS_WORK_DIR unset → AWS_PROFILE unchanged (unset)"; else fail "ER1: AWS_PROFILE should remain unset, got '$profile'"; fi

# ---------------------------------------------------------------------------
# Idempotency cases
# ---------------------------------------------------------------------------
echo ""
echo "--- Idempotency cases ---"

echo "=== I1: source snippet block twice → PROMPT_COMMAND contains _aws_prompt_cmd exactly once ==="
out=$(bash --norc --noprofile - <<ENDSCRIPT
OSDIST=ubuntu
AWS_WORK_DIR="$WORK_DIR"
export OSDIST AWS_WORK_DIR
unset ZSH_VERSION

aws() {
    case "\$*" in
        "configure get region --profile work")    echo "ap-northeast-1" ;;
        "configure get region --profile personal") echo "us-east-1" ;;
        *) return 1 ;;
    esac
}

# Source the FULL snippet block (including PROMPT_COMMAND logic) twice
_source_block() {
    if type aws >/dev/null 2>&1 && [ "\$OSDIST" != "qnap" ] && [ -n "\${AWS_WORK_DIR:-}" ]; then
        _aws_select_profile() {
            local _profile _work_dir
            _work_dir="\${AWS_WORK_DIR%/}"
            if [ "\$PWD" = "\$_work_dir" ] || case "\$PWD" in "\$_work_dir"/*) true;; *) false;; esac; then
                _profile="work"
            else
                _profile="personal"
            fi
            if [ "\${AWS_PROFILE:-}" != "\$_profile" ]; then
                export AWS_PROFILE="\$_profile"
                local _region
                _region=\$(aws configure get region --profile "\$_profile" 2>/dev/null)
                if [ -n "\$_region" ]; then
                    export AWS_DEFAULT_REGION="\$_region"
                else
                    unset AWS_DEFAULT_REGION
                fi
            fi
        }
        _aws_select_profile
        if [ -n "\$ZSH_VERSION" ]; then
            autoload -Uz add-zsh-hook
            if ! (( \${chpwd_functions[(Ie)_aws_select_profile]} )); then
                add-zsh-hook chpwd _aws_select_profile
            fi
        else
            case "\$PROMPT_COMMAND" in
                *_aws_prompt_cmd*) ;;
                *) PROMPT_COMMAND="\${PROMPT_COMMAND:+\${PROMPT_COMMAND}; }_aws_prompt_cmd" ;;
            esac
            _aws_prompt_cmd() { _aws_select_profile; }
        fi
    fi
}

cd "$WORK_DIR"
_source_block
_source_block

# Count occurrences of _aws_prompt_cmd in PROMPT_COMMAND
count=\$(echo "\$PROMPT_COMMAND" | grep -o '_aws_prompt_cmd' | wc -l | tr -d ' ')
echo "PROMPT_CMD_COUNT=\$count"
echo "PROMPT_COMMAND=\$PROMPT_COMMAND"
ENDSCRIPT
)
count=$(echo "$out" | grep 'PROMPT_CMD_COUNT=' | cut -d= -f2)
if [ "$count" = "1" ]; then pass "I1: source twice → _aws_prompt_cmd appears exactly once in PROMPT_COMMAND"; else fail "I1: _aws_prompt_cmd should appear once (not $count) in PROMPT_COMMAND"; fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="

rm -rf "$TMPBASE"

[ "$FAIL" -eq 0 ] || exit 1
