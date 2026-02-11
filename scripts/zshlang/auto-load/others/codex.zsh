##
function h-codex-notify {
    local info="$1"

    # ec "${info}" | jq . >> ~/logs/codex_notifs|| true
    #: These might leak private data, so only enable it if you need it for debugging.

    bell-codex
}
##
function codex-ask {
    local model="${codex_model:-gpt-5.2}" reasoning_effort="${codex_reasoning_effort:-high}" color="${codex_color}"
    local inargs prompt out opts=()
    in-or-args2 "$@" @RET
    prompt="${inargs[*]}"

    typeset -x DISABLE_BRISH=y
    #: To forcefully disable [agfi:bell-codex]

    out="$(gmktemp --suffix=.md)" @TRET

    if test -n "${model}" ; then
        opts+=(--model="${model}")
    fi
    if test -n "${color}" ; then
        opts+=(--color="${color}")
        #: always, never, auto
    fi

    (
        assert cdm ~/tmp/codex_ask @RET
        assert silent git init @RET

        ec "${prompt}" | revaldbg codex -c model_reasoning_effort="${reasoning_effort}" -c model_reasoning_summary="detailed" --search --ask-for-approval on-failure --sandbox read-only exec "${opts}" --output-last-message="${out}" -
    ) >&2 @RET

    if isTty ; then
        ecgray $'\n'"$0: saved last message to: ${out}"
    fi

    cat "${out}" |
        cat-copy-if-tty
}

function codex-ask-low {
    codex_reasoning_effort=low codex-ask "$@"
}

function codex-ask-med {
    codex_reasoning_effort=medium codex-ask "$@"
}
##
#: @duplicateCode/5a56c1bf4c428167af08cffefb52d3aa
function codex {
    tty-title "⚡${PWD:t}"
    command codex "$@"
}

function codex-m {
    memoi_expire=$(( 3600*24*1 )) reval-memoi codex-install
    #: run codex-install every once in a while
    ##
    ensure-array codex_security_opts
    local security_opts=( "${codex_security_opts[@]}" )
    if (( ${#security_opts[@]} == 0 )) ; then
        security_opts=(--ask-for-approval on-failure --sandbox workspace-write)
    fi

    # -c model_reasoning_effort="high"
    reval-ec codex "${security_opts[@]}" -c model_reasoning_summary="detailed" -c web_search="true" --search "$@"
    # -c model_verbosity="high"
    #
    # --ask-for-approval:
    # - untrusted: Only run "trusted" commands (e.g. ls, cat, sed) without  asking for user approval. Will escalate to the user if the model proposes  a command that is not in the "trusted" set
    # - on-failure: Run all commands without asking for user approval. Only asks  for approval if a command fails to execute, in which case it will  escalate to the user to ask for un-sandboxed execution
    # - on-request: The model decides when to ask the user for approval
    # - never: Never ask for user approval Execution failures are immediately returned to the model
}

function codex-yolo {
    codex_security_opts=(--dangerously-bypass-approvals-and-sandbox) codex-m "$@"
}
##
function codex-install {
    reval-ecgray npm-install '@openai/codex'
}
##
function codex-clean-text {
    in-or-args "$@" |
        perl -CSD -pe 's/\x{258C}//g' |
        cat-copy-if-tty
}
##
function codex-status {
    local codex_status_timeout_s="${codex_status_timeout_s:-20}"
    local codex_status_profile="${codex_status_profile:-}"
    local codex_status_cd="${codex_status_cd:-$HOME/tmp}"
    local codex_status_strip_ansi_p="${codex_status_strip_ansi_p:-n}"

    ensure-array codex_status_args
    local codex_args=("${codex_status_args[@]}")

    if ! command -v -- codex_status.py >/dev/null 2>&1 ; then
        ecerr "codex-status: codex_status.py not found in PATH"
        return 127
    fi
    local script_args=(--timeout "${codex_status_timeout_s}")
    if test -n "${codex_status_profile}" ; then
        script_args+=(--profile "${codex_status_profile}")
    fi
    if test -n "${codex_status_cd}" ; then
        script_args+=(--cd "${codex_status_cd}")
    fi
    if bool "${codex_status_strip_ansi_p}" ; then
        script_args+=(--color never)
    fi

    local arg
    for arg in "${codex_args[@]}" ; do
        script_args+=(--codex-arg "${arg}")
    done

    command codex_status.py "${script_args[@]}" "$@"
}
##
