##
function h-codex-notify {
    local info="$1"

    # ec "${info}" | jq . >> ~/logs/codex_notifs|| true
    #: These might leak private data, so only enable it if you need it for debugging.

    bell-codex
}
##
function codex-ask {
    local model="${codex_model:-gpt-5.2}" reasoning_effort="${codex_reasoning_effort:-high}"
    local inargs prompt out opts=()
    in-or-args2 "$@" @RET
    prompt="${inargs[*]}"

    out="$(gmktemp --suffix=.md)" @TRET

    if test -n "${model}" ; then
        opts+=(--model="${model}")
    fi

    (
        assert cdm ~/tmp/codex_ask @RET
        assert silent git init @RET

        ec "${prompt}" | revaldbg codex -c model_reasoning_effort="${reasoning_effort}" -c model_reasoning_summary="detailed" -c tools.web_search="true" --search --ask-for-approval on-failure --sandbox read-only exec "${opts}" --output-last-message="${out}" -
    ) @RET

    if isTty ; then
        ecgray $'\n'"$0: saved last message to: ${out}"
        cat "${out}" | pbcopy
    fi
}

function codex-ask-low {
    codex_reasoning_effort=low codex-ask "$@"
}

function codex-ask-med {
    codex_reasoning_effort=medium codex-ask "$@"
}
##
function codex-m {
    memoi_expire=$(( 3600*24*1 )) reval-memoi codex-install
    #: run codex-install every once in a while
    ##
    ensure-array codex_security_opts
    local security_opts=( "${codex_security_opts[@]}" )
    if (( ${#security_opts[@]} == 0 )) ; then
        security_opts=(--ask-for-approval on-failure --sandbox workspace-write)
    fi

    tty-title "⚡${PWD:t}"

    reval-ec codex "${security_opts[@]}" -c model_reasoning_effort="high" -c model_reasoning_summary="detailed" -c tools.web_search="true" --search "$@"
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
