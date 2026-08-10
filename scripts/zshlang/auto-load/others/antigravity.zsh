##
function antigravity {
    #: Google's Antigravity CLI.
    #:
    #: It ships as =@google/gemini-cli= and its binary is `gemini`; the
    #: Antigravity-specific state lives under `~/.gemini/antigravity-cli/`.
    #: A standalone `antigravity` binary is preferred if one ever appears, so
    #: this keeps working either way.
    #: @duplicateCode/fd706e6b8475e27ca5cf27951b1d8ddc
    ##
    local cmd=("${antigravity_cmd[@]}")
    if (( ${#cmd[@]} == 0 )) ; then
        if isdefined-cmd antigravity-cli ; then
            cmd=(antigravity-cli)
        else
            cmd=(gemini)
        fi
    fi
    ensure-cmd "${cmd[1]}" @RET

    local -x EDITOR=nvim
    local -x VISUAL="${EDITOR}"

    #: Keeps ~/.gemini/AGENTS.md current with its sources; asks before
    #: launching with stale instructions.
    h-agents-md-sync-ask @RET

    tty-title "🪐${PWD:t}"

    $proxyenv reval-ec "${cmd[@]}" "$@"
}
aliasfn antigravity-m antigravity
##
