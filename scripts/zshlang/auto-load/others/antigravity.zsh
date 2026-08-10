##
function antigravity {
    #: Google's Antigravity CLI.
    #:
    #: The binary is `agy`, not `antigravity` and not `gemini`; its state
    #: lives under `~/.gemini/antigravity-cli/`, and its global rules file is
    #: `~/.gemini/GEMINI.md`.
    #: @duplicateCode/fd706e6b8475e27ca5cf27951b1d8ddc
    ##
    local cmd=("${antigravity_cmd[@]:-agy}")
    ensure-cmd "${cmd[1]}" @RET

    local -x EDITOR=nvim
    local -x VISUAL="${EDITOR}"

    #: Keeps ~/.gemini/GEMINI.md current with its sources; asks before
    #: launching with stale instructions.
    h-agents-md-sync-ask @RET

    tty-title "🪐${PWD:t}"

    $proxyenv reval-ec "${cmd[@]}" "$@"
}
aliasfn antigravity-m antigravity
##
