##
function antigravity {
    #: Google's Antigravity CLI.
    #:
    #: The binary is `agy`, not `antigravity` and not `gemini`; its state
    #: lives under `~/.gemini/antigravity-cli/`, and its global rules file is
    #: `~/.gemini/GEMINI.md`. The shared preamble is [agfi:h-agent-launch].
    ##
    local cmd=("${antigravity_cmd[@]:-agy}")
    ensure-cmd "${cmd[1]}" @RET

    h-agent-launch agy reval-ec "${cmd[@]}" "$@"
}
aliasfn antigravity-m antigravity
##
