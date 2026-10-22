## Shared history index, supervised independently of Hammerspoon reloads.
typeset -g omni_history_launcher_version=1.0.0
function omni-history-boot {
    go-local-dep omni-history "${NIGHTDIR}/golang/omni-history" || return $?
    tmuxnewsh2 omni-history lo_s=10 loop reval-notifexit omni-history \
        -state-file "${HOME}/.local/state/omni-history/server.json"
}
