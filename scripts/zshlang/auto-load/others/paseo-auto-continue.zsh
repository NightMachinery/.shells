##
#: Quota-aware continuation for local Paseo agents. See =docs/paseo-auto-continue.md=.
##
function h-paseo-auto-continue {
    command python3 "${NIGHTDIR}/python/paseo_auto_continue.py" "$@"
}

function paseo-auto-continue-on {
    : "usage: paseo-auto-continue-on [agent-uuid] [--home PATH] [--poll SECONDS]"
    h-paseo-auto-continue on "$@"
}

function paseo-auto-continue-off {
    : "usage: paseo-auto-continue-off [agent-uuid] [--home PATH]"
    h-paseo-auto-continue off "$@"
}

function paseo-auto-continue-status {
    : "usage: paseo-auto-continue-status [agent-uuid] [--home PATH]"
    h-paseo-auto-continue status "$@"
}
