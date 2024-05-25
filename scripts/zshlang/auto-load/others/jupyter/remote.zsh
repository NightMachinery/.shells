##
function socat-to-6035 {
    local from="${1}" host="${2:-127.0.0.1}"
    assert-args from @RET

    tmuxnew jupyter-socat-to-6035 socat TCP-LISTEN:6035,bind=127.0.0.1,fork TCP:"${host}:${from}"
}
##
