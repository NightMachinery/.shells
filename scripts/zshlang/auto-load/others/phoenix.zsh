typeset -g PHOENIX_CONF=~/.phoenix.js
##
function phoenix-reload() {
    if test -e "$PHOENIX_CONF" ; then
        touch "$PHOENIX_CONF"
    else
        return 0
    fi
}
