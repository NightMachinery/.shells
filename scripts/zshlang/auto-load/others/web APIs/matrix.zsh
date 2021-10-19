# https://github.com/ahmedsaadxyzz/matrixcli
# https://matrix.org/clients/
###
function matrix-send-self() {
    if ! isdefined-cmd matrixcli ; then
        ecgray "$0: matrixcli not found; Skipping."
        return 0
    fi

    local msg="$*" room='!sCJIlZxICIvTJWNsrQ:matrix.org'
    local config
    config="$nightNotes/private/configs/matrixcli/config.py"
    assert test -e "$config" @RET

    matrixcli -c "$config" send -r "$room" "$msg"
}
##
