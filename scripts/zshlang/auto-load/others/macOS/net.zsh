##
function router-mac-darwin {
    local gw
    gw="$(route -n get default 2>/dev/null | awk '/gateway:/{print $2}')" @TRET

    assert silent ping -c1 -t1 "$gw" @RET
    arp -n "$gw" | awk '{print $4}'
}
##
