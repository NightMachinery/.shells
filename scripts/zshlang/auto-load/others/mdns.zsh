##
function mdns-init-darwin {
    assert isDarwin @RET

    assert-args MDNS_NAME @RET

    local h
    for h in "${MDNS_NAME}.local" "files.${MDNS_NAME}.local" ; do
        assert mdns-register-host-darwin "$h" @RET
    done
}

function mdns-register-host-darwin {
    assert isDarwin @RET

    local hostname="$1" ip="${2}"
    assert-args hostname @RET
    if test -z "$ip" ; then
        ip="$(ip-internal-get1 | ghead -1)" @TRET
    fi

    tmuxnewsh2 "mDNS| $hostname" dns-sd -P dummy _http._tcp local 0 "$hostname" "$ip" @RET
}
##
