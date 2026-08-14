##
function router-mac-darwin {
    local gw
    gw="$(route -n get default 2>/dev/null | awk '/gateway:/{print $2}')" @TRET

    assert silent ping -c1 -t1 "$gw" @RET
    arp -n "$gw" | awk '{print $4}' |
        perl -nE 'chomp; say join ":", map { sprintf "%02x", hex $_ } split /:/' |
        cat-copy-if-tty
}
##
function wifi-internet-sharing-fix-ap1 {
    ##
    # Repair macOS Internet Sharing when clients hang at "Obtaining IP address".
    #
    # On Apple Silicon the Wi-Fi chip exposes en0 (station radio) and ap1 (SoftAP).
    # Internet Sharing must bridge ap1, but macOS 14 enrols en0 into bridge100
    # instead, so client DHCP DISCOVERs land on ap1 and never reach bootpd (which
    # listens only on bridge100). Nothing in the prefs can be edited to fix it.
    #
    # Normally the local.internetsharing.apbridge LaunchDaemon handles this; this is
    # for the rare miss, or before installing the daemon.
    #
    # This ADDS ap1 and deliberately does not remove en0, leaving both enrolled.
    # Do not "tidy that up" with `ifconfig bridge100 deletem en0': measured on
    # [2026-08-14], that takes the SoftAP down -- ap1 and bridge100 both go
    # inactive the instant it runs and the hotspot drops -- and it does NOT undo
    # with `addm'. Re-adding restores the membership list but leaves the radio
    # down; recovery is toggling Internet Sharing off and on, which rebuilds the
    # bridge and re-enrols en0 anyway.
    #
    # Bridge membership is owned by Internet Sharing, so mutating it out from
    # under the service tears down the AP it manages. Adding is the safe
    # direction; the inverse is not.
    #
    # The cost of leaving en0 enrolled: bridge100 is the interface bootpd serves,
    # so joining a Wi-Fi network while sharing is active would put that network
    # in our DHCP server's segment. The uplink here is wired (en10), so en0 is
    # normally associated with nothing. Written up in
    # ~[nt]/public/cheatsheets/OS/macOS/internet sharing/breadcrumbs/internet-sharing-bridge.md
    # -- kept out of this repo, which is public.
    #
    # See: [[id:f0c71d19-2c6f-4b48-82f1-d28ccaed5e90][breadcrumbs/ap1-bug]]
    ##
    assert isDarwin @RET

    local br=bridge100 ap=ap1

    if ! ifconfig "$br" &>/dev/null ; then
        ecerr "$0: $br does not exist; is Internet Sharing on?"
        return 1
    fi

    if ifconfig "$br" 2>/dev/null | grep -q "member: $ap" ; then
        ecgray "$0: $ap is already enrolled in $br; nothing to do."
        ifconfig "$br" | grep -E 'member:|status:'
        return 0
    fi

    if ! ifconfig "$ap" 2>/dev/null | grep -q 'status: active' ; then
        ecerr "$0: warning: $ap is not active; the hotspot might not be running."
    fi

    ecgray "$0: enrolling $ap into $br (needs sudo) ..."
    sudo ifconfig "$br" addm "$ap" @RET

    ec-sep-h
    ifconfig "$br" | grep -E 'member:|status:'

    ec-sep-h
    # bootpd is socket-activated; a non-zero run count means DHCP is being served.
    sudo launchctl print system/com.apple.bootpd 2>/dev/null |
        grep -E '^[[:space:]]+(state|runs) '
}
##
