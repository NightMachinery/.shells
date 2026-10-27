##
aliasfn tailscale-status tailscale status
##
function tailscale-firewall-allow-darwin {
    #: Register Homebrew's tailscaled with the macOS Application Firewall (ALF).
    #:
    #: WHY: like mosh-server (see [agfi:firewall-allow-mosh-darwin]), Homebrew's
    #: tailscaled is ad-hoc signed, so with the firewall on ALF silently drops
    #: its inbound WireGuard UDP. Nothing looks broken: Tailscale falls back to a
    #: DERP relay, `tailscale ping` reports "via DERP", and bulk transfers crawl
    #: (~2.7 MB/s between two Macs on the same LAN, 2026-10). Proven by sending
    #: UDP to two listeners on the same port range: Apple's signed python3
    #: received it, Homebrew's ad-hoc signed python3 did not.
    #:
    #: The App Store/standalone Tailscale.app is Developer ID signed and does not
    #: need this. The pf anchor is a separate layer; see
    #: [[NIGHTDIR:launchers/pf/install.org]].
    #:
    #: @warn Re-run after every `brew upgrade tailscale`: the registered path is
    #: the version-stamped Cellar binary.
    #:
    #: @warn Restart tailscaled afterwards (`sudo brew services restart tailscale`).
    #: Its UDP socket predates the registration and stays blocked: the daemon kept
    #: logging "netcheck: UDP is blocked" until restarted, then reported udp=true.
    #:
    #: @usage tailscale-firewall-allow-darwin                #: register (needs root)
    #: @usage tailscale-firewall-allow-darwin --check        #: ALF's claim only; no root
    #: @usage tailscale-firewall-allow-darwin --probe PEER   #: real test: is the path to PEER direct?
    ##
    if [[ "$1" == '--probe' ]] ; then
        local peer="$2"
        assert-args peer @RET

        #: `tailscale ping` keeps pinging until a direct path appears and exits
        #: non-zero if none does, so its status is the answer.
        reval-ec tailscale ping -c 10 "$peer"
        return $?
    fi

    local ts_sym="${commands[tailscaled]}"
    if test -z "$ts_sym" ; then
        ecerr "$0: tailscaled is not in PATH"
        return 1
    fi
    local ts_abs
    ts_abs="$(greadlink -f "$ts_sym")" @TRET

    if [[ "$1" == '--check' ]] ; then
        if h-firewall-allow-app-darwin --check "$0" "$ts_sym" "$ts_abs" ; then
            ec "$0: That is ALF's claim, not proof; \`--probe PEER\` tests the actual path."
            return 0
        else
            ecerr "$0: run \`${0}\` to register it (peers will relay through DERP until you do)."
            return 1
        fi
    fi

    h-firewall-allow-app-darwin "$0" "$ts_sym" "$ts_abs" @RET

    tailscale-firewall-allow-darwin --check
    ecgray "$0: now restart tailscaled (\`sudo brew services restart tailscale\`); its existing socket stays blocked until then."
    ecgray "$0: for real proof, run \`$0 --probe PEER\` with a peer on the same network."
}
##
