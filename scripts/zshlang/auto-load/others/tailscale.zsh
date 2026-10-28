##
aliasfn tailscale-status tailscale status
##
function tailscale-fix-process-priority {
    #: Make Homebrew's tailscaled an Interactive launchd job, so the shells it
    #: starts for Tailscale SSH run at full speed.
    #:
    #: WHY: the formula's LaunchDaemon sets no ProcessType, and launchd then
    #: applies light CPU and I/O limits to the job (see launchd.plist(5)). Every
    #: Tailscale SSH session is a child of tailscaled and inherits them, and so
    #: does a mosh-server started through it. Measured 2026-10 between two Macs:
    #: a CPU loop ran 5x slower over Tailscale SSH (2.93s against 0.59s
    #: locally), and an interactive zsh took 20s to start instead of 4.8s, which
    #: looked like `ssh HOST -t zsh' hanging.
    #:
    #: Applying it restarts tailscaled, dropping every tailnet connection for a
    #: few seconds, including a Tailscale SSH session you run this from. So the
    #: unload and reload run detached, in their own session, and the reload is
    #: retried: a Mac left with tailscaled unloaded would be off the tailnet.
    #:
    #: @warn Homebrew rewrites the plist on `brew services restart tailscale'
    #: and on upgrades, dropping the setting. Re-run this afterwards.
    #:
    #: @usage tailscale-fix-process-priority          #: set and reload (needs root)
    #: @usage tailscale-fix-process-priority --check  #: report only; no root
    ##
    local plist=( /Library/LaunchDaemons/{sh.brew,homebrew.mxcl}.tailscale.plist(N) )
    if (( ${#plist} != 1 )) ; then
        ecerr "$0: expected one Homebrew tailscale LaunchDaemon, found ${#plist}: ${plist[*]}"
        return 1
    fi
    plist="${plist[1]}"
    local label="${${plist:t}%.plist}"
    local pb=/usr/libexec/PlistBuddy current
    current="$("$pb" -c 'Print :ProcessType' "$plist" 2>/dev/null)"

    if [[ "$1" == '--check' ]] ; then
        ec "$0: ${plist}: ProcessType=${current:-<unset>}"
        #: The plist can say Interactive while launchd still runs the job it
        #: loaded before the edit; this line shows what is actually loaded.
        command launchctl print "system/${label}" 2>/dev/null | command grep -E 'spawn type'
        [[ "$current" == Interactive ]]
        return $?
    fi

    local sudo_cmd=( ${(@f)"$(h-sudo-cmd)"} )
    if [[ "$current" != Interactive ]] ; then
        if test -n "$current" ; then
            reval-ec "$sudo_cmd[@]" "$pb" -c 'Set :ProcessType Interactive' "$plist" @RET
        else
            reval-ec "$sudo_cmd[@]" "$pb" -c 'Add :ProcessType string Interactive' "$plist" @RET
        fi
    fi

    #: Fork, setsid, then exec: the parent returns at once, and the child is
    #: outside this (possibly Tailscale SSH) session when tailscaled goes down.
    local log=/tmp/tailscale-fix-process-priority.log
    reval-ec "$sudo_cmd[@]" /usr/bin/perl -MPOSIX -e 'exit 0 if fork; POSIX::setsid(); exec @ARGV or die' -- \
        /bin/sh -c 'exec >"$3" 2>&1 < /dev/null
            sleep 1
            /bin/launchctl bootout "system/$1"
            for i in 1 2 3 4 5 6 7 8 9 10 ; do
                /bin/launchctl bootstrap system "$2" && exit 0
                sleep 2
            done
            exit 1' sh "$label" "$plist" "$log" @RET

    ecgray "$0: tailscaled is restarting; tailnet connections drop for a few seconds (log: ${log})."
    ecgray "$0: then confirm with \`$0 --check'."
}

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
    #: Homebrew ships tailscaled only *linker*-signed (`Identifier=a.out`, flags
    #: adhoc,linker-signed), unlike mosh-server's full ad-hoc signature. On one
    #: Mac (2026-10) registering and restarting that binary changed nothing: ALF
    #: listed it as permitted, netcheck still said UDP was blocked. Re-signing it
    #: ad-hoc with a real identifier, then registering and restarting, fixed it.
    #: Another Mac receives UDP with the linker signature, so it is not always
    #: fatal; but the re-sign is cheap and harmless, so we do it whenever the
    #: binary is linker-signed. An upgrade brings the linker signature back.
    #:
    #: This does not help when one Mac is the Internet Sharing hotspot for the
    #: other: that host's tailscaled binds its UDP socket to the default-route
    #: interface (the uplink), so its packets to the peer's 192.168.2.x address
    #: leave through the uplink and are lost. The path stays on DERP, with no
    #: supported knob to change it (`TS_BIND_TO_INTERFACE_BY_ROUTE` skips the
    #: listening socket). Use a direct route for bulk data there; see
    #: [agfi:h-rsp-lan-route].
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
        if command codesign -dv "$ts_abs" 2>&1 | command grep -q -- 'linker-signed' ; then
            ecgray "$0: ${ts_abs} is still only linker-signed; registering re-signs it first."
        fi
        if h-firewall-allow-app-darwin --check "$0" "$ts_sym" "$ts_abs" ; then
            ec "$0: That is ALF's claim, not proof; \`--probe PEER\` tests the actual path."
            return 0
        else
            ecerr "$0: run \`${0}\` to register it (peers will relay through DERP until you do)."
            return 1
        fi
    fi

    if command codesign -dv "$ts_abs" 2>&1 | command grep -q -- 'linker-signed' ; then
        local sudo_cmd=( ${(@f)"$(h-sudo-cmd)"} )
        reval-ec "$sudo_cmd[@]" /usr/bin/codesign --force --sign - --identifier com.tailscale.tailscaled "$ts_abs" @RET
    fi

    h-firewall-allow-app-darwin "$0" "$ts_sym" "$ts_abs" @RET

    tailscale-firewall-allow-darwin --check
    ecgray "$0: now restart tailscaled (\`sudo brew services restart tailscale\`); its existing socket stays blocked until then."
    ecgray "$0: for real proof, run \`$0 --probe PEER\` with a peer on the same network."
}
##
