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
#: Recalling a saved Wi-Fi password.
#:
#: macOS files Wi-Fi secrets in the *System* keychain -- which is not on the
#: keychain search list `security' uses by default -- under the item kind
#: =AirPort network password=. Both facts are load-bearing: the obvious
#: `security find-generic-password -w <ssid>' finds nothing, and fails in a way
#: that reads exactly like "this network has no password".
#:
#: See =docs/wifi-password.md=.
##
function h-wifi-device-darwin {
    #: Prints the BSD device name of the Wi-Fi hardware port, e.g. `en0'.
    #:
    #: Not hardcoded: the Wi-Fi port is en1 on Macs with built-in ethernet, and
    #: moves again behind some Thunderbolt docks.
    ##
    @darwinOnly

    local device
    device="$(networksetup -listallhardwareports |
                  perl -ne 'if (/^Hardware Port:\s*Wi-Fi\s*$/) { $found = 1 ; next }
                            if ($found && /^Device:\s*(\S+)/) { print "$1\n" ; exit }')" @TRET

    if test -z "${device}" ; then
        ecerr "$0: no Wi-Fi hardware port on this machine"
        return 1
    fi

    ec "${device}"
}

function wifi-ssid-list-networksetup {
    #: Lists the SSIDs this Mac prefers on its Wi-Fi interface, in macOS's own
    #: preference order.
    ##
    @darwinOnly

    local device="${wifi_ssid_list_device}"
    if test -z "${device}" ; then
        device="$(h-wifi-device-darwin)" @TRET
    fi

    #: The output is a `Preferred networks on en0:' header followed by one
    #: tab-indented SSID per line. Keying on the tab rather than skipping the
    #: first line keeps SSIDs with leading spaces intact.
    networksetup -listpreferredwirelessnetworks "${device}" |
        perl -ne 'next unless s/^\t// ; print'
}
@opts-setprefix wifi-ssid-list-networksetup wifi_ssid_list

function wifi-ssid-list-known {
    #: Lists every SSID this Mac remembers, from the known-networks plist.
    #: Needs root; the plist is 0600 root:wheel.
    #:
    #: Measured on [2026-09-07] this returned exactly the same set as
    #: [agfi:wifi-ssid-list-networksetup]. It is here for the cases where the
    #: two diverge -- a network dropped from the preferred list but still in
    #: the keychain, or a second Wi-Fi interface -- which is why it stays
    #: opt-in rather than becoming the default: it costs a sudo prompt.
    ##
    @darwinOnly

    local plist="${wifi_ssid_list_known_plist:-/Library/Preferences/com.apple.wifi.known-networks.plist}"
    assert test -e "${plist}" @RET

    ecgray "$0: reading ${plist} (needs sudo) ..."

    #: `plutil -convert json' fails on this file -- the SSID blobs and the
    #: dates have no JSON form -- so xml1 it is.
    local xml
    xml="$(sudo plutil -convert xml1 -o - "${plist}")" @TRET

    #: Top-level keys are `wifi.network.ssid.<SSID>', and nothing nested
    #: carries that prefix: the `wifi.ssid.<hex>' entries under
    #: CollocatedGroup are <string>s, not <key>s. So a keyed match is enough
    #: and we do not need a real plist parser.
    ec "${xml}" |
        perl -ne 'if (m{<key>wifi\.network\.ssid\.(.*)</key>}) {
                      my $ssid = $1 ;
                      $ssid =~ s/&lt;/</g ;
                      $ssid =~ s/&gt;/>/g ;
                      $ssid =~ s/&amp;/&/g ;
                      print "$ssid\n" ;
                  }'
}

function wifi-password-get-darwin {
    #: Usage: wifi-password-get <SSID>
    #:
    #: Prints the saved password of <SSID>, and copies it when stdout is a tty.
    ##
    @darwinOnly

    local ssid="${1}"
    assert-args ssid @RET

    local keychain="${wifi_password_get_keychain:-/Library/Keychains/System.keychain}"

    #: -D selects the item *kind*, and the System keychain has to be named
    #: explicitly because it is not on the default search list. Without both,
    #: this finds nothing at all.
    #:
    #: Expect a GUI authorization prompt: `security' is not the application
    #: that created the item, so macOS asks -- and it asks once per item, so
    #: granting it for one network does nothing for the next.
    #:
    #: security's own stderr is deliberately not redirected, so its message
    #: reaches the user alongside ours.
    local password retcode=0
    password="$(security find-generic-password -D 'AirPort network password' -a "${ssid}" -w "${keychain}")" || retcode=$?

    if (( retcode == 44 )) ; then
        #: 44 is errSecItemNotFound. A remembered network with no stored
        #: password -- open, or 802.1X with the secret held elsewhere -- lands
        #: here, so this is an ordinary answer and not a fault worth a trace.
        ecerr "$0: no saved password for SSID $(gquote-sq "${ssid}")"
        return 1
    elif (( retcode != 0 )) ; then
        ecerr "$0: could not read the keychain for SSID $(gquote-sq "${ssid}") (security exited ${retcode}); the authorization prompt may have been denied"
        return "${retcode}"
    fi

    ec "${password}" | cat-copy-if-tty
}
@opts-setprefix wifi-password-get-darwin wifi_password_get

function wifi-password-get-fz {
    #: Usage: wifi-password-get-fz [query ...]
    #:
    #: Interactively picks a remembered Wi-Fi network and returns its password.
    ##
    @darwinOnly

    local source="${wifi_password_get_fz_source:-preferred}"
    ensure-array wifi_password_get_fz_fz_opts
    local fz_opts=("${wifi_password_get_fz_fz_opts[@]}")

    local query
    query="$(fz-createquery "$@")"

    local ssids
    case "${source}" in
        preferred)
            ssids="$(wifi-ssid-list)" @TRET
            ;;
        known)
            ssids="$(wifi-ssid-list-known)" @TRET
            ;;
        *)
            ecerr "$0: unknown source $(gquote-sq "${source}"); expected 'preferred' or 'known'"
            return 1
            ;;
    esac

    if test -z "${ssids}" ; then
        ecerr "$0: no remembered Wi-Fi networks found"
        return 1
    fi

    #: --no-multi because FZF_DEFAULT_OPTS turns --multi on globally, and one
    #: password on the clipboard is the entire point.
    #:
    #: Plain @RET rather than @TRET: pressing Esc is an ordinary outcome here,
    #: not an exception worth a stacktrace.
    #:
    #: [agfi:fz] ends with [agfi:cat-copy-if-tty], but stdout is not a tty
    #: inside this subshell, so the SSID is not copied -- only the password
    #: below reaches the clipboard.
    local ssid
    ssid="$(ec "${ssids}" | fz --no-multi --prompt='wifi SSID> ' --query "${query}" "${fz_opts[@]}")" @RET
    ssid="${ssid%%$'\n'*}"

    test -n "${ssid}" || return 1

    #: On stderr, so the label reaches neither stdout nor the clipboard.
    ecgray "$0: ${ssid}"

    wifi-password-get "${ssid}"
}
##
aliasfn wifi-ssid-list wifi-ssid-list-networksetup
aliasfn wifi-password-get wifi-password-get-darwin
#: The same picker against the complete, root-read list.
aliasfn wifi-password-get-known-fz @opts source known @ wifi-password-get-fz
##
