#!/usr/bin/env zsh
# Install (or remove) the pf ruleset that denies inbound traffic from public
# source addresses, plus the self-healing LaunchDaemon that re-applies it at
# boot and after macOS updates.
#
#   ./install-publicnet-pf.zsh             # install; leaves IPv6 alone
#   ./install-publicnet-pf.zsh --no-ipv6   # also disable IPv6 on the wired service
#   ./install-publicnet-pf.zsh --ipv6      # explicitly keep IPv6 (the default)
#   ./install-publicnet-pf.zsh --uninstall
#   ./install-publicnet-pf.zsh --check     # report state, change nothing
#
# IPv6 is left enabled by default: the ruleset filters on source address and
# covers both families, so a public v6 peer is already blocked. Disabling v6
# only additionally covers the window where pf is not loaded -- early boot, or
# after a manual `pfctl -d`.
#
# Escape hatch if anything goes wrong: sudo pfctl -d
#
# Run as your normal user, NOT with sudo -- the script elevates only the
# individual commands that need it, and running the whole thing as root would
# resolve ~ to /var/root.

emulate -L zsh
set -o pipefail

ANCHOR_NAME='com.user.publicnet'
SRC_DIR="${0:A:h}"
ANCHOR_SRC="${SRC_DIR}/${ANCHOR_NAME}"
ANCHOR_DST="/etc/pf.anchors/${ANCHOR_NAME}"
PLIST_SRC="${SRC_DIR}/${ANCHOR_NAME}.pf.plist"
PLIST_DST="/Library/LaunchDaemons/${ANCHOR_NAME}.pf.plist"
BOOT_SRC="${SRC_DIR}/publicnet-pf-boot.sh"
BOOT_DST="/usr/local/libexec/publicnet-pf-boot.sh"
ANCHOR_MASTER="/usr/local/libexec/${ANCHOR_NAME}.anchor"
PF_CONF='/etc/pf.conf'
V6_SERVICE='USB 10/100/1000 LAN'
EXPECTED_IF='en10'
DISABLE_V6=0   # default: leave IPv6 alone; --no-ipv6 turns it off

# --- output helpers -----------------------------------------------------------
_info() { print -r -- "  $*" }
_step() { print -r -- $'\n''=> '"$*" }
_warn() { print -ru2 -- "!! $*" }
_die()  { print -ru2 -- "!! $*"$'\n''!! aborted; nothing further changed.'; exit 1 }

# --- sudo: prompt once, then keep the timestamp warm --------------------------
# sudo caches credentials for ~5 minutes per tty. `sudo -v` up front means one
# password prompt; the background refresher covers scripts that run longer.
_sudo_setup() {
    (( EUID == 0 )) && _die "run this as your normal user, not with sudo."
    _info 'requesting sudo (one prompt, then cached)...'
    sudo -v || _die 'sudo authentication failed.'
    ( while true ; do sudo -n true 2>/dev/null; sleep 45; kill -0 $$ 2>/dev/null || exit; done ) &
    SUDO_KEEPALIVE_PID=$!
    trap '[[ -n $SUDO_KEEPALIVE_PID ]] && kill $SUDO_KEEPALIVE_PID 2>/dev/null' EXIT INT TERM
}

# --- ruleset validation -------------------------------------------------------
# pfctl is not trustworthy on exit status alone: a typo such as
# "icmp6-type packettoobig" makes it print "unknown icmp6-type" and still exit
# 0, so a broken rule would be silently dropped from the loaded set. Treat any
# diagnostic that is not the routine -f warning as a failure.
_pf_check() {
    local file="$1" out rc
    out="$(sudo pfctl -n -f "$file" 2>&1)" ; rc=$?
    out="$(print -r -- "$out" \
        | grep -viE 'could result in flushing|present in the main ruleset|see /etc/pf\.conf' \
        | grep -vE '^[[:space:]]*$')"
    if (( rc != 0 )) || [[ -n $out ]] ; then
        [[ -n $out ]] && print -ru2 -- "$out"
        return 1
    fi
    return 0
}

# --- checks -------------------------------------------------------------------
_report_state() {
    _step 'current state'
    local pf_state
    pf_state="$(sudo pfctl -s info 2>/dev/null | head -1)"
    _info "pf:            ${pf_state:-unknown}"
    if grep -q "$ANCHOR_NAME" "$PF_CONF" 2>/dev/null ; then
        _info "pf.conf hook:  present"
    else
        _info "pf.conf hook:  ABSENT (rules are not being enforced)"
    fi
    [[ -f $ANCHOR_DST ]] && _info "anchor file:   installed" || _info "anchor file:   absent"
    [[ -f $PLIST_DST  ]] && _info "launchdaemon:  installed" || _info "launchdaemon:  absent"

    local v6
    v6="$(ifconfig "$EXPECTED_IF" 2>/dev/null | grep -c 'inet6 2001')"
    _info "public IPv6:   ${v6} address(es) on ${EXPECTED_IF}"

    _step 'listeners on publicly reachable addresses'
    local pub
    pub="$(lsof -nP -iTCP -sTCP:LISTEN 2>/dev/null \
        | awk 'NR>1 && $9 !~ /^(127\.0\.0\.1|\[::1\]|192\.168\.)/ {print "  " $9, $1}' | sort -u)"
    if [[ -n $pub ]] ; then print -r -- "$pub" ; else _info 'none' ; fi
}

_verify_interface() {
    # The ruleset is default-deny inbound on every interface, so it does not
    # care which one carries the public address and keeps working when this
    # machine moves to a phone hotspot or another network. Nothing to validate
    # about the uplink; just report it.
    local actual
    actual="$(route -n get default 2>/dev/null | awk '/interface:/{print $2}')"
    _info "default route via ${actual:-unknown} (ruleset is interface-agnostic, so this is informational)"

    # The local allow-list does name interfaces. If Internet Sharing is on but
    # its bridge is missing or renamed, hotspot clients would be cut off.
    local sharing_on
    sharing_on="$(defaults read /Library/Preferences/SystemConfiguration/com.apple.nat NAT 2>/dev/null | awk '/Enabled/{print $3}' | tr -d ';' | grep -c '^1$')"
    if (( sharing_on > 0 )) ; then
        if ifconfig bridge100 >/dev/null 2>&1 ; then
            _info 'Internet Sharing is on and bridge100 exists; hotspot clients stay allowed.'
        else
            _warn 'Internet Sharing appears enabled but bridge100 is missing.'
            _warn "check its interface name and adjust the 'pass in on bridge100' line in ${ANCHOR_SRC}."
            read -q '?   continue anyway? [y/N] ' || { print; _die 'stopped.' }
            print
        fi
    else
        _info 'Internet Sharing is off; the bridge100/ap1 allow rules simply will not match.'
    fi
}

# --- install ------------------------------------------------------------------
_install() {
    [[ -f $ANCHOR_SRC ]] || _die "missing ${ANCHOR_SRC}"
    [[ -f $PLIST_SRC  ]] || _die "missing ${PLIST_SRC}"

    _sudo_setup
    _step 'sanity checks'
    _verify_interface

    _step 'parsing the ruleset before touching anything'
    _pf_check "$ANCHOR_SRC" || _die 'the anchor does not parse.'
    _info 'anchor parses cleanly.'

    _step 'installing anchor file'
    sudo cp "$ANCHOR_SRC" "$ANCHOR_DST" || _die 'copy failed.'
    sudo chown root:wheel "$ANCHOR_DST"
    sudo chmod 644 "$ANCHOR_DST"
    _info "$ANCHOR_DST"

    _step 'hooking it into pf.conf'
    if grep -q "$ANCHOR_NAME" "$PF_CONF" ; then
        _info 'already referenced; leaving pf.conf alone (idempotent).'
    else
        local backup="${PF_CONF}.bak-$(date +%Y%m%d-%H%M%S)"
        sudo cp "$PF_CONF" "$backup" || _die 'could not back up pf.conf.'
        _info "backed up to ${backup}"
        # Appended, so these land after the com.apple anchors. Order matters.
        printf '\nanchor "%s"\nload anchor "%s" from "%s"\n' \
            "$ANCHOR_NAME" "$ANCHOR_NAME" "$ANCHOR_DST" \
            | sudo tee -a "$PF_CONF" >/dev/null || _die 'could not append to pf.conf.'
        _info 'anchor lines appended.'
    fi

    _step 'dry run of the combined ruleset'
    _pf_check "$PF_CONF" \
        || _die "combined ruleset failed to parse. Restore with: sudo cp ${PF_CONF}.bak-* ${PF_CONF}"
    _info 'combined ruleset parses cleanly.'

    _step 'loading (pfctl -E keeps a reference count so Internet Sharing coexists)'
    sudo pfctl -E -f "$PF_CONF" 2>&1 | grep -viE 'could result in flushing|see /etc/pf.conf' >&2
    _info 'loaded.'

    _step 'active rules in the anchor'
    sudo pfctl -a "$ANCHOR_NAME" -s rules 2>/dev/null | sed 's/^/  /'

    _step 'installing self-healing boot script'
    # root:wheel 755 in a root-owned directory is not optional: the LaunchDaemon
    # runs this as root, so a user-writable copy would be a privilege-escalation
    # path for anything running as $USER.
    sudo mkdir -p "${BOOT_DST:h}"
    sudo cp "$BOOT_SRC" "$BOOT_DST" || _die 'copy failed.'
    sudo chown root:wheel "$BOOT_DST"
    sudo chmod 755 "$BOOT_DST"
    # Master copy of the anchor, so the boot script can restore it if
    # /etc/pf.anchors is ever cleared.
    sudo cp "$ANCHOR_SRC" "$ANCHOR_MASTER" || _die 'copy failed.'
    sudo chown root:wheel "$ANCHOR_MASTER"
    sudo chmod 644 "$ANCHOR_MASTER"
    _info "$BOOT_DST (root:wheel 755)"
    _info "$ANCHOR_MASTER"

    _step 'installing LaunchDaemon so this survives reboots and OS updates'
    sudo cp "$PLIST_SRC" "$PLIST_DST" || _die 'copy failed.'
    sudo chown root:wheel "$PLIST_DST"
    sudo chmod 644 "$PLIST_DST"
    sudo launchctl unload "$PLIST_DST" 2>/dev/null
    sudo launchctl load -w "$PLIST_DST" || _warn 'launchctl load failed; rules apply now but not after reboot.'
    _info "$PLIST_DST"
    _info 'watches /etc/pf.conf, so an OS update that strips the anchor is repaired automatically.'

    if (( ! DISABLE_V6 )) ; then
        _step 'leaving IPv6 enabled (default; --no-ipv6 to disable)'
        _info 'the ruleset filters by source address across both families, so public'
        _info 'v6 peers are already denied. Disabling v6 would only additionally cover'
        _info 'the window where pf is not loaded (early boot, or after pfctl -d).'
    else
        _step "disabling IPv6 on '${V6_SERVICE}'"
        # Opt-in via --no-ipv6, because pf already denies public v6 peers by
        # source address. Worth it only if you do not need v6 on this service:
        # it additionally covers the window where pf is not loaded at all.
        sudo networksetup -setv6off "$V6_SERVICE" \
            || _warn 'setv6off failed; check the name with: networksetup -listallnetworkservices'
        sleep 2
        if ifconfig "$EXPECTED_IF" 2>/dev/null | grep -q 'inet6 2001' ; then
            _warn 'public IPv6 addresses still present; they may take a moment to clear.'
        else
            _info 'public IPv6 addresses gone.'
        fi
    fi

    _step 'connectivity check'
    local code
    code="$(curl -sS -o /dev/null -w '%{http_code}' --max-time 15 https://example.com 2>/dev/null)"
    if [[ $code == 200 ]] ; then
        _info 'outbound HTTPS works.'
    else
        _warn "outbound check returned '${code}'. If the network is dead, run: sudo pfctl -d"
    fi

    _report_state
    print -r -- $'\n''Done. Verify the hotspot from your phone, then reboot once to confirm persistence.'
    print -r -- 'Escape hatch: sudo pfctl -d'
}

# --- uninstall ----------------------------------------------------------------
_uninstall() {
    _sudo_setup

    _step 'disabling pf'
    sudo pfctl -d 2>/dev/null; _info 'pf disabled.'

    _step 'removing LaunchDaemon'
    if [[ -f $PLIST_DST ]] ; then
        sudo launchctl unload -w "$PLIST_DST" 2>/dev/null
        sudo rm -f "$PLIST_DST"
        _info 'removed.'
    else
        _info 'not installed.'
    fi

    _step 'restoring pf.conf'
    local newest
    newest="$(ls -t ${PF_CONF}.bak-* 2>/dev/null | head -1)"
    if [[ -n $newest ]] ; then
        sudo cp "$newest" "$PF_CONF"
        _info "restored from ${newest}"
    else
        _warn "no backup found; strip the '${ANCHOR_NAME}' lines from ${PF_CONF} by hand."
    fi

    _step 'removing anchor file'
    sudo rm -f "$ANCHOR_DST" "$BOOT_DST" "$ANCHOR_MASTER" && _info "removed anchor, boot script and master copy."

    _step "re-enabling IPv6 on '${V6_SERVICE}'"
    sudo networksetup -setv6automatic "$V6_SERVICE" && _info 'IPv6 set back to automatic.'

    _report_state
    print -r -- $'\n''Uninstalled. Note that your services are still bound narrowly -- that is the'
    print -r -- 'primary protection and is unaffected by removing pf.'
}

# --- main ---------------------------------------------------------------------
while (( $# )) ; do
    case "$1" in
        --ipv6)      DISABLE_V6=0; shift ;;
        --no-ipv6)   DISABLE_V6=1; shift ;;
        *) ACTION="$1"; shift ;;
    esac
done

case "${ACTION:-install}" in
    install|'')  _install ;;
    --uninstall) _uninstall ;;
    --check)     _sudo_setup; _report_state ;;
    *)           print -ru2 -- "usage: ${0:t} [install|--uninstall|--check] [--ipv6|--no-ipv6]"; exit 2 ;;
esac
