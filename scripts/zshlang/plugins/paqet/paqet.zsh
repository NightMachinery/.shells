# -*- mode: sh; sh-shell: zsh; -*-
###
# Paqet client wrapper
###

function h-paqet-platform {
    local uname_s
    uname_s="$(command uname -s)" @TRET

    case "${uname_s}" in
        Darwin)
            ec darwin
            ;;
        Linux)
            ec linux
            ;;
        *)
            ecerr "$0: unsupported platform: ${uname_s}"
            return 1
            ;;
    esac
}

function h-paqet-normalize-mac {
    local mac="${1}"

    command perl -e '
        my $mac = shift // "";
        chomp $mac;
        $mac = lc $mac;
        $mac =~ s/-/:/g;
        if ($mac =~ /^[0-9a-f]{1,2}(?::[0-9a-f]{1,2}){5}$/) {
            print join(":", map { sprintf "%02x", hex $_ } split /:/, $mac), "\n";
            exit 0;
        }
        exit 1;
    ' -- "${mac}"
}

function h-paqet-default-interface-linux {
    local interface

    interface="$(command ip -4 route get 1.1.1.1 2>/dev/null |
        command perl -nle 'if (/\bdev\s+(\S+)/) { print $1; exit }')" || true

    if test -n "${interface}" ; then
        ec "${interface}"
        return 0
    fi

    command ip -4 route show default 2>/dev/null |
        command perl -nle 'if (/\bdev\s+(\S+)/) { print $1; exit }'
}

function h-paqet-default-interface-darwin {
    command route -n get default 2>/dev/null |
        command awk '/interface:/{print $2; exit}'
}

function h-paqet-default-interface {
    local interface="${paqet_interface:-}"
    if test -n "${interface}" ; then
        ec "${interface}"
        return 0
    fi

    local platform
    platform="$(h-paqet-platform)" @TRET

    case "${platform}" in
        linux)
            h-paqet-default-interface-linux
            ;;
        darwin)
            h-paqet-default-interface-darwin
            ;;
    esac
}

function h-paqet-local-ip-linux {
    local interface="${1}"
    local local_ip

    local_ip="$(command ip -4 addr show dev "${interface}" scope global 2>/dev/null |
        command perl -nle 'if (/\binet\s+([0-9.]+)\//) { print $1; exit }')" || true

    if test -n "${local_ip}" ; then
        ec "${local_ip}"
        return 0
    fi

    command ip -4 route get 1.1.1.1 2>/dev/null |
        command perl -nle 'if (/\bsrc\s+([0-9.]+)/) { print $1; exit }'
}

function h-paqet-local-ip-darwin {
    local interface="${1}"

    command ipconfig getifaddr "${interface}" 2>/dev/null && return 0

    command ifconfig "${interface}" 2>/dev/null |
        command perl -nle 'if (/\binet\s+([0-9.]+)/ && $1 ne "127.0.0.1") { print $1; exit }'
}

function h-paqet-local-ip {
    local local_ip="${paqet_local_ip:-}"
    if test -n "${local_ip}" ; then
        ec "${local_ip}"
        return 0
    fi

    local interface="${1:-}"
    if test -z "${interface}" ; then
        interface="$(h-paqet-default-interface)" @TRET
    fi

    local platform
    platform="$(h-paqet-platform)" @TRET

    case "${platform}" in
        linux)
            h-paqet-local-ip-linux "${interface}"
            ;;
        darwin)
            h-paqet-local-ip-darwin "${interface}"
            ;;
    esac
}

function h-paqet-gateway-ip-linux {
    command ip -4 route show default 2>/dev/null |
        command perl -nle 'if (/\bvia\s+([0-9.]+)/) { print $1; exit }'
}

function h-paqet-gateway-ip-darwin {
    command route -n get default 2>/dev/null |
        command awk '/gateway:/{print $2; exit}'
}

function h-paqet-gateway-ip {
    local gateway_ip="${paqet_gateway_ip:-}"
    if test -n "${gateway_ip}" ; then
        ec "${gateway_ip}"
        return 0
    fi

    local platform
    platform="$(h-paqet-platform)" @TRET

    case "${platform}" in
        linux)
            h-paqet-gateway-ip-linux
            ;;
        darwin)
            h-paqet-gateway-ip-darwin
            ;;
    esac
}

function h-paqet-router-mac-linux {
    local interface="${1}"
    local gateway_ip="${2}"

    command ping -c 1 -W 1 "${gateway_ip}" >/dev/null 2>&1 || true

    local mac
    mac="$(command ip neigh show "${gateway_ip}" dev "${interface}" 2>/dev/null |
        command perl -nle 'if (/\blladdr\s+([0-9a-fA-F:-]+)/) { print $1; exit }')" || true

    if test -z "${mac}" ; then
        mac="$(command arp -n "${gateway_ip}" 2>/dev/null | command awk 'NR > 1 { print $3; exit }')" || true
    fi

    h-paqet-normalize-mac "${mac}"
}

function h-paqet-router-mac-darwin {
    local gateway_ip="${2}"
    local mac

    command ping -c 1 -t 1 "${gateway_ip}" >/dev/null 2>&1 || true

    mac="$(command arp -n "${gateway_ip}" 2>/dev/null | command awk '{print $4; exit}')" || true
    h-paqet-normalize-mac "${mac}"
}

function h-paqet-router-mac {
    local router_mac="${paqet_router_mac:-}"
    if test -n "${router_mac}" ; then
        h-paqet-normalize-mac "${router_mac}" @RET
        return 0
    fi

    local interface="${1:-}"
    if test -z "${interface}" ; then
        interface="$(h-paqet-default-interface)" @TRET
    fi

    local gateway_ip="${2:-}"
    if test -z "${gateway_ip}" ; then
        gateway_ip="$(h-paqet-gateway-ip)" @TRET
    fi

    local platform
    platform="$(h-paqet-platform)" @TRET

    case "${platform}" in
        linux)
            h-paqet-router-mac-linux "${interface}" "${gateway_ip}"
            ;;
        darwin)
            h-paqet-router-mac-darwin "${interface}" "${gateway_ip}"
            ;;
    esac
}

function paqet-config-network-update {
    local config_path="${1}"
    local interface="${2}"
    local addr="${3}"
    local router_mac="${4}"

    assert test -n "${config_path}" @RET
    assert test -n "${interface}" @RET
    assert test -n "${addr}" @RET
    assert test -n "${router_mac}" @RET

    command perl -0pi -e '
        BEGIN {
            our $interface = shift @ARGV;
            our $addr = shift @ARGV;
            our $router_mac = shift @ARGV;
        }
        our ($interface, $addr, $router_mac);
        my $replacement = qq{network:\n  interface: "$interface"  # Network interface (en0, eth0, wlan0, etc.)\n  ipv4:\n    addr: "$addr:0"  # Local IP; port 0 requests a random source port\n    router_mac: "$router_mac"  # Gateway/router MAC address\n};
        s/^network:\n(?:^[ \t].*\n)*/$replacement/m or die "network block not found\n";
    ' "${interface}" "${addr}" "${router_mac}" "${config_path}" @RET
}

function paqet-proxy-listen-get {
    local config_path="${1:-${paqet_config_path:-${HOME}/paqet/config.yaml}}"
    assert test -n "${config_path}" @RET

    command perl -ne '
        if (/^socks5:\s*$/) { $in = 1; next }
        if ($in && /^\S/ && !/^socks5:\s*$/) { $in = 0 }
        if ($in && /^\s*-\s*listen:\s*["\047]?([^"\047\s#]+)["\047]?/) { print "$1\n"; exit }
    ' "${config_path}"
}

function paqet-on {
    local config_path="${1:-${paqet_config_path:-${HOME}/paqet/config.yaml}}"
    local paqet_bin="${paqet_binary:-${commands[paqet]:-}}"
    local session_name="${paqet_session_name:-paqet-client}"
    local startup_sleep="${paqet_startup_sleep:-1}"
    local ip_test_url="${paqet_ip_test_url:-https://api.ipify.org}"
    local proxy_listen="${paqet_proxy_listen:-}"
    local interface addr gateway_ip router_mac i ipify_out

    assert test -n "${paqet_bin}" @RET
    assert test -f "${config_path}" @RET
    ensure-cmd curl tmux perl @RET

    case "$(h-paqet-platform)" in
        linux)
            ensure-cmd ip ping @RET
            ;;
        darwin)
            ensure-cmd route ipconfig arp ping @RET
            ;;
    esac

    interface="$(h-paqet-default-interface)" @TRET
    addr="$(h-paqet-local-ip "${interface}")" @TRET
    gateway_ip="$(h-paqet-gateway-ip)" @TRET
    router_mac="$(h-paqet-router-mac "${interface}" "${gateway_ip}")" @TRET

    assert test -n "${interface}" @RET
    assert test -n "${addr}" @RET
    assert test -n "${gateway_ip}" @RET
    assert test -n "${router_mac}" @RET

    paqet-config-network-update "${config_path}" "${interface}" "${addr}" "${router_mac}" @RET

    tmuxnew "${session_name}" sudo "${paqet_bin}" run -c "${config_path}" @RET

    sleep "${startup_sleep}"
    if test -z "${proxy_listen}" ; then
        proxy_listen="$(paqet-proxy-listen-get "${config_path}")" @TRET
    fi
    proxy_listen="${proxy_listen:-127.0.0.1:1040}"

    for i in {1..10} ; do
        ipify_out="$(command curl --fail --silent --show-error --connect-timeout 2 --max-time 10 --proxy "socks5h://${proxy_listen}" "${ip_test_url}" 2>&1)" && { ec "${ipify_out}" ; return 0 }
        sleep 1
    done

    ecerr "${ipify_out}"
    return 1
}

function paqet-off {
    local session_name="${paqet_session_name:-paqet-client}"

    ensure-cmd tmux @RET

    if command tmux has-session -t "${session_name}" &> /dev/null ; then
        tmux-session-processes-kill "${session_name}" @RET
        ec "paqet down: ${session_name}"
    else
        ec "paqet already down: ${session_name}"
    fi
}
