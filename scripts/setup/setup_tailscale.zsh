#!/usr/bin/env zsh
# Install and enable Tailscale without storing tailnet, account, hostname, or
# auth-key material in this public repository.

emulate -L zsh
setopt err_return no_unset pipe_fail

function log {
    print -ru2 -- "==> $*"
}

function ok {
    print -ru2 -- "  ok $*"
}

function warn {
    print -ru2 -- "warn $*"
}

function die {
    print -ru2 -- " err $*"
    return 1
}

function have {
    command -v "$1" >/dev/null 2>&1
}

function run {
    print -ru2 -- "  $ ${(j: :)${(q-)@}}"
    command "$@"
}

function as-root {
    if (( EUID == 0 )) ; then
        run "$@"
    else
        have sudo || die "sudo is required for Linux Tailscale setup"
        run sudo -kA "$@"
    fi
}

function usage {
    print -r -- "Usage: setup/setup_tailscale.zsh [--install-only]"
    print -r -- ""
    print -r -- "Installs Tailscale, starts/enables the daemon or app, then runs"
    print -r -- "the public-safe login flow. No auth keys or tailnet names are"
    print -r -- "stored in this repository."
}

typeset -g install_only_p=n

while (( $# > 0 )) ; do
    case "$1" in
        --install-only)
            install_only_p=y
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            usage >&2
            die "unknown argument: $1"
            ;;
    esac
done

function tailscale-login-needed-p {
    have tailscale || return 0
    command tailscale ip --4 >/dev/null 2>&1 && return 1
    return 0
}

function setup-tailscale-darwin {
    have brew || die "Homebrew is required to install Tailscale on macOS"

    if command brew list --cask tailscale-app >/dev/null 2>&1 || [[ -d /Applications/Tailscale.app ]] ; then
        ok "Tailscale.app already installed"
    else
        log "installing Tailscale standalone app"
        run brew install --cask tailscale-app
    fi

    log "opening Tailscale"
    run open -a Tailscale

    if [[ "${install_only_p}" == y ]] ; then
        ok "install-only requested; finish login in the Tailscale app"
        return 0
    fi

    if tailscale-login-needed-p ; then
        if have tailscale ; then
            log "starting Tailscale login flow"
            command tailscale up || warn "finish onboarding in the Tailscale app"
        else
            warn "tailscale CLI is not on PATH yet; finish onboarding in the Tailscale app"
        fi
    else
        ok "Tailscale already has an IPv4 address"
    fi
}

function install-tailscale-linux {
    if have tailscale ; then
        ok "tailscale already installed: $(command -v tailscale)"
        return 0
    fi

    have curl || die "curl is required to fetch Tailscale's official Linux installer"

    local installer
    installer="$(command mktemp "${TMPDIR:-/tmp}/tailscale-install.XXXXXXXX.sh")"
    {
        log "fetching Tailscale Linux installer"
        run curl --fail --location --show-error --silent \
            --output "${installer}" \
            https://tailscale.com/install.sh
        as-root sh "${installer}"
    } always {
        command rm -f -- "${installer}" || true
    }

    have tailscale || die "tailscale command was not installed"
}

function start-tailscale-linux {
    if have systemctl ; then
        log "enabling tailscaled via systemd"
        as-root systemctl enable --now tailscaled
    elif have service ; then
        log "starting tailscaled via service(8)"
        as-root service tailscaled start
        warn "no systemctl found; verify Tailscale autostart for this init system"
    else
        warn "no supported service manager found; start tailscaled manually"
    fi
}

function setup-tailscale-linux {
    install-tailscale-linux
    start-tailscale-linux

    if [[ "${install_only_p}" == y ]] ; then
        ok "install-only requested; run 'sudo -kA tailscale up' when ready to log in"
        return 0
    fi

    if tailscale-login-needed-p ; then
        log "starting Tailscale login flow"
        as-root tailscale up
    else
        ok "Tailscale already has an IPv4 address"
    fi
}

case "$(command uname -s)" in
    Darwin)
        setup-tailscale-darwin
        ;;
    Linux)
        setup-tailscale-linux
        ;;
    *)
        die "unsupported OS: $(command uname -s)"
        ;;
esac

log "done"
