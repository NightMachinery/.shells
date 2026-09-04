#!/usr/bin/env zsh
# Install and enable Tailscale without storing tailnet, account, hostname, or
# auth-key material in this public repository.

emulate -L zsh
export PS4='> '
setopt LOCAL_OPTIONS PIPE_FAIL PRINT_EXIT_VALUE ERR_RETURN SOURCE_TRACE XTRACE
setopt TYPESET_SILENT NO_CASE_GLOB multios re_match_pcre extendedglob pipefail interactivecomments hash_executables_only
setopt NO_BANG_HIST 2>/dev/null || true

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
    exit 1
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
        run "$@" || die "root command failed: ${(j: :)${(q-)@}}"
    else
        have sudo || die "sudo is required for Linux Tailscale setup"
        run sudo -kA "$@" || die "root command failed: sudo -kA ${(j: :)${(q-)@}}"
    fi
}

function usage {
    print -r -- "Usage: setup/setup_tailscale.zsh [--install-only]"
    print -r -- ""
    print -r -- "Installs the Tailscale CLI daemon, starts/enables it, then runs"
    print -r -- "the public-safe login flow. GUI removal is reported and confirmed"
    print -r -- "before the script uninstalls anything. No auth keys or tailnet"
    print -r -- "names are stored in this repository."
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

function confirm-destructive {
    local action="$1"
    local answer=''

    warn "${action}"
    if [[ ! -t 0 ]] ; then
        warn "stdin is not interactive; skipping destructive action"
        return 1
    fi

    print -ru2 -- "Type 'yes' to continue:"
    read -r answer
    [[ "${answer}" == yes ]]
}

function tailscale-bin {
    if [[ -n "${tailscale_cli_bin:-}" ]] ; then
        print -r -- "${tailscale_cli_bin}"
        return 0
    fi

    if have brew && command brew list --formula tailscale >/dev/null 2>&1 ; then
        print -r -- "$(command brew --prefix tailscale)/bin/tailscale"
        return 0
    fi

    if have tailscale ; then
        command -v tailscale
        return 0
    fi

    return 1
}

function tailscale-login-needed-p {
    local tailscale_bin
    tailscale_bin="$(tailscale-bin)" || return 0
    command "${tailscale_bin}" ip --4 >/dev/null 2>&1 && return 1
    return 0
}

function remove-tailscale-gui-darwin {
    have brew || die "Homebrew is required to install Tailscale on macOS"

    local cask_installed_p=n
    local app_present_p=n

    command brew list --cask tailscale-app >/dev/null 2>&1 && cask_installed_p=y
    [[ -d /Applications/Tailscale.app ]] && app_present_p=y

    if [[ "${cask_installed_p}" != y && "${app_present_p}" != y ]] ; then
        ok "Tailscale GUI app not present"
        return 0
    fi

    warn "Tailscale GUI app detected"
    [[ "${cask_installed_p}" == y ]] && warn "Homebrew cask installed: tailscale-app"
    [[ "${app_present_p}" == y ]] && warn "Application bundle exists: /Applications/Tailscale.app"

    if [[ "${cask_installed_p}" == y ]] ; then
        if confirm-destructive "Uninstall Homebrew cask 'tailscale-app'?" ; then
            run brew uninstall --cask tailscale-app || die "failed to uninstall tailscale-app cask"
        else
            warn "leaving Homebrew cask installed"
        fi
    fi

    if [[ -d /Applications/Tailscale.app ]] ; then
        if confirm-destructive "Remove remaining /Applications/Tailscale.app bundle?" ; then
            as-root /bin/rm -rf /Applications/Tailscale.app
        else
            warn "leaving /Applications/Tailscale.app in place"
        fi
    fi

    if command brew list --cask tailscale-app >/dev/null 2>&1 || [[ -d /Applications/Tailscale.app ]] ; then
        die "Tailscale GUI app is still present; not installing CLI variant alongside it"
    fi
}

function install-tailscale-darwin-cli {
    have brew || die "Homebrew is required to install Tailscale CLI on macOS"

    if command brew list --formula tailscale >/dev/null 2>&1 ; then
        ok "Tailscale CLI formula already installed"
    else
        log "installing Tailscale CLI formula"
        run brew install tailscale || die "failed to install Tailscale CLI formula"
    fi

    typeset -g tailscale_cli_bin="$(command brew --prefix tailscale)/bin/tailscale"
    [[ -x "${tailscale_cli_bin}" ]] || die "tailscale CLI not executable: ${tailscale_cli_bin}"
}

function start-tailscale-darwin-cli {
    log "starting Tailscale CLI daemon via Homebrew services"
    as-root brew services start tailscale
}

function setup-tailscale-darwin {
    remove-tailscale-gui-darwin
    install-tailscale-darwin-cli
    start-tailscale-darwin-cli

    if [[ "${install_only_p}" == y ]] ; then
        ok "install-only requested; run 'sudo -kA ${tailscale_cli_bin} up' when ready to log in"
        return 0
    fi

    if tailscale-login-needed-p ; then
        log "starting Tailscale CLI login flow"
        as-root "${tailscale_cli_bin}" up
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
            https://tailscale.com/install.sh || die "failed to fetch Tailscale Linux installer"
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
