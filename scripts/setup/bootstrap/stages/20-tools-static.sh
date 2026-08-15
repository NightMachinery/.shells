#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 20: static CLI binaries + language runtimes, via mise.
#:
#: Why mise and not brew/conda for these:
#:   - Homebrew on Linux only bottles for /home/linuxbrew/.linuxbrew, which we
#:     cannot create without root, so every formula would build from source.
#:   - conda-forge works but a CLI-tool env is thousands of small files; these
#:     are single static binaries instead.
#:   - Measured: exec latency from NFS == local disk, so ~/.local/bin over NFS
#:     costs nothing and serves all hosts from one install.
#: mise also covers node/go/rust, so we do not need a second mechanism.

# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

##
if have mise ; then
    ok "mise already present: $(mise --version 2>&1 | head -1)"
else
    log "installing mise"
    MISE_INSTALL_PATH="${NIGHT_BIN}/mise" \
        MISE_QUIET=1 \
        sh -c "$(fetch_stdout https://mise.run)" \
        || die "mise install failed"
    path_prepend "${NIGHT_BIN}"
    have mise || die "mise installed but not on PATH"
    ok "mise $(mise --version 2>&1 | head -1)"
fi

##
#: mise wants to know we accept its idea of trust for the global config.
export MISE_YES=1
#: Never let a tool install block on a credential prompt (see stage 30).
export GIT_TERMINAL_PROMPT=0

night_tools='
ripgrep
fd
fzf
jq
bat
delta
eza
zoxide
starship
gh
uv
'

failed=''
for t in ${night_tools} ; do
    if mise which "${t}" >/dev/null 2>&1 ; then
        dim "${t} already installed"
        continue
    fi
    log "mise use -g ${t}"
    if ! mise use -g --yes "${t}@latest" >/dev/null 2>&1 ; then
        warn "mise could not install ${t}"
        failed="${failed} ${t}"
    fi
done

[ -n "${failed}" ] && warn "tools mise could not install:${failed}"

##
#: mise exposes tools through shims; the shim dir must be on PATH.
shim_dir="${MISE_DATA_DIR:-${HOME}/.local/share/mise}/shims"
if [ -d "${shim_dir}" ] ; then
    if ! grep -q 'mise/shims' "${HOME}/.night-bootstrap.env" 2>/dev/null ; then
        cat >> "${HOME}/.night-bootstrap.env" <<EOF

#: mise shims (added by stage 20)
case ":\${PATH}:" in
    *":${shim_dir}:"*) : ;;
    *) export PATH="${shim_dir}:\${PATH}" ;;
esac
EOF
        ok "added mise shims to the env contract"
    fi
    path_prepend "${shim_dir}"
fi

##
#: The registry name is not always the binary name (ripgrep -> rg).
night_tool_bins='rg fd fzf jq bat delta eza zoxide starship gh uv'

log "installed tools"
for b in ${night_tool_bins} ; do
    if command -v "${b}" >/dev/null 2>&1 ; then
        dim "$(printf '%-10s %s' "${b}" "$(command -v "${b}")")"
    else
        warn "$(printf '%-10s MISSING' "${b}")"
    fi
done
