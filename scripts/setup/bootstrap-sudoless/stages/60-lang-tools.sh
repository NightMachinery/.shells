#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 60: language runtimes, and the dotfiles' own Go/Rust programs.
#:
#: setup/ins_go lists programs NightMachinary wrote that the shell calls
#: directly -- `ntagcolor` is required by the `l` listing wrapper via
#: FZF_PREVIEW_NTAG, and its absence is what produced:
#:   command not found: ntagcolor (exited 127)
#: So this stage is part of "the server works", not an optional extra.

# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

have mise || die "stage 20 (mise) must run first"
export MISE_YES=1
export GIT_TERMINAL_PROMPT=0

##
#: --- runtimes ---
for rt in go node rust ; do
    if mise which "${rt}" >/dev/null 2>&1 ; then
        dim "${rt} already installed"
    else
        log "mise use -g ${rt}"
        run_soft mise use -g --yes "${rt}@latest"
    fi
done

path_prepend "${MISE_DATA_DIR:-${HOME}/.local/share/mise}/shims"

##
#: --- where build caches and binaries go ---
#: Module/registry caches are GBs of small files: big store, not the 48 GB home.
#: Binaries go to NIGHT_BIN so they are on PATH on every host at once.
if ! grep -q 'GOPATH' "${HOME}/.night-bootstrap.env" 2>/dev/null ; then
    cat >> "${HOME}/.night-bootstrap.env" <<'EOF'

#: Go / Rust (stage 60). Caches on the big store; binaries on PATH.
export GOPATH="${NIGHT_BIG_STORE}/go"
export GOMODCACHE="${NIGHT_BIG_STORE}/go/pkg/mod"
export GOBIN="${NIGHT_BIN}"
export CARGO_HOME="${NIGHT_BIG_STORE}/cargo"
export RUSTUP_HOME="${NIGHT_BIG_STORE}/rustup"
case ":${PATH}:" in
    *":${CARGO_HOME}/bin:"*) : ;;
    *) export PATH="${PATH}:${CARGO_HOME}/bin" ;;
esac
EOF
    ok "recorded GOPATH/CARGO_HOME in the env contract"
fi
# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

ensure_dir "${GOPATH}"
ensure_dir "${CARGO_HOME}"

##
#: --- the dotfiles' own Go programs ---
ins_go_list="${NIGHTDIR}/setup/ins_go"
if ! have go ; then
    warn "go unavailable; skipping ${ins_go_list}"
else
    dim "go $(go version 2>&1 | awk '{print $3}')  GOBIN=${GOBIN}"

    #: Only the ones the shell actually calls. The full ins_go list contains
    #: long-abandoned packages that no longer build against modern Go, and a
    #: failed `go install` there would be noise, not signal.
    night_go_pkgs='
github.com/NightMachinary/ntagcolor
github.com/NightMachinary/prefixer
github.com/NightMachinary/jalalicli
github.com/NightMachinary/ntom
github.com/NightMachinary/possiblycat
'
    go_failed=''
    for p in ${night_go_pkgs} ; do
        name="${p##*/}"
        if have "${name}" ; then
            dim "${name} already installed"
            continue
        fi
        log "go install ${p}@latest"
        if ! run go install "${p}@latest" ; then
            go_failed="${go_failed} ${name}"
        fi
    done
    [ -n "${go_failed}" ] && warn "go install failed for:${go_failed}"
fi

##
#: --- Rust programs the shell calls ---
if ! have cargo ; then
    warn "cargo unavailable; skipping the rust programs"
else
    #: rtl_reshaper_rs is called by the RTL command-modifier in .zshrc; the
    #: others are used by assorted helpers. Kept short on purpose.
    night_cargo_gits='
https://github.com/NightMachinary/rtl_reshaper_rs
https://github.com/NightMachinary/rmprefix
'
    for g in ${night_cargo_gits} ; do
        name="${g##*/}"
        if have "${name}" ; then
            dim "${name} already installed"
            continue
        fi
        log "cargo install --git ${g}"
        run_soft env CARGO_NET_GIT_FETCH_WITH_CLI=true \
            cargo install --quiet --force --root "${NIGHT_BIG_STORE}/cargo" --git "${g}"
    done
fi

##
log "results"
for c in ntagcolor prefixer go node cargo ; do
    if command -v "${c}" >/dev/null 2>&1 ; then
        dim "$(printf '%-12s %s' "${c}" "$(command -v "${c}")")"
    else
        warn "$(printf '%-12s MISSING' "${c}")"
    fi
done
