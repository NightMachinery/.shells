#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 55: the coding-agent CLIs, plus [agfi:llm].
#:
#: All of these are installed as NATIVE binaries rather than through npm, and
#: they are left free to update themselves. That is a deliberate departure
#: from the "one owner per binary" rule that stages 20/45 follow, so it is
#: worth saying why:
#:
#:   - They ship real per-platform binaries, so npm would only buy us a node
#:     runtime dependency shared by four tools that do not otherwise need one.
#:   - They release very frequently, and their self-updaters are the supported
#:     path. Pinning them in a lockfile would mean pinning something stale
#:     within days.
#:
#: The cost is that the installed version drifts from anything recorded here,
#: and that several hosts share one $HOME on a shared-home site, so two hosts
#: can try to self-update the same binary at once. We accept that.
#:
#: @note llm is the exception: it is a Python tool, so it gets a uv-managed
#: venv. Several zshlang functions call it -- see [agfi:llm-run] and friends.

# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

ensure_dir "${NIGHT_BIN}"

##
#: --- platform ---
#: These projects each spell the architecture differently, so normalise once.
_uname_m="$(uname -m)"
case "${_uname_m}" in
    x86_64|amd64)  rust_arch=x86_64  ; oc_arch=x64   ;;
    arm64|aarch64) rust_arch=aarch64 ; oc_arch=arm64 ;;
    *) warn "stage 55: unsupported architecture ${_uname_m}; skipping the agent CLIs"
       return 0 2>/dev/null || exit 0 ;;
esac

#: @warn Pick the glibc build, not the musl one. The musl tarballs are not
#: static: their ELF interpreter is /lib/ld-musl-x86_64.so.1, which a glibc
#: host does not have, and the binary dies with the thoroughly unhelpful
#: "cannot execute: required file not found". Verified on beta.
if [ -f /lib/libc.musl-x86_64.so.1 ] || ldd /bin/ls 2>&1 | grep -q musl ; then
    libc=musl
else
    libc=gnu
fi

##
#: --- helper: newest non-prerelease tag matching a pattern ---
#: `releases/latest' is not usable for every project here: openai/codex tags
#: alphas (rust-v0.148.0-alpha.6) that sort above the newest stable release.
night_latest_tag() {
    repo="$1" ; pattern="$2"
    fetch_stdout "https://api.github.com/repos/${repo}/releases" 2>/dev/null \
        | grep -oE '"tag_name": "[^"]*"' \
        | sed 's/.*: "//;s/"$//' \
        | grep -E "${pattern}" \
        | head -1
}

#: night_install_tarball URL BINARY_NAME
#: Unpacks a release tarball and installs the single executable it contains.
night_install_tarball() {
    url="$1" ; name="$2"
    tmp="$(mktemp -d "${NIGHT_LOCAL_CACHE:-/tmp}/${name}.XXXXXX")" || return 1
    if ! fetch "${url}" "${tmp}/a.tar.gz" ; then
        rm -rf "${tmp}" ; return 1
    fi
    if ! tar -xzf "${tmp}/a.tar.gz" -C "${tmp}" ; then
        rm -rf "${tmp}" ; return 1
    fi
    #: The archives are not consistent about nesting or about naming the
    #: binary after the project, so find the executable rather than guess.
    bin="$(find "${tmp}" -type f -perm -u+x ! -name '*.tar.gz' | head -1)"
    if [ -z "${bin}" ] ; then
        rm -rf "${tmp}" ; return 1
    fi
    install -m 755 "${bin}" "${NIGHT_BIN}/${name}"
    rm -rf "${tmp}"
}

##
#: --- claude code ---
#: The installer is genuinely native: it resolves a version, downloads one
#: per-platform binary from downloads.claude.ai, and checks it against a
#: sha256 in manifest.json. It contains no reference to node or npm.
#: Downloaded and then run, rather than piped straight into a shell, so a
#: truncated or redirected response cannot execute half a script.
if have claude && [ -z "${NIGHT_BOOTSTRAP_FORCE:-}" ] ; then
    ok "claude code present ($(claude --version 2>/dev/null | head -1))"
else
    log "installing claude code"
    claude_installer="${NIGHT_LOCAL_CACHE:-/tmp}/claude-install.sh"
    #: @warn Must run under bash, not sh. The installer uses [[ ]] and the =~
    #: regex operator, so under dash (which /bin/sh is on Debian/Ubuntu) it
    #: dies on a syntax error -- and because the failure is soft, the stage
    #: otherwise reports success while installing nothing.
    if ! have bash ; then
        warn "claude code's installer needs bash, which is not on PATH"
    elif fetch "https://claude.ai/install.sh" "${claude_installer}" ; then
        run_soft bash "${claude_installer}"
        rm -f "${claude_installer}"
    else
        warn "could not download the claude code installer"
    fi
fi

##
#: --- codex ---
#: Stable tags look like rust-v0.147.0; alphas like rust-v0.148.0-alpha.6 are
#: excluded by requiring the tag to end at the patch number.
if have codex && [ -z "${NIGHT_BOOTSTRAP_FORCE:-}" ] ; then
    ok "codex present ($(codex --version 2>/dev/null | head -1))"
else
    log "installing codex"
    codex_tag="$(night_latest_tag openai/codex '^rust-v[0-9]+\.[0-9]+\.[0-9]+$')"
    if [ -n "${codex_tag}" ] ; then
        #: codex publishes only -musl for linux, and unlike opencode's it is
        #: statically linked, so it runs on glibc hosts too. Verified on beta.
        codex_asset="codex-${rust_arch}-unknown-linux-musl.tar.gz"
        if night_install_tarball \
            "https://github.com/openai/codex/releases/download/${codex_tag}/${codex_asset}" \
            codex ; then
            ok "codex ${codex_tag}"
        else
            warn "could not install codex ${codex_tag} (${codex_asset})"
        fi
    else
        warn "could not resolve a stable codex release tag"
    fi
fi

##
#: --- opencode ---
#: [agfi:pxaify-command] already expects `opencode' on PATH.
if have opencode && [ -z "${NIGHT_BOOTSTRAP_FORCE:-}" ] ; then
    ok "opencode present ($(opencode --version 2>/dev/null | head -1))"
else
    log "installing opencode"
    oc_tag="$(night_latest_tag sst/opencode '^v[0-9]+\.[0-9]+\.[0-9]+$')"
    if [ -n "${oc_tag}" ] ; then
        #: -baseline targets the older x86-64 feature levels; the plain build
        #: assumes x86-64-v2 or newer. On arm64 there is no baseline variant.
        if [ "${oc_arch}" = x64 ] ; then
            oc_asset="opencode-linux-x64-baseline.tar.gz"
            [ "${libc}" = musl ] && oc_asset="opencode-linux-x64-baseline-musl.tar.gz"
        else
            oc_asset="opencode-linux-arm64.tar.gz"
            [ "${libc}" = musl ] && oc_asset="opencode-linux-arm64-musl.tar.gz"
        fi
        if night_install_tarball \
            "https://github.com/sst/opencode/releases/download/${oc_tag}/${oc_asset}" \
            opencode ; then
            ok "opencode ${oc_tag}"
        else
            warn "could not install opencode ${oc_tag} (${oc_asset})"
        fi
    else
        warn "could not resolve an opencode release tag"
    fi
fi

##
#: --- antigravity (agy) ---
#: [agfi:antigravity] runs `agy'; see PE/Agents/readme.org.
#:
#: @warn Its installer ends by running `agy install', which appends a PATH
#: line to the shell rc files. That is wrong for us twice over: those files
#: are tracked by vcsh, so it silently dirties the repo -- and blocks the next
#: `vcsh night.sh pull' with "local changes would be overwritten" -- and the
#: line hardcodes an ABSOLUTE path ($HOME expanded on the machine that ran it)
#: into files shared with every other host, including the laptop, where that
#: path does not exist. NIGHT_BIN is already on PATH via the env contract, so
#: the line is redundant as well as harmful. There is no flag to suppress it.
#:
#: @warn It writes to FOUR files, not just ~/.profile: .zshrc, .zprofile,
#: .bashrc and .bash_profile were all modified on beta. Reverting only the one
#: named in its log output left the other three dirty. Snapshot the whole set.
night_agy_rcfiles=".profile .zshrc .zprofile .zshenv .zlogin .bashrc .bash_profile .bash_login"

if have agy && [ -z "${NIGHT_BOOTSTRAP_FORCE:-}" ] ; then
    ok "agy present ($(agy --version 2>/dev/null | head -1))"
elif ! have bash ; then
    warn "antigravity's installer needs bash, which is not on PATH"
else
    log "installing antigravity (agy)"
    agy_installer="${NIGHT_LOCAL_CACHE:-/tmp}/agy-install.sh"
    agy_snap="$(mktemp -d "${NIGHT_LOCAL_CACHE:-/tmp}/agy-rc.XXXXXX")"

    #: @warn Plain `cp', never `cp -p'. The snapshot lands in NIGHT_LOCAL_CACHE
    #: (/var/tmp, ext4) while $HOME is NFS with ACLs, and -p then fails with
    #: "preserving permissions: Operation not supported" -- which, under
    #: `set -e', aborted the whole stage. Modes do not need preserving here:
    #: the restore copies onto an existing file, which keeps its own mode.
    for f in ${night_agy_rcfiles} ; do
        if [ -f "${HOME}/${f}" ] ; then
            cp "${HOME}/${f}" "${agy_snap}/${f}"
        fi
    done

    if fetch "https://antigravity.google/cli/install.sh" "${agy_installer}" ; then
        run_soft bash "${agy_installer}" --dir "${NIGHT_BIN}"
        rm -f "${agy_installer}"

        #: Restore only files that gained the installer's own marker; anything
        #: else that changed meanwhile is not ours to revert.
        for f in ${night_agy_rcfiles} ; do
            [ -f "${HOME}/${f}" ] || continue
            grep -q 'Added by Antigravity CLI installer' "${HOME}/${f}" 2>/dev/null || continue
            if [ -f "${agy_snap}/${f}" ] ; then
                cp "${agy_snap}/${f}" "${HOME}/${f}"
                ok "reverted antigravity's PATH line in ~/${f}"
            else
                #: The installer created this file; it did not exist before.
                rm -f "${HOME}/${f}"
                ok "removed ~/${f}, created by antigravity's installer"
            fi
        done
    else
        warn "could not download the antigravity installer"
    fi
    rm -rf "${agy_snap}"
fi

##
#: --- llm (Simon Willison's) ---
#: Python, so it gets its own uv-managed venv rather than a release binary.
#: python/requirements.txt pins the plugin set: llm and llm-gemini.
#:
#: @warn Plugins must be added with `llm install', which installs into llm's
#: OWN venv. `uv tool install llm-gemini' would create a second, separate
#: venv and the plugin would never register with llm.
if have uv ; then
    if have llm && [ -z "${NIGHT_BOOTSTRAP_FORCE:-}" ] ; then
        ok "llm present ($(llm --version 2>/dev/null | head -1))"
    else
        log "installing llm"
        run_soft uv tool install llm
    fi

    if have llm ; then
        for plugin in llm-gemini ; do
            if llm plugins 2>/dev/null | grep -q "\"${plugin}\"" ; then
                ok "${plugin} registered"
            else
                run_soft llm install "${plugin}"
            fi
        done
    fi
else
    warn "uv not found (stage 20); skipping llm"
fi

##
dim "claude:   $(command -v claude   2>/dev/null || echo '-')"
dim "codex:    $(command -v codex    2>/dev/null || echo '-')"
dim "opencode: $(command -v opencode 2>/dev/null || echo '-')"
dim "agy:      $(command -v agy      2>/dev/null || echo '-')"
dim "llm:      $(command -v llm      2>/dev/null || echo '-')"
