##
function rm-deleteus {
    trs-rm "$deleteusdir"
    ##
    ! test -e "$deleteus" && return 0
    trs-rm "${(@f)$(<"$deleteus")}"
    mv "$deleteus" "$deleteus.pbak"
}
##
function rm-caches-sudo {
    if isLinux && ! isBorg ; then

        sudo apt-get clean
        sudo apt-get autoclean
        sudo apt-get autoremove -y

        sudo journalctl --vacuum-time=1s
        # sudo rm -rf /var/log/* #: This can break things that depend on some directories existing. Delete manually via ncdu.
    fi

    if isDarwin ; then
        sudo rm -rf /Library/Logs
    fi
}


function rm-caches {
    rm-deleteus
    trs-rm ~/.local/share/Trash/*(DN)
    trs-rm ~/sent
    trs-rm /tmp/*.png(DN) #: wallpaper-auto can leave garbage here, which on a long-lived server will obviously not clear on the nonexistent reboots

    trs-rm ~/logs/*(DN) ~/log/*(DN) #: do NOT delete these folders themselves

    trs-rm ~/julia_tmp/*(DN) ~/tmp-kindle/*(DN)
    trs-rm ~/tmp/hs_whisper
    trs-rm "$(brew --cache)"

    pip cache remove '*'
    if isdefined conda ; then
        conda clean --all --yes #: STILL does not delete old python version stuff!
    fi

    trash-empty-all
}

function rm-caches-all {
    rm-caches

    if isSudo || isI; then
        rm-caches-sudo
    else
        ecgray "$0: sudo needed for rm-caches-sudo; skipped"
    fi
}
##
function cleanup {
    reval-ec rm-caches-all
    reval-ec trash-empty-all
    isLocal && reval-ec clean-deps
    reval-ec brew cleanup
}
##
function h-cargo-root {
    #: The directory whose `target/` a cargo build would use: the nearest
    #: Cargo.toml at or above $1, else an unambiguous one just below it (Tauri
    #: keeps its crate in `src-tauri/`, so the repo root has no manifest).
    local start="${1:-$PWD}"
    start="${start:A}"

    local dir="${start}"
    while [[ "${dir}" != / ]] ; do
        if test -e "${dir}/Cargo.toml" ; then
            ec "${dir}"
            return 0
        fi

        dir="${dir:h}"
    done

    local candidates=("${start}"/*/Cargo.toml(N))
    if (( ${#candidates} == 1 )) ; then
        ec "${candidates[1]:h}"
        return 0
    elif (( ${#candidates} > 1 )) ; then
        ecerr "$0: several cargo projects under ${start}; pass one explicitly:"

        local c
        for c in "${candidates[@]}" ; do
            ecerr "  ${c:h}"
        done

        return 1
    fi

    ecerr "$0: no Cargo.toml at, above or just below: ${start}"
    return 1
}

function cargo-sweep-here {
    #: Reclaim the dead half of a cargo target dir without paying for it later.
    #:
    #: Cargo never garbage-collects. Every change of feature flags, dependency
    #: version or toolchain compiles a fresh copy under a new hash and orphans
    #: the old one forever, so a long-lived target dir is mostly generations no
    #: build will ever open again. Handy's was 19G, of which ~70% was that.
    #:
    #: We build both profiles, then let cargo-sweep drop everything the current
    #: build graph does not reference. What goes is by construction what no
    #: future build could have reused, so this frees disk at zero rebuild cost.
    #: The builds are the mechanism, not a courtesy: sweep keeps what a build
    #: just claimed, so skipping a profile deletes that profile. For the same
    #: reason a failed build must not reach the sweep, hence @RET on both.
    #:
    #: Local tests only: `cargo test --no-run` targets are not in the default
    #: build graph, so they are swept. Set cargo_sweep_here_build_opts to
    #: (--workspace --all-targets) to keep them.
    ##
    ensure-cmd cargo cargo-sweep @RET

    local dry_run_p="${cargo_sweep_here_dry_run_p:-n}"

    ensure-array cargo_sweep_here_build_opts
    local build_opts=("${cargo_sweep_here_build_opts[@]}")
    if (( ${#build_opts} == 0 )) ; then
        #: --workspace so members outside the default build graph are not
        #: orphaned; in a single-crate project it costs nothing.
        build_opts=(--workspace)
    fi

    local root
    root="$(h-cargo-root "${1}")" @TRET

    local sweep_opts=()
    if bool "${dry_run_p}" ; then
        sweep_opts+=(--dry-run)
    fi

    pushf "${root}" @RET
    {
        ecgray "$0: ${root} (target: $(h-cargo-sweep-here-size ./target))"

        assert cargo sweep --stamp . @RET
        {
            reval-ec cargo build "${build_opts[@]}" @RET
            reval-ec cargo build --release "${build_opts[@]}" @RET

            reval-ec cargo sweep --file "${sweep_opts[@]}" . @RET
        } always {
            trs-rm ./sweep.timestamp @STRUE
        }

        ecgray "$0: done (target: $(h-cargo-sweep-here-size ./target))"
    } always {
        popf
    }
}

function h-cargo-sweep-here-size {
    #: Cosmetic, so never worth failing the sweep over.
    local dir="${1}"

    isdefined-cmd gdu || return 0

    local out
    out="$(command gdu --summarize --human-readable -- "${dir}" 2>/dev/null)" || return 0
    ec "${out%%$'\t'*}"
}
##
