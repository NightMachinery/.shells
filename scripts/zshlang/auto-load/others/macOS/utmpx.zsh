##
#: Stale remote-login rows in /var/run/utmpx, and the root LaunchDaemon that
#: ends them. See [[NIGHTDIR:docs/utmpx-stale-ssh.md]].
##
function h-utmpx-clean-build {
    #: Compiles c/utmpx_clean_stale.c into a fresh temp dir and prints the
    #: binary's path; the caller removes its directory.
    #:
    #: Keep the output named utmpx-clean-stale. The ad-hoc code signature
    #: embeds the file name, and same-named builds are otherwise
    #: byte-identical, which is how [agfi:utmpx-clean-daemon-install] detects
    #: drift with a plain cmp.
    ##
    local dir
    dir="$(command mktemp -d -t utmpx-clean)" @TRET
    if ! /usr/bin/cc -O2 -Wall -Wextra -Werror \
        -o "${dir}/utmpx-clean-stale" "${NIGHTDIR}/c/utmpx_clean_stale.c" ; then
        command rm -rf "$dir"
        return 1
    fi
    print -r -- "${dir}/utmpx-clean-stale"
}

function utmpx-stale-list {
    #: Lists the stale rows the daemon would end. Needs no root, installs
    #: nothing.
    ##
    @darwinOnly
    local bin retcode
    bin="$(h-utmpx-clean-build)" @RET
    "$bin" ; retcode=$?
    command rm -rf "${bin:h}"
    return $retcode
}

function h-utmpx-root-owned-p {
    #: True if $1 is owned by root and writable by nobody else.
    ##
    local owner mode
    owner="$(command stat -f '%Su' "$1")" || return 1
    mode="$(command stat -f '%Lp' "$1")" || return 1
    [[ $owner == root ]] && (( (8#$mode & 8#022) == 0 ))
}

function h-utmpx-clean-dirs-safe-p {
    #: A root-owned file in a directory someone else can write to can be
    #: renamed away and replaced, so check every ancestor of both install
    #: directories, not just the directories themselves.
    ##
    local d p
    for d in /usr/local/libexec /Library/LaunchDaemons ; do
        p="$d"
        while [[ $p != / ]] ; do
            if ! h-utmpx-root-owned-p "$p" ; then
                ecerr "$0: not root-owned, or writable by others: $p"
                return 1
            fi
            p="${p:h}"
        done
    done
}

function utmpx-clean-daemon-install {
    #: Installs, or brings up to date, the LaunchDaemon that ends stale utmpx
    #: rows. Idempotent: when the installed binary and plist match the
    #: repository and the job is loaded, it changes nothing and asks for no
    #: password.
    #:
    #: The daemon runs a root-owned *snapshot*, compiled here and installed
    #: root:wheel 755 in /usr/local/libexec, never the repository's files: a
    #: root job executing a user-writable file is a privilege-escalation path.
    #: Re-run this after editing c/utmpx_clean_stale.c or the plist. A compiler
    #: update also changes the build, so it then reinstalls once.
    ##
    @darwinOnly
    if (( EUID == 0 )) ; then
        ecerr "$0: run as your normal user; it elevates only the install step"
        return 1
    fi

    local label='com.user.utmpx-clean'
    local bin_dst='/usr/local/libexec/utmpx-clean-stale'
    local plist_src="${NIGHTDIR}/launchers/utmpx-clean/${label}.plist"
    local plist_dst="/Library/LaunchDaemons/${label}.plist"
    if ! [[ -r $plist_src ]] ; then
        ecerr "$0: missing ${plist_src}"
        return 1
    fi

    local bin
    bin="$(h-utmpx-clean-build)" || {
        ecerr "$0: compiling the cleaner failed"
        return 1
    }
    {
        local do_bin=y do_plist=y do_load=y
        if [[ -f $bin_dst ]] && command cmp -s "$bin" "$bin_dst" \
            && h-utmpx-root-owned-p "$bin_dst" \
            && [[ "$(command stat -f '%Lp' "$bin_dst")" == 755 ]] ; then
            do_bin=n
        fi
        if [[ -f $plist_dst ]] && command cmp -s "$plist_src" "$plist_dst" \
            && h-utmpx-root-owned-p "$plist_dst" \
            && [[ "$(command stat -f '%Lp' "$plist_dst")" == 644 ]] ; then
            do_plist=n
        fi
        #: Readable without root. A changed plist must be reloaded to apply.
        if [[ $do_plist == n ]] \
            && command launchctl print "system/${label}" &>/dev/null ; then
            do_load=n
        fi

        if [[ "${do_bin}${do_plist}${do_load}" == nnn ]] ; then
            ecgray "$0: up to date"
            return 0
        fi
        h-utmpx-clean-dirs-safe-p @RET

        ecgray "$0: binary: ${do_bin}, plist: ${do_plist}, reload: ${do_load}"
        #: One sudo for every root step: with `sudo -k -A', each separate
        #: call would ask again.
        #:
        #: bootstrap straight after bootout can fail with "5: Input/output
        #: error" while launchd is still tearing the old job down, hence the
        #: retries.
        local -a sudo_cmd=( ${(@f)"$(h-sudo-cmd)"} )
        "${sudo_cmd[@]}" /bin/sh -c '
            set -e
            bin_new=$1 bin_dst=$2 plist_src=$3 plist_dst=$4 label=$5
            do_bin=$6 do_plist=$7 do_load=$8
            if [ "$do_bin" = y ] ; then
                /usr/bin/install -o root -g wheel -m 755 "$bin_new" "${bin_dst}.new"
                /bin/mv -f "${bin_dst}.new" "$bin_dst"
            fi
            if [ "$do_plist" = y ] ; then
                /usr/bin/install -o root -g wheel -m 644 "$plist_src" "$plist_dst"
            fi
            if [ "$do_load" = y ] ; then
                /bin/launchctl bootout "system/${label}" 2>/dev/null || true
                tries=0
                until /bin/launchctl bootstrap system "$plist_dst" ; do
                    tries=$((tries + 1))
                    [ "$tries" -lt 5 ] || exit 1
                    sleep 1
                done
            fi
        ' utmpx-clean-install "$bin" "$bin_dst" "$plist_src" "$plist_dst" \
            "$label" "$do_bin" "$do_plist" "$do_load" @TRET

        command launchctl print "system/${label}" 2>/dev/null \
            | command grep -E '^\s*(state|last exit code|run interval) ='
        return 0
    } always {
        command rm -rf "${bin:h}"
    }
}

function utmpx-clean-daemon-uninstall {
    @darwinOnly
    local label='com.user.utmpx-clean'
    local -a sudo_cmd=( ${(@f)"$(h-sudo-cmd)"} )
    "${sudo_cmd[@]}" /bin/sh -c '
        /bin/launchctl bootout "system/$1" 2>/dev/null
        /bin/rm -f "/Library/LaunchDaemons/$1.plist" /usr/local/libexec/utmpx-clean-stale
    ' utmpx-clean-uninstall "$label"
}
##
