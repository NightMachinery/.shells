##
function zsh_directory_name_1() {
    # @docs 14.7.1 Dynamic named directories at http://zsh.sourceforge.net/Doc/Release/Expansion.html#Filename-Expansion

    local type="$1" arg="$2" z_mode=y
    if [[ "$type" == n ]] ; then
        # Dynamic named directories
        # e.g., `ll ~[dl]`

        # re typ type arg
        ##
        local o
        case "$arg" in
            DATE)
                o="$(date)"
                ;;
            *)
                o="${aliased_dirs[$arg]}"
                if test -d "$o" ; then
                elif test -n "$z_mode" ; then
                    o="$(ffz-get "$arg")" @RET
                    # re typ arg o
                else
                    return 1
                fi
                ;;
        esac

        typeset -ga reply
        reply=("$o")
        return 0
    elif [[ "$type" == c ]]; then
        # complete names
        local expl
        local -a dirs
        dirs=( ${(@k)aliased_dirs} )
        _wanted dynamic-dirs expl 'dynamic directory' compadd -S\] -a dirs
        return
    elif [[ "$type" == d ]]; then
        return 1
    else
        ecerr "$0: Unknown type '$type'"
        return 1
    fi
    return 1
}
typeset -ag zsh_directory_name_functions=()
zsh_directory_name_functions+='zsh_directory_name_1'
##
# diraction-personal-config (){
#     #     tdl  $HOME/Downloads/Telegram\\ Desktop
#     # whitespace bug in batch
#     diraction create tdl "$HOME/Downloads/Telegram Desktop" --create-missing-dirs
#     fnswap alias aliassafe diraction-batch-create --create-missing-dir <<< "
# "
# }
# source-plugin "adrieankhisbe/diractions"
##
typeset -Ag aliased_dirs
function aliasdir() {
    local name="$1" dir="$2"

    { test -z "$dir" || test -z "$name" } && {
        ecerr "$0: empty arguments. Aborting."
        return 1
    }
    mkdir -p "$dir"
    aliassafe2 "$name" indir "$dir"
    aliased_dirs[$name]="$dir"
    if [[ "$name" != *=* ]] ; then
        # Static named directories http://zsh.sourceforge.net/Doc/Release/Expansion.html#Filename-Expansion
        # There does not seem to be a way to disable the abbreviation of paths via these
        #  I emailed this: `Is it possible to disable abbreviation of named directories?`
        hash -d "$name"="$dir"
    fi
}
if isDarwin ; then
    aliasdir vol /Volumes
fi
aliasdir base $HOME/base
aliasdir cod $codedir
aliasdir dl  $HOME/Downloads
aliasfn indl dl
aliasdir dlt ~"/Downloads/Telegram Desktop"
aliasdir dlv  $HOME/Downloads/video
aliasdir tmp  $HOME/tmp
aliasdir jtmp $HOME/julia_tmp
aliasdir ktmp $HOME/tmp-kindle
aliasdir cel $cellar
aliasfn incell cel
aliasdir jrl "${nightNotesPrivate}/journal"
aliasdir dom $DOOMDIR
aliasdir innt "${nightNotesN}"
aliasdir nt "${nightNotesN}"
aliasdir incache ~/base/cache
aliasdir cac ~/base/cache
##
aliasdir mu "$music_dir"

function path-abbrev-to-music-dir {
    in-or-args "$@" \
        | sd '/Volumes/hyper-diva/Songs/' '~mu/Songs/' \
        | sd '/Volumes/hyper-diva/video/V' '~mu/V/' \
        | sd '/Volumes/Yellow Fruit/Music/' '~mu/' \
        | sd "$music_dir/" '~mu/' \
        | cat-copy-if-tty
}
##
function cellp {
    assert-net @RET

    #: Use =fnswap brishzr true= to disable this:
    brishzr-repeat
    #: to commit remote stuff so that we can pull them

    for root in "${remindayRootDir}" "${nightNotesPrivate}" "${nightNotesPublic}" "${timetracker_dir}" "${nightResourcesPrivate}" ; do
        ec-sep-h
        ecbold "* ${root}"
        pushf "$root"
        {
            if git-merge-p ; then
                local err="git merge already in progress"
                ecerr "$0 (${root}): $err"

                fsay-noidle "$err in the cellar's ${root:t}"

                # return 3
                continue
            fi

            reval-ec trs-empty-files "$root"

            git_commitmsg_ask=no reval-ec gsync
        } always { popf }
    done

    brishzr-repeat
    #: to pull the new changes
}
##
function vcn-getrepo {
    local repo=night.sh
    isMBP && repo=.shells
    ec $repo
}

function vc-with {
    local repo="$1" ; shift
    assert-args repo @RET

    gsync_noadd="${gsync_noadd:-y}" fnswap git "vcsh $(gq "$repo")" "$@"
}

function vcn-with {
    local repo
    repo="$(vcn-getrepo)" @TRET

    vc-with "$repo" "$@"
}

function vcns {
    pushf ~/ # to have nice paths in git's output
    {
        vcn-with git add "$NIGHTDIR" # To see newly added files in the status
        vcn-with gss -uno
    } always { popf }
}

function vcndiff {
    vcn-with git add --intent-to-add ~/scripts/
    vcn-with git-diff HEAD~"${1:-0}"
}

function vcnpp {
    local msg="${*}"
    # if isIReally ; then
    #     assert-args msg @RET
    # fi

    pushf ~/
    {
        assert reval-ec vcn-with git add "${NIGHTDIR}" @RET
        assert reval-ec vcn-with @opts noadd y @ gsync "$msg" @RET

        brishz-restart

        # brishzr-repeat #: runs the same command on the default server
        true
    } always { popf }
}
##
function gsync-extra-private() {
    fnswap git 'vcsh extra-private' @opts noadd y @ gsync "$@"
}
##
function cp2ltmp() {
    rsp-dl "$@" ~"/Base/_Local TMP/"
}
##
function cdtmp {
    local name="$*"

    reval-ec cdm ~tmp/"${name}_cdtmp_$(uuidm)"
}
##
