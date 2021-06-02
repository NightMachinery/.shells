##
function zsh_directory_name_1() {
    # @docs 14.7.1 Dynamic named directories at http://zsh.sourceforge.net/Doc/Release/Expansion.html#Filename-Expansion

    local type="$1" arg="$2" z_mode=y
    if [[ "$type" == n ]] ; then
        # Dynamic named directories
        # e.g., `ll ~[dl]`

        # re typ type arg
        local o="${aliased_dirs[$arg]}"
        if test -d "$o" ; then
        elif test -n "$z_mode" ; then
            o="$(ffz-get "$arg")" @RET
            # re typ arg o
        else
            return 1
        fi
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
# antibody bundle "adrieankhisbe/diractions"
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
aliasdir jrl $HOME/cellar/notes/journal
aliasdir dom $DOOMDIR
aliasdir innt $cellar/notes/
aliasdir nt $cellar/notes/
aliasdir incache ~/base/cache
aliasdir cac ~/base/cache
##
function cellp() {
    brishzr-repeat # now that eva is a remote, we just need to make sure things are clean and committed there

    trs-empty-files $nightNotes

    git_commitmsg_ask=no reval-ec incell gsync
}
##
function vcn-getrepo() {
    local repo=night.sh
    isMBP && repo=.shells
    ec $repo
}
function vcn-with() {
    local repo="$(vcn-getrepo)"
    fnswap git "vcsh $(gq "$repo")" "$@"
}
function vcns() {
    pushf ~/ # to have nice paths in git's output
    {
        vcn-with git add "$NIGHTDIR" # To see newly added files in the status
        vcn-with gss -uno
    } always { popf }
}
function vcndiff() {
    vcn-with git add ~/scripts/
    vcn-with git-diff HEAD~"${1:-0}"
}
function vcnpp() {
    local msg="${*}"

    pushf ~/
    {
        assert vcn-with git add ~/scripts/ @RET
        assert vcn-with @opts noadd y @ gsync "$msg" @RET

        brishz-restart

        brishzr-repeat
        true
    } always { popf }
}
##
function gsync-extra-private() {
    fnswap git 'vcsh extra-private' @opts noadd y @ gsync "$@"
}
##
function cp2tmp() {
    rsp-dl "$@" ~"/Base/_Local TMP/"
}
