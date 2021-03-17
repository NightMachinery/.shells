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
function aliasdir() {
    local name="$1" dir="$2"

    { test -z "$dir" || test -z "$name" } && {
        ecerr "$0: empty arguments. Aborting."
        return 1
    }
    mkdir -p "$dir"
    aliassafe2 "$name" indir "$dir"
}
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
    reval-ec incell gsync
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
function vcnpp() {
    local msg="${*}"

    local repo="$(vcn-getrepo)"
    pushf ~/
    {
        vcsh $repo add ~/scripts/

        # local automsg="$(vcn-with git-commitmsg)"
        # vcsh $repo commit -uno -am "${msg:-$automsg}"

        vcn-with @opts noadd y @ gsync "$msg"
        ##
        # local remote remotes=("${(@f)$(vcsh $repo remote)}")
        # for remote in $remotes[@] ; do
        # vcsh $repo pull "$remote" master --no-edit
        # done
        # for remote in $remotes[@] ; do
        # vcsh $repo push "$remote" master
        # done
        ##
        brishzr-repeat
    } always { popf }
}
function cp2tmp() {
    rsp-dl "$@" ~"/Base/_Local TMP/"
}
