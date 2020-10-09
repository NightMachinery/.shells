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
aliasdir base $HOME/Base
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
aliasfn cellp incell gsync
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
function vcnpp() {
    local msg="${*}"

    local repo="$(vcn-getrepo)"
    pushf ~/
    {
        vcsh $repo add ~/scripts/
        vcsh $repo commit -uno -am "${msg:-$(vcn-with git-commitmsg)}"
        vcsh $repo pull --no-edit
        vcsh $repo push
    } always { popf }
    # fnswap git "vcsh $(gq "$repo")" @opts noadd y @ gsync "$msg"
}
function cp2tmp() {
    rsp-dl "$@" ~"/Base/_Local TMP/"
}
