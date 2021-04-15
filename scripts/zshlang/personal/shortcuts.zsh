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
    vcn-with git diff --submodule=diff HEAD~"${1:-0}"
}
function vcnpp() {
    local msg="${*}"

    pushf ~/
    {
        assert vcn-with git add ~/scripts/ @RET
        assert vcn-with @opts noadd y @ gsync "$msg" @RET

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
