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
vcnpp() {
    vcsh night.sh add ~/scripts/
    vcsh night.sh commit -uno -am "." ; vcsh night.sh pull --no-edit ; vcsh night.sh push
}
function cp2tmp() {
    rsp-dl "$@" ~"/Base/_Local TMP/"
}
