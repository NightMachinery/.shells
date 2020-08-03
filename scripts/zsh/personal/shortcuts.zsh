##
diraction-personal-config (){
    #     tdl  $HOME/Downloads/Telegram\\ Desktop
    # whitespace bug in batch
    diraction create tdl "$HOME/Downloads/Telegram Desktop" --create-missing-dirs
    fnswap alias aliassafe diraction-batch-create --create-missing-dir <<< "
    base $HOME/Base
    cod $codedir
    dl  $HOME/Downloads
    vdl  $HOME/Downloads/video
    tmp  $HOME/tmp
    jtmp $HOME/julia_tmp
    ktmp $HOME/tmp-kindle
    cel $cellar
    nt $HOME/cellar/notes/
    jrl $HOME/cellar/notes/journal
    dom $DOOMDIR
"
}
antibody bundle "adrieankhisbe/diractions"

aliasfn cellp incell gsync
aliasfnq incell indir "$cellar"
aliasfnq indl indir ~/Downloads/
aliasfnq intdl indir ~"/Downloads/Telegram Desktop"
##
vcnpp() {
    vcsh night.sh add ~/scripts/
    vcsh night.sh commit -uno -am "." ; vcsh night.sh pull --no-edit ; vcsh night.sh push
}
function cp2tmp() {
    rsp-dl "$@" ~"/Base/_Local TMP/"
}
