diraction-personal-config (){
    #     tdl  $HOME/Downloads/Telegram\\ Desktop
    # whitespace bug in batch
    diraction create tdl "$HOME/Downloads/Telegram Desktop" --create-missing-dirs
    diraction-batch-create --create-missing-dir <<< "
    base $HOME/Base
    dl  $HOME/Downloads
    vdl  $HOME/Downloads/video
    tmp  $HOME/tmp
	  jtmp $HOME/julia_tmp
	  ktmp $HOME/tmp-kindle
    cel $cellar
    nt $HOME/cellar/notes/
    jrl $HOME/cellar/notes/journal
"
}
antibody bundle "adrieankhisbe/diractions"
vcnpp() {
	vcsh night.sh add ~/scripts/
    vcsh night.sh commit -uno -am "." ; vcsh night.sh pull --no-edit ; vcsh night.sh push
}
inqcell() incell "$(gquote "$@")"
incell() {
    pushf "$cellar"
    eval "$@"
    popf
}
alias cellp="incell 'ga . ; gcam . ; gl --no-edit ; gp'"
indl() {
    pushf ~/Downloads/
    eval "$@"
    popf
}
