diraction-personal-config (){
    # put your config here
    diraction-batch-create <<< "
        dl  ~/Downloads
        tmp  ~/tmp
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
alias cellp="incell 'gcam . ; gl --no-edit ; gp'"
indl() {
    pushf ~/Downloads/
    eval "$@"
    popf
}
