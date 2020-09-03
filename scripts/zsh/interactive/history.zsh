###
alias tmnte=increment-last\ \''(E)(\d+)'\'
alias tmnt=increment-last\ \''()(\d+)(?=\D*\z)'\'
##
# increment-episode() {
# superseded by tmnte
#   emulate -L zsh
#   setopt extendedglob
#   local cmd=${$(fc -nl -1 -1)/(#b)(*E)(<->)/$match[1]${(l:${#match[2]}::0:)$((match[2]+${1:-1}))}}
#   geval "$cmd"
# }
##
function increment-last() {
    #$1 is supplied in our alias tmnt. :D
    local pe='s/'$1'/$1 . (sprintf "%0*d", length($2), $2 + '"${2:-1}"')/e'
    #ec "$pe"
    #fc might not work in bash
    local cmd=${$(fc -nl -1 -1| perl -pe "$pe")}
    geval "$cmd"
}
###
function increment-last2() {
    local pattern="$1"
    local after="${2}"
    local cmd="${$(fc -nl -1 -1)}"

    local newcmd=''
    if [[ "$cmd" =~ "$pattern" ]] ; then
        # dact typ cmd
        # dact typ match
        local len="${#match[2]}"
        typeset -Z$len num # option -Z which is used for zero padding. Will remove digits in overflow.
        num=$((match[2] + 1))
        test -z "$after" && after="${match[3]}"
        newcmd="${match[1]}${num}$after"
        geval $newcmd
    fi
}
function tmnt-hi10() {
    increment-last2 '^(.*_)(\d+)(_.*)$' '*.m*'
}
aliasfn tmnth tmnt-hi10
