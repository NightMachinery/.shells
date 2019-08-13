lpassf() {
    lpass ls  | fz -q "$*" | awk '{print $(NF)}' | sed 's/\]//g'
}
function lpassf_() {
    doc 'macro to create lpassf* functions'
    local body="re lpass$1 "'"${(@f)$(lpassf "$*")}"'
    eval "function lpassf$1() { $body }" 
}
re lpassf_ u p g
alias lpg=lpassfg
alias lpp=lpassfp
alias lpu=lpassfu
function lp() {
    comment 'Empty "$@" passes the -n test -_-'
    test -n "$*" && { ecdbg Resetting lp_last ; lp_last='' }
    ecdbg lp_last is "$lp_last"
    [[ -z "$lp_last" ]] && {
        lp_last=("${(@f)$(lpassf "$*")}")
        lpassu "$lp_last"
        local u="$(pbpaste)"
        ec "$u"
    } || {
        lpassp "$lp_last"
        lp_last=''
    }
}
lpassg() {
    lpass show --basic-regexp --expand-multi "$@"
}
lpassu() {
    lpassg --username "$@" |pbcopy
}
lpassp() {
    lpassg --password "$@" |pbcopy
}
