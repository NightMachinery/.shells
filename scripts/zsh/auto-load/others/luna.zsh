alias lunar='deluna & ; lo_s=$((60*${1:-36})) lo_noinit=y lo_p=${2:-~/tmp/.luna} loop'
luna() {
    lunar pmset displaysleepnow
}
lunas() {
    lunar redo avarice 25 
    # 25  1:23.87 total
    # each is about 3.5s
}
avarice() {
    # say "disquiet creatures of avarice have risen yet again ..."
    hearinvisible "$(rndarr $NIGHTDIR/resources/luna/$~audioglob)"
    ecdate "Luna iterated."
}
alias lq='loop-startover ~/tmp/.luna'
function deluna() {
    local nonce="$(oneinstance-setup $0)" || return 1
    local timeout="${1:-150}"
    while oneinstance $0 $nonce
    do
        (( $(getidle-darwin) >= $timeout || $(getlastunlock-darwin) <= 60 )) && {
            edPre=$'\n' ecdate "$(color 255 100 255 'Deluna committed homicide!')"
            lq " via deluna"
            { isDbg && sleep 1 } || sleep 30
        }
    done
    ec deluna exited "(nonce: $nonce)"
}
