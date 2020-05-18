alias lunar='deluna & ; lo_s=$((60*${1:-45})) lo_noinit=y lo_p=${2:-~/tmp/.luna} loop'
luna() {
    lunar pmset displaysleepnow
}
lunas() {
    lunar avaricemany
}
avaricemany() {
    redo avarice 25
    # 25  1:23.87 total
    # each is about 3.5s

    ecdate "Luna iterated."
}
avarice() {
    # say "disquiet creatures of avarice have risen yet again ..."
    hearinvisible "$(rndarr $NIGHTDIR/resources/luna/$~audioglob)"
}
alias lq='loop-startover ~/tmp/.luna'
function deluna() {
    local nonce="$(oneinstance-setup $0)" || return 1
    local timeout="${1:-150}"
    while oneinstance $0 $nonce
    do
        (( $(getidle-darwin) >= $timeout || $(getlastunlock-darwin) <= 80 )) && {
            edPre=$'\n' ecdate "$(color 255 100 255 'Deluna committed homicide!')"
            lq " via deluna"
            # { isDbg && sleep 1 } || sleep 30
        }
        sleep 30 # to avoid cpu usage
    done
    ec deluna exited "(nonce: $nonce)"
}
function nnl() {
    mdoc "Not Now Luna!" MAGIC
    local started="$(date +"%s")"
    local vol="$(get-volume)"
    local timeout=150
    isDbg && timeout=5

    set-volume 0

    # No need for this much force:
    # while (( ($(date +"%s") - $started) <= $timeout ))
    # do
    #     set-volume 0
    # done

    sleep $timeout

    set-volume "$vol"
}
