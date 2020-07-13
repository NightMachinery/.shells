function lunar() {
    deluna ${deluna} & # timeout of deluna
    lo_s=$((60*${lo_min:-45})) lo_noinit=y lo_p=${lo_p:-~/tmp/.luna} loop "$@"
}
luna() {
    lunar pmset displaysleepnow
}
lunas() {
    lunar avaricemany
}
avaricemany() {
    setopt localtraps
    # So I don't understand these all that well, but here is my guess:
    trap "" INT # disables INT handling in this function, so we don't quit by INT
    (redo avarice 25) # this is a new process so it has its own signal handling and so does quit on INT
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
    local nonce
    nonce="$(oneinstance-setup $0)" || return 1
    local timeout="${1:-150}" # 150 is good for PC work, but 800 might be better for reading, as the screen dims in 10 minutes
    ec "deluna (nonce: $nonce) started with timeout $timeout"
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
