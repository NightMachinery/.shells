function ensurerun() {
    ruu "retry gtimeout $1" "${@:2}"
}
function loop() {
    mdoc "Usage: [lo_s=<interval> lo_noinit=<skip-first-iteration> lo_p=<socket-to-startover> lo_sig2cancel=] $0 CMD
    Set lo_sig2cancel=y to terminate the whole loop when receiving a signal.
    If you need to be able to ctrl-c to just cancel the loop command (not the whole loop), you'll need to support it in your loop command. See the source of avaricemany for what needs to be done.
" MAGIC
    setopt localoptions localtraps
    # Well I couldn't debug this. Setting set +m is enough to cause bugs, but the other lines make things progressively worse ...
    # set +m
    local inter=${lo_s:-1}
    local cancelMode="$lo_sig2cancel"
    local cmd="$(gquote "${@}")"
    local lo_p="$lo_p"
    ensure-dir "$lo_p"

    >&2 chalk -t "{rgb(255,255,255).bgRgb(0,30,230) Looping {rgb(0,30,230).bgRgb(255,255,255) $cmd} with interval {rgb(255,73,28) $inter}}"
    local sig=1 neon c=0 sig2=0
    test -z "$lo_noinit" || {
        color 0 255 100 "$(colorbg 255 255 255)Skipping first iteration" >&2
        sig=0
    }
    test -n "$cancelMode" && {
        color 0 255 100 "$(colorbg 255 255 255)Loop will terminate if a signal is received." >&2
    }
    while (( sig2 != 666 )) && (( sig != 130 ))
    do
        (( sig )) && { eval "$cmd" ; c=$[c+1] }
        print -n "\r$(colorbg 0 30 230)$(colorfg 255 255 100)Iteration $c$(resetcolor)"
        # </dev/null sleep-neon $inter &
        # neon=$!
        trap _loop_trap INT
        comment "We can't handle the signals when we are running a command; see https://unix.stackexchange.com/questions/24087/wrapper-program-that-sets-signal-handler"
        [[ -z "$lo_p" ]] && { sleep $inter ; sig=1 } || {
            gtimeout --preserve-status ${inter}s socat -u unix-listen:${lo_p},unlink-early -
            sig=$?
            if (( sig == 0 )) && test -n "$cancelMode" ; then
                return 0
            fi
        }
        trap - INT # resets to default
        # ec sig $sig >> a
        # kill $neon
    done
}
function oneinstance-setup() {
    ensure-redis || return 1
    
    local someNonce="${1}"
    test -z "$someNonce" && { ecerr someNonce is empty. ; return 1 }
    someNonce="nonce_${someNonce}"

    (( $(redis-cli --raw exists $someNonce) )) || sout redis-cli set $someNonce 0
    local nonce=$(( $(redis-cli --raw get $someNonce) + 1 ))
    (( $nonce == 10000 )) && nonce=0
    sout redis-cli set $someNonce $nonce
    ec $nonce
}
oneinstance() {
    local nonce="$2"
    test -z "$nonce" && { ecerr nonce is empty. ; return 1 }

    local someNonce="${1}"
    test -z "$someNonce" && { ecerr someNonce is empty. ; return 1 }
    someNonce="nonce_${someNonce}"

    (( $nonce == $(redis-cli --raw get $someNonce) ))
}

_loop_trap() {
    ec '
loop interrupted' ; sig2=666
}
loop-startover() { edPre=$'\n' ecdate "Signal from loop-startover${@:2}" | socat -u - unix-connect:${1} }
alias loops='loop-startover' #Oops :D
##
inbg() {
    { eval "$(gquote "$@")" & }
    disown &>/dev/null  # Prevent whine if job has already completed
}
awaysh() inbg silent "$@"
function away() {
    ruu 'nohup --' "$@" &
    disown &>/dev/null  # Prevent whine if job has already completed
}
##
killjobs() {
    local kill_list="$(jobs)"
    if [ -n "$kill_list" ]; then
        # this runs the shell builtin kill, not unix kill, otherwise jobspecs cannot be killed
        # the `$@` list must not be quoted to allow one to pass any number parameters into the kill
        # the kill list must not be quoted to allow the shell builtin kill to recognise them as jobspec parameters
        kill $@ $(gsed --regexp-extended --quiet 's/\[([[:digit:]]+)\].*/%\1/gp' <<< "$kill_list" | tr '\n' ' ')
    else
        return 0
    fi
}
