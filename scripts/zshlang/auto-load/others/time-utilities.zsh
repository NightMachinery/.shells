function h_timer-bell() {
    tts-glados1-cached "The time is now up, commander"
    awaysh redo2 2 bell-visual-flash1
    ##
    # awaysh tts-gateway-i2 "Deploy the delayed sequence." ;  tts-gateway-i1 "Deploy the delayed sequence."
    # awaysh tts-gateway-i2 "Deploy the delayed sequence." ;  awaysh tts-gateway-i1 "Deploy the delayed sequence." ; tts-gateway "Deploy the delayed sequence."
    ##
    # bell-pp-electricity
    # bell-visual-flash1
}
function timer() {
    doc aliased to timer with noglob
    local t=$1 cmd=("${@[2,-1]}")
    test -z "$cmd[*]" && cmd=(eval lo_s=20 loop h_timer-bell)

    sleep-neon $(($t * 60)) && {
        terminal-notifier -title "Timer" -message ""
        reval "$cmd[@]"  #eval ${(q+@)@[2,-1]:-${(z)/#/loop ot-play-happybirthday}}
    }
}
noglobfn timer
function timer-lm() {
    timer "${1:-30}" eval lo_s="${2:-60}" loop bell-lm-whattimeisit
}
noglobfn timer-lm
aliasfn timer-late timer-lm
