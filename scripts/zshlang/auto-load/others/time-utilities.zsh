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
    local t=$1 cmd=("${@[2,-1]}") notify="${timer_notify:-y}"
    test -z "$cmd[*]" && cmd=(eval lo_s=20 loop h_timer-bell)

    sleep-neon $(($t * 60)) && {
        if bool $notify ; then
            terminal-notifier -title "Timer" -message ""
        fi
        reval "$cmd[@]"  #eval ${(q+@)@[2,-1]:-${(z)/#/loop ot-play-happybirthday}}
    }
}
noglobfn timer
##
function h_timer-late() {
    local i="${prv_loop_iteration}" i_dur="$lo_s"

    ##
    # now using awaysh
    # i_dur=$(( i_dur + 5.3 )) # compensate for our time budget
    ##
    local bell_awaysh=no
    bell-visual-flash1
    bell-lm-whattimeisit
    tts-glados1-cached "$(( int(i * i_dur / 60) )) minutes late ... so so late"'!'
}
function timer-late() {
    @opts notify no @ timer "${1:-30}" eval lo_s="${2:-60}" loop awaysh h_timer-late
}
noglobfn timer-late
##
