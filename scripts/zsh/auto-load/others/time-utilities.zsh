function timer-raw() {
    doc aliased to timer with noglob
    eval "sleep $((($1)*60))" && eval ${(q+@)@[2,-1]:-${(z)/#/loop ot-play-happybirthday}}
}
