(( $+functions[dvar] )) || function dvar() {
        local pre=''
        test -z "$2" || pre="CODE $2 | "
        echo "$pre$(typeset -p "$1" 2>&1)"
        echo "$pre$1 in env: $(printenv "$1")"
    }
localesc() {
    echo Innocence
}
tlocalesc() {
    dvar music_dir t0
    local music_dir #=outside1
    music_dir=outside2
    dvar music_dir t1
    # localesc
    music_dir=hi localesc
    dvar music_dir t2
}
