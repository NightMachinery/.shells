(( $+functions[dvar] )) || function dvar() {
        local pre=''
        test -z "$2" || pre="CODE $2 | "
        echo "$pre$(typeset -p "$1" 2>&1)"
        echo "$pre$1 in env: $(printenv "$1")"
    }
localesc2() {
    echo Innocence
    dvar ino inside0
    typeset -Ag ino
    ino[apple]=yes
    dvar ino inside1
    # memoi-eval ec "$@"
    find-music "$@"
}
tlocalesc2() {
    dvar music_dir t0
    local music_dir #=outside1
    music_dir=outside2
    dvar music_dir t1
    dvar ino out0
    music_dir=~/my-music/ localesc2 "$@"
    dvar ino out1
    dvar music_dir t2
}
