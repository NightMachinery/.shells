fzf-noempty() {
    local in="$(</dev/stdin)"
    test -z "$in" && (exit 130) || { ec "$in" | fzf "$@" }
}
fr() {
    sels=( "${(@f)$(fd "${fd_default[@]}" "${@:2}"|fz --cycle)}" )
    test -n "$sels" && print -z -- "$1${fr_sep:- }${sels[@]:q:q}"
}
f() fr "$@" --max-depth 1
fmn() {
    man -k . | fz --prompt='Man> ' | awk '{print $1}' | rgx '\(\d+\)$' '' | gxargs -r man
}
fcm() { printz "$(agc "${@:-.}" | fz)" }
