tmuxkillf() {
    local sessions
    sessions="$(tmux ls|fz)" || return $?
    local i
    for i in "${(f@)sessions}"
    do
        [[ $i =~ '([^:]*):.*' ]] && {
            ec "Killing $match[1]"
            tmux kill-session -t "$match[1]"
            }
    done
}
fr() {
    sels=( "${(@f)$(fd "${fd_default[@]}" "${@:2}"|fz --cycle)}" )
    test -n "$sels" && printz "$1${fr_sep:- }${sels[@]:q:q}"
}
f() fr "$@" --max-depth 1
mnf() {
    man -k . | fz --prompt='Man> ' | awk '{print $1}' | rgx '\(\d+\)$' '' | gxargs -r man
}
cmf() {
    # command finder
    printz "$(agc "${@:-.}" | fz)" }
chis() {
    # Forked from fzf's wiki
    # browse chrome history
    local cols sep google_history open
    cols=$(( COLUMNS / 3 ))
    sep='{::}'

    if [ "$(uname)" = "Darwin" ]; then
        google_history="$HOME/Library/Application Support/Google/Chrome/Default/History"
        open=open
    else
        google_history="$HOME/.config/google-chrome/Default/History"
        open=xdg-open
    fi
    cp -f "$google_history" /tmp/h
    local links="$(sqlite3 -separator $sep /tmp/h \
            "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
        awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
        ag "${@:-.}" |fz --ansi | gsed 's#.*\(https*://\)#\1#')"
    ec "$links"
    ec "$links"|pbcopy
    [[ "$o" == y ]] && { local i
    for i in "${(@f)links}"
    do
        $open "$i"
    done }
    return 0
}
v() {
    local files
    files="$(ggrep '^>' ~/.viminfo | cut -c3- |
                while read line; do
                    [ -f "${line/\~/$HOME}" ] && echo "$line"
                done | fz -q "$*" -1)" && "${ve:-nvim}" ${files//\~/$HOME}
}
