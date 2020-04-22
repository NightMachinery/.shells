### SEE ALSO
# spotlight, spt, spot
###
fftmux() {
    local engine=(tmux a -t)
    test -n "$ftE[*]" && engine=("$ftE[@]")
    local sessions
    sessions="$(tmux ls|fz)" || return $?
    local i
    for i in "${(f@)sessions}"
    do
        [[ $i =~ '([^:]*):.*' ]] && {
            ec "acting on session $match[1]"
            reval "${engine[@]}" "$match[1]"
            }
    done
}
alias fft=fftmux
fftmuxkill() { ftE=(tmux kill-session -t) fftmux }
fr() {
    sels=( "${(@f)$(fd "${fd_default[@]}" "${@:2}"|fz --cycle)}" )
    test -n "$sels" && printz "$1${fr_sep:- }${sels[@]:q}"
}
f() fr "$@" --max-depth 1
ffman() {
    # mnf
    man -k . | fz --prompt='Man> ' | awk '{print $1}' | rgx '\(\d+\)$' '' | gxargs -r man
}
alias ffm=ffman
ffaliases() {
    # also see aga
    for k in "${(@k)aliases}"; do
        ec "$k=${aliases[$k]}"
    done | fz --prompt='Aliases> '
}
alias ffa=ffaliases
ffcommands() {
    # cmf (previous name)
    # command finder
    printz "$(agc "${@:-.}" | fz --prompt 'Commands> ')"
}
alias ffc=ffcommands
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
        ag "${@:-.}" |fz --no-sort --ansi | gsed 's#.*\(https*://\)#\1#')"
    ec "$links"
    ec "$links"|pbcopy
    [[ "$o" == y ]] && { local i
    for i in "${(@f)links}"
    do
        $open "$i"
    done }
    return 0
}
alias ffchrome=chis
v() {
    local files
    files=()
    command rg '^>' ~/.viminfo | cut -c3- |
                while read line; do
                    line="${line/\~/$HOME}"
                    [ -f "$line" ] && files+="$line"
                done
    test -f ~/.emacs.d/.cache/recentf && {
        command rg --only-matching --replace '$1' '^\s*"(.*)"$' ~/.emacs.d/.cache/recentf |
            while read line; do
                [ -f "$line" ] && files+="$line"
            done
    }
    # files=( ${(@)files//\~/$HOME} ) # emacs doesn't need this
    # stat doesn't expand ~
    # sort files by modification date
    # %Y     time of last data modification, seconds since Epoch
    files=(${(0@)"$(gstat  --printf='%040.18Y:%n\0' "$files[@]" | gsort -rz | gcut -z -d':' -f2-)"}) #Don't quote this there is always a final empty element
    files=( ${(0@)"$(<<<"${(F)files}" fz --print0 --query "$*")"} ) || return 1
    local ve="$ve"
    test -z "$ve" && ! isSSH && ve=nvim
    "${ve:-vim}" -p "${(@)files}"
    doc '-o opens in split view, -p in tabs. Use gt, gT, <num>gt to navigate tabs.'
}
