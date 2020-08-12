chis() {
    # Forked from fzf's wiki
    # browse Chrome's history

    local query="${*:-.}"
    local doOpen="${chis_open:-y}"

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
        rgbase "$query" |fz --exact --no-sort --ansi | gsed 's#.*\(https*://\)#\1#')"
    ec "$links"
    ec "$links"|pbcopy
    [[ "${doOpen}" == y ]] && { local i
                              for i in ${(@f)links}
                              do
                                  $open "$i"
                              done }
    return 0
}
alias ffchrome=chis
