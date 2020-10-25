candidate-aliases() {
    fc -l 1 9999999 | awk '{print $2" "$3}' | sort | uniq -c | sort -n
    # fc -l 1 = history (in bash?)
}
char-usage() {
    # local text="$(arrN "$@" | xargs cat)"
    local groupby="${cuG:-1}"
    local file
    for file in "$@"
    do
        test -f "$file" || continue
        isbinary "$file" && continue
        local i=0
        local acc=""
        for char in "${(@s//)$(<$file)}"
        do
            acc+="$char"
            i=$((i+1))
            if (( i == groupby ))
            then
                i=0
                ec $acc
                acc=""
            fi
        done
        ecerr "processed $file ..."
    done | gsort | uniq -c | gsort -n
}
char-usage-all() {
    : GLOBAL vfiles
    init-vfiles

    char-usage $vfiles[@]  $NIGHTDIR/**/*(.) | tac > "all $(dateshort).cusage"
}
##
function tokei-largest() {
    wc -l **/${~codeglob} | gsort -n
}
function tokei-percent() {
    # tokei 12.0.4
    local t="$(tokei)"
    local headers=($(<<<$t gsed -n 2p))
    local totals=( $(<<<$t tac | gsed -n 2p) )
    local lines=( "${(@f)$(<<<$t gsed 1,2d)}" )

    local line field fields i newfield lastfield newline=()
    for line in "$lines[@]" ; do
        if [[ "$line" =~ '(^-*$)|(Total)' ]] ; then
            continue
        fi
        fields=( $(print -r -- $line) )
        i=2
        lastfield=()
        for field in "$fields[@]" ; do
            if [[ "$field" =~ '^\d+$' ]] ; then
                newfield=${"$(( field * 100 / ${totals[$i]}. ))"}
                newfield="$(printf "%.1f" "$newfield")"
                test -n "$lastfield[*]" && {
                    newline+=$lastfield[*]
                    lastfield=()
                }
                newline+=$newfield
                i=$((i+1))
            else
                lastfield+=$field
            fi
        done
    done
    table-print "$headers[@]" -- "$newline[@]"
}
