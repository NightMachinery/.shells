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
