function sqlite-cat() {
    : "@alt catsql"

    sqlite3 "$1" .dump
}
##
function sqlite2csv-table {
    local db="${1}" table="${2}" output="${3}"
    assert-args db table @RET
    if test -z "$output" ; then
        output="${db:r}_hi${table}.csv"
    fi
    [[ "$output" =~ '.csv$' ]] || output+='.csv'

    ensure-dir "$output" @RET
    ec "$0: outputting table '$table' to '$output'"
    assert sqlite3 -header -csv "$db" "select * from ${table};" > "$output" || return $?
}

function sqlite2csv {
    : "@seeAlso sqlite2txt.py"

    local db="${1}" o="${2}"
    assert-args db @RET

    tables=($(sqlite3 $db ".tables")) @RET #: this might need quoting
    local t
    for table in $tables[@] ; do
        assert sqlite2csv-table "$db" "$table" "${o}_${table}.csv" @RET
    done
}
##
