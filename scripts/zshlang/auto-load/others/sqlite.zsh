function sqlite-cat() {
    : "@alt catsql"

    sqlite3 "$1" .dump
}
