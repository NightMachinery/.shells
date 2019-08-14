increment-last() {
    #$1 is supplied in our alias tmnt. :D
    local pe='s/'$1'/$1 . (sprintf "%0*d", length($2), $2 + '"${2:-1}"')/e'
    #ec "$pe"
    #fc might not work in bash
    local cmd=${$(fc -nl -1 -1| perl -pe "$pe")}
    geval "$cmd"
}
