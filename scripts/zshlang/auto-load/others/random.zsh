##
aliasfn cat-random cat_random.pl
aliasfn cat-rnd cat-random
##
function rndint {
    mdoc "Usage: $0 <from> <to> <how-many>
Doesn't output duplicates. (Outputs permutations.)
The from-to range is inclusive." MAGIC

    shuf -i "$1-$2" -n "${3:-1}"
}
##
function array-random {
    if (( ${#@} == 0 )) ; then
        return 1
    fi

    local i="$(rndint 1 ${#@})"
    ec "${@[$i]}"
}
aliasfn rndarr array-random
aliasfn arrrnd array-random
aliasfn arr-rnd array-random
##
function rnd-b64() {
    doc "<BYTE-LENGTH> [<OUTPUT-FORMAT>=base64,hex]"
    openssl rand -"${2:-base64}" "$1"
}
